;;; clatter-connection.el --- IRC network connection management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Network connection management for clatter.el.
;; Handles TCP/TLS connections, async I/O via process filters,
;; reconnection with exponential backoff, and health monitoring.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-socks)

(declare-function gnutls-negotiate "gnutls")

;; --- Connection Structure ---

(cl-defstruct (clatter-connection (:constructor clatter-connection--create))
  "An IRC network connection."
  network-id           ; string: network name from config
  config               ; effective plist, including `clatter-connect' overrides
  connect-overrides    ; original keyword overrides for reconnecting
  process              ; network process
  state                ; :disconnected :connecting :registering :connected
  nick                 ; current nickname
  ;; SASL
  sasl-state           ; nil :requested :authenticating :done
  cap-negotiating      ; t during CAP negotiation
  cap-enabled          ; list of enabled capability strings
  cap-available        ; list of capabilities advertised by the server
  cap-string           ; capability string sent by the server
  ;; IRCv3 batch/label tracking
  active-batches       ; hash-table: batch-id -> plist
  pending-labels       ; hash-table: label -> callback
  label-counter        ; integer
  ;; Reconnection
  reconnect-enabled    ; t to auto-reconnect
  reconnect-attempts   ; integer
  reconnect-timer      ; timer object
  regain-kill-count    ; integer: consecutive "regained by services" kills
  regain-kill-time     ; float-time of last regain kill, nil if none
  desired-nick         ; string: the configured nick we want to reclaim
  nick-reclaim-timer   ; timer for periodic nick reclaim attempts
  ;; Health monitoring
  last-activity        ; float-time of last activity
  ping-sent-time       ; float-time of last health ping, nil if no pending
  health-timer         ; timer for periodic pings
  ;; Receive buffer (partial line accumulation)
  recv-buffer          ; string: incomplete data from last read
  ;; ISUPPORT (005) parameters
  isupport             ; hash-table: param -> value (from RPL_ISUPPORT)
  ;; MOTD/WHOIS accumulation
  -motd-lines          ; list: accumulating MOTD lines
  -whois-data)         ; plist: accumulating WHOIS data

;; --- Active connections registry ---

(defvar clatter-connections (make-hash-table :test 'equal)
  "Hash table mapping network-id to `clatter-connection'.")

(defun clatter-get-connection (network-id)
  "Get the connection for NETWORK-ID."
  (gethash network-id clatter-connections))

;; --- Debug Logging ---

(defun clatter--debug (format-string &rest args)
  "Log FORMAT-STRING with ARGS if `clatter-log-raw-protocol' is non-nil."
  (when clatter-log-raw-protocol
    (with-current-buffer (get-buffer-create "*clatter-debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

(defcustom clatter-watchdog-log
  (expand-file-name "clatter/watchdog.log" user-emacs-directory)
  "File path for connection watchdog log.
This log persists across Emacs restarts to help diagnose lockups."
  :type 'file
  :group 'clatter)

(defun clatter--watchdog (format-string &rest args)
  "Write a timestamped line (FORMAT-STRING with ARGS) to the watchdog log file.
Always writes regardless of `clatter-log-raw-protocol' setting."
  (let ((dir (file-name-directory clatter-watchdog-log))
        (line (concat (format-time-string "[%F %T] ")
                      (apply #'format format-string args)
                      "\n")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (write-region line nil clatter-watchdog-log 'append 'silent)))

;; --- Send ---

(defun clatter-send (conn line)
  "Send LINE to the IRC server via CONN.
Appends CR-LF automatically.  Refuses to write if the connection
appears stale (no data received for `clatter-ping-timeout' seconds)
to avoid blocking Emacs on a dead GnuTLS socket."
  (cl-block clatter-send
    (let ((proc (clatter-connection-process conn)))
      (when (and proc (process-live-p proc)
                 (memq (process-status proc) '(open run)))
        ;; Refuse to write on a likely-dead connection.  GnuTLS send can
        ;; block indefinitely at the C level if the remote peer is gone,
        ;; freezing the entire Emacs event loop.
        (let ((since-recv (- (float-time)
                             (or (clatter-connection-last-activity conn)
                                 (float-time)))))
          (when (> since-recv clatter-ping-timeout)
            (clatter--watchdog "SEND-REFUSE %s (stale %.0fs, killing)"
                               (clatter-connection-network-id conn) since-recv)
            (delete-process proc)
            (cl-return-from clatter-send nil)))
        (clatter--debug ">> %s" line)
        (run-hook-with-args 'clatter-rawlog-outgoing-hook
                            (clatter-connection-network-id conn) line)
        (let ((payload (concat line "\r\n"))
              (network-id (clatter-connection-network-id conn)))
          (condition-case err
              (with-local-quit
                (process-send-string proc payload))
            (error
             (clatter--watchdog "SEND-FAIL %s %s"
                                network-id (error-message-string err))
             (delete-process proc))))))))

;; --- Receive (Process Filter) ---

(defun clatter--process-filter (proc string)
  "Process filter for incoming IRC data (STRING) from PROC.
Accumulates partial lines and dispatches complete ones."
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (clatter-get-connection network-id)))
    (when conn
      (setf (clatter-connection-last-activity conn) (float-time))
      ;; Immediately append incoming chunks to the connection buffer.
      (setf (clatter-connection-recv-buffer conn)
            (concat (string-to-multibyte (or (clatter-connection-recv-buffer conn) ""))
                    (string-to-multibyte string)))
      ;; Guard against process filter re-entrancy using a busy flag
      (unless (process-get proc :clatter-filter-busy)
        (process-put proc :clatter-filter-busy t)
        (unwind-protect
            ;; Process complete lines (terminated by CRLF or LF).  Strip any
            ;; number of trailing CRs: the builtin network transport auto
            ;; converts CRLF to LF, but the external subprocess pipe does not,
            ;; so the raw CR must be removed here to avoid trailing ^M.
            (while (string-match "\r*\n" (or (clatter-connection-recv-buffer conn) ""))
              (let* ((data (clatter-connection-recv-buffer conn))
                     (line (substring data 0 (match-beginning 0))))
                ;; Slice the buffer *before* handling the line.
                ;; If a re-entrant call happens during `clatter--handle-line`,
                ;; it will safely append to this remainder.
                (setf (clatter-connection-recv-buffer conn) (substring data (match-end 0)))
                (when (> (length line) 0)
                  ;; Log raw lines to watchdog during registration
                  (unless (eq (clatter-connection-state conn) :connected)
                    (clatter--watchdog "RECV %s %s" network-id
                                       (substring line 0 (min (length line) 200))))
                  (clatter--debug "<< %s" line)
                  (run-hook-with-args 'clatter-rawlog-incoming-hook
                                      network-id line)
                  (clatter--handle-line conn line))))
          ;; Always release the lock, even if an error is thrown during parsing
          (process-put proc :clatter-filter-busy nil))))))

(defun clatter--handle-line (conn line)
  "Parse and handle a single IRC LINE on CONN.
This is the main entry point from the process filter into the handler layer."
  (let ((msg (clatter-parse-line line)))
    (when msg
      ;; Dispatch to handlers (defined in clatter-handlers.el)
      (clatter-dispatch-message conn msg))))

;; Forward declaration - implemented in clatter-handlers.el
(declare-function clatter-dispatch-message "clatter-handlers")

;; --- Process Sentinel (disconnect handling) ---

(defun clatter--process-sentinel (proc event)
  "Process sentinel for PROC handling disconnect EVENT."
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (clatter-get-connection network-id)))
    (when conn
      (clatter--debug "sentinel: %s %s" network-id (string-trim event))
      (clatter--watchdog "DISCONNECT %s event=%s" network-id (string-trim event))
      (message "[clatter] Disconnected from %s: %s" network-id (string-trim event))
      (setf (clatter-connection-state conn) :disconnected)
      (setf (clatter-connection-process conn) nil)
      ;; Cancel health timer
      (when (clatter-connection-health-timer conn)
        (cancel-timer (clatter-connection-health-timer conn))
        (setf (clatter-connection-health-timer conn) nil))
      ;; Cancel nick reclaim timer
      (when (clatter-connection-nick-reclaim-timer conn)
        (cancel-timer (clatter-connection-nick-reclaim-timer conn))
        (setf (clatter-connection-nick-reclaim-timer conn) nil))
      ;; Notify UI
      (run-hook-with-args 'clatter-disconnect-hook network-id event)
      ;; Auto-reconnect
      (when (clatter-connection-reconnect-enabled conn)
        (let ((attempts (clatter-connection-reconnect-attempts conn)))
          (clatter--watchdog "RECONNECT-SCHEDULE %s attempt=%d delay=%ds"
                             network-id (1+ attempts)
                             (min (* clatter-reconnect-initial-delay
                                     (expt 2 attempts))
                                  clatter-reconnect-max-delay))
          (clatter--schedule-reconnect conn))))))

;; --- External TLS helper subprocess ---

(defun clatter--tls-external-args (server port client-cert)
  "Build the command list for an external TLS client to SERVER:PORT.
CLIENT-CERT, when non-nil and existing, is presented to the server.
The program is selected by `clatter-tls-external-command'."
  (let ((have-cert (and client-cert (file-exists-p client-cert))))
    (pcase clatter-tls-external-command
      ("gnutls-cli"
       (append (list "gnutls-cli" "--port" (number-to-string port))
               (when have-cert
                 (list (concat "--x509certfile=" client-cert)
                       (concat "--x509keyfile=" client-cert)))
               (list server)))
      (_
       (append (list (if (equal clatter-tls-external-command "openssl")
                         "openssl" clatter-tls-external-command)
                     "s_client" "-quiet"
                     "-connect" (format "%s:%d" server port)
                     "-servername" server)
               (when have-cert
                 (list "-cert" client-cert "-key" client-cert)))))))

(defun clatter--make-external-tls-process (network-id server port client-cert)
  "Spawn an external TLS subprocess for NETWORK-ID connecting to SERVER:PORT.
CLIENT-CERT is an optional client certificate file.  Returns the process.
IRC is spoken in plaintext over the subprocess pipe, so a dead network
socket blocks the subprocess instead of Emacs's event loop."
  (let* ((cmd (clatter--tls-external-args server port client-cert))
         (program (car cmd)))
    (unless (executable-find program)
      (error "External TLS client not found: %s" program))
    ;; Force line-buffered output so incoming IRC lines are not delayed by
    ;; the helper's stdio block buffering when its stdout is a pipe.
    (when (executable-find "stdbuf")
      (setq cmd (append (list "stdbuf" "-oL" "-eL") cmd)))
    (make-process
     :name (format "clatter-%s" network-id)
     :command cmd
     :coding '(utf-8 . utf-8)
     :connection-type 'pipe
     :noquery t
     :filter #'clatter--process-filter
     :sentinel #'clatter--process-sentinel
     :stderr (get-buffer-create (format " *clatter-tls-%s*" network-id)))))

(defun clatter--connect-external (conn config network-id server port)
  "Connect CONN to SERVER:PORT via an external TLS subprocess.
CONFIG is the network plist; NETWORK-ID names the connection.
Begins IRC registration once the subprocess is spawned."
  (let ((proc (clatter--make-external-tls-process
               network-id server port (plist-get config :client-cert))))
    (setf (clatter-connection-process conn) proc)
    (process-put proc :clatter-network-id network-id)
    (clatter--watchdog "EXT-TLS-SPAWN %s via %s"
                       network-id clatter-tls-external-command)
    ;; The subprocess performs the TLS handshake; any plaintext we write
    ;; is queued in the pipe and flushed once the tunnel is up, so it is
    ;; safe to begin IRC registration immediately.
    (clatter--begin-registration conn config)
    ;; Readiness watchdog: if not fully connected (001 received) within
    ;; 30s, tear down the subprocess and let reconnect logic take over.
    (let ((wp proc))
      (run-at-time 30 nil
                   (lambda ()
                     (when (and (process-live-p wp)
                                (not (eq (clatter-connection-state conn) :connected)))
                       (clatter--watchdog "EXT-WATCHDOG-KILL %s (not connected in 30s)"
                                          network-id)
                       (delete-process wp)))))
    conn))

(defun clatter--connect-abort (proc reason)
  "Report REASON and delete in-progress PROC.
Deleting PROC triggers its sentinel, which sets the disconnected state and
schedules any reconnect, so callers must not duplicate that work here."
  (message "[clatter] %s" reason)
  (when (process-live-p proc)
    (delete-process proc)))

(defun clatter--after-tcp-open (conn proc config server use-tls)
  "Continue connecting CONN once the TCP/tunnel to SERVER is open on PROC.
Negotiate TLS when USE-TLS, then begin IRC registration.  CONFIG is the merged
network configuration."
  (let ((network-id (clatter-connection-network-id conn))
        (ok t))
    (when use-tls
      (clatter--watchdog "TLS-START %s" network-id)
      (condition-case tls-err
          (with-timeout (10
                         (clatter--watchdog "TLS-TIMEOUT %s" network-id)
                         (error "TLS handshake timed out (10s)"))
            (gnutls-negotiate
             :process proc
             :hostname server
             :keylist (let ((client-cert (plist-get config :client-cert)))
                        (when (and client-cert (file-exists-p client-cert))
                          (list (list client-cert client-cert))))))
        (error
         (setq ok nil)
         (clatter--watchdog "TLS-FAIL %s %s" network-id
                            (error-message-string tls-err))
         (clatter--connect-abort
          proc (format "TLS failed: %s" (error-message-string tls-err))))))
    (when (and ok (process-live-p proc))
      (clatter--watchdog "TLS-OK %s" network-id)
      (set-process-sentinel proc #'clatter--process-sentinel)
      (clatter--begin-registration conn config))))

;; --- Connect ---

(defun clatter-connect (network-id &rest args)
  "Connect to IRC network NETWORK-ID.
ARGS are keyword arguments that override `clatter-networks' config:
  :server :port :tls :nick :username :realname :sasl :client-cert
  :autojoin :password :bouncer"
  (interactive
   (list (completing-read "Network: "
                          (mapcar #'car clatter-networks))))
  ;; Merge args with stored config
  (let* ((stored (cdr (assoc network-id clatter-networks #'equal)))
         (config (if args (append args stored) stored))
         (server (plist-get config :server))
         (port (or (plist-get config :port) clatter-default-port))
         (use-tls (if (plist-member config :tls)
                      (plist-get config :tls)
                    clatter-default-tls))
         (nick (or (plist-get config :nick) clatter-default-nick))
         (proxy (clatter-proxy-config config)))
    (unless server
      (error "No server specified for network %s" network-id))
    (unless nick
      (error "No nick specified for network %s" network-id))

    ;; STS policy enforcement: force TLS if cached policy exists
    (when (and (fboundp 'clatter-sts-lookup)
               (not use-tls))
      (let ((sts-policy (clatter-sts-lookup server)))
        (when sts-policy
          (setq use-tls t)
          (setq port (plist-get sts-policy :port))
          (clatter--debug "STS: enforcing TLS on port %d for %s" port server))))

    (when (and proxy (not (and (plist-get proxy :host) (plist-get proxy :port))))
      (error "Invalid SOCKS proxy: both :host and :port are required"))

    (when (and proxy use-tls (eq clatter-tls-method 'external))
      (error "SOCKS proxy requires builtin TLS (set `clatter-tls-method' to \\='builtin)"))

    ;; Create or reuse connection struct
    (let ((conn (or (clatter-get-connection network-id)
                    (let ((new-conn (clatter-connection--create
                                     :network-id network-id
                                     :config config
                                     :connect-overrides args
                                     :state :disconnected
                                     :nick nick
                                     :reconnect-enabled t
                                     :reconnect-attempts 0
                                     :active-batches (make-hash-table :test 'equal)
                                     :pending-labels (make-hash-table :test 'equal)
                                     :label-counter 0)))
                      (puthash network-id new-conn clatter-connections)
                      new-conn))))

      ;; Disconnect existing connection if any
      (when (clatter-connection-process conn)
        (delete-process (clatter-connection-process conn)))

      ;; Cancel any pending reconnect
      (when (clatter-connection-reconnect-timer conn)
        (cancel-timer (clatter-connection-reconnect-timer conn))
        (setf (clatter-connection-reconnect-timer conn) nil))

      (setf (clatter-connection-state conn) :connecting)
      ;; Keep the resolved config after the process is gone so reconnects
      ;; retain temporary `clatter-connect' keyword overrides (notably
      ;; `:bouncer').
      (setf (clatter-connection-config conn) config)
      (setf (clatter-connection-connect-overrides conn) args)
      (setf (clatter-connection-nick conn) nick)
      (setf (clatter-connection-desired-nick conn) nick)
      (setf (clatter-connection-recv-buffer conn) (decode-coding-string "" 'utf-8))
      (setf (clatter-connection-cap-enabled conn) nil)
      (setf (clatter-connection-cap-string conn) nil)
      (setf (clatter-connection-sasl-state conn) nil)
      (setf (clatter-connection-ping-sent-time conn) nil)
      (clrhash (clatter-connection-active-batches conn))
      (clrhash (clatter-connection-pending-labels conn))

      (message "[clatter] Connecting to %s:%d%s..."
               server port (if use-tls " (TLS)" ""))
      (clatter--watchdog "CONNECT %s %s:%d tls=%s" network-id server port use-tls)

      (condition-case err
          (if (and use-tls (eq clatter-tls-method 'external))
              ;; External TLS subprocess path: TLS runs in a separate OS
              ;; process so a dead socket cannot block Emacs's event loop.
              (clatter--connect-external conn config network-id server port)
          (let* (;; Always connect plain+nowait first - TLS handshake
                 ;; happens async in the sentinel to avoid blocking Emacs
                 (proc (make-network-process
                        :name (format "clatter-%s" network-id)
                        :host (if proxy (plist-get proxy :host) server)
                        :service (if proxy (plist-get proxy :port) port)
                        :nowait t
                        :coding (if proxy '(binary . binary) '(utf-8 . utf-8))
                        :filter #'clatter--process-filter
                        :sentinel
                        (lambda (p e)
                          (clatter--watchdog "SENTINEL %s event=%s status=%s"
                                             network-id (string-trim e)
                                             (process-status p))
                          (cond
                           ;; TCP connected - run SOCKS handshake or go direct
                           ((string-match-p "open" e)
                            (if proxy
                                (clatter-socks-begin
                                 p server port proxy
                                 (lambda ()
                                   (set-process-coding-system p 'utf-8 'utf-8)
                                   (set-process-filter p #'clatter--process-filter)
                                   (clatter--watchdog "SOCKS-OK %s" network-id)
                                   (clatter--after-tcp-open conn p config server use-tls))
                                 (lambda (reason)
                                   (clatter--watchdog "SOCKS-FAIL %s %s" network-id reason)
                                   (clatter--connect-abort
                                    p (format "SOCKS5: %s" reason))))
                              (clatter--after-tcp-open conn p config server use-tls)))
                           ;; Connection failed or closed
                           (t
                            (clatter--process-sentinel p e)))))))
            (setf (clatter-connection-process conn) proc)
            (process-put proc :clatter-network-id network-id)
            ;; Watchdog: kill process if still connecting after 15 seconds
            (let ((watchdog-proc proc))
              (run-at-time 15 nil
                           (lambda ()
                             (when (and (process-live-p watchdog-proc)
                                        (eq (clatter-connection-state conn) :connecting))
                               (clatter--watchdog "WATCHDOG-KILL %s (stuck connecting)" network-id)
                               (delete-process watchdog-proc)))))
            conn))
        (error
         (clatter--watchdog "CONNECT-FAIL %s %s" network-id (error-message-string err))
         (setf (clatter-connection-state conn) :disconnected)
         (message "[clatter] Connection failed: %s" (error-message-string err))
         nil)))))

;; --- Registration ---

(defun clatter--begin-registration (conn config)
  "Begin IRC registration sequence on CONN with CONFIG.
Always starts with CAP LS 302 for IRCv3 negotiation."
  (clatter--watchdog "REGISTER-START %s" (clatter-connection-network-id conn))
  (setf (clatter-connection-state conn) :registering)
  (setf (clatter-connection-cap-negotiating conn) t)
  (setf (clatter-connection-last-activity conn) (float-time))
  ;; Start health monitor
  (clatter--start-health-timer conn)
  ;; Store config on connection for CAP handler to reference
  (process-put (clatter-connection-process conn) :clatter-config config)
  ;; Always start with CAP negotiation
  (clatter-send conn "CAP LS 302"))

;; --- Disconnect ---

(defun clatter-disconnect (network-id &optional message)
  "Disconnect from NETWORK-ID with optional quit MESSAGE."
  (interactive
   (list (completing-read "Disconnect: "
                          (cl-loop for k being the hash-keys of clatter-connections
                                   collect k))))
  (let ((conn (clatter-get-connection network-id)))
    (when conn
      (setf (clatter-connection-reconnect-enabled conn) nil)
      (when (clatter-connection-reconnect-timer conn)
        (cancel-timer (clatter-connection-reconnect-timer conn)))
      (when (clatter-connection-nick-reclaim-timer conn)
        (cancel-timer (clatter-connection-nick-reclaim-timer conn))
        (setf (clatter-connection-nick-reclaim-timer conn) nil))
      (let ((proc (clatter-connection-process conn)))
        (when (and proc (process-live-p proc))
          (clatter-send conn (clatter-irc-quit (or message clatter-quit-message)))
          ;; Give the QUIT time to send, then delete async
          (run-at-time 0.5 nil
                       (lambda ()
                         (when (process-live-p proc)
                           (delete-process proc))))))
      (setf (clatter-connection-state conn) :disconnected)
      (message "[clatter] Disconnected from %s" network-id))))

(defun clatter-disconnect-all (&optional message)
  "Disconnect from all connected networks with optional quit MESSAGE.
Intended for use on Emacs exit: sends QUIT synchronously and deletes
each process so no orphaned ghost sessions remain on the servers."
  (let ((quit-msg (or message clatter-quit-message)))
    (maphash
     (lambda (_network-id conn)
       (setf (clatter-connection-reconnect-enabled conn) nil)
       (when (clatter-connection-reconnect-timer conn)
         (cancel-timer (clatter-connection-reconnect-timer conn))
         (setf (clatter-connection-reconnect-timer conn) nil))
       (when (clatter-connection-nick-reclaim-timer conn)
         (cancel-timer (clatter-connection-nick-reclaim-timer conn))
         (setf (clatter-connection-nick-reclaim-timer conn) nil))
       (let ((proc (clatter-connection-process conn)))
         (when (and proc (process-live-p proc)
                    (memq (process-status proc) '(open run)))
           (clatter--watchdog "QUIT-ON-EXIT %s"
                              (clatter-connection-network-id conn))
           ;; Send QUIT synchronously.  We cannot rely on async timers
           ;; here because Emacs is exiting.
           (ignore-errors
             (with-local-quit
               (process-send-string proc
                                    (concat (clatter-irc-quit quit-msg) "\r\n"))
               ;; Briefly let the QUIT flush to the socket.
               (accept-process-output proc 0.3)))
           (ignore-errors (delete-process proc)))
         (setf (clatter-connection-process conn) nil)
         (setf (clatter-connection-state conn) :disconnected)))
     clatter-connections)))

(defun clatter--quit-on-exit ()
  "Cleanly disconnect all networks when Emacs exits.
Added to `kill-emacs-hook'.  No-op unless `clatter-quit-on-exit'."
  (when clatter-quit-on-exit
    (clatter-disconnect-all)))

;; `clatter--quit-on-exit' is installed on `kill-emacs-hook' by
;; `clatter-setup', so that merely loading clatter adds no global hooks.

;; --- Reconnection ---

(defun clatter--schedule-reconnect (conn)
  "Schedule a reconnection attempt for CONN with exponential backoff.
If the last disconnect was a recent services nick-regain kill, the
delay is raised to at least `clatter-regain-kill-backoff' seconds to
avoid an immediate re-collision and another kill."
  (unless (eq (clatter-connection-state conn) :connecting)
    (let* ((attempts (clatter-connection-reconnect-attempts conn))
           (delay (min (* clatter-reconnect-initial-delay (expt 2 attempts))
                       clatter-reconnect-max-delay))
           (kill-time (clatter-connection-regain-kill-time conn))
           (recent-regain-kill (and kill-time
                                    (< (- (float-time) kill-time) 30)))
           (delay (if recent-regain-kill
                      (max delay clatter-regain-kill-backoff)
                    delay))
           (network-id (clatter-connection-network-id conn))
           (overrides (clatter-connection-connect-overrides conn)))
      (when recent-regain-kill
        (clatter--watchdog "RECONNECT-BACKOFF %s regain-kill-count=%d delay=%ds"
                           network-id
                           (or (clatter-connection-regain-kill-count conn) 0)
                           delay))
      (message "[clatter] Reconnecting to %s in %ds (attempt %d)..."
               network-id delay (1+ attempts))
      (run-hook-with-args 'clatter-reconnect-hook network-id delay (1+ attempts))
      (setf (clatter-connection-reconnect-attempts conn) (1+ attempts))
      (setf (clatter-connection-reconnect-timer conn)
            (run-at-time delay nil
                         (lambda ()
                           (setf (clatter-connection-reconnect-timer conn) nil)
                           (apply #'clatter-connect network-id overrides)))))))

;; --- Health Monitoring ---

(defun clatter--start-health-timer (conn)
  "Start periodic health check timer for CONN."
  (when (clatter-connection-health-timer conn)
    (cancel-timer (clatter-connection-health-timer conn)))
  (setf (clatter-connection-health-timer conn)
        (run-at-time clatter-ping-interval clatter-ping-interval
                     #'clatter--health-check conn)))

(defun clatter--health-check (conn)
  "Check connection health for CONN.
Like ERC, uses a single timeout on last-received-time:
- If no data received for `clatter-ping-timeout' seconds, kill the
  connection (it is dead).
- Otherwise, send a PING to keep the connection alive and provoke
  a PONG that resets the receive timer.
PINGs are only sent when state is :connected (after 001)."
  (let* ((proc (clatter-connection-process conn))
         (network-id (clatter-connection-network-id conn)))
    (cond
     ;; Process gone -- cancel our own timer
     ((not (and proc (process-live-p proc)
                 (memq (process-status proc) '(open run))))
      (clatter--watchdog "HEALTH-STALE %s (process gone)" network-id)
      (when (clatter-connection-health-timer conn)
        (cancel-timer (clatter-connection-health-timer conn))
        (setf (clatter-connection-health-timer conn) nil)))
     ;; Not yet registered -- don't send PINGs, let watchdog handle stuck state
     ((not (eq (clatter-connection-state conn) :connected))
      nil)
     ;; Connected -- check health
     (t
      (let ((since-recv (- (float-time)
                           (or (clatter-connection-last-activity conn) 0))))
        (cond
         ;; No data received for too long -- connection is dead
         ((> since-recv clatter-ping-timeout)
          (clatter--watchdog "HEALTH-KILL %s (no data for %.0fs, timeout %ds)"
                             network-id since-recv clatter-ping-timeout)
          (message "[clatter] No data from %s for %.0fs, killing connection"
                   network-id since-recv)
          (setf (clatter-connection-ping-sent-time conn) nil)
          (delete-process proc))
         ;; Send keepalive PING
         (t
          (clatter--watchdog "HEALTH-PING %s (last-recv %.0fs ago)"
                             network-id since-recv)
          (setf (clatter-connection-ping-sent-time conn) (float-time))
          (clatter-send conn (clatter-irc-ping "clatter")))))))))

;; --- Hooks ---

(defvar clatter-connect-hook nil
  "Hook run after successfully connecting (001 received).
Called with NETWORK-ID.")

(defvar clatter-disconnect-hook nil
  "Hook run after disconnecting.
Called with NETWORK-ID and EVENT string.")

(defvar clatter-reconnect-hook nil
  "Hook run when scheduling a reconnection attempt.
Called with NETWORK-ID, DELAY (seconds), and ATTEMPT (integer).")

;; --- Labeled Responses ---

(defun clatter-generate-label (conn)
  "Generate a unique label for CONN."
  (format "clatter%d" (cl-incf (clatter-connection-label-counter conn))))

(defun clatter-send-labeled (conn command &optional callback)
  "Send COMMAND on CONN with a label tag for labeled-response.
CALLBACK is called with the response message when received.
Returns the label used, or nil if labeled-response not available."
  (if (member "labeled-response" (clatter-connection-cap-enabled conn))
      (let ((label (clatter-generate-label conn)))
        (when callback
          (puthash label callback (clatter-connection-pending-labels conn)))
        (clatter-send conn (format "@label=%s %s" label command))
        label)
    (progn
      (clatter-send conn command)
      nil)))

(provide 'clatter-connection)

;;; clatter-connection.el ends here
