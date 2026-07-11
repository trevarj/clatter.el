;;; clatter-handlers.el --- IRC message dispatch and handling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; IRC message dispatch for clatter.el.
;; Routes parsed messages to appropriate handlers.
;; Ported from CLatter's net/irc.lisp irc-handle-message.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-cap)
(require 'clatter-list)

;; --- Event hooks for UI layer ---

(defvar clatter-privmsg-hook nil
  "Hook for PRIVMSG events.
Called with (CONN SENDER TARGET TEXT SERVER-TIME).")

(defvar clatter-notice-hook nil
  "Hook for NOTICE events.
Called with (CONN SENDER TARGET TEXT SERVER-TIME).")

(defvar clatter-join-hook nil
  "Hook for JOIN events.
Called with (CONN NICK CHANNEL ACCOUNT REALNAME).")

(defvar clatter-part-hook nil
  "Hook for PART events.
Called with (CONN NICK CHANNEL MESSAGE).")

(defvar clatter-quit-hook nil
  "Hook for QUIT events.
Called with (CONN NICK MESSAGE).")

(defvar clatter-nick-hook nil
  "Hook for NICK change events.
Called with (CONN OLD-NICK NEW-NICK).")

(defvar clatter-irc-mode-hook nil
  "Hook for MODE events.
Called with (CONN TARGET SETTER MODES).")

(defvar clatter-topic-hook nil
  "Hook for TOPIC events.
Called with (CONN CHANNEL NICK TOPIC AT).")

(defvar clatter-kick-hook nil
  "Hook for KICK events.
Called with (CONN CHANNEL NICK KICKED-NICK REASON).")

(defvar clatter-away-hook nil
  "Hook for AWAY events (IRCv3 away-notify).
Called with (CONN NICK AWAY-MSG).")

(defvar clatter-typing-hook nil
  "Hook for typing indicator events.
Called with (CONN NICK TARGET STATE).")

(defvar clatter-react-hook nil
  "Hook for reaction events (draft/react).
Called with (CONN NICK TARGET EMOJI MSGID).")

(defvar clatter-names-hook nil
  "Hook for NAMES reply events.
Called with (CONN CHANNEL NAMES-STRING).")

(defvar clatter-numeric-hook nil
  "Hook for unhandled numeric replies.
Called with (CONN NUMERIC PARAMS).")

(defvar clatter-whois-hook nil
  "Hook for WHOIS reply events.
Called with (CONN NICK INFO-ALIST).
INFO-ALIST keys: :user :host :realname :server :server-info
:channels :idle :signon :account :secure :away.")

(defvar clatter-motd-hook nil
  "Hook for MOTD display.
Called with (CONN MOTD-LINES) where MOTD-LINES is a list of strings.")

(defvar clatter-system-hook nil
  "Hook for system/log messages.
Called with (CONN TEXT).")

(defvar clatter-ctcp-hook nil
  "Hook for CTCP events (non-ACTION).
Called with (CONN SENDER TARGET COMMAND ARGS).")

(defvar clatter-ctcp-reply-hook nil
  "Hook for CTCP replies received via NOTICE.
Called with (CONN SENDER COMMAND REPLY-TEXT).")

(defvar clatter-action-hook nil
  "Hook for CTCP ACTION (/me) events.
Called with (CONN SENDER TARGET TEXT SERVER-TIME).")

(defvar clatter-welcome-hook nil
  "Hook for 001 RPL_WELCOME.
Called with (CONN NICK).")

(defvar clatter-batch-complete-hook nil
  "Hook for completed batch delivery.
Called with (CONN BATCH-TYPE TARGET MESSAGES).")

(defvar clatter-invite-hook nil
  "Hook for INVITE events.
Called with (CONN SENDER NICK CHANNEL).")

;; --- Sent message tracking --

(defvar clatter-sent (make-hash-table :test #'equal)
  "Sent messages, keyed by network ID.")

(defconst clatter-sent-tracking-limit 1024
  "Fallback number of sent message IDs to remember per network.
Used when `clatter-buffer-max-lines' is nil (buffer truncation disabled),
so sent-message tracking still has a bounded ring size.")

(defun clatter-sent-add (network msgid)
  "Record MSGID as a sent message within NETWORK."
  (let* ((messages-and-ring (or (gethash network clatter-sent)
                                (puthash network (cons (make-hash-table :test #'equal)
                                                       (make-ring (or clatter-buffer-max-lines
                                                                      clatter-sent-tracking-limit)))
                                         clatter-sent)))
         (messages (car messages-and-ring))
         (inserted (cdr messages-and-ring)))
    ;; Evict older entries
    (unless (gethash msgid messages)
      (when (= (ring-length inserted) (ring-size inserted))
        (remhash (ring-ref inserted 0) messages))
      (ring-insert-at-beginning inserted msgid)
      (puthash msgid t messages))))

(defun clatter-sent-p (network msgid)
  "Returns whether we sent MSGID to NETWORK."
  (let ((messages-and-ring (gethash network clatter-sent)))
    (and messages-and-ring (gethash msgid (car messages-and-ring)))))

;; --- Main Dispatch ---

(defmacro clatter--set-unprefixed (target prefixes)
  "Trim TARGET's prefix character if within PREFIXES.
Return the trimmed character."
  `(when (and ,prefixes
              (> (length ,target) 1)
              (seq-contains-p ,prefixes (aref ,target 0)))
     (prog1 (string (aref ,target 0))
       (setq ,target (substring ,target 1)))))

(defun clatter--prepend-status-prefix (status-prefix text)
  "Prepend a human-readable description of STATUS-PREFIX to TEXT."
  (if status-prefix
      (let ((label (cond ((string= status-prefix "@") "[ops]")
                         ((string= status-prefix "+") "[voiced]")
                         (t (format "[%s]" status-prefix)))))
        (concat (propertize label 'face 'clatter-notice) " " text))
    text))

(defun clatter-dispatch-message (conn msg)
  "Dispatch parsed MSG on CONN to the appropriate handler."
  (let ((command (clatter-message-command msg))
        (params (clatter-message-params msg))
        (prefix (clatter-message-prefix msg))
        (tags (clatter-message-tags msg)))
    ;; Check for labeled-response
    (let* ((parsed-tags (clatter-parse-tags tags))
           (label (cdr (assoc "label" parsed-tags))))
      (when label
        (clatter--handle-labeled-response conn label msg)))
    (pcase command
      ;; --- Core protocol ---
      ("PING"
       (setf (clatter-connection-last-activity conn) (float-time))
       (clatter-send conn (clatter-irc-pong (or (car params) ""))))

      ("PONG"
       (setf (clatter-connection-last-activity conn) (float-time))
       (setf (clatter-connection-ping-sent-time conn) nil))

      ;; --- CAP / SASL ---
      ("CAP"
       (clatter-cap-handle conn params))

      ("AUTHENTICATE"
       (clatter-cap-handle-authenticate conn params))

      ("903"  ; RPL_SASLSUCCESS
       (clatter-cap-handle-sasl-success conn))

      ((or "904" "905" "906")  ; SASL failures / aborted
       (clatter-cap-handle-sasl-failure conn params))

      ;; --- Standard Replies (IRCv3) ---
      ("FAIL"
       (let* ((cmd (nth 0 params))
              (code (nth 1 params))
              (description (car (last params)))
              (context (when (> (length params) 3)
                         (string-join (cl-subseq params 2 (1- (length params))) " "))))
         (run-hook-with-args 'clatter-system-hook conn
                             (propertize
                              (if context
                                  (format "FAIL [%s/%s] %s: %s" cmd code context description)
                                (format "FAIL [%s/%s] %s" cmd code description))
                              'face 'clatter-error))))

      ("WARN"
       (let* ((cmd (nth 0 params))
              (code (nth 1 params))
              (description (car (last params)))
              (context (when (> (length params) 3)
                         (string-join (cl-subseq params 2 (1- (length params))) " "))))
         (run-hook-with-args 'clatter-system-hook conn
                             (propertize
                              (if context
                                  (format "WARN [%s/%s] %s: %s" cmd code context description)
                                (format "WARN [%s/%s] %s" cmd code description))
                              'face 'clatter-notice))))

      ("NOTE"
       (let* ((cmd (nth 0 params))
              (code (nth 1 params))
              (description (car (last params)))
              (context (when (> (length params) 3)
                         (string-join (cl-subseq params 2 (1- (length params))) " "))))
         (run-hook-with-args 'clatter-system-hook conn
                             (if context
                                 (format "NOTE [%s/%s] %s: %s" cmd code context description)
                               (format "NOTE [%s/%s] %s" cmd code description)))))

      ;; --- Server ERROR (sent before forced disconnect) ---
      ("ERROR"
       (let ((reason (or (car params) "Unknown")))
         (clatter--watchdog "SERVER-ERROR %s %s"
                            (clatter-connection-network-id conn) reason)
         ;; Detect a services nick-regain kill so the reconnect logic
         ;; can back off and avoid an immediate re-collision loop.
         (when (string-match-p "regained by services" reason)
           (setf (clatter-connection-regain-kill-time conn) (float-time))
           (setf (clatter-connection-regain-kill-count conn)
                 (1+ (or (clatter-connection-regain-kill-count conn) 0)))
           (clatter--watchdog "REGAIN-KILL %s count=%d"
                              (clatter-connection-network-id conn)
                              (clatter-connection-regain-kill-count conn)))
         (message "[clatter] Server ERROR: %s" reason)
         (run-hook-with-args 'clatter-system-hook conn
                             (format "ERROR: %s" reason))))

      ;; --- Registration complete ---
      ("001"
       (clatter--watchdog "REGISTERED %s nick=%s"
                          (clatter-connection-network-id conn)
                          (clatter-connection-nick conn))
       (setf (clatter-connection-state conn) :connected)
       ;; Reset reconnect attempts after 60s of stable connection
       (let ((this-conn conn))
         (run-at-time 60 nil
                      (lambda ()
                        (when (eq (clatter-connection-state this-conn) :connected)
                          (setf (clatter-connection-reconnect-attempts this-conn) 0)
                          (setf (clatter-connection-regain-kill-count this-conn) 0)
                          (setf (clatter-connection-regain-kill-time this-conn) nil)))))
       (let* ((process (clatter-connection-process conn))
              ;; The process plist contains keyword overrides supplied to
              ;; `clatter-connect'.  Fall back to the saved network config
              ;; for tests and for connections predating that plist.
              (config (or (and process
                               (process-get process :clatter-config))
                          (cdr (assoc (clatter-connection-network-id conn)
                                      clatter-networks #'equal))))
              (bouncer-p (plist-get config :bouncer)))
         ;; NickServ identify if not using SASL
         (let ((password (clatter-get-password
                          (clatter-connection-network-id conn) config)))
           (when (and clatter-auto-identify
                      (not bouncer-p)
                      password
                      (not (eq (clatter-connection-sasl-state conn) :done)))
             (clatter-send conn (clatter-irc-privmsg
                                 "NickServ"
                                 (format "IDENTIFY %s" password)))))
         ;; Autojoin - stagger joins to avoid overwhelming Emacs with
         ;; NAMES/WHOX responses for all channels at once
         (let ((channels (plist-get config :autojoin))
               (delay 0))
           (dolist (ch channels)
             (if (zerop delay)
                 (clatter-send conn (clatter-irc-join ch))
               (let ((channel ch))
                 (run-at-time delay nil
                              (lambda ()
                                (when (eq (clatter-connection-state conn) :connected)
                                  (clatter-send conn (clatter-irc-join channel)))))))
             (setq delay (+ delay 2)))))
       (run-hook-with-args 'clatter-welcome-hook conn (clatter-connection-nick conn))
       (run-hook-with-args 'clatter-connect-hook (clatter-connection-network-id conn))
       (message "[clatter] Connected to %s as %s"
                (clatter-connection-network-id conn)
                (clatter-connection-nick conn))
       ;; Start nick reclaim if we ended up with a fallback nick
       (clatter--maybe-start-nick-reclaim conn))

      ;; --- 005 RPL_ISUPPORT ---
      ("005"
       (unless (clatter-connection-isupport conn)
         (setf (clatter-connection-isupport conn)
               (make-hash-table :test 'equal)))
       (let ((isup (clatter-connection-isupport conn)))
         (dolist (param (cdr params))  ; skip first (nick) and last (:are supported...)
           (unless (string-prefix-p ":" param)
             (let ((eq-pos (cl-position ?= param)))
               (if eq-pos
                   (puthash (upcase (substring param 0 eq-pos))
                            (substring param (1+ eq-pos))
                            isup)
                 (puthash (upcase param) t isup)))))))

      ;; --- Banned ---
      ("465"
       (setf (clatter-connection-reconnect-enabled conn) nil)
       (let ((reason (or (car (last params)) "No reason given")))
         (message "[clatter] BANNED from %s: %s (auto-reconnect disabled)"
                  (clatter-connection-network-id conn) reason)
         (run-hook-with-args 'clatter-system-hook conn
                             (format "BANNED: %s" reason))))

      ;; --- Nick in use ---
      ("433"
       (if (eq (clatter-connection-state conn) :connected)
           ;; Already connected - this is a failed reclaim attempt, just log it
           (run-hook-with-args 'clatter-system-hook conn
                               (format "Nick %s still in use, will retry"
                                       (clatter-connection-desired-nick conn)))
         ;; During registration - append _ and retry
         (let ((new-nick (concat (clatter-connection-nick conn) "_")))
           (setf (clatter-connection-nick conn) new-nick)
           (clatter-send conn (clatter-irc-nick new-nick))
           (run-hook-with-args 'clatter-system-hook conn
                               (format "Nick in use, trying %s" new-nick)))))

      ;; --- PRIVMSG ---
      ("PRIVMSG"
       (let* ((target (nth 0 params))
              (raw-text (nth 1 params))
              (parsed-tags (clatter-parse-tags tags))
              ;; --- >> EXTRACT MESSAGE TAGS ---
              (server-time (clatter-get-parsed-server-time parsed-tags))
              (msgid (clatter-get-parsed-tag parsed-tags "msgid"))
              (is-bot (assoc "bot" parsed-tags))
              (reply-to (or (clatter-get-parsed-tag parsed-tags "+draft/reply")
                            (clatter-get-parsed-tag parsed-tags "+reply")
                            (clatter-get-parsed-tag parsed-tags "draft/reply")))
              ;; --- << EXTRACT MESSAGE TAGS ---
              (is-reply-to-me (and reply-to
                                   (clatter-sent-p
                                    (clatter-connection-network-id conn) reply-to)))
              (parsed-prefix (clatter-parse-prefix prefix))
              (sender-nick (clatter-prefix-nick parsed-prefix))
              ;; STATUSMSG: detect prefix like @#channel or +#channel and strip
              ;; it from target
              (statusmsg-chars (let ((isup (clatter-connection-isupport conn)))
                                 (when isup (gethash "STATUSMSG" isup))))
              (status-prefix (clatter--set-unprefixed target statusmsg-chars)))
         ;; Record sent message
         (when (and msgid
                    (string-equal-ignore-case (clatter-connection-nick conn) sender-nick))
           (clatter-sent-add (clatter-connection-network-id conn) msgid))
         ;; CTCP
         (if (and (> (length raw-text) 1)
                  (= (aref raw-text 0) 1)
                  (= (aref raw-text (1- (length raw-text))) 1))
             (clatter--handle-ctcp conn parsed-prefix target raw-text status-prefix
                                   msgid server-time is-bot reply-to is-reply-to-me)
           (let* ((text (clatter--prepend-status-prefix status-prefix raw-text))
                  (batch-id (clatter-get-parsed-tag parsed-tags "batch-id")))
             ;; Mark sender as bot if draft/bot tag present
             (when is-bot
               (setq sender-nick (propertize sender-nick 'clatter-bot t))
               (setf (car parsed-prefix) sender-nick))
             ;; Attach msgid and reply-to as text properties
             (when msgid
               (setq text (propertize text 'clatter-msgid msgid)))
             (when reply-to
               (setq text (propertize text 'clatter-reply-to reply-to)))
             (when is-reply-to-me
               (setq text (propertize text 'clatter-reply-to-me t)))
             (cond
              ;; Batched message
              (batch-id
               (clatter--accumulate-batch conn batch-id sender-nick text server-time))
              ;; Normal message
              (t
               (run-hook-with-args 'clatter-privmsg-hook
                                   conn parsed-prefix target text server-time)))))))

      ;; --- NOTICE ---
      ("NOTICE"
       (let ((raw-text (nth 1 params))
             (parsed-prefix (clatter-parse-prefix prefix)))
         ;; Check for CTCP reply in NOTICE (don't respond)
         (if (and (> (length raw-text) 1)
                  (= (aref raw-text 0) 1)
                  (= (aref raw-text (1- (length raw-text))) 1))
             (let* ((ctcp-content (substring raw-text 1 (1- (length raw-text))))
                    (space-pos (cl-position ?\s ctcp-content))
                    (ctcp-cmd (if space-pos (substring ctcp-content 0 space-pos) ctcp-content))
                    (ctcp-args (if space-pos (substring ctcp-content (1+ space-pos)) "")))
               (run-hook-with-args 'clatter-ctcp-reply-hook
                                   conn parsed-prefix ctcp-cmd ctcp-args))
           (let* ((sender-nick (clatter-prefix-nick parsed-prefix))
                  (parsed-tags (clatter-parse-tags tags))
                  ;; --- >> EXTRACT MESSAGE TAGS ---
                  (server-time (clatter-get-parsed-server-time parsed-tags))
                  (msgid (clatter-get-parsed-tag parsed-tags "msgid"))
                  (is-bot (assoc "bot" parsed-tags))
                  (reply-to (or (clatter-get-parsed-tag parsed-tags "+draft/reply")
                                (clatter-get-parsed-tag parsed-tags "+reply")
                                (clatter-get-parsed-tag parsed-tags "draft/reply")))
                  ;; --- << EXTRACT MESSAGE TAGS ---
                  (is-reply-to-me (and reply-to
                                       (clatter-sent-p
                                        (clatter-connection-network-id conn) reply-to)))
                  (target (nth 0 params))
                  ;; STATUSMSG: detect prefix like @#channel or +#channel and strip
                  ;; it from target
                  (statusmsg-chars (let ((isup (clatter-connection-isupport conn)))
                                     (when isup (gethash "STATUSMSG" isup))))
                  (status-prefix (clatter--set-unprefixed target statusmsg-chars))
                  (text (clatter--prepend-status-prefix status-prefix raw-text)))
             ;; Mark sender as bot if draft/bot tag present
             (when is-bot
               (setq sender-nick (propertize sender-nick 'clatter-bot t))
               (setf (car parsed-prefix) sender-nick))
             ;; Attach msgid and reply-to as text properties
             (when msgid
               (setq text (propertize text 'clatter-msgid msgid)))
             (when reply-to
               (setq text (propertize text 'clatter-reply-to reply-to)))
             (when is-reply-to-me
               (setq text (propertize text 'clatter-reply-to-me t)))
             (run-hook-with-args 'clatter-notice-hook
                                 conn parsed-prefix target text server-time)))))

      ;; --- INVITE ---
      ("INVITE"
       (let* ((nick (nth 0 params))
              (channel (nth 1 params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-invite-hook conn parsed-prefix nick channel)))

      ;; --- JOIN ---
      ("JOIN"
       (let* ((channel (nth 0 params))
              (account (nth 1 params))
              (realname (nth 2 params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-join-hook
                             conn parsed-prefix channel account realname)))

      ;; --- PART ---
      ("PART"
       (let* ((channel (nth 0 params))
              (message (nth 1 params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-part-hook conn parsed-prefix channel message)))

      ;; --- QUIT ---
      ("QUIT"
       (let* ((message (nth 0 params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-quit-hook conn parsed-prefix message)))

      ;; --- NICK ---
      ("NICK"
       (let* ((new-nick (nth 0 params))
              (parsed-prefix (clatter-parse-prefix prefix))
              (old-nick (clatter-prefix-nick parsed-prefix)))
         (when (string-equal old-nick (clatter-connection-nick conn))
           (setf (clatter-connection-nick conn) new-nick)
           ;; Stop reclaim timer if we got our desired nick
           (when (and (clatter-connection-desired-nick conn)
                      (string-equal new-nick (clatter-connection-desired-nick conn))
                      (clatter-connection-nick-reclaim-timer conn))
             (cancel-timer (clatter-connection-nick-reclaim-timer conn))
             (setf (clatter-connection-nick-reclaim-timer conn) nil)
             (message "[clatter] Reclaimed nick %s" new-nick)))
         (run-hook-with-args 'clatter-nick-hook conn parsed-prefix new-nick)))

      ;; --- MODE ---
      ("MODE"
       (let* ((target (nth 0 params))
              (modes (cdr params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-irc-mode-hook conn target parsed-prefix modes)))

      ;; --- TOPIC ---
      ("TOPIC"
       (let* ((channel (nth 0 params))
              (topic (nth 1 params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-topic-hook conn channel parsed-prefix topic nil)))

      ;; --- KICK ---
      ("KICK"
       (let* ((channel (nth 0 params))
              (kicked (nth 1 params))
              (reason (nth 2 params))
              (parsed-prefix (clatter-parse-prefix prefix)))
         (run-hook-with-args 'clatter-kick-hook conn channel parsed-prefix kicked reason)))

      ;; --- RENAME (draft/channel-rename) ---
      ("RENAME"
       (let* ((old-channel (nth 0 params))
              (new-channel (nth 1 params))
              (reason (nth 2 params))
              (network (clatter-connection-network-id conn))
              (buf (clatter-get-buffer network old-channel)))
         (when buf
           ;; Update buffer-local target
           (with-current-buffer buf
             (setq clatter--target new-channel)
             (rename-buffer (format "*clatter: %s/%s*" network new-channel) t))
           ;; Update buffer registry: remove old, add new
           (clatter-remove-buffer network old-channel)
           (let ((new-key (cons network (downcase new-channel))))
             (setf (alist-get new-key clatter--buffer-alist nil nil #'equal) buf))
           ;; Notify
           (run-hook-with-args 'clatter-system-hook conn
                               (format "Channel %s renamed to %s%s"
                                       old-channel new-channel
                                       (if reason (format " (%s)" reason) ""))))))

      ;; --- AWAY (IRCv3 away-notify) ---
      ("AWAY"
       (let* ((parsed-prefix (clatter-parse-prefix prefix))
              (away-msg (nth 0 params)))
         (run-hook-with-args 'clatter-away-hook conn parsed-prefix away-msg)))

      ;; --- TAGMSG (typing indicators + reactions) ---
      ("TAGMSG"
       (let* ((parsed-prefix (clatter-parse-prefix prefix))
              (nick (clatter-prefix-nick parsed-prefix))
              (target (nth 0 params))
              (parsed-tags (clatter-parse-tags tags))
              (typing-state (cdr (assoc "+typing" parsed-tags)))
              (react-emoji (or (cdr (assoc "+draft/react" parsed-tags))
                               (cdr (assoc "draft/react" parsed-tags))))
              (react-msgid (or (cdr (assoc "+draft/reply" parsed-tags))
                               (cdr (assoc "+reply" parsed-tags))
                               (cdr (assoc "draft/reply" parsed-tags)))))
         ;; Typing indicator
         (when (and typing-state
                    (not (string-equal nick (clatter-connection-nick conn))))
           (run-hook-with-args 'clatter-typing-hook conn parsed-prefix target typing-state))
         ;; Reaction
         (when (and react-emoji react-msgid)
           (run-hook-with-args 'clatter-react-hook
                               conn parsed-prefix target react-emoji react-msgid))))

      ;; --- MARKREAD (IRCv3 read-marker) ---
      ("MARKREAD"
       (when (fboundp 'clatter-read-marker--handle)
         (clatter-read-marker--handle conn tags params)))

      ;; --- BATCH ---
      ("BATCH"
       (clatter--handle-batch conn tags params))

      ;; --- 353 RPL_NAMREPLY ---
      ("353"
       (let ((channel (nth 2 params))
             (names-str (nth 3 params)))
         (when (and channel names-str)
           (run-hook-with-args 'clatter-names-hook conn channel names-str))))

      ;; --- 366 RPL_ENDOFNAMES ---
      ("366"
       ;; Send WHOX to get account names after NAMES completes
       (let ((channel (nth 1 params)))
         (when (and channel (string-prefix-p "#" channel))
           (clatter-send-whox conn channel))))

      ;; --- 354 RPL_WHOSPCRPL (WHOX reply) ---
      ("354"
       ;; Format: 354 <me> <token> <channel> <nick> <user> <host> <realname> <account> <flags>
       (let ((token (nth 1 params))
             (channel (nth 2 params))
             (nick (nth 3 params))
             (account (nth 7 params)))
         (when (and (string= token clatter-whox-token)
                    nick account
                    (not (string= account "0")))  ; "0" means no account
           (let ((buf (clatter-get-buffer
                       (clatter-connection-network-id conn) channel)))
             (when buf
               (clatter-nick-set-account buf nick account))))))

      ;; --- MOTD numerics ---
      ("375"  ; RPL_MOTDSTART
       (setf (clatter-connection--motd-lines conn) nil))
      ("372"  ; RPL_MOTD
       (push (or (car (last params)) "") (clatter-connection--motd-lines conn)))
      ("376"  ; RPL_ENDOFMOTD
       (let ((lines (nreverse (clatter-connection--motd-lines conn))))
         (run-hook-with-args 'clatter-motd-hook conn lines)
         (setf (clatter-connection--motd-lines conn) nil)))
      ("422"  ; ERR_NOMOTD
       (run-hook-with-args 'clatter-system-hook conn "No MOTD"))

      ;; --- WHOIS numerics ---
      ("311"  ; RPL_WHOISUSER
       (let ((nick (nth 1 params))
             (user (nth 2 params))
             (host (nth 3 params))
             (realname (nth 5 params)))
         (setf (clatter-connection--whois-data conn)
               (list :nick nick :user user :host host :realname realname))))
      ("276"  ; RPL_WHOISCERTFP
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :certfp (nth 2 params)))))
      ("307"  ; RPL_WHOISREGNICK
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :regnick (string-join (cddr params) "")))))
      ("312"  ; RPL_WHOISSERVER
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :server (nth 2 params))
           (plist-put data :server-info (nth 3 params)))))
      ("313"  ; RPL_WHOISOPERATOR
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :oper t))))
      ("319"  ; RPL_WHOISCHANNELS
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :channels (nth 2 params)))))
      ("320"  ; RPL_WHOISSPECIAL
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :special (string-join (cddr params) "")))))
      ("317"  ; RPL_WHOISIDLE
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :idle (nth 2 params))
           (plist-put data :signon (nth 3 params)))))
      ("330"  ; RPL_WHOISACCOUNT
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :account (nth 2 params)))))
      ("335"  ; RPL_WHOISBOT
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :bot t))))
      ("338"  ; RPL_WHOISACTUALLY
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :actually (string-join (reverse (cddr params)) " ")))))
      ("378"  ; RPL_WHOISHOST
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :conn (string-join (cddr params) " ")))))
      ("379"  ; RPL_WHOISMODES
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :modes (string-join (cddr params) " ")))))
      ("671"  ; RPL_WHOISSECURE
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :secure t))))
      ("301"  ; RPL_AWAY
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (plist-put data :away (nth 2 params)))))
      ("318"  ; RPL_ENDOFWHOIS
       (let ((data (clatter-connection--whois-data conn)))
         (when data
           (run-hook-with-args 'clatter-whois-hook conn
                               (plist-get data :nick) data)
           (setf (clatter-connection--whois-data conn) nil))))

      ;; --- LIST numerics ---
      ("322"  ; RPL_LIST
       (let ((channel (nth 1 params))
             (users (nth 2 params))
             (topic (or (nth 3 params) "")))
         (clatter-list--on-entry conn channel users topic)))
      ("323"  ; RPL_LISTEND
       (clatter-list--on-end conn))

      ;; --- TOPIC numerics ---
      ("332"  ; RPL_TOPIC
       (let* ((channel (nth 1 params))
              (topic (nth 2 params))
              (network (clatter-connection-network-id conn))
              (buf (clatter-get-buffer network channel)))
         (clatter-set-topic buf topic)))
      ("333"  ; RPL_TOPICWHOTIME
       (let* ((channel (nth 1 params))
              (sender (clatter-parse-prefix (nth 2 params)))
              (at (when (nth 3 params) (string-to-number (nth 3 params))))
              (network (clatter-connection-network-id conn))
              (buf (clatter-get-buffer network channel))
              (topic (clatter-get-topic buf)))
         (run-hook-with-args 'clatter-topic-hook conn channel sender topic at)))

      ;; --- MONITOR numerics ---
      ("730"  ; RPL_MONONLINE
       (run-hook-with-args 'clatter-system-hook conn
                           (format "Online: %s" (nth 1 params))))
      ("731"  ; RPL_MONOFFLINE
       (run-hook-with-args 'clatter-system-hook conn
                           (format "Offline: %s" (nth 1 params))))
      ("732" nil)  ; RPL_MONLIST (ignore)
      ("733" nil)  ; RPL_ENDOFMONLIST
      ("734"       ; ERR_MONLISTFULL
       (run-hook-with-args 'clatter-system-hook conn
                           (format "Monitor list full (limit: %s)" (nth 1 params))))

      ;; --- Other numerics ---
      (_
       (if (and (> (length command) 0)
                (cl-every #'cl-digit-char-p command))
           (run-hook-with-args 'clatter-numeric-hook conn command params)
         (run-hook-with-args 'clatter-system-hook conn
                             (format "%s %s" command
                                     (string-join params " "))))))))

;; --- CTCP Handling ---

(defun clatter--handle-ctcp (conn
                             sender target raw-text
                             &optional
                             status-prefix
                             msgid server-time is-bot reply-to is-reply-to-me)
  "Handle CTCP request on CONN from SENDER to TARGET with RAW-TEXT, STATUS-PREFIX,
MSGID, SERVER-TIME, IS-BOT, REPLY-TO, and IS-REPLY-TO-ME."
  (let* ((ctcp-content (substring raw-text 1 (1- (length raw-text))))
         (space-pos (cl-position ?\s ctcp-content))
         (ctcp-cmd (upcase (if space-pos
                               (substring ctcp-content 0 space-pos)
                             ctcp-content)))
         (ctcp-args (if space-pos
                        (substring ctcp-content (1+ space-pos))
                      ""))
         (sender-nick (clatter-prefix-nick sender))
         (self-p (string-equal sender-nick (clatter-connection-nick conn))))
    (pcase ctcp-cmd
      ("ACTION"
       (let ((text (clatter--prepend-status-prefix status-prefix ctcp-args)))
         ;; Mark sender as bot if draft/bot tag present
         (when is-bot
           (setq sender-nick (propertize sender-nick 'clatter-bot t))
           (setf (car sender) sender-nick))
         ;; Attach msgid and reply-to as text properties
         (when msgid
           (setq text (propertize text 'clatter-msgid msgid)))
         (when reply-to
           (setq text (propertize text 'clatter-reply-to reply-to)))
         (when is-reply-to-me
           (setq text (propertize text 'clatter-reply-to-me t)))
         (run-hook-with-args 'clatter-action-hook
                             conn sender target text
                             server-time)))
      ;; Don't respond to our own CTCP requests
      ((guard self-p) nil)
      ("VERSION"
       (clatter-send conn (clatter-irc-ctcp-reply
                           sender-nick "VERSION"
                           (format "clatter.el %s" clatter-version)))
       (run-hook-with-args 'clatter-system-hook conn
                           (format "CTCP VERSION from %s" sender-nick)))
      ("PING"
       (clatter-send conn (clatter-irc-ctcp-reply sender-nick "PING" ctcp-args))
       (run-hook-with-args 'clatter-system-hook conn
                           (format "CTCP PING from %s" sender-nick)))
      ("TIME"
       (clatter-send conn (clatter-irc-ctcp-reply
                           sender-nick "TIME" (format-time-string "%F %T")))
       (run-hook-with-args 'clatter-system-hook conn
                           (format "CTCP TIME from %s" sender-nick)))
      (_
       (run-hook-with-args 'clatter-ctcp-hook
                           conn sender target ctcp-cmd ctcp-args)))))

;; --- Batch Handling ---

(defun clatter--accumulate-batch (conn batch-id sender text server-time)
  "Accumulate a message into active batch BATCH-ID on CONN."
  (let ((batch (gethash batch-id (clatter-connection-active-batches conn))))
    (when batch
      (push (list :sender sender :text text :time server-time)
            (plist-get batch :messages)))))

(defun clatter--handle-batch (conn _tags params)
  "Handle BATCH command on CONN with PARAMS."
  (let* ((ref (nth 0 params))
         (starting (and (> (length ref) 0) (= (aref ref 0) ?+)))
         (batch-id (substring ref 1)))
    (if starting
        ;; Start batch
        (puthash batch-id
                 (list :type (nth 1 params) :target (nth 2 params) :messages nil)
                 (clatter-connection-active-batches conn))
      ;; End batch
      (let ((batch (gethash batch-id (clatter-connection-active-batches conn))))
        (when batch
          (let ((messages (nreverse (plist-get batch :messages))))
            (run-hook-with-args 'clatter-batch-complete-hook conn
                                (plist-get batch :type)
                                (plist-get batch :target)
                                messages))
          (remhash batch-id (clatter-connection-active-batches conn)))))))

;; --- Labeled Response ---

(defun clatter--handle-labeled-response (conn label msg)
  "Handle labeled response MSG on CONN for LABEL."
  (let ((callback (gethash label (clatter-connection-pending-labels conn))))
    (when callback
      (funcall callback msg)
      (remhash label (clatter-connection-pending-labels conn)))))

;; --- Nick Reclaim ---

(defvar clatter-nick-reclaim-interval 15
  "Seconds between nick reclaim attempts.")

(defvar clatter-nick-reclaim-max-attempts 40
  "Max nick reclaim attempts before giving up (10 minutes at 15s interval).")

(defun clatter--connection-bouncer-p (conn)
  "Return non-nil when CONN uses a bouncer-managed upstream session.
The live process config takes precedence over the saved network entry so
temporary `clatter-connect' keyword overrides are honored."
  (let* ((process (clatter-connection-process conn))
         (config (or (and process (process-get process :clatter-config))
                     (cdr (assoc (clatter-connection-network-id conn)
                                 clatter-networks #'equal)))))
    (plist-get config :bouncer)))

(defun clatter--maybe-start-nick-reclaim (conn)
  "Start a nick reclaim timer on CONN if current nick differs from desired."
  ;; Cancel any existing reclaim timer
  (when (clatter-connection-nick-reclaim-timer conn)
    (cancel-timer (clatter-connection-nick-reclaim-timer conn))
    (setf (clatter-connection-nick-reclaim-timer conn) nil))
  (let ((desired (clatter-connection-desired-nick conn))
        (current (clatter-connection-nick conn)))
    (when (and clatter-nick-reclaim-enabled
               (not (clatter--connection-bouncer-p conn))
               desired current
               (not (string-equal current desired)))
      ;; Start periodic reclaim attempts.  The first attempt is delayed
      ;; by `clatter-nick-reclaim-initial-delay' so that services have
      ;; settled our SASL login and any ENFORCE hold before we act.
      (let ((attempts 0))
        (setf (clatter-connection-nick-reclaim-timer conn)
              (run-at-time clatter-nick-reclaim-initial-delay
                           clatter-nick-reclaim-interval
                           (lambda ()
                             (setq attempts (1+ attempts))
                             (cond
                              ;; Disabled at runtime, gave up, or disconnected
                              ((or (not clatter-nick-reclaim-enabled)
                                   (> attempts clatter-nick-reclaim-max-attempts)
                                   (not (eq (clatter-connection-state conn) :connected)))
                               (when (clatter-connection-nick-reclaim-timer conn)
                                 (cancel-timer (clatter-connection-nick-reclaim-timer conn))
                                 (setf (clatter-connection-nick-reclaim-timer conn) nil))
                               (when (> attempts clatter-nick-reclaim-max-attempts)
                                 (message "[clatter] Gave up reclaiming nick %s" desired)))
                              ;; Already have it
                              ((string-equal (clatter-connection-nick conn) desired)
                               (cancel-timer (clatter-connection-nick-reclaim-timer conn))
                               (setf (clatter-connection-nick-reclaim-timer conn) nil))
                              ;; Try reclaim
                              (t
                               (clatter--reclaim-nick conn desired))))))
        (message "[clatter] Will try to reclaim nick %s every %ds"
                 desired clatter-nick-reclaim-interval)))))

(defun clatter--reclaim-nick (conn desired)
  "Attempt to reclaim DESIRED nick on CONN.
When SASL-identified and `clatter-nick-reclaim-use-regain' is set, use
NickServ REGAIN so services hand us the nick as the authenticated owner
\(cooperating with ENFORCE).  Otherwise fall back to a bare NICK."
  (unless (clatter--connection-bouncer-p conn)
    (if (and clatter-nick-reclaim-use-regain
           (eq (clatter-connection-sasl-state conn) :done))
        (progn
          (clatter--watchdog "RECLAIM-REGAIN %s nick=%s"
                             (clatter-connection-network-id conn) desired)
          (clatter-send conn (clatter-irc-privmsg
                              "NickServ" (format "REGAIN %s" desired))))
      (clatter-send conn (clatter-irc-nick desired)))))

(provide 'clatter-handlers)

;;; clatter-handlers.el ends here
