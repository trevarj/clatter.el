;;; clatter-model.el --- Buffer and channel state management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Channel, nick, and buffer state management for clatter.el.
;; Maps IRC channels/queries to Emacs buffers and tracks nick lists,
;; topics, and channel modes.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)

(declare-function clatter-cmd-close "clatter-commands")
(declare-function clatter-completion-setup "clatter-completion")

;; --- Buffer naming ---

(defun clatter-buffer-name (network target)
  "Generate buffer name for NETWORK and TARGET (channel or nick).
Server buffers use TARGET \"*server*\"."
  (format "*clatter:%s/%s*" network target))

(defun clatter-server-buffer-name (network)
  "Generate server buffer name for NETWORK."
  (clatter-buffer-name network "*server*"))

;; --- Channel state (stored as buffer-local variables) ---

(defvar-local clatter--network nil
  "Network ID for this clatter buffer.")

(defvar-local clatter--target nil
  "Target (channel name, nick, or \"*server*\") for this buffer.")

(defvar-local clatter--nick-list nil
  "Hash table of nicks in this channel.
Keys are nicks (downcased), values are prefix chars (@ + etc).")

(defvar-local clatter--nick-accounts nil
  "Hash table mapping nicks (downcased) to account names.
Populated by WHOX (extended WHO) replies.")

(defvar-local clatter--topic nil
  "Current topic for this channel.")

(defvar-local clatter--channel-modes nil
  "Current channel modes string.")

(defvar-local clatter--buffer-type nil
  "Buffer type: server, channel, or query.")

;; --- Local read state ---

(defvar-local clatter--last-read-time nil
  "Newest server-time persisted as read for this buffer.")

(defvar-local clatter--latest-message-time nil
  "Newest server-time inserted into this buffer.")

(defvar clatter-read-state--table (make-hash-table :test 'equal)
  "Hash table of persisted local read timestamps keyed by network/target.")

(defvar clatter-read-state--loaded nil
  "Non-nil once `clatter-read-state-file' has been loaded.")

(defvar clatter-read-state--save-timer nil
  "Pending debounce timer for saving local read state.")

(defun clatter-read-state--key (network target)
  "Return the read-state key for NETWORK and TARGET."
  (cons network (downcase target)))

(defun clatter-read-state--load ()
  "Load local read state from `clatter-read-state-file' once."
  (when (and clatter-read-state-enabled
             (not clatter-read-state--loaded))
    (setq clatter-read-state--loaded t)
    (clrhash clatter-read-state--table)
    (when (file-readable-p clatter-read-state-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents clatter-read-state-file)
            (dolist (entry (read (current-buffer)))
              (when (and (consp entry) (consp (car entry)))
                (puthash (car entry) (cdr entry) clatter-read-state--table))))
        (error
         (message "[clatter] Could not load read state: %s"
                  (error-message-string err)))))))

(defun clatter-read-state--alist ()
  "Return `clatter-read-state--table' as an alist."
  (let (entries)
    (maphash (lambda (key value)
               (push (cons key value) entries))
             clatter-read-state--table)
    entries))

(defun clatter-read-state--save-now ()
  "Write local read state to `clatter-read-state-file'."
  (when clatter-read-state--save-timer
    (cancel-timer clatter-read-state--save-timer)
    (setq clatter-read-state--save-timer nil))
  (when clatter-read-state-enabled
    (let ((dir (file-name-directory clatter-read-state-file)))
      (when dir
        (make-directory dir t)))
    (with-temp-file clatter-read-state-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 (clatter-read-state--alist) (current-buffer))))))

(defun clatter-read-state--schedule-save ()
  "Debounce saving local read state."
  (when clatter-read-state-enabled
    (when clatter-read-state--save-timer
      (cancel-timer clatter-read-state--save-timer))
    (setq clatter-read-state--save-timer
          (run-with-timer clatter-read-state-save-delay nil
                          #'clatter-read-state--save-now))
    (add-hook 'kill-emacs-hook #'clatter-read-state--save-now)))

(defun clatter-read-state-restore-buffer (buffer)
  "Restore persisted read state into BUFFER."
  (when clatter-read-state-enabled
    (clatter-read-state--load)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and clatter--network clatter--target)
          (setq clatter--last-read-time
                (gethash (clatter-read-state--key clatter--network clatter--target)
                         clatter-read-state--table)))))))

(defun clatter-read-state-record-buffer (buffer)
  "Persist BUFFER's latest message time as read."
  (when (and clatter-read-state-enabled (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (and clatter--network clatter--target clatter--latest-message-time)
        (let ((key (clatter-read-state--key clatter--network clatter--target)))
          (setq clatter--last-read-time clatter--latest-message-time)
          (puthash key clatter--last-read-time clatter-read-state--table)
          (clatter-read-state--schedule-save))))))

(defun clatter-note-message-time (buffer server-time)
  "Record SERVER-TIME as the newest message time seen in BUFFER."
  (when (and server-time (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (or (null clatter--latest-message-time)
                (time-less-p clatter--latest-message-time server-time))
        (setq clatter--latest-message-time server-time)))))

(defun clatter-read-state-message-read-p (buffer server-time)
  "Return non-nil if SERVER-TIME is already read in BUFFER."
  (and clatter-read-state-enabled
       server-time
       (buffer-live-p buffer)
       (with-current-buffer buffer
         (and clatter--last-read-time
              (not (time-less-p clatter--last-read-time server-time))))))

;; --- Buffer registry ---

(defvar clatter--buffer-alist nil
  "Alist mapping (network . target) to buffer objects.")

(defun clatter-get-buffer (network target)
  "Get existing clatter buffer for NETWORK and TARGET, or nil."
  (let ((key (cons network (downcase target))))
    (alist-get key clatter--buffer-alist nil nil #'equal)))

(defun clatter-get-server-buffer (network)
  "Get server buffer for NETWORK."
  (clatter-get-buffer network "*server*"))

(defun clatter-get-or-create-buffer (network target &optional type)
  "Get or create a clatter buffer for NETWORK and TARGET.
TYPE is server, channel, or query (auto-detected if nil)."
  (or (clatter-get-buffer network target)
      (let* ((buf-name (clatter-buffer-name network target))
             (buf (get-buffer-create buf-name))
             (buf-type (or type
                           (cond
                            ((string= target "*server*") 'server)
                            ((clatter-channel-name-p target) 'channel)
                            (t 'query)))))
        (with-current-buffer buf
          (clatter-mode)
          (setq clatter--network network)
          (setq clatter--target target)
          (setq clatter--buffer-type buf-type)
          (when (eq buf-type 'channel)
            (setq clatter--nick-list (make-hash-table :test 'equal)))
          (clatter-read-state-restore-buffer buf))
        ;; Register
        (let ((key (cons network (downcase target))))
          (setf (alist-get key clatter--buffer-alist nil nil #'equal) buf))
        buf)))

(defun clatter-remove-buffer (network target)
  "Remove buffer for NETWORK and TARGET from registry."
  (let ((key (cons network (downcase target))))
    (setf clatter--buffer-alist
          (cl-remove key clatter--buffer-alist :key #'car :test #'equal))))

(defun clatter-all-buffers (&optional network)
  "Return all clatter buffers, optionally filtered by NETWORK."
  (cl-loop for (key . buf) in clatter--buffer-alist
           when (and (buffer-live-p buf)
                     (or (null network)
                         (string= (car key) network)))
           collect buf))

(defun clatter-channel-buffers (network)
  "Return all channel buffers for NETWORK."
  (cl-loop for buf in (clatter-all-buffers network)
           when (with-current-buffer buf
                  (eq clatter--buffer-type 'channel))
           collect buf))

;; --- Input ring ---

(defvar-local clatter-input-ring nil
  "Ring object for input history.")

(defvar-local clatter-input-ring-index 0
  "Current position in the input history.")

(defun clatter-input-ring-setup ()
  "Initialize the buffer-local input history ring."
  (setq clatter-input-ring
        (if (and (ring-p clatter-input-ring)
                 (= (ring-size clatter-input-ring)
                    clatter-input-ring-size))
            clatter-input-ring
          (make-ring clatter-input-ring-size))))

(defun clatter-input-ring-add (input)
  "Add INPUT to the input history ring."
  (ring-insert clatter-input-ring input)
  (setq clatter-input-ring-index 0))

(defun clatter-input-ring-nth (n)
  "Get element N steps back in the input history ring.
Return nil when the ring is empty or N is out of range."
  (when (and (ring-p clatter-input-ring)
             (not (ring-empty-p clatter-input-ring))
             (>= n 0)
             (< n (ring-length clatter-input-ring)))
    (ring-ref clatter-input-ring n)))

;; --- Nick list management ---

(defun clatter-nick-add (buffer nick &optional prefix)
  "Add NICK with optional PREFIX (@, +, etc) to BUFFER's nick list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (puthash (downcase nick) (cons (or prefix "") nick) clatter--nick-list)))))

(defun clatter-nick-remove (buffer nick)
  "Remove NICK from BUFFER's nick list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (remhash (downcase nick) clatter--nick-list)))))

(defun clatter-nick-rename (buffer old-nick new-nick)
  "Rename OLD-NICK to NEW-NICK in BUFFER's nick list."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (let* ((prefix-and-nick (gethash (downcase old-nick) clatter--nick-list))
               (prefix (car prefix-and-nick)))
          (remhash (downcase old-nick) clatter--nick-list)
          (puthash (downcase new-nick) (cons (or prefix "") new-nick) clatter--nick-list))))))

(defun clatter-nick-list (buffer)
  "Return sorted list of (nick . prefix) pairs for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--nick-list
        (let ((nicks nil))
          (maphash (lambda (_k v) (push (cons (cdr v) (car v)) nicks)) clatter--nick-list)
          (sort nicks (lambda (a b) (string< (car a) (car b)))))))))

(defun clatter-nick-count (buffer)
  "Return number of nicks in BUFFER's nick list."
  (if (and (buffer-live-p buffer)
           (buffer-local-value 'clatter--nick-list buffer))
      (hash-table-count (buffer-local-value 'clatter--nick-list buffer))
    0))

(defun clatter-nick-set-account (buffer nick account)
  "Set ACCOUNT name for NICK in BUFFER's account table."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless clatter--nick-accounts
        (setq clatter--nick-accounts (make-hash-table :test 'equal)))
      (puthash (downcase nick) account clatter--nick-accounts))))

(defun clatter-nick-get-account (buffer nick)
  "Get account name for NICK in BUFFER, or nil."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'clatter--nick-accounts buffer))
    (gethash (downcase nick)
             (buffer-local-value 'clatter--nick-accounts buffer))))

(defconst clatter-whox-token "645"
  "Token used in WHOX queries to identify our replies.")

(defun clatter-send-whox (conn channel)
  "Send WHOX query for CHANNEL on CONN if WHOX is supported.
Uses format: WHO #channel %tcnuhraf,TOKEN to get account names."
  (clatter-send conn (format "WHO %s %%tcnuhraf,%s"
                              channel clatter-whox-token)))

(defun clatter-parse-names (names-str &optional prefixes)
  "Parse NAMES-STR into a list of (nick . prefix), honoring PREFIXES.
Handles prefixes like @nick, +nick, ~nick."
  (unless prefixes
    (setq prefixes clatter-prefix-rank))
  (mapcar (lambda (entry)
            (cl-loop for character being the elements of (string-trim entry)
                     with boundary = 0
                     while (seq-contains-p prefixes character)
                     do (cl-incf boundary)
                     finally return (cons (car (split-string (substring entry boundary) (rx ?!)))
                                          (substring entry 0 boundary))))
              (split-string names-str)))

;; --- Topic management ---

(defun clatter-set-topic (buffer topic)
  "Set TOPIC for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq clatter--topic topic))))

(defun clatter-get-topic (buffer)
  "Get TOPIC for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      clatter--topic)))

;; --- Channel modes ---

(defun clatter-set-channel-modes (buffer modes)
  "Set channel MODES for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq clatter--channel-modes modes))))

;; --- Activity tracking ---

(defvar-local clatter--unread-count 0
  "Number of unread messages in this buffer.")

(defvar-local clatter--has-mention nil
  "Non-nil if there is an unread mention in this buffer.")

(defun clatter-mark-activity (buffer &optional mention)
  "Mark BUFFER as having activity.  If MENTION, also mark as mentioned."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cl-incf clatter--unread-count)
      (when mention
        (setq clatter--has-mention t)))))

(defun clatter-clear-activity (buffer)
  "Clear activity tracking for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq clatter--unread-count 0)
      (setq clatter--has-mention nil))
    (clatter-read-state-record-buffer buffer)))

;; --- Major mode (minimal, UI fills in details) ---

(defvar clatter-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `clatter-mode'.")

(defvar clatter-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Make channel prefixes part of symbol syntax so
    ;; (thing-at-point 'symbol) picks up #channel names.
    (modify-syntax-entry ?# "_" st)
    (modify-syntax-entry ?& "_" st)
    (modify-syntax-entry ?! "_" st)
    (modify-syntax-entry ?+ "_" st)
    ;; Common nick/channel chars
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?\[ "_" st)
    (modify-syntax-entry ?\] "_" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?` "_" st)
    (modify-syntax-entry ?^ "_" st)
    (modify-syntax-entry ?{ "_" st)
    (modify-syntax-entry ?} "_" st)
    (modify-syntax-entry ?| "_" st)
    st)
  "Syntax table for `clatter-mode'.
Makes IRC channel prefixes and nick characters part of symbol syntax.")

(defun clatter--on-kill-buffer ()
  "Close this clatter buffer, disconnecting or parting as appropriate.
Installed buffer-locally on `kill-buffer-hook' by `clatter-mode'."
  ;; Avoid infinite recursion when the cleanup itself kills the buffer.
  (remove-hook 'kill-buffer-hook #'clatter--on-kill-buffer t)
  (cond
   ((not (boundp 'clatter--buffer-type)) nil)
   ((or (eq 'channel clatter--buffer-type)
        (eq 'query clatter--buffer-type))
    (when (fboundp 'clatter-cmd-close)
      (clatter-cmd-close nil)))
   ((and (eq 'server clatter--buffer-type)
         (boundp 'clatter--network)
         clatter--network
         (fboundp 'clatter-disconnect))
    (clatter-disconnect clatter--network)
    (when (and (fboundp 'clatter-remove-buffer)
               (boundp 'clatter--target))
     (clatter-remove-buffer clatter--network clatter--target)))))

(define-derived-mode clatter-mode fundamental-mode "CLatter"
  "Major mode for clatter.el IRC buffers."
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1)
  (setq buffer-read-only nil)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (setq-local wrap-prefix (make-string (1+ clatter-nick-column-width) ?\s))
  ;; Margin for timestamps (always on first visual line)
  (let ((ts-width (1+ (length (format-time-string clatter-timestamp-format)))))
    (setq-local left-margin-width (if (eq clatter-timestamp-side 'left) ts-width 0))
    (setq-local right-margin-width (if (eq clatter-timestamp-side 'right) ts-width 0)))
  ;; Per-buffer setup (kept here, not on a global hook, so that loading
  ;; clatter has no side effects).
  (add-hook 'kill-buffer-hook #'clatter--on-kill-buffer nil t)
  (when (fboundp 'clatter-completion-setup)
    (clatter-completion-setup))
  ;; Flyspell: only check words in the input area (not read-only messages)
  (when clatter-flyspell-enable
    (require 'flyspell)
    (setq-local flyspell-generic-check-word-predicate
                #'clatter--flyspell-predicate)
    (flyspell-mode 1)))

(defun clatter--flyspell-predicate ()
  "Return non-nil if point is in the input area (not read-only message text)."
  (and clatter--input-marker
       clatter--messages-marker
       (>= (point) (marker-position clatter--input-marker))
       (<= (point) (if (eq clatter-message-order 'oldest-first)
                       (point-max)
                     (1- (marker-position clatter--messages-marker))))))

(provide 'clatter-model)

;;; clatter-model.el ends here
