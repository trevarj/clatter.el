;;; test-ui.el --- Tests for clatter-ui.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-commands)
(require 'clatter-nicklist)
(require 'clatter-track)

;; --- Automatic buffer display ---

(defmacro clatter-test-with-ui-connection (conn &rest body)
  "Run BODY with CONN and remove its clatter buffers afterwards."
  (declare (indent 1))
  `(let ((initial-buffers clatter--buffer-alist)
         (,conn (clatter-test-make-connection)))
     (unwind-protect
         (progn ,@body)
       (dolist (entry clatter--buffer-alist)
         (when (and (not (memq entry initial-buffers))
                    (buffer-live-p (cdr entry)))
           (kill-buffer (cdr entry))))
       (setq clatter--buffer-alist initial-buffers)
       (clatter-test-cleanup))))

(ert-deftest clatter-test-join-display-can-be-disabled ()
  "Self JOIN still creates its buffer when automatic display is disabled."
  (let ((clatter-display-on-join nil)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (clatter-test-with-mock-send
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (&rest _) (setq displayed t))))
          (clatter-ui--on-join conn '("testnick" "user" "host") "#quiet" nil nil)))
      (should (clatter-get-buffer "testnet" "#quiet"))
      (should-not displayed))))

(ert-deftest clatter-test-join-display-defaults-to-enabled ()
  "Self JOIN uses `display-buffer' by default."
  (let ((clatter-display-on-join t)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (clatter-test-with-mock-send
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buf &rest _) (setq displayed buf))))
          (clatter-ui--on-join conn '("testnick" "user" "host") "#shown" nil nil)))
      (should (bufferp displayed)))))

(ert-deftest clatter-test-welcome-display-can-be-disabled ()
  "Welcome still creates the server buffer when automatic display is disabled."
  (let ((clatter-display-on-welcome nil)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (&rest _) (setq displayed t))))
        (clatter-ui--on-welcome conn "testnick"))
      (should (clatter-get-server-buffer "testnet"))
      (should-not displayed))))

(ert-deftest clatter-test-welcome-display-defaults-to-enabled ()
  "Welcome uses `display-buffer' by default."
  (let ((clatter-display-on-welcome t)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buf &rest _) (setq displayed buf))))
        (clatter-ui--on-welcome conn "testnick"))
      (should (bufferp displayed)))))

(ert-deftest clatter-test-received-query-display-modes ()
  "Incoming queries are buried, displayed, or popped as configured."
  (dolist (case '((bury nil nil) (buffer t nil) (pop nil t)))
    (pcase-let ((`(,mode ,expect-display ,expect-pop) case))
      (let ((clatter-receive-query-display mode)
            (displayed nil)
            (popped nil))
        (clatter-test-with-ui-connection conn
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _) (setq displayed t)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _) (setq popped t))))
            (clatter-ui--on-privmsg conn '("alice" "user" "host") "TeStNiCk"
                                     "hello" nil))
          (should (clatter-get-buffer "testnet" "alice"))
          (should (eq displayed expect-display))
          (should (eq popped expect-pop)))))))

(ert-deftest clatter-test-received-query-ctcp-action-displays-with-mixed-case-target ()
  "Received CTCP ACTION uses query display policy with a case-folded target."
  (let ((clatter-receive-query-display 'pop)
        (popped nil))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buf &rest _) (setq popped buf))))
        (clatter-dispatch-message
         conn (clatter-test-parse
               ":alice!user@host PRIVMSG TeStNiCk :\C-aACTION waves\C-a")))
      (should (eq popped (clatter-get-buffer "testnet" "alice"))))))

(ert-deftest clatter-test-rfc1459-self-echo-does-not-display-query ()
  "RFC1459-equivalent self echoes never apply received-query display policy."
  (let ((clatter-receive-query-display 'pop)
        (popped nil))
    (clatter-test-with-ui-connection conn
      (setf (clatter-connection-nick conn) "{nick")
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (&rest _) (setq popped t))))
        ;; Under RFC1459 CASEMAPPING, [NICK and {nick are the same nick.
        (clatter-ui--on-privmsg conn '("[NICK" "user" "host") "{nick"
                                 "echo" nil))
      (should (clatter-get-buffer "testnet" "[NICK"))
      (should-not popped))))
;; --- Self echo ---

(ert-deftest clatter-test-optimistic-self-echo-reconciles-server-metadata ()
  "An optimistic local line is replaced by its server echo metadata."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-one"))
                 (buf (clatter-get-or-create-buffer "echo-one" "#test")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "hello"))
            (should (= 1 (length (with-current-buffer buf clatter--pending-self-echoes))))
            (let ((server-text (propertize "hello" 'clatter-msgid "server-id"))
                  (server-time (encode-time 0 2 3 4 5 2026)))
              (clatter-ui--on-privmsg
               conn (clatter-parse-prefix "testnick!u@h") "#test" server-text server-time)
              (with-current-buffer buf
                (should-not clatter--pending-self-echoes)
                (let ((pos (clatter--find-message-position-by-msgid
                            buf "server-id")))
                  (should pos)
                  (should (equal (get-text-property pos 'clatter-server-time)
                                 server-time))
                  (should (equal (get-text-property pos 'clatter-text)
                                 server-text)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-optimistic-self-echo-queues-identical-messages ()
  "Identical optimistic sends consume one pending entry per server echo."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-two"))
                 (buf (clatter-get-or-create-buffer "echo-two" "#test"))
                 (sender (clatter-parse-prefix "testnick!u@h")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "same")
              (clatter-ui--send-privmsg conn "#test" "same"))
            (let ((before-first-echo (with-current-buffer buf (buffer-string))))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "same" 'clatter-msgid "one") nil)
            (with-current-buffer buf
              (should (= 1 (length clatter--pending-self-echoes)))
              ;; Reconciliation updates the tentative line in place; a
              ;; duplicate server echo would change the buffer contents.
              (should (equal before-first-echo (buffer-string))))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "same" 'clatter-msgid "two") nil)
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should (equal before-first-echo (buffer-string)))
              (should (clatter--find-message-position-by-msgid buf "one"))
              (should (clatter--find-message-position-by-msgid buf "two"))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-server-self-echo-waits-for-echo-message ()
  "The default mode retains the existing server-echo behavior."
  (let ((clatter-self-echo-mode 'server)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-three"))
                 (buf (clatter-get-or-create-buffer "echo-three" "#test")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "delayed")
              (should-not clatter--pending-self-echoes)
              (should-not (string-match-p "delayed" (buffer-string))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-optimistic-self-echo-without-echo-message-does-not-reconcile ()
  "Optimistic fallback does not retain state that swallows a later self message."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection-with-caps
                        '("server-time" "message-tags") "echo-no-cap"))
                 (buf (clatter-get-or-create-buffer "echo-no-cap" "#test"))
                 (sender (clatter-parse-prefix "testnick!u@h")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "fallback")
              (should-not clatter--pending-self-echoes))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "fallback" 'clatter-msgid "late-server") nil)
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should (clatter--find-message-position-by-msgid buf "late-server"))
              (should (= 2 (how-many "fallback" (point-min) (point-max)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-expired-optimistic-self-echo-does-not-reconcile ()
  "A delayed self message is not reconciled with an expired local echo."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-self-echo-timeout 1)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-expired"))
                 (buf (clatter-get-or-create-buffer "echo-expired" "#test"))
                 (sender (clatter-parse-prefix "testnick!u@h")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "delayed")
              (setf (plist-get (car clatter--pending-self-echoes) :created-at) 0))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "delayed" 'clatter-msgid "playback") nil)
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should (clatter--find-message-position-by-msgid buf "playback"))
              (should (= 2 (how-many "delayed" (point-min) (point-max)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-disconnect-clears-optimistic-self-echoes ()
  "Disconnecting clears pending local echoes before any later replay."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-disconnect"))
                 (buf (clatter-get-or-create-buffer "echo-disconnect" "#test"))
                 nonce)
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "before-disconnect")
              (setq nonce (plist-get (car clatter--pending-self-echoes) :nonce))
              (should clatter--pending-self-echoes))
            (clatter-ui--on-disconnect "echo-disconnect" "closed")
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should-not (text-property-any
                           (point-min) (point-max) 'clatter-self-echo-nonce nonce)))))
      (clatter-test-cleanup))))
(ert-deftest clatter-tab-navigates-buttons-outside-input ()
  "TAB moves to a message button when outside the input area."
  (with-temp-buffer
    (clatter-mode)
    (insert "x" (clatter-hl-urls-in-string "https://example.com/a"))
    (goto-char (point-min))
    (clatter-tab)
    (should (button-at (point)))))

;; --- Timestamp margins ---

(defun clatter-test--timestamp-overlay-count ()
  "Return the number of message timestamp overlays in the current buffer."
  (cl-count-if (lambda (overlay) (overlay-get overlay 'clatter-timestamp))
               (overlays-in (point-min) (point-max))))

(ert-deftest clatter-test-timestamps-only-if-changed-coalesces-formatted-values ()
  "Repeated formatted timestamps use one margin timestamp when enabled."
  (let ((clatter-timestamp-only-if-changed t)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-side 'right)
        (first (encode-time 30 12 10 1 1 2026))
        (same-minute (encode-time 59 12 10 1 1 2026))
        (next-minute (encode-time 0 13 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "first" nil nil first)
      (clatter--insert-message (current-buffer) "same minute" nil nil same-minute)
      (clatter--insert-message (current-buffer) "next minute" nil nil next-minute)
      (should (= (clatter-test--timestamp-overlay-count) 2)))))

(ert-deftest clatter-test-timestamps-only-if-changed-is-buffer-local ()
  "Timestamp suppression does not carry over to another buffer."
  (let ((clatter-timestamp-only-if-changed t)
        (clatter-timestamp-format "%H:%M")
        (time (encode-time 30 12 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "first" nil nil time)
      (clatter--insert-message (current-buffer) "same buffer" nil nil time)
      (should (= (clatter-test--timestamp-overlay-count) 1)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "other buffer" nil nil time)
      (should (= (clatter-test--timestamp-overlay-count) 1)))))

(ert-deftest clatter-test-timestamps-only-if-changed-default-keeps-every-timestamp ()
  "The default preserves the current per-message timestamp behavior."
  (let ((clatter-timestamp-only-if-changed nil)
        (clatter-timestamp-format "%H:%M")
        (time (encode-time 30 12 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "first" nil nil time)
      (clatter--insert-message (current-buffer) "second" nil nil time)
      (should (= (clatter-test--timestamp-overlay-count) 2)))))

(ert-deftest clatter-test-timestamp-side-left-margin ()
  "Left timestamp side configures the left margin."
  (let ((clatter-timestamp-side 'left)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 6))
      (should (= right-margin-width 0)))))

(ert-deftest clatter-test-timestamp-side-right-margin ()
  "Right timestamp side configures the right margin."
  (let ((clatter-timestamp-side 'right)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 0))
      (should (= right-margin-width 6)))))

(ert-deftest clatter-test-timestamp-side-sync-clears-stale-window-margin ()
  "Window margin sync clears the previous timestamp side."
  (let ((clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (switch-to-buffer (current-buffer))
      (let ((clatter-timestamp-side 'right))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should (= (cdr (window-margins)) 6)))
      (let ((clatter-timestamp-side 'left))
        (clatter--sync-window-margins)
        (should (= (car (window-margins)) 6))
        (should-not (cdr (window-margins))))
      (let ((clatter-timestamp-side nil))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should-not (cdr (window-margins)))))))

;; --- Message filling ---

(ert-deftest clatter-test-insert-message-fills-at-fill-column ()
  "Inserted messages are hard-wrapped at `clatter-fill-column'."
  (let ((clatter-fill-column 40)
        (clatter-nick-column-width 7))
    (with-temp-buffer
      (clatter--insert-message
       (current-buffer)
       "<alice> this is a long message that should wrap around the configured fill column"
       t)
      (should
       (equal (buffer-string)
              "<alice> this is a long message that\n        should wrap around the\n        configured fill column\n")))))

;; --- Fool visibility ---

;; --- Smart noise visibility ---

(ert-deftest clatter-test-smart-noise-seeds-new-buffer-by-default ()
  "New clatter buffers hide smart-filtered noise by default."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-smart-noise-disabled-does-not-seed-new-buffer ()
  "Disabling smart noise leaves the automatic category out of new buffers."
  (let ((clatter-smart-enabled nil)
        (clatter-smart-noise '(join))
        (clatter-suppress-messages '(muted)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-empty-smart-noise-does-not-seed-new-buffer ()
  "No automatic category is added when no message types are smart-filtered."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise nil)
        (clatter-suppress-messages '(muted)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-explicit-noise-suppression-is-preserved ()
  "Global noise suppression remains effective when smart noise is disabled."
  (let ((clatter-smart-enabled nil)
        (clatter-smart-noise nil)
        (clatter-suppress-messages '(muted noise)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-smart-noise-tags-noisy-events-in-new-buffer ()
  "A smart-filtered event in a new buffer carries the hidden noise category."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
          (cl-letf (((symbol-function 'clatter-smart-eval)
                   (lambda (&rest _args) t)))
          (clatter-ui--on-join conn '("noisy" nil nil) "#test" nil nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (should (memq 'noise buffer-invisibility-spec))
              (goto-char (point-min))
              (search-forward "noisy has joined")
              (should (memq 'noise
                            (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (dolist (buffer (clatter-all-buffers))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-hides-first-join-from-non-chatter ()
  "A nick with no prior channel signal has its first smart-noise event hidden."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join part away))
        (clatter-suppress-messages '(muted))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-ui--on-join conn '("lurker" nil nil) "#test" nil nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "lurker has joined")
              (should (memq 'noise
                            (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-keeps-part-from-active-nick ()
  "A nick that chatted in a channel keeps its later PART visible."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join part away))
        (clatter-suppress-messages '(muted))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-ui--on-privmsg conn '("alice" nil nil) "#test" "hello" nil)
          (clatter-ui--on-part conn '("alice" nil nil) "#test" "bye")
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "alice has left #test")
              (should-not (memq 'noise
                                (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-keeps-away-from-active-nick ()
  "A nick that chatted in a channel keeps its later AWAY visible."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join part away))
        (clatter-suppress-messages '(muted))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-ui--on-privmsg conn '("alice" nil nil) "#test" "hello" nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (clatter-nick-add buffer "alice")))
          (clatter-ui--on-away conn '("alice" nil nil) "away")
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "alice is away: away")
              (should-not (memq 'noise
                                (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-keeps-repeated-noise-from-active-nick ()
  "Once a nick has signal, later noise stays visible regardless of volume."
  (let ((clatter-smart-noise '(join part quit nick away))
        (clatter-smart-threshold 0.5))
    (with-temp-buffer
      (clatter-smart-put (current-buffer) "alice" 'privmsg)
      (dotimes (_ 8)
        (should-not (clatter-smart-eval (current-buffer) "alice" 'away))))))

(ert-deftest clatter-test-smart-noise-preserves-active-state-across-nick-change ()
  "Nick changes carry active state to the new nick."
  (let ((clatter-smart-noise '(join part quit nick away))
        (clatter-smart-threshold 0.5))
    (with-temp-buffer
      (clatter-smart-put (current-buffer) "alice" 'privmsg)
      (should-not (clatter-smart-eval (current-buffer) "alice" "alice_"))
      (dotimes (_ 8)
        (should-not (clatter-smart-eval (current-buffer) "alice_" 'part))))))

(ert-deftest clatter-test-fool-message-gets-dim-face ()
  "Messages with the fool invisibility category get `clatter-fool'."
  (with-temp-buffer
    (clatter--insert-message (current-buffer) "<fool> no thanks" t nil nil 'clatter-fool)
    (should (eq (get-text-property (point-min) 'invisible) 'clatter-fool))
    (should (memq 'clatter-fool (ensure-list (get-text-property (point-min) 'face))))))

(ert-deftest clatter-test-fool-face-takes-priority ()
  "The `clatter-fool' face is prepended to existing faces."
  (with-temp-buffer
    (clatter--insert-message
     (current-buffer)
     (propertize "<fool> no thanks" 'face 'clatter-notice)
     t nil nil 'clatter-fool)
    (should (equal (ensure-list (get-text-property (point-min) 'face))
                   '(clatter-fool clatter-notice)))))

(ert-deftest clatter-test-fool-visibility-seeds-buffer-invisibility ()
  "New clatter buffers hide fools unless `clatter-fools-visible' is non-nil."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

(ert-deftest clatter-test-toggle-fools-updates-existing-buffer ()
  "Toggling fool visibility updates existing clatter buffers."
  (let ((old clatter-fools-visible))
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq buffer-invisibility-spec '(clatter-fool muted))
          (clatter-toggle-fools 1)
          (should clatter-fools-visible)
          (should-not (memq 'clatter-fool buffer-invisibility-spec))
          (clatter-toggle-fools -1)
          (should-not clatter-fools-visible)
          (should (memq 'clatter-fool buffer-invisibility-spec)))
      (setq clatter-fools-visible old))))

(ert-deftest clatter-test-suppress-preserves-fool-visibility ()
  "Generic suppression commands keep fool visibility independent."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-suppress "none")
      (should (equal buffer-invisibility-spec '(clatter-fool)))
      (clatter-cmd-suppress "all")
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-suppress "none")
      (should-not (memq 'clatter-fool buffer-invisibility-spec))
      (clatter-cmd-suppress "all")
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

(ert-deftest clatter-test-suppress-cannot-desync-fool-visibility ()
  "Explicit generic suppressions cannot override the fool toggle state."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-unsuppress "clatter-fool")
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(muted))
      (clatter-cmd-suppress "clatter-fool")
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

;; --- Channel-at-point detection ---

(ert-deftest clatter-test-channel-at-point-hash ()
  "Detects #channel at point."
  (with-temp-buffer
    (insert "hello #emacs world")
    (goto-char 8)  ; on the #
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

(ert-deftest clatter-test-channel-at-point-middle ()
  "Detects channel when cursor is in the middle."
  (with-temp-buffer
    (insert "see #emacs for help")
    (goto-char 10)  ; on 'a' in emacs
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

(ert-deftest clatter-test-channel-at-point-ampersand ()
  "Detects &channel prefix."
  (with-temp-buffer
    (insert "join &local")
    (goto-char 6)
    (should (equal (clatter-ui--channel-at-point) "&local"))))

(ert-deftest clatter-test-channel-at-point-none ()
  "Returns nil when no channel at point."
  (with-temp-buffer
    (insert "just normal text")
    (goto-char 5)
    (should-not (clatter-ui--channel-at-point))))

(ert-deftest clatter-test-channel-at-point-hyphen ()
  "Detects channels with hyphens."
  (with-temp-buffer
    (insert "try #system-crafters")
    (goto-char 10)
    (should (equal (clatter-ui--channel-at-point) "#system-crafters"))))

(ert-deftest clatter-test-channel-at-point-start-of-line ()
  "#channel at start of line."
  (with-temp-buffer
    (insert "#emacs is great")
    (goto-char 3)
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

;; --- Eldoc function ---

(ert-deftest clatter-test-eldoc-channel-with-nicks ()
  "Eldoc shows user count for known channel."
  (let* ((network "testnet")
         (channel "#emacs")
         (buf (clatter-get-or-create-buffer network channel)))
    (unwind-protect
        (progn
          ;; Populate nick list
          (with-current-buffer buf
            (let ((ht (make-hash-table :test 'equal)))
              (puthash "alice" "@" ht)
              (puthash "bob" "" ht)
              (puthash "carol" "+" ht)
              (setq-local clatter--nick-list ht)
              (setq-local clatter--topic "Welcome to #emacs")))
          ;; Simulate being in a clatter buffer on this network
          (with-temp-buffer
            (setq-local clatter--network network)
            (insert "#emacs")
            (goto-char 3)
            (let ((result nil))
              (clatter-ui--eldoc-function
               (lambda (text &rest _) (setq result text)))
              (should result)
              (should (string-match-p "3 users" result))
              (should (string-match-p "Welcome to #emacs" result)))))
      (kill-buffer buf)
      (clatter-remove-buffer network channel))))

(ert-deftest clatter-test-eldoc-channel-no-data ()
  "Eldoc returns nothing for unknown channel."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (insert "#nonexistent")
    (goto-char 3)
    (let ((result nil))
      (clatter-ui--eldoc-function
       (lambda (text &rest _) (setq result text)))
      (should-not result))))

(ert-deftest clatter-test-eldoc-sender ()
  "Eldoc shows sender info on message."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (insert (propertize "hello world"
                        'clatter-sender "alice"
                        'clatter-msgid "msg123"))
    (goto-char 3)
    (let ((result nil))
      (clatter-ui--eldoc-function
       (lambda (text &rest _) (setq result text)))
      (should result)
      (should (string-match-p "alice" result))
      (should (string-match-p "msg123" result)))))

;; --- Header-line ---

(ert-deftest clatter-test-header-line-default-disabled ()
  "Clatter leaves the header line disabled by default."
  (with-temp-buffer
    (clatter-mode)
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "#emacs")
    (clatter-ui-setup-buffer (current-buffer))
    (should-not header-line-format)))

(ert-deftest clatter-test-header-line-renders-channel-context ()
  "Built-in header-line renderer shows full channel context."
  (let ((clatter-header-line-preset 'context))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--topic "A deliberately long topic that is not truncated in the header line")
      (setq-local clatter--channel-modes "+nt")
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" '("" . "alice") clatter--nick-list)
      (puthash "bob" '("@" . "bob") clatter--nick-list)
      (clatter-ui-setup-buffer (current-buffer))
      (should (equal header-line-format
                     '(:eval (clatter--header-line-string))))
      (let ((rendered (clatter--header-line-string)))
        (should (string-match-p "\\[testnet/#emacs\\]" rendered))
        (should (string-match-p "\\+nt" rendered))
        (should (string-match-p "2 nicks" rendered))
        (should (string-match-p "not truncated in the header line" rendered))))))

(ert-deftest clatter-test-header-line-topic-preset-deduplicates-topic ()
  "The topic preset moves only the topic out of the mode-line."
  (let ((clatter-header-line-preset 'topic))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--topic "A full topic")
      (clatter-ui-setup-buffer (current-buffer))
      (should (equal header-line-format
                     '(:eval (clatter--header-line-topic-string))))
      (should (equal (clatter--header-line-topic-string) "A full topic"))
      (let ((mode-line (clatter--mode-line-string)))
        (should (string-match-p "\\[testnet/#emacs\\]" mode-line))
        (should-not (string-match-p "A full topic" mode-line))))))

(ert-deftest clatter-test-header-line-context-preset-deduplicates-context ()
  "The context preset leaves only the current nick in the mode-line."
  (let ((clatter-header-line-preset 'context))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--topic "A full topic")
      (setq-local clatter--channel-modes "+nt")
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" t clatter--nick-list)
      (clatter-ui-setup-buffer (current-buffer))
      (should (equal header-line-format
                     '(:eval (clatter--header-line-string))))
      (should-not (memq 'mode-line-buffer-identification mode-line-format))
      (let ((mode-line (clatter--mode-line-string)))
        (should (string-match-p "\\?" mode-line))
        (should-not (string-match-p "testnet/#emacs" mode-line))
        (should-not (string-match-p "1" mode-line))
        (should-not (string-match-p "A full topic" mode-line))))))
;; --- Read state ---

(ert-deftest clatter-test-read-state-clear-persists-and-restores ()
  "Clearing activity persists and restores the latest read timestamp."
  (let* ((file (make-temp-file "clatter-read-state"))
         (clatter-read-state-enabled t)
         (clatter-read-state-file file)
         (clatter-read-state--table (make-hash-table :test 'equal))
         (clatter-read-state--loaded t)
         (clatter-read-state--save-timer nil)
         (time (encode-time 0 0 12 1 1 2026 t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "libera")
            (setq-local clatter--target "#emacs")
            (setq-local clatter--latest-message-time time)
            (clatter-clear-activity (current-buffer)))
          (clatter-read-state--save-now)
          (setq clatter-read-state--table (make-hash-table :test 'equal))
          (setq clatter-read-state--loaded nil)
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "libera")
            (setq-local clatter--target "#emacs")
            (clatter-read-state-restore-buffer (current-buffer))
            (should (equal clatter--last-read-time time))))
      (when clatter-read-state--save-timer
        (cancel-timer clatter-read-state--save-timer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest clatter-test-read-state-suppresses-read-history-activity ()
  "History at or before the last-read timestamp does not mark activity."
  (let ((conn (clatter-test-make-connection "libera" "me"))
        (buf nil)
        (clatter-read-state-enabled t))
    (unwind-protect
        (let* ((last-read (encode-time 0 0 12 1 1 2026 t))
               (old (encode-time 0 59 11 1 1 2026 t))
               (equal-time (encode-time 0 0 12 1 1 2026 t))
               (new (encode-time 0 1 12 1 1 2026 t)))
          (setq buf (clatter-get-or-create-buffer "libera" "#emacs" 'channel))
          (with-current-buffer buf
            (clatter-ui-setup-buffer buf)
            (setq-local clatter--last-read-time last-read))
          (with-temp-buffer
            (clatter-insert-privmsg buf "alice" "old" conn old)
            (clatter-insert-privmsg buf "alice" "equal" conn equal-time)
            (clatter-insert-privmsg buf "alice" "new" conn new))
          (with-current-buffer buf
            (should (= clatter--unread-count 1)))
          (let* ((infos (clatter-track--collect))
                 (info (cl-find buf infos
                                :key (lambda (entry)
                                       (plist-get entry :buffer)))))
            (should info)
            (should (= (plist-get info :unread) 1))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "libera" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;; --- Typing mode-line ---

(ert-deftest clatter-test-typing-mode-line-empty ()
  "No typing nicks returns nil."
  (with-temp-buffer
    (setq-local clatter--typing-nicks nil)
    (should-not (clatter--typing-mode-line))))

(ert-deftest clatter-test-typing-mode-line-one ()
  "One nick typing shows name."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "alice is typing" result)))))

(ert-deftest clatter-test-typing-mode-line-two ()
  "Two nicks typing shows both names."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (puthash "bob" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "are typing" result)))))

(ert-deftest clatter-test-typing-mode-line-many ()
  "Three+ nicks shows count."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (puthash "bob" t clatter--typing-nicks)
    (puthash "carol" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "3 people typing" result)))))

;; --- Outbound typing throttle ---

(ert-deftest clatter-test-typing-capable-no-network ()
  "Not typing-capable without network."
  (with-temp-buffer
    (setq-local clatter--network nil)
    (setq-local clatter--target "#test")
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-no-target ()
  "Not typing-capable without target."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target nil)
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-server-buffer ()
  "Not typing-capable in server buffer."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "*server*")
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-disabled ()
  "Not typing-capable when disabled."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "#test")
    (let ((clatter-send-typing nil))
      (should-not (clatter--typing-capable-p)))))

(ert-deftest clatter-test-typing-capable-with-caps ()
  "Typing-capable when message-tags enabled."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (with-temp-buffer
          (setq-local clatter--network "testnet")
          (setq-local clatter--target "#test")
          (let ((clatter-send-typing t))
            (should (clatter--typing-capable-p))))
      (clatter-test-cleanup))))

;; --- Nicklist hooks registered ---

(ert-deftest clatter-test-nicklist-hooks-registered ()
  "Nicklist auto-refresh hooks are registered."
  (should (memq #'clatter-nicklist--on-join (default-value 'clatter-join-hook)))
  (should (memq #'clatter-nicklist--on-part (default-value 'clatter-part-hook)))
  (should (memq #'clatter-nicklist--on-quit (default-value 'clatter-quit-hook)))
  (should (memq #'clatter-nicklist--on-nick (default-value 'clatter-nick-hook)))
  (should (memq #'clatter-nicklist--on-names (default-value 'clatter-names-hook))))

(provide 'test-ui)

;;; test-ui.el ends here
