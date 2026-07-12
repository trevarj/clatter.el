;;; test-read-marker.el --- Tests for clatter-read-marker.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-read-marker)

(defun clatter-test-read-marker--timestamp (time)
  "Return read-marker timestamp string for TIME."
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" time t))

(ert-deftest clatter-test-read-marker-tracks-channel-buffer ()
  "PRIVMSG timestamps are recorded in the message target buffer."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (time (encode-time 1 2 3 4 5 2026 t))
        buf)
    (unwind-protect
        (progn
          (with-temp-buffer
            (clatter-mode)
            (clatter-read-marker--track-msgid
             conn '("alice" nil nil) "#emacs" "hello" time)
            (should-not clatter-read-marker--local-msgid))
          (setq buf (clatter-get-buffer "testnet" "#emacs"))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (eq clatter--buffer-type 'channel))
            (should (equal clatter-read-marker--local-msgid
                           (clatter-test-read-marker--timestamp time)))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "testnet" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest clatter-test-read-marker-tracks-query-sender-buffer ()
  "PRIVMSGs to our nick are recorded in the sender query buffer."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (time (encode-time 1 2 3 4 5 2026 t))
        buf)
    (unwind-protect
        (progn
          (clatter-read-marker--track-msgid
           conn '("alice" nil nil) "me" "hello" time)
          (setq buf (clatter-get-buffer "testnet" "alice"))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (eq clatter--buffer-type 'query))
            (should (equal clatter-read-marker--local-msgid
                           (clatter-test-read-marker--timestamp time)))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "testnet" "alice")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest clatter-test-read-marker-enable-tracks-actions ()
  "Read-marker mode tracks both PRIVMSG and CTCP ACTION hooks."
  (unwind-protect
      (progn
        (clatter-read-marker-enable)
        (should (memq #'clatter-read-marker--track-msgid clatter-privmsg-hook))
        (should (memq #'clatter-read-marker--track-msgid clatter-action-hook)))
    (clatter-read-marker-disable)))

(ert-deftest clatter-test-read-marker-window-change-marks-window-buffer ()
  "Window-change hooks send MARKREAD for the changed window's buffer."
  (let ((conn (clatter-test-make-connection-with-caps
               '("draft/read-marker") "testnet" "me"))
        (old-buffer (window-buffer (selected-window)))
        (timestamp "2026-05-04T03:02:01.000Z")
        buf)
    (unwind-protect
        (progn
          (setq buf (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
          (with-current-buffer buf
            (setq-local clatter-read-marker--local-msgid timestamp))
          (clatter-test-with-mock-send
            (set-window-buffer (selected-window) buf)
            (with-temp-buffer
              (clatter-mode)
              (clatter-read-marker--window-change (selected-window)))
            (should (equal (clatter-test-last-sent)
                           (format "MARKREAD #emacs timestamp=%s" timestamp)))))
      (set-window-buffer (selected-window) old-buffer)
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "testnet" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest clatter-test-read-marker-live-selected-buffer-sends-update ()
  "Messages arriving in the selected target buffer advance MARKREAD."
  (let ((conn (clatter-test-make-connection-with-caps
               '("draft/read-marker") "testnet" "me"))
        (old-buffer (window-buffer (selected-window)))
        (time (encode-time 1 2 3 4 5 2026 t))
        buf)
    (unwind-protect
        (progn
          (setq buf (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
          (clatter-test-with-mock-send
            (set-window-buffer (selected-window) buf)
            (clatter-read-marker--track-msgid
             conn '("alice" nil nil) "#emacs" "hello" time)
            (should (equal (clatter-test-last-sent)
                           (format "MARKREAD #emacs timestamp=%s"
                                   (clatter-test-read-marker--timestamp time))))))
      (set-window-buffer (selected-window) old-buffer)
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "testnet" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'test-read-marker)

;;; test-read-marker.el ends here
