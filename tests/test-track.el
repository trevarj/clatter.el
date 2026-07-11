;;; test-track.el --- Tests for clatter activity tracking -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-track)
(require 'clatter-ui)

(defmacro clatter-track-test--with-buffer (target &rest body)
  "Run BODY in a temporary clatter buffer for TARGET."
  (declare (indent 1))
  `(clatter-test-with-buffer
     (setq-local clatter--target ,target)
     (setq-local clatter--buffer-type
                 (if (string= ,target "*server*") 'server 'channel))
     ,@body))

(ert-deftest clatter-track-exclude-targets-omits-buffer-info ()
  "Excluded targets are absent from the tracker collection."
  (let ((clatter-track-exclude-targets '("#quiet")))
    (clatter-track-test--with-buffer "#quiet"
      (setq-local clatter--unread-count 2)
      (should-not (clatter-track--buffer-info (current-buffer)))
      (should-not (clatter-track--collect)))))

(ert-deftest clatter-track-muted-channels-remain-visible ()
  "Muted targets remain visible and use the muted tracker face."
  (let ((clatter-track-muted-channels '("#bots")))
    (clatter-track-test--with-buffer "#bots"
      (setq-local clatter--unread-count 1)
      (let* ((info (clatter-track--buffer-info (current-buffer)))
             (entry (clatter-track--format-entry info)))
        (should info)
        (should (plist-get info :muted))
        (should (eq (get-text-property 0 'face entry)
                    'clatter-track-muted))))))

(ert-deftest clatter-track-server-notices-do-not-set-mentions ()
  "A server-buffer NOTICE containing our nick is activity, not a mention."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((server (clatter-get-or-create-buffer "testnet" "*server*" 'server))
              (source (generate-new-buffer " *clatter-track-source*")))
          (unwind-protect
              (with-current-buffer source
                (clatter-mode)
                (clatter-insert-notice server "NickServ" "testnick is registered" conn)
                (with-current-buffer server
                  (should (= clatter--unread-count 1))
                  (should-not clatter--has-mention)))
            (when (buffer-live-p source) (kill-buffer source))
            (when (buffer-live-p server) (kill-buffer server))
            (clatter-remove-buffer "testnet" "*server*")))
      (clatter-test-cleanup))))

(provide 'test-track)

;;; test-track.el ends here
