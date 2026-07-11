;;; test-url-preview.el --- Tests for URL title preview placement -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-url-preview)

(ert-deftest clatter-url-preview-insert-stays-at-message-anchor ()
  "An async title is inserted below its URL, not at the current buffer end."
  (with-temp-buffer
    (insert "<alice> https://example.com/one\n")
    (let ((anchor (copy-marker (point))))
      ;; Simulate messages arriving while curl fetches the title.  A nil
      ;; insertion-type anchor remains before this later text.
      (insert "<bob> later message\n")
      (clatter-url-preview--insert "Example title" (current-buffer) anchor)
      (should (equal (buffer-string)
                     "<alice> https://example.com/one\n↳ Example title\n<bob> later message\n")))))

(ert-deftest clatter-url-preview-hook-passes-message-anchor-to-fetch ()
  "The hook gives the fetcher an anchor after the just-inserted message."
  (let ((clatter-url-preview-enable t)
        (clatter-url-preview--cache (make-hash-table :test 'equal))
        (clatter-url-preview--pending (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert "<alice> https://example.com/one\n")
      (let ((clatter--messages-marker (copy-marker (point) t))
            (conn (clatter-test-make-connection "preview-test" "me"))
            captured-marker)
        (cl-letf (((symbol-function 'clatter-get-buffer)
                   (lambda (&rest _) (current-buffer)))
                  ((symbol-function 'clatter-url-preview--fetch)
                   (lambda (_url _buffer marker) (setq captured-marker marker))))
          (clatter-url-preview--on-privmsg
           conn "alice" "#chat" "https://example.com/one" nil))
        (should (clatter-url-preview--anchor-p captured-marker))
        (should (eq (clatter-url-preview--anchor-buffer captured-marker)
                    (current-buffer)))
        (should (= (marker-position
                    (clatter-url-preview--anchor-tail captured-marker))
                   (point-max)))
        (should-not (marker-insertion-type
                     (clatter-url-preview--anchor-tail captured-marker)))))))

(ert-deftest clatter-url-preview-hook-renders-after-message-in-both-orders ()
  "URL previews follow their source message for either message order."
  (dolist (order '(newest-first oldest-first))
    (let ((clatter-message-order order)
          (clatter-url-preview-enable t)
          (clatter-url-preview--cache (make-hash-table :test 'equal))
          (clatter-url-preview--pending (make-hash-table :test 'equal))
          (clatter-privmsg-hook (list #'clatter-ui--on-privmsg)))
      (puthash "https://example.com/one" "Example title"
               clatter-url-preview--cache)
      (clatter-url-preview-init)
      (should (eq (car clatter-privmsg-hook) #'clatter-ui--on-privmsg))
      (should (eq (cadr clatter-privmsg-hook)
                  #'clatter-url-preview--on-privmsg))
      (with-temp-buffer
        (clatter-mode)
        (setq-local clatter--network "preview-order")
        (setq-local clatter--target "#chat")
        (let ((conn (clatter-test-make-connection "preview-order" "me")))
          (cl-letf (((symbol-function 'clatter-get-or-create-buffer)
                     (lambda (&rest _) (current-buffer)))
                    ((symbol-function 'clatter-get-buffer)
                     (lambda (&rest _) (current-buffer))))
            (run-hook-with-args 'clatter-privmsg-hook
                                conn '("alice" nil nil) "#chat"
                                "https://example.com/one" nil)))
        (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
               (message-pos (string-match "https://example.com/one" contents))
               (preview-pos (string-match "↳ Example title" contents))
               (prompt-pos (string-match "#chat>" contents)))
          (should message-pos)
          (should preview-pos)
          (should prompt-pos)
          (should (< message-pos preview-pos))
          (if (eq order 'oldest-first)
              (should (< preview-pos prompt-pos))
            (should (< prompt-pos message-pos))))))))

(ert-deftest clatter-url-preview-message-anchor-finds-newest-message-end ()
  "A newest-first messages marker is before the message, not after it."
  (with-temp-buffer
    (insert "<alice> https://example.com/one\n<older> previous\n")
    (add-text-properties (point-min) (+ (point-min) 32)
                         '(clatter-text "https://example.com/one"))
    (let ((clatter--messages-marker (copy-marker (point-min))))
      (should (= (marker-position
                  (clatter-url-preview--message-end-marker
                   (current-buffer) "https://example.com/one"))
                 (+ (point-min) 32))))))

(ert-deftest clatter-url-preview-cached-title-uses-message-anchor ()
  "Cached previews use the same anchor as asynchronous previews."
  (let ((clatter-url-preview-enable t)
        (clatter-url-preview--cache (make-hash-table :test 'equal))
        (clatter-url-preview--pending (make-hash-table :test 'equal)))
    (puthash "https://example.com/one" "Cached title" clatter-url-preview--cache)
    (with-temp-buffer
      (insert "<alice> https://example.com/one\n")
      (let ((clatter--messages-marker (copy-marker (point) t))
            (conn (clatter-test-make-connection "preview-cache" "me")))
        (cl-letf (((symbol-function 'clatter-get-buffer)
                   (lambda (&rest _) (current-buffer))))
          (clatter-url-preview--on-privmsg
           conn "alice" "#chat" "https://example.com/one" nil))
        (should (equal (buffer-string)
                       "<alice> https://example.com/one\n↳ Cached title\n"))))))

(ert-deftest clatter-url-preview-fetch-start-failure-cleans-up ()
  "A failed curl start clears pending state without signaling repeatedly."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal)))
    (with-temp-buffer
      (let ((start (copy-marker (point-min)))
            (end (copy-marker (point-max)))
            (tail (copy-marker (point-max))))
        (let ((anchor
               (clatter-url-preview--make-anchor-record
                :buffer (current-buffer)
                :start start
                :end end
                :tail tail
                :text nil
                :remaining 1)))
          (cl-letf (((symbol-function 'start-process)
                     (lambda (&rest _)
                       (signal 'file-missing '("curl")))))
            (clatter-url-preview--fetch
             "https://example.com/failure" (current-buffer) anchor))
          (should-not (gethash "https://example.com/failure"
                               clatter-url-preview--pending))
          (should-not (marker-buffer start))
          (should-not (marker-buffer end))
          (should-not (marker-buffer tail)))))))

(ert-deftest clatter-url-preview-fetch-sentinel-cleans-up ()
  "A completed curl sentinel inserts the title and releases its resources."
  (let ((clatter-url-preview--pending (make-hash-table :test 'equal))
        (clatter-url-preview--cache (make-hash-table :test 'equal))
        sentinel process output-buffer)
    (with-temp-buffer
      (insert "https://example.com/one\n")
      (add-text-properties (point-min) (1- (point-max))
                           '(clatter-text "https://example.com/one"))
      (let ((start (copy-marker (point-min)))
            (end (copy-marker (point-max)))
            (tail (copy-marker (point-max))))
        (let ((anchor
               (clatter-url-preview--make-anchor-record
                :buffer (current-buffer)
                :start start
                :end end
                :tail tail
                :text "https://example.com/one"
                :remaining 1)))
          (setq process 'fake-process)
          (cl-letf (((symbol-function 'start-process)
                     (lambda (_name buffer &rest _args)
                       (setq output-buffer buffer)
                       process))
                    ((symbol-function 'set-process-sentinel)
                     (lambda (_process function)
                       (setq sentinel function)))
                    ((symbol-function 'process-status)
                     (lambda (_process) 'exit))
                    ((symbol-function 'process-exit-status)
                     (lambda (_process) 0))
                    ((symbol-function 'process-buffer)
                     (lambda (_process) output-buffer))
                    ((symbol-function 'process-live-p)
                     (lambda (_process) nil))
                    ((symbol-function 'delete-process)
                     (lambda (_process) nil)))
            (clatter-url-preview--fetch
             "https://example.com/one" (current-buffer) anchor)
            (with-current-buffer output-buffer
              (insert "<html><title>Example title</title></html>"))
            (funcall sentinel process "finished"))
          (should (equal (buffer-string)
                         "https://example.com/one\n↳ Example title\n"))
          (should (equal (gethash "https://example.com/one"
                                  clatter-url-preview--cache)
                         "Example title"))
          (should-not (gethash "https://example.com/one"
                               clatter-url-preview--pending))
          (should-not (buffer-live-p output-buffer))
          (should-not (marker-buffer start))
          (should-not (marker-buffer end))
          (should-not (marker-buffer tail)))))))

(ert-deftest clatter-url-preview-stale-anchor-is-safe-at-buffer-end ()
  "A truncated anchor is rejected without reading past point-max."
  (with-temp-buffer
    (insert "message")
    (let ((start (copy-marker (point-max)))
          (end (copy-marker (point-max)))
          (tail (copy-marker (point-max))))
      (let ((anchor
             (clatter-url-preview--make-anchor-record
              :buffer (current-buffer)
              :start start
              :end end
              :tail tail
              :text "message"
              :remaining 1)))
        (should-not (clatter-url-preview--anchor-live-p anchor))))))

(provide 'test-url-preview)

;;; test-url-preview.el ends here
