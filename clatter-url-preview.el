;;; clatter-url-preview.el --- URL title preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Asynchronous URL title fetching for clatter.el.
;; When a URL is posted in a channel, fetches the page title
;; and displays it inline as a system message.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-protocol)
(require 'clatter-model)
(require 'clatter-ui)

;; --- Configuration ---

(defcustom clatter-url-preview-enable nil
  "Enable automatic URL title preview."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-url-preview-max-length 200
  "Maximum length for displayed URL titles."
  :type 'integer
  :group 'clatter)

(defcustom clatter-url-preview-timeout 10
  "Timeout in seconds for URL fetching."
  :type 'integer
  :group 'clatter)

(defcustom clatter-url-preview-exclude-patterns
  '("\\.png$" "\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.webp$" "\\.svg$"
    "\\.pdf$" "\\.zip$" "\\.tar" "\\.mp3$" "\\.mp4$" "\\.mkv$"
    "\\.iso$" "\\.deb$" "\\.rpm$")
  "URL patterns to skip title fetching for (binary/media files)."
  :type '(repeat regexp)
  :group 'clatter)

;; --- Internal ---

(defvar clatter-url-preview--cache (make-hash-table :test 'equal)
  "Cache of URL -> title mappings to avoid re-fetching.")

(defvar clatter-url-preview--cache-max 500
  "Maximum number of cached URL titles.")

(defvar clatter-url-preview--pending (make-hash-table :test 'equal)
  "URLs currently being fetched (to avoid duplicate requests).")

(cl-defstruct (clatter-url-preview--anchor
               (:constructor clatter-url-preview--make-anchor-record))
  "Insertion state shared by every preview belonging to one message.

START and END delimit the original rendered message.  TAIL is advanced after
each preview, so synchronous cache hits retain URL order and later previews
are not inserted before earlier ones.  REMAINING owns the markers: the last
cache hit or asynchronous request detaches them."
  buffer start end tail text remaining)

(defun clatter-url-preview--should-fetch-p (url)
  "Return non-nil if URL should be fetched for title preview."
  (and clatter-url-preview-enable
       (string-match-p "^https?://" url)
       (not (gethash url clatter-url-preview--pending))
       (not (cl-some (lambda (pat) (string-match-p pat url))
                     clatter-url-preview-exclude-patterns))))

(defun clatter-url-preview--extract-title (html)
  "Extract the <title> content from HTML string."
  (when (string-match "<title[^>]*>\\([^<]*\\)</title>" html)
    (let ((raw (match-string 1 html)))
      (setq raw (replace-regexp-in-string "[\n\r\t]+" " " raw))
      (setq raw (string-trim raw))
      ;; Decode HTML entities
      (setq raw (replace-regexp-in-string "&amp;" "&" raw))
      (setq raw (replace-regexp-in-string "&lt;" "<" raw))
      (setq raw (replace-regexp-in-string "&gt;" ">" raw))
      (setq raw (replace-regexp-in-string "&quot;" "\"" raw))
      (setq raw (replace-regexp-in-string "&#39;" "'" raw))
      (setq raw (replace-regexp-in-string "&nbsp;" " " raw))
      (when (> (length raw) 0)
        (if (> (length raw) clatter-url-preview-max-length)
            (concat (substring raw 0 clatter-url-preview-max-length) "...")
          raw)))))

(defun clatter-url-preview--anchor-live-p (anchor)
  "Return non-nil when ANCHOR still refers to its source message."
  (and (clatter-url-preview--anchor-p anchor)
       (let ((buffer (clatter-url-preview--anchor-buffer anchor))
             (start (clatter-url-preview--anchor-start anchor))
             (end (clatter-url-preview--anchor-end anchor)))
         (and (buffer-live-p buffer)
              (markerp start) (markerp end)
              (eq (marker-buffer start) buffer)
              (eq (marker-buffer end) buffer)
              (with-current-buffer buffer
                (let ((start-pos (marker-position start))
                      (end-pos (marker-position end)))
                  (and (<= (point-min) start-pos)
                       (< start-pos (point-max))
                       (<= end-pos (point-max))
                       (< start-pos end-pos)
                       ;; Actual UI messages carry this property.  The
                       ;; fallback keeps the helper usable in minimal tests.
                       (or (null (clatter-url-preview--anchor-text anchor))
                           (equal (get-text-property start-pos 'clatter-text)
                                  (clatter-url-preview--anchor-text anchor))))))))))

(defun clatter-url-preview--dispose-anchor (anchor)
  "Detach every marker owned by ANCHOR."
  (when (clatter-url-preview--anchor-p anchor)
    (dolist (marker (list (clatter-url-preview--anchor-start anchor)
                          (clatter-url-preview--anchor-end anchor)
                          (clatter-url-preview--anchor-tail anchor)))
      (when (markerp marker) (set-marker marker nil)))))

(defun clatter-url-preview--release-anchor (anchor)
  "Release one URL request's ownership of ANCHOR."
  (when (clatter-url-preview--anchor-p anchor)
    (setf (clatter-url-preview--anchor-remaining anchor)
          (1- (clatter-url-preview--anchor-remaining anchor)))
    (when (<= (clatter-url-preview--anchor-remaining anchor) 0)
      (clatter-url-preview--dispose-anchor anchor))))

(defun clatter-url-preview--insert (title buffer anchor)
  "Insert TITLE below ANCHOR's original message.

ANCHOR's tail marker is advanced after insertion, retaining cached URL order.
No text is inserted if truncation or deletion has removed the source message."
  (cond
   ((clatter-url-preview--anchor-p anchor)
    (when (and (eq buffer (clatter-url-preview--anchor-buffer anchor))
               (clatter-url-preview--anchor-live-p anchor))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (prefix (make-string (1+ clatter-nick-column-width) ?\s)))
          (save-excursion
            (goto-char (clatter-url-preview--anchor-tail anchor))
            (insert (propertize (format "↳ %s\n" title)
                                'face '(:foreground "#89ddff" :slant italic)
                                'read-only t
                                'front-sticky t
                                'line-prefix prefix
                                'wrap-prefix prefix))
            ;; A nil-insertion-type marker stays before unrelated messages,
            ;; but must move past this preview to form a stable local tail.
            (set-marker (clatter-url-preview--anchor-tail anchor) (point)))))))
   ;; Compatibility for callers of the former low-level marker API.
   ((and (buffer-live-p buffer) (markerp anchor) (marker-buffer anchor))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (prefix (make-string (1+ clatter-nick-column-width) ?\s)))
        (save-excursion
          (goto-char anchor)
          (insert (propertize (format "↳ %s\n" title)
                              'face '(:foreground "#89ddff" :slant italic)
                              'read-only t
                              'front-sticky t
                              'line-prefix prefix
                              'wrap-prefix prefix))))))))

(defun clatter-url-preview--fetch (url buffer anchor)
  "Asynchronously fetch URL and display its title in BUFFER at ANCHOR.
Uses curl subprocess to avoid blocking Emacs on DNS/TLS."
  (let ((clean-url (substring-no-properties url))
        (fetch-buffer (generate-new-buffer " *clatter-url-fetch*")))
    (puthash clean-url t clatter-url-preview--pending)
    (condition-case nil
        (let ((process (start-process
                      "clatter-url-preview" fetch-buffer
                      "curl" "-sL" "-m" (number-to-string clatter-url-preview-timeout)
                      "-o" "-" "-r" "0-16384"
                      "-H" "User-Agent: Mozilla/5.0 (compatible; clatter.el)"
                      "-H" "Accept: text/html"
                      clean-url)))
          (set-process-sentinel
           process
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let ((output-buffer (process-buffer process)))
                 (remhash clean-url clatter-url-preview--pending)
                 (unwind-protect
                     (when (and (eq (process-exit-status process) 0)
                                (buffer-live-p output-buffer))
                       (let ((body (with-current-buffer output-buffer
                                     (buffer-string))))
                         (let ((title (clatter-url-preview--extract-title body)))
                           (when (and title (buffer-live-p buffer))
                             (when (> (hash-table-count clatter-url-preview--cache)
                                      clatter-url-preview--cache-max)
                               (clrhash clatter-url-preview--cache))
                             (puthash clean-url title clatter-url-preview--cache)
                             (clatter-url-preview--insert title buffer anchor))))
                   (when (buffer-live-p output-buffer)
                     (kill-buffer output-buffer))
                   (clatter-url-preview--release-anchor anchor))))))))
      (error
       (remhash clean-url clatter-url-preview--pending)
       (when (buffer-live-p fetch-buffer)
         (kill-buffer fetch-buffer))
       (clatter-url-preview--release-anchor anchor)))))

;; --- Hook Handler ---

(defun clatter-url-preview--message-end-marker (buffer text)
  "Return a fixed marker immediately after TEXT's rendered message in BUFFER.

For `newest-first', the messages marker is immediately before the newly
inserted message; for `oldest-first', it is immediately after it.  The UI
stores the unformatted message text as `clatter-text', which lets this work
for either order and for wrapped messages."
  (with-current-buffer buffer
    (let* ((messages-marker (or clatter--messages-marker (point-max)))
           (position (marker-position messages-marker))
           (before (and (> position (point-min)) (1- position)))
           (end (cond
                 ((equal (get-text-property position 'clatter-text) text)
                  (next-single-property-change position 'clatter-text nil (point-max)))
                 ((and before
                       (equal (get-text-property before 'clatter-text) text))
                  position)
                 (t position))))
      (copy-marker end))))

(defun clatter-url-preview--message-anchor (buffer text request-count)
  "Return a shared anchor for REQUEST-COUNT previews of TEXT in BUFFER.

The rendered message's `clatter-text' property is retained as an identity
check, preventing a delayed curl sentinel from inserting at a marker left
behind by buffer truncation."
  (with-current-buffer buffer
    (let* ((end (clatter-url-preview--message-end-marker buffer text))
           (end-pos (marker-position end))
           (start (or (previous-single-property-change
                       end-pos 'clatter-text nil (point-min))
                      (point-min)))
           ;; `previous-single-property-change' returns the beginning when
           ;; END is just after a property run; use the property's value at
           ;; the first character to distinguish real UI messages from a
           ;; minimal buffer used by a low-level caller.
           (source-text (and (< start end-pos)
                             (get-text-property start 'clatter-text)))
           (tail (copy-marker end-pos)))
      (set-marker-insertion-type end nil)
      (set-marker-insertion-type tail nil)
      (clatter-url-preview--make-anchor-record
       :buffer buffer :start (copy-marker start) :end end :tail tail
       :text source-text :remaining request-count))))

(defun clatter-url-preview--on-privmsg (conn sender target text _server-time)
  "Check TEXT from SENDER for URLs and fetch titles for TARGET buffer on CONN."
  (when clatter-url-preview-enable
    (let* ((network (clatter-connection-network-id conn))
           (my-nick (clatter-connection-nick conn))
           (buf-target (if (clatter-channel-name-p target)
                           target
                         (if (string-equal target my-nick) sender target)))
           (buf (clatter-get-buffer network buf-target))
           (pos 0)
           requests)
      (when buf
        (while (string-match "https?://[^ \t\n\r<>\"']+" text pos)
          (let ((url (match-string 0 text)))
            (setq pos (match-end 0))
            (when (clatter-url-preview--should-fetch-p url)
              (push url requests))))
        ;; Build exactly one tail per source message.  In particular, do not
        ;; make a fresh marker for each cache hit: nil-insertion markers at
        ;; the same position otherwise reverse source URL order.
        (when requests
          (setq requests (nreverse requests))
          (let ((anchor (clatter-url-preview--message-anchor
                         buf text (length requests))))
            (dolist (url requests)
              (let ((cached (gethash url clatter-url-preview--cache)))
                (if cached
                    (unwind-protect
                        (clatter-url-preview--insert cached buf anchor)
                      (clatter-url-preview--release-anchor anchor))
                  (clatter-url-preview--fetch url buf anchor))))))))))

;; --- Init/Teardown ---

(defun clatter-url-preview-init ()
  "Register URL preview hooks."
  ;; The UI hook must render the source message first.  The preview anchor is
  ;; located relative to that rendered message, and a default-depth hook would
  ;; run before the UI because `add-hook' prepends same-depth functions.
  (add-hook 'clatter-privmsg-hook #'clatter-url-preview--on-privmsg 90))

(defun clatter-url-preview-teardown ()
  "Remove URL preview hooks."
  (remove-hook 'clatter-privmsg-hook #'clatter-url-preview--on-privmsg))

;; Enabled by `clatter-setup' when `clatter-url-preview-enable' is
;; non-nil, so that merely loading this file has no side effects.

(provide 'clatter-url-preview)

;;; clatter-url-preview.el ends here
