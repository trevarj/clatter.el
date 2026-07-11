;;; clatter-smart.el --- Smart noise suppression for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT
;; URL: https://github.com/parenworks/clatter.el

;;; Commentary:

;; A simple signal-to-noise filter for clatter buffers.  Each nick
;; accumulates a count of signal messages (for example PRIVMSG) versus
;; noise messages (for example JOIN, PART, QUIT).  When a nick's
;; signal-to-noise ratio falls below `clatter-smart-threshold', its
;; noisy events are hidden via the buffer invisibility spec.

;;; Code:

(defcustom clatter-smart-noise '(join part quit nick away)
  "Message types considered to be noise in clatter's smart filter."
  :type '(repeat (choice (const :tag "JOIN" join)
                         (const :tag "PART" part)
                         (const :tag "QUIT" quit)
                         (const :tag "NICK" nick)
                         (const :tag "MODE" mode)
                         (const :tag "AWAY" away)))
  :group 'clatter)

(defcustom clatter-smart-enabled t
  "When non-nil, hide smart-filtered noise in newly created buffers.

This adds the `noise' invisibility category to new clatter buffers when
`clatter-smart-noise' is non-nil.  Explicit `clatter-suppress-messages'
settings continue to be honored independently."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-smart-threshold 0.5
  "SNR threshold under which noisy message types are hidden."
  :type 'number
  :group 'clatter)

(defvar clatter-smart-data nil
  "Count of noisy and non-noisy messages, keyed by nick.")

(defun clatter-smart-on (buf)
  "Get smart filter data for BUF."
  (with-current-buffer buf
    (or clatter-smart-data
        (setq-local clatter-smart-data
                    (make-hash-table :test 'equal)))))

(defun clatter-smart-put (buf nick elt)
  "Record ELT for NICK in BUF and return the SNR value."
  ;; (stringp elt) => t implies NICK changed to ELT.
  ;; Normalize nicks.
  (setq nick (downcase nick))
  (when (stringp elt)
    (setq elt (downcase elt)))
  (let* ((is-nick-change (and (stringp nick)
                              (stringp elt)))
         (data (clatter-smart-on buf))
         (signal-noise (gethash nick data))
         (signal-count (or (car signal-noise) 1))
         (noise-count (or (cdr signal-noise) 1))
         (is-noise (memq (if is-nick-change 'nick elt) clatter-smart-noise)))
    (when is-nick-change
      (remhash nick data)
      (setq nick elt))
    (setq signal-count (+ signal-count (if is-noise 0 1)))
    (setq noise-count (+ noise-count (if is-noise 1 0)))
    (puthash nick (cons signal-count noise-count) data)
    ;; Float division: integer division would collapse the ratio to 0 or 1
    ;; and make `clatter-smart-threshold' meaningless.
    (/ (float signal-count) noise-count)))

(defun clatter-smart-eval (buf nick elt)
  "Record ELT for NICK in BUF and return whether NICK is noisy."
  (< (clatter-smart-put buf nick elt) clatter-smart-threshold))

(provide 'clatter-smart)

;;; clatter-smart.el ends here
