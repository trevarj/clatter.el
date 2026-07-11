;;; clatter-track.el --- Buffer activity tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Smart activity tracker for clatter.el buffers.
;; Tracks unread messages and mentions per channel,
;; displays activity in the global mode-line,
;; and integrates with consult for buffer switching.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-model)

;; --- Configuration ---

(defcustom clatter-track-enabled t
  "Enable activity tracking in the global mode-line."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-track-position 'after-modes
  "Where to place the activity indicator in the global mode-line.
Valid values: before-modes, after-modes, end."
  :type '(choice (const :tag "Before modes" before-modes)
                 (const :tag "After modes" after-modes)
                 (const :tag "End" end))
  :group 'clatter)

(defcustom clatter-track-muted-channels nil
  "List of channel names to dim in the activity tracker.
Muted channels remain visible in the tracker, using
`clatter-track-muted'.  Use `clatter-track-exclude-targets' to hide
targets completely.
Example: (\"#spam\" \"#bots\")"
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-track-exclude-targets nil
  "List of targets to hide completely from the activity tracker.
Excluded targets do not appear in the mode-line indicator, activity list,
activity switch command, or Consult activity source.  Target names use the
same spelling as `clatter--target', for example \"*server*\" or \"#spam\"."
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-track-faces-alist
  '((mention . clatter-track-mention)
    (activity . clatter-track-activity)
    (muted . clatter-track-muted))
  "Alist mapping activity types to faces for the track indicator."
  :type '(alist :key-type symbol :value-type face)
  :group 'clatter)

(defcustom clatter-track-shorten-names t
  "Shorten buffer names in the track indicator.
Removes the clatter: prefix and network name."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-track-show-counts t
  "Show unread message counts in the track indicator."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-track-in-buffer-mode-line nil
  "Show the activity crumbs in each clatter buffer's own mode line.
By default the track indicator is appended to the global
`mode-line-format', which clatter buffers override with their own
buffer-local mode line, so the crumbs are not visible while you are in a
clatter buffer.  When this is non-nil, the indicator is also inserted
into each clatter buffer's mode line (just before the trailing spaces),
so the crumbs appear everywhere.  Setting this through Customize or
`setopt' updates all existing clatter buffers immediately."
  :type 'boolean
  :group 'clatter
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'clatter-track--refresh-mode-lines)
           (clatter-track--refresh-mode-lines))))

;; --- Faces ---

(defface clatter-track-mention
  '((t :foreground "#ff5370" :weight bold))
  "Face for channels with unread mentions."
  :group 'clatter)

(defface clatter-track-activity
  '((t :foreground "#c3e88d"))
  "Face for channels with unread messages."
  :group 'clatter)

(defface clatter-track-muted
  '((t :foreground "#7c7c7c"))
  "Face for muted channels with activity."
  :group 'clatter)

(defface clatter-track-dm
  '((t :foreground "#ffcb6b" :weight bold))
  "Face for DM buffers with unread messages."
  :group 'clatter)

;; --- Track state ---

(defvar clatter-track--timer nil
  "Timer for periodic mode-line updates.")

(defvar clatter-track--string ""
  "Current track string for the mode-line.")

;; --- Track info collection ---

(defun clatter-track--buffer-info (buf)
  "Return activity info for BUF as plist, or nil if no activity.
Plist keys: :buffer :name :unread :mention :muted :dm"
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (derived-mode-p 'clatter-mode)
                 clatter--target
                 (not (member clatter--target clatter-track-exclude-targets))
                 (> clatter--unread-count 0))
        (let* ((target clatter--target)
               (is-channel (and target (string-match-p "^[#&!+]" target)))
               (is-muted (member target clatter-track-muted-channels))
               (display-name (if clatter-track-shorten-names
                                 target
                               (buffer-name buf))))
          (list :buffer buf
                :name display-name
                :unread clatter--unread-count
                :mention clatter--has-mention
                :muted is-muted
                :dm (not is-channel)))))))

(defun clatter-track--collect ()
  "Collect activity info from all clatter buffers.
Returns list of plists sorted by priority: mentions > DMs > activity."
  (let ((infos nil))
    (dolist (buf (buffer-list))
      (let ((info (clatter-track--buffer-info buf)))
        (when info
          (push info infos))))
    ;; Sort: mentions first, then DMs, then regular activity
    (sort infos
          (lambda (a b)
            (let ((a-mention (plist-get a :mention))
                  (b-mention (plist-get b :mention))
                  (a-dm (plist-get a :dm))
                  (b-dm (plist-get b :dm)))
              (cond
               ((and a-mention (not b-mention)) t)
               ((and b-mention (not a-mention)) nil)
               ((and a-dm (not b-dm)) t)
               ((and b-dm (not a-dm)) nil)
               (t (> (plist-get a :unread) (plist-get b :unread)))))))))

;; --- Format track string ---

(defun clatter-track--format-entry (info)
  "Format a single track INFO plist into a propertized string."
  (let* ((name (plist-get info :name))
         (unread (plist-get info :unread))
         (mention (plist-get info :mention))
         (muted (plist-get info :muted))
         (dm (plist-get info :dm))
         (face (cond
                (muted 'clatter-track-muted)
                (mention 'clatter-track-mention)
                (dm 'clatter-track-dm)
                (t 'clatter-track-activity)))
         (prefix (cond
                  (mention "@")
                  (dm "*")
                  (t "")))
         (count-str (if (and clatter-track-show-counts (> unread 0))
                        (format ":%d" unread)
                      "")))
    (propertize (format "%s%s%s" prefix name count-str)
                'face face
                'help-echo (format "%s - %d unread%s"
                                   name unread
                                   (if mention " (mentioned)" ""))
                'mouse-face 'highlight
                'local-map (clatter-track--make-click-map (plist-get info :buffer)))))

(defun clatter-track--make-click-map (buffer)
  "Return a keymap that switches to BUFFER on click."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (_event)
        (interactive "e")
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer)
          (clatter-clear-activity buffer))))
    map))

(defun clatter-track--format-string ()
  "Build the full track indicator string."
  (let ((infos (clatter-track--collect)))
    (if infos
        (concat " ["
                (mapconcat #'clatter-track--format-entry infos " ")
                "]")
      "")))

;; --- Mode-line integration ---

(defvar clatter-track-mode-line-item
  '(:eval clatter-track--string)
  "Mode-line construct showing clatter activity.")

(put 'clatter-track-mode-line-item 'risky-local-variable t)

(defun clatter-track--insert-mode-line-item (format)
  "Return mode-line FORMAT with the track item before the trailing spaces.
If the item is already present, FORMAT is returned unchanged."
  (if (memq 'clatter-track-mode-line-item format)
      format
    (let ((tail (member 'mode-line-end-spaces format)))
      (if tail
          (append (butlast format (length tail))
                  (list 'clatter-track-mode-line-item)
                  tail)
        (append format (list 'clatter-track-mode-line-item))))))

(defun clatter-track--refresh-mode-lines ()
  "Add or remove the track item in all clatter buffers' mode lines.
The presence of the item follows `clatter-track-in-buffer-mode-line'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'clatter-mode)
        (setq-local mode-line-format
                    (if clatter-track-in-buffer-mode-line
                        (clatter-track--insert-mode-line-item mode-line-format)
                      (delq 'clatter-track-mode-line-item
                            (copy-sequence mode-line-format))))
        (force-mode-line-update)))))

(defun clatter-track--update ()
  "Update the track string and force mode-line refresh."
  (let ((new-string (clatter-track--format-string)))
    (unless (string= new-string clatter-track--string)
      (setq clatter-track--string new-string)
      (force-mode-line-update t))))

;; --- Auto-clear on buffer switch ---

(defun clatter-track--on-buffer-switch ()
  "Clear activity for the current buffer when it becomes visible."
  (when (and (derived-mode-p 'clatter-mode)
             (> clatter--unread-count 0))
    (clatter-clear-activity (current-buffer))
    (clatter-track--update)))

;; --- Consult integration ---

(defun clatter-track-buffer-source ()
  "Consult buffer source for clatter buffers with activity.
Use with `consult-buffer' by adding to `consult-buffer-sources'."
  (let ((infos (clatter-track--collect)))
    (mapcar (lambda (info)
              (buffer-name (plist-get info :buffer)))
            infos)))

(defvar clatter-track--consult-source
  (when (featurep 'consult)
    (list :name "IRC Activity"
          :narrow ?i
          :category 'buffer
          :face 'clatter-track-activity
          :items #'clatter-track-buffer-source
          :action (lambda (name)
                    (let ((buf (get-buffer name)))
                      (when buf
                        (switch-to-buffer buf)
                        (clatter-clear-activity buf))))))
  "Consult source for clatter buffers with activity.
Add to `consult-buffer-sources' to enable.")

;; --- Interactive commands ---

(defun clatter-track-switch ()
  "Switch to the clatter buffer with the most urgent activity.
Priority: mentions > DMs > highest unread count."
  (interactive)
  (let ((infos (clatter-track--collect)))
    (if infos
        (let ((buf (plist-get (car infos) :buffer)))
          (switch-to-buffer buf)
          (clatter-clear-activity buf))
      (message "No clatter activity"))))

(defun clatter-track-list ()
  "Display all clatter buffer activity in a temporary buffer."
  (interactive)
  (let ((infos (clatter-track--collect)))
    (if infos
        (with-current-buffer (get-buffer-create "*clatter-activity*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "CLatter Activity\n")
            (insert (make-string 40 ?-) "\n\n")
            (dolist (info infos)
              (let ((name (plist-get info :name))
                    (unread (plist-get info :unread))
                    (mention (plist-get info :mention))
                    (dm (plist-get info :dm))
                    (muted (plist-get info :muted)))
                (insert (format "  %-25s %3d unread%s%s%s\n"
                                name unread
                                (if mention "  @mentioned" "")
                                (if dm "  DM" "")
                                (if muted "  (muted)" "")))))
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)))
      (message "No clatter activity"))))

(defun clatter-track-mute (channel)
  "Add CHANNEL to the muted list."
  (interactive
   (list (completing-read "Mute channel: "
                          (let (channels)
                            (dolist (buf (buffer-list))
                              (with-current-buffer buf
                                (when (and (derived-mode-p 'clatter-mode)
                                           clatter--target
                                           (string-match-p "^[#&!+]" clatter--target))
                                  (push clatter--target channels))))
                            channels))))
  (unless (member channel clatter-track-muted-channels)
    (push channel clatter-track-muted-channels)
    (message "Muted %s" channel)
    (clatter-track--update)))

(defun clatter-track-unmute (channel)
  "Remove CHANNEL from the muted list."
  (interactive
   (list (completing-read "Unmute channel: " clatter-track-muted-channels)))
  (setq clatter-track-muted-channels
        (delete channel clatter-track-muted-channels))
  (message "Unmuted %s" channel)
  (clatter-track--update))

;; --- Enable/disable ---

(defun clatter-track-enable ()
  "Enable the activity tracker."
  (interactive)
  ;; Install mode-line item
  (unless (memq 'clatter-track-mode-line-item
                (default-value 'mode-line-format))
    (let ((fmt (default-value 'mode-line-format)))
      (set-default 'mode-line-format
                   (append fmt (list 'clatter-track-mode-line-item)))))
  ;; Start update timer
  (when clatter-track--timer
    (cancel-timer clatter-track--timer))
  (setq clatter-track--timer
        (run-with-timer 1 2 #'clatter-track--update))
  ;; Hook into buffer switches
  (add-hook 'window-buffer-change-functions #'clatter-track--window-change)
  ;; Hook into clatter activity
  (add-hook 'clatter-privmsg-hook #'clatter-track--on-activity)
  (add-hook 'clatter-action-hook #'clatter-track--on-activity-action)
  (add-hook 'clatter-notice-hook #'clatter-track--on-activity-notice)
  ;; Register consult source if available
  (when (and (featurep 'consult)
             (boundp 'consult-buffer-sources)
             clatter-track--consult-source)
    (add-to-list 'consult-buffer-sources clatter-track--consult-source))
  (when (called-interactively-p 'interactive)
    (message "[clatter-track] Activity tracking enabled")))

(defun clatter-track-disable ()
  "Disable the activity tracker."
  (interactive)
  (when clatter-track--timer
    (cancel-timer clatter-track--timer)
    (setq clatter-track--timer nil))
  (remove-hook 'window-buffer-change-functions #'clatter-track--window-change)
  (remove-hook 'clatter-privmsg-hook #'clatter-track--on-activity)
  (remove-hook 'clatter-action-hook #'clatter-track--on-activity-action)
  (remove-hook 'clatter-notice-hook #'clatter-track--on-activity-notice)
  (setq clatter-track--string "")
  (force-mode-line-update t)
  (when (called-interactively-p 'interactive)
    (message "[clatter-track] Activity tracking disabled")))

(defun clatter-track--window-change (&rest _)
  "Update the tracker when the selected window's buffer changes.
Ignores its arguments; suitable for `window-buffer-change-functions'."
  (clatter-track--on-buffer-switch))

;; --- Activity hooks ---

(defun clatter-track--on-activity (_conn _sender _target _text &rest _args)
  "Update track on PRIVMSG activity."
  (clatter-track--update))

(defun clatter-track--on-activity-action (_conn _sender _target _text &rest _args)
  "Update track on ACTION activity."
  (clatter-track--update))

(defun clatter-track--on-activity-notice (_conn _sender _target _text &rest _args)
  "Update track on NOTICE activity."
  (clatter-track--update))

;; Tracking is enabled by `clatter-setup' when `clatter-track-enabled'
;; is non-nil, so that merely loading this file has no side effects.

(provide 'clatter-track)

;;; clatter-track.el ends here
