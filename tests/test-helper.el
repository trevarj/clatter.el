;;; test-helper.el --- Test helpers for clatter.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Common helpers, mocks, and fixtures for clatter.el ERT tests.
;; Load this before running any test file.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add project root to load-path
(let ((project-dir (file-name-directory
                    (directory-file-name
                     (file-name-directory
                      (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-dir))

;; Load core modules
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-handlers)

;; --- Mock Connection ---

(defvar clatter-test--sent-lines nil
  "List of lines sent via `clatter-send' during tests.")

(defun clatter-test-make-connection (&optional network-id nick)
  "Create a mock connection for testing.
NETWORK-ID defaults to \"testnet\", NICK to \"testnick\"."
  (let ((conn (clatter-connection--create
               :network-id (or network-id "testnet")
               :process nil
               :state :connected
               :nick (or nick "testnick")
               :sasl-state :done
               :cap-negotiating nil
               :cap-enabled '("server-time" "message-tags" "batch"
                              "echo-message" "multi-prefix")
               :active-batches (make-hash-table :test 'equal)
               :pending-labels (make-hash-table :test 'equal)
               :label-counter 0
               :reconnect-enabled t
               :reconnect-attempts 0
               :reconnect-timer nil
               :desired-nick (or nick "testnick")
               :nick-reclaim-timer nil
               :last-activity (float-time)
               :ping-sent-time nil
               :health-timer nil
               :recv-buffer ""
               :isupport (make-hash-table :test 'equal)
               :-motd-lines nil
               :-whois-data nil)))
    ;; Register in global connections table
    (puthash (clatter-connection-network-id conn) conn clatter-connections)
    conn))

(defun clatter-test-make-connection-with-caps (caps &optional network-id nick)
  "Create a mock connection with specific CAPS enabled."
  (let ((conn (clatter-test-make-connection network-id nick)))
    (setf (clatter-connection-cap-enabled conn) caps)
    conn))

(defun clatter-test-cleanup ()
  "Clean up after tests."
  (clrhash clatter-connections)
  (setq clatter-test--sent-lines nil)
  ;; Some UI tests create named Clatter buffers directly rather than through
  ;; `clatter-test-with-buffer'.  Remove them here so later tests do not see
  ;; stale activity when collecting global tracker state.
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-prefix-p "*clatter:" (buffer-name buf)))
      (kill-buffer buf)))
  (setq clatter--buffer-alist nil))

;; --- Mock Send ---

(defun clatter-test-mock-send (conn line)
  "Mock `clatter-send' that captures lines instead of sending."
  (push line clatter-test--sent-lines))

(defmacro clatter-test-with-mock-send (&rest body)
  "Execute BODY with `clatter-send' replaced by mock."
  `(cl-letf (((symbol-function 'clatter-send) #'clatter-test-mock-send))
     (setq clatter-test--sent-lines nil)
     ,@body))

(defun clatter-test-last-sent ()
  "Return the most recently sent line."
  (car clatter-test--sent-lines))

(defun clatter-test-sent-matching (regexp)
  "Return first sent line matching REGEXP."
  (cl-find-if (lambda (line) (string-match-p regexp line))
              clatter-test--sent-lines))

;; --- Message Construction Helpers ---

(defun clatter-test-make-privmsg (sender target text &optional tags)
  "Create a raw IRC PRIVMSG line from SENDER to TARGET with TEXT."
  (let ((tag-str (or tags ""))
        (prefix (format "%s!~%s@user/%s" sender sender sender)))
    (if (string-empty-p tag-str)
        (format ":%s PRIVMSG %s :%s" prefix target text)
      (format "@%s :%s PRIVMSG %s :%s" tag-str prefix target text))))

(defun clatter-test-make-tagmsg (sender target tags)
  "Create a raw IRC TAGMSG line from SENDER to TARGET with TAGS."
  (let ((prefix (format "%s!~%s@user/%s" sender sender sender)))
    (format "@%s :%s TAGMSG %s" tags prefix target)))

(defun clatter-test-parse (raw-line)
  "Parse a raw IRC line into a `clatter-message'."
  (clatter-parse-line raw-line))

;; --- Hook Capture ---

(defvar clatter-test--hook-calls nil
  "List of hook calls captured during tests.")

(defmacro clatter-test-capture-hook (hook &rest body)
  "Execute BODY, capturing all calls to HOOK.
Returns the list of argument lists passed to the hook."
  (declare (indent 1))
  `(let ((clatter-test--hook-calls nil))
     (let ((capture-fn (lambda (&rest args)
                         (push args clatter-test--hook-calls))))
       (add-hook ',hook capture-fn)
       (unwind-protect
           (progn ,@body)
         (remove-hook ',hook capture-fn)))
     (nreverse clatter-test--hook-calls)))

;; --- Buffer Helpers ---

(defmacro clatter-test-with-buffer (&rest body)
  "Execute BODY in a temporary clatter-mode buffer."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer " *clatter-test*")))
     (unwind-protect
         (with-current-buffer buf
           (clatter-mode)
           ,@body)
       (kill-buffer buf))))

(provide 'test-helper)

;;; test-helper.el ends here
