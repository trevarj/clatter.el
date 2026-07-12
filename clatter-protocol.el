;;; clatter-protocol.el --- IRC protocol parsing and formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; IRC message parsing and formatting for clatter.el.
;; Handles RFC 1459/2812 and IRCv3 message format including tags.
;; Ported from CLatter's core/protocol.lisp.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'clatter-config)

;; --- IRC Message Structure ---

(cl-defstruct (clatter-message (:constructor clatter-message-create))
  "An IRC protocol message with optional IRCv3 tags."
  tags      ; raw tags string (before parsing)
  prefix    ; raw prefix string (nick!user@host or servername)
  command   ; command string (uppercased)
  params)   ; list of parameter strings

;; --- Prefix Parsing ---

(defun clatter-parse-prefix (prefix-str)
  "Parse nick!user@host PREFIX-STR into (nick user host).
Returns (servername nil nil) if no ! or @ found."
  (when prefix-str
    (let ((bang (cl-position ?! prefix-str))
          (at (cl-position ?@ prefix-str)))
      (if (and bang at (< bang at))
          (list (substring prefix-str 0 bang)
                (substring prefix-str (1+ bang) at)
                (substring prefix-str (1+ at)))
        (list prefix-str nil nil)))))

(defun clatter-join-prefix (prefix)
  "Join a split (nick user host) prefix back into a nick!user@host."
  (if (and (stringp (car prefix))
           (stringp (cadr prefix))
           (stringp (caddr prefix)))
      (apply #'format "%s!%s@%s" prefix)
    (car prefix)))

(defun clatter-prefix-p (obj)
  "Return t if OBJ is a prefix, nil otherwise."
  (and (listp obj)
       ; nick (required)
       (stringp (car obj))
       (progn
         (setq obj (cdr obj))
         (and
          (or
           ; (user host)
           (and (stringp (car obj))
                (stringp (cadr obj)))
           ; (nil nil)
           (and (not (car obj))
                (not (cadr obj))))
          (not (cddr obj))))))

(defun clatter-prefix-nick (prefix)
  "Extract nick from parsed PREFIX list."
  (car prefix))

;; --- IRC Case Mapping ---

(defun clatter-casefold-nick (nick &optional case-mapping)
  "Return NICK folded according to IRC CASE-MAPPING.
CASE-MAPPING should be the CASEMAPPING ISUPPORT value.  Nil uses
the RFC1459 mapping, matching IRC's historical default."
  (let ((nick (downcase nick)))
    (pcase case-mapping
      ("ascii" nick)
      ("strict-rfc1459"
       (string-replace "\\" "|"
                       (string-replace "]" "}"
                                       (string-replace "[" "{" nick))))
      (_
       (string-replace "^" "~"
                       (string-replace "\\" "|"
                                       (string-replace "]" "}"
                                                       (string-replace "[" "{" nick))))))))

(defun clatter-nick-equal-p (first second &optional case-mapping)
  "Return non-nil when FIRST and SECOND are equal IRC nicks.
CASE-MAPPING should be the CASEMAPPING ISUPPORT value.  Nil uses
the RFC1459 mapping."
  (string-equal (clatter-casefold-nick first case-mapping)
                (clatter-casefold-nick second case-mapping)))

;; --- Input Sanitization ---

(defun clatter-sanitize-input (text)
  "Remove CR, LF, and NUL from TEXT to prevent IRC command injection."
  (when text
    (replace-regexp-in-string "[\r\n\0]" "" text)))

(defun clatter-validate-input (text &optional max-length)
  "Validate and sanitize TEXT for sending to IRC.
Returns (sanitized . warning) cons cell.
MAX-LENGTH defaults to `clatter-max-line-length'."
  (let ((max-len (or max-length clatter-max-line-length)))
    (cond
     ((or (null text) (string-empty-p text))
      (cons "" "Empty message"))
     (t
      (let* ((sanitized (clatter-sanitize-input text))
             (truncated (if (> (length sanitized) max-len)
                            (substring sanitized 0 max-len)
                          sanitized))
             (warning (cond
                       ((not (string= text sanitized))
                        "Message contained invalid characters (removed)")
                       ((not (string= sanitized truncated))
                        (format "Message truncated to %d characters" max-len))
                       (t nil))))
        (cons truncated warning))))))

;; --- Channel Name Validation ---

(defun clatter-channel-prefix-p (char)
  "Check if CHAR is a valid IRC channel prefix (#, &, +, !)."
  (memq char '(?# ?& ?+ ?!)))

(defun clatter-channel-name-p (string)
  "Check if STRING looks like a channel name."
  (and (stringp string)
       (> (length string) 1)
       (clatter-channel-prefix-p (aref string 0))))

(defun clatter-valid-channel-name-p (name)
  "Validate channel NAME per RFC 2812.
Must start with channel prefix, no spaces/commas/bell, max 50 chars."
  (and (clatter-channel-name-p name)
       (<= (length name) 50)
       (not (string-match-p "[ ,\a]" name))))

;; --- IRC Formatting Code Stripping ---

(defun clatter-strip-irc-formatting (text)
  "Remove IRC formatting codes from TEXT.
Strips bold (^B), color (^C), reset (^O), reverse (^R),
italic (^]), and underline (^_)."
  (when text
    (let ((chars nil)
          (i 0)
          (len (length text)))
      (while (< i len)
        (let ((ch (aref text i)))
          (cond
           ;; Bold, reset, reverse, italic, underline
           ((memq ch '(#x02 #x0F #x16 #x1D #x1F))
            (cl-incf i))
           ;; Color code - skip digits
           ((= ch #x03)
            (cl-incf i)
            ;; Skip foreground (up to 2 digits)
            (let ((count 0))
              (while (and (< i len) (< count 2)
                          (<= ?0 (aref text i)) (<= (aref text i) ?9))
                (cl-incf i)
                (cl-incf count)))
            ;; Skip comma + background (up to 2 digits)
            (when (and (< i len) (= (aref text i) ?,))
              (cl-incf i)
              (let ((count 0))
                (while (and (< i len) (< count 2)
                            (<= ?0 (aref text i)) (<= (aref text i) ?9))
                  (cl-incf i)
                  (cl-incf count)))))
           ;; Normal character
           (t
            (push ch chars)
            (cl-incf i)))))
      (apply #'string (nreverse chars)))))

;; --- IRC Message Parsing ---

(defun clatter-parse-line (line)
  "Parse an IRC protocol LINE into a `clatter-message'.
Handles IRCv3 tags (@key=value;...), prefix (:nick!user@host),
command, and parameters including trailing."
  (condition-case err
      (let ((pos 0)
            (len (length line))
            tags prefix command params)
        ;; Skip leading whitespace
        (while (and (< pos len) (= (aref line pos) ?\s))
          (cl-incf pos))

        ;; Parse IRCv3 tags (starts with @)
        (when (and (< pos len) (= (aref line pos) ?@))
          (cl-incf pos)
          (let ((end (cl-position ?\s line :start pos)))
            (when end
              (setq tags (substring line pos end))
              (setq pos (1+ end)))))

        ;; Skip whitespace
        (while (and (< pos len) (= (aref line pos) ?\s))
          (cl-incf pos))

        ;; Parse prefix (starts with :)
        (when (and (< pos len) (= (aref line pos) ?:))
          (cl-incf pos)
          (let ((end (cl-position ?\s line :start pos)))
            (when end
              (setq prefix (substring line pos end))
              (setq pos (1+ end)))))

        ;; Skip whitespace
        (while (and (< pos len) (= (aref line pos) ?\s))
          (cl-incf pos))

        ;; Parse command
        (let ((end (or (cl-position ?\s line :start pos) len)))
          (setq command (upcase (substring line pos end)))
          (setq pos end))

        ;; Parse params
        (while (< pos len)
          ;; Skip whitespace
          (while (and (< pos len) (= (aref line pos) ?\s))
            (cl-incf pos))
          (when (< pos len)
            (if (= (aref line pos) ?:)
                ;; Trailing param (rest of line)
                (progn
                  (push (substring line (1+ pos)) params)
                  (setq pos len))
              ;; Regular param
              (let ((end (or (cl-position ?\s line :start pos) len)))
                (push (substring line pos end) params)
                (setq pos end)))))

        (clatter-message-create
         :tags tags
         :prefix prefix
         :command command
         :params (nreverse params)))
    (error
     (message "[clatter] Parse error: %s on line: %s" (error-message-string err) line)
     (clatter-message-create
      :command "ERROR"
      :params (list (format "Parse error: %s" (error-message-string err)))
      :prefix "clatter-internal"))))

;; --- IRCv3 Tag Parsing ---

(defun clatter-parse-tags (tags-string)
  "Parse IRCv3 TAGS-STRING into an alist of (key . value) pairs.
Tags format: key1=value1;key2=value2;key3"
  (when tags-string
    (mapcar (lambda (tag)
              (let ((eq-pos (cl-position ?= tag)))
                (if eq-pos
                    (cons (substring tag 0 eq-pos)
                          (substring tag (1+ eq-pos)))
                  (cons tag nil))))
            (split-string tags-string ";"))))

(defun clatter-get-parsed-tag (tags-alist key)
  "Get the value of KEY from TAGS-ALIST"
  (cdr (assoc key tags-alist #'string=)))

(defun clatter-get-tag (tags-string key)
  "Get the value of KEY from TAGS-STRING."
  (clatter-get-parsed-tag (clatter-parse-tags tags-string) key))

(defun clatter-get-parsed-server-time (tags-alist)
  "Extract server-time from IRCv3 TAGS-ALIST.
Returns an Emacs time value or nil."
  (let ((time-str (clatter-get-parsed-tag tags-alist "time")))
    (when time-str
      (clatter-parse-iso8601 time-str))))

(defun clatter-get-server-time (tags-string)
  "Extract server-time from IRCv3 TAGS-STRING.
Returns an Emacs time value or nil."
  (clatter-get-parsed-server-time (clatter-parse-tags tags-string)))

(defun clatter-parse-iso8601 (time-string)
  "Parse ISO8601 TIME-STRING to Emacs time value.
Returns nil on failure."
  (condition-case nil
      (encode-time (iso8601-parse time-string))
    (error nil)))

;; --- IRC Line Formatting ---

(defun clatter-format-line (command &rest params)
  "Format an IRC COMMAND with PARAMS into a protocol line.
The last param is treated as trailing if it contains spaces."
  (let ((parts (list command)))
    (when params
      (let ((last (car (last params)))
            (rest (butlast params)))
        (dolist (p rest)
          (push p parts))
        ;; Trailing param with : if needed
        (if (or (string-empty-p last)
                (string-match-p " " last)
                (and (> (length last) 0) (= (aref last 0) ?:)))
            (push (concat ":" last) parts)
          (push last parts))))
    (string-join (nreverse parts) " ")))

;; --- Common IRC Command Formatters ---

(defun clatter-irc-nick (nick)
  "Format NICK command."
  (clatter-format-line "NICK" nick))

(defun clatter-irc-user (username realname)
  "Format USER command for USERNAME and REALNAME."
  (clatter-format-line "USER" username "0" "*" realname))

(defun clatter-irc-pass (password)
  "Format PASS command with PASSWORD."
  (clatter-format-line "PASS" password))

(defun clatter-irc-join (channel &optional key)
  "Format JOIN command for CHANNEL with optional KEY."
  (if key
      (clatter-format-line "JOIN" channel key)
    (clatter-format-line "JOIN" channel)))

(defun clatter-irc-part (channel &optional message)
  "Format PART command for CHANNEL with optional MESSAGE."
  (if message
      (clatter-format-line "PART" channel message)
    (clatter-format-line "PART" channel)))

(defun clatter-irc-privmsg (target text)
  "Format PRIVMSG to TARGET with TEXT."
  (clatter-format-line "PRIVMSG" target text))

(defun clatter-irc-notice (target text)
  "Format NOTICE to TARGET with TEXT."
  (clatter-format-line "NOTICE" target text))

(defun clatter-irc-quit (&optional message)
  "Format QUIT command with optional MESSAGE."
  (if message
      (clatter-format-line "QUIT" message)
    (clatter-format-line "QUIT")))

(defun clatter-irc-pong (server)
  "Format PONG reply to SERVER."
  (clatter-format-line "PONG" server))

(defun clatter-irc-ping (server)
  "Format PING to SERVER."
  (clatter-format-line "PING" server))

(defun clatter-irc-cap (subcommand &rest args)
  "Format CAP SUBCOMMAND with ARGS."
  (apply #'clatter-format-line "CAP" subcommand args))

(defun clatter-irc-whois (nick)
  "Format WHOIS query for NICK."
  (clatter-format-line "WHOIS" nick))

(defun clatter-irc-topic (channel &optional new-topic)
  "Format TOPIC command for CHANNEL with optional NEW-TOPIC."
  (if new-topic
      (clatter-format-line "TOPIC" channel new-topic)
    (clatter-format-line "TOPIC" channel)))

(defun clatter-irc-kick (channel nick &optional reason)
  "Format KICK for NICK from CHANNEL with optional REASON."
  (if reason
      (clatter-format-line "KICK" channel nick reason)
    (clatter-format-line "KICK" channel nick)))

(defun clatter-irc-mode (target &optional mode &rest args)
  "Format MODE for TARGET with optional MODE and ARGS."
  (if mode
      (apply #'clatter-format-line "MODE" target mode args)
    (clatter-format-line "MODE" target)))

(defun clatter-irc-away (&optional message)
  "Format AWAY command.  If MESSAGE is nil, clears away."
  (if (and message (> (length message) 0))
      (clatter-format-line "AWAY" message)
    (clatter-format-line "AWAY")))

(defun clatter-irc-invite (nick channel)
  "Format INVITE for NICK to CHANNEL."
  (clatter-format-line "INVITE" nick channel))

(defun clatter-irc-names (channel)
  "Format NAMES request for CHANNEL."
  (clatter-format-line "NAMES" channel))

(defun clatter-irc-monitor-add (nicks)
  "Format MONITOR + for NICKS (string or list)."
  (let ((nick-str (if (listp nicks)
                      (string-join nicks ",")
                    nicks)))
    (format "MONITOR + %s" nick-str)))

(defun clatter-irc-monitor-remove (nicks)
  "Format MONITOR - for NICKS."
  (let ((nick-str (if (listp nicks)
                      (string-join nicks ",")
                    nicks)))
    (format "MONITOR - %s" nick-str)))

(defun clatter-irc-monitor-clear ()
  "Format MONITOR C (clear list)."
  "MONITOR C")

(defun clatter-irc-monitor-list ()
  "Format MONITOR L (request list)."
  "MONITOR L")

(defun clatter-irc-monitor-status ()
  "Format MONITOR S (request status)."
  "MONITOR S")

;; --- CTCP ---

(defun clatter-irc-ctcp-reply (target command &optional text)
  "Format a CTCP reply (via NOTICE) to TARGET for COMMAND with optional TEXT."
  (let ((ctcp-text (if text
                       (format "\C-a%s %s\C-a" command text)
                     (format "\C-a%s\C-a" command))))
    (clatter-irc-notice target ctcp-text)))

(defun clatter-irc-ctcp-request (target command &optional text)
  "Format a CTCP request (via PRIVMSG) to TARGET for COMMAND with optional TEXT."
  (let ((ctcp-text (if text
                       (format "\C-a%s %s\C-a" command text)
                     (format "\C-a%s\C-a" command))))
    (clatter-irc-privmsg target ctcp-text)))

;; --- TAGMSG / Typing ---

(defun clatter-irc-tagmsg (target &rest tags)
  "Format a TAGMSG to TARGET with TAGS (plist of name/value pairs)."
  (let ((tag-parts nil))
    (cl-loop for (name value) on tags by #'cddr
             do (push (if value
                         (format "%s=%s" name value)
                       name)
                      tag-parts))
    (format "@%s TAGMSG %s"
            (string-join (nreverse tag-parts) ";")
            target)))

(defun clatter-irc-typing (target state)
  "Format typing indicator to TARGET.
STATE should be \"active\", \"paused\", or \"done\"."
  (clatter-irc-tagmsg target "+typing" state))

;; --- Message Length / Splitting ---

(defun clatter-message-overhead (command target)
  "Calculate overhead bytes for COMMAND to TARGET.
Format: :nick!user@host COMMAND target :text"
  (+ 1 30 1 10 1 63 1 (length command) 1 (length target) 2))

(defun clatter-max-message-length (command target)
  "Calculate maximum safe message length for COMMAND to TARGET."
  (- clatter-max-irc-line-length (clatter-message-overhead command target)))

(defun clatter-split-long-message (target text &optional command)
  "Split TEXT into multiple messages for TARGET if needed.
COMMAND defaults to \"PRIVMSG\".  Returns a list of strings."
  (let* ((cmd (or command "PRIVMSG"))
         (max-len (clatter-max-message-length cmd target))
         (len (length text)))
    (if (<= len max-len)
        (list text)
      (let ((parts nil)
            (start 0))
        (while (< start len)
          (let* ((end (min (+ start max-len) len))
                 (chunk (substring text start end)))
            ;; Try to break on space
            (when (and (< end len)
                       (/= (aref text end) ?\s))
              (let ((space-pos (cl-position ?\s chunk :from-end t)))
                (when (and space-pos (> space-pos 0))
                  (setq end (+ start space-pos)
                        chunk (substring text start end)))))
            (push chunk parts)
            (setq start (if (and (< end len)
                                 (= (aref text end) ?\s))
                            (1+ end)
                          end))))
        (nreverse parts)))))

(provide 'clatter-protocol)

;;; clatter-protocol.el ends here
