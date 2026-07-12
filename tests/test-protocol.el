;;; test-protocol.el --- Tests for clatter-protocol.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)

;; --- Message Parsing ---

(ert-deftest clatter-test-parse-simple-privmsg ()
  "Parse a basic PRIVMSG."
  (let ((msg (clatter-parse-line ":nick!user@host PRIVMSG #channel :hello world")))
    (should (equal (clatter-message-command msg) "PRIVMSG"))
    (should (equal (clatter-message-prefix msg) "nick!user@host"))
    (should (equal (clatter-message-params msg) '("#channel" "hello world")))
    (should-not (clatter-message-tags msg))))

(ert-deftest clatter-test-parse-with-tags ()
  "Parse a message with IRCv3 tags."
  (let ((msg (clatter-parse-line
              "@time=2026-01-01T00:00:00.000Z;msgid=abc123 :nick!user@host PRIVMSG #channel :hello")))
    (should (equal (clatter-message-command msg) "PRIVMSG"))
    (should (string-match-p "time=" (clatter-message-tags msg)))
    (should (string-match-p "msgid=abc123" (clatter-message-tags msg)))))

(ert-deftest clatter-test-parse-no-trailing ()
  "Parse a message without trailing parameter."
  (let ((msg (clatter-parse-line ":server 001 testnick")))
    (should (equal (clatter-message-command msg) "001"))
    (should (equal (clatter-message-params msg) '("testnick")))))

(ert-deftest clatter-test-parse-ping ()
  "Parse PING with no prefix."
  (let ((msg (clatter-parse-line "PING :irc.libera.chat")))
    (should (equal (clatter-message-command msg) "PING"))
    (should (equal (clatter-message-params msg) '("irc.libera.chat")))))

(ert-deftest clatter-test-parse-empty-trailing ()
  "Parse a message with empty trailing parameter."
  (let ((msg (clatter-parse-line ":nick!user@host PART #channel :")))
    (should (equal (clatter-message-command msg) "PART"))
    (should (equal (clatter-message-params msg) '("#channel" "")))))

;; --- Prefix Parsing ---

(ert-deftest clatter-test-parse-prefix-full ()
  "Parse nick!user@host prefix."
  (let ((parsed (clatter-parse-prefix "nick!user@host")))
    (should (equal parsed '("nick" "user" "host")))))

(ert-deftest clatter-test-parse-prefix-server ()
  "Parse server-only prefix."
  (let ((parsed (clatter-parse-prefix "irc.libera.chat")))
    (should (equal parsed '("irc.libera.chat" nil nil)))))

(ert-deftest clatter-test-parse-prefix-nil ()
  "Parse nil prefix."
  (should-not (clatter-parse-prefix nil)))

(ert-deftest clatter-test-prefix-nick ()
  "Extract nick from parsed prefix."
  (should (equal (clatter-prefix-nick '("nick" "user" "host")) "nick")))

;; --- IRC Case Mapping ---

(ert-deftest clatter-test-nick-equal-rfc1459 ()
  "RFC1459 CASEMAPPING treats []\\^ as equivalent to {}|~."
  (should (clatter-nick-equal-p "Nick[\\^" "nick{|~" "rfc1459")))

(ert-deftest clatter-test-nick-equal-strict-rfc1459 ()
  "Strict RFC1459 CASEMAPPING does not fold ^ to ~."
  (should (clatter-nick-equal-p "Nick[\\" "nick{|" "strict-rfc1459"))
  (should-not (clatter-nick-equal-p "Nick^" "nick~" "strict-rfc1459")))

(ert-deftest clatter-test-nick-equal-ascii ()
  "ASCII CASEMAPPING only downcases ASCII letters."
  (should (clatter-nick-equal-p "Nick" "nick" "ascii"))
  (should-not (clatter-nick-equal-p "Nick[" "nick{" "ascii")))

;; --- Tag Parsing ---

(ert-deftest clatter-test-parse-tags ()
  "Parse IRCv3 tag string."
  (let ((tags (clatter-parse-tags "time=2026-01-01T00:00:00Z;msgid=abc;bot")))
    (should (equal (cdr (assoc "time" tags)) "2026-01-01T00:00:00Z"))
    (should (equal (cdr (assoc "msgid" tags)) "abc"))
    (should (assoc "bot" tags))
    (should-not (cdr (assoc "bot" tags)))))

(ert-deftest clatter-test-parse-tags-nil ()
  "Parse nil tags string."
  (should-not (clatter-parse-tags nil)))

(ert-deftest clatter-test-get-tag ()
  "Get specific tag value."
  (should (equal (clatter-get-tag "time=2026-01-01T00:00:00Z;msgid=abc" "msgid")
                 "abc")))

;; --- Channel Name Validation ---

(ert-deftest clatter-test-channel-name-p ()
  "Identify channel names."
  (should (clatter-channel-name-p "#emacs"))
  (should (clatter-channel-name-p "&local"))
  (should (clatter-channel-name-p "+channel"))
  (should (clatter-channel-name-p "!ABCDE"))
  (should-not (clatter-channel-name-p "nick"))
  (should-not (clatter-channel-name-p "#"))
  (should-not (clatter-channel-name-p "")))

(ert-deftest clatter-test-valid-channel-name ()
  "Validate channel names per RFC 2812."
  (should (clatter-valid-channel-name-p "#emacs"))
  (should-not (clatter-valid-channel-name-p "#has space"))
  (should-not (clatter-valid-channel-name-p "#has,comma"))
  (should-not (clatter-valid-channel-name-p
               (concat "#" (make-string 60 ?a)))))

;; --- Input Sanitization ---

(ert-deftest clatter-test-sanitize-input ()
  "Remove CR, LF, NUL from input."
  (should (equal (clatter-sanitize-input "hello\r\nworld\0")
                 "helloworld"))
  (should (equal (clatter-sanitize-input "clean") "clean"))
  (should-not (clatter-sanitize-input nil)))

(ert-deftest clatter-test-validate-input ()
  "Validate and sanitize input for sending."
  (let ((result (clatter-validate-input "hello")))
    (should (equal (car result) "hello"))
    (should-not (cdr result)))
  (let ((result (clatter-validate-input "")))
    (should (equal (cdr result) "Empty message")))
  (let ((result (clatter-validate-input nil)))
    (should (equal (cdr result) "Empty message"))))

;; --- IRC Formatting Stripping ---

(ert-deftest clatter-test-strip-formatting ()
  "Strip mIRC formatting codes."
  (should (equal (clatter-strip-irc-formatting
                  (concat (string #x02) "bold" (string #x02)))
                 "bold"))
  (should (equal (clatter-strip-irc-formatting
                  (concat (string #x03) "4,5colored" (string #x03)))
                 "colored"))
  (should (equal (clatter-strip-irc-formatting
                  (concat (string #x1D) "italic" (string #x1D)))
                 "italic"))
  (should (equal (clatter-strip-irc-formatting "plain text") "plain text"))
  (should-not (clatter-strip-irc-formatting nil)))

;; --- Line Formatting ---

(ert-deftest clatter-test-format-line ()
  "Format IRC protocol lines."
  (should (equal (clatter-format-line "PRIVMSG" "#channel" "hello world")
                 "PRIVMSG #channel :hello world"))
  (should (equal (clatter-format-line "JOIN" "#channel")
                 "JOIN #channel"))
  (should (equal (clatter-format-line "NICK" "newnick")
                 "NICK newnick")))

(provide 'test-protocol)

;;; test-protocol.el ends here
