;;; test-handlers.el --- Tests for clatter-handlers.el dispatch -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-commands)

;; --- PRIVMSG dispatch ---

(ert-deftest clatter-test-dispatch-privmsg ()
  "PRIVMSG dispatches to clatter-privmsg-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG #emacs :hello everyone")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "hello everyone"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-time-tag ()
  "PRIVMSG with @time tag dispatches to clatter-privmsg-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let* ((time "2011-10-19T16:40:51.620Z")
               (time-tag (concat "@time=" time))
               (calls (clatter-test-capture-hook
                       clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              (concat time-tag " :alice!~a@host PRIVMSG #emacs :hello everyone"))))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "hello everyone"))
            (should (equal (nth 4 args) (clatter-parse-iso8601 time)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-ctcp-action ()
  "PRIVMSG CTCP ACTION dispatches to clatter-action-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG #emacs :ACTION greets everyone")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "greets everyone"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-ctcp-action-time-tag ()
  "PRIVMSG CTCP ACTION with @time tag dispatches to clatter-action-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let* ((time "2011-10-19T16:40:51.620Z")
               (time-tag (concat "@time=" time))
               (calls (clatter-test-capture-hook
                       clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              (concat time-tag " :alice!~a@host PRIVMSG #emacs :ACTION greets everyone"))))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "greets everyone"))
            (should (equal (nth 4 args) (clatter-parse-iso8601 time)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-dm ()
  "PRIVMSG to our nick dispatches with our nick as target."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":bob!~b@host PRIVMSG testnick :hey there")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("bob" "~b" "host")))
            (should (equal (nth 2 args) "testnick"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-dm-ctcp-action ()
  "PRIVMSG CTCP ACTION to our nick dispatches with our nick as target."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":bob!~b@host PRIVMSG testnick :ACTION waves")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("bob" "~b" "host")))
            (should (equal (nth 2 args) "testnick"))))
      (clatter-test-cleanup))))

;; --- NOTICE dispatch ---

(ert-deftest clatter-test-dispatch-notice ()
  "NOTICE dispatches to clatter-notice-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-notice-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":NickServ!srv@services NOTICE testnick :You are identified")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("NickServ" "srv" "services")))
            (should (equal (nth 3 args) "You are identified"))))
      (clatter-test-cleanup))))

;; --- JOIN dispatch ---

(ert-deftest clatter-test-dispatch-join ()
  "JOIN dispatches to clatter-join-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-join-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host JOIN #emacs alice :Alice A")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "alice"))))
      (clatter-test-cleanup))))

;; --- PART dispatch ---

(ert-deftest clatter-test-dispatch-part ()
  "PART dispatches to clatter-part-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-part-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PART #emacs :bye")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "bye"))))
      (clatter-test-cleanup))))

;; --- QUIT dispatch ---

(ert-deftest clatter-test-dispatch-quit ()
  "QUIT dispatches to clatter-quit-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-quit-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host QUIT :connection reset")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "connection reset"))))
      (clatter-test-cleanup))))

;; --- NICK dispatch ---

(ert-deftest clatter-test-dispatch-nick ()
  "NICK dispatches to clatter-nick-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-nick-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host NICK :alice_")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "alice_"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-nick-self ()
  "NICK for self updates connection nick."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (progn
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":testnick!~t@host NICK :newnick"))
          (should (equal (clatter-connection-nick conn) "newnick")))
      (clatter-test-cleanup))))

;; --- TOPIC dispatch ---

(ert-deftest clatter-test-dispatch-topic ()
  "TOPIC dispatches to clatter-topic-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-topic-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host TOPIC #emacs :new topic here")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) "#emacs"))
            (should (equal (nth 2 args) '("alice" "~a" "host")))
            (should (equal (nth 3 args) "new topic here"))))
      (clatter-test-cleanup))))

;; --- PING/PONG ---

(ert-deftest clatter-test-dispatch-ping ()
  "PING sends PONG reply."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse "PING :irc.libera.chat"))
          (should (clatter-test-sent-matching "PONG.*irc.libera.chat")))
      (clatter-test-cleanup))))

;; --- TAGMSG: Typing indicators ---

(ert-deftest clatter-test-dispatch-typing ()
  "TAGMSG with +typing dispatches to clatter-typing-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-typing-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+typing=active :alice!~a@host TAGMSG #emacs")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "active"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-typing-self-ignored ()
  "Typing from self is not dispatched."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-typing-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+typing=active :testnick!~t@host TAGMSG #emacs")))))
          (should (= (length calls) 0)))
      (clatter-test-cleanup))))

;; --- TAGMSG: Reactions ---

(ert-deftest clatter-test-dispatch-reaction ()
  "TAGMSG with draft/react dispatches to clatter-react-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-react-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              (concat "@+draft/react=%F0%9F%91%8D"
                                      ";+draft/reply=abc123"
                                      " :alice!~a@host TAGMSG #emacs"))))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 4 args) "abc123"))))
      (clatter-test-cleanup))))

;; --- BOT tag ---

(ert-deftest clatter-test-dispatch-bot-tag ()
  "PRIVMSG with bot tag marks sender."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@bot :botuser!~b@host PRIVMSG #emacs :automated msg")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (sender (clatter-prefix-nick (nth 1 args))))
            (should (get-text-property 0 'clatter-bot sender))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-bot-tag-ctcp-action ()
  "PRIVMSG CTCP ACTION with bot tag marks sender."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@bot :botuser!~b@host PRIVMSG #emacs :ACTION automated msg")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (sender (clatter-prefix-nick (nth 1 args))))
            (should (get-text-property 0 'clatter-bot sender))))
      (clatter-test-cleanup))))

;; --- Reply/Thread tags ---

(ert-deftest clatter-test-dispatch-reply-tag ()
  "PRIVMSG with draft/reply tag attaches reply-to property."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+draft/reply=msg999;msgid=msg1000 :alice!~a@host PRIVMSG #emacs :replying")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (text (nth 3 args)))
            (should (equal (get-text-property 0 'clatter-reply-to text) "msg999"))
            (should (equal (get-text-property 0 'clatter-msgid text) "msg1000"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-reply-tag-ctcp-action ()
  "PRIVMSG CTCP ACTION with draft/reply tag attaches reply-to property."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+draft/reply=msg999;msgid=msg1000 :alice!~a@host PRIVMSG #emacs :ACTION replying")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (text (nth 3 args)))
            (should (equal (get-text-property 0 'clatter-reply-to text) "msg999"))
            (should (equal (get-text-property 0 'clatter-msgid text) "msg1000"))))
      (clatter-test-cleanup))))

;; --- RENAME dispatch ---

(ert-deftest clatter-test-dispatch-rename ()
  "RENAME dispatches system message (no crash even without buffer)."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-system-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server RENAME #old #new :channel migrated")))))
          ;; No buffer for #old exists, so no system hook fires for rename
          ;; but the dispatch should not error
          (should t))
      (clatter-test-cleanup))))

;; --- MARKREAD dispatch ---

(ert-deftest clatter-test-dispatch-markread ()
  "MARKREAD is dispatched without error."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server MARKREAD #emacs timestamp=2026-01-01T00:00:00.000Z"))
          (should t))
      (clatter-test-cleanup))))

;; --- STATUSMSG ---

(ert-deftest clatter-test-dispatch-statusmsg ()
  "PRIVMSG to @#channel strips prefix and adds label."
  (let ((conn (clatter-test-make-connection)))
    ;; Set up ISUPPORT with STATUSMSG
    (let ((isup (make-hash-table :test 'equal)))
      (puthash "STATUSMSG" "@+" isup)
      (setf (clatter-connection-isupport conn) isup))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG @#emacs :ops only message")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (target (nth 2 args))
                 (text (nth 3 args)))
            ;; Target should be #emacs (prefix stripped)
            (should (equal target "#emacs"))
            ;; Text should contain [ops] prefix
            (should (string-match-p "\\[ops\\]" text))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-statusmsg-ctcp-action ()
  "PRIVMSG CTCP ACTION to @#channel strips prefix and adds label."
  (let ((conn (clatter-test-make-connection)))
    ;; Set up ISUPPORT with STATUSMSG
    (let ((isup (make-hash-table :test 'equal)))
      (puthash "STATUSMSG" "@+" isup)
      (setf (clatter-connection-isupport conn) isup))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG @#emacs :ACTION ops only action")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (target (nth 2 args))
                 (text (nth 3 args)))
            ;; Target should be #emacs (prefix stripped)
            (should (equal target "#emacs"))
            ;; Text should contain [ops] prefix
            (should (string-match-p "\\[ops\\]" text))))
      (clatter-test-cleanup))))

;; --- MONITOR numerics ---

(ert-deftest clatter-test-dispatch-monitor-online ()
  "730 RPL_MONONLINE dispatches system message."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-system-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server 730 testnick :alice!~a@host")))))
          (should (= (length calls) 1))
          (should (string-match-p "Online" (nth 1 (car calls)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-monitor-offline ()
  "731 RPL_MONOFFLINE dispatches system message."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-system-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server 731 testnick :alice")))))
          (should (= (length calls) 1))
          (should (string-match-p "Offline" (nth 1 (car calls)))))
      (clatter-test-cleanup))))

;; --- Nick in use (433) ---

;; --- Bouncer authentication and nick reclaim ---

(defmacro clatter-test--with-live-config (conn config &rest body)
  "Run BODY with CONN's process configured with CONFIG."
  (declare (indent 2))
  `(let ((proc (make-pipe-process :name "clatter-test-live-config" :buffer nil)))
     (unwind-protect
         (progn
           (setf (clatter-connection-process ,conn) proc)
           (process-put proc :clatter-config ,config)
           ;; The welcome path records watchdog diagnostics; tests do not
           ;; need or have permission to write the user state directory.
           (cl-letf (((symbol-function 'clatter--watchdog)
                      (lambda (&rest _))))
             ,@body))
       (when (process-live-p proc)
         (delete-process proc)))))

(ert-deftest clatter-test-welcome-bouncer-skips-nickserv-identify ()
  "A bouncer password is not sent to NickServ after 001."
  (let ((conn (clatter-test-make-connection "znc" "testnick")))
    (setf (clatter-connection-sasl-state conn) nil)
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "bouncer-secret" :bouncer t)
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should-not (clatter-test-sent-matching "NickServ"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-welcome-identifies-for-non-bouncer-live-config ()
  "Non-bouncer connections retain automatic IDENTIFY using live config."
  (let ((conn (clatter-test-make-connection "znc" "testnick")))
    (setf (clatter-connection-sasl-state conn) nil)
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "live-secret")
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should (clatter-test-sent-matching
                     "PRIVMSG NickServ :IDENTIFY live-secret"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-welcome-auto-identify-can-be-disabled ()
  "`clatter-auto-identify' suppresses IDENTIFY for direct connections."
  (let ((conn (clatter-test-make-connection "testnet" "testnick"))
        (clatter-auto-identify nil))
    (setf (clatter-connection-sasl-state conn) nil)
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "server-secret")
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should-not (clatter-test-sent-matching "NickServ"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-welcome-sasl-still-suppresses-identify ()
  "SASL success continues to suppress automatic NickServ IDENTIFY."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "server-secret")
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should-not (clatter-test-sent-matching "NickServ"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-bouncer-disables-automatic-nick-reclaim ()
  "Bouncer connections never start automatic NICK or REGAIN reclaim."
  (let ((conn (clatter-test-make-connection "znc" "fallback")))
    (setf (clatter-connection-desired-nick conn) "wanted"
          (clatter-connection-sasl-state conn) :done)
    (unwind-protect
        (clatter-test--with-live-config conn '(:bouncer t)
          (clatter-test-with-mock-send
            (let ((clatter-nick-reclaim-use-regain t))
              (clatter--maybe-start-nick-reclaim conn)
              (clatter--reclaim-nick conn "wanted"))
            (should-not (clatter-connection-nick-reclaim-timer conn))
            (should-not clatter-test--sent-lines)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-reconnect-preserves-bouncer-override ()
  "Reconnect retries with original overrides, including `:bouncer'."
  (let ((conn (clatter-test-make-connection "znc" "testnick"))
        scheduled reconnect-args)
    (setf (clatter-connection-state conn) :disconnected
          (clatter-connection-config conn)
          '(:server "znc.example" :nick "testnick" :bouncer t)
          (clatter-connection-connect-overrides conn) '(:bouncer t))
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args)
                     (setq scheduled (car (last args)))
                     'clatter-test-timer))
                  ((symbol-function 'clatter-connect)
                   (lambda (&rest args) (setq reconnect-args args))))
          (clatter--schedule-reconnect conn)
          (funcall scheduled)
          (should (equal reconnect-args '("znc" :bouncer t))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-nickserv-recovery-never-sends-network-password ()
  "Manual GHOST and REGAIN never append a server or bouncer password."
  (let ((conn (clatter-test-make-connection "znc" "testnick"))
        (clatter-networks '(("znc" :password "bouncer-secret"))))
    (unwind-protect
        (cl-letf (((symbol-function 'clatter-insert-system) (lambda (&rest _))))
          (clatter-test-with-mock-send
            (clatter--nickserv-recover conn "GHOST" "wanted")
            (clatter--nickserv-recover conn "REGAIN" "wanted")
            (should (equal (nreverse clatter-test--sent-lines)
                           '("PRIVMSG NickServ :GHOST wanted"
                             "PRIVMSG NickServ :REGAIN wanted")))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-nick-in-use ()
  "433 during registration appends underscore to nick and retries."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (setf (clatter-connection-state conn) :connecting)
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server 433 * testnick :Nickname is already in use"))
          (should (equal (clatter-connection-nick conn) "testnick_"))
          (should (clatter-test-sent-matching "NICK testnick_")))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-nick-in-use-while-connected ()
  "433 while connected (reclaim attempt) does not append underscore."
  (let ((conn (clatter-test-make-connection "testnet" "testnick_")))
    (setf (clatter-connection-desired-nick conn) "testnick")
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server 433 * testnick :Nickname is already in use"))
          (should (equal (clatter-connection-nick conn) "testnick_"))
          (should-not (clatter-test-sent-matching "NICK")))
      (clatter-test-cleanup))))

(provide 'test-handlers)

;;; test-handlers.el ends here
