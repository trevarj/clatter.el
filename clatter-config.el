;;; clatter-config.el --- Configuration for clatter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; User-facing configuration variables and auth-source integration
;; for clatter.el IRC client.

;;; Code:

(require 'auth-source)

;; --- Version ---

(defconst clatter-version
  (or (package-get-version) "devel")
  "clatter.el version.")

;; --- Customization ---

(defgroup clatter nil
  "An IRCv3-compliant IRC client for Emacs."
  :group 'communication
  :prefix "clatter-")

(defcustom clatter-networks nil
  "List of IRC networks to connect to.
Each entry is a list of (NAME . PLIST) where PLIST contains:
  :server    - Server hostname (required)
  :port      - Server port (default 6697)
  :tls       - Use TLS (default t)
  :nick      - Nickname (required)
  :username  - Username (default: nick)
  :realname  - Real name (default: nick)
  :sasl      - SASL type: nil, \\='plain, or \\='external
  :client-cert - Path to client certificate for SASL EXTERNAL
  :autojoin  - List of channels to join on connect
  :password  - Server password (or use auth-source)
  :bouncer   - This connection is to a bouncer which manages upstream
               NickServ identity and nick reclaim
  :proxy     - SOCKS5 proxy plist (:type socks5 :host H :port P
               [:user U] [:pass P]); see `clatter-proxy'
  :tor       - When non-nil, shorthand for Tor's local SOCKS5
               proxy (127.0.0.1:9050)

Example:
  \\='((\"libera\"
     :server \"irc.libera.chat\"
     :port 6697
     :tls t
     :nick \"yournick\"
     :sasl plain
     :autojoin (\"#emacs\" \"#commonlisp\")))"
  :type '(repeat (cons string plist))
  :group 'clatter)

(defcustom clatter-default-nick (user-login-name)
  "Default nickname if not specified per-network."
  :type 'string
  :group 'clatter)

(defcustom clatter-default-realname "clatter.el user"
  "Default real name if not specified per-network."
  :type 'string
  :group 'clatter)

(defcustom clatter-default-port 6697
  "Default server port."
  :type 'integer
  :group 'clatter)

(defcustom clatter-default-tls t
  "Use TLS by default."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-proxy nil
  "Default SOCKS5 proxy for networks without a per-network `:proxy'.
A plist of the form (:type socks5 :host HOST :port PORT [:user U] [:pass P]),
or nil to connect directly.  A network's own `:proxy' (or `:tor t') takes
precedence.  When a proxy is configured the connection is fail-closed: if the
handshake fails clatter will not fall back to a direct connection.  The target
hostname is always resolved by the proxy (remote DNS), so .onion works."
  :type '(choice (const :tag "No proxy" nil) (plist :tag "Proxy plist"))
  :group 'clatter)

(defcustom clatter-tls-method 'builtin
  "How to establish TLS connections.
`builtin' uses Emacs's internal GnuTLS via `gnutls-negotiate'.  This is
simple but, because Emacs is single-threaded, a write to a silently
half-open socket can block the entire Emacs event loop for minutes.

`external' runs an external TLS client subprocess (see
`clatter-tls-external-command') and speaks plaintext IRC to it over a
pipe.  TLS then lives in a separate OS process, so a dead network socket
blocks that subprocess rather than Emacs."
  :type '(choice (const :tag "Built-in GnuTLS" builtin)
                 (const :tag "External subprocess" external))
  :group 'clatter)

(defcustom clatter-tls-external-command "gnutls-cli"
  "External TLS client used when `clatter-tls-method' is `external'.
Recognized values are \"gnutls-cli\" and \"openssl\" (which uses
`openssl s_client').  \"gnutls-cli\" is recommended: it streams
line-oriented data promptly and verifies certificates strictly.  Any
other string is treated as an `openssl'-style program name.

The chosen client is wrapped with `stdbuf' when available so its output
is line-buffered and incoming IRC lines are not delayed by pipe
buffering."
  :type '(choice (const "gnutls-cli") (const "openssl") string)
  :group 'clatter)

(defcustom clatter-timestamp-format "%H:%M"
  "Format string for message timestamps.
See `format-time-string' for format specifiers."
  :type 'string
  :group 'clatter)

(defcustom clatter-timestamp-only-if-changed nil
  "Display a message timestamp only when its formatted value changes.

When non-nil, consecutive messages in the same buffer whose timestamps
format to the same string share a single displayed timestamp.  The
comparison is local to each Clatter buffer."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-display-on-join t
  "Whether to display a channel buffer when you join it.

The buffer is always created so that activity tracking continues to work.
Set this to nil to keep autojoined channels from changing the window layout."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-display-on-welcome t
  "Whether to display the server buffer after receiving the welcome message.

The server buffer is always created.  Set this to nil to connect without
changing the window layout."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-receive-query-display 'bury
  "How to display a buffer for an incoming private message.

`bury' creates the query buffer without displaying it, so activity tracking
can notify you without disrupting the current window.  `buffer' uses
`display-buffer', and `pop' uses `pop-to-buffer'."
  :type '(choice (const :tag "Create without displaying" bury)
                 (const :tag "Display buffer" buffer)
                 (const :tag "Pop to buffer" pop))
  :group 'clatter)

(defcustom clatter-timestamp-side 'right
  "Side of the window where message timestamps are displayed.
The value `left' uses the left margin, `right' uses the right margin,
and nil disables margin timestamps."
  :type '(choice (const :tag "Left margin" left)
                 (const :tag "Right margin" right)
                 (const :tag "Disabled" nil))
  :group 'clatter)

(defcustom clatter-self-echo-mode 'server
  "How messages sent by this client are displayed.

The default `server' waits for the server's echo-message response when that
capability is available (and preserves the existing immediate fallback when
it is not).  `optimistic' inserts a tentative local message immediately, then
reconciles the matching server echo so that server time and msgid metadata are
retained without displaying a duplicate."
  :type '(choice (const :tag "Wait for server echo" server)
                 (const :tag "Show immediately" optimistic))
  :group 'clatter)

(defcustom clatter-self-echo-timeout 30
  "Seconds an optimistic self echo may wait for its server echo.

After this interval Clatter keeps the locally displayed message, but no
longer reconciles a matching incoming message.  This prevents delayed
playback from a later connection from being mistaken for an echo of an old
outgoing message."
  :type 'number
  :group 'clatter)

(defcustom clatter-fill-column 80
  "Column at which to wrap messages in channel buffers."
  :type 'integer
  :group 'clatter)

(defcustom clatter-nick-column-width 20
  "Width of the nick column for right-aligned nick display.
Nicks are right-aligned within this column.  Increase if you
have channels with very long nicknames."
  :type 'integer
  :group 'clatter)

(defcustom clatter-max-line-length 400
  "Maximum length of a single IRC message (excluding protocol overhead)."
  :type 'integer
  :group 'clatter)

(defcustom clatter-reconnect-max-delay 300
  "Maximum delay in seconds between reconnection attempts."
  :type 'integer
  :group 'clatter)

(defcustom clatter-reconnect-initial-delay 10
  "Initial delay in seconds before first reconnection attempt."
  :type 'integer
  :group 'clatter)

(defcustom clatter-ping-interval 30
  "Seconds between sending keepalive pings to the server.
Matches ERC default.  Lower values keep the connection alive through
NAT timeouts but produce more traffic."
  :type 'integer
  :group 'clatter)

(defcustom clatter-ping-timeout 120
  "Seconds with no data received before considering connection dead.
If no data (including PONG replies) has been received from the server
for this many seconds, the connection is killed and a reconnect is
scheduled.  Must be greater than `clatter-ping-interval'.
Matches ERC default."
  :type 'integer
  :group 'clatter)

(defcustom clatter-use-auth-source t
  "Use `auth-source' to look up passwords.
Passwords are looked up by :host (server) and :user (nick)."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-auto-identify t
  "Whether to automatically identify with NickServ after registration.
When non-nil, clatter preserves its historical behavior of sending
`IDENTIFY' with the configured server password after connecting without
SASL.  A network marked with `:bouncer t' always skips this step: its
password authenticates this client to the bouncer, not to NickServ."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-nick-reclaim-enabled t
  "Whether to periodically try to reclaim the configured nick.
When non-nil, if you connect and your desired nick is in use (so you
land on a fallback like \"nick_\"), clatter periodically sends NICK to
reclaim it.  Disable this if another long-lived client (such as a
bouncer) legitimately holds your nick, to avoid fighting over it."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-nick-reclaim-use-regain nil
  "Reclaim the nick as the identified account owner via NickServ REGAIN.
When non-nil and the connection is SASL-identified, automatic reclaim
attempts use \"REGAIN\" on NickServ rather than a bare NICK command.

This defaults to nil deliberately: REGAIN forcibly kills whatever session
holds the nick.  If a legitimate second client (a bouncer, or the same
account connected from another host) holds it, auto-REGAIN turns a
harmless fallback nick into a mutual kill loop, since both sides keep
regaining and killing each other.  With nil, automatic reclaim only
issues a passive NICK and waits, which never kills anyone; use the
explicit `/regain' command when you actually want to seize the nick.

When non-nil and not SASL-identified, a bare NICK is used as a fallback."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-nick-reclaim-initial-delay 8
  "Seconds to wait after connecting before the first nick reclaim attempt.
Gives services time to fully associate your SASL login with your account
and to settle any ENFORCE hold on the nick, so the first REGAIN succeeds
cleanly instead of racing the enforcement timer."
  :type 'integer
  :group 'clatter)

(defcustom clatter-regain-kill-backoff 120
  "Minimum reconnect delay in seconds after a services nick-regain kill.
When the server kills the connection with a \"regained by services\"
message, an immediate reconnect tends to trigger another collision and
another kill.  After such a kill, clatter waits at least this many
seconds before reconnecting to break the ping-pong loop."
  :type 'integer
  :group 'clatter)

(defcustom clatter-quit-on-exit t
  "Send a QUIT to all connected networks when Emacs exits.
This prevents leaving orphaned ghost sessions on the server that hold
your nick until the server times them out."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-quit-message "clatter.el"
  "Default QUIT message sent when disconnecting or exiting Emacs."
  :type 'string
  :group 'clatter)

(defcustom clatter-log-raw-protocol nil
  "Log raw IRC protocol lines to *clatter-debug* buffer."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-flyspell-enable t
  "Enable flyspell in the clatter input area.
Uses `flyspell-mode' which is built into Emacs."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-paste-flood-threshold 3
  "Number of lines above which a paste triggers a flood warning.
If input contains more than this many lines, the user is prompted
before sending.  Set to nil to disable the warning."
  :type '(choice integer (const nil))
  :group 'clatter)

(defcustom clatter-input-ring-size 1024
  "Size of the input history ring."
  :type 'integer
  :group 'clatter)

(defcustom clatter-message-order 'newest-first
  "Order in which messages appear in channel buffers.
`newest-first' places new messages directly below the input line
with older messages scrolling downward (default).
`oldest-first' places new messages at the bottom of the buffer,
like a traditional IRC client."
  :type '(choice (const :tag "Newest first (below input)" newest-first)
                 (const :tag "Oldest first (traditional)" oldest-first))
  :group 'clatter)

(defcustom clatter-move-to-prompt t
  "When non-nil, typing text anywhere in a clatter buffer jumps to the input.
Like ERC's `erc-move-to-prompt': if point is outside the input area when
you start typing a self-inserting character, point first moves to the
prompt so the character is entered there."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-prompt-format "%t> "
  "Format used for the input prompt in Clatter buffers.

The value may be a string or a function.  In a string, `%t' expands to
the buffer target, `%n' to the current nickname, `%N' to the network
name, and `%%' to a literal percent sign.  A function is called with
one argument, the Clatter buffer, and must return a string.

The default preserves Clatter's historical `target> ' prompt."
  :type '(choice (string :tag "Format string")
                 (function :tag "Function"))
  :group 'clatter)

(defcustom clatter-header-line-preset nil
  "Preset that moves channel context into the header line.
When nil, Clatter leaves the header line disabled and preserves the normal
mode-line.  `topic' shows the full topic in the header line and removes it
from the mode-line.  `context' shows the network/target, channel modes,
member count, and full topic in the header line, leaving only the current
nick in the mode-line.

Typing and activity indicators remain in the mode-line for every preset."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Topic" topic)
                 (const :tag "Full channel context" context))
  :group 'clatter)

(defcustom clatter-read-state-enabled t
  "Persist local last-read timestamps for Clatter buffers.
This is a local fallback for bouncers or servers that replay backlog
without IRCv3 read-marker support."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-read-state-file
  (locate-user-emacs-file "clatter/read-state.el")
  "File where Clatter persists local read state."
  :type 'file
  :group 'clatter)

(defcustom clatter-read-state-save-delay 2
  "Seconds to debounce writes to `clatter-read-state-file'."
  :type 'number
  :group 'clatter)

(defcustom clatter-buffer-max-lines 10000
  "Maximum number of lines to keep in a clatter buffer.
When exceeded, oldest messages are removed.
Set to nil to disable truncation."
  :type '(choice integer (const nil))
  :group 'clatter)

(defcustom clatter-suppress-messages '(muted)
  "Default message types hidden in new channel buffers.
Valid values: join, part, quit, nick, mode, away, kick, topic.
Example: \\='(join part quit away) to hide join/part/quit/away noise.

This list seeds each new buffer's `buffer-invisibility-spec'.
Suppression is non-destructive: matching messages are still
inserted but hidden, so they can be revealed again later.  Use
the per-buffer `/suppress' and `/unsuppress' commands to toggle
types at runtime without losing any history."
  :type '(repeat (choice (const :tag "JOIN" join)
                         (const :tag "PART" part)
                         (const :tag "QUIT" quit)
                         (const :tag "NICK" nick)
                         (const :tag "MODE" mode)
                         (const :tag "AWAY" away)
                         (const :tag "KICK" kick)
                         (const :tag "TOPIC" topic)
                         (const :tag "NOISE" noise)
                         (const :tag "MUTED" muted)))
  :group 'clatter)

(defcustom clatter-prefix-rank "~&@%+"
  "Default prefix ranking.  This may be overridden by the server."
  :type 'string
  :group 'clatter)

;; --- IRCv3 capabilities we want to negotiate ---

(defconst clatter-wanted-capabilities
  '("server-time"
    "multi-prefix"
    "away-notify"
    "account-notify"
    "extended-join"
    "chghost"
    "invite-notify"
    "setname"
    "account-tag"
    "message-tags"
    "batch"
    "labeled-response"
    "echo-message"
    "draft/chathistory"
    "chathistory"
    "cap-notify"
    "userhost-in-names"
    "typing"
    "draft/typing"
    "monitor"
    "draft/read-marker"
    "read-marker"
    "standard-replies"
    "draft/bot"
    "draft/channel-rename"
    "draft/reply"
    "+draft/reply"
    "draft/react"
    "+draft/react"
    "message-tags")
  "IRCv3 capabilities to request during CAP negotiation.")

;; --- Ignore list ---

(defcustom clatter-ignore-list nil
  "List of ignored nick patterns.
Each entry is a string or pair of (PATTERN . NETWORK).
Entries are matched case-insensitively.
Glob-style wildcards (* and ?) are supported.
Example: (\"spambot\" \"*!*@bad.host.example.com\")"
  :type '(repeat (choice (string :tag "Pattern Only")
                         (cons :tag "Pattern and Network"
                               (string :tag "Pattern")
                               (string :tag "Network"))))
  :group 'clatter)

(defun clatter-ignored-p (sender &optional network)
  "Return non-nil if SENDER should be ignored in NETWORK.
Matches against `clatter-ignore-list' case-insensitively.
If NETWORK is nil or not given, SENDER is ignored globally.
Supports glob wildcards (* and ?)."
  (and sender
       (progn
         (setq sender (downcase sender))
         (cl-some (lambda (elt)
                    (pcase elt
                      (`(,pat . ,in)
                       (and (string-match-p (wildcard-to-regexp (downcase pat)) sender)
                            network (string-equal network in) t))
                      (pat
                       (string-match-p (wildcard-to-regexp (downcase pat)) sender))))
                  clatter-ignore-list))))

;; --- IRC protocol constants ---

(defconst clatter-max-irc-line-length 510
  "Maximum IRC line length (512 minus CR LF).")

;; --- Matching functions ---

(defvar clatter-nick-syntax-table
  (let ((table (copy-syntax-table))
        (extra-nick-chars '(?\[ ?\] ?{ ?} ?| ?- ?\\ ?` ?^ ?_)))
    (dolist (char extra-nick-chars)
      (modify-syntax-entry char "w" table))
    table)
  "Syntax table for IRC nicknames.")

(defun clatter-nick-match-p (nick text)
  "Return t if NICK is in TEXT; nil otherwise."
  (with-syntax-table clatter-nick-syntax-table
    (string-match-p (rx bow (literal nick) eow) text)))

(defun clatter-substring-match-p (substring text)
  "Return t if SUBSTRING is in TEXT; nil otherwise."
  (string-match-p (regexp-quote substring) text))

(defcustom clatter-mention-p-function #'clatter-nick-match-p
  "The function used to check for mentions.
The function must accept two arguments: the needle (nick/substring)
and the haystack (text), and return non-nil if a match is found."
  :type '(choice (const :tag "Whole Word/Nick Match" clatter-nick-match-p)
                 (const :tag "Substring Match" clatter-substring-match-p)
                 (function :tag "Other Function"))
  :group 'clatter)

(defun clatter-mention-p (nick text)
  "Return t if NICK is in TEXT; nil otherwise.
Uses CLATTER-MENTION-P-FUNCTION."
  (funcall clatter-mention-p-function nick text))

;; --- Helper functions ---

(defun clatter-network-get (network key &optional default)
  "Get KEY from NETWORK config plist, or DEFAULT."
  (let ((config (cdr (assoc network clatter-networks #'equal))))
    (or (plist-get config key) default)))

(defun clatter-get-password (network &optional config)
  "Get the password for NETWORK.
Checks CONFIG or the network config first, then auth-source."
  (let* ((config (or config (cdr (assoc network clatter-networks #'equal))))
         (server (plist-get config :server))
         (port (let ((value (or (plist-get config :port)
                                clatter-default-port)))
                 (if (integerp value) (number-to-string value) value)))
         (nick (or (plist-get config :nick) clatter-default-nick))
         (explicit-pw (plist-get config :password)))
    (cond
     (explicit-pw explicit-pw)
     (clatter-use-auth-source
      (catch 'password
        (dolist (host (delete-dups (delq nil (list network server))))
          (dolist (port-value (delete-dups (delq nil (list port "irc"))))
            (let ((found (car (auth-source-search :host host
                                                  :port port-value
                                                  :user nick
                                                  :require '(:secret)
                                                  :max 1))))
              (when found
                (let ((secret (plist-get found :secret)))
                  (throw 'password
                         (if (functionp secret) (funcall secret) secret)))))))
        nil))
     (t nil))))

(defun clatter-proxy-config (config)
  "Return the resolved SOCKS5 proxy plist for network CONFIG, or nil for direct.
Precedence: per-network `:proxy', then `:tor' shorthand, then `clatter-proxy'."
  (or (plist-get config :proxy)
      (and (plist-get config :tor)
           '(:type socks5 :host "127.0.0.1" :port 9050))
      clatter-proxy))

(provide 'clatter-config)

;;; clatter-config.el ends here
