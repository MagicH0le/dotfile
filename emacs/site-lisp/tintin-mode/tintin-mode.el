;; -*- lexical-binding: t -*-
(require 'tintin-commands)

(defvar tintin-mode-hook nil)
(defvar tintin-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent) ;placeholder
    map)
  "Keymap for tintin major mode")

(setq auto-mode-alist
      (append '(("\\.tt\\'" . tintin-mode)
                ("\\.tin\\'" . tintin-mode))
              auto-mode-alist))

(defcustom  tintin-verbatim-character "\\"
  "The symbol used to mark lines that are not parsed by TinTin++.")
(rx-define tintin-verbatim-character (eval tintin-verbatim-character))

(defcustom  tintin-repeating-character "!"
  "The symbol used to repeat previous commands.")
(rx-define tintin-repeating-character (eval tintin-repeating-character))


;; Handle pattern matchers, formatters, regular expressions
(rx-define tintin-capture (pattern) (: "%" (? (any "%\\")) pattern))
(rx-define number-or-variable (group (or (+ digit) tintin-variable)))
(rx-define regexp-classes
  (: (? "+" number-or-variable (? (: ".." (* number-or-variable)))) (any "aAdDpPsSuUwW")))
(rx-define format-basic (group (any "acdfghlmnprstuwxACDHLMSTUX")))
(rx-define format-strftime (group (any "abdjmpuwyzABCHIMPSUWYZ")))
(rx-define format-numeric (group (: (any "-+.") (+ number-or-variable) "s")))
(rx-define regexp-ops (or regexp-classes (any "+?.*") (any "iI")))
(rx-define regexp-ops-wrapped (: (? "!") (group (optionally-braced group regexp-ops))))
(rx-define numeric-capture (: (? (any "1-9")) digit))
(defvar tintin-regexp-matches (rx "&" numeric-capture))
(defvar tintin-double-percent (rx "%%"))
(defvar tintin-capture-matcher
  (rx (tintin-capture
       (or format-basic format-strftime format-numeric
           regexp-ops-wrapped numeric-capture "*"))))

;; Handle various simple highlighted faces
(defvar default-chars-matcher (rx (: (any "{};"))))
(defvar ansi-color-code (rx "<" (? (any "FB")) (= 3 hex) ">"))
(defvar ansi-gray-code (rx "<" (any "gG") (= 2 digit) ">"))
(defvar tintin-function (rx (group "@" var-chars ) "{"))
(defvar tintin-special-symbols
  (rx (group (or (: bol (or tintin-repeating-character tintin-verbatim-character)) "~"))
      (* nonl)))
(defvar double-semicolon-warning (rx ";" (group ";")))
(defvar tintin-repeat-cmd
  (rx (group tintin-command-character (+ digit)) (or (any "\s\t;") eol)))

;; Deal with comments, which are bonkers in TinTin++
(rx-define comment-start-regexp
  (: tintin-command-character (any "nN") (any "oO") (? (any "pP")) (+ (any " \t\n"))))
(defvar comment-start-regexp (rx comment-start-regexp))
(defvar comment-regexp (rx comment-start-regexp (? (* (not (any ";")))) ";"))
(defvar insert-comment-str (rx tintin-command-character "nop "))

(defun tintin-comment-extend-region ()
  "Mark multiline constructs for highlighting #nop? comments"
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found-point (re-search-backward comment-start-regexp nil t)))
      (if found-point
          (progn
            (goto-char font-lock-end)
            (if (re-search-forward ";" nil "wow")
                (setq font-lock-end (point))
              (setq font-lock-end (point-max)))
            (setq font-lock-beg found-point))))))

;;
;; There are a number of different escape codes, all beginning with a `\`
(rx-define basic-escape (any "aefnrtv"))
(rx-define control-char (: "c" (any alphanumeric)))
(rx-define brace-hex-escape (: "x" (or (: "7" (any "BD") ) (= 2 hex))))
(rx-define no-line-feed (: line-end))
(rx-define unicode-16-bit (: "u" (= 4 hex)))
(rx-define unicode-21-bit (: "U" (= 6 hex)))
(defvar tintin-escape-codes (rx (group (: (syntax escape) (or
    no-line-feed basic-escape control-char brace-hex-escape unicode-16-bit unicode-21-bit)))))

(defun escape-code-matcher-func (limit)
  (let ((case-fold-search nil))
    (re-search-forward tintin-escape-codes limit t)))

;; Regular expressions for speedwalks and dice rolls, which are syntactically similar
;; and collide often, so need to be handled together
(rx-define start-marker (or (any "{\s\t") line-start))
(rx-define end-marker (or (any "}\s\t;") line-end))
(rx-define move-direction (or (any "nsewud")))
(rx-define no-pad-int (or "0" (: (any "1-9") (* (any "0-9")))))
(defvar dice-roll
  (rx (: start-marker
         (group (or (+ no-pad-int) tintin-variable)
                "d"
                (or (+ no-pad-int) tintin-variable))
         (not move-direction)
         end-marker)))
(defvar speedwalk
  (rx (: start-marker
         (group (+ (: (+ (any "0-9")) move-direction)))
         end-marker)))

;; Command lists for different classes of TinTin++ commands
(defvar variable-commands-list
  '( "variable" 3   "local" 3      "cat" 0
     "format" 4     "math" 0       "replace" 3))
(defvar unvariable-commands-list '( "unvariable" 5 "unlocal" 5))
(defvar function-command-list '("function" 3))
(defvar unfunction-command-list '("unfunction" 5))
(defvar loop-command-list '("loop" 0))
(defvar parse-foreach-command-list '("parse" 0 "foreach" 0))
(defvar flow-control-command-list
  '( "if" 0         "else" 0       "elseif" 0     "return" 3
     "while" 0      "break" 0      "continue" 4
     "switch" 0     "case" 0       "default" 3))
(defvar mud-command-list
  '(  "action" 3     "alias" 0      "echo" 0       "showme" 4
      "highlight" 2  "substitute" 3 "ticker" 4
      "delay" 3      "cr" 0         "gag" 0
      "tab" 0        "event" 0      "send" 0))
(defvar unmud-command-list
  '( "unaction" 5   "unalias" 0    "unticker" 6
     "ungag" 0      "untab" 0      "unevent" 0))
(defvar script-command-list '("script" 3))
(defvar builtin-command-list
  '( "all" 0        "commands" 2   "detatch" 0    "draw" 0
     "edit" 0       "end" 0        "grep" 0       "gts" 0
     "help" 1       "ignore" 3     "log" 0
     "macro" 3      "map" 0        "mesage" 4     "port" 0
     "path" 0       "pathdir" 5    "prompt" 4     "regexp" 3
     "read" 0       "run" 0        "scan" 1       "screen" 3
     "session" 3    "snoop" 0      "split" 3      "ssl" 0
     "textin" 4     "write" 0      "zap" 0        "ats" 0))


;; Several commands accept this list of commands as options
(defvar command-list-commands
  '("actions" 1 "aliases" 1 "buttons" 1 "classes" 1 "commands" 1 "configs" 1 "delays" 1
    "events" 1 "functions" 1 "gags" 1 "highlights" 1 "macros" 1 "pathdirs" 1 "prompts" 1
    "substitutes" 1 "tabs" 1 "tickers" 1 "variables" 1))
(defvar command-list-option (tintin-option :vals command-list-commands :final t))

;; Special handling for the #bell command and its subcommands
(defvar bell-command-list '("bell" 0))
(defvar bell-ring-option (tintin-option :vals '("ring" 1) :final t))
(defvar bell-volume-option (tintin-option :vals '("volume" 1)))
(defvar bell-toggle-option (tintin-option :vals '("flash" 2 "focus" 2 "margin" 1)))

;; Special handling for the #buffer command and its subcommands
(defvar buffer-command-list '("buffer" 2))
(defvar buffer-get-option (tintin-option :vals '("get" 1)))
(defvar buffer-toggle-option (tintin-option :vals '("lock" 1)))
(defvar buffer-info-option (tintin-option :vals '("info" 1)))
(defvar buffer-save-option (tintin-option :vals '("save" 1)))
(defvar buffer-find-option (tintin-option :vals '("find" 1)))
(defvar buffer-standard-option
  (tintin-option :vals '("home" 1 "end" 1 "up" 1 "down" 1 "clear" 1 "write" 1 "info" 1) :final t))

;; Special handling for the #chat command and its subcommands
(defvar chat-command-list '("chat" 2))
(defvar chat-all-constant (tintin-constant :vals '("all" 0)))
(defvar chat-send-option
  (tintin-option :vals '("message" 1 "emote" 1 "paste" 2 "private" 2 "public" 2 "send" 3)))
(defvar chat-standard-option
  (tintin-option :vals '("init" 2 "name" 1 "message" 1 "accept" 1 "call" 1 "cancel" 3
                         "color" 2 "decline" 1 "dnd" 2 "download" 2 "emote" 1 "forward" 1
                         "forwardall" 8 "filestat" 2 "group" 1 "ignore" 2 "info" 3 "ip" 2
                         "paste" 2 "peek" 2 "ping" 2 "private" 2 "public" 2 "reply" 3
                         "request" 3 "send" 3 "sendfile" 5 "serve" 3 "uninitialize" 1
                         "who" 1 "zap" 1)))

;; Special handling for the #line command and its subcommands
(defvar line-command-list '("line" 1))
(defvar line-gag-option (tintin-option :vals '("gag" 1) :final t))
(defvar line-capture-option (tintin-option :vals '("capture" 2)))
(defvar line-standard-option
  (tintin-option :vals '("strip" 2 "substitute" 2 "background" 1 "convert" 2 "debug" 1
                         "local" 3 "log" 3 "logmode" 4 "msdp" 2 "multishot" 2
                         "oneshot" 1 "quiet" 1 "verbatim" 5 "verbose" 5 "logverbatim" 4)))

;; Special handling for the #list command and its subcommands
(defvar list-command-list '("list" 3))
(defvar list-create-option (tintin-option :vals '("create" 2 "tokenize" 1)))
(defvar list-size-option (tintin-option :vals '("size" 2)))
(defvar list-retrieval-option (tintin-option :vals '("find" 1 "get" 1)))
(defvar list-argless-option
  (tintin-option :vals '("clear" 2 "collapse" 2 "explode" 1 "index" 2 "shuffle" 2) :final t))
(defvar list-standard-option
  (tintin-option :vals '("add" 1 "delete" 1 "insert" 2 "order" 1 "set" 2 "simplify" 2 "sort" 2)))

;; Special handling for the #class command and its subcommands
(defvar class-command-list '("class" 2))
(defvar class-create-option (tintin-option :vals '("read" 1)))
(defvar class-create-argless-option (tintin-option :vals '("load" 2 "open" 1) :final t))
(defvar class-size-option (tintin-option :vals '("size" 2)))
(defvar class-use-option (tintin-option :vals '("assign" 1 "write" 1)))
(defvar class-use-argless-option
  (tintin-option :vals '("list" 2 "save" 2 "clear" 3 "close" 3 "kill" 1) :final t))

;; Special handling for the #config command and its subcommands
(defvar config-command-list '("config" 3))

(defvar config-toggle-keywords
  '("child lock" 3 "color patch" 7 "command echo" 9 "convert meta" 4 "debug telnet" 1
    "inheritance" 1 "mccp" 1 "mouse tracking" 2 "repeat enter" 8 "screen reader" 1
    "scroll lock" 4 "speedwalk" 2 "telnet" 2 "verbatim" 1 "verbose" 5 "wordwrap" 1))
(defvar config-toggle-option (tintin-option :vals config-toggle-keywords))
(defvar config-toggle-option-final (tintin-option :vals config-toggle-keywords :final t))

(defvar config-standard-keywords
  '("auto tab" 1 "buffer size" 1 "charset" 1 "color mode" 2 "command color" 3
    "connect retry" 3 "history size" 1 "log mode" 1 "log level" 5 "packet patch" 1
    "random seed" 2 "tab width" 1))
(defvar config-standard-option (tintin-option :vals config-standard-keywords))
(defvar config-standard-option-final (tintin-option :vals config-standard-keywords :final t))

(defvar config-char-keywords '("repeat char" 2 "tintin char" 2 "verbatim char" 10))
(defvar config-char-option (tintin-option :vals config-char-keywords))
(defvar config-char-option-final (tintin-option :vals config-char-keywords :final t))

;; Special handling for the #cursor command and its subcommands
(defvar cursor-command-list '("cursor" 2))
(defvar cursor-option-keywords
  '("backspace" 1 "brace open" 2 "brace close" 7 "backward" 5 "clear" 1
    "clear left" 7 "clear line" 8 "clear right" 7 "convert meta" 2 "ctrl delete" 2
    "delete" 1 "delete word left" 8 "delete word right" 13 "echo" 1 "end" 2 "enter" 3
    "exit" 2 "forward" 1 "get" 1 "history next" 1 "history prev" 9 "history search" 9
    "home" 2 "info" 1 "insert" 3 "next word" 1 "paste buffer" 1 "prev word" 2
    "redraw input" 1 "screen focus in" 1 "screen focus out" 14 "set" 2 "suspend" 2
    "tab" 1 "tab l s backward" 5 "tab l s forward" 9))
(defvar cursor-option (tintin-option :vals cursor-option-keywords :final t))

;; Special handling for the #daemon command and its subcommands
(defvar daemon-command-list '("daemon" 1))
(defvar daemon-option
  (tintin-option :vals '("attach" 1 "detach" 1 "input" 1 "kill" 1 "list" 1) :final t))

;; Special handling for the #debug command
(defvar debug-command-list '("debug" 2))
(defvar debug-toggle-value (tintin-constant :vals '("off" 2 "on" 1 "log" 1)))

;; Special handling for the #history command and its subcommands
(defvar history-command-list '("history" 3))
(defvar history-arg-option (tintin-option :vals '("insert" 1 "read" 1 "write" 1)))
(defvar history-no-arg-option (tintin-option :vals '("delete" 1 "list" 1) :final t))

;; Special handling for the #ignore command
(defvar ignore-command-list '("ignore" 1))

;; Special handling for the #info command
(defvar info-command-list '("info" 2))
(defvar info-toggle-value (tintin-constant :vals '("off" 2 "on" 1 "list" 1 "save" 1)))
(defvar info-no-arg-option-args
  '("cpu" 2 "environ" 2 "input" 1 "mccp" 2 "memory" 2 "stack" 2
    "session" 2 "sessions" 0 "system" 2 "unicode" 1))
(defvar info-no-arg-options
  (tintin-option :final t :vals (append command-list-commands info-no-arg-option-args)))

;; Special handling for the #kill command
(defvar kill-command-list '("kill" 1))


(setq tintin-font-lock-keywords (append

  `(;; Highlight variables as they're used. This is done up top and we're explicit
    ;; about the default face of the initial symbol [&$*] and any braces so subsequent
    ;; elements in font-lock-keywords can use the `keep` override mode, filling in the
    ;; unhighighted adjacent characters as veriable definitions as necessary, for
    ;; example in this case:
    ;;
    ;;   #var var_name thing;
    ;;   #var {the_$var_name} data;
    ;;
    ;; a variable called `the_thing` is created with value `data` and we'd want
    ;; `the_` to be highlighted as font-lock-variable-name-face but then the
    ;; remaining portion to be highlighted as a variable usage.
    (,(rx simple-variable)
     (2 'tintin-variable-usage-face nil t) ;; match the unbraced form
     (3 'tintin-variable-usage-face nil t) ;; match the braced form
     (0 'default keep))
    (,(rx optionally-braced-tintin-variable)
     (2 'tintin-variable-usage-face keep t) ;; match the unbraced form
     (3 'tintin-variable-usage-face keep t) ;; match the braced form
     (0 'default keep))

    ;; Highlight captures in actions, aliases, etc.
    (,tintin-capture-matcher 0 'tintin-capture-face keep)
    (,tintin-double-percent 0 'tintin-capture-face keep)
    (,tintin-regexp-matches 0 'tintin-capture-face keep)

    ;; Handle functions as they're used
    (,tintin-function 1 'tintin-function-face)

    ;; Handle repeat command
    (,tintin-repeat-cmd 1 'tintin-command-face)

    ;; Handle colors.
    (,ansi-color-code . 'tintin-ansi-face)
    (,ansi-gray-code . 'tintin-ansi-face)

    ;; Handle special symbols, speedwalk, and dice rolls
    (,tintin-special-symbols 1 'font-lock-warning-face)
    (,'escape-code-matcher-func 1 'font-lock-warning-face keep)
    (,speedwalk 1 'font-lock-warning-face)
    (,dice-roll 1 'font-lock-warning-face keep)
    (,double-semicolon-warning 1 'font-lock-warning-face)
    (,default-chars-matcher 0 'default keep))

  ;; Highlight the #list command and its various modes
  (let ((list-command (tintin-command :cmds 'list-command-list)))
    (fontify-tintin-cmd list-command
                        '(var-assignment list-create-option)
                        '(var-usage list-argless-option)
                        '(var-usage list-standard-option)
                        '(var-usage list-size-option final-var-assignment)
                        '(var-usage list-retrieval-option arg final-var-assignment)))

  ;; Highlight variable defining and deleting commands like #var and #unvar
  (let ((variable-command (tintin-command :cmds 'variable-commands-list)))
    (fontify-tintin-cmd variable-command
                        '(var-assignment)))
  (let ((unvariable-command (tintin-command :cmds 'unvariable-commands-list)))
    (fontify-tintin-cmd unvariable-command
                        '(final-var-usage)))

  ;; Highlight the #class command and its various modes
  (let ((class-command (tintin-command :cmds 'class-command-list)))
    (fontify-tintin-cmd class-command
                        '(var-usage class-use-option)
                        '(var-usage class-use-argless-option)
                        '(var-assignment class-create-option)
                        '(var-assignment class-create-argless-option)
                        '(var-usage class-size-option final-var-assignment)))

  ;; Highlight the #function and #unfunction commands
  (let ((function-command (tintin-command :cmds 'function-command-list)))
    (fontify-tintin-cmd function-command
                        '(function-name)))
  (let ((unfunction-command (tintin-command :cmds 'unfunction-command-list)))
    (fontify-tintin-cmd unfunction-command
                        '(final-var-usage)))

  ;; Highlight the #loop command
  (let ((loop-command (tintin-command :cmds 'loop-command-list)))
    (fontify-tintin-cmd loop-command
                        '(arg arg var-assignment)))

  ;; Highlight the #parse and #foreach commands
  (let ((parse-foreach-command (tintin-command :cmds 'parse-foreach-command-list)))
    (fontify-tintin-cmd parse-foreach-command
                        '(arg var-assignment)))

  ;; Highlight flow control commands such as #if and #else
  (let ((flow-control-command (tintin-command :cmds 'flow-control-command-list)))
    (fontify-tintin-cmd flow-control-command))

  ;; Highlight the #line command
  (let ((line-command (tintin-command :cmds 'line-command-list :face 'tintin-command-face)))
    (fontify-tintin-cmd line-command
                        '(line-standard-option)
                        '(line-gag-option)
                        '(line-capture-option var-assignment)))

  ;; Highlight mud scripting commands
  (let ((mud-command (tintin-command :cmds 'mud-command-list :face 'tintin-command-face)))
    (fontify-tintin-cmd mud-command))
  (let ((unmud-command (tintin-command :cmds 'unmud-command-list :face 'tintin-command-face)))
    (fontify-tintin-cmd unmud-command))

  ;; Highlight tintin builtins for working with tintin or setting up sessions
  (let ((built-command (tintin-command :cmds 'builtin-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd built-command))
  (let ((script-command (tintin-command :cmds 'script-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd script-command
                        '(var-assignment final-arg)))

  ;; Highlight #bell command
  (let ((bell-command (tintin-command :cmds 'bell-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd bell-command
                        '(bell-ring-option)
                        '(bell-volume-option final-arg)
                        '(bell-toggle-option toggle-value)))

  ;; Highlight #buffer command
  (let ((buffer-command (tintin-command :cmds 'buffer-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd buffer-command
                        '(buffer-info-option buffer-save-option final-var-assignment)
                        '(buffer-standard-option)
                        '(buffer-get-option var-assignment)
                        '(buffer-toggle-option toggle-value)
                        '(buffer-find-option arg final-arg)
                        '(buffer-find-option final-arg)))

  ;; Highlight #chat command
  (let ((chat-command (tintin-command :cmds 'chat-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd chat-command
                        '(chat-send-option chat-all-constant)
                        '(chat-standard-option)))

  ;; Highlight #config command
  (let ((config-command (tintin-command :cmds 'config-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd config-command
                        '(config-toggle-option-final)
                        '(config-toggle-option toggle-value)
                        '(config-standard-option-final)
                        '(config-standard-option final-arg)
                        '(config-char-option-final)
                        '(config-char-option settable-character)))

  ;; Highlight #cursor command
  (let ((cursor-command (tintin-command :cmds 'cursor-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd cursor-command
                        '(cursor-option)))

  ;; Highlight #daemon command
  (let ((daemon-command (tintin-command :cmds 'daemon-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd daemon-command
                        '(daemon-option)))

  ;; Highlight #debug command
  (let ((debug-command (tintin-command :cmds 'debug-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd debug-command
                        '(command-list-option debug-toggle-value)
                        '(command-list-option)))

  ;; Highlight #history command
  (let ((history-command (tintin-command :cmds 'history-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd history-command
                        '(history-arg-option final-arg)
                        '(history-no-arg-option)))

  ;; Highlight #ignore command
  (let ((ignore-command (tintin-command :cmds 'ignore-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd ignore-command
                        '(command-list-option toggle-value)
                        '(command-list-option)))

  ;; Highight #info command
  (let ((info-command (tintin-command :cmds 'info-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd info-command
                        '(command-list-option info-toggle-value)
                        '(info-no-arg-options)))

  ;; Highlight #kill command
  (let ((kill-command (tintin-command :cmds 'kill-command-list :face 'font-lock-builtin-face)))
    (fontify-tintin-cmd kill-command
                        '(command-list-option)))

  ;; Finish with the comment face that overrides everything
  `((,comment-regexp 0 'font-lock-comment-face t))))

(defvar tintin-mode-syntax-table
  (let ((st (make-syntax-table)))

    (modify-syntax-entry ?_ "w" st)      ; sets underscore to be counted as word
    (modify-syntax-entry ?# "w" st)      ; sets hash to be counted as word
    (modify-syntax-entry ?\' "w" st)     ; quotes are super weird in TinTin++, and are
    (modify-syntax-entry ?\" "w" st)     ;   kind of just normal word characters

    (modify-syntax-entry ?\/ ". 14" st)  ; support for c-style multiline comments
    (modify-syntax-entry ?* ". 23" st)   ;   which are apparently acceptable in TinTin++!?

    st)
  "Syntax table for tintin-mode")

;;;###autoload
(defun tintin-mode ()
  "Major mode for editing TinTin++ code."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table tintin-mode-syntax-table)
  (use-local-map tintin-mode-map)

  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'indent-line-function) 'tintin-indent-line)

  (set (make-local-variable 'comment-start) insert-comment-str)
  (set (make-local-variable 'comment-start-skip) comment-start-regexp)
  (set (make-local-variable 'comment-end) ";")

  (set (make-local-variable 'font-lock-defaults) '(tintin-font-lock-keywords nil t))
  (make-local-variable 'font-lock-extend-region-functions)
  (add-hook 'font-lock-extend-region-functions 'tintin-comment-extend-region)

  (setq major-mode 'tintin-mode)
  (setq mode-name "TinTin++")
  (run-hooks 'tintin-mode-hook))

(defun tintin-indent-line ()
  "Indent current line as TinTin++ code."
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check for rule 1
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)

      (if (looking-at "^[ \t]*}[ ]*\\({.+}\\)*;?[ \t]*$"); Check for rule 2
          (progn
            (save-excursion
              (forward-line -1)
              (if (looking-at "^[ \t]*.*?{[ \t]*$")
                  (setq cur-indent (current-indentation))
                  (setq cur-indent (- (current-indentation) tab-width)))
              )
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}[ ]*\\({[0-9\.]+}\\)*;?[ \t]*$") ; Check for rule 3
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
                                        ; Check for rule 4
              (if (looking-at "^[ \t]*.*?{[ \t]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(provide 'tintin-mode)
