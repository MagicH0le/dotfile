;; -*- lexical-binding: t -*-
;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Core package, configuration macro
(require 'site-bootstrap)
(require 'site-setup)

;; Performance
(require 'init-esup)
(require 'init-no-littering)

;; Vim keybinding
(require 'init-evil)
(require 'init-general)

;; Help buffer
(require 'init-helpful)

;; Keybinding hint
(require 'init-which-key)

;; Editing
(require 'init-editing)

;; Buffer
(require 'init-buffer)

;; Minibuffer
(require 'init-vertico)
(require 'init-orderless)
(require 'init-consult)
(require 'init-prescient)
(require 'init-marginalia)
(require 'init-embark)

;; Colorful parens
(require 'init-rainbow-delimiters)

;; TODO: Paren structure editing

;; File browser
(require 'init-dired)
(require 'init-dirvish)

;; Time
(require 'init-time)

;; Language environment
(require 'init-international)
(require 'init-cnfonts)

;; Shell
(require 'init-shell)

;; Fuzzy matching
(require 'init-flx)

;; Programming general
(require 'init-eldoc)

;; Version control
(require 'init-git)

;; Code completion
(require 'init-corfu)
(require 'init-cape)

;; Code snippet
(require 'init-tempel)

;; Language server
(require 'init-eglot)

;; Debug adapter
(require 'init-dape)

;; Programming languages
(require 'init-clang)
(require 'init-python)
(require 'init-javascript)
(require 'init-typescript)
(require 'init-html)
(require 'init-lua)

;; Chating
(require 'init-telega)

;; Org-mode
(require 'init-org)

;; Mail
(require 'init-mail)

;; GnuPG
(require 'init-gnupg)
(provide 'init)
