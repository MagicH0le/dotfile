;; -*- lexical-binding: t -*-
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'site-bootstrap)
(require 'site-setup)

(require 'init-evil)

(require 'init-performance)

(require 'init-interface)
(require 'init-editing)
(require 'init-buffer)
(require 'init-minibuffer)
(require 'init-time)
(require 'init-international)
(require 'init-shell)

(require 'init-programming)
(require 'init-git)
(require 'init-completion)
(require 'init-snippet)
(require 'init-lsp)
(require 'init-dap)

(require 'init-clang)
(require 'init-python)
(require 'init-web)

(require 'init-chat)

(require 'init-org)
(require 'init-mail)
(require 'init-gnupg)
