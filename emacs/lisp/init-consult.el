;; -*- lexical-binding: t -*-
(setup (:straight consult)
  (:require consult)
  (:option register-preview-delay 0.5
	       register-preview-function #'consult-register-format
	       xref-show-xrefs-function #'consult-xref
	       xref-show-definitions-function #'consult-xref)
  (:global "C-c h" consult-history
	       "C-c m" consult-mode-command
	       "C-c k" consult-kmacro
	       "C-x M-:" consult-complex-command
	       [remap switch-to-buffer] consult-buffer
	       [remap switch-to-buffer-other-window] consult-buffer-other-window
	       [remap switch-to-buffer-other-frame] consult-buffer-other-frame
	       [remap bookmark-jump] consult-bookmark
	       [remap project-switch-to-buffer] consult-project-buffer
	       "M-#" consult-register-load
	       "M-'" consult-register-store
	       "C-M-#" consult-register
	       "M-y" consult-yank-pop
	       "<help> a" consult-apropos)
  (:with-map goto-map
    (:bind "e" 'consult-compile-error
	       "f" 'consult-flymake
	       "g" 'consult-goto-line
	       "M-g" 'consult-goto-line
	       "o" 'consult-outline
	       "m" 'consult-mark
	       "k" 'consult-global-mark
	       "i" 'consult-imenu
	       "I" 'consult-imenu-multi))
  (:with-map search-map
    (:bind "d" 'consult-fd
	       "D" 'consult-locate
	       "g" 'consult-grep
	       "G" 'consult-git-grep
	       "r" 'consult-ripgrep
	       "l" 'consult-line
	       "L" 'consult-line-multi
	       "m" 'consult-multi-occsur
	       "k" 'consult-keep-lines
	       "u" 'consult-focus-lines
	       "e" 'consult-isearch-history))
  (:with-mode isearch-mode
    (:bind "M-e" consult-isearch-history
	       "M-s e" consult-isearch-history
	       "M-s l" consult-line
	       "M-s L" consult-line-multi))
  (:with-map minibuffer-local-map
    (:bind "M-s" consult-history
	       "M-r" consult-history))
  (:hooks completion-list-mode-hook consult-preview-at-point-mode)
  (consult-customize
   consult-theme :preview-key '(:debounce 1.0 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (defun consult-ripgrep-current-directory ()
    (interactive)
    (consult-ripgrep default-directory))
  (:with-map search-map
    (:bind "R" consult-ripgrep-current-directory)))
(provide 'init-consult)
