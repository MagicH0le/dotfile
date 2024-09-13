;; -*- lexical-binding: t -*-
(setup eshell
  (:require em-smart)
  (:option eshell-scroll-to-bottom-on-input 'all
           eshell-error-if-no-glob t
           eshell-hist-ignoredups t
           eshell-save-history-on-exit t
           eshell-prefer-lisp-functions nil
           eshell-destroy-buffer-when-process-dies t
           eshell-where-to-jump 'begin
           eshell-review-quick-commands nil
           eshell-smart-space-goes-to-end t)
  (:global "C-c !" eshell
           "C-!" eshell-here)
  (defun eshell-here ()
    "Open up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*")))))
(provide 'init-eshell)
