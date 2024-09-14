;; -*- lexical-binding: t -*-
(setup backup
  (:option backup-directory-alist `(("." . ,(expand-file-name "backups" no-littering-var-directory)))))

(setup editing
  (:option kill-whole-line t
	       indent-tabs-mode nil
	       tab-width 4
	       use-short-answers t
           truncate-lines t))

(setup autorevert
  (:option auto-revert-avoid-polling t
	       auto-revert-interval 1
	       auto-revert-check-vc-info t)
  (global-auto-revert-mode))

(setup savehist
  (savehist-mode))

(setup number
  (line-number-mode)
  (column-number-mode))
(provide 'init-editing)
