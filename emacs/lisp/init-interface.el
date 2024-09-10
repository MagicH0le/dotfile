;; -*- lexical-binding: t -*-
(setup (:straight dashboard)
  (:option inhibit-startup-screen t
	       initial-buffer-choice (lambda ()
				                   (get-buffer-create dashboard-buffer-name)))
  (dashboard-setup-startup-hook))

(setup (:straight helpful)
  (:bind "C-h f" helpful-callable
	     "C-h v" helpful-variable
	     "C-h k" helpful-key
	     "C-h x" helpful-command
	     "C-h F" helpful-function
	     "C-c C-d" helpful-at-point))

(setup (:straight which-key)
  (:hide-mode which-key-mode)
  (which-key-mode))
(provide 'init-interface)
