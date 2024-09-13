;; -*- lexical-binding: t -*-
(setup (:straight helpful)
  (:global "C-h f" helpful-callable
	       "C-h v" helpful-variable
	       "C-h k" helpful-key
	       "C-h x" helpful-command
	       "C-h F" helpful-function
	       "C-c C-d" helpful-at-point))
(provide 'init-helpful)
