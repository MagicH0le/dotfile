;; -*- lexical-binding: t -*-
(setup (:straight cape)
  (defun set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
		        (list (cape-capf-properties
		               (cape-capf-case-fold
			            (cape-capf-buster
			             (cape-capf-super (if arg arg (car completion-at-point-functions))
					                      :with #'tempel-complete #'cape-dabbrev #'cape-file)))
		               :sort t
		               :exclusive 'no))))
  (:hooks prog-mode-hook set-super-capf)
  (:hooks eglot-managed-mode-hook set-super-capf))
(provide 'init-cape)
