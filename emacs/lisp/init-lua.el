;; -*- lexical-binding: t -*-
(setup (:straight lua-mode)
  (:option lua-indent-level 4
           lua-indent-nested-block-content-align nil
           lua-indent-close-paren-align nil)
  (defun lua-at-most-one-indent (old-function &rest arguments)
    (let ((old-res (apply old-function arguments)))
      (if (> old-res lua-indent-level) lua-indent-level old-res)))
  (:advice lua-calculate-indentation-block-modifier :around #'lua-at-most-one-indent))
(provide 'init-lua)
