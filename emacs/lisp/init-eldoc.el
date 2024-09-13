;; -*- lexical-binding: t -*-
(setup eldoc
  (:hide-mode eldoc-mode))

(setup (:straight eldoc-box)
  (:with-mode eglot-managed-mode
    (:hook eldoc-box-hover-mode)))
(provide 'init-eldoc)
