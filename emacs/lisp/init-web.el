;; -*- lexical-binding: t -*-
(setup js-mode
  (:option js-indent-level 2)
  (:hook eglot-ensure))

(setup (:straight typescript-mode)
  (:hook eglot-ensure))

(setup (:straight web-mode)
  (:option web-mode-markup-indent-offset 2
	       web-mode-css-indent-offset 2
	       web-mode-code-indent-offset 2))

(setup (:straight emmet-mode)
  (:option emmet-move-cursor-between-quotes t
	       emmet-self-closing-tag-style "")
  (:hook-into web-mode))
(provide 'init-web)
