;; -*- lexical-binding: t -*-
(setup (:straight web-mode)
  (:option web-mode-markup-indent-offset 2
	       web-mode-css-indent-offset 2
	       web-mode-code-indent-offset 2))

(setup (:straight emmet-mode)
  (:option emmet-move-cursor-between-quotes t
	       emmet-self-closing-tag-style "")
  (:hook-into web-mode))
(provide 'init-html)
