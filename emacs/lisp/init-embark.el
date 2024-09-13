;; -*- lexical-binding: t -*-
(setup (:straight embark)
  (:option prefix-help-command #'embark-prefix-help-command)
  (:global "C-." embark-act
	       "C-;" embark-dwim
	       "C-h B" embark-bindings))

(setup (:straight embark-consult)
  (:load-after consult embark)
  (:with-mode embark-collect-mode
    (:hook consult-preview-at-point-mode)))
(provide 'init-embark)
