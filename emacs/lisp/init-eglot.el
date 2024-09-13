;; -*- lexical-binding: t -*-
(setup eglot
  (:with-map eglot-mode-map
    (:bind "C-c r" eglot-rename
	       "C-c o" eglot-code-action-organize-imports
	       "C-c a" eglot-code-actions
	       "C-c h" eldoc
	       "C-c d" xref-find-definitions))
  (:option eglot-events-buffer-config '(:size 0 :format short)
	       eglot-ignored-server-capabilities '(:documentationHighlightProvider)
	       eglot-stay-out-of '(flymake)
	       eglot-send-changes-idle-time 1.0))

(setup (:straight eglot-tempel)
  (:load-after tempel eglot)
  (:hook-into eglot-managed-mode))

(setup (:straight consult-eglot)
  (:load-after eglot consult)
  (:with-mode eglot-mode
    (:bind "C-c s" consult-eglot-symbols)))

(setup (:straight (eglot-x :type git :host github :repo "nemethf/eglot-x"))
  (:load-after eglot)
  (:require eglot-x)
  (eglot-x-setup))

(setup (:straight eglot-signature-eldoc-talkative)
  (:load-after eldoc-box eglot)
  (:advice eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative))
(provide 'init-eglot)
