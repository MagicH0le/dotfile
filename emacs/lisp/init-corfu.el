;; -*- lexical-binding: t -*-
(setup (:straight corfu)
  (:option corfu-style t
	       corfu-count 16
	       corfu-auto t
	       corfu-auto-prefix 1
	       corfi-auto-delay 0
	       corfu-on-exact-match nil
           corfu-preselect 'prompt
	       tab-always-indent 'complete)
  (:hook-into prog-mode)
  (:with-map corfu-map
    (:bind "TAB" corfu-next
	       "<tab>" corfu-next
	       "S-TAB" corfu-previous
	       "<backtab>" corfu-previous))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico are not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil
		          corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (:hooks minibuffer-setup-hook corfu-enable-always-in-minibuffer))

(setup corfu-popupinfo
  (:load-after corfu)
  (:hook-into corfu-mode))

(setup (:straight corfu-terminal)
  (:load-after corfu)
  (:unless (display-graphic-p)
    (:hook-into corfu-mode)))

(setup (:straight corfu-prescient)
  (:load-after prescient corfu)
  (:option corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode))
(provide 'init-corfu)
