;; -*- lexical-binding: t -*-
(setup buffer
  (:general
   (:states '(normal)
            "b" '(:ignore t :which-key "Buffers")
            "bs" '(:ignore t :which-key "Split")
            "bsv" '(split-right :which-key "Vertical")
            "bsh" '(split-below :which-key "Horizontal")))

  (defun split-right ()
    "Split current window into two, left and right."
    (interactive)
    (split-window-right)
    (other-window 1))

  (defun split-below ()
    "Split current window into two, top and bottom."
    (interactive)
    (split-window-below)
    (other-window 1)))
(provide 'init-buffer)
