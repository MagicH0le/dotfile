;; -*- lexical-binding: t -*-
(setup (:straight-when cnfonts (display-graphic-p))
  (:option cnfonts-directory (expand-file-name "cnfonts" no-littering-var-directory))
  (:hook-into after-init-hook))
(provide 'init-cnfonts)
