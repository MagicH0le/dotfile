;; -*- lexical-binding: t -*-
(setup (:straight dirvish)
  (:load-after dired)
  (:global "C-c f" dirvish-side)
  (dirvish-override-dired-mode))
(provide 'init-dirvish)
