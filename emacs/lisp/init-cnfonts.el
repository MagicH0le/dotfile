;; -*- lexical-binding: t -*-
(setup (:straight cnfonts)
  (:option cnfonts-directory (expand-file-name "cnfonts" no-littering-var-directory))
  (:hook-into after-init-hook))
(provide 'init-cnfonts)
