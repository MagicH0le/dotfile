;; -*- lexical-binding: t -*-
(setup dired
  (when (string= system-type "darwin")
    (:option dired-use-ls-dired t
             insert-directory-program "/opt/homebrew/bin/gls"
             dired-listing-switches "-aBhl --group-directories-first")))

(setup (:straight dirvish)
  (dirvish-override-dired-mode))
(provide 'init-dired)
