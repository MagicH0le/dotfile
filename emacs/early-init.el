;; -*- lexical-binding: t -*-
(dolist (mode '(tool-bar-mode
                menu-bar-mode
                scroll-bar-mode))
  (if (fboundp mode)
      (funcall mode -1)))

(setq package-enable-at-startup nil)
