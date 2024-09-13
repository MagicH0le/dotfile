;; -*- lexical-binding: t -*-
(setup (:straight prescient)
  (:require prescient)
  (:option prescient-aggressive-file-save t)
  (prescient-persist-mode))

(setup (:straight vertico-prescient)
  (:load-after prescient vertico)
  (:option vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode))
(provide 'init-prescient)
