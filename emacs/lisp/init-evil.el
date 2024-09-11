;; -*- lexical-binding: t -*-
(setup (:straight evil)
  (:option evil-want-integration t
           evil-want-keybinding nil
           evil-search-module 'evil-search
           evil-undo-system 'undo-redo)
  (evil-mode))

(setup (:straight evil-collection)
  (:load-after evil-mode)  
  (:after evil-collection-unimpaired
    (:hide-mode evil-collection-unimpaired-mode))
  (evil-collection-init))

(setup (:straight evil-commentary)
  (:load-after evil-mode)
  (:hide-mode evil-commentary-mode)
  (evil-commentary-mode))

(setup (:straight evil-surround)
  (:load-after evil-mode)
  (:hide-mode evil-surround-mode)
  (global-evil-surround-mode))

(setup (:straight evil-mc)
  (:load-after evil-mode)
  (:hide-mode evil-mc-mode)
  (global-evil-mc-mode))

(setup (:straight general)
  (general-auto-unbind-keys))
(provide 'init-evil)
