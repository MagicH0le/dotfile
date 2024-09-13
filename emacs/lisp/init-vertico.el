;; -*- lexical-binding: t -*-
(setup (:straight vertico)
  (:option vertico-cycle t)
  (vertico-mode))

(setup vertico-repeat
  (:load-after vertico)
  (:hooks minibuffer-setup-hook vertico-repeat-save))

(setup vertico-directory
  (:load-after vertico)
  (:with-map vertico-map
    (:bind "<backspace>" vertico-directory-delete-char)))

(setup vertico-buffer
  (:load-after vertico)
  (:option vertico-buffer-display-action '(display-buffer-at-bottom))
  (vertico-buffer-mode))

(setup (:straight (vertico-truncate :type git :host github :repo "jdtsmith/vertico-truncate"))
  (:load-after vertico)
  (:require vertico-truncate)
  (vertico-truncate-mode))
(provide 'init-vertico)
