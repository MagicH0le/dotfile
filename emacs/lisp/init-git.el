;; -*- lexical-binding: t -*-
(setup (:straight magit)
  (:general
   (:states '(normal visual emacs)
            :keymaps 'override
            :prefix "SPC"
            "g" '(:ignore t :which-key "Git")
            "gs" '(magit-status :which-key "Show status")
            "gb" '(magit-branch :which-key "Show all branches")
            "gco" '(magit-checkout :which-key "Checkout branch")
            "gr" '(magit-rebase :which-key "Rebase on a branch")
            "gsq" '(magit-rebase-squash :which-key "Squash commits")
            "gc" '(magit-commit :which-key "Commit")
            "gp" '(magit-push-popup :which-key "Push"))))
(provide 'init-git)
