;; -*- lexical-binding: t -*-
(setup epa
  (:option epa-pinentry-mode 'loopback))

(setup (:straight-when pinentry (executable-find "gpg"))
  (pinentry-start))
(provide 'init-gnupg)
