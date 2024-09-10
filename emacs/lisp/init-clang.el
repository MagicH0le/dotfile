;; -*- lexical-binding: t -*-
(setup cc-mode
  (:option c-basic-offset 4)
  (:with-mode c-mode (:hook eglot-ensure))
  (:with-mode c++-mode (:hook eglot-ensure)))
(provide 'init-clang)
