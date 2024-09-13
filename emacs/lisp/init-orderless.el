;; -*- lexical-binding: t -*-
(setup (:straight orderless)
  (:option completion-styles '(orderless basic)
	       completion-category-defaults nil
	       completion-category-overrides nil)
  (:after corfu
    (:require orderless)
    (defun orderless-fast-dispatch (word index total)
      (and (= index 0) (= total 1) (length< word 4)
	       'orderless-literal-prefix))
    (orderless-define-completion-style orderless-fast
      (orderless-style-dispatchers '(orderless-fast-dispatch))
      (orderless-matching-styles '(orderless-flex)))
    (defun setup-corfu-for-orderless ()
      (setq-local corfu-auto-delay 0
		          corfu-auto-prefix 1
		          completion-styles '(orderless-fast)))
    (:with-mode corfu-mode
      (:hook setup-corfu-for-orderless))))
(provide 'init-orderless)
