(straight-use-package 'setup)
(require 'setup)
(add-to-list 'setup-modifier-list 'setup-wrap-to-demote-errors)
(setup-define :quit
  #'setup-quit
  :documentation "Unconditionally abort the evaluation of the current body.")

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
	    (setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :hide-mode
  (lambda (&optional mode)
    (let* ((mode (or mode (setup-get 'mode)))
	       (mode (if (string-match-p "-mode\\'" (symbol-name mode))
		             mode
		           (intern (format "%s-mode" mode)))))
      `(setq minor-mode-alist
	         (delq (assq ',mode minor-mode-alist)
		           minor-mode-alist))))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE."
  :indent 1)

(setup-define :unless
  (lambda (condition &rest body)
    `(unless ,condition ,@body))
  :documentation "Evaluates BODY unless the condition evaluates to `t'."
  :indent 1)

(with-eval-after-load 'straight
  (setup-define :straight
    (lambda (recipe)
      `(unless (straight-use-package ',recipe)
	     ,(setup-quit)))
    :documentation
    "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
    :repeatable t
    :shorthand (lambda (sexp)
		         (let ((recipe (cadr sexp)))
		           (if (consp recipe)
		               (car recipe)
		             recipe))))

  (setup-define :straight-when
    (lambda (recipe condition)
      `(if ,condition
	       (straight-use-package ',recipe)
	     ,(setup-quit)))
    :documentation
    "Install RECIPE with `straight-use-package' when CONDITION is met.
If CONDITION is false, stop evaluating the body.  This macro can
be used as HEAD, and will replace itself with the RECIPE's
package.  This macro is not repeatable."
    :repeatable nil
    :indent 1
    :shorthand (lambda (sexp)
		         (let ((recipe (cadr sexp)))
		           (if (consp recipe)
		               (car recipe)
		             recipe)))))

(with-eval-after-load 'general
  (setup-define :general
    (lambda (&rest definitions)
      (let (expansions)
        (dolist (definition definitions)
          (push `(general-define-key ,@definition) expansions))
        (macroexp-progn (nreverse expansions))))
    :documentation "Configure keybindings using general."))
(provide 'site-setup)
