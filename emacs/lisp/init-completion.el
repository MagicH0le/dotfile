;; -*- lexical-binding: t -*-
(setup (:straight corfu)
  (:option corfu-style t
	       corfu-count 16
	       corfu-auto t
	       corfu-auto-prefix 1
	       corfi-auto-delay 0
	       corfu-on-exact-match nil
           corfu-preselect 'prompt
	       tab-always-indent 'complete)
  (:hook-into prog-mode)
  (:with-map corfu-map
    (:bind "TAB" corfu-next
	       "<tab>" corfu-next
	       "S-TAB" corfu-previous
	       "<backtab>" corfu-previous))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico are not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-echo-delay nil
		          corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (:hooks minibuffer-setup-hook corfu-enable-always-in-minibuffer))

(setup corfu-popupinfo
  (:hook-into corfu-mode))

(setup (:straight cape)
  (defun set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
		        (list (cape-capf-properties
		               (cape-capf-case-fold
			            (cape-capf-buster
			             (cape-capf-super (if arg arg (car completion-at-point-functions))
					                      :with #'tempel-complete #'cape-dabbrev #'cape-file)))
		               :sort t
		               :exclusive 'no))))
  (:hooks prog-mode-hook set-super-capf)
  (:hooks eglot-managed-mode-hook set-super-capf))

(setup (:straight corfu-terminal)
  (:unless (display-graphic-p)
    (:hook-into corfu-mode)))

(setup (:straight corfu-prescient)
  (:load-after prescient corfu)
  (:option corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode))

(setup (:straight flx)
  (:after prescient
    (defvar-local input-query nil)
    (defun store-input-query (string &rest _args)
      "Store the current completion query in `input-query'."
      (setq input-query (replace-regexp-in-string " " "" string)))
    (advice-add 'completion-all-completions :before #'store-input-query)

    (defvar vectico--total nil)
    (defvar corfu--total nil)

    (defvar flx-cache (make-hash-table :test 'equal :size 1000))
    (defun get-flx-score (str query)
      (or (gethash (cons str query) flx-cache)
	      (let ((score (condition-case nil
			               (car (flx-score str query flx-file-cache))
			             (error nil))))
	        (puthash (cons str query) score flx-cache)
	        score)))
    (defun flx-tiebreaker (c1 c2)
      (let ((total (or vertico--total corfu--total 0))
	        (query-length (length input-query)))
	    (if (and (< total 3000)
		         (> query-length 2)
		         (< (length c1) 100)
		         (< (length c2) 1000))
	        (let ((score1 (get-flx-score c1 input-query))
		          (score2 (get-flx-score c2 input-query)))
	          (cond ((and (integerp score1) (integerp score2))
		             (cond ((> score1 score2) -1)
			               ((< score1 score2) 1)
			               (t (- (length c1) (length c2)))))
		            (t 0)))
	      (- (length c1) (length c2)))))
    (setq prescient-tiebreaker #'flx-tiebreaker)

    (defun clear-flx-cache ()
      (clrhash flx-cache))

    (defvar flx-cache-timer nil)
    (setq flx-cache-timer (run-with-timer 3600 3600 #'clear-flx-cache))))
(provide 'init-completion)
