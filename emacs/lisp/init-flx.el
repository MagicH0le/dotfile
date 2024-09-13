;; -*- lexical-binding: t -*-
(setup (:straight flx)
  (:after prescient
    (defvar-local input-query nil)
    (defun store-input-query (string &rest _args)
      "Store the current completion query in `input-query'."
      (setq input-query (replace-regexp-in-string " " "" string)))
    (:advice completion-all-completions :before #'store-input-query)

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
(provide 'init-flx)
