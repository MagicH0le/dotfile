;; -*- lexical-binding: t -*-
(setup org
  (:option org-directory "~/Org"
	       org-agenda-files (list "inbox.org" "agenda.org" "project.org")
	       org-capture-templates `(("i" "收件箱" entry (file "inbox.org")
				                    ,(concat "* TODO %?\n" "/录入于/ %U")))
	       org-refile-use-outline-path nil
	       org-refile-targets '((org-agenda-files :maxlevel . 9))
	       org-outline-path-complete-in-steps nil
	       org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANCELED(c)"))
	       org-log-done 'time
	       org-agenda-custom-commands    '(("g" "GTD"
					                        ((agenda ""
                                                     ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                                                      (org-deadline-warning-days 0)))
                                             (todo "NEXT"
						                           ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
						                            (org-agenda-prefix-format "  %i %-12:c [%e] ")
						                            (org-agenda-overriding-header "\n任务\n")))
                                             (agenda nil
                                                     ((org-agenda-entry-types '(:deadline))
                                                      (org-agenda-format-date "")
                                                      (org-deadline-warning-days 7)
                                                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                                                      (org-agenda-overriding-header "\n近期截止")))
                                             (tags-todo "收件箱"
							                            ((org-agenda-prefix-format "  %?-12t% s")
							                             (org-agenda-overriding-header "\n收件箱\n")))
                                             (tags "CLOSED>=\"<today>\""
						                           ((org-agenda-overriding-header "\n今日已完成\n")))))))
  (:global "C-c o c" org-capture
	       "C-c o a" org-agenda)
  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'."
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (:hooks org-after-todo-state-change-hook log-todo-next-creation-date)
  (:with-mode org-capture-mode
    (:hook delete-other-windows)))
(provide 'init-org)
