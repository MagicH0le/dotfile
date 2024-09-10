;; -*- lexical-binding: t -*-
(setup userinfo
  (:option user-full-name "c4droid"
	       user-mail-address "c4droid@foxmail.com"))

(setup smtpmail
  (:option smtpmail-default-smtp-server "smtp.qq.com"
	       smtpmail-smtp-server "smtp.qq.com"
	       smtpmail-smtp-service 465
	       smtpmail-stream-type 'ssl))

(setup sendmail
  (:option send-mail-function 'smtpmail-send-it
	       message-send-mail-function 'smtpmail-send-it))

(setup gnus
  (:option gnus-select-method '(nnimap "Foxmail"
				                       (nnimap-address "imap.qq.com")
				                       (nnimap-stream ssl)
				                       (nnimap-expunge t)
				                       (nnimap-expiry-wait 30)))
  (:with-mode gnus-group-mode
    (:hook gnus-topic-mode)))
(provide 'init-mail)
