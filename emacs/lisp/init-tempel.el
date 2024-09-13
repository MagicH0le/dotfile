;; -*- lexical-binding: t -*-
(setup (:straight tempel)
  (:global "M-+" tempel-complete
	       "M-*" tempel-insert))

(setup (:straight tempel-collection)
  (:load-after tempel))
(provide 'init-tempel)
