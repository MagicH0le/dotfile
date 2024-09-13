;; -*- lexical-binding: t -*-
(setup (:straight telega)
  (:option telega-use-images (or (display-graphic-p) (daemonp))
	       telega-server-libs-prefix (if (eq system-type 'darwin) "/opt/homebrew")
	       telega-proxies '((:server "127.0.0.1" :port 7890 :enable t :type (:@type "proxyTypeHttp")))
	       telega-avatar-workaround-gaps-for '(return t)))
(provide 'init-telega)
