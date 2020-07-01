;;;; tootapult.asd

(asdf:defsystem #:tootapult
  :description "mastodon->twitter crossposter"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.4"
  :serial t
  :depends-on (#:chirp #:dexador #:cl-json
	       #:str #:with-user-abort #:plump
	       #:simple-config #:websocket-driver
	       #:html-entities #:unix-opts
	       #:trivial-open-browser
	       #:tooter #:log4cl #:cl-ppcre
	       #:deploy)
  :components ((:file "package")
	       (:file "util")
	       (:file "posts")
               (:file "tootapult"))
  :build-operation "deploy-op"
  :build-pathname "tootapult"
  :entry-point "tootapult::main")
