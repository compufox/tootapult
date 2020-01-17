;;;; tootapult.asd

(asdf:defsystem #:tootapult
  :description "mastodon->twitter crossposter"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.2"
  :serial t
  :depends-on (#:chirp #:dexador #:cl-json
	       #:str #:with-user-abort #:plump
	       #:simple-config #:websocket-driver
	       #:html-entities #:unix-opts)
  :components ((:file "package")
	       (:file "util")
               (:file "tootapult"))
  :build-operation "program-op"
  :build-pathname "bin/tootapult"
  :entry-point "tootapult::main")


#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
