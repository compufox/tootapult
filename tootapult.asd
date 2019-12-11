;;;; tootapult.asd

(asdf:defsystem #:tootapult
  :description "mastodon->twitter crossposter"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:tooter #:chirp #:dexador #:parser.ini)
  :components ((:file "package")
               (:file "tootapult"))
  :build-operation "program-op"
  :build-pathname "bin/tootapult"
  :entry-point "tootapult::main")


#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
