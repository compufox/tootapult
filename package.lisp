;;;; package.lisp

(defpackage #:tootapult
  (:use #:cl)
  (:import-from :parser.ini
		:parse)
  (:import-from :uiop
		:temporary-directory))
