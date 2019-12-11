;;;; package.lisp

(defpackage #:tootapult
  (:use #:cl #:with-user-abort)
  (:import-from :json
		:decode-json-from-string)
  (:import-from :sanitize
		:clean)
  (:import-from :chirp
		:*oauth-api-key*
		:*oauth-api-secret*
		:*oauth-access-token*
		:*oauth-access-secret*)
  (:import-from :str
		:replace-all
		:trim
		:words
		:containsp
		:starts-with-p)
  (:import-from :uiop
		:temporary-directory))
