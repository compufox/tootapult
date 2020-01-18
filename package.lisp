;;;; package.lisp

(defpackage #:tootapult
  (:use #:cl #:with-user-abort)
  (:import-from :json
		:decode-json-from-string)
  (:import-from :chirp
		:*oauth-api-key*
		:*oauth-api-secret*
		:*oauth-access-token*
		:*oauth-access-secret*)
  (:import-from :unix-opts
		:define-opts
		:get-opts)
  (:import-from :str
		:replace-all
		:trim
		:join
		:words
		:split
		:blankp
		:containsp
		:starts-with-p)
  (:import-from :trivial-open-browser
		:open-browser)
  (:import-from :websocket-driver
		:make-client
		:ready-state)
  (:import-from :uiop
		:temporary-directory))
