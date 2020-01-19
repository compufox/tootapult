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

(in-package :tootapult)

(defvar *max-tweet-length* 280)
(defvar *privacy-values* '("direct" "private" "unlisted" "public"))
(defvar *crosspostable-file-types*
  '("jpg" "jpeg" "png" "gif")
  "filetypes that we can crosspost")

(defvar *map-filename*)
(defvar *config-file*)
(defvar *mastodon-instance*)
(defvar *mastodon-account-id*)
(defvar *mastodon-token*)
(defvar *privacy-level*)
(defvar *crosspost-mentions*)
(defvar *websocket-client*)

;; these need 'nil' because otherwise they cant be evaluated, period
(defvar *id-mappings* nil)
(defvar *filters* nil)
