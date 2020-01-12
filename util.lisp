(in-package :tootapult)

(defun load-config ()
  "loads our config file and sets our variables accordingly"
  (when (conf:load-config (or (first (uiop:command-line-arguments))
			      "tootapult.config"))
    (setf *mastodon-instance* (conf:config :mastodon-url)
	  *mastodon-token* (conf:config :mastodon-token)
	  *mastodon-account-id* (get-mastodon-account-id)
	  *oauth-api-key* (conf:config :twitter-consumer-key)
	  *oauth-api-secret* (conf:config :twitter-consumer-secret)
	  *oauth-access-token* (conf:config :twitter-access-token)
	  *oauth-access-secret* (conf:config :twitter-token-secret)

	  *privacy-level* (member (conf:config :privacy-level '("public")) *privacy-values*
				  :test #'string=)
	  *filters* (mapcar #'str:trim (conf:config :filters))
	  *crosspost-mentions* (conf:config :crosspost-mentioned))))

(defun get-mastodon-account-id ()
  "fetches the account that belongs to the token"
  (handler-case 
      (agetf (decode-json-from-string
	      (dex:get (format nil "https://~a/api/v1/accounts/verify_credentials"
			       (replace-all "https://" "" *mastodon-instance*))
		       :headers `(("Authorization" . ,(concatenate 'string
								   "Bearer "
								   *mastodon-token*)))))
	     :id)
    (dex:http-request-unauthorized (e) (error "incorrect mastodon token"))
    (dex:http-request-forbidden (e) (error "account unavailable"))
    (error (e) (error "unexpected error occurred"))))

(defun get-mastodon-streaming-url ()
  "gets the websocket url for the mastodon instance"
  (handler-case
      (agetf
       (agetf (decode-json-from-string
	       (dex:get (format nil "https://~a/api/v1/instance"
				(replace-all "https://" "" *mastodon-instance*))))
	      :urls)
       :streaming--api)
    (error (e) (error "unexpected error occurred"))))
	
(defun agetf (place indicator &optional default)
  "getf but for alists"
  (or (cdr (assoc indicator place :test #'equal))
      default))

(defun get-words (text)
  "properly splits a toot up into words, preserving newlines"
  (reverse
    (loop
       with words = ()
       with cur-start = 0
				   
       if (< cur-start (length text))
       do
	 (let ((s (search " " text :test #'string= :start2 cur-start)))
	   (setf cur-start
		 (if s
		     (prog1 (1+ s)
		       (push (str:substring cur-start s text) words))
		     (length text))))
       else
       return words)))

(defun self-reply-p (status)
  "checks if STATUS is a self-reply"
  (let ((is-reply (agetf status :in--reply--to--id)))
    (cond
      ((and is-reply (equal (agetf status :in--reply--to--account--id)
			    *mastodon-account-id*))
       t)
      ((not is-reply) t)
      (t nil))))

(defun should-crosspost-p (status)
  "checks if we should crosspost the status"
  (let ((filtered (filter-present-p (agetf status :content)))
	(mentions (agetf status :mentions))
	(is-reply (agetf status :in--reply--to--id)))
    (and (member (agetf status :visibility) *privacy-level* :test #'string=)
	 (or *crosspost-mentions* (null mentions))
	 (not filtered)
	 (or (not is-reply)
	     (self-reply-p status)))))

(defun filter-present-p (status-text)
  "checks if any filter words appear in STATUS-TEXT"
  (loop
     for f in *filters*

     when (containsp f status-text)
     return t))

(defun sanitize-content (post)
  "removes all html tags from POST

returns a list of each newline-separated paragraph"
  (remove-if #'blankp
	     (flatten
	      (loop
		 with content
		 for c across (plump:children (plump:parse (html-entities:decode-entities post)))
		 do (push (loop
			   for c1 across (plump:children c)
			   collect (plump:text c1))
			  content)
		 finally (return (reverse content))))))

(defun import-id-map ()
  "reads the mappings and sets *ID-MAPPINGS*"
  (with-open-file (in *map-filename*
		      :direction :input
		      :if-does-not-exist nil)
    (when in
      (let ((map (read in nil nil)))
	(setf *id-mappings* map)))))

(defun export-id-map ()
  "saves our mappings for later"
  (with-open-file (out *map-filename*
		       :direction :output
		       :if-exists :overwrite
		       :if-does-not-exist :create)
    (write-string (format nil "~s" *id-mappings*) out)))

(defun flatten (lst)
  "flattens a list"
  (labels ((rflatten (lst1 acc)
	     (dolist (el lst1)
	       (if (listp el)
		   (setf acc (rflatten el acc))
		   (push el acc)))
	     acc))
    (reverse (rflatten lst nil))))

(defun gather-tweet-ids (toot-id)
  "gets all tweet ids that map to TOOT-ID from *ID-MAPPINGS*"
  (loop
   for (key . value) in *id-mappings*
		      
   when (equal key toot-id)
   collect value))

(defun replace-all-mentions (mentions content)
  "replaces all @mentions in CONTENT with URL to account"
  (let ((fixed-content content))
    (dolist (mtn mentions fixed-content)
      (setf fixed-content
	    (replace-all (concatenate 'string "@" (first (split #\@ (agetf mtn :acct))))
			 (agetf mtn :url)
			 fixed-content)))))
