(in-package :tootapult)

(define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :version
   :description "prints version"
   :short #\v
   :long "version")
  (:name :config
   :description "use CONFIG"
   :short #\c
   :long "config"
   :arg-parser #'identity
   :meta-var "CONFIG")
  (:name :map
   :description "write map to FILE"
   :short #\m
   :long "map"
   :arg-parser #'identity
   :meta-var "FILE"))

(defun load-config ()
  "loads our config file and sets our variables accordingly"
  (when (conf:load-config *config-file*)
    (setf *mastodon-instance* (or (conf:config :mastodon-url)
				  (error "mastodon instance not provided"))
	  *mastodon-token* (or (conf:config :mastodon-token)
			       (authenticate-mastodon))
	  *mastodon-account-id* (get-mastodon-account-id)

	  *privacy-level* (member (conf:config :privacy-level '("public")) *privacy-values*
				  :test #'string=)
	  *filters* (mapcar #'str:trim (conf:config :filters))
	  *crosspost-mentions* (conf:config :crosspost-mentioned))

    ;; for the official builds we'll provide these tokens, so we
    ;;  don't wanna overwrite them lol
    ;; but if they arent already set then we load from our config
    (unless (and *oauth-api-key* *oauth-api-secret*)
	(setf *oauth-api-key* (conf:config :twitter-consumer-key)
	      *oauth-api-secret* (conf:config :twitter-consumer-secret)))
    
    (if (and (conf:config :twitter-access-token)
	     (conf:config :twitter-token-secret))
	(setf *oauth-access-token* (conf:config :twitter-access-token)
	      *oauth-access-secret* (conf:config :twitter-token-secret))
	(authenticate-twitter)))

  
  ;; if we had to authenticate with mastodon/twitter
  ;;  then our in-memory values are out of sync with our config
  ;;  and we should update the saved config
  (unless (string= (conf:config :mastodon-token "")
		   *mastodon-token*)
    (update-config "mastodon-token" *mastodon-token*))
  
  (unless (string= (conf:config :twitter-access-token "")
		   *oauth-access-token*)
    (update-config "twitter-access-token" *oauth-access-token*))

  (unless (string= (conf:config :twitter-token-secret "")
		   *oauth-access-secret*)
    (update-config "twitter-token-secret" *oauth-access-secret*)))

(defun get-mastodon-account-id ()
  "fetches the account that belongs to the token"
  (when (and *mastodon-instance* *mastodon-token*)
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
    (error (e) (error "unexpected error occurred")))))

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

(defun authenticate-twitter ()
  "opens the twitter authentication prompt"
  (open-browser (chirp:initiate-authentication))
  (chirp:complete-authentication (get-user-input "pin> ")))
  

(defun authenticate-mastodon ()
  "opens the mastodon authentication prompt"
  (let ((client (make-mastodon-client)))
    (multiple-value-bind (_ url) (tooter:authorize client)
      (open-browser url))
    (multiple-value-bind (_ token) (tooter:authorize client
						     (get-user-input "authentication token> "))
      token)))

(defun make-mastodon-client ()
  "returns a mastodon client with our proper values"
  (make-instance 'tooter:client
		 :base (format nil "https://~a" *mastodon-instance*)
		 :name "tootapult"
		 :website "https://github.com/compufox/tootapult"
		 :scopes '("read")))


(defun update-config (key value)
  (when *config-file*
    (with-open-file (out *config-file*
		     :direction :output 
		     :if-exists :append)
      (write-string (format nil "~%~a = ~a~%" key value) out))))
    

(defun get-user-input (&optional prompt)
  "gets input from the user through the command line

if PROMPT is provided, prints prompt out before waiting for user input"
  (when prompt
    (princ prompt)
    (finish-output))
  (read-line))
