;;;; tootapult.lisp

(in-package #:tootapult)

(defun main ()
  "binary entry point"
  (let ((exit-status 0))
    (log:config :warn)
    
    (multiple-value-bind (opts args) (get-opts)

      ;; print out help usage
      (when (and (getf opts :help nil)
		 (every #'null opts args))
	(unix-opts:describe
	 :prefix "crossposts mastodon posts to twitter"
	 :usage-of "tootapult")
	(uiop:quit 0))

      ;; print out version info
      (when (getf opts :version nil)
	(format t "tootapult v~a"
		(asdf:component-version (asdf:find-system :tootapult)))
	(uiop:quit 0))

      ;; enables logging info
      (when (getf opts :log nil)
	(log:config :info :sane))

      ;; set our config and map file, if provided
      (setf *config-file* (getf opts :config)
	    *map-filename* (getf opts :map "posts.map")))

    (unless *config-file*
      (log:error "ERROR: config file not supplied~%       view help for correct usage")
      (uiop:quit 1))

    (handler-case
	(progn
	  (load-config)
	  (import-id-map))
      (error (e)
	(log:error "unexpected error occurred:" e)))
    
    (handler-case
	(with-user-abort
	  (start-crossposter))

      ;; if the user quits (ctl+c)
      (user-abort ()
	(log:info "shutting down"))

      ;; if we hit a major error
      (error (e)
	(log:error "encountered error:" e)
	(setf exit-status 1)))

    ;; ensure we save our mappings if we have any
    (when *id-mappings*
      (export-id-map))

    ;; quit using the proper status code
    (uiop:quit exit-status)))

(defun start-crossposter ()
  "starts streaming from the mastodon endpoint, handling new events as the arise"
  (unless (chirp:account/verify-credentials)
    (error "incorrect twitter credentials"))

  (if (equal (conf:config :polling-method) "streaming")
      (start-streaming-polling)
      (start-rest-polling)))

(defun start-streaming-polling ()
  (setf *websocket-client*
	(make-client (format nil "~a/api/v1/streaming?access_token=~a&stream=user"
			     (get-mastodon-streaming-url)
			     *mastodon-token*)))

  (wsd:on :open *websocket-client*
	  #'print-open)
  (wsd:on :message *websocket-client*
	  #'process-message)
  (wsd:on :close *websocket-client*
	  #'print-close)

  (wsd:start-connection *websocket-client*)

  ;; drop into a loop
  (loop while (eql (ready-state *websocket-client*) :open)
	do (sleep 2)))

(defun start-rest-polling ()
  (loop
    
    ;; get our starting id (most recent post made by account)
    with starting-id = (mastodon-request (concatenate 'string "accounts/" *mastodon-account-id* "/statuses") t)

    ;; wait the configured amount of time
    do (sleep (conf:config :polling-frequency 500))

       ;; then get the most recent posts and loop through them
       (loop
	 for p in (mastodon-request (concatenate 'string "accounts/" *mastodon-account-id* "/statuses") t
				    `("min_id" ,starting-id))
	 do (when (and (equal (agetf (agetf p :account) :id) *mastodon-account-id*)
		       (should-crosspost-p p))

	      ;; if the status id is greater that our starting one we save it
	      (when (> (parse-integer (agetf p :id))
		       (parse-integer starting-id))
		(setf starting-id (agetf p :id)))
	      
	      ;; if the post doesnt have a reblog
	      ;;   crosspost the status
	      ;; otherwise
	      ;;   see if we have any stored posts with the reblog's id
	      ;;   and retweet the corresponding tweets
	      (if (null (agetf p :reblog))
		  (post-to-twitter p)
		  (retweet-post (agetf (agetf p :reblog) :id)))))
       (scan-and-delete)))

(defun process-message (message)
  "processes our incoming websocket message"
  (let ((parsed (decode-json-from-string message)))
    (handler-case (dispatch (agetf parsed :event) (agetf parsed :payload))
      (error (e)
	(log:error "an error occurred:" e)))))

(defun print-open ()
  "prints a message when the websocket connection opens"
  (log:info "connected!"))

(defun print-close (&key code reason)
  "prints a message when the websocket closes, printing the reason and code"
  (when (and code reason)
    (log:error "connection broken.~%reason: '" reason "' (code=" code ")")))


#|

event-types: update, delete, notification
data: json post object, status id, json notification object

|#
  
(defun dispatch (event-type data)
  "calls different functions with DATA depending on what EVENT-TYPE we recieved"
  (let ((parsed-data (decode-json-from-string data)))
    (cond
      ;; if its a new status
      ;;    the posting account is ours
      ;;    and the post doesnt contain any of our filtered words
      ;;  crosspost it
      ((and (string= event-type "update")
	    (equal (agetf (agetf parsed-data :account) :id) *mastodon-account-id*)
	    (should-crosspost-p parsed-data))

       ;; if the post doesnt have a reblog
       ;;   crosspost the status
       ;; otherwise
       ;;   see if we have any stored posts with the reblog's id
       ;;   and retweet the corresponding tweets
       (if (null (agetf parsed-data :reblog))
	   (post-to-twitter parsed-data)
	   (retweet-post (agetf (agetf parsed-data :reblog) :id))))

      ;; if its a delete event, and we have the id stored somewhere
      ;;  delete it
      ((and (string= event-type "delete")
	    (member data *id-mappings* :key #'car :test #'equal))
       (delete-post data))

      ;; if we dont know what the event is, ignore it :3
      (t nil))))

(defun post-to-twitter (status)
  ;; this loop macro is bad and i feel bad writing it.
  ;; should be split into PROPER lisp code lol
  (when (log:info)
    (let ((id (agetf status :id)))
      (log:info "crossposting status " id)))
  
  (loop
     with tweet-length = 0

     with tweet-words = (get-words (build-post status))
     with tweet = nil
     with last-id = (cdr (find (agetf status :in--reply--to--id nil) *id-mappings*
			       :test #'equal :key #'car :from-end t))
     with media-list = (get-post-media (agetf status :media--attachments))
       
     while tweet-words
     do
       (push (pop tweet-words) tweet)
       (setf tweet-length (chirp:compute-status-length (join " " tweet)))
       
     when (> tweet-length *max-tweet-length*)
     do
       (push (pop tweet) tweet-words)
       (setf last-id (slot-value (chirp:tweet (join " " (reverse tweet))
					      :reply-to last-id
					      :file media-list)
				 'chirp::%id)
	     media-list (clean-downloads media-list)
	     tweet nil
	     *id-mappings* (append *id-mappings*
				   `((,(agetf status :id) . ,last-id))))
       
     finally
       (when tweet
	 (setf *id-mappings*
	       (append *id-mappings*
		       `((,(agetf status :id) . ,(slot-value (chirp:tweet (join " " (reverse tweet))
									  :reply-to last-id
									  :file media-list)
							     'chirp::%id))))))))

(defun delete-post (id)
  "deletes tweets with matching toot ID

removes them from the map list"
  ;; get all of our mapped tweet ids
  (let ((tweet-ids (gather-tweet-ids id)))

    (when (log:info)
      (log:info "deleting tweets:" tweet-ids))
    
    ;; delete all of them from twitter
    (mapcar #'chirp:statuses/destroy tweet-ids)

    ;; and them remove them from our mappings
    (setf *id-mappings* (remove id *id-mappings* :test #'equal :key #'car))))

(defun retweet-post (id)
  (when (log:info)
    (log:info "retweeting status " id))
  (mapcar #'chirp:statuses/retweet (gather-tweet-ids id)))
