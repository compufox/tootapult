;;;; tootapult.lisp

(in-package #:tootapult)

(defvar *max-tweet-length* 280)
(defvar *mastodon-instance*)
(defvar *mastodon-account*)
(defvar *mastodon-token*)
(defvar *id-mappings*)
(defvar *filters*)
(defvar *privacy-level* "public")
(defvar *privacy-values* '("direct" "private" "unlisted" "public"))
(defvar *crosspost-mentions*)

(defun main ()
  "binary entry point"
  (load-config)
  (handler-case
      (with-user-abort (start-crossposter))
    (user-abort ()
      (format t "shutting down~%")
      (uiop:quit))
    (error (e)
      (format t "encountered error ~a~%" e)
      (uiop:quit 1))))

(defun load-config ()
  "loads our config file and sets our variables accordingly"
  (let ((config (parse-file (or (first (uiop:command-line-arguments))
				"tootapult.config"))))
    (setf *mastodon-instance* (agetf config "mastodon-url")
	  *mastodon-token* (agetf config "mastodon-token")
	  *mastodon-account* (get-account)
	  *oauth-api-key* (agetf config "twitter-consumer-key")
	  *oauth-api-secret* (agetf config "twitter-consumer-secret")
	  *oauth-access-token* (agetf config "twitter-access-token")
	  *oauth-access-secret* (agetf config "twitter-token-secret")

	  *filters* (mapcar #'str:trim (split #\, (agetf config "filters")))
	  *crosspost-mentions* (agetf config "crosspost-mentioned"))))

(defun get-account ()
  "fetches the account that belongs to the token"
  (decode-json-from-string
   (dex:get (format nil "https://~a/api/v1/accounts/verify_credentials"
		    (replace-all "https://" "" *mastodon-instance*))
	    :headers `(("Authorization" . ,(concatenate 'string
							"Bearer "
							*mastodon-token*))))))

(defun parse-file (file)
  "parses FILE, returning an alist"
  (with-open-file (conf file)
    (loop
       for line = (read-line conf nil)
       for input = (split #\= line)
       while line
       when (and (not (starts-with-p "#" line)) (not (blankp line)))
       collect (cons
	        (str:trim (car input))
		(let ((value (trim (cadr input))))
		  (if (or (string= value "false")
			  (string= value "nil"))
		      nil
		      value))))))
	
(defun agetf (place indicator &optional default)
  "getf but for alists"
  (or (cdr (assoc indicator place :test #'equal))
      default))

(defun start-crossposter ()
  "starts streaming from the mastodon endpoint, handling new events as the arise"
  (unless (chirp:account/verify-credentials)
    (error "incorrect credentials"))

  (loop
     with masto-stream = (dex:get (format nil "https://~a/api/v1/streaming/user"
					  (replace-all "https://" "" *mastodon-instance*))
				  :keep-alive t
				  :want-stream t)

     while masto-stream
     do (let* ((line (read-line masto-stream))
	       (type (subseq line 7)))
	  (when (starts-with-p "event:" line)
	    (dispatch type (subseq (read-line masto-stream) 6))))))

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
      ;;    and its not a reblog
      ;;  crosspost it
      ((and (string= event-type "update")
	    (equal (agetf parsed-data :account) *mastodon-account*)
	    (null (agetf parsed-data :reblog)))
       (post-to-twitter (decode-json-from-string data)))

      ;; if its a delete event, and we have the id stored somewhere
      ;;  delete it
      ((and (string= event-type "delete")
	    (member parsed-data *id-mappings* :key #'car :test #'=))
       (delete-post parsed-data))

      ;; if we dont know what the event is, ignore it :3c
      (t nil))))

;;  clean doesnt properly handle newlines, so thats gonna be something
;;  im probably going to have to do
(defun post-to-twitter (status)
  ;; this loop macro is bad and i feel bad writing it.
  ;; should be split into PROPER lisp code lol
    
  (when (should-crosspost-p status)
    
      ;; should also add cw's please. do not forget
    (loop
       with tweet-length = 0
       with tweet-words = (words (build-post status))
       with tweet = nil
       with last-id = nil
       with media-list = (get-post-media (agetf status :media-attachments))
	 
       while tweet-words
       do
	 (push (pop tweet-words) tweet)
	 (setf tweet-length (chirp:compute-status-length (join #\Space tweet)))
	 
       when (> tweet-length *max-tweet-length*)
       do
	 (push (pop tweet) tweet-words)
	 (setf last-id (slot-value (chirp:tweet (join #\Space (reverse tweet))
						:reply-to last-id
						:file media-list)
				   'chirp::%id)
	       media-list nil
	       tweet nil
	       *id-mappings* (append *id-mappings*
				     (cons (agetf status :id)
					   last-id)))
				     
       finally
	 (chirp:statuses/update (join #\Space (reverse tweet))))))

(defun should-crosspost-p (status)
  "checks if we should crosspost the status"
  (let ((filtered (filter-present-p content))
	(mentions (agetf status :mentions)))
    (and (or *crosspost-mentions* (null mentions))
	 (not filtered))))

(defun filter-present-p (status-text)
  "checks if any filter words appear in STATUS-TEXT"
  (loop
     for f in *filters*

     when (containsp f status-text)
     return t))

(defun build-post (status)
  "builds post from status for crossposting

adds CW, if needed
sanitizes html tags
replaces mentions, if specified"
  (let ((cw (agetf status :spoiler-text))
	(mentions (agetf status :mentions))
	(content (trim (clean (replace-all "</p><p>"
					   (string #\Newline)
					   (agetf status :content))))))
    (labels ((replace-mentions (m)
	       (let ((handle (concatenate 'string "@"
					  (first
					   (split #\@ (agetf m :acct))))))
		 (replace-all handle (agetf m :url) content))))
      
    (concatenate 'string
		 cw (when cw (format nil ":~&~&"))
		 (if *crosspost-mentions*
		     (mapcar #'replace-mention mentions)
		     content)))))

(defun get-post-media (media-list)
  "downloads all media in MEDIA-LIST"
  (mapcar (lambda (attachment)
	    (download-media (agetf attachment :url)))
	  media-list))

(defun download-media (url)
  "downloads URL to a generated filename.
returns the filename"
  (let ((filename (merge-pathnames (concatenate 'string
						(symbol-name (gensym "ATTACHMENT-"))
						(pathname-type url))
				   (temporary-directory))))
    (handler-case
	(dex:fetch url filename)
      (error ()
	nil))
    filename))

(defun delete-post (id)
  ;; get all of our mapped tweet ids
  (let ((tweet-ids (loop
		      for (key . value) in *id-mappings*

		      when (= key id)
		      collect value)))
    
    ;; delete all of them from twitter
    (mapcar #'chirp:statuses/destroy tweet-ids)

    ;; and them remove them from our mappings
    (setf *id-mappings* (remove id *id-mappings* :test #'= :key #'car))))
