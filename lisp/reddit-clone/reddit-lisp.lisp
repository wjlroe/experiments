(in-package :cl-user)

;(defmacro uselibrary ((library))
;  `(asdf:oos 'asdf:load-op ',library)
;  `(format t "~%Library: ~A loaded! Yay~%" ',library))

(require 'hunchentoot)
(require 'cl-who)
(require 's-utils)
(require 'arnesi)
(require 'cl-utilities)
(require 'clsql)

(clsql:locally-enable-sql-reader-syntax)
(setf clsql:*default-caching* nil)

;; handler functions - add, create, update, delete, index? 
;; url functions - model, action, params
;; model classes
;; templates? - not in this example...

(defvar *server* (hunchentoot:start-server :port 8989))

(defmacro with-db ((database) &body body)
  `(clsql:with-database (,database '("localhost" "reddit" "reddit" "reddit")
				   :pool t :if-exists :old :database-type :mysql)
     ,@body))

(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))

(defvar *handlers* (make-hash-table))

(defmethod gethandler ((name symbol))
  (gethash name *handlers*))

(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))

(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))

(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))

(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
		(url arnesi:it)))

(defmethod command ((name symbol) &rest args)
  (format nil "~A?~{~(~A~)=~A~^&~}" (url name) args))

(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))

(setf hunchentoot:*dispatch-table* (list 'our-handler))

(defmacro defpage ((name url) &body body)
  (cl-utilities:once-only (url)
			  `(setf (gethandler ',name)
				 (make-instance 'handler
						:url ,url
						:name ',name
						:handler (lambda ()
							   ,@body)))))

(clsql:def-view-class link ()
  ((title
    :reader title
    :initarg :title
    :type string)
   (url
    :reader url
    :initarg :url
    :type string)
   (id
    :reader id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (timestamp
    :reader timestamp
    :initform (get-universal-time)
    :type integer ;; TODO: Change to DB timestamp type
    :db-type "bigint")))

(defmethod (setf score) :after (value (link link))
  (with-db (db)
    (clsql:update-record-from-slot link 'score :database db)))

(defmethod score ((link link))
  (with-db (db)
    (loop for r in (clsql:select 'rating
				 :where [ = [ linkid ] (id link) ]
				 :flatp t :database db)
	 sum (size r))))

(defun create-link (title url)
  (make-instance 'link :title title :url url))

(defmethod age ((link link))
  (- (get-universal-time) 
     (timestamp link)))

(defun add-link (link)
  (with-db (db)
    (clsql:update-records-from-instance link :database db)
    (when (null (id link))
      (setf (slot-value link 'id)
	    (first
	     (first
	      (clsql:query "SELECT MAX(ID) FROM LINK" :database db)))))))

(defun del-link (link)
  (clsql:delete-instance-records link))

(defun get-link-by-id (id)
  (with-db (db)
    (first
     (clsql:select 'link :flatp t :database db
		   :where [= [id] id]))))

(defun get-links ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db)))

(defun get-links-most-recent ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([timestamp] :desc)))))

(defun get-links-highest-rank ()
  (with-db (db)
    (clsql:select 'link :flatp t :database db
		  :order-by '(([score] :desc)))))

(clsql:def-view-class user ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (username
    :accessor username
    :initarg :username
    :type string)
   (password
    :accessor password
    :initarg :password
    :type string)))

(defun create-user (username password)
  (make-instance 'user :username username :password password))

(defun add-user (user)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance user :database db)
      (when (null (id user))
	(setf (slot-value user 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM USER" :flatp t :database db)))))))

(defun get-user-by-username (username)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [= [username] username]))))

(defun get-user-by-username-and-password (username password)
  (with-db (db)
    (first
     (clsql:select 'user :flatp t :database db
		   :where [ and [ = [ username ] username ]
		   [ = [ password ] password ] ]))))

(clsql:def-view-class rating ()
  ((id
    :accessor id
    :initform nil
    :type integer
    :db-kind :key
    :db-constraints (:not-null :auto-increment))
   (raterid
    :accessor raterid
    :initarg :raterid
    :type integer)
   (linkid
    :accessor linkid
    :initarg :linkid
    :type integer)
   (size
    :accessor size
    :initform 1
    :initarg :size
    :type integer)
   (timestamp
    :accessor timestamp
    :initform (get-universal-time)
    :type integer
    :db-type "bigint")))

(defun create-rating (userid linkid size)
  (make-instance 'rating :raterid userid :linkid linkid :size size))

(defun add-rating (rating)
  (with-db (db)
    (clsql:with-transaction (:database db)
      (clsql:update-records-from-instance rating :database db)
      (when (null (id rating))
	(setf (slot-value rating 'id)
	      (first
	       (clsql:query "SELECT MAX(ID) FROM RATING" :flatp t :database db)))))))

(defun get-rating-by-userid-and-linkid (userid linkid)
  (with-db (db)
    (first
     (clsql:select 'rating :flatp t :database db
		   :where [ and [= [ raterid ] userid ]
		   [= [ linkid ] linkid ] ]))))

(defpage (reddit-home "/")
    (cl-who:with-html-output-to-string (str)
      (:html
       (:head (:title "Reddit clone using common lisp"))
       (:body 
	(:h1 "A Reddit implementation clone using common lisp")
	(:h2 "SBCL and Hunchentoot")
	(:a :href (url 'add-form) "Add a link")
	" | "
	(if (hunchentoot:session-value 'user)
	    (cl-who:htm
	     (:a :href (url 'logout) "Logout"))
	    (cl-who:htm 
	     (:a :href (url 'new-user-form) "Log in or create a new user")))
	(:h3 "Highest Ranking Links")
	(:ol :id "highest"
	     (display-links str (get-links-highest-rank)))
	(:h3 "Most recent links")
	(:ol :id "recent"
	     (display-links str (get-links-most-recent)))))))
  
(defun display-links (str list)
  (cl-who:with-html-output (str)
    (dolist (l list)
      (cl-who:htm (:li (display-link-box str l))))))

(defun display-link-box (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:div :id (format nil "~Abox" (id l))
	   (display-link str l)
	   (display-link-age str l)
	   (display-link-score str l)
	   (display-link-score-control str l)
	   (:a :href (format nil "/remove?id=~A" (id l)) "Remove")))))

(defun display-link (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:a :href (url l)
	 (cl-who:str (title l))))))

(defun display-link-age (str l)
  (cl-who:with-html-output (str)
    (:span :id (format nil "~Aagebox" (id l))
	   (cl-who:fmt " Created ~A ago."
			     (s-utils:format-duration 
			      (max 1 (age l)))))))

(defun display-link-score (str l)
  (cl-who:with-html-output (str)
    (:span :id (format nil "~Ascorebox" (id l))
	   (cl-who:fmt " ~d points " (score l)))))

(defun display-link-score-control (str l)
  (cl-who:with-html-output (str)
    (cl-who:htm
     (:span :id (format nil "~Ascorecontrol" (id l))
	    (:a :id (format nil "~Aup" (id l)) 
		:href (command 'incpoints "id" (id l))
		"Up")
	    (:a :id (format nil "~Adown" (id l))
		:href (command 'decpoints "id" (id l))
		"Down")))))

(defpage (add-form "/add")
    (add-form))

(defun add-form (&optional message)
  (if (hunchentoot:session-value 'user)
      (cl-who:with-html-output-to-string (str)
	(:html
	 (:head (:title "Reddig in lisp - add a link"))
	 (:body
	  (:h1 "Add a link")
	  (when message (cl-who:htm (:div (cl-who:str message))))
	  (:form :action "/save" :method "post"
		 (:div "title:" (:input :type "text" :name "title"))
		 (:div "url:" (:input :type "text" :name "url"))
		 (:input :type "submit" :value "submit" )))))
      (new-user-form "You must be logged in to add a link")))

(defpage (rem-link "/remove")
    (let* ((id (get-integer-param "id"))
	   (link (get-link-by-id id)))
      (when link
	(del-link link))
      (hunchentoot:redirect "/")))

(defpage (save-link "/save")
    (let ((title (hunchentoot:parameter "title"))
	  (url (hunchentoot:parameter "url"))
	  (user (hunchentoot:session-value 'user)))
      (cond
	((null user)
	 (new-user-form "You must be logged in to add a link"))
	((zero-string title)
	 (add-form "The title is required"))
	((zero-string url)
	 (add-form "The URL is required"))
	(t
	 (add-rating (create-rating (id user)
				    (add-link (create-link title url))
				    1))
	 (hunchentoot:redirect "/")))))

(defun zero-string (str)
  (or (null str)
      (zerop (length str))))

(defpage (incpoints "/incpoints")
    (with-db (db)
      (clsql:with-transaction (:database db)
	(let* ((id (get-integer-param "id"))
	       (link (get-link-by-id id))
	       (user (hunchentoot:session-value 'user)))
	  (cond
	    ((null user)
	     (new-user-form "You must be logged in to rate links"))
	    ((get-rating-by-userid-and-linkid (id user) (id link))
	     (hunchentoot:redirect (url 'reddit-home)))
	    (link
	     (add-rating (create-rating (id user) (id link) 1))
	     (hunchentoot:redirect (url 'reddit-home)))
	    (t
	     (hunchentoot:redirect (url 'reddit-home))))))))

(defpage (decpoints "/decpoints")
    (with-db (db)
      (clsql:with-transaction (:database db)
	(let* ((id (get-integer-param "id"))
	       (link (get-link-by-id id))
	       (user (hunchentoot:session-value 'user)))
	  (cond
	    ((null user)
	     (new-user-form "You must be logged in to rate links"))
	    ((get-rating-by-userid-and-linkid (id user) (id link))
	     (hunchentoot:redirect (url 'reddit-home)))
	    (link
	      (add-rating (create-rating (id user) (id link) -1))
	      (hunchentoot:redirect (url 'reddit-home)))
	     (t
	      (hunchentoot:redirect (url 'reddit-home))))))))

(defun get-integer-param (param)
  (s-utils:parse-integer-safely 
   (hunchentoot:parameter param)))

(defpage (new-user-form "/new-user")
    (new-user-form))

(defun new-user-form (&optional message)
  (cl-who:with-html-output-to-string (str)
    (cl-who:htm
     (:html
      (:head (:title "New Reddit user"))
      (:body
       (when message (cl-who:htm (:div (cl-who:str message))))
       (:h1 "Login")
       (:form :action (url 'login) :method "post"
	      (:div "username: " (:input :type "text" :name "username"))
	      (:div "password: " (:input :type "password" :name "password"))
	      (:input :type "submit" :value "login"))
       (:h1 "Add a new user")
       (:form :action (url 'add-user) :method "post"
	      (:div "username: " (:input :type "text" :name "username"))
	      (:div "password: " (:input :type "password" :name "password"))
	      (:div "repeat: " (:input :type "password" :name "password2"))
	      (:input :type "submit" :value "sign up")))))))

(defpage (add-user "/add-user")
    (let ((username (hunchentoot:parameter "username"))
	  (password (hunchentoot:parameter "password"))
	  (password2 (hunchentoot:parameter "password2")))
      (cond 
	((zero-string username)
	 (new-user-form "You must provide a username"))
	((get-user-by-username username)
	 (new-user-form "That username already exists, please choose another one"))
	((zero-string password)
	 (new-user-form "You much provide a password"))
	((not (equal password password2))
	 (new-user-form "The two passwords do not match, please type them again"))
	(t
	 (add-user (create-user username password))
	 (hunchentoot:redirect (url 'reddit-home))))))

(defpage (login "/login")
    (let* ((username (hunchentoot:parameter "username"))
	   (password (hunchentoot:parameter "password"))
	   (user (get-user-by-username-and-password username password)))
      (cond
	((not (and username password))
	 (new-user-form "Both the username and password are required"))
	((null user)
	 (new-user-form "Username and password do not match someone on this site..."))
	(t
	 (setf (hunchentoot:session-value 'user)
	       user)
	 (hunchentoot:redirect (url 'reddit-home))))))

(defpage (logout "/logout")
    (setf (hunchentoot:session-value 'user) nil)
  (hunchentoot:redirect (url 'reddit-home)))
  

(clsql:locally-disable-sql-reader-syntax)