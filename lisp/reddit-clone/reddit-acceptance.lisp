(in-package :cl-user)

(require 'lisp-unit)
(require 'cl-ppcre)
(require 'puri)
(require 's-utils)
(require 'selenium)

(defvar sel-server "localhost:4444")
(defvar lisp-server "liszt:8989")

;(defmacro with-sel (&body body)
;  `(sele

(setq selenium:*selenium-driver-url* (puri:parse-uri (format nil "http://~A/selenium-server/driver" sel-server)))

(lisp-unit:define-test reddit-story-1 
  (selenium:with-selenium-session (selenium:*selenium-session* "*firefox3 /usr/bin/epiphany" (format nil "http://~A" lisp-server))
    (selenium:do-open "/")
    (lisp-unit:assert-true (selenium:do-is-text-present "common lisp"))
    (lisp-unit:assert-true (selenium:do-is-text-present "SBCL"))
    (lisp-unit:assert-true (selenium:do-is-text-present "Hunchentoot"))
    (lisp-unit:assert-true (selenium:do-is-text-present "Reddit"))
    (lisp-unit:assert-true (selenium:do-is-element-present "//a[. = 'Add a link']"))
    (dolist (l *links*)
      (let ((text (selenium:do-get-text (format nil "//div[@id = '~Abox']" (id l)))))
	(lisp-unit:assert-true (cl-ppcre:scan "Created .+ ago" text))
	(lisp-unit:assert-true (cl-ppcre:scan "\\d+ points" text))
	(lisp-unit:assert-true (search (title l) text))))))

(lisp-unit:define-test reddit-story-2
  (let ((title (format nil "Title~A" (get-universal-time)))
	(url (format nil "http://www.wjlr.org.uk/~A" (get-universal-time))))
    (selenium:with-selenium-session (selenium:*selenium-session* "*firefox" "http://liszt:8989")
      (selenium:do-open "/")
      (lisp-unit:assert-true (selenium:do-is-element-present "//a[. = 'Add a link']"))
      (selenium:do-click "//a[. = 'Add a link']")
      (selenium:do-wait-for-page-to-load 5000)
      (lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
      (lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'title']"))
      (lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'url']"))
      (selenium:do-type "//input[@name = 'title']" title)
      (selenium:do-type "//input[@name = 'url']" url)
      (selenium:do-submit "//form[@action = '/save']")
      (selenium:do-wait-for-page-to-load 5000)
      ;(selenium:do-open "/")
      (lisp-unit:assert-equal title (selenium:do-get-text (format nil "//a[@href = '~A']" url)))
      (lisp-unit:assert-true 
       (cl-ppcre:scan "0 points" 
		      (selenium:do-get-text 
			  (format nil "//div[a/@href='~A']/span[contains(@id, 'scorebox')]" url))))
      (setf *links* (delete title *links* :key #'title :test #'equal)))))

;; Type neither URL not title, should redirect to add page
(lisp-unit:define-test reddit-story-3
    (selenium:with-selenium-session (selenium:*selenium-session* "*firefox" "http://liszt:8989")
      (selenium:do-open "/")
      (lisp-unit:assert-true (selenium:do-is-element-present "//a[. = 'Add a link']"))
      (selenium:do-click "//a[. = 'Add a link']")
      (selenium:do-wait-for-page-to-load 5000)
      (lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
      (selenium:do-submit "//form[@action = '/save']")
      (selenium:do-wait-for-page-to-load 5000)
      (lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
      (lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'title']"))
      (lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'url']"))))

;; Type URL but not title, should redirect to add page
(lisp-unit:define-test reddit-story-4
    (let ((url (format nil "http://www.wjlr.org.uk/~A" (get-universal-time))))
      (selenium:with-selenium-session (selenium:*selenium-session* "*firefox" "http://liszt:8989")
	(selenium:do-open "/")
	(lisp-unit:assert-true (selenium:do-is-element-present "//a[. = 'Add a link']"))
	(selenium:do-click "//a[. = 'Add a link']")
	(selenium:do-wait-for-page-to-load 5000)
	(lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'title']"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'url']"))
	(selenium:do-type "//input[@name = 'url']" url)
	(selenium:do-submit "//form[@action = '/save']")
	(selenium:do-wait-for-page-to-load 5000)
	(lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'title']"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'url']")))))

;; Type Title but not URL, should redirect to add page
(lisp-unit:define-test reddit-story-5
    (let ((title (format nil "Title~A" (get-universal-time))))
      (selenium:with-selenium-session (selenium:*selenium-session* "*firefox" "http://liszt:8989")
	(selenium:do-open "/")
	(lisp-unit:assert-true (selenium:do-is-element-present "//a[. = 'Add a link']"))
	(selenium:do-click "//a[. = 'Add a link']")
	(selenium:do-wait-for-page-to-load 5000)
	(lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'title']"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'url']"))
	(selenium:do-type "//input[@name = 'title']" title)
	(selenium:do-submit "//form[@action = '/save']")
	(selenium:do-wait-for-page-to-load 5000)
	(lisp-unit:assert-true (selenium:do-is-text-present "Add a link"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'title']"))
	(lisp-unit:assert-true (selenium:do-is-element-present "//input[@name = 'url']")))))

;; 
(lisp-unit:define-test reddit-story-6
    (let ((linkid 0)
	  (score 0)
	  (newscore 0))
      (when (null *links*)
	(push (make-instance 'link :title "Google" :url "http://www.google.com") *links*))
      (selenium:with-selenium-session (selenium:*selenium-session* "*firefox" "http://liszt:8989")
	(selenium:do-open "/")
	(multiple-value-bind (match values)
	    (cl-ppcre:scan-to-strings 
	     "(\\d+)scorebox"
	     (selenium:do-get-attribute "//span[contains(@id, 'scorebox')]/@id"))
	  (setf linkid (elt values 0)))
	(multiple-value-bind (match values)
	    (cl-ppcre:scan-to-strings 
	     "(\\d+) points"
	     (selenium:do-get-text (format nil "//span[@id = '~ascorebox']" linkid)))
	  (setf score (s-utils:parse-integer-safely (elt values 0))))
	(lisp-unit:assert-true (selenium:do-is-element-present
				   (format nil "//a[@id = '~Aup' and . = 'Up']" linkid)))
	(selenium:do-click (format nil "//a[@id = '~Aup' and . = 'Up']" linkid))
	(selenium:do-wait-for-page-to-load 5000)
	(lisp-unit:assert-true (selenium:do-is-element-present 
				   (format nil "//span[@id = '~Ascorebox']" linkid)))
	(multiple-value-bind (match values)
	    (cl-ppcre:scan-to-strings "(\\d+) points" (selenium:do-get-text (format nil "//span[@id = '~Ascorebox']" linkid)))
	  (setf newscore (s-utils:parse-integer-safely (elt values 0))))
	(lisp-unit:assert-equal newscore (+ score 1)))))

(lisp-unit:define-test reddit-story-7
    (let ((linkid 0)
	  (score 0)
	  (newscore 0))
      (when (null *links*)
	(push (make-instance 'link :title "Google" :url "http://www.google.com") *links*))
      (selenium:with-selenium-session (selenium:*selenium-session* "*firefox" "http://liszt:8989")
	(selenium:do-open "/")
	(multiple-value-bind (match values)
	    (cl-ppcre:scan-to-strings 
	     "(\\d+)scorebox"
	     (selenium:do-get-attribute "//span[contains(@id, 'scorebox')]/@id"))
	  (setf linkid (elt values 0)))
	(multiple-value-bind (match values)
	    (cl-ppcre:scan-to-strings 
	     "(\\d+) points"
	     (selenium:do-get-text (format nil "//span[@id = '~ascorebox']" linkid)))
	  (setf score (s-utils:parse-integer-safely (elt values 0))))
	(lisp-unit:assert-true (selenium:do-is-element-present
				   (format nil "//a[@id = '~Adown' and . = 'Down']" linkid)))
	(selenium:do-click (format nil "//a[@id = '~Adown' and . = 'Down']" linkid))
	(selenium:do-wait-for-page-to-load 5000)
	(lisp-unit:assert-true (selenium:do-is-element-present 
				   (format nil "//span[@id = '~Ascorebox']" linkid)))
	(multiple-value-bind (match values)
	    (cl-ppcre:scan-to-strings "(\\d+) points" (selenium:do-get-text (format nil "//span[@id = '~Ascorebox']" linkid)))
	  (setf newscore (s-utils:parse-integer-safely (elt values 0))))
	(lisp-unit:assert-equal newscore (- score 1)))))
      

(lisp-unit:run-tests 
 reddit-story-1 
; reddit-story-2 
; reddit-story-3 
; reddit-story-4
; reddit-story-5
; reddit-story-6
; reddit-story-7
)