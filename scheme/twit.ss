#lang scheme

(require net/url
         net/base64)

(define (call-api path auth-required?)
  (define url
    (make-url "http"
	      #f
	      "twitter.com"
	      80
	      #t
	      (list (make-path/param "statuses" empty)
		    (make-path/param "public_timeline.xml" empty))
	      '()
	      #f))
  (let ((p
	 (get-pure-port
	  url)))
    (let loop ((line (read-line p)))
      (unless (eof-object? line)
	      (display line)
	      (loop (read-line p))))
    (close-input-port p)))
	      

(define (mentions)
  ; http://twitter.com/statuses/mentions.format
  (call-api "/statuses/mentions" #t))

(define (public-timeline)
  (call-api "/statuses/public_timeline" #f))

(call-api #f #f)
