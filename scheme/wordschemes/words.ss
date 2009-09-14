(module words mzscheme
	(require (lib "class.ss")
		 (lib "mred.ss" "mred"))

	(define frame (new frame% [label "Example"] [style '(metal)]))

	(define letters (new message% [parent frame]
			     [label "No events so far..."]))

	(define timer-length 
	  (new text-field% 
	       [parent frame]
	       [label "Timer length"]
	       (init-value "10")))

	(define letter-pool-length
	  (new combo-field%
	       [parent frame]
	       [label "Number of letters"]
	       [choices '("8")]))

	(define (get-timer-value)
	  (let* ((timer-value (send timer-length get-value))
		 (int-value (string->number timer-value)))
	    (* 1000 int-value)))
	
	(define (get-letter-pool-length)
	  (let* ((pool-length (send letter-pool-length get-value))
		 (int-value (string->number pool-length)))
	    int-value))

	(define atimer (new timer% 
			    [notify-callback 
			     (lambda void 
			       (send letters set-label "Timer expired!")
			       (send button set-label "Start timer"))]))

	(define button (new button% [parent frame]
			    [label "Start timer"]
			    (callback (lambda (button event)
					(send button set-label "Stop timer")
					(send letters set-label "Timer running")
					(send letters-pool set-value (list->string (seed-letters (get-letter-pool-length) '())))
					(send atimer start (get-timer-value))))))

	(define letters-pool
	  (new text-field%
	       [parent frame]
	       [label "Letters"]))

	(define (random-letter)
	  (let ((letter (integer->char (+ 65 (random 26)))))
	    (display letter)
	    letter))

	(define (seed-letters number-of-letters letters)
	  (cond
	   [(< number-of-letters 1)
	    (cond
	     [(list? letters)
	      letters]
	     [else '()])]
	   [else
	    (let ((new-letter (random-letter)))
	      (display new-letter)
	      (seed-letters (- number-of-letters 1)
			    (cons new-letter letters)))]))

	(send frame show #t)
)