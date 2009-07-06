;*=====================================================================*/
;*    .../project/bigloo/api/fthread/examples/srfi18/srfi18.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Mon Jun  6 05:55:32 2005 (serrano)                */
;*    Copyright   :  2002-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of SRFI18.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module srfi18
   (library fthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err . msg)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (for-each write msg)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    call-with-current-continuation :: ...                            */
;*---------------------------------------------------------------------*/
(define-macro (call-with-current-continuation proc)
   `(bind-exit (exit)
      (,proc exit)))

;*---------------------------------------------------------------------*/
;*    do-something-else ...                                            */
;*---------------------------------------------------------------------*/
(define (do-something-else)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (default-scheduler (make-scheduler))
   (let ((provided (prgm)))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (print "ok.")
	  (begin
	     (print "error.")
	     (print "   ==> provided: [" provided
		    "]\n       expected: ["
		    (if (procedure? res) (res 'result) res)
		    "]")))))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))

;*---------------------------------------------------------------------*/
;*    cond-expand ...                                                  */
;*---------------------------------------------------------------------*/
(define-test cond-expand
   (cond-expand (srfi-18 #t)
		(else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    make-thread ...                                                  */
;*---------------------------------------------------------------------*/
(define-test make-thread
   (make-thread (lambda () (write 'hello)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "a thread"
		  (thread? v))))

;*---------------------------------------------------------------------*/
;*    thread-name ...                                                  */
;*---------------------------------------------------------------------*/
(define-test thread-name
   (thread-name (make-thread (lambda () #f) 'foo))
   :result 'foo)

;*---------------------------------------------------------------------*/
;*    thread-specific ...                                              */
;*---------------------------------------------------------------------*/
(define-test thread-specific
   (let ((t (make-thread (lambda () #f))))
      (thread-specific-set! t "hello")
      (thread-specific t))
   :result "hello")
   
;*---------------------------------------------------------------------*/
;*    thread-start! ...                                                */
;*---------------------------------------------------------------------*/
(define-test thread-start
   (with-output-to-string
      (lambda ()
	 (letrec ((t0 (thread-start!
		       (make-thread (lambda ()
				       (write 'b)
				       (thread-join! t1)))))
		  (t1 (thread-start!
		       (make-thread (lambda ()
				       (write 'a))))))
	    (scheduler-start!))))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "ab or ba"
		  (or (equal? v "ab") (equal? v "ba")))))

;*---------------------------------------------------------------------*/
;*    thread-yield! ...                                                */
;*---------------------------------------------------------------------*/
(define-test thread-yield
   (with-output-to-string
      (lambda ()
	 (let ((m (make-mutex)))
	    (thread-start!
	     (make-thread (lambda ()
			     (let loop ()
				(if (mutex-lock! m 0)
				    (begin
				       (display "locked")
				       (mutex-unlock! m))
				    (begin
				       (thread-yield!)
				       (loop))))))))
	 (scheduler-start! 1)))
   :result "locked")

;*---------------------------------------------------------------------*/
;*    thread-sleep! ...                                                */
;*---------------------------------------------------------------------*/
(define-test thread-sleep
   (with-output-to-string
      (lambda ()
	 (thread-start!
	  (make-thread (lambda ()
			  (let loop ((x 1))
			     (thread-sleep! 1)
			     (display 't1:)
			     (write x)
			     (display* "@" (current-time) " ")
			     (loop (+ x 1))))))
	 (thread-start!
	  (make-thread (lambda ()
			  (let ((start (time->seconds (current-time))))
			     (let loop ((x 1))
				(thread-sleep! (seconds->time (+ x start)))
				(display 't2:)
				(write x)
				(display* "@" (current-time) " ")
				(loop (+ x 1)))))))
	 (let ((old *thread-strict-order*))
	    (set! *thread-strict-order* #t)
	    (scheduler-start! 10)
	    (set! *thread-strict-order* old))))
   :result "t1:1@2 t1:2@3 t2:1@3 t1:3@4 t1:4@5 t1:5@6 t2:2@6 t1:6@7 t1:7@8 t1:8@9 t1:9@10 t2:3@10 ")

;*---------------------------------------------------------------------*/
;*    thread-terminate! ...                                            */
;*---------------------------------------------------------------------*/
(define-test thread-terminate
   (begin
      (define (amb thunk1 thunk2)
	 (let ((result #f)
	       (result-mutex (make-mutex))
	       (done-mutex (make-mutex)))
	    (letrec ((child1
		      (make-thread
		       (lambda ()
			  (let ((x (thunk1)))
			     (mutex-lock! result-mutex #f #f)
			     (set! result x)
			     (thread-terminate! child2)
			     (mutex-unlock! done-mutex)))
		       'child1))
		     (child2
		      (make-thread
		       (lambda ()
			  (let ((x (thunk2)))
			     (mutex-lock! result-mutex #f #f)
			     (set! result x)
			     (thread-terminate! child1)
			     (mutex-unlock! done-mutex)))
		       'child2)))
	       (mutex-lock! done-mutex #f #f)
	       (thread-start! child1)
	       (thread-start! child2)
	       (scheduler-start! 5)
	       (mutex-lock! done-mutex #f #f)
	       result)))
      (amb (lambda () (thread-yield!) 1) (lambda () 2)))
   :result 2)

;*---------------------------------------------------------------------*/
;*    thread-terminate2! ...                                           */
;*---------------------------------------------------------------------*/
(define-test thread-terminate2
   (let ((res 0))
      (define (spawn thunk)
	 (let ((t (make-thread thunk)))
	    (thread-specific-set! t #t)
	    (thread-start! t)
	    t))
      (define (stop! thread)
	 (thread-specific-set! thread #f)
	 (thread-join! thread))
      (define (keep-going?)
	 (thread-specific (current-thread)))
      (define count!
	 (let ((m (make-mutex))
	       (i 0))
	    (lambda ()
	       (mutex-lock! m)
	       (let ((x (+ i 1)))
		  (set! i x)
		  (mutex-unlock! m)
		  x))))
      (define (increment-forever!)
	 (let loop () (count!) (if (keep-going?) (loop))))
      (thread-start! (make-thread
		      (lambda ()
			 (let ((t1 (spawn increment-forever!))
			       (t2 (spawn increment-forever!)))
			    (thread-sleep! 1)
			    (stop! t1)
			    (stop! t2)
			    (set! res (count!))))))
      (scheduler-start!)
      res)
   :result 4)

;*---------------------------------------------------------------------*/
;*    thread-join ...                                                  */
;*---------------------------------------------------------------------*/
(define-test thread-join
   (let* ((res 0)
	  (t (thread-start! (make-thread
			     (lambda () (expt 2 10))
			     'thread-join1.1)))
	  (t2 (thread-start! (make-thread
			      (lambda ()
				 (do-something-else)
				 (set! res (thread-join! t)))
			      'thread-join1.2))))
      (scheduler-start!)
      res)
   :result 1024)

;*---------------------------------------------------------------------*/
;*    thread-join2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join2
   (let ((t (thread-start! (make-thread (lambda ()
					   (raise 123))
					'thread-join2.1)))
	 (res 0))
      (do-something-else)
      (thread-start! (make-thread
		      (lambda ()
			 (with-exception-handler
			  (lambda (exc)
			     (if (uncaught-exception? exc)
				 (* 10 (uncaught-exception-reason exc))
				 99999))
			  (lambda ()
			     (set! res (+ 1 (thread-join! t))))))
		      'thread-join2.2))
      (scheduler-start!)
      res)
   :result 1231)

;*---------------------------------------------------------------------*/
;*    thread-join3 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join3
   (let ((res #unspecified))
      (define thread-alive?
	 (let ((unique (list 'unique)))
	    (lambda (thread)
	       (eq? (thread-join! thread 1 unique) unique))))
      (let* ((t1 (thread-start! (make-thread (lambda ()
						'done)
					     'thread-join3.1)))
	     (t2 (thread-start! (make-thread (lambda ()
						(thread-sleep! 10))
					     'thread-join3.2)))
	     (t3 (thread-start! (make-thread (lambda ()
						(let loop ()
						   (thread-yield!)
						   (loop)))
					     'thread-join3.3)))
	     (t4 (thread-start! (make-thread (lambda ()
						(let loop ()
						   (thread-yield!)
						   (loop)))
					     'thread-join3.4)))
	     (m (thread-start! (make-thread (lambda ()
					       (thread-terminate! t4)
					       (thread-yield!)
					       (set! res
						     (list
						      (thread-alive? t1)
						      (thread-alive? t2)
						      (thread-alive? t3)
						      (thread-alive? t4))))
					    'thread-join3.5))))
	 (scheduler-start! 20)
	 res))
   :result '(#f #t #t #f))

;*---------------------------------------------------------------------*/
;*    thread-join4 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join4
   (let ((res '()))
      (define (wait-for-termination! thread)
	 (let ((eh (current-exception-handler)))
	    (with-exception-handler
	     (lambda (exc)
		(if (not (or (terminated-thread-exception? exc)
			     (uncaught-exception? exc)))
		    (eh exc)))
	     (lambda ()
		(thread-join! thread)
		#f))))
      (let* ((t1 (thread-start! (make-thread (lambda ()
						(thread-sleep! 5))
					     'thread-join4.1)))
	     (t2 (thread-start! (make-thread (lambda ()
						(thread-sleep! 10))
					     'thread-join4.2)))
	     (t3 (thread-start! (make-thread (lambda ()
						(thread-sleep! 3)
						(thread-terminate! t2))
					     'thread-join4.3)))
	     (t4 (thread-start! (make-thread (lambda ()
						(thread-sleep! 3)
						(raise #t))
					     'thread-join4.4)))
	     (t5 (thread-start! (make-thread
				 (lambda ()
				    (set! res (cons (wait-for-termination! t1)
						    res))
				    (set! res (cons (wait-for-termination! t3)
						    res))
				    (set! res (cons (wait-for-termination! t4)
						    res)))
				 'thread-join4.5))))
	 (scheduler-start!)
	 res))
   :result '(#f #f #f))

;*---------------------------------------------------------------------*/
;*    mutex ...                                                        */
;*---------------------------------------------------------------------*/
(define-test mutex
   (list (mutex? (make-mutex))
	 (mutex? 'foo)
	 (mutex-name (make-mutex 'foo)))
   :result '(#t #f foo))

;*---------------------------------------------------------------------*/
;*    mutex-specific ...                                               */
;*---------------------------------------------------------------------*/
(define-test mutex-specific
   (let ((m (make-mutex)))
      (mutex-specific-set! m "hello")
      (mutex-specific m))
   :result "hello")

;*---------------------------------------------------------------------*/
;*    mutex-specific2 ...                                              */
;*---------------------------------------------------------------------*/
(define-test mutex-specific2
   (let ((res '()))
      (define (mutex-lock-recursively! mutex)
	 (if (eq? (mutex-state mutex) (current-thread))
	     (let ((n (mutex-specific mutex)))
		(mutex-specific-set! mutex (+ n 1)))
	     (begin
		(mutex-lock! mutex)
		(mutex-specific-set! mutex 0))))
      (define (mutex-unlock-recursively! mutex)
	 (let ((n (mutex-specific mutex)))
	    (if (= n 0)
		(mutex-unlock! mutex)
		(mutex-specific-set! mutex (- n 1)))))
      (let ((t (thread-start!
		(make-thread
		 (lambda ()
		    (let ((m (make-mutex)))
		       (mutex-lock-recursively! m)
		       (mutex-lock-recursively! m)
		       (mutex-lock-recursively! m)
		       (set! res (cons (mutex-specific m) res))
		       (mutex-unlock-recursively! m)
		       (mutex-unlock-recursively! m)
		       (mutex-unlock-recursively! m)
		       (set! res (cons (mutex-specific m) res))))))))
	 (scheduler-start!)
	 res))
   :result '(0 2))

;*---------------------------------------------------------------------*/
;*    mutex-state ...                                                  */
;*---------------------------------------------------------------------*/
(define-test mutex-state
   (mutex-state (make-mutex))
   :result 'not-abandoned)

;*---------------------------------------------------------------------*/
;*    mutex-state2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test mutex-state2
   (let ((res '()))
      (define (thread-alive? thread)
	 (let ((mutex (make-mutex)))
	    (mutex-lock! mutex #f thread)
	    (let ((state (mutex-state mutex)))
	       (mutex-unlock! mutex)
	       (eq? state thread))))
      (letrec ((t1 (thread-start!
		    (make-thread (lambda ()
				    (thread-sleep! 10))
				 'thread-mutex-state2.1)))
	       (t2 (thread-start!
		    (make-thread (lambda ()
				    'done)
				 'thread-mutex-state2.2)))
	       (t3 (thread-start!
		    (make-thread (lambda ()
				    (thread-sleep! 10))
				 'thread-mutex-state2.3)))
	       (t4 (thread-start!
		    (make-thread (lambda ()
				    (let loop ()
				       (thread-yield!)
				       (loop)))
				 'thread-mutex-state2.4)))
	       (t5 (thread-start!
		    (make-thread (lambda ()
				    (thread-terminate! t3)
				    (set! res
					  (list (thread-alive? t1)
						(thread-alive? t2)
						(thread-alive? t3)
						(thread-alive? t4)
						(thread-alive? t5))))
				 'thread-mutex-state2.5))))
	 (scheduler-start! 20)
	 res))
   :result '(#t #f #f #t #t))

;*---------------------------------------------------------------------*/
;*    mailbox ...                                                      */
;*---------------------------------------------------------------------*/
(define-test mailbox
   (let ((res '()))
      (define (make-empty-mailbox)
	 (let ((put-mutex (make-mutex))
	       (get-mutex (make-mutex))
	       (cell #f))
	    (define (put! obj)
	       (mutex-lock! put-mutex #f #f)
	       (set! cell obj)
	       (mutex-unlock! get-mutex))
	    (define (get!)
	       (mutex-lock! get-mutex #f #f)
	       (let ((result cell))
		  (set! cell #f)
		  (mutex-unlock! put-mutex)
		  result))
	    (mutex-lock! get-mutex #f #f)
	    (lambda (msg)
	       (case msg
		  ((put!) put!)
		  ((get!) get!)
		  (else (error "mailbox" "unknown message" msg))))))
      (define (mailbox-put! m obj) ((m 'put!) obj))
      (define (mailbox-get m) ((m 'get!)))
      (let* ((mbox (make-empty-mailbox))
	     (t1 (thread-start! (make-thread
				 (lambda ()
				    (thread-yield!)
				    (mailbox-put! mbox 'foo)
				    (thread-yield!)
				    (set! res (cons (mailbox-get mbox)
						    res))))))
	     (t2 (thread-start! (make-thread
				 (lambda ()
				    (set! res (cons (mailbox-get mbox) res))
				    (thread-yield!)
				    (mailbox-put! mbox 'bar))))))
	 (scheduler-start! 5)
	 res))
   :result '(bar foo))

;*---------------------------------------------------------------------*/
;*    sleep ...                                                        */
;*---------------------------------------------------------------------*/
(define-test sleep
   (begin
      (define (sleep! timeout)
	 (let ((m (make-mutex)))
	    (mutex-lock! m #f #f)
	    (mutex-lock! m timeout #f)))
      (let ((t (thread-start! (make-thread (lambda () (sleep! 10))))))
	 (scheduler-start!)
	 (current-time)))
   :result 11)

;*---------------------------------------------------------------------*/
;*    lock-one-of ...                                                  */
;*---------------------------------------------------------------------*/
(define-test lock-one-of
   (begin
      (define (lock-one-of! mutex1 mutex2)
	 (let ((ct (current-thread))
	       (done-mutex (make-mutex)))
	    (mutex-lock! done-mutex #f #f)
	    (let ((t1 (thread-start!
		       (make-thread
			(lambda ()
			   (mutex-lock! mutex1 #f ct)
			   (mutex-unlock! done-mutex)))))
		  (t2 (thread-start!
		       (make-thread
			(lambda ()
			   (mutex-lock! mutex2 #f ct)
			   (mutex-unlock! done-mutex))))))
	       (mutex-lock! done-mutex #f #f)
	       (thread-terminate! t1)
	       (thread-terminate! t2)
	       (if (eq? (mutex-state mutex1) ct)
		   (begin
		      (if (eq? (mutex-state mutex2) ct)
			  (mutex-unlock! mutex2))
		      mutex1)
		   mutex2))))
      (let* ((m1 (make-mutex))
	     (m2 (make-mutex))
	     (t1 (thread-start!
		  (make-thread
		   (lambda ()
		      (mutex-lock! m1)
		      (mutex-lock! m2)
		      (thread-sleep! 5)
		      (mutex-unlock! m2)))))
	     (t2 (thread-start!
		  (make-thread
		   (lambda ()
		      (lock-one-of! m1 m2)))))
	     (t3 (thread-start!
		  (make-thread
		   (lambda ()
		      (mutex-lock! m1)
		      (mutex-lock! m2)
		      (thread-sleep! 5)
		      (mutex-unlock! m1)))))
	     (t4 (thread-start!
		  (make-thread
		   (lambda ()
		      (lock-one-of! m1 m2))))))
	 (scheduler-start!)
	 (let* ((scdl (make-scheduler))
		(m1 (make-mutex))
		(m2 (make-mutex))
		(t1 (thread-start!
		     (make-thread
		      (lambda ()
			 (mutex-lock! m1)
			 (mutex-lock! m2)))
		     scdl))
		(t2 (thread-start!
		     (make-thread
		      (lambda ()
			 (lock-one-of! m1 m2)))
		     scdl)))
	    (scheduler-start! 5 scdl)
	    (current-time scdl))))
   :result 5)

;*---------------------------------------------------------------------*/
;*    mutex-unlock! ...                                                */
;*---------------------------------------------------------------------*/
(define-test mutex-unlock!
   (let ((m (make-mutex))
	 (cv (make-condition-variable)))
      (thread-start!
       (make-thread
	(lambda ()
	   (let loop ((n 0))
	      (mutex-lock! m)
	      (if (> n 10)
		  (mutex-unlock! m)
		  (begin
		     (mutex-unlock! m cv)
		     (loop (+ 1 n)))))))))
   :result #unspecified)

;*---------------------------------------------------------------------*/
;*    condition-variable ...                                           */
;*---------------------------------------------------------------------*/
(define-test condition-variable
   (list (condition-variable? (make-condition-variable))
	 (condition-variable? 'foo)
	 (condition-variable-name (make-condition-variable 'foo))
	 (let ((cv (make-condition-variable)))
	    (condition-variable-specific-set! cv 'bar)
	    (condition-variable-specific cv)))
   :result `(#t #f foo bar))

;*---------------------------------------------------------------------*/
;*    condition-variable2 ...                                          */
;*---------------------------------------------------------------------*/
(define-test condition-variable2
   (let ((res '()))
      (define (make-empty-mailbox)
	 (let ((mutex (make-mutex))
	       (put-condvar (make-condition-variable))
	       (get-condvar (make-condition-variable))
	       (full? #f)
	       (cell #f))
	    (define (put! obj)
	       (mutex-lock! mutex)
	       (if full?
		   (begin
		      (mutex-unlock! mutex put-condvar)
		      (put! obj))
		   (begin
		      (set! cell obj)
		      (set! full? #t)
		      (condition-variable-signal! get-condvar)
		      (mutex-unlock! mutex))))
	    (define (get!)
	       (mutex-lock! mutex)
	       (if (not full?)
		   (begin
		      (mutex-unlock! mutex get-condvar)
		      (get!))
		   (let ((result cell))
		      (set! cell #f)
		      (set! full? #f)
		      (condition-variable-signal! put-condvar)
		      (mutex-unlock! mutex)
		      result)))
	    (lambda (msg)
	       (case msg
		  ((put!) put!)
		  ((get!) get!)
		  (else (error "mailbox" "unknown message" msg))))))
      (define (mailbox-put! m obj) ((m 'put!) obj))
      (define (mailbox-get m) ((m 'get!)))
      (let* ((mbox (make-empty-mailbox))
	     (t1 (thread-start! (make-thread
				 (lambda ()
				    (thread-yield!)
				    (mailbox-put! mbox 'foo)
				    (thread-yield!)
				    (set! res (cons (mailbox-get mbox)
						    res))))))
	     (t2 (thread-start! (make-thread
				 (lambda ()
				    (set! res (cons (mailbox-get mbox) res))
				    (thread-yield!)
				    (mailbox-put! mbox 'bar))))))
	 (scheduler-start! 5)
	 res))
      :result '(bar foo))

;*---------------------------------------------------------------------*/
;*    semaphore ...                                                    */
;*---------------------------------------------------------------------*/
(define-test semaphore
   (let ((res 0))
      (define (make-semaphore n)
	 (vector n (make-mutex) (make-condition-variable)))
      (define (semaphore-wait! sema)
	 (mutex-lock! (vector-ref sema 1))
	 (let ((n (vector-ref sema 0)))
	    (if (> n 0)
		(begin
		   (vector-set! sema 0 (- n 1))
		   (mutex-unlock! (vector-ref sema 1)))
		(begin
		   (mutex-unlock! (vector-ref sema 1) (vector-ref sema 2))
		   (semaphore-wait! sema)))))
      (define (semaphore-signal-by! sema increment)
	 (mutex-lock! (vector-ref sema 1))
	 (let ((n (+ (vector-ref sema 0) increment)))
	    (vector-set! sema 0 n)
	    (if (> n 0)
		(condition-variable-broadcast! (vector-ref sema 2)))
	    (mutex-unlock! (vector-ref sema 1))))
      (let ((sema (make-semaphore 10)))
	 (let ((t1 (thread-start! (make-thread
				   (lambda ()
				      (semaphore-wait! sema)
				      (set! res (current-time))))))
	       (t2 (thread-start! (make-thread
				   (lambda ()
				      (let loop ((n 10))
					 (if (> n 0)
					     (begin
						(semaphore-signal-by! sema 1)
						(thread-yield!)
						(loop (- n 1))))))))))
	    (scheduler-start!)
	    res)))
   :result 2)

;*---------------------------------------------------------------------*/
;*    current-exception-handler ...                                    */
;*---------------------------------------------------------------------*/
(define-test current-exception-handler
   (current-exception-handler)
   :result procedure?)

;*---------------------------------------------------------------------*/
;*    with-exception-handler ...                                       */
;*---------------------------------------------------------------------*/
(define-test with-exception-handler
   (with-exception-handler list current-exception-handler)
   :result list)

;*---------------------------------------------------------------------*/
;*    raise ...                                                        */
;*---------------------------------------------------------------------*/
(define-test raise
   (let* ((res '())
	  (v2 (begin
		 (define (f n)
		    (if (< n 0) (raise "negative arg") (sqrt n)))
		 (define (g)
		    (call-with-current-continuation
		     (lambda (return)
			(with-exception-handler
			 (lambda (exc)
			    (return
			     (if (string? exc)
				 (string-append "error: " exc)
				 "unknown error")))
			 (lambda ()
			    (set! res (cons (f 4.) res))
			    (set! res (cons (f -1.) res))
			    (set! res (cons (write (f 9.)) res)))))))
		 (g))))
      (cons v2 res))
   :result '("error: negative arg" 2.))
	   
;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (("-g" (help "Set debug mode"))
	  (bigloo-debug-set! (+fx 1 (bigloo-debug))))
	 (("-G" (help "Set debug mode (disable color)"))
	  (bigloo-debug-set! (+fx 1 (bigloo-debug)))
	  (bigloo-trace-color-set! #f))
	 (("-p" (help "Preserve thread execution order"))
	  (set! *thread-strict-order* #t))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (for-each (lambda (pvn)
		   (apply test pvn))
		(if (null? tests)
		    (reverse *tests*)
		    (reverse (filter (lambda (t) (memq (car t) tests))
				     *tests*))))
      ;; if we reach that point, we are done
      (print "\n" (if (null? tests)
		      "All"
		      tests)
	     " tests completed...")))


   

