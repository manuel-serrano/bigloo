;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/recette/recette.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Thu Oct 15 15:24:45 2015 (serrano)                */
;*    Copyright   :  2002-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of SRFI18.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library pthread srfi18)
   (main    main))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err . msg)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (for-each write msg)
	 (newline))))

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
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let ((provided (with-handler
		      (lambda (e)
			 (error-notify e)
			 (vector res))
		      (prgm))))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (begin
	     (set! *success* (+fx 1 *success*))
	     (print "ok."))
	  (begin
	     (set! *failure* (cons name *failure*))
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
   (cond-expand
      (srfi18 #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    make-thread ...                                                  */
;*---------------------------------------------------------------------*/
(define-test make-thread
   (instantiate::srfi18thread (body (lambda () (write 'hello))))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "a thread"
		  (isa? v thread))))

;*---------------------------------------------------------------------*/
;*    thread-name ...                                                  */
;*---------------------------------------------------------------------*/
(define-test thread-name
   (with-access::thread (instantiate::srfi18thread (body (lambda () #f)) (name 'foo))
	 (name)
      name)
   :result 'foo)

;*---------------------------------------------------------------------*/
;*    thread-specific ...                                              */
;*---------------------------------------------------------------------*/
(define-test thread-specific
   (let ((t (instantiate::srfi18thread (body (lambda () #f)))))
      (with-access::thread t (specific)
	 (set! specific "hello")
	 specific))
   :result "hello")

;*---------------------------------------------------------------------*/
;*    thread-start! ...                                                */
;*---------------------------------------------------------------------*/
(define-test thread-start
   (with-output-to-string
      (lambda ()
	 (let ((m (make-mutex)))
	    (letrec ((t0 (instantiate::srfi18thread
			    (body (lambda ()
				     (mutex-lock! m)
				     (write 'b)
				     (mutex-unlock! m)
				     (thread-join! t1)))))
		     (t1 (instantiate::srfi18thread
			    (body (lambda ()
				     (mutex-lock! m)
				     (write 'a)
				     (mutex-unlock! m))))))
	       (mutex-lock! m)
	       (thread-start-joinable! t0)
	       (thread-start-joinable! t1)
	       (mutex-unlock! m)
	       (thread-join! t0)))))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "ab or ba"
		  (or (equal? v "ab") (equal? v "ba")))))

;*---------------------------------------------------------------------*/
;*    bind-exit ...                                                    */
;*---------------------------------------------------------------------*/
(define-test bind-exit
   (let ((res #f))
      (let ((val (thread-join!
		  (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (set! res
				     (bind-exit (stop)
					(+ 1 (stop #t)))))))))))
	 res))
   :result #t)

;*---------------------------------------------------------------------*/
;*    mutex ...                                                        */
;*---------------------------------------------------------------------*/
(define-test mutex
   (let ((m (make-mutex)))
      (mutex-lock! m)
      (mutex-unlock! m)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    mutex2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test mutex2
   (let ((m (make-mutex)))
      (with-lock m
	 (lambda ()
	    #t)))
   :result #t)

;*---------------------------------------------------------------------*/
;*    mutex3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test mutex3
   (with-lock (make-mutex)
      (lambda ()
	 #t))
   :result #t)

;*---------------------------------------------------------------------*/
;*    mutex4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test mutex3
   (with-lock (make-mutex)
      (let ((x #t))
	 (lambda ()
	    x)))
   :result #t)

;*---------------------------------------------------------------------*/
;*    thread-yield! ...                                                */
;*---------------------------------------------------------------------*/
(define-test thread-yield
   (with-output-to-string
      (lambda ()
	 (let ((m (make-mutex)))
	    (thread-join!
	     (thread-start-joinable!
	      (instantiate::srfi18thread
		 (body (lambda ()
			  (let loop ()
			     (if (mutex-lock! m)
				 (begin
				    (display "locked")
				    (mutex-unlock! m))
				 (begin
				    (thread-yield!)
				    (loop))))))))))))
   :result "locked")

;*---------------------------------------------------------------------*/
;*    thread-join1 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join1
   (thread-join!
    (thread-start-joinable!
     (instantiate::srfi18thread (body (lambda () 23)))))
   :result 23)

;*---------------------------------------------------------------------*/
;*    thread-join2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join2
   (let* ((res 0)
	  (t (thread-start-joinable!
	      (instantiate::srfi18thread
		 (body (lambda () (expt 2 10)))
		 (name 'thread-join1.1))))
	  (t2 (thread-start-joinable!
	       (instantiate::srfi18thread
		  (body (lambda ()
			   (do-something-else)
			   (set! res (thread-join! t))))
		  (name 'thread-join1.2)))))
      (thread-join! t2)
      res)
   :result 1024)

;*---------------------------------------------------------------------*/
;*    thread-join3 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join3
   (with-error-to-port (open-output-string)
      (lambda ()
	 (let ((t (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (raise 123)))
		      (name 'thread-join2.1))))
	       (res 0))
	    (do-something-else)
	    (thread-join!
	     (thread-start-joinable!
	      (instantiate::srfi18thread
		 (body (lambda ()
			  (with-exception-handler
			     (lambda (exc)
				(if (isa? exc uncaught-exception)
				    (with-access::uncaught-exception exc (reason)
				       (* 10 reason))
				    99999))
			     (lambda ()
				(set! res (+ 1 (thread-join! t)))))))
		 (name 'thread-join2.2))))
	    res)))
   :result 1231)

;*---------------------------------------------------------------------*/
;*    thread-join4 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test thread-join4
   (with-error-to-port (open-output-string)
      (lambda ()
	 (let ((res '()))
	    (define (wait-for-termination! thread)
	       (let ((eh (current-exception-handler)))
		  (with-exception-handler
		     (lambda (exc)
			(tprint "EXC=" (find-runtime-type exc))
			(if (not (or (isa? exc terminated-thread-exception)
				     (isa? exc uncaught-exception)))
			    (eh exc)))
		     (lambda ()
			(thread-join! thread)
			#f))))
	    (let* ((t1 (thread-start-joinable!
			(instantiate::srfi18thread
			   (body (lambda ()
				    (sleep 5)))
			   (name 'thread-join4.1))))
		   (t2 (thread-start-joinable!
			(instantiate::srfi18thread
			   (body (lambda ()
				    (sleep 10)))
			   (name 'thread-join4.2))))
		   (t3 (thread-start-joinable!
			(instantiate::srfi18thread
			   (body (lambda ()
				    (sleep 3)
				    (thread-terminate! t2)))
			   (name 'thread-join4.3))))
		   (t4 (thread-start-joinable!
			(instantiate::srfi18thread
			   (body (lambda ()
				    (sleep 3)
				    (raise #t)))
			   (name 'thread-join4.4))))
		   (t5 (thread-start-joinable!
			(instantiate::srfi18thread
			   (body
			    (lambda ()
			       (set! res (cons (wait-for-termination! t1)
					       res))
			       (set! res (cons (wait-for-termination! t3)
					       res))
			       (set! res (cons (wait-for-termination! t4)
					       res))))
			   (name 'thread-join4.5)))))
	       (thread-join! t5)
	       res))))
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
   (cond-expand
      (bigloo-jvm
       '(0 2))
      (else
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
	  (let ((t (thread-start-joinable!
		    (instantiate::srfi18thread
		       (body (lambda ()
				(let ((m (make-mutex)))
				   (mutex-lock-recursively! m)
				   (mutex-lock-recursively! m)
				   (mutex-lock-recursively! m)
				   (set! res (cons (mutex-specific m) res))
				   (mutex-unlock-recursively! m)
				   (mutex-unlock-recursively! m)
				   (mutex-unlock-recursively! m)
				   (set! res (cons (mutex-specific m) res)))))))))
	     (thread-join! t)
	     res))))
   :result '(0 2))

;*---------------------------------------------------------------------*/
;*    mutex-state ...                                                  */
;*---------------------------------------------------------------------*/
(define-test mutex-state
   (mutex-state (make-mutex))
   :result 'not-abandoned)

;*---------------------------------------------------------------------*/
;*    mutex-lock                                                       */
;*---------------------------------------------------------------------*/
(define-test mutex-lock
   (begin
      (define m (make-mutex))
      (define (mutex-toggle m bool next)
	 (if bool
	     (begin
		(mutex-lock! m)
		#f)
	     (begin
		(mutex-unlock! m)
		#t)))
      (let ((th1 (thread-start-joinable!
		  (instantiate::srfi18thread
		     (body (lambda ()
			      (mutex-toggle m #t mutex-toggle)))))))
	 (thread-join! th1)
	 (mutex-state m)))
   :result 'abandoned)

;*---------------------------------------------------------------------*/
;*    mutex-lock2 ...                                                  */
;*---------------------------------------------------------------------*/
(define-test mutex-lock2
   (begin
      (define m (make-mutex))
      (let* ((res #unspecified)
	     (th1 (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (set! res (mutex-lock! m 1000))))))))
	 (thread-join! th1)
	 res))
   :result #t)

;*---------------------------------------------------------------------*/
;*    mutex-lock3 ...                                                  */
;*---------------------------------------------------------------------*/
(define-test mutex-lock3
   (begin
      (define m (make-mutex))
      (mutex-lock! m)
      (let* ((res #unspecified)
	     (th1 (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda () (set! res (mutex-lock! m 100))))))))
	 (thread-join! th1)
	 res))
   :result #f)

;*---------------------------------------------------------------------*/
;*    mutex-lock4                                                      */
;*---------------------------------------------------------------------*/
(define-test mutex-lock4
   (with-output-to-string
      (lambda ()
	 (let ((m (make-mutex)))
	    (thread-join!
	     (thread-start-joinable!
	      (instantiate::srfi18thread
		 (body (lambda ()
			  (let loop ()
			     (if (mutex-lock! m 0)
				 (begin
				    (display "locked")
				    (mutex-unlock! m))
				 (begin
				    (thread-yield!)
				    (loop))))))))))))
   :result "locked")

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
;*    condition-variable2                                              */
;*---------------------------------------------------------------------*/
(define-test condition-variable2
   (let ((res #f)
	 (lock (make-mutex))
	 (cv (make-condition-variable)))
      (let* ((th1 (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (mutex-lock! lock)
			       (condition-variable-signal! cv)
			       (mutex-unlock! lock)
			       (set! res 23)))))))
	 (thread-join! th1))
      res)
   :result 23)

;*---------------------------------------------------------------------*/
;*    condition-variable3                                              */
;*---------------------------------------------------------------------*/
(define-test condition-variable3
   (let ((res #f)
	 (lock (make-mutex))
	 (cv (make-condition-variable)))
      (let* ((th2 (instantiate::srfi18thread
		     (body (lambda ()
			      (mutex-lock! lock)
			      (condition-variable-signal! cv)
			      (mutex-unlock! lock)))))
	     (th1 (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (mutex-lock! lock)
			       (thread-start! th2)
			       (condition-variable-wait! cv lock)
			       (mutex-unlock! lock)
			       (set! res 23)))))))
	 (thread-join! th1))
      res)
   :result 23)

;*---------------------------------------------------------------------*/
;*    condition-variable4 ...                                          */
;*---------------------------------------------------------------------*/
(define-test condition-variable4
   (let ((res #f)
	 (lock (make-mutex))
	 (cv (make-condition-variable)))
      (let* ((th1 (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (mutex-lock! lock)
			       (condition-variable-signal! cv)
			       (mutex-unlock! lock)
			       (set! res 23)))))))
	 (thread-join! th1))
      res)
   :result 23)

;*---------------------------------------------------------------------*/
;*    condition-variable5 ...                                          */
;*---------------------------------------------------------------------*/
(define-test condition-variable5
   (let ((res #f)
	 (lock (make-mutex))
	 (cv (make-condition-variable)))
      (let* ((th1 (thread-start-joinable!
		   (instantiate::srfi18thread
		      (body (lambda ()
			       (mutex-lock! lock)
			       (set! res (condition-variable-wait! cv lock 100))
			       (mutex-unlock! lock)))))))
	 (thread-join! th1))
      res)
   :result #f)

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
;*    raise2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test raise2
   (thread-join!
    (thread-start-joinable!
     (instantiate::srfi18thread
	(body (lambda ()
		 (bind-exit (exit)
		    (with-exception-handler
		       (lambda (e)
			  (exit 45))
		       (lambda ()
			  (error 1 2 3)))))))))
   :result 45)

;*---------------------------------------------------------------------*/
;*    eval ...                                                         */
;*---------------------------------------------------------------------*/
(define-test eval
   (cond-expand
      (static-bigloo
       23)
      (else
       (begin
	  (eval '(library-load 'srfi18))
	  (eval '(thread-join!
		  (thread-start-joinable!
		     (instantiate::srfi18thread (body (lambda () 23)))))))))
   :result 23)
   
;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
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
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))
