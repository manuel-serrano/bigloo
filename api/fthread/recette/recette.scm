;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/recette/recette.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Tue Nov 15 16:35:35 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of SRFI18.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library fthread)
   (main    main))

;*---------------------------------------------------------------------*/
;*    make-thread ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander make-thread
   (lambda (x e)
      (match-case x
	 ((make-thread ?body)
	  (e `(instantiate::fthread (body ,body)) e))
	 ((make-thread ?body ?name)
	  (e `(instantiate::fthread (body ,body) (name ,name)) e))
	 (else
	  (error 'make-thread "Illegal thread" x)))))

;*---------------------------------------------------------------------*/
;*    *thread-strict-order* ...                                        */
;*---------------------------------------------------------------------*/
(define *thread-strict-order* #f)

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
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (default-scheduler (make-scheduler *thread-strict-order*))
   (flush-output-port (current-error-port))
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
   (cond-expand (fthread #t)
		(else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    cond-expand2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-test cond-expand2
   (eval '(cond-expand (fthread #t)
		       (else #f)))
   :result #f)

;*---------------------------------------------------------------------*/
;*    scheduler ...                                                    */
;*---------------------------------------------------------------------*/
(define-test scheduler
   (make-scheduler)
   :result (lambda (x) (isa? x scheduler)))

;*---------------------------------------------------------------------*/
;*    scheduler-react! ...                                             */
;*---------------------------------------------------------------------*/
(define-test scheduler-react
   (let ((s (make-scheduler)))
      (scheduler-react! s)
      (scheduler-react! s)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    yield ...                                                        */
;*---------------------------------------------------------------------*/
(define-test yield
   (let ((res 0))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ((i 22))
				    (if (> i 0)
					(begin
					   (set! res (+ res 1))
					   (thread-yield!)
					   (loop (- i 1)))))))))
      (scheduler-start! 7)
      res)
   :result 7)

;*---------------------------------------------------------------------*/
;*    yield2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test yield2
   (begin
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (thread-yield!)
				 (thread-yield!)
				 (thread-yield!)))))
      (scheduler-start!)
      (current-time))
   :result 4)

;*---------------------------------------------------------------------*/
;*    sleep ...                                                        */
;*---------------------------------------------------------------------*/
(define-test sleep
   (begin
      (thread-start! (instantiate::fthread
			(body (lambda () (thread-sleep! 10)))))
      (scheduler-start!)
      (current-time))
   :result 11)

;*---------------------------------------------------------------------*/
;*    sleep.2                                                          */
;*---------------------------------------------------------------------*/
(define-test sleep2
   (let ((res -1))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (thread-sleep! 10)
				 (set! res (current-time))))))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (thread-await! 'foo)
				    (broadcast! 'bar)
				    (thread-yield!)
				    (loop))))))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (thread-await! 'bar)
				    (broadcast! 'gee)
				    (thread-yield!)
				    (loop))))))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (broadcast! 'foo)
				    (thread-yield!)
				    (loop))))))
      (scheduler-start! 11)
      res)
   :result 11)

;*---------------------------------------------------------------------*/
;*    sleep.3                                                          */
;*---------------------------------------------------------------------*/
(define-test sleep3
   (let ((res -1))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (thread-sleep! 10)
				 (set! res (current-time))))))
      
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (thread-await! 'bar)
				    (broadcast! 'gee)
				    (thread-yield!)
				    (loop))))))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (thread-await! 'foo)
				    (broadcast! 'bar)
				    (thread-yield!)
				    (loop))))))
      (thread-start! (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (broadcast! 'foo)
				    (thread-yield!)
				    (loop))))))
      (scheduler-start! 11)
      res)
   :result 11)

;*---------------------------------------------------------------------*/
;*    await ...                                                        */
;*---------------------------------------------------------------------*/
(define-test await
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda () (thread-await! 'foo)))))))
      (scheduler-start! 2)
      res)
   :result 0)

;*---------------------------------------------------------------------*/
;*    await2 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await2
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo))))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 4)))))))
      (scheduler-start! 2)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await3 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await3
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ x res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo))))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 1)
					 (broadcast! 'bar 2)
					 (broadcast! 'bar 3)))))))
      (scheduler-start! 2)
      res)
   :result 6)

;*---------------------------------------------------------------------*/
;*    await4 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await4
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo))))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 4)))))))
      (scheduler-start! 2)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await5 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await5
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo)))
				(name 't1))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 4)
					 (thread-yield!)
					 (broadcast! 'foo 4)))
				(name 't2)))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await6 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await6
   (let* ((res 0)
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 4)
					 (thread-yield!)
					 (broadcast! 'foo 4))))))
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo)))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await7 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await7
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo)))
				(name 't1))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 4)
					 (thread-yield!)
					 (broadcast! 'foo 4)))
				(name 't2)))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await8 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await8
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo)
					 (thread-yield!)))
				(name 't1))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 4)
					 (thread-yield!)
					 (broadcast! 'foo 4)
					 (thread-yield!)))
				(name 't2)))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await9 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test await9
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ x res)))
						   (thread-get-values! 'bar))))
				(name 't1))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (broadcast! 'bar 1)
					 (broadcast! 'bar 2)
					 (broadcast! 'foo 2)))
				(name 't2)))))
      (scheduler-start!)
      res)
   :result 3)

;*---------------------------------------------------------------------*/
;*    await10 ...                                                      */
;*---------------------------------------------------------------------*/
(define-test await10
   (let* ((res 0)
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 4)
			   (broadcast! 'foo 4))))))
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await11 ...                                                      */
;*---------------------------------------------------------------------*/
(define-test await11
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar)))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'foo 4)
			   (broadcast! 'bar 4)))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await12 ...                                                      */
;*---------------------------------------------------------------------*/
(define-test await12
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))
			   (thread-yield!)
			   (thread-await! 'foo))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 4)
			   (thread-sleep! 2)
			   (broadcast! 'foo 4)))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await13 ...                                                      */
;*---------------------------------------------------------------------*/
(define-test await13
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))
			   (thread-yield!)
			   (thread-await! 'foo))))))
	      (t2 (thread-start!
		   (instantiate::fthread
		      (body (lambda ()
			       (thread-yield!)
			       (broadcast! 'foo 4)
			       (thread-yield!)
			       (broadcast! 'bar 4)
			       (broadcast! 'foo 4)))))))
      (scheduler-start!)
      res)
   :result 0)

;*---------------------------------------------------------------------*/
;*    await14 ...                                                      */
;*---------------------------------------------------------------------*/
(define-test await14
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ x res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo)
			   (set! res (+ 10 res)))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 1)
			   (broadcast! 'bar 2)
			   (thread-yield!)))))))
      (scheduler-start! 3)
      res)
   :result 3)

;*---------------------------------------------------------------------*/
;*    await-ntimes ...                                                 */
;*---------------------------------------------------------------------*/
(define-test await-ntimes
   (let ((t1 (thread-start! (instantiate::fthread
			       (body (lambda ()
					(thread-await! 'foo 6)))))))
      (scheduler-start!)
      (current-time))
   :result 7)

;*---------------------------------------------------------------------*/
;*    await-ntimes2 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes2
   (let ((t1 (thread-start! (instantiate::fthread
			       (body (lambda ()
					(thread-await! 'foo 6)))
			       (name 't1))))
	 (t2 (thread-start! (instantiate::fthread
			       (body (lambda ()
					(thread-yield!)
					(broadcast! 'foo)))
			       (name 't2)))))
      (scheduler-start!)
      (current-time))
   :result 2)

;*---------------------------------------------------------------------*/
;*    await-ntimes3 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes3
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (thread-await! 'foo 6)
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))))))))
      (scheduler-start! 3)
      res)
   :result 0)

;*---------------------------------------------------------------------*/
;*    await-ntimes4 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes4
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (thread-await! 'foo 6)
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))))
		  (name 't1))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda () (broadcast! 'bar 4)))
		  (name 't2)))))
      (scheduler-start!)
      res)
   :result 0)

;*---------------------------------------------------------------------*/
;*    await-ntimes5 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes5
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo 6))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 4)))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await-ntimes6 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes6
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ 1 res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo 6))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 4)))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await-ntimes7 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes7
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (thread-yield!)
			   (for-each (lambda (x)
					(set! res (+ x res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo 6))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (thread-yield!)
			   (broadcast! 'bar 1)
			   (broadcast! 'bar 2)
			   (broadcast! 'foo)))))))
      (scheduler-start!)
      res)
   :result 3)

;*---------------------------------------------------------------------*/
;*    await-ntimes8 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes8
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (for-each (lambda (x)
						      (set! res (+ 1 res)))
						   (thread-get-values! 'bar))
					 (thread-await! 'foo 6))))))
	  (t2 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (thread-yield!)
					 (broadcast! 'foo)
					 (broadcast! 'bar 4)))))))
      (scheduler-start!)
      res)
   :result 0)

;*---------------------------------------------------------------------*/
;*    await-ntimes9 ...                                                */
;*---------------------------------------------------------------------*/
(define-test await-ntimes9
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (thread-await! 'foo 6)
			   (for-each (lambda (x)
					(set! res (+ x res)))
				     (thread-get-values! 'bar)))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 4)
			   (thread-yield!)
			   (broadcast! 'foo)
			   (broadcast! 'bar 4)))))))
      (scheduler-start!)
      res)
   :result 4)

;*---------------------------------------------------------------------*/
;*    await-ntimes10 ...                                               */
;*---------------------------------------------------------------------*/
(define-test await-ntimes10
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ x res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo 6))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 1)
			   (thread-yield!)
			   (broadcast! 'bar 2)
			   (broadcast! 'foo)))))))
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await-ntimes11 ...                                               */
;*---------------------------------------------------------------------*/
(define-test await-ntimes11
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ x res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo 6))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (broadcast! 'bar 1)
			   (thread-yield!)
			   (broadcast! 'bar 2)))))))
      (scheduler-start!)
      (current-time))
   :result 8)

;*---------------------------------------------------------------------*/
;*    await-ntimes12 ...                                               */
;*---------------------------------------------------------------------*/
(define-test await-ntimes12
   (let* ((res 0)
	  (t1 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (for-each (lambda (x)
					(set! res (+ x res)))
				     (thread-get-values! 'bar))
			   (thread-await! 'foo 6))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body (lambda ()
			   (thread-sleep! 10)
			   (broadcast! 'bar 1)
			   (thread-yield!)
			   (broadcast! 'bar 2)))))))
      (scheduler-start!)
      res)
   :result 0)

;*---------------------------------------------------------------------*/
;*    wait-values ...                                                  */
;*---------------------------------------------------------------------*/
(define-test wait-values
   (let* ((res 0)
	  (mark #f)
	  (t (thread-start!
	      (instantiate::fthread
		 (body
		  (lambda ()
		     (for-each (lambda (x)
				  (if (not mark)
				      (begin
					 (set! mark #t)
					 (set! res (+ (current-time) res)))))
			       (thread-get-values! 'gee))
		     (set! mark #f)
		     (for-each (lambda (x)
				  (if (not mark)
				      (begin
					 (set! mark #t)
					 (set! res (+ (current-time) res)))))
			       (thread-get-values! 'gee))
		     (set! mark #f)
		     (for-each (lambda (x)
				  (if (not mark)
				      (begin
					 (set! mark #t)
					 (set! res (+ (current-time) res)))))
			       (thread-get-values! 'gee)))))))
	  (t2 (thread-start!
	       (instantiate::fthread
		  (body
		   (lambda ()
		      (broadcast! 'gee 1)
		      (broadcast! 'gee 2)
		      (broadcast! 'gee 3)
		      (thread-yield!)
		      (broadcast! 'gee 1)
		      (broadcast! 'gee 2)
		      (broadcast! 'gee 3)
		      (thread-yield!)
		      (broadcast! 'gee 1)
		      (broadcast! 'gee 2)
		      (broadcast! 'gee 3)
		      (thread-yield!))))))
	  (t3 (thread-start!
	       (instantiate::fthread
		  (body
		   (lambda ()
		      (broadcast! 'gee 1)
		      (broadcast! 'gee 2)
		      (broadcast! 'gee 3)
		      (thread-yield!)
		      (broadcast! 'gee 1)
		      (broadcast! 'gee 2)
		      (broadcast! 'gee 3)
		      (thread-yield!)
		      (broadcast! 'gee 1)
		      (broadcast! 'gee 2)
		      (broadcast! 'gee 3)
		      (thread-yield!)))))))
      (scheduler-start!)
      res)
   :result 9)

;*---------------------------------------------------------------------*/
;*    bind-exit ...                                                    */
;*---------------------------------------------------------------------*/
(define-test bind-exit
   (let ((res #f))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (set! res
			 (bind-exit (stop)
			    (+ 1 (stop #t))))))))
      (scheduler-start!)
      res)
   :result #t)

;*---------------------------------------------------------------------*/
;*    exception ...                                                    */
;*---------------------------------------------------------------------*/
(define-test exception
   (let ((res #f))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (with-exception-handler
		      (lambda (e)
			 (set! res e))
		      (lambda ()
			 (raise #t)))))))
      (scheduler-start!)
      res)
   :result #t)

;*---------------------------------------------------------------------*/
;*    exception2 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test exception2
   (let ((res #f))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (bind-exit (exit)
		      (with-exception-handler
			 (lambda (e)
			    (set! res #t)
			    (exit #f))
			 (lambda ()
			    (error 1 2 3))))))))
      (scheduler-start!)
      res)
   :result #t)

;*---------------------------------------------------------------------*/
;*    exception3 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test exception3
   (let ((res #f))
      (thread-start! (instantiate::fthread
			(body
			 (lambda ()
			    (for-each (lambda (x) (thread-yield!))
				      (iota 100 1))))))
      (thread-start! (instantiate::fthread
			(body
			 (lambda ()
			    (print 3)
			    (with-exception-handler
			       (lambda (e)
				  (set! res e))
			       (lambda ()
				  (for-each (lambda (x) (thread-yield!))
					    (iota 150 1))
				  (raise
				   (scheduler-instant
				    (current-scheduler)))))))))
      (scheduler-start!)
      res)
   :result 151)

;*---------------------------------------------------------------------*/
;*    join ...                                                         */
;*---------------------------------------------------------------------*/
(define-test join
   (let* ((th1 (thread-start!
		(instantiate::fthread
		   (body (lambda ()
			    (thread-sleep! 2))))))
	  (th2 (thread-start!
		(instantiate::fthread
		   (body (lambda ()
			    (thread-join! th1)))))))
      (scheduler-start!)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    terminate/join ...                                               */
;*---------------------------------------------------------------------*/
(define-test terminate/join
   (letrec ((th1 (thread-start!
		  (instantiate::fthread
		     (body (lambda ()
			      (thread-sleep! 2)
			      (thread-terminate! th1)))
		     (name 'terminate/join1))))
	    (th2 (thread-start!
		  (instantiate::fthread
		     (body (lambda ()
			      (bind-exit (exit)
				 (with-exception-handler
				    (lambda (e)
				       (exit e))
				    (lambda ()
				       (thread-join! th1))))))
		     (name 'terminate/join2)))))
      (scheduler-start!)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    get-values                                                       */
;*---------------------------------------------------------------------*/
(define-test get-values
   (let ((len 10))
      (for-each (lambda (x)
		   (thread-start!
		    (instantiate::fthread
		       (body (lambda ()
				(thread-yield!)
				(broadcast! 'foo x))))))
		(iota len 1))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-await! 'foo)
		   (let ((v (thread-get-values! 'foo)))
		      (set! len (-fx len (length v))))))))
      (scheduler-start!)
      len)
   :result 0)

;*---------------------------------------------------------------------*/
;*    terminate/join2 ...                                              */
;*---------------------------------------------------------------------*/
(define-test terminate/join2
   (let ((tick 0))
      (letrec ((th1 (thread-start!
		     (instantiate::fthread
			(body (lambda ()
				 (thread-sleep! 2)
				 (thread-terminate! th2)))
			(name "th1"))))
	       (th2 (thread-start!
		     (instantiate::fthread
			(body (lambda ()
				 (thread-sleep! 2)
				 (thread-terminate! th1)))
			(name "th2"))))
	       (th3 (thread-start!
		     (instantiate::fthread
			(body (lambda ()
				 (with-exception-handler
				    (lambda (e)
				       #f)
				    (lambda ()
				       (thread-join! th1)
				       (thread-join! th2)))
				 (broadcast! 'done 'th3)))
			(name "th3"))))
	       (th4 (thread-start!
		     (instantiate::fthread
			(body (lambda ()
				 (with-exception-handler
				    (lambda (e)
				       #f)
				    (lambda ()
				       (thread-join! th2)
				       (thread-join! th1)))
				 (broadcast! 'done 'th4)))
			(name "th4"))))
	       (th5 (thread-start!
		     (instantiate::fthread
			(body (lambda ()
				 (let loop ()
				    (set! tick (+fx 1 tick))
				    (let ((v (thread-get-values! 'done)))
				       (if (not (or (equal? v '(th3 th4))
						    (equal? v '(th4 th3))))
					   (loop))))))
			(name "th5")))))
	 (scheduler-start!)
	 tick))
   :result 3)

;*---------------------------------------------------------------------*/
;*    cleanup ...                                                      */
;*---------------------------------------------------------------------*/
(define-test cleanup
   (let* ((res 0)
	  (th1 (instantiate::fthread
		  (body (lambda ()
			   (thread-sleep! 10)))
		  (name 'thread-cleanup1)))
	  (th2 (instantiate::fthread
		  (body (lambda ()
			   (thread-yield!)
			   (thread-terminate! th1)))
		  (name 'thread-cleanup2))))
      (with-access::thread th1 (cleanup)
	 (set! cleanup (lambda (t) (set! res (current-time)))))
      (thread-start! th1)
      (thread-start! th2)
      (scheduler-start!)
      res)
   :result 3)

;*---------------------------------------------------------------------*/
;*    cleanup2 ...                                                     */
;*---------------------------------------------------------------------*/
(define-test cleanup2
   (let* ((res 0)
	  (th1 (instantiate::fthread
		  (body (lambda ()
			   (thread-sleep! 10)))
		  (name 'thread-cleanup1)))
	  (th2 (instantiate::fthread
		  (body (lambda ()
			   (thread-yield!)
			   (thread-terminate! th1)))
		  (name 'thread-cleanup2)))
	  (th3 (instantiate::fthread
		  (body (lambda ()
			   (thread-sleep! 2)
			   (thread-terminate! th1)))
		  (name 'thread-cleanup3))))
      (with-access::thread th1 (cleanup)
	 (set! cleanup (lambda (r) (set! res (current-time)))))
      (thread-start! th1)
      (thread-start! th2)
      (thread-start! th3)
      (scheduler-start!)
      res)
   :result 3)

;*---------------------------------------------------------------------*/
;*    call/cc ...                                                      */
;*---------------------------------------------------------------------*/
(define-test call/cc
   (cond-expand
      (bigloo-c
       (let ((res #f))
	  (thread-start!
	   (instantiate::fthread
	      (body (lambda ()
		       (set! res
			     (call/cc (lambda (kont)
					 (+ 14 (kont #t)))))))))
	  (scheduler-start!)
	  res))
      (else
       #t))
   :result #t)

;*---------------------------------------------------------------------*/
;*    call/cc2                                                         */
;*---------------------------------------------------------------------*/
(define-test call/cc2
   (cond-expand
      (bigloo-c
       (let ((res 0))
	  (let ((t1 (thread-start!
		     (instantiate::fthread
			(body
			 (lambda ()
			    (set! res
				  (+ 1 (call/cc (lambda (kont)
						   (broadcast! 'kont kont)
						   0))))))
			(name 't1))))
		(t2 (thread-start!
		     (instantiate::fthread
			(body
			 (lambda ()
			    (let ((kont (thread-await! 'kont)))
			       (try (kont 23)
				    (lambda (escape obj proc msg)
				       (set! res 36)
				       (escape #t))))))
			(name 't2)))))
	     (scheduler-start!)
	     res)))
      (else
       36))
   :result 36)

;*---------------------------------------------------------------------*/
;*    current-ports ...                                                */
;*---------------------------------------------------------------------*/
(define-test current-ports
   (let ((g '()))
      (define (add! v)
	 (set! g (cons v g)))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (with-input-from-string "1 2 3"
		      (lambda ()
			 (with-output-to-string
			    (lambda ()
			       (thread-yield!)
			       (add! (read))
			       (thread-yield!)
			       (add! (read))
			       (thread-yield!)
			       (add! (read))))))))))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (with-input-from-string "a b c"
		      (lambda ()
			 (with-output-to-string
			    (lambda ()
			       (thread-yield!)
			       (add! (read))
			       (thread-yield!)
			       (add! (read))
			       (thread-yield!)
			       (add! (read))))))))))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (with-input-from-string "toto tutu tata"
		      (lambda ()
			 (with-output-to-string
			    (lambda ()
			       (thread-yield!)
			       (add! (read))
			       (thread-yield!)
			       (add! (read))
			       (thread-yield!)
			       (add! (read))))))))))

      (with-access::scheduler (default-scheduler) (strict-order?)
	 (set! strict-order? #t)
	 (scheduler-start!)
	 (set! strict-order? #f))
      g)
   :result '(tata c 3 tutu b 2 toto a 1))

;*---------------------------------------------------------------------*/
;*    await*                                                           */
;*---------------------------------------------------------------------*/
(define-test await*
   (let ((res #f))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (let ((sig* (list 'foo 'bar)))
		      (multiple-value-bind (val1 sig1)
			 (thread-await*! sig*)
			 (multiple-value-bind (val2 sig2)
			    (thread-await*! sig*)
			    (thread-yield!)
			    (multiple-value-bind (val3 sig3)
			       (thread-await*! sig*)
			       (set! res (list sig1 sig2 sig3))))))))))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-sleep! 2)
		   (broadcast! 'foo 1)))))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-sleep! 3)
		   (broadcast! 'bar 2)))))
      (scheduler-start!)
      res)
   :result '(foo foo bar))

;*---------------------------------------------------------------------*/
;*    await*                                                           */
;*---------------------------------------------------------------------*/
(define-test await*-ntimes
   (let ((res #f))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (let ((sig* (list 'foo 'bar)))
		      (multiple-value-bind (val1 sig1)
			 (thread-await*! sig* 1)
			 (thread-yield!)
			 (multiple-value-bind (val2 sig2)
			    (thread-await*! sig* 1)
			    (thread-yield!)
			    (multiple-value-bind (val3 sig3)
			       (thread-await*! sig* 2)
			       (set! res (list sig1 sig2 sig3))))))))
	  (name 't1)))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-sleep! 2)
		   (broadcast! 'foo 1)))
	  (name 't2)))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-sleep! 3)
		   (broadcast! 'bar 2)))
	  (name 't3)))
      (scheduler-start!)
      res)
   :result '(#f foo bar))

;*---------------------------------------------------------------------*/
;*    get-nth-line ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-nth-line file num)
   (with-input-from-file file
      (lambda ()
	 (let loop ((num num)
		    (ln (read-line (current-input-port))))
	    (if (>fx num 0)
		(loop (-fx num 1)
		      (read-line (current-input-port)))
		ln)))))

;*---------------------------------------------------------------------*/
;*    get-values* ...                                                  */
;*---------------------------------------------------------------------*/
(define-test get-values*
   (let ((s1 'foo)
	 (s2 'bar)
	 (s3 'gee)
	 (res #f))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-sleep! 2)
		   (broadcast! 'foo (current-time))
		   (broadcast! 'bar 0)))))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-await*! (list s1 s2 s3))
		   (set! res (thread-get-values*! (list s1 s2 s3)))))))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-sleep! 2)
		   (broadcast! 'bar (current-time))))))
      (scheduler-start!)
      res)
   :result '((foo 3) (bar 3 0) (gee)))

;*---------------------------------------------------------------------*/
;*    make-sleep-signal ...                                            */
;*---------------------------------------------------------------------*/
(define (make-sleep-signal ms)
   (make-asynchronous-signal (lambda (s) (sleep ms))))

;*---------------------------------------------------------------------*/
;*    fair-sleep ...                                                   */
;*---------------------------------------------------------------------*/
(define-test fair-sleep
   (let ((r 0))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-await! (make-sleep-signal 1000))
		   (set! r 1)))))
      (scheduler-start!)
      r)
   :result 1)

;*---------------------------------------------------------------------*/
;*    fair-sleep2 ...                                                  */
;*---------------------------------------------------------------------*/
(define-test fair-sleep2
   (let ((r 0))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (thread-await! (make-sleep-signal 1000) 10)
		   (set! r (current-time))))))
      (scheduler-start!)
      r)
   :result (lambda (v) (<= v 11)))

;*---------------------------------------------------------------------*/
;*    fair-sleep3 ...                                                  */
;*---------------------------------------------------------------------*/
(define-test fair-sleep3
   (let ((r 0))
      (thread-start!
       (instantiate::fthread
	  (body (lambda ()
		   (make-sleep-signal 1000)
		   (set! r (current-time))))))
      (scheduler-start!)
      r)
   :result 1)

;*---------------------------------------------------------------------*/
;*    resume ...                                                       */
;*---------------------------------------------------------------------*/
(define-test resume
   (let* ((res 0)
	  (t1 (thread-start! (instantiate::fthread
				(body (lambda ()
					 (thread-suspend! (current-thread))
					 (set! res 1)))))))
      (scheduler-start! 2)
      (thread-resume! t1)
      (scheduler-start! 2)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    terminate1 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test terminate1
   (letrec ((th1 (instantiate::fthread
		    (body (lambda () (let loop ()
					(thread-terminate! th2)
					(thread-yield!)
					(loop))))))
	    (th2 (instantiate::fthread
		    (body (lambda () (let loop ()
					(thread-terminate! th1)
					(thread-yield!)
					(loop)))))))
      (thread-start! th1)
      (thread-start! th2)
      (scheduler-start!)
      #t)
   :result #t)


;*---------------------------------------------------------------------*/
;*    terminate2 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test terminate2
   (letrec ((th1 (instantiate::fthread
		    (body (lambda () (let loop ()
					(thread-terminate! th2)
					(thread-yield!)
					(loop))))))
	    (th2 (instantiate::fthread
		    (body (lambda () (let loop ()
					(thread-terminate! th1)
					(thread-yield!)
					(loop)))))))
      (thread-start! th2)
      (thread-start! th1)
      (scheduler-start!)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    terminate3 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test terminate3
   (letrec ((th1 (instantiate::fthread
		    (body (lambda () 
			     (thread-yield!)
			     (thread-terminate! th2)))))
	    (th2 (instantiate::fthread
		    (body (lambda () (thread-await! 'foo))))))
      (thread-start! th1)
      (thread-start! th2)
      (scheduler-start!)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    terminate4 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test terminate4
   (letrec ((th1 (instantiate::fthread
		    (body (lambda () 
			     (thread-yield!)
			     (thread-terminate! th2)))))
	    (th2 (instantiate::fthread
		    (body (lambda () (thread-await! 'foo))))))
      (thread-start! th2)
      (thread-start! th1)
      (scheduler-start!)
      #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    terminate5 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test terminate5
   (let ((res #f))
      (letrec ((th1 (instantiate::fthread
		       (body (lambda ()
				(thread-yield!)
				(thread-terminate! th2)))))
	       (th2 (instantiate::fthread
		       (body (lambda () (thread-await! 'foo))))))
	 (with-access::thread th2 (cleanup)
	    (set! cleanup (lambda (t) (set! res #t))))
	 (thread-start! th2)
	 (thread-start! th1)
	 (scheduler-start!)
	 res))
   :result #t)

;*---------------------------------------------------------------------*/
;*    terminate6 ...                                                   */
;*---------------------------------------------------------------------*/
(define-test terminate6
   (let* ((res -1)
	  (th1 (instantiate::fthread
		  (body
		   (lambda ()
		      (let loop ()
			 (set! res (thread-await! 'foo))
			 (thread-yield!)
			 (scheduler-terminate!)
			 (loop))))))
	  (th2 (instantiate::fthread
		  (body
		   (lambda ()
		      (let loop ((n 0))
			 (broadcast! 'foo n)
			 (thread-yield!)
			 (loop (+ 1 n))))))))
      (thread-start! th1)
      (thread-start! th2)
      (scheduler-start!)
      res)
   :result 1)

;*---------------------------------------------------------------------*/
;*    await-values*! ...                                               */
;*---------------------------------------------------------------------*/
(define-test await-values*!
   (let ((res #f))
      (let loop ((i 10))
	 (when (> i 0)
	    (begin
	       (thread-start!
		(instantiate::fthread
		   (body
		    (lambda ()
		       (let loop ()
			  (let ((sigs (thread-await-values*! (list i (+ i 1)))))
			     (when (= i 7)
				(set! res (assq 7 sigs)))
			     (broadcast! (- i 1))
			     (thread-yield!)
			     (loop)))))))
	       (loop (- i 1)))))
      (thread-start!
       (instantiate::fthread
	  (body
	   (lambda ()
	      (thread-yield!)
	      (broadcast! 1 #f)
	      (broadcast! 2 #f)
	      (broadcast! 4 #f)
	      (broadcast! 8 #f)))))
      (scheduler-start! 3)
      res)
   :result '(7))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (("-p" (help "Preserve thread execution order"))
	  (set! *thread-strict-order* #t))
	 (("-g?level" (help "Debug"))
	  (bigloo-debug-set! (string->integer level)))
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
	     " tests completed...")))




