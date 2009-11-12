;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi27/recette/recette.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Thu Nov 12 16:20:26 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of srfi27.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library srfi27)
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
	     (print "   ==> provided: [" (with-output-to-string
					    (lambda () (write provided)))
		    "]\n       expected: ["
		    (let ((r (if (procedure? res) (res 'result) res)))
		       (with-output-to-string
			  (lambda () (write r))))
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
      (srfi27 #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    my-random-integer ...                                            */
;*---------------------------------------------------------------------*/
(define (my-random-integer n)
   (let ((x (random-integer n)))
      (if (<= 0 x (- n 1))
	  x
	  (error 'srfi27 "(random-integer n) returned illegal value" x))))

;*---------------------------------------------------------------------*/
;*    my-random-real ...                                               */
;*---------------------------------------------------------------------*/
(define (my-random-real)
   (let ((x (random-real)))
      (if (< 0 x 1)
	  x
	  (error 'srfi27 "(random-real) returned illegal value" x))))

;*---------------------------------------------------------------------*/
;*    basic ...                                                        */
;*---------------------------------------------------------------------*/
(define-test srfi27.large-numbers
   (do ((k 0 (+ k #z1))
	(n 1 (* n #z2)))
       ((> k #z1024))
       (my-random-integer n)))

(define-test srfi27.reals
   (do ((k 0 (+ k 1))
	(x (my-random-real) (+ x (my-random-real))))
       ((= k 1000)
	x)))

(define-test srfi27.get/set
   (let* ((state1 (random-source-state-ref default-random-source))
	  (x1 (my-random-integer (expt #z2 32)))
	  (state2 (random-source-state-ref default-random-source))
	  (x2 (my-random-integer (expt #z2 32))))
      (random-source-state-set! default-random-source state1)
      (let ((y1 (my-random-integer (expt #z2 32))))
	 (if (not (= x1 y1))
	     (error 'srfi27
		    "state get/set doesn't work"
		    (list x1 y1 state1))))
      (random-source-state-set! default-random-source state2)
      (let ((y2 (my-random-integer (expt #z2 32))))
	 (if (not (= x2 y2))
	     (error 'srfi27
		    "state get/set doesn't work"
		    (list x2 y2 state2))))))

(define-test srfi27.randomize
   (let* ((state1 (random-source-state-ref default-random-source))
	  (x1 (my-random-integer (expt #z2 32))))
      (random-source-state-set! default-random-source state1)
      (random-source-randomize! default-random-source)
      (let ((y1 (my-random-integer (expt #z2 32))))
	 (if (= x1 y1)
	     (error 'srfi27
		    "random-source-randomize! didn't work"
		    (list x1 state1))))))

(define-test srfi27.pseudo-randomize
   (let* ((state1 (random-source-state-ref default-random-source))
	  (x1 (my-random-integer (expt #z2 32))))
      (random-source-state-set! default-random-source state1)
      (random-source-pseudo-randomize! default-random-source 0 1)
      (let ((y1 (my-random-integer (expt #z2 32))))
	 (if (= x1 y1)
	     (error 'srfi27
		    "random-source-pseudo-randomize! didn't work"
		    (list x1 state1))))
      (random-source-state-set! default-random-source state1)
      (random-source-pseudo-randomize! default-random-source 1 0)
      (let ((y1 (my-random-integer (expt #z2 32))))
	 (if (= x1 y1)
	     (error 'srfi27
		    "random-source-pseudo-randomize! didn't work"
		    (list x1 state1))))))

;*---------------------------------------------------------------------*/
;*    MRG32ka                                                          */
;*---------------------------------------------------------------------*/
(define-test srfi27-mrg32ka.1
   (let* ((s (make-random-source))
	  (state1 (random-source-state-ref s))
	  (rand (random-source-make-reals s)))
      (random-source-state-set! s '(lecuyer-mrg32k3a 1 0 0 1 0 0))
      (do ((k 0 (+ k 1)))
	  ((= k 16)
	   (let ((state2 (random-source-state-ref s)))
	      (if (not (equal? state1 state2))
		  (error 'srfi27
			 "16-th state after (1 0 0 1 0 0) is wrong"
			 (list state1 state2)))))
	  (rand))))

(define-test srfi27-mrg32ka.2
   (let ((s (make-random-source)))
      (random-source-pseudo-randomize! s 1 2)
      (if (not (equal? (random-source-state-ref s)
		       '(lecuyer-mrg32k3a 
			 1250826159 
			 3004357423 
			 431373563 
			 3322526864 
			 623307378 
			 2983662421)))
	  (error 'srfi27
		 "pseudo-randomize! gives wrong result"
		 #unspecified))))

(define-test srfi27-mrg32ka.3
   (let* ((x 0.0) 
	  (s (make-random-source))
	  (rand (random-source-make-reals s)))
      (random-source-state-set!
       s
       '(lecuyer-mrg32k3a 12345 12345 12345 12345 12345 12345))
      (do ((k 0 (+ k 1)))
	  ((= k 10000000)
	   (if (not (< (abs (- x 5001090.95)) 0.01))
	       (error 'srfi27 "bad sum over 10^7 reals" x)))
	  (set! x (+ x (rand))))))

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
