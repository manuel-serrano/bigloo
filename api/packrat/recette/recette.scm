;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/packrat/recette/recette.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Mon Apr 20 09:00:20 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of packrat.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library packrat)
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
      (packrat #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    generator ...                                                    */
;*---------------------------------------------------------------------*/
(define (generator tokens)
   (let ((stream tokens))
      (lambda ()
         (if (null? stream)
             (values #f #f)
             (let ((base-token (car stream)))
                (set! stream (cdr stream))
                (values #f base-token))))))

;*---------------------------------------------------------------------*/
;*    calc ...                                                         */
;*---------------------------------------------------------------------*/
(define calc
   (packrat-parser expr
       (expr ((a <- mulexp '+ b <- mulexp)
	      (+ a b))
	     ((a <- mulexp) a))
       (mulexp ((a <- simple '* b <- simple)
		(* a b))
	       ((a <- simple) a))
       (simple ((a <- 'num) a)
	       (('oparen a <- expr 'cparen) a))))

;*---------------------------------------------------------------------*/
;*    parse ...                                                        */
;*---------------------------------------------------------------------*/
(define-test packrat.parse
   (let* ((g (generator '((num . 1) (+) (num . 2) (*) (num . 3))))
          (r (calc (base-generator->results g))))
      (when (parse-result-successful? r)
	 (parse-result-semantic-value r)))
   :result 7)

;*---------------------------------------------------------------------*/
;*    json                                                             */
;*---------------------------------------------------------------------*/
(define-test packrat-json.write
   (with-output-to-string
      (lambda ()
	 (json-write '(3 4 5 6))))
   :result "[3, 4, 5, 6]")

(define-test packrat-json.read
   (with-input-from-string "[3, 4, 5, 6]"
      json-read)
   :result '(3 4 5 6))

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


   

