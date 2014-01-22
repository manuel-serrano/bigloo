;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/kapture.scm                  */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 11 10:37:55 1992                          */
;*    Last change :  Wed Jan 22 08:17:43 2014 (serrano)                */
;*                                                                     */
;*    Des tests de capture de variables                                */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module kapture
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-kapture)
	    (plante-9 file))
   (option  (set! *inlining?* #f)))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1 x y z)
   (labels ((hux (a)  x)
	    (bar (a) (hux y))
	    (gee ()  (bar z)))
      gee))

;*---------------------------------------------------------------------*/
;*    test2 ...                                                        */
;*---------------------------------------------------------------------*/
(define test2 (labels ((lam_0 ()
			    (labels ((ignore ()
					     (let ((v 1))
						(labels ((foo2 () v))
						   (foo2)))))
			       (ignore))))
	       lam_0))

;*---------------------------------------------------------------------*/
;*    luc ...                                                          */
;*---------------------------------------------------------------------*/
(define luc
   (lambda ()
      (let ((counter 0))
	 (lambda x
	    (set! counter (+ counter 1))
	    (string->symbol
	     ((if (null? x)
		  (lambda (u)
		     (string-append ":X"  u))
		  (if (string? (car x))
		      (lambda (u)
			 (string-append (car x)  u))
		      (lambda (u)
			 (string-append (symbol->string (car x))  u))))
	      (number->string counter)))))))

;*---------------------------------------------------------------------*/
;*    kapture:test ...                                                 */
;*---------------------------------------------------------------------*/
(define (kapture:test)
   (labels ((gee (y) (lambda (x) (set! y x) y)))
      ((gee 0) 2)))

;*---------------------------------------------------------------------*/
;*    plante-7 ...                                                     */
;*---------------------------------------------------------------------*/
(define (plante-7 file7)
   (let* ((toto (lambda () file7))
	  (foo7 (lambda ()
		  (let ((bar (lambda ()
				(let ((gee (lambda ()
					      (let ((hux (lambda () (toto))))
						 (hux)))))
				   (gee)))))
		     (bar)))))
      (toto)
      foo7))

;*---------------------------------------------------------------------*/
;*    plante-8 ...                                                     */
;*---------------------------------------------------------------------*/
(define (plante-8 file8)
   (letrec ((lexer (begin
		      (set-car! (cons 1 2) 4)
		      (lambda (input-port)
			 (labels ((toto () (labels ((gee () (foo8)))
					      (parse)
					      (gee))))
			    (toto)))))
	    (parse (lambda ()
		      (letrec ((numl
				(begin
				   (set-car! (cons 1 2) 4)
				   (lambda (input-port)
				      (labels ((tata ()
						     (labels ((hux () (foo8)))
							(hux))))
					 (tata))))))
			 (numl #f))))
	    (foo8 (lambda () file8)))
      (lexer file8)))

;*---------------------------------------------------------------------*/
;*    plante-9 ...                                                     */
;*---------------------------------------------------------------------*/
(define (plante-9 file9)
   (let ((port (open-input-file file9)))

      (define (lexical-error msg obj)
	 (let ((name (input-port-name port))
	       (filepos (input-port-position port)))
	    (error 'lexer msg obj)))
  
      (define (number-parse str)
	 (define number-lexer
	    (regular-grammar ()
	       ((+ (in #\0 #\9)) (the-fixnum))
	       (else (lexical-error "ERR" str))))

	 (let* ((oport (open-input-string str))
		(value (read/rp number-lexer oport)))
	    (close-input-port oport)
	    value))

      (define lexer
	 (regular-grammar ()
	    ((+ (in #\0 #\9)) (number-parse (the-string)))
	    (else (lexical-error "ERR" (the-failure)))))

      (if port (read/rp lexer port)
	  (error 'read-models "Cannot open file" file9))))

;*---------------------------------------------------------------------*/
;*    dataflow ...                                                     */
;*---------------------------------------------------------------------*/
(define (dataflow l)
   (let ((x (cons 2 l)))
      (letrec ((f1 (lambda ()
		      (set! x '#(2))))
	       (f2 (lambda ()
		      (if (pair? x)
			  (begin
			     (f1)
			     (if (pair? x)
				 (car x)
				 (vector-ref x 0)))))))
	 (f2))))

;*---------------------------------------------------------------------*/
;*    lightfuncall ...                                                 */
;*---------------------------------------------------------------------*/
(define (lightfuncall)
   (define (f1) #t)
   (define (f2) #f)
   (+ (lightfuncall-foo f1) (lightfuncall-foo f2)))

(define (lightfuncall-foo f)
   (if (f) 1 2))

;*---------------------------------------------------------------------*/
;*    integrate-type ...                                               */
;*---------------------------------------------------------------------*/
(define (integrate-type::pair-nil locals::pair-nil node-or-nodes)
   
   (define (kill-used-in-node n)
      (set! locals (remq n locals)))

   (when (pair? locals)
      (cond
	 ((null? node-or-nodes)
	  #unspecified)
	 ((pair? node-or-nodes)
	  (for-each kill-used-in-node node-or-nodes))
	 (else
	  (kill-used-in-node node-or-nodes))))

   locals)

;*---------------------------------------------------------------------*/
;*    test-kapture ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-kapture)
   (test-module "kapture" "kapture.scm")
   (test "kapture.1" ((test1 1 2 3)) 1)
   (test "kapture.2" (test2) 1)
   (test "luc-kapture" (let ((gen-sym (luc)))
			  (gen-sym 'a)) 'a1)
   (test "side-effect" (kapture:test) 2)
   (test "nesting.1" ((plante-7 7)) 7)
   (test "nesting.2" (plante-8 8) 8)
   (test "dataflow" (dataflow '(1)) 2)
   (test "light-funcall" (lightfuncall) 3)
   (test "integrate-type" (integrate-type (list 1 2 4) (list 4 6 67)) '(1 2)))
