;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/hygiene.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 27 12:52:59 1998                          */
;*    Last change :  Wed Aug  4 06:44:44 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Hygienic macro tests                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hygiene
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-hygiene)))

;*---------------------------------------------------------------------*/
;*    global syntax                                                    */
;*---------------------------------------------------------------------*/
(define-syntax funcall
  (syntax-rules ()
     ((funcall function arguments ...)
      (function arguments ...) ) ) )

(define-syntax hunless
   (syntax-rules ()
      ((hunless condition form ...)
       (if (not condition) (begin form ...)) ) ) )

(define-syntax hwhen
  (syntax-rules ()
    ((hwhen condition form ...)
     (if condition (begin form ...)) ) ) )

(define-syntax progn
  (syntax-rules ()
    ((progn body ...)
     (begin body ...) ) ) )

(define (test1)
  (list
   (let ((f (lambda (x) (funcall cons x x))))
     (funcall f (+ 2 3)) )
   (let ((x 'a))
     (hunless 1 (set! x 'b) 3)
     x )
   (hunless #f 2 3)
   (let ((x 'a))
     (hunless #f (set! x 'b) 3)
     x )
   (progn (hwhen #f 2 3) (hwhen 1 2 3))
   (let ((x 'a))
     (hwhen #f (set! x 'b) 3)
     x )
   ) )

(define (test1-eval)
   (eval '(define-syntax funcall
	     (syntax-rules ()
		((funcall function arguments ...)
		 (function arguments ...) ) ) ) )

   (eval '(define-syntax hunless
	     (syntax-rules ()
		((hunless condition form ...)
		 (if (not condition) (begin form ...)) ) ) ) )

   (eval '(define-syntax hwhen
	     (syntax-rules ()
		((hwhen condition form ...)
		 (if condition (begin form ...)) ) ) ) )

   (eval '(define-syntax progn
	     (syntax-rules ()
		((progn body ...)
		 (begin body ...) ) ) ) )

   (eval '(define (test1)
	     (list
	      (let ((f (lambda (x) (funcall cons x x))))
		 (funcall f (+ 2 3)) )
	      (let ((x 'a))
		 (hunless 1 (set! x 'b) 3)
		 x )
	      (hunless #f 2 3)
	      (let ((x 'a))
		 (hunless #f (set! x 'b) 3)
		 x )
	      (progn (hwhen #f 2 3) (hwhen 1 2 3))
	      (let ((x 'a))
		 (hwhen #f (set! x 'b) 3)
		 x )
	      ) ) )
   (eval '(test1)))

;*---------------------------------------------------------------------*/
;*    define ...                                                       */
;*---------------------------------------------------------------------*/
(define-syntax foodefine
  (syntax-rules ()
    ((foodefine x v)
     (define x v))))
(foodefine rglup 4)

(define (test2-eval)
   (eval '(define-syntax foodefine
	     (syntax-rules ()
		((foodefine x v)
		 (define x v)))))
   (eval ' (foodefine rglup 4)))

(define-syntax array
  (syntax-rules ()
    ((array e e-get (t u ...))
     (begin (define e (make-vector (* t u ...) 0))
            (define e-get (array-sub e 0 (i j k l m n o p) () (u ... 1)))))))

(define-syntax array-sub
  (syntax-rules ()
    ((array-sub v old (i index ...) (arg ...) ())
     (lambda (arg ...) (vector-ref v old)))
    ((array-sub v old (i index ...) (arg ...) (s slice ...))
     (array-sub v (* (+ old i) s) (index ...) (arg ... i) (slice ...)))))

(array y gety (4 5 6))
(gety 2 3 4)

(define e (make-vector (* 10 10 10) 0))
(define e-get (array-sub e 0 (i j k l) () (10 10 1)))

;*---------------------------------------------------------------------*/
;*    test-let ...                                                     */
;*---------------------------------------------------------------------*/
(define-syntax tel
   (syntax-rules ()
      ((tel ((val var) ...) body ...)
       (let ((var val) ...) body ...))
      ((tel * ((val var) ...) body ...)
       (let* ((var val) ...) body ...))))

(define (test-let)
   (+ (let ((x 1))
	 (tel ((2 x) ((+ 3 x) y)) x y (+ x y x)))
      (let ((x 1))
	 (tel * ((2 x) ((+ 3 x) y)) x y (+ x y x)))))

;*---------------------------------------------------------------------*/
;*    test-bind-exit ...                                               */
;*---------------------------------------------------------------------*/
(define-syntax my-begin
  (syntax-rules ()
    ((my-begin body ...)
     (begin body ...))))

(define (test-bind-exit val)
   (my-begin
    (bind-exit (return)
       (return (+ val 1))
       val)))

;*---------------------------------------------------------------------*/
;*    test-r5rs-cond ...                                               */
;*---------------------------------------------------------------------*/
(define (test-r5rs-cond)
   (eval '(let ((x 3)
		(y 2))
	   (letrec-syntax ((cond' (syntax-rules (else =>)
				     ((cond' (else result1 result2 ...))
				      (begin result1 result2 ...))
				     ((cond' (test => result))
				      (let ((temp test))
					 (if temp (result temp))))
				     ((cond' (test => result) clause1 clause2 ...)
				      (let ((temp test))
					 (if temp
					     (result temp)
					     (cond' clause1 clause2 ...))))
				     ((cond' (test)) test)
				     ((cond' (test) clause1 clause2 ...)
				      (let ((temp test))
					 (if temp
					     temp
					     (cond' clause1 clause2 ...))))
				     ((cond' (test result1 result2 ...))
				      (if test (begin result1 result2 ...)))
				     ((cond' (test result1 result2 ...)
					     clause1 clause2 ...)
				      (if test
					  (begin result1 result2 ...)
					  (cond' clause1 clause2 ...))))))
	      (cond' ((>= x 4) #f)
		     ((>= y 2) #t)
		     (else #t))))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-case ...                                               */
;*---------------------------------------------------------------------*/
(define (test-r5rs-case)
   (eval '(let ((x 3)
		(y 2))
	   (letrec-syntax ((case' (syntax-rules (else)
				     ((case' (key ...)
					     clauses ...)
				      (let ((atom-key (key ...)))
					 (case' atom-key clauses ...)))
				     ((case' key
					     (else result1 result2 ...))
				      (begin result1 result2 ...))
				     ((case' key
					     ((atoms ...) result1 result2 ...))
				      (if (memv key '(atoms ...))
					  (begin result1 result2 ...)))
				     ((case' key
					     ((atoms ...) result1 result2 ...)
					     clause clauses ...)
				      (if (memv key '(atoms ...))
					  (begin result1 result2 ...)
					  (case' key clause clauses ...))))))
	      (case' x
		     ((1) 1)
		     ((2) 2)
		     ((3) #t)
		     (else #f))))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-and ...                                                */
;*---------------------------------------------------------------------*/
(define (test-r5rs-and)
   (eval '(let ((x 3)
		(y 2))
	   (letrec-syntax ((and' (syntax-rules ()
				    ((and') #t)
				    ((and' test) test)
				    ((and' test1 test2 ...)
				     (if test1 (and' test2 ...) #f)))))
	      (if (and' x y) #t #f)))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-or ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-r5rs-or)
   (eval '(let ((x 3)
		(y 2))
	   (letrec-syntax ((or' (syntax-rules ()
				   ((or') #f)
				   ((or' test) test)
				   ((or' test1 test2 ...)
				    (let ((x test1))
				       (if x x (or' test2 ...)))))))
	      (if (or' (> x 4) (> y 1)) #t #f)))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-let ...                                                */
;*---------------------------------------------------------------------*/
(define (test-r5rs-let)
   (eval '(let ((x 3)
		(y 2))
	   (let-syntax ((let' (syntax-rules ()
				 ((let' ((name val) ...) body1 body2 ...)
				  ((lambda (name ...) body1 body2 ...)
				   val ...))
				 ((let' tag ((name val) ...) body1 body2 ...)
				  ((letrec ((tag (lambda (name ...)
						    body1 body2 ...)))
				      tag)
				   val ...)))))
	      (let' ((x x)
		     (y y))
		    (> x y))))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-let* ...                                               */
;*---------------------------------------------------------------------*/
(define (test-r5rs-let*)
   (eval '(let ((x 3)
		(y 2))
	   (letrec-syntax ((let*' (syntax-rules ()
				     ((let*' () body1 body2 ...)
				      (let () body1 body2 ...))
				     ((let*' ((name1 val1) (name2 val2) ...)
					     body1 body2 ...)
				      (let ((name1 val1))
					 (let*' ((name2 val2) ...)
						body1 body2 ...))))))
	      (let*' ((x y)
		      (z x))
		     (=fx z 2))))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-letrec ...                                             */
;*---------------------------------------------------------------------*/
(define (test-r5rs-letrec)
   (eval '(letrec-syntax ((letrec' (syntax-rules ()
				      ((letrec' ((var1 init1) ...) body ...)
				       (letrec' "generate temp names"
						(var1 ...)
						()
						((var1 init1) ...)
	 					body ...))
				      ((letrec' "generate temp names"
						()
						(temp1 ...)
						((var1 init1) ...)
						body ...)
				       (let ((var1 #unspecified) ...)
					  (let ((temp1 init1) ...)
					     (set! var1 temp1)
					     ...
					     body ...)))
				      ((letrec' "generate temp names"
						(x y ...)
						(temp ...)
						((var1 init1) ...)
						body ...)
				       (letrec' "generate temp names"
						(y ...)
						(newtemp temp ...)
						((var1 init1) ...)
						body ...)))))
	   (letrec' ((f1 (lambda (x) (if (< x 1) #t (f2 (-fx x 1)))))
		     (f2 (lambda (x) (if (< x 1) #t (f1 (-fx x 1))))))
		    (f1 3)))))

;*---------------------------------------------------------------------*/
;*    test-r5rs-do ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-r5rs-do)
   (eval '(letrec-syntax ((do' (syntax-rules ()
				  ((do' ((var init step ...) ...)
					(test expr ...)
					command ...)
				   (letrec ((luup
					     (lambda (var ...)
						(if test
						    (begin
						       (if #f #f)
						       expr ...)
						    (begin
						       command
						       ...
						       (luup (do' "step" var step ...)
							     ...))))))
				      (luup init ...)))
				  ((do' "step" x)
				   x)
				  ((do' "step" x y)
				   y))))
	   (do' ((vec (make-vector 5))
		 (i 0 (+ i 1)))
		((= i 5) vec)
		(vector-set! vec i i)))))

;*---------------------------------------------------------------------*/
;*    test-hygiene ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-hygiene)
   (test-module "hygiene" "hygiene.scm")
   (test "let-syntax.1"
	 (let-syntax ((when (syntax-rules ()
			       ((when test stmt1 stmt2 ...)
				(if test
				    (begin stmt1 stmt2 ...))))))
	    (let ((if #t))
	       (when if (set! if 'now))
	       if))
	 'now)
   (test "let-syntax.2"
	 (let ((x 'outer))
	    (let-syntax ((m (syntax-rules () ((m) x))))
	       (let ((x 'inner))
		  (m))))
	 'outer)
   (test "letrec-syntax"
	 (letrec-syntax ((my-or (syntax-rules ()
				   ((my-or) #f)
				   ((my-or e) e)
				   ((my-or e1 e2 ...)
				    (let ((temp e1))
				       (if temp
					   temp
					   (my-or e2 ...)))))))
	    (let ((x #f)
		  (y 7)
		  (temp 8)
		  (let2 oddfx?)
		  (if2 evenfx?))
	       (my-or x
		      (let2 temp)
		      (if2 y)
		      y)))
	 7)
   (test "define-syntax" (test1) '((5 . 5) a 3 b 3 a))
   (test "eval" (test1-eval) '((5 . 5) a 3 b 3 a))
   (test "define" rglup 4)
   (test "define.2" (e-get 4 3 5) 0)
   (test "define(eval)" (begin (test2-eval) (eval 'rglup)) 4)
   (test "let" (test-let) 17)
   (test "let vs letrec.1" (let ((syn2 (lambda l 'ok-let)))
			      (let-syntax ((syn2 (syntax-rules ()
						    ((syn2 "*" body ...)
						     'nok-let)
						    ((syn2 body ...)
						     (syn2 "*" body ...)))))
				 (syn2 'foo)))
	 'ok-let)
   (test "let vs letrec.2" (let ((syn1 (lambda () 'nok-letrec)))
			      (letrec-syntax ((syn1 (syntax-rules ()
						       ((syn1 "*" body ...)
							'ok-letrec)
						       ((syn1 body ...)
							(syn1 "*" body ...)))))
				 (syn1 'foo)))
	 'ok-letrec)
   (test "hygiene.1" (letrec-syntax ((foo (syntax-rules ()
					     ((_) (letrec-syntax ((bar (syntax-rules () ((_) 'blah))))
						     (bar))))))
			(foo))
	 'blah)
   (test "bind-exit" (test-bind-exit 10) 11)
   (test "cond" (test-r5rs-cond) #t)
   (test "case" (test-r5rs-case) #t)
   (test "and" (test-r5rs-and) #t)
   (test "or" (test-r5rs-or) #t)
   (test "let" (test-r5rs-let) #t)
   (test "let*" (test-r5rs-let*) #t)
   (test "letrec" (test-r5rs-letrec) #t)
   (test "do" (test-r5rs-do) '#(0 1 2 3 4)))


