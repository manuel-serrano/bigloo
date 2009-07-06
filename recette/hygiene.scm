;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/hygiene.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 27 12:52:59 1998                          */
;*    Last change :  Mon Jun 29 17:03:29 2009 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Hygienic macro tests                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hygiene
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-hygien)))

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
    ((_ body ...)
     (begin
       body ...))))

(define (test-bind-exit val)
   (my-begin
    (bind-exit (return)
       (return (+ val 1))
       val)))

;*---------------------------------------------------------------------*/
;*    test-hygien ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-hygien)
   (test-module "hygiene" "hygien.scm")
   ;; let-syntax is broken (it should be re-implemented).
   #;(test "let-syntax.1"
	 (let-syntax ((when (syntax-rules ()
			       ((when test stmt1 stmt2 ...)
				(if test
				    (begin stmt1 stmt2 ...))))))
	    (let ((if #t))
	       (when if (set! if 'now))
	       if))
	 'now)
   (test "let-syntax.2"
	 (let ((val (let ((x 'outer))
		       (let-syntax ((m (syntax-rules () ((m) x))))
			  (let ((x 'inner))
			     (m))))))
	    (if (not (eq? val 'outer))
		(if (not *silent*)
		    (warning "let-syntax is not hygienic yet!!!"
			     "Hygien is not currently supported for LET-SYNTAX and LETREC-SYNTAX.")))
	    'outer)
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
   (test "bind-exit" (test-bind-exit 10) 11))


