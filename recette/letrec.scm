;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/letrec.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 19:18:37 1992                          */
;*    Last change :  Mon Oct 18 08:33:25 2010 (serrano)                */
;*                                                                     */
;*    On test `letrec'                                                 */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module letrec
   (import  (main "main.scm"))
   (include "test.sch")
   (eval (export foo-loop))
   (export  (test-letrec)))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1 y)
   (letrec ((x (integer->string y))
	    (foo (lambda (string)
		    (string->symbol (string-append string x)))))
      foo))

;*---------------------------------------------------------------------*/
;*    plante1                                                          */
;*    -------------------------------------------------------------    */
;*    un test qui plantait a la compilation                            */
;*---------------------------------------------------------------------*/
(define (foo a)
   (letrec ((foo (lambda (x) (bar 0) (set! foo 8) 'done))
	    (bar (lambda (x) (if (= x 0)
				 'done
				 (foo x)))))
      (foo a)))

;*---------------------------------------------------------------------*/
;*    foo-loop ...                                                     */
;*---------------------------------------------------------------------*/
(define (foo-loop)
   (let ((f -)) (let f ((n (f 1))) n)))

;*---------------------------------------------------------------------*/
;*    bar-loop ...                                                     */
;*---------------------------------------------------------------------*/
(define (bar-loop n)
   (let string-append ((n n))
      (if (=fx n 0)
	  0
	  (+fx n (string-append (-fx n 1))))))

;*---------------------------------------------------------------------*/
;*    test-begin ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-begin)
   (let ((x 5))
      (begin (begin (begin)
		    (begin (begin (begin) (define foo (lambda (y) (bar x y)))
				  (begin)))
		    (begin))
	     (begin)
	     (begin)
	     (begin (define bar (lambda (a b) (+ (* a b) a))))
	     (begin))
      (begin)
      (begin (foo (+ x 3)))))

;*---------------------------------------------------------------------*/
;*    test-begin-eval ...                                              */
;*---------------------------------------------------------------------*/
(define (test-begin-eval)
   (eval
    '(let ((x 5))
	(begin (begin (begin)
		      (begin (begin (begin) (define foo (lambda (y) (bar x y)))
				    (begin)))
		      (begin))
	       (begin)
	       (begin)
	       (begin (define bar (lambda (a b) (+ (* a b) a))))
	       (begin))
	(begin)
	(begin (foo (+ x 3))))))

;*---------------------------------------------------------------------*/
;*    test-letrec*1 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*1)
   (letrec* ((x 1)
	     (f (lambda (y) (+ x y))))
      (f 3)))

;*---------------------------------------------------------------------*/
;*    test-letrec*2 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*2)
   (eval '(letrec* ((x 1)
		    (f (lambda (y) (+ x y))))
	   (f 3))))

;*---------------------------------------------------------------------*/
;*    test-letrec*3 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*3)
   (letrec* ((p (lambda (x)
		   (+ 1 (q (- x 1)))))
	     (q (lambda (y)
		   (if (zero? y)
		       0
		       (+ 1 (p (- y 1))))))
	     (x (p 5))
	     (y x))
      y))

;*---------------------------------------------------------------------*/
;*    test-letrec*4 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*4)
   (eval '(letrec* ((p (lambda (x)
			  (+ 1 (q (- x 1)))))
		    (q (lambda (y)
			  (if (zero? y)
			      0
			      (+ 1 (p (- y 1))))))
		    (x (p 5))
		    (y x))
	   y)))

;*---------------------------------------------------------------------*/
;*    test-letrec*5 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*5)
   (define (f) (- y x))
   (define x 1)
   (define y x)
   (define (g) (f))
   (g))

;*---------------------------------------------------------------------*/
;*    test-letrec*6 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*6)
   (eval '((lambda ()
	      (define (f) (- y x))
	      (define x 1)
	      (define y x)
	      (define (g) (f))
	      (g)))))

;*---------------------------------------------------------------------*/
;*    test-letrec*7 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*7)
   (let ((r 0))
      (define (print) (begin 1 2 3 4 #t))
      (letrec* ((x 1)
                (y x))
         (set! r y))	    
      (print)
      r))

;*---------------------------------------------------------------------*/
;*    test-letrec*8 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*8)
   (eval '(let ((r 0))
	   (define (print) (begin 1 2 3 4 #t))
	   (letrec* ((x 1)
		     (y x))
	      (set! r y))	    
	   (print)
	   r)))

;*---------------------------------------------------------------------*/
;*    test-letrec ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-letrec)
   (test-module "letrec" "letrec.scm")
   (test "letrec" ((test1 1) "TOTO") 'TOTO1)
   (test "letrec" (foo 10) 'done)
   (test "let loop.1" (foo-loop) -1)
   (test "let loop.2" (eval '(foo-loop)) -1)
   (test "let loop.3" (bar-loop 10) 55)
   (test "letrec.begin.1" (test-begin) 45)
   (test "letrec.begin.eval.1" (test-begin-eval) 45)
   (test "delay"  (procedure? (letrec ((foo (delay foo))) (force foo))) #t)
   (test "letrec*.1" (test-letrec*1) 4)
   (test "letrec*.2" (test-letrec*2) 4)
   (test "letrec*.3" (test-letrec*3) 5)
   (test "letrec*.4" (test-letrec*4) 5)
   (test "letrec*.5" (test-letrec*5) 0)
   (test "letrec*.6" (test-letrec*6) 0)
   (test "letrec*.7" (test-letrec*7) 1)
   (test "letrec*.8" (test-letrec*8) 1))
