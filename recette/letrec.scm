;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/letrec.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 19:18:37 1992                          */
;*    Last change :  Tue Apr 11 12:03:41 2017 (serrano)                */
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
;*    test-letrec*9 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-letrec*9)
   (define bar (list '(1 . 2)))
   (cdar bar))

;*---------------------------------------------------------------------*/
;*    test-letrec*10 ...                                               */
;*---------------------------------------------------------------------*/
(define (test-letrec*10 n)
   (letrec* ((u 1)
	     (a 0)
	     (foo (begin
		     (set! a (+ 1 a))
		     (lambda (x) (when (> x 0) (bar (- x u))))))
	     (t 1)
	     (bar (begin
		     (set! a (+ 1 a))
		     (lambda (x) (when (> x 0) (foo (- x t))))))
	     (z 4))
      (bar (+ n z))))

;*---------------------------------------------------------------------*/
;*    test-letrec*11 ...                                               */
;*---------------------------------------------------------------------*/
(define (test-letrec*11 n)
   (letrec* (
	     (foo (begin
		     (set! a 1)
		     (lambda (x) a)))
	     (a 10)
	     (bar (begin
		     (set! a (+ 1 a))
		     (lambda (x) a)))
	     )
      (+ (foo 1) a)))

;*---------------------------------------------------------------------*/
;*    test-letrec*11b ...                                              */
;*---------------------------------------------------------------------*/
(define (test-letrec*11b n)
   (letrec* ((foo (begin
		     (set! a 1)
		     (lambda (x) a)))
	     (a 10))
      (+ (foo 1) a)))

;*---------------------------------------------------------------------*/
;*    test-letrec*11c ...                                              */
;*---------------------------------------------------------------------*/
(define (test-letrec*11c n)
   (letrec* ((u 1)
	     (foo (begin
		     (set! a 1)
		     (lambda (x) a)))
	     (a 10)
	     (bar (begin
		     (set! a (+ 1 a))
		     (lambda (x) a)))
	     (z 4))
      (+ (foo 1) (bar 2))))

;*---------------------------------------------------------------------*/
;*    test-letrec*12 ...                                               */
;*---------------------------------------------------------------------*/
(define (test-letrec*12)
   (letrec* ((foo (lambda () a))
	     (a 10))
      (foo)))

;*---------------------------------------------------------------------*/
;*    test-letrec*13 ...                                               */
;*---------------------------------------------------------------------*/
(define (test-letrec*13)
   (define x (begin (set! v 0) 0))
   (define y (begin (set! v 1) 1))
   (define z y)
   v)

(define v 45)

;*---------------------------------------------------------------------*/
;*    test-letrec*14 ...                                               */
;*---------------------------------------------------------------------*/
(define (test-letrec*14 a)
   (letrec* ((u 1)
	     (foo (begin
		     (set! a 5)
		     4))
	     (bar (begin
		     (set! a 10)
		     (lambda (x) a)))
	     (z 4))
      a))

;*---------------------------------------------------------------------*/
;*    test-letrec*15 ...                                               */
;*---------------------------------------------------------------------*/
(define (test-letrec*15)
   (let ((g (regular-grammar ()
	       ((+ #\a) 1)
	       ((+ #\b) (ignore))
	       (else 2))))
      (call-with-input-string "bbbaaa"
	 (lambda (ip)
	    (read/rp g ip)))))

;*---------------------------------------------------------------------*/
;*    test-narrow ...                                                  */
;*    -------------------------------------------------------------    */
;*    This definition corresponds to the expansion of a letrec*        */
;*---------------------------------------------------------------------*/
(define (test-narrow)
   (let ((x #f)
	 (y #t))
      (set! x (lambda () y))
      (set! y 5)
      (x)))

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
   (test "letrec*.8" (test-letrec*8) 1)
   (test "letrec*.9" (test-letrec*9) 2)
   (test "letrec*.10" (test-letrec*10 3) #f)
   (test "letrec*.11" (test-letrec*11 3) 22)
   (test "letrec*.11b" (test-letrec*11b 3) 20)
   (test "letrec*.11c" (test-letrec*11c 3) 22)
   (test "letrec*.12" (test-letrec*12) 10)
   (test "letrec*.13" (test-letrec*13) 1)
   (test "letrec*.14" (test-letrec*14 4) 10)
   (test "letrec*.15" (test-letrec*15) 1)
   (test "narrow" (test-narrow) 5))
