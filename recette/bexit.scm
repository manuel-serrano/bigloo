;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/bexit.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 12 10:06:03 1992                          */
;*    Last change :  Thu Dec 22 19:55:31 2016 (serrano)                */
;*                                                                     */
;*    On test les trois sortes de `bind-exit'                          */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module bind-exit
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-bind-exit)
	    (bug-bexit-1 x)))

;*---------------------------------------------------------------------*/
;*    test0 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test0 n)
   (+ n
      (bind-exit (out)
	     (labels ((loop (m)
			    (if (= m 0)
				(out 1)
				(loop (- m 1)))))
		(loop n)
		2))))

;*---------------------------------------------------------------------*/
;*    test1                                                            */
;*---------------------------------------------------------------------*/
(define (test1 . l)
   (bind-exit (error)
	  (labels ((sum (l)
			(if (null? l)
			    0
			    (if (integer? (car l))
				(+ (car l) (sum (cdr l)))
				(error -1)))))
	     (sum l))))

;*---------------------------------------------------------------------*/
;*    test2                                                            */
;*---------------------------------------------------------------------*/
(define (fake-call-with-current-continuation f)
   (bind-exit (c) (f c)))

(define test2 (lambda l
	       (fake-call-with-current-continuation
		(lambda (error)
		   (labels ((sum (l)
 				 (if (null? l)
				     0
				     (if (integer? (car l))
					 (+ (car l) (sum (cdr l)))
					 (error -1)))))
		      (sum l))))))

;*---------------------------------------------------------------------*/
;*    test4 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test4 f)
   (let ((kapture 1))
      (bind-exit (c) (f kapture c))))

;*---------------------------------------------------------------------*/
;*    Un code qui ne se compilait pas (jusqu'a Bigloo1.6b):            */
;*---------------------------------------------------------------------*/
(define bug-bexit-1
   (lambda (x1)
      (labels ((!-d,try1015 (!-d,1008)
			    (if !-d,1008
				(bind-exit
				 (!-d,staticexit1010)
				 0)
				0)))
	 (!-d,try1015 0))))

;*---------------------------------------------------------------------*/
;*    test5 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test5 x)
   (let ((g 0))
      (try (if (string? x)
	       (test5 '(1 2 3))
	       (car x))
	   (lambda (a b c d)
	      (if (=fx g 0)
		  (begin
		     (set! g (+fx g 1))
		     (a #t))
		  (a #f))))))

;*---------------------------------------------------------------------*/
;*    *escape* ...                                                     */
;*---------------------------------------------------------------------*/
(define *escape* #unspecified)

;*---------------------------------------------------------------------*/
;*    A bug in Cfa make this function return #unspecified.             */
;*---------------------------------------------------------------------*/
(define (test6)
   (let ((f (bind-exit (ex)
	       (lambda () 345))))
      (f)))

;*---------------------------------------------------------------------*/
;*    test-stack-traces ...                                            */
;*---------------------------------------------------------------------*/
(define (test-stack-traces-main x)
   (car x)
   (bind-exit (exit)
      (test-stack-traces-foo x exit))
   (caddr (cddr x)))

(define (test-stack-traces-foo x exit)
   (cadr x)
   (bind-exit (stop)
      (test-stack-traces-bar x stop))
   (caddr (cdr x))
   (exit 1))

(define (test-stack-traces-bar x stop)
   (caddr x)
   (stop 2))

(define (test-stack-traces)
   (let ((l '(() (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)))
	 (old (bigloo-debug)))
      (bigloo-debug-set! 10)
      (let ((r (= (apply + (map (lambda (v) 
				   (try (begin (test-stack-traces-main v) 0)
					(lambda (a b c d)
					   (a 1))))
				l))
		  5)))
	 (bigloo-debug-set! old)
	 r)))
 
;*---------------------------------------------------------------------*/
;*    test-trace-stack ...                                             */
;*---------------------------------------------------------------------*/
(define (test-trace-stack)
   (try
      (begin
	 (with-output-to-string
	    (lambda ()
	       (bind-exit (exit)
		  (test-trace-stack-hux exit))
	       (display-trace-stack (get-trace-stack) (current-output-port))
	       (bind-exit (exit)
		  (test-trace-stack-hux exit))
	       (display-trace-stack (get-trace-stack) (current-output-port))))
	 #t)
      (lambda (e a b c)
	 (fprint (current-error-port) "+++ ERROR: " a " " b " -- " c)
	 (e #f))))

(define (test-trace-stack-hux x)
   (test-trace-stack-bar x))

(define (test-trace-stack-bar x)
   (x 3))

(define (test-trace-stack2)
   (let ((p (open-output-string)))
      (let loop ((i 3))
	 (when (> i 0)
	    (display-trace-stack (get-trace-stack) p)
	    (bind-exit (exit)
	       (test-trace-stack2-aux (list exit)))
	    (display-trace-stack (get-trace-stack) p)
	    (loop (- i 1)))))
   #t)

(define (test-trace-stack2-aux l)
   (bind-exit (esc)
      ((car l) #t)))

;*---------------------------------------------------------------------*/
;*    test-multiple-return ...                                         */
;*---------------------------------------------------------------------*/
(define (test-multiple-return)
   (bind-exit (return)
      (unwind-protect
	 (return 1111)
	 (return 2222))))

;*---------------------------------------------------------------------*/
;*    test-bexit-type ...                                              */
;*---------------------------------------------------------------------*/
(define (test-bexit-type::bool x)
   (bind-exit (exit)
      (exit x)))

;*---------------------------------------------------------------------*/
;*    test-bind-exit ...                                               */
;*---------------------------------------------------------------------*/
(define (test-bind-exit)
   (test-module "bind-exit" "bind-exit.scm")
   (test "goto bind-exit" (test0 4) 5)
   (test "simple bind-exit.1" (test1 1 2 4) 7)
   (test "simple bind-exit.2" (test1 1 'toto 4) -1)
   (test "fake-call-with-current-continuation.1" (test2 1 2 4) 7)
   (test "fake-call-with-current-continuation.2" (test2 1 'toto 4) -1)
   (test "kapture bind-exit" (test4 (lambda (x y) x)) 1)
   (test "try.1" (try (error 1 2 3) (lambda (a b c d) (a #t))) #t)
   (test "try.2" (begin
		  (try (error 1 2 3) (lambda (a b c d) (a #t)))
		  (try (test5 5)
		       (lambda (a b c d)
			  #f)))
	 #t)
   (test "cfa" (test6) 345)
   (test "unwind.1" (eval '(unwind-protect 10 10)) 10)
   (test "unwind.2" (eval '(bind-exit (exit) (unwind-protect (exit 10) 9))) 10)
   (test "unwind.3" (eval '(bind-exit (exit) (unwind-protect 10 9))) 10)
   (test "trace stack.1" (test-stack-traces) #t)
   (test "trace stack.2" (test-trace-stack) #t)
   (test "trace stack.3" (test-trace-stack2) #t)
   (test "multiple-return" (test-multiple-return) 2222)
   (test "bind-exit type" (test-bexit-type #t) #t))
	 
