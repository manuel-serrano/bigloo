;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/callcc.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 13:57:49 1992                          */
;*    Last change :  Mon Nov 11 08:36:25 2013 (serrano)                */
;*                                                                     */
;*    On test differents call/cc                                       */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module callcc
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-callcc)
	    (plante-1)))

;*---------------------------------------------------------------------*/
;*    cons-gd ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (cons-gd a d)
   `(let ((a ,a))
       (cons a ,d)))

;*---------------------------------------------------------------------*/
;*    plante-1 ...                                                     */
;*---------------------------------------------------------------------*/
(define (plante-1)
   (begin
      (set-exit
       (an-exit1013)
       (let ()
	  (push-exit! an-exit1013 1)
	  (let ((exp (let ((body1018 (lambda () 9)))
			(set-exit
			 (an-exit1025)
			 (let ()
			    (push-exit! an-exit1025 0)
			    (body1018))))))
	     (set! exp #unspecified))))
      9))
   
;*---------------------------------------------------------------------*/
;*    qnc-plante ...                                                   */
;*---------------------------------------------------------------------*/
(define (qnc-plante x)
   (labels ((f1 (a) (+fx a x)))
      (set! x (+fx x 1))
      f1))

;*---------------------------------------------------------------------*/
;*    list-length ...                                                  */
;*---------------------------------------------------------------------*/
(define list-length
   (lambda (obj)
      (call/cc
       (lambda (return)
	  (letrec ((r (lambda (obj)
			 (cond ((null? obj) 0)
			       ((pair? obj) (+ (r (cdr obj)) 1))
			       (else (return #f))))))
	     (r obj))))))

;*---------------------------------------------------------------------*/
;*    test-set ...                                                     */
;*---------------------------------------------------------------------*/
(define (test-set)
   (let (v)
      (let ((k (call/cc (lambda (x) (set! v x)))))
	 (if k
	     v
	     (lambda (x) x)))))

;*---------------------------------------------------------------------*/
;*    test2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test2 prefix)
   (call/cc (lambda (exit)
	       (if (not (string? prefix))
		   (exit #f)
		   (exit (mgensym prefix))))))
   
(define mgensym
   (let ((v -1))
      (lambda args
	 (set! v (+ 1 v))
	 (if (null? args)
	     (string->symbol (string-append "&" (integer->string v)))
	     (string->symbol (string-append (car args)
					    (integer->string v)))))))

;*---------------------------------------------------------------------*/
;*    test3 ...                                                        */
;*    -------------------------------------------------------------    */
;*    Cette fonction teste les interferences possibles entre le GC et  */
;*    call/cc                                                          */
;*---------------------------------------------------------------------*/
(define (test3) 
   (let ((s (make-string (* 1024 600))))
      (eval '(define resume (lambda (v) (display "not initialized"))))
      (eval '(define (callcc-foo n)
		(bind-exit (stop)
		   (define (loop f)
		      (call/cc (lambda (k)
				  (set! resume k)
				  (f)))
		      (loop f))
		   (loop (lambda () (begin
				       (set! n (- n 1))
				       (if (= n 0)
					   (stop n)
					   (resume n))))))))
      (eval '(callcc-foo 100))))

;*---------------------------------------------------------------------*/
;*    make-box ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-box value)
  (let ((box
         (call/cc
          (lambda (exit)
            (letrec
                ((behavior
                  (call/cc
                   (lambda (store)
                     (exit (lambda (msg . new)
                             (call/cc
                              (lambda (caller)
                                (case msg
                                  ((get) (store (cons (car behavior) caller)))
                                  ((set)
                                   (store (cons (car new) caller)) ) )))))))))
              ((cdr behavior) (car behavior)) ) ) ) ) )
    (box 'set value)
    box ) )

;*---------------------------------------------------------------------*/
;*    lambda-trace                                                     */
;*---------------------------------------------------------------------*/
(define (lambda-trace x)
   (let (r)
      (try (ccbar1 x)
	   (lambda (k a b c)
	      (set! r c)
	      (k #unspecified)))
      r))

(define (ccbar1 x)
   (car (call/cc (lambda (exit)
		    (ccbar2 exit x)))))

(define (ccbar2 f x)
   (f x))

;*---------------------------------------------------------------------*/
;*    test-callcc ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-callcc)
   (test-module "callcc" "callcc.scm")
   (test "call/cc(r4).1" (call/cc (lambda (exit)
				   (for-each (lambda (x)
						(if (negative? x)
						    (exit x)))
					     '(54 0 37 -3 245 19))
				   #t))
	 -3)
   (test "call/cc(r4).2" (list-length '(1 2 3 4)) 4)
   (test "call/cc(r4).3" (list-length '(a b . c)) #f)
   (test "call/cc(simple).1" (call/cc (lambda (k) 'a)) 'a)
   (test "call/cc(simple).2" (call/cc (lambda (k) (k 'a))) 'a)
   (test "call/cc(simple).3" (call/cc (lambda (k) (cons (k 'a) 1))) 'a)
   (when-call/cc (test-callcc2)))

;*---------------------------------------------------------------------*/
;*    test-stack-traces ...                                            */
;*---------------------------------------------------------------------*/
(define-expander lbind-exit
   (lambda (x e)
      (match-case x
	 ((lbind-exit (?exit) . ?body)
	  (e `(call/cc (lambda (,exit)
			  ,@body))
	     e)))))

(define (test-stack-traces-main x)
   (car x)
   (lbind-exit (exit)
      (test-stack-traces-foo x exit))
   (caddr (cddr x)))

(define (test-stack-traces-foo x exit)
   (cadr x)
   (lbind-exit (stop)
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
;*    test-callcc2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-callcc2)
   (test "call/cc(set!).1" ((test-set) #f) #f)
   (test "call/cc(set!).2" ((lambda (a)
			       (begin (set! a (call/cc (lambda (k) k)))
				      (a (lambda (x) 1))))
			    2)
	 1)
   (test "call/cc(set!).3" ((lambda (a b)
			       (begin (set! a (call/cc (lambda (k) k)))
				      (begin (set! b (+ 1 b))
					     (a (lambda (x) b)))))
			    3 0)
	 2)
   (test "call/cc(set!).4"
	 ((lambda (foo)
	     (begin
		(set! foo ((lambda (z)
			      (lambda (x)
				 ((lambda (y) (begin (set! z x) y))
				  z)))
			   'z))
		(cons-gd (foo (call/cc (lambda (k) ; store A and return Z
				       (k 'a))))
		      (cons-gd (foo (call/cc (lambda (k) ; store B, return A
					     (cons-gd 1 (k 'b)))))
			    (cons-gd (foo (call/cc (lambda (k)
						   (cons-gd 2 (k k)))))
				  (cons-gd ((lambda (x)
					    (if (symbol? x) 'ee (x 'e)))
					 (foo 'd))
					(cons-gd (foo 'f)
					      '())))))))
	  'foo)
	 '(z a d ee d))
   (test "call/cc(set!).5"
	 ((lambda (foo)
	     (begin
		(set! foo ((lambda (z)
			      (lambda (x u)
				 ((lambda (y) (begin (set! z x) y))
				  z)))
			   'z))
		(cons-gd (foo (call/cc (lambda (k)
				       (k 'a)))
			   1)
		      (cons-gd (foo (call/cc (lambda (k)
					     (cons-gd 1 (k 'b))))
				 2)
			    (cons-gd (foo (call/cc (lambda (k)
						   (cons-gd 2 (k k))))
				       3)
				  (cons-gd ((lambda (x)
					    (if (symbol? x) 'ee (x 'e)) )
					 (foo 'd 4))
					(cons-gd (foo 'f 5) '())))))))
	  'foo)
	 '(z a d ee d))
   (test "call/cc(set!).6" (test2 'toto) #f)
   (test "call/cc(set!).7" (test2 "TOTO") 'TOTO0)
   (test "call/cc(gc)" (test3) 0)
   (let ((box1 (make-box 33)))
      (test "box" (box1 'get) 33)
      (test "box" (begin (box1 'set 44) (box1 'get)) 44))
   (test "eval" (begin
		   (eval '(define (next-leaf-generator obj eot)
			     (letrec ((return #f)
				      (cont (lambda (x)
					       (recur obj)
					       (set! cont (lambda (x)
							     (return eot)))
					       (cont #f)))
				      (recur (lambda (obj)
						(if (pair? obj)
						    (for-each recur obj)
						    (begin
						       (call/cc
							(lambda (c)
							   (set! cont c)
							   (return obj))))))))
				(lambda ()
				   (call/cc
				    (lambda (ret)
				       (set! return ret) (cont #f)))))))

		   (eval '(define (leaf-eq? x y)
			     (let* ((eot (list 'eot))
				    (xf (next-leaf-generator x eot))
				    (yf (next-leaf-generator y eot)))
				(let loop ((x (xf)) (y (yf)))
				   (cond ((not (eq? x y)) #f)
					 ((eq? eot x) #t)
					 (else
					  (loop (xf) (yf))))))))

		   (eval '(leaf-eq? '(a a) '(a b))))
	 #f)
   (test "lambda-stack" (lambda-trace 1) 1)
   (test "cell-ref" ((qnc-plante 4) 5) 10)
   (test "call-with-values" (call-with-values (lambda ()
						 (call/cc (lambda (cont)
							     (cont 'x 'y))))
					      cons)
	 '(x . y))
   (test "stack traces" (test-stack-traces) #t))
