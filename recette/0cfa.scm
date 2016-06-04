;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/0cfa.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  7 11:13:48 1993                          */
;*    Last change :  Sat Jun  4 07:22:11 2016 (serrano)                */
;*                                                                     */
;*    Quelques tests sur la 0cfa                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module 0cfa
   (import  (main "main.scm")
	    (module "module.scm")
	    (cfa2 "cfa2.scm"))
   (include "test.sch")
   (export  (test-0cfa)
	    (cfa-dataflow-error ::obj ::procedure)
	    sc-vm))

(define-macro (O4)
   (set! *optim* 4)
   (set! *unsafe-type* #t)
   (set! *unsafe-eval* #t)
   (set! *unsafe-range* #t)
   '#unspecified)

(O4)

;*---------------------------------------------------------------------*/
;*    Un test qui ne se compilait pas                                  */
;*---------------------------------------------------------------------*/
(define (foo x)
   (begin 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9
	  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
   (labels ((bar () gee1)
            (gee1 (y) (+fx y 1)))
      ((bar) 5)))

(foo 1)

;*---------------------------------------------------------------------*/
;*    set-car ...                                                      */
;*---------------------------------------------------------------------*/
(define (set-car)
   (let ((l (cons 1 2)))
      (let ((y (car l)))
	 (set-car! l '())
	 (integer? (car l)))))

;*---------------------------------------------------------------------*/
;*    extra-light ...                                                  */
;*---------------------------------------------------------------------*/
(define (foo-of-extra-light f)
   (begin 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9
	  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9)
   (f))

(define (extra-light x)
   (labels ((f1 () x)
	    (f2 () (f1)))
      (foo-of-extra-light f2)))

;*---------------------------------------------------------------------*/
;*    Va args in -O4 mode ...                                          */
;*---------------------------------------------------------------------*/
(define va-args
    (lambda (name . default)
       ((lambda (scan)
           (begin
	      (set! scan
		    (lambda (i)
		       (let ((class i))
			  (if (pair? default)
			      (let ((fun (car default)))
				 (fun name))))))
	      (scan 1)))
	(unspecified))))


(define (va-args-main)
   (let ((gee (lambda (name) (begin #t))))
      (va-args 0 gee)))

;*---------------------------------------------------------------------*/
;*    parse ...                                                        */
;*---------------------------------------------------------------------*/
(define (parse iport)
   (let ((body1017 (lambda () (let ((lalr::procedure fun))
				 (let ((v (lalr 4)))
				    v))))
	 (result1019 '()))
      (set! result1019 (body1017))))

;*---------------------------------------------------------------------*/
;*    X-bug ...                                                        */
;*---------------------------------------------------------------------*/
(define (X-bug x)
   (let ((f (lambda (y) x)))
      (cfa2 f)))

;*---------------------------------------------------------------------*/
;*    Another compiler error                                           */
;*---------------------------------------------------------------------*/
(define my-fetch-byte
   (lambda (max-pc prgm)
      0 1 2 3 4 5 6 7 8 9
      0 1 2 3 4 5 6 7 8 9
      0 1 2 3 4 5 6 7 8 9
      (if (=fx max-pc 0)
          #f
          (let ((bug (vector-ref prgm 0)))
             bug))))

(define (sc-vm . action)
   (+fx 1 (my-fetch-byte 1 (cdr action))))

;*---------------------------------------------------------------------*/
;*    A compilation bug appeared with dataflow analysis                */
;*---------------------------------------------------------------------*/
(define (cfa-dataflow-error x e)
   ((labels ((TAG-1389
	      (x y)
	      (if (if (fixnum? x) (fixnum? y) #f)
		  (-fx x y)
		  0)))
       (lambda (E-1391)
	  (TAG-1389 (car E-1391) (car E-1391))))
    x))

;*---------------------------------------------------------------------*/
;*    test-l-procedure ...                                             */
;*---------------------------------------------------------------------*/
(define (test-l-procedure argv)
   (let ((l (list "juste" "to" 'be 'sure 'that "there" 'is 'no
		  'inlining 'in 'this 'function)))
      (let ((f (bar argv)))
	 (f 3 4 5))))


(define (bar argv)
   (cond
      ((null? (cdr argv))
       (lambda (a b c) (cons argv 1)))
      ((null? (cddr argv))
       (lambda (a b c) (cons argv 2)))
      ((null? (cdddr argv))
       (lambda (a b c) (cons argv 3)))
      (else
       (lambda (a b c) (cons argv 4)))))

(define (dataflow-test x)
   (let ((y #unspecified))
      (set! y (list 1 2))
      (set! y '())
      (let loop ((x x))
         (if (= x 0)
             (car y)
             (begin
                (set! y (cons x y))
                (loop (- x 1)))))))

;*---------------------------------------------------------------------*/
;*    test-l-procedure-types                                           */
;*---------------------------------------------------------------------*/
(define (test-l-procedure-type x)
   (let ((f1 (lambda (x v)
		(/ (vector-length (vector x)) 10)))
	 (f2 (lambda (x v)
		(vector-length (vector x x)))))
      (case x
	 ((1) (gee f1 x))
	 ((2) (gee f2 x))
	 (else (gee f3 x)))))

(define (f3::pair x v)
   (list (vector-length (vector x x x))))

(define (gee f x)
   (f x (vector 1 2 3)))

;*---------------------------------------------------------------------*/
;*    test-l-procedure-types2                                          */
;*---------------------------------------------------------------------*/
(define (test-l-procedure-type2-caller1 d::uchar p)
   (let ((l (list "juste" "to" 'be 'sure 'that "there" 'is 'no
	       'inlining 'in 'this 'function)))
      (p d)))

(define (test-l-procedure-type2-caller2 d::bchar p)
   (let ((l (list "juste" "to" 'be 'sure 'that "there" 'is 'no
	       'inlining 'in 'this 'function)))
      (p d)))

(define (test-l-procedure-type2 d0)
   (let ((prac (lambda (d) (char=? d0 d)))
	 (proc (lambda (d) (char=? d0 d)))
	 (pric (lambda (d) (char=? d0 d))))
      (when (test-l-procedure-type2-caller2 d0 prac)
	 (when (test-l-procedure-type2-caller2 d0 pric)
	    (when (test-l-procedure-type2-caller1 d0 pric)
	       (test-l-procedure-type2-caller1 d0 proc))))))

;*---------------------------------------------------------------------*/
;*    test-0cfa ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-0cfa)
   (test-module "0cfa" "0cfa.scm")
   (test "set-car!" (set-car) #f)
   (test "extra-light" (extra-light 1) 1)
   (test "va-args" (va-args-main) #t)
   (test "funcall" (parse #t) #unspecified)
   (test "X" (X-bug 4) 4)
   (let ((l '(1)))
      (test "light" (test-l-procedure l) (cons l 1)))
   (let ((l '(1 2)))
      (test "light" (test-l-procedure l) (cons l 2)))
   (test "dataflow-test" (dataflow-test 10) 1)
   (test "light-typed.1" (test-l-procedure-type 1) .1)
   (test "light-typed.2" (test-l-procedure-type 2) 2)
   (test "light-typed.3" (test-l-procedure-type 3) '(3))
   (test "light-typed.4" (test-l-procedure-type2 #\a) #t))
