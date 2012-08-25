;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/define.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  7 16:20:22 1993                          */
;*    Last change :  Fri Aug 24 17:02:55 2012 (serrano)                */
;*                                                                     */
;*    On test les `define's internes                                   */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module define
   (import  (main "main.scm")
	    (fun  module "module.scm"))
   (include "test.sch")
   (export  (test-define)))

;*---------------------------------------------------------------------*/
;*    aliasing                                                         */
;*---------------------------------------------------------------------*/
(define (foo1 x) (+fx x 1))
(define gee1 foo1)

(define (foo2 . x) (+fx (car x) 1))
(define gee2 foo2)

(define (foo3 x . y) (apply + (cons x y)))
(define gee3 foo3)

(define (foo4) 5)
(define gee4 foo4)

(define (foo5 x) (+fx x 1))
(define gee5 foo5)
(set! foo5 (lambda (y) 9))

(define-inline (foo6 x)
   (define (foo-inner x)
      x)
   (foo-inner x))

;*---------------------------------------------------------------------*/
;*    typed inner define                                               */
;*---------------------------------------------------------------------*/
(define (typed-define y)
   (define (inner::int x) x)
   (inner y))

;*---------------------------------------------------------------------*/
;*    n-ary inner define                                               */
;*---------------------------------------------------------------------*/
(define (n-ary-define y)
   (define (inner x . y)
      (cons x y))
   (inner y y))

;*---------------------------------------------------------------------*/
;*    n-ary typed inner define                                         */
;*---------------------------------------------------------------------*/
(define (n-ary-typed-define y)
   (define (inner::pair x . y) (cons x y))
   (inner y y))

;*---------------------------------------------------------------------*/
;*    untyped inner define ...                                         */
;*---------------------------------------------------------------------*/
(define (set-derives)
  (define dset  (make-vector 10 -1))
  dset)

;*---------------------------------------------------------------------*/
;*    test expand                                                      */
;*---------------------------------------------------------------------*/
(define-macro (mk-hwdebug-available)
   `(begin
       (define hwdebug 7)))
   
(define (texpand)
   (mk-hwdebug-available)
   8)

;*---------------------------------------------------------------------*/
;*    inner-print                                                      */
;*---------------------------------------------------------------------*/
(define (inner-print)
   (define (print) (begin 1 2 3 4 #t))
   (print))

;*---------------------------------------------------------------------*/
;*    test-define ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-define)
   (test-module "define" "define.scm")
   (test "internal.1" (test1 1) 1)
   (test "internal.2" (test3 1) 1)
   (test "internal.3" (test4 1) 1)
   (test "internal.4" (test5 (lambda (x)
				(define (bar x)
				   (define (hux x)
				      x)
				   (hux x))
				(bar x))) 1)
   (test "internal.5" (test5 (lambda (x)
				(define (bar x)
				   x)
				(bar x))) 1)
   (test "function" (procedure? fun) #t)
   (test "alias.1" (gee1 5) 6)
   (test "alias.2" (gee2 5) 6)
   (test "alias.3" (gee3 5 6) 11)
   (test "alias.4" (gee4) 5)
   (test "alias.5" (gee5 5) 6)
   (test "alias.6" (foo5 5) 9)
   (test "inline" (foo6 6) 6)
   (test "inner.1" (typed-define 5) 5)
   (test "inner.2" (n-ary-define 5) '(5 5))
   (test "inner.3" (n-ary-typed-define 5) '(5 5))
   (test "inner.4" (vector? (set-derives)) #t)
   (test "inner.5" (inner-print) #t) 
   (test "expand" (texpand) 8))

;*---------------------------------------------------------------------*/
;*    Une forme top-level                                              */
;*---------------------------------------------------------------------*/
(lambda (x)
   (define (bar x)
      x)
   (bar x))
 
(lambda (x)
   (define (bar x)
      (define (hux x)
	 x)
      (hux x))
   (bar x))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define test1
   (lambda (x)
      (define (bar x)
	 x)
      (bar x)))

;*---------------------------------------------------------------------*/
;*    test3 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test3 x)
   (define (bar x)
      x)
   (bar x))

;*---------------------------------------------------------------------*/
;*    test4 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test4 x)
   (define (bar x)
      (define (hux x)
	 x)
      (hux x))
   (bar x))

;*---------------------------------------------------------------------*/
;*    test5 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test5 f)
   (f 1))


