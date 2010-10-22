;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/macro.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 23 15:13:19 1992                          */
;*    Last change :  Fri Oct 22 16:25:28 2010 (serrano)                */
;*                                                                     */
;*    Des tests d'expanseurs.                                          */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module macro
   (import  (main "main.scm"))
   (include "test.sch")
   (static  (integer->char))
   (export  (test-macro)))

;*---------------------------------------------------------------------*/
;*    defun ...                                                        */
;*---------------------------------------------------------------------*/
(define-expander defun 
   (lambda (f e)
      (e `(define ,(cadr f) 
	     (begin
		,@(cddr f)
		1))
	 e)))

;*---------------------------------------------------------------------*/
;*    foo ...                                                          */
;*---------------------------------------------------------------------*/
(defun (foo x y)
   (+ x y))

;*---------------------------------------------------------------------*/
;*    integer->char ...                                                */
;*---------------------------------------------------------------------*/
(define (integer->char)
   1)

;*---------------------------------------------------------------------*/
;*    bar ...                                                          */
;*    -------------------------------------------------------------    */
;*    On verifie que les O-macros sont ecrasables par des variables    */
;*    locales                                                          */
;*---------------------------------------------------------------------*/
(define (bar read)
   (read))

;*---------------------------------------------------------------------*/
;*    Les formes interpretees                                          */
;*---------------------------------------------------------------------*/
(define-expander put
   (define a 4)
   (define b 6)
   (lambda (x e) ''dummy))

(put)

(define-macro (get)
   (+ a b))

(define eval-macro (get))

;*---------------------------------------------------------------------*/
;*    Une macro dans un case sur des integers (ca plantait jusqu'a     */
;*    la premiere version 1.6)                                         */
;*---------------------------------------------------------------------*/
(define-macro (case-test-macro x)
   x)

(define (case-macro x)
   (case (case-test-macro x)
      ((1) 1)
      ((2) 2)
      (else 3)))

;*---------------------------------------------------------------------*/
;*    Nested defines                                                   */
;*---------------------------------------------------------------------*/
(define-macro (define-glop a v)
   `(begin
       (current-date)
       (define ,a ,v)))

(define-macro (define-node sig v)
      `(define ,sig ,v))

(define (nested-define)
   (define x 1)
   (current-seconds)
   (define-node t 10)
   (define-glop y 20)
   (current-seconds)
   (+ x t y))

;*---------------------------------------------------------------------*/
;*    nested define 2                                                  */
;*---------------------------------------------------------------------*/
(define-macro (insert-code)
   '(begin (define tag3 (list 'foo))))

(define nested-define2 #f)

(insert-code)
   
(let ((x tag3))
   (begin
      (insert-code)
      (set! nested-define2 (eq? x tag3))))

;*---------------------------------------------------------------------*/
;*    nested define 3                                                  */
;*---------------------------------------------------------------------*/
(begin
   (define tag2 (list 'foo)))

(define nested-define3 #f)

(let ((x tag2))
   (begin
     (define tag2 (list 'foo)))
   (set! nested-define3 (eq? x tag2)))

;*---------------------------------------------------------------------*/
;*    nested define 4 ...                                              */
;*---------------------------------------------------------------------*/
(define tag1 (list 'foo))

(define nested-define4 #f)

(let ((x tag1))

   (define tag1 (list 'foo))

   (set! nested-define4 (eq? x tag1)))

(define-expander cond-expandi
   (lambda (x e)
      (match-case x
         ((?- ?- ((kwote else) . ?body))
          (e `(begin ,@body) e)))))

(define (nested-define5 x)
   (cond-expandi
      (toto '(print 'coucou))
      (else (define (foo x) (not x)) (foo x))))

;*---------------------------------------------------------------------*/
;*    test-macro ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-macro)
   (test-module "macro" "macro.scm")
   (test "expander" (foo 1 2) 1)
   (test "O-macro" (integer->char) 1)
   (test "O-macro" (bar (lambda () 1)) 1)
   (test "eval" eval-macro 10)
   (test "case-macro" (case-macro 2) 2)
   (test "nested define" (nested-define) 31)
   (test "nested define.2" nested-define2 #f)
   (test "nested define.3" nested-define3 #f)
   (test "nested define.4" nested-define4 #f)
   (test "nested define.5" (nested-define5 #t) #f))

 
