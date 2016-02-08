;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/inline.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov  6 10:40:50 1992                          */
;*    Last change :  Sat Feb  6 07:41:14 2016 (serrano)                */
;*                                                                     */
;*    Des petits tests qui verifie que l'inlining se fait              */
;*    convenablement.                                                  */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module inline
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-inline)
	    (link-with x)
	    (gee-inline a b c d e f)
	    (foo-inline x)
	    (type-hux x)
	    (read-file-break-dsssl input-string)))

;*---------------------------------------------------------------------*/
;*    link-with ...                                                    */
;*    -------------------------------------------------------------    */
;*    A compilation bug introduced when optimization var, var          */
;*    let bindings (see file Inline/inline.scm, let-var construction). */
;*---------------------------------------------------------------------*/
(define (link-with scm-files)
   (let ((exp '(8)))
      (let ((foo exp))
	 (let ((p foo))
	    (if p
		(letrec ((try-120
			  (lambda (g-119)
			     (let ((res (if 8
					    (try-120 8)
					    p)))
				5
				res))))
		   (try-120 9))
		6)
	    6))))

;*---------------------------------------------------------------------*/
;*    foo ...                                                          */
;*---------------------------------------------------------------------*/
(define (foo a)
   (define (hux x)
      (let* ((formal (cadr x)))
	 formal))
   hux)
 
;*---------------------------------------------------------------------*/
;*    bar ...                                                          */
;*---------------------------------------------------------------------*/
(define bar
   (let ((a 1))
      (lambda ()
	 (set! a (+ 1 a))
	 (list a a a a))))
 
;*---------------------------------------------------------------------*/
;*    hux ...                                                          */
;*---------------------------------------------------------------------*/
(define (hux)
   (cadr (bar)))

;*---------------------------------------------------------------------*/
;*    loop ...                                                         */
;*---------------------------------------------------------------------*/
(define (loop x . y)
   (if (>fx x 1)
       (loop 1 y)
       y))

;*---------------------------------------------------------------------*/
;*    loop2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (loop2 n s1 s2 test)
   (if (=fx n 0)
       'done
       (loop2 (-fx n 1) s1 s2 (equal? s1 s2))))

;*---------------------------------------------------------------------*/
;*    Inline of exported recursive functions                           */
;*---------------------------------------------------------------------*/
(define (gee-inline a b c d e f)
   (gee-inline a b c d e f))

(define (foo-inline x)
   (gee-inline 1 2 3 4 5 6)
   (gee-inline 1 2 3 4 5 6))

;*---------------------------------------------------------------------*/
;*    Type result                                                      */
;*---------------------------------------------------------------------*/
(define (type-hux x) x)

(define-inline (type-foo::char x)
   (type-hux x))

(define (type-gee x::obj)
   ;; the inlining of (try-foo x) must produce a type error
   (try (type-foo x)
	(lambda (a b c d)
	   (a (+fx x 1)))))

;*---------------------------------------------------------------------*/
;*    read-file-break-dsssl ...                                        */
;*---------------------------------------------------------------------*/
(define (read-file-break-dsssl input-stream)
   (read-parts read input-stream))


(define (read-parts read-cmd input-stream)
   (let read-all ((term (read-cmd input-stream))
		  (l '()))
      (cond ((eof-object? term)
	     (reverse l))
	    (else
	     (read-all (read-cmd input-stream) (cons term l))))))

;*---------------------------------------------------------------------*/
;*    test-inline ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-inline)
   (test-module "inline" "inline.scm")
   (test "inline" ((foo 1) '(1 2 3 4)) 2)
   (test "type-rq" (hux) 2)
   (test "loop" (loop 2 2) '((2)))
   (test "loop" (loop2 10 2 2 #f) 'done)
   (test "type" (type-gee 4) 5))
