;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/apply.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 10:58:26 1992                          */
;*    Last change :  Mon Jun  7 16:57:13 2010 (serrano)                */
;*                                                                     */
;*    On test differentes sortes d'apply                               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module apply
   (import  (main "main.scm")
	    (alias-aux "alias_aux.scm"))
   (extern  (macro c-dummy::obj (::obj . ::obj) ""))
   (java    (abstract-class c
	       (method static dummy::obj (::obj ::obj) "c_dummy")
	       "foo"))
   (include "test.sch")
   (eval    (export f33))
   (export  (test-apply)
	    (test-apply2 x)
	    (apply-dummy x y)))
 
;*---------------------------------------------------------------------*/
;*    gtest1 ...                                                       */
;*---------------------------------------------------------------------*/
(define gtest1
   (lambda (x y)
      (+ x y)))

;*---------------------------------------------------------------------*/
;*    gtest2 ...                                                       */
;*---------------------------------------------------------------------*/
(define (gtest2 . x)
   (+ (car x) (cadr x)))

;*---------------------------------------------------------------------*/
;*    gtest3 ...                                                       */
;*---------------------------------------------------------------------*/
(define (gtest3 x . y)
   (+ x (car y)))

;*---------------------------------------------------------------------*/
;*    gtest4 ...                                                       */
;*---------------------------------------------------------------------*/
(define (gtest4)
   'foo)

;*---------------------------------------------------------------------*/
;*    gtest4b ...                                                      */
;*---------------------------------------------------------------------*/
(define (gtest4b . x)
   'foo)

;*---------------------------------------------------------------------*/
;*    gtest5 ...                                                       */
;*---------------------------------------------------------------------*/
(define (gtest5)
   (lambda ()
      'foo))

;*---------------------------------------------------------------------*/
;*    gtest6 ...                                                       */
;*---------------------------------------------------------------------*/
(define (gtest6)
   (lambda x
      'foo))

;*---------------------------------------------------------------------*/
;*    ltest1 ...                                                       */
;*---------------------------------------------------------------------*/
(define (ltest1 a b)
   (labels ((foo (x y)
		 (+ x y)))
      (apply foo (list (+ 1 a) (+ 1 b)))))

;*---------------------------------------------------------------------*/
;*    ltest2 ...                                                       */
;*---------------------------------------------------------------------*/
(define (ltest2 a b)
   (labels ((foo (x y)
		 (+ x (+ y (+ a b)))))
      foo))

;*---------------------------------------------------------------------*/
;*    ltest3 ...                                                       */
;*---------------------------------------------------------------------*/
(define (ltest3 a)
   (labels ((foo (z . x)
		(let loop ((x x))
		   (if (null? x)
		       a
		       (+ (car x) (loop (cdr x)))))))
      foo))

;*---------------------------------------------------------------------*/
;*    extern-apply ...                                                 */
;*---------------------------------------------------------------------*/
(define (extern-apply x)
   (apply foo1 x))

;*---------------------------------------------------------------------*/
;*    apply-dummy ...                                                  */
;*    -------------------------------------------------------------    */
;*    Bigloo1.9 was unable to compile this extern apply form.          */
;*---------------------------------------------------------------------*/
(define (apply-dummy x y)
   (apply c-dummy (cons x y)))

;*---------------------------------------------------------------------*/
;*    test-apply2 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-apply2 x)
   (apply x (vector->list '#(43))))

;*---------------------------------------------------------------------*/
;*    f33 ...                                                          */
;*---------------------------------------------------------------------*/
(define (f33 s0 s1 s2 s3 s4 s5 s6 s7 s8 s9
             t0 t1 t2 t3 t4 t5 t6 t7 t8 t9
             u0 u1 u2 u3 u4 u5 u6 u7 u8 u9
             v0 v1 v2)
   (+ s0 s1 s2 s3 s4 s5 s6 s7 s8 s9
      t0 t1 t2 t3 t4 t5 t6 t7 t8 t9
      u0 u1 u2 u3 u4 u5 u6 u7 u8 u9
      v0 v1 v2))

;*---------------------------------------------------------------------*/
;*    test-apply ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-apply)
   (test-module "apply" "apply.scm")
   (test "extern apply" (extern-apply '(1)) 1)
   (test "gapply" (apply gtest1 '(1 3)) 4)
   (test "gapply" ((begin gtest1) 1 3) 4)
   (test "gapply" (apply gtest2 '(1 3)) 4)
   (test "gapply" ((begin gtest2) 1 3) 4)
   (test "gapply" (apply gtest3 '(1 3)) 4)
   (test "gapply" ((begin gtest3) 1 3) 4)
   (test "gapply" (apply (begin gtest1) '(1 3)) 4)
   (test "gapply" (apply (begin gtest2) '(1 3)) 4)
   (test "gapply" (apply (begin gtest3) '(1 3)) 4)
   (test "gapply" (apply gtest4 '()) 'foo)
   (test "gapply" (apply gtest4b '()) 'foo)
   (test "gapply" (apply (gtest5) '()) 'foo)
   (test "gapply" (apply (gtest6) '()) 'foo)
   (test "lapply" (ltest1 1 2) 5)
   (test "lapply" ((ltest2 2 3) 1 2) 8)
   (test "lapply" (apply (ltest2 2 3) (list 1 2)) 8)
   (test "lapply" ((ltest3 1) 0 2 3 4) 10)
   (test "lapply" (apply (ltest3 1) (list 0 2 3 4)) 10)
   (test "lapply" (apply (lambda (x y) (list x y)) '(1 2)) '(1 2))
   (test "napply" (apply cons 1 '(2)) '(1 . 2))
   (test "napply" (apply cons 1 2 '()) '(1 . 2))
   (test "aapply" (apply apply cons (list 1 2 '())) '(1 . 2))
   (test "mapply" (apply (lambda (z) z) 1 '()) 1)
   (test "mapply" (apply (lambda (z) z) '(1)) 1)
   (test "mapply" (apply (lambda (a z) z) '(1 2)) 2)
   (test "mapply" (apply (lambda (a z) z) 1 '(2)) 2)
   (test "mapply" (apply (lambda (a z) z) 1 2 '()) 2)
   (test "mapply" (apply (lambda (a b c z) z) '(1 2 3 4)) 4)
   (test "mapply" (apply (lambda (a b c z) z) 1 '(2 3 4)) 4)
   (test "mapply" (apply (lambda (a b c z) z) 1 2 '(3 4)) 4)
   (test "mapply" (apply (lambda (a b c z) z) 1 2 3 '(4)) 4)
   (test "mapply" (apply (lambda (a b c z) z) 1 2 3 4 '()) 4)
   (test "mapply" (apply (lambda (a b c d z) z) 1 2 3 4 '(5))5)
   (test "mapply" (apply (lambda (a b c d z) z) 1 2 3 4 5 '()) 5)
   (test "mapply" (apply (lambda (a b c d e z) z) 1 2 3 4 '(5 6)) 6)
   (test "mapply" (apply (lambda (a b c d e f z) z) 1 2 3 4 '(5 6 7)) 7)
   (test "mapply" (apply (lambda (a b . z) (car z)) 1 2 3 4 5 '(6 7)) 3)
   (test "mapply" (apply (lambda (a . z) (car z)) 1 2 3 4 '(5 6 7)) 2)
   (test "mapply" (apply (lambda (a b c d . z) (car z)) 1 2 3 4 '(5 6 7)) 5)
   (test "mapply" (apply (lambda (a b c d e . z) (car z)) 1 '(2 3 4 5 6 7)) 6)
   (test "mapply" (apply (lambda (a b c d e f . z) (car z)) 1 2 3 4 '(5 6 7)) 7)
   (test "mapply" (apply (lambda (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32) (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32))
			 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32))
	 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32))
   (test "mapply" (apply (lambda (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32)
			    (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32))
			 (apply (lambda (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32) (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32))
				(list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32)))
	 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32))
   (test "mapply" (apply (lambda (a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 . a32) (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31 a32))
			 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32))
	 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 (32)))
   (test "test-apply2" (test-apply2 list) '(43))
   (eval '(define v33 (let ((x (f33 0 1 2 3 4 5 6 7 8 9
				    10 11 12 13 14 15 16 17 18 19
				    20 21 22 23 24 25 26 27 28 29
				    30 31 32)))
			 x)))
   (test "eval33" (eval 'v33) 528))

	 



