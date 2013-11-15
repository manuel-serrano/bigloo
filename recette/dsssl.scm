;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/dsssl.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 31 09:21:59 1998                          */
;*    Last change :  Mon Nov 11 08:39:25 2013 (serrano)                */
;*    -------------------------------------------------------------    */
;*    DSSSL funcall tests                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dsssl
   (import  (main "main.scm")
	    (vararity "vararity.scm"))
   (include "test.sch")
   (export  (test-dsssl)
	    (foo ::int y #!optional z (zz 1) #!rest r #!key i (j 1))
	    (dsssl2 a #!optional b (c 'c) (d 'd))
	    (dsssl4 a #!key b (c 'c) (d 'd))
	    (dsssl6 #!optional (a 'a) b)
	    (dsssl7 #!optional (dsssl7 2) (z dsssl7))
	    (dsssl8 #!key (y 2) (z y))
	    (dsssl9 ::input-port ::output-port #!optional (s 1) (o -1))
	    (dsssl11 x y #!optional z #!key i (j 1) #!rest r)
	    (dsssl-lexical b c #!optional (=::procedure equal?))
	    (dt->sec a #!key (x #t)))
   (eval    (export dsssl2)
	    (export dsssl3)
	    (export dsssl3b)
	    (export dsssl4)
	    (export dsssl6)))

(define (foo x y #!optional z (zz 1) #!rest r #!key i (j 1))
   (let ((f (lambda (z y #!key (i 8)) (list z y i))))
      (labels ((g (a b #!rest l) (list a b l)))
	 (list x y z zz i: i j: j))))

(define (test-dsssl-eval)
   (eval '(define (dsssl x y #!optional z (zz 1) #!rest r #!key i (j 1))
	     (let ((f (lambda (z y #!key (i 8)) (list z y i))))
		(labels ((g (a b #!rest l) (list a b l)))
		   (list x y z zz i: i j: j)))))
   #t)

(define (dsssl-lexical b c #!optional (=::procedure equal?))
   (= b c))

(define (connect #!optional environment #!key (hostname "localhost"))
   (cons environment hostname))

;*---------------------------------------------------------------------*/
;*    dsssl2                                                           */
;*---------------------------------------------------------------------*/
(define (dsssl2 a #!optional b (c 'c) (d 'd))
   (list a b c d))

(define (dsssl2-test1)
   (list (dsssl2 11 22 33 44)
	 (dsssl2 11 22 33)
	 (dsssl2 11 22)
	 (dsssl2 11)))

(define (dsssl2-test1-eval)
   (eval '(list (dsssl2 11 22 33 44)
		(dsssl2 11 22 33)
		(dsssl2 11 22)
		(dsssl2 11))))

(define (dsssl2-test2 f)
   (list (f 11 22 33 44)
	 (f 11 22 33)
	 (f 11 22)
	 (f 11)))

(define (dsssl2-test3 f)
   (list (apply f '(11 22 33 44))
	 (apply f '(11 22 33))
	 (apply f '(11 22))
	 (apply f '(11))))

(define (dsssl2-test4 f . args)
   (apply f args))

(define (dsssl3-test1)
   (list (dsssl3 11 22 33 44)
	 (dsssl3 11 22 33)
	 (dsssl3 11 22)
	 (dsssl3 11)))

(define (dsssl3-test2)
   (list ((car (list dsssl3)) 11 22 33 44)
	 ((car (list dsssl3)) 11 22 33)
	 ((car (list dsssl3)) 11 22)
	 ((car (list dsssl3)) 11)))

(define (dsssl3b-test1)
   (list (dsssl3b 11 c: 33 b: 22 d: 44)
	 (dsssl3b 11 b: 22 c: 33)
	 (dsssl3b 11 b: 22)
	 (dsssl3b 11)))

(define (dsssl3b-test2)
   (list ((car (list dsssl3b)) 11 d: 44 b: 22 c: 33)
	 ((car (list dsssl3b)) 11 c: 33 b: 22)
	 ((car (list dsssl3b)) 11 b: 22)
	 ((car (list dsssl3b)) 11)))

;*---------------------------------------------------------------------*/
;*    dsssl4 ...                                                       */
;*---------------------------------------------------------------------*/
(define (dsssl4 a #!key b (d 'd) (c 'c))
   (list a b c d))

(define (dsssl4-test1)
   (list
    (dsssl4 11 c: 33 b: 22 d: 44)
    (dsssl4 11 b: 22 c: 33)
    (dsssl4 11 b: 22)
    (dsssl4 11)))

(define (dsssl4-test1-eval)
   (eval '(list
	   (dsssl4 11 c: 33 b: 22 d: 44)
	   (dsssl4 11 b: 22 c: 33)
	   (dsssl4 11 b: 22)
	   (dsssl4 11))))

(define c c:)
(define b b:)

(define (dsssl4-test1b)
   (list
    (dsssl4 11 d: 44 b: 22 c 33)
    (dsssl4 11 c 33 b 22)
    (dsssl4 11 b 22)
    (dsssl4 11)))

(define (dsssl4-test1b-eval)
   (eval '(let ((c c:)
		(b b:))
	     (list
	      (dsssl4 11 d: 44 b: 22 c 33)
	      (dsssl4 11 c 33 b 22)
	      (dsssl4 11 b 22)
	      (dsssl4 11)))))

(define (dsssl4-test2 f)
   (list
    (f 11 c: 33 b: 22 d: 44)
    (f 11 c: 33 b: 22 )
    (f 11 b: 22)
    (f 11)))

(define (dsssl4-test3 f)
   (list
    (apply f (list 11 b: 22 d: 44 c: 33))
    (apply f (list 11 c: 33 b: 22))
    (apply f (list 11 b: 22))
    (apply f (list 11))))

(define (dsssl4-test4 f . args)
   (apply f args))

;*---------------------------------------------------------------------*/
;*    dsssl5 ...                                                       */
;*---------------------------------------------------------------------*/
(define (dsssl5 ident
		#!key
		(version #unspecified)
		(format "raw")
		(filter #f)
		(delegate #f)
		(symbol-table '())
		(custom '())
		(info '()))
   (list ident version custom info format))

;*---------------------------------------------------------------------*/
;*    dsssl6                                                           */
;*---------------------------------------------------------------------*/
(define (dsssl6 #!optional (a 'a) b)
   (list a b))

(define (dsssl6-test1)
   (list (dsssl6 11 22)
	 (dsssl6 11)
	 (dsssl6)))

(define (dsssl6-test1-eval)
   (eval '(list (dsssl6 11 22)
		(dsssl6 11)
		(dsssl6))))

(define (dsssl6-test2 f)
   (list (f 11 22)
	 (f 11)
	 (f)))

(define (dsssl6-test3 f)
   (list (apply f '(11 22))
	 (apply f '(11))
	 (apply f '())))

(define (dsssl6-test4 f . args)
   (apply f args))

;*---------------------------------------------------------------------*/
;*    dsssl6b ...                                                      */
;*---------------------------------------------------------------------*/
(define dsssl6b dsssl6)

(define (dsssl6b-test1)
   (list (dsssl6b 11 22)
	 (dsssl6b 11)
	 (dsssl6b)))

(define (dsssl6b-test2 f)
   (list (f 11 22)
	 (f 11)
	 (f)))

(define (dsssl6b-test3 f)
   (list (apply f '(11 22))
	 (apply f '(11))
	 (apply f '())))

(define (dsssl6b-test4 f . args)
   (apply f args))

;*---------------------------------------------------------------------*/
;*    dsssl7 ...                                                       */
;*---------------------------------------------------------------------*/
(define (dsssl7 #!optional (dsssl7 2) (z dsssl7))
   z)

;*---------------------------------------------------------------------*/
;*    dsssl8 ...                                                       */
;*---------------------------------------------------------------------*/
(define (dsssl8 #!key (y 2) (z y))
   z)

;*---------------------------------------------------------------------*/
;*    dsssl9 ...                                                       */
;*---------------------------------------------------------------------*/
(define (dsssl9 ip::input-port op::output-port #!optional (s 1) (o -1))
   (+fx s o))

;*---------------------------------------------------------------------*/
;*    dsssl11 ...                                                      */
;*---------------------------------------------------------------------*/
(define (dsssl11 x y #!optional z #!key i (j 1) #!rest r)
    (list x y z i: i j: j r))

;*---------------------------------------------------------------------*/
;*    DSSSL-mac ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (DSSSL-mac x)
   x)

;*---------------------------------------------------------------------*/
;*    dt->sec ...                                                      */
;*---------------------------------------------------------------------*/
(define (dt->sec a #!key (x #t))
   (date->seconds x))

;*---------------------------------------------------------------------*/
;*    dsssl-bug ...                                                    */
;*---------------------------------------------------------------------*/
(define (dsssl-bug #!key (foo::bool #t))
   foo)

;*---------------------------------------------------------------------*/
;*    test-dsssl ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-dsssl)
   (test-module "dsssl" "dsssl.scm")
   (test "dsssl" (foo 1 2 3 4 i: 5) '(1 2 3 4 i: 5 j: 1))
   (test "dsssl" (foo 1 2 3 4 i: 5 j: 3) '(1 2 3 4 i: 5 j: 3))
   (test "dsssl" ((lambda (x y #!optional z #!rest r #!key i (j 1))
		     (list x y z i: i j: j))
		  3 4 5 i: 6 i: 7)
	 '(3 4 5 i: 6 j: 1))
   (test "dsssl-eps" ((lambda (x y #!optional z #!rest r #!key i (j (DSSSL-mac 1)))
			 (list x y z i: i j: j))
		      3 4 5 i: 6 i: 7)
	 '(3 4 5 i: 6 j: 1))
   (test "eval" (test-dsssl-eval) #t)
   (test "eval" (eval '(dsssl 1 2 3 4 i: 5)) '(1 2 3 4 i: 5 j: 1))
   (test "eval-eps" (eval '(begin
			      (define-macro (DSSSL-mac x)
				 x)
			      ((lambda (x y #!optional z #!rest r #!key i (j (DSSSL-mac 1)))
				  (list x y z i: i j: j))
			       3 4 5 i: 6 i: 7)))
	 '(3 4 5 i: 6 j: 1))
   (test "dsssl" (eval '((lambda (x y #!optional z #!rest r #!key i (j 1))
			    (list x y z i: i j: j))
			 3 4 5 i: 6 i: 7))
	 '(3 4 5 i: 6 j: 1))
   (test "lexical" (dsssl-lexical '(1 2) (list 1 2)) #t)
   (test "lexical.2" (let ((equal? (lambda (a b) #f)))
			(dsssl-lexical '(1 2) (list 1 2)))
	 #t)
   (test "lexical.3" (let ((equal? (lambda (a b) #f)))
			((car (list dsssl-lexical)) '(1 2) (list 1 2)))
	 #t)
   (test "optional+key" (connect 3) '(3 . "localhost"))
   (test "optional+key" (connect 3 hostname: "hueco") '(3 . "hueco"))
   (test "optional+rest+key"
	 (eval '((lambda (x y #!optional z #!rest r #!key i (j 1))
		    (list x y z r: r i: i j: j))
		 3 4 5 6 i: 6))
	 '(3 4 5 r: (6 i: 6) i: 6 j: 1))
   (test "optional+rest+key"
	 (eval '((lambda (x y #!optional z #!rest r #!key i (j 1))
		    (list x y z r: r i: i j: j))
		 3 4 5 6 7 8 9 i: 6))
	 '(3 4 5 r: (6 7 8 9 i: 6) i: 6 j: 1))
   (test "ucs2-char" (ucs2->integer (integer->ucs2 (char->integer #\a)))
	 (char->integer #\a))
   (test "dsssl2.1" (dsssl2-test1)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl2-eval.1" (dsssl2-test1-eval)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl2.2" (dsssl2-test2 dsssl2)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl2.2b" (dsssl2-test2 (car (list dsssl2)))
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl2.3" (dsssl2-test3 dsssl2)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl2.3b" (dsssl2-test3 (car (list dsssl2)))
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.1" (dsssl3-test1)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.2" (dsssl2-test2 dsssl3)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.2b" (dsssl2-test2 (car (list dsssl3)))
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.3" (dsssl2-test3 dsssl3)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.3" (dsssl2-test4 dsssl3 11 22 33 44)
	 '(11 22 33 44))
   (test "dsssl3.3" ((car (list dsssl2-test4)) (car (list dsssl3)) 11 22 33 44)
	 '(11 22 33 44))
   (test "dsssl3.3b" (dsssl2-test3 (car (list dsssl3)))
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.4" (dsssl3-test2)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.5" (dsssl3b-test1)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.6" (dsssl3b-test2)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl3.5" (dsssl3b-test2)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4.1" (dsssl4-test1)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4-eval.1" (dsssl4-test1-eval)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4.1b" (dsssl4-test1b)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4-eval.1b" (dsssl4-test1b-eval)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4.2" (dsssl4-test2 dsssl4)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4.2b" (dsssl4-test2 (car (list dsssl4)))
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4.3" (dsssl4-test3 dsssl4)
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4.3" (dsssl4-test4 dsssl4 11 b: 22 d: 44 c: 33)
	 '(11 22 33 44))
   (test "dsssl4.3" ((car (list dsssl4-test4)) (car (list dsssl4)) 11 b: 22 d: 44 c: 33)
	 '(11 22 33 44))
   (test "dsssl4.3b" (dsssl4-test3 (car (list dsssl4)))
	 '((11 22 33 44)
	   (11 22 33 d)
	   (11 22 c d)
	   (11 #f c d)))
   (test "dsssl4-error" (with-handler
			   (lambda (e)
			      56)
			   ((car (list dsssl4)) 1 z: 23))
	 56)
   (test "dsssl5.1" (dsssl5 'bootstrap :version "18")
	 '(bootstrap "18" () () "raw"))
   (test "dsssl5.2" ((car (list dsssl5)) 'bootstrap :version "18")
	 '(bootstrap "18" () () "raw"))
   (test "dsssl6.1" (dsssl6-test1)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6-eval.1" (dsssl6-test1-eval)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6.2" (dsssl6-test2 dsssl6)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6.2b" (dsssl6-test2 (car (list dsssl6)))
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6.3" (dsssl6-test3 dsssl6)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6.3b" (dsssl6-test3 (car (list dsssl6)))
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6.4" ((car (list dsssl6-test4)) (car (list dsssl6)) 1 2)
	 '(1 2))
   (test "dsssl6b.1" (dsssl6b-test1)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6b.2" (dsssl6b-test2 dsssl6b)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6b.2b" (dsssl6b-test2 (car (list dsssl6b)))
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6b.3" (dsssl6b-test3 dsssl6b)
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6b.3b" (dsssl6b-test3 (car (list dsssl6b)))
	 '((11 22)
	   (11 #f)
	   (a #f)))
   (test "dsssl6b.4" ((car (list dsssl6b-test4)) (car (list dsssl6b)) 1 2)
	 '(1 2))
   (test "dsssl7.1" (dsssl7 43) 43)
   (test "dsssl7.2" ((car (list dsssl7)) 43) 43)
   (test "dsssl7.3" (apply (car (list dsssl7)) '(43)) 43)
   (test "dsssl8.1" (dsssl8 y: 44) 44)
   (test "dsssl8.2" ((car (list dsssl7)) y: 44) 44)
   (test "dsssl8.3" (apply (car (list dsssl7)) '(y: 44)) 44)
   (test "dsssl9.1" (dsssl9 (current-input-port) (current-output-port)) 0)
   (test "dsssl9.2" ((vector-ref (vector dsssl9) 0)
		     (current-input-port)
		     (current-output-port))
	 0)
   (test "keyword.1" (eq? :foo foo:) #t)
   (test "keyword.2" (eq? :foo bar:) #f)
   (test "keyword.3" (eq? :foo :Foo) #f)
   (test "keyword.4" (eq? (string->keyword "foo") foo:) #t)
   (test "keyword.5" (eq? (string->keyword "foo") :foo) #t)
   (test "keyword.6" (eq? (string->keyword "foo:") foo:) #f)
   (let* ((dt (current-date))
	  (res (date->seconds dt)))
      (test "keyword.7" ((if (> res 0) dt->sec list) 1 x: dt) res))
   (test "keyword.8" (dsssl-bug :foo #f) #f)
   (test "dssss11.1" (dsssl11 3 4 5 i: 6 i: 7 8 9)
	 '(3 4 5 i: 6 j: 1 (8 9)))
   (test "dssss11.2" (apply dsssl11 '(3 4 5 i: 6 i: 7 8 9))
	 '(3 4 5 i: 6 j: 1 (8 9)))
   (test "dssss11.3" ((cadr (list dsssl9 dsssl11)) 3 4 5 i: 6 i: 7 8 9)
		      '(3 4 5 i: 6 j: 1 (8 9))))
     
