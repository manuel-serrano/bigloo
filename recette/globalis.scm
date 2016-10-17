;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/globalis.scm                 */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 11:30:18 1992                          */
;*    Last change :  Mon Oct 17 10:04:27 2016 (serrano)                */
;*                                                                     */
;*    Des tests qui globalisent des fonctions locales par la passe Glo */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module globalisation
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-globalisation)
	    (plante-2 x)
	    (plante-3 x)
	    (plante-4 x)
	    (_plante-4 x y)
	    (plante-k2 l)
	    (compile x)
	    (plante-5 x)
	    (plante-6 x e)
	    (plante-7))
   (option  (set! *inlining?* #f)))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1)
   (labels ((fun1 () 0)
	    (fun2 () (labels ((loop () (fun1)))
			(loop))))
      fun2))

;*---------------------------------------------------------------------*/
;*    test2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test2 x y z)
   (labels ((hux (a1)  x)
	    (bar (a1) (hux y))
	    (gee ()  (bar z)))
      gee))

;*---------------------------------------------------------------------*/
;*    test3 ...                                                        */
;*---------------------------------------------------------------------*/
(define test3 (labels ((lam_0 ()
			      (labels ((ignore ()
					       (let ((v 1))
						  (labels ((foo () v))
						     (foo)))))
				 (ignore))))
		 lam_0))

;*---------------------------------------------------------------------*/
;*    test4 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test4 a b)
   (labels ((foo (x) (set! x b) x))
      foo))

;*---------------------------------------------------------------------*/
;*    test5 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test5 a b . c)
   (labels ((foo (x y) (set! c y) y)
	    (bar (z) (foo 2 1))
	    (hux (t) (set! a (foo t t)) c))
      hux))

;*---------------------------------------------------------------------*/
;*    test6                                                            */
;*---------------------------------------------------------------------*/
(define (test6 . c)
   (let ((a2 0)
	 (b 0))
      (labels ((foo (x) (set! a2 x) a2)
	       (bar (y) (set! b y) (foo b)))
	 (bar 1)
	 foo)))

;*---------------------------------------------------------------------*/
;*    test7 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test7 x)
   (labels ((bar (a3) (hux a3))
	    (gee (b) bar)
	    (hux (c) x))
      bar
      gee))

;*---------------------------------------------------------------------*/
;*    Ici, il y a un prgm qui boucle mais qui est interressant car     */
;*    k2 se plante quand il essaye de le compiler                      */
;*---------------------------------------------------------------------*/
(define (plante-k2 l)
   (labels ((l1 (x) (labels ((l2 () (bar x (l3 (g)))))
                       (gee (l2) (l2))))
            (l3 (y) (labels ((l4 () (zee y (l1 (g)))))
                       (gee (l4) (l4)))))
      (l1 (g))))

(define (bar x y) (if (= x y) (+ x y) 0))
(define (gee x y) (+ x y))
(define (zee x y) (- x y))
(define (g) (if (= (gee 4 2) (zee 8 2)) 2 1))

;*---------------------------------------------------------------------*/
;*    plante-2 ...                                                     */
;*    -------------------------------------------------------------    */
;*    Ces exemples plantaient une version anterieur de bigloo          */
;*---------------------------------------------------------------------*/
(define (plante-2 c_1)
   (labels ((c1_13 () (c_1 (list 1))))
      (lar (lambda () (hux (lambda () (c1_13)))))))

;*---------------------------------------------------------------------*/
;*    plante-3                                                         */
;*---------------------------------------------------------------------*/
(define (plante-3  c_1)
   (labels ((c1_13 (x) (c_1 (c1_13 (list 1)))))
      (lar (lambda () (hux (lambda () (c1_13 1)))))))

(define (lar x)
   x)

(define (hux x)
   x)

;*---------------------------------------------------------------------*/
;*    plante-4 ...                                                     */
;*    -------------------------------------------------------------    */
;*    Encore un prgm qui ne se compilait pas correctement !!!          */
;*---------------------------------------------------------------------*/
(define (plante-4 a)
   (labels ((hux (z) foo)
	    (foo (x) x))
      (hux foo)))

;*---------------------------------------------------------------------*/
;*    _plante-4 ...                                                    */
;*---------------------------------------------------------------------*/
(define (_plante-4 x y)
   (+ x y))

;*---------------------------------------------------------------------*/
;*    Un prgm qui ne se compilait pas convenablement.                  */
;*---------------------------------------------------------------------*/
(define (compile exp)
  (begin
    (print 'compile)
    (print
      (begin
        ((labels
           ((tag-101 () '(52))
            (tag-102 () '())
            (tag-103 (fun args) '())
            (tag-104 (args) (compile-application)))
           (lambda (e-105)
             (begin
               (if (pair? e-105)
                 (let ((cdr-118 (cdr e-105)) (car-117 (car e-105)))
                   (if (eq? car-117 'module)
                     (if (eq? cdr-118 '())
                       (tag-101)
                       (let ((cdr-149 (cdr e-105)) (car-148 (car e-105)))
                         (labels
                           ((kap-152
                              ()
                              (let ((cdr-156 (cdr e-105))
                                    (car-155 (car e-105)))
                                (tag-104 cdr-156))))
                           (if (pair? car-148)
                             (let ((cdr-161 (cdr car-148))
                                   (car-160 (car car-148)))
                               (kap-152))
                             (if (vector? car-148)
                               (kap-152)
                               (tag-103 car-148 cdr-149))))))
                     (labels
                       ((kap-121
                          ()
                          (let ((cdr-127 (cdr e-105)) (car-126 (car e-105)))
                            (labels
                              ((kap-130
                                 ()
                                 (let ((cdr-134 (cdr e-105))
                                       (car-133 (car e-105)))
                                   (tag-104 cdr-134))))
                              (if (pair? car-126)
                                (let ((cdr-139 (cdr car-126))
                                      (car-138 (car car-126)))
                                  (kap-130))
                                (if (vector? car-126)
                                  (kap-130)
                                  (tag-103 car-126 cdr-127)))))))
                       (let ((cdr-141 (cdr e-105)) (car-140 (car e-105)))
                         (kap-121)))))
                 (if (vector? e-105) #f (tag-102))))))
         exp)))))

(define (compile-application)
  (begin
    (print 'compile-application)
    (print
      (begin
        (let ((aux 1))
	   (case aux
	      ((0) '())
	      (else '())))))))

;*---------------------------------------------------------------------*/
;*    encore une fonction qui ne se compilait pas.                     */
;*---------------------------------------------------------------------*/
(define plante-5
   (lambda (a4)
      (letrec ((x3 (lambda (x4)
		      (a4 0)
		      (lambda (c)
			 (labels ((foo () (lambda (x9) (x3 0))))
			    (foo))))))
	 x3)))

;*---------------------------------------------------------------------*/
;*    plante-6                                                         */
;*---------------------------------------------------------------------*/
(define (plante-6 x e)
   (labels ((loop ()
		  (labels ((foo () (labels ((gee () (loop)))
				      gee)))
		     (foo) x)))
      (loop)))

;*---------------------------------------------------------------------*/
;*    plante-7 ...                                                     */
;*---------------------------------------------------------------------*/
(define (plante-7)
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
	     (set! exp #unspecified)
	     (labels ((foo () (print exp)))
		(plante-7-bar foo)))))
      9))

(define (plante-7-bar f)
   (print (f)))

;*---------------------------------------------------------------------*/
;*    test8 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test8 str)
   (let ((g (regular-grammar ()
	       (#\)
		'())
	       (#\(
		(let* ((car (ignore))
		       (cdr (ignore)))
		   (cons car cdr)))
	       ((+ (out #\( #\)))
		(the-symbol))))
	 (p (open-input-string str)))
      (read/rp g p)))

;*---------------------------------------------------------------------*/
;*    static-application ...                                           */
;*---------------------------------------------------------------------*/
(define (static-application)
   (((lambda (a b) b)
     3
     (lambda (x) x)) 3))

;*---------------------------------------------------------------------*/
;*    test-globalisation ...                                           */
;*---------------------------------------------------------------------*/
(define (test-globalisation)
   (test-module "globalisation" "globalisation.scm")
   (test "globalisation.1" ((test1)) 0)
   (test "globalisation.2" (((test7 5) 6) 7) 5)
   (test "kapture.1" ((test2 1 2 3)) 1)
   (test "kapture.2" (test3) 1)
   (test "cell.1" ((test4 1 2) 3) 2)
   (test "cell.2" ((test5 1 2 3 4) 2) 2)
   (test "cell.3" ((test6 1 2 3 4) 2) 2)
   (test "rgc" (test8 "(foo(bar(gee)))") '(foo bar gee))
   (test "_" (_plante-4 5 6) 11)
   (test "ast" (static-application) 3))
