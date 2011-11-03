;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/sqic.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 14:27:18 1992                          */
;*    Last change :  Thu Nov  3 14:19:38 2011 (serrano)                */
;*                                                                     */
;*    On fait quelque tests qui portent sur la phase sqic->c.          */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module sqic
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-sqic)
	    (filter@types x)
	    (2-171-concat_228@baltree x1 x2)
	    (bug-integ-1)
	    (stupid)
	    (dummy)
	    (generic-write x)
	    (generic-write-2 x)))

;*---------------------------------------------------------------------*/
;*    generic-write ...                                                */
;*    -------------------------------------------------------------    */
;*    A bug in the pretty-printer.                                     */
;*---------------------------------------------------------------------*/
(define (generic-write indent-general)
   (letrec ((pp-comment (lambda () (labels ((tag-101 () (begin
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   0 1 2 3 4 5 6 7
							   (pp-general))))
				      (tag-101))))
	    (pp-general (lambda () (begin
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      indent-general)))
	    (pp-expr	(lambda () (begin
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      0 1 2 3 4 5 6 7
				      (let ((proc pp-comment))
					 (pp-general))))))
      pp-expr))

;*---------------------------------------------------------------------*/
;*    Le meme mais en plus simple                                      */
;*---------------------------------------------------------------------*/
(define (generic-write-2 x)
   (labels ((gee () (labels ((hux () (lee)))
		       (hux)))
	    (lee () x))
      (lee)
      gee))

;*---------------------------------------------------------------------*/
;*    foo ...                                                          */
;*---------------------------------------------------------------------*/
(define (foo y)
   (let ((x 0))
      (labels ((foo (a) (set! x a))
	       (bar (b) (if (null? b)
			    (set! x 3)
			    (cons x (bar (cdr b))))))
	 (set! x 1)
	 (foo 2)
	 (bar '(1 2 3))
	 x)))

;*---------------------------------------------------------------------*/
;*    bar ...                                                          */
;*---------------------------------------------------------------------*/
(define (bar a)
   (labels ((gee (x)
		 (set! a 4)
		 a))
      (gee 3)
      (gee 3)
      a))
		 
;*---------------------------------------------------------------------*/
;*    Une fonction qui ne se compile pas avec la version 1.1           */
;*---------------------------------------------------------------------*/
(define (f x)
   (labels ((foo (x) (bar x))
	    (bar (x) x)
	    (gee (x) (if x
			 (foo x)
			 (bar x))))
      (foo (gee x))))

(f 1)

;*---------------------------------------------------------------------*/
;*    Une fonction qui ne se compile pas avec la version 1.2           */
;*---------------------------------------------------------------------*/
(define (type-of)
  (begin
    (hux ((labels
            ((tag-103 () #f)
             (tag-102 () (type-unification 1 2))
             (tag-101 (body) #f))
            (lambda (e-104)
              (begin
                (if (pair? e-104)
                  (labels
                    ((kap-108
                       ()
                       (if 1
                         (if 1 (if 1 (tag-101 1) (tag-103)) (tag-103))
                         (labels
                           ((kap-112 () (tag-102)))
                           (if 4 (kap-112) (if 3 (kap-112) (tag-103)))))))
                    (if 4
                      (kap-108)
                      (if 4
                        (kap-108)
                        (labels
                          ((kap-121 () (tag-102)))
                          (if 7 (kap-121) (if 9 (kap-121) (tag-103)))))))
                  (tag-103)))))
          exp))))

(define (type-unification t1 t2)
  0 1 2 3 4 5 6 7 8 9
  0 1 2 3 4 5 6 7 8 9
  0 1 2 3 4 5 6 7 8 9)

(define (hux x)
  0 1 2 3 4 5 6 7 8 9
  0 1 2 3 4 5 6 7 8 9
  0 1 2 3 4 5 6 7 8 9)

;*---------------------------------------------------------------------*/
;*    side ...                                                         */
;*---------------------------------------------------------------------*/
(define (side)
   (let ((x 1))
      (if (begin (set! x 2) #t)
	  x
	  3)))

;*---------------------------------------------------------------------*/
;*    Encore un code que ne se compilait pas:                          */
;*---------------------------------------------------------------------*/
(define filter_list@types (lambda (x)
			     0 1 2 3 4 5 6 7 8 9
			     0 1 2 3 4 5 6 7 8 9
			     0 1 2 3 4 5 6 7 8 9))

(define filter@types
   (lambda (x1)
      (labels
	    ((!-d-staticfail1024 () 9))
	 (case x1
	    ((2)
	     (case x1
		((2)
		 4)
		(else (!-d-staticfail1024))))
	    ((3)
	     (case x1
		((3)
		 x1)
		(else (!-d-staticfail1024))))
	    (else
	     (case x1
		((4)
		 (if x1
		     (filter_list@types 4)
		     8))
		(else (!-d-staticfail1024))))))))

;*---------------------------------------------------------------------*/
;*    Un autre programme qui plantait `integ'                          */
;*---------------------------------------------------------------------*/
(define join_132@baltree
   (lambda (x1)
      (lambda (x2)
	 (lambda (x3) 0))))
  
(define 2-171-concat_228@baltree
   (lambda (x1 x2)
      (labels
	    ((!-d-staticfail1016
	      ()
	      0))
	 (labels
	       ((!-d-staticfail1017
		 ()
		 (labels
		       ((!-d-staticfail1018
			 ()
			 (case x1
			    ((2)
			     (case x2
				((2)
				 (((join_132@baltree 0) 0) 0))
				(else (!-d-staticfail1016))))
			    (else (!-d-staticfail1016)))))
		    (!-d-staticfail1018))))
	    (!-d-staticfail1017)))))


;*---------------------------------------------------------------------*/
;*    un prgm qui ne se compile pas sans la  modif dans le fichier     */
;*    cn.scm (dans la fonction G au tout debut diff 1.5, 1.6)          */
;*---------------------------------------------------------------------*/
(define (dummy)
   (letrec ((x1
	     (lambda (x2)
		(lambda (x3)
		   (labels ((hux (!-d.1059) ((x1 x2) x3)))
		      (hux 0)
		      0)))))
      (x1 '())))

;*---------------------------------------------------------------------*/
;*    bug-integ-1 ...                                                  */
;*    -------------------------------------------------------------    */
;*    Cette fonction ne se compile pas avec Bigloo1.6, 1.6b et 1.6c.   */
;*---------------------------------------------------------------------*/
(define (bug-integ-1)
   (begin
    (letrec ((gee (lambda ()
		     (labels ((foo () (bar))
			      (bar () (begin
					 (gee)
					 (foo))))
                        (foo)))))
       (gee))))

;*---------------------------------------------------------------------*/
;*    stupid ...                                                       */
;*---------------------------------------------------------------------*/
(define (stupid)
   (labels ((foo (x) (gee x))
	    (gee (x) (foo x)))
      5))

;*---------------------------------------------------------------------*/
;*    test-sqic ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-sqic)
   (test-module "sqic" "sqic.scm")
   (test "sqic" (foo '(1 2 3)) 3)
   (test "sqic" (bar 1) 4)
   (test "sqic" (side) 2))
      
