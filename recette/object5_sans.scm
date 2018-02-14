;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/recette/object5_sans.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 24 13:29:40 2000                          */
;*    Last change :  Wed Feb 14 08:01:25 2018 (serrano)                */
;*    Copyright   :  2000-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Testing with-access and instantiate.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module object5-sans
   (import  (main "main.scm"))
   (include "test.sch")
   (export (test-object5-sans))
   (static (class object5-sans
	      (x (default 1))
	      (y (default 1))
	      (z (default '()))))
   (export (class object6-sans
	      (rec (default (class-nil object6-sans))))
	   (class object6b-sans::object6-sans))
   (export (class object7-sans (host (default #t)))
	   (final-class object8-sans
	      value::byte))
   (export (generic show-sans ::object6-sans #!optional a b))
   (static (class recursive-dog-sans name next::recursive-dog-sans))
   (eval (export show-sans)
      (class object5-sans)
      (class object6-sans)
      (class object6b-sans)
      (class object7-sans)
      (class object8-sans)))

;*---------------------------------------------------------------------*/
;*    show-sans ::object6-sans ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (show-sans o::object6-sans #!optional a b)
   (+fx a b))

;*---------------------------------------------------------------------*/
;*    test-object5-sans ...                                            */
;*---------------------------------------------------------------------*/
(define (test-object5-sans)
   (test-module "object5-sans" "object5-sans.scm")
   (test "instantiate" (isa? (eval '(instantiate::object5-sans)) object5-sans)
      #t)
   (test "with-access.1" (eval '(let ((o (instantiate::object5-sans)))
				   (with-access::object5-sans o (x y)
				      (+ x y))))
	 2)
   (test "with-access.2" (eval '(let ((o (instantiate::object5-sans)))
				   (with-access::object5-sans o (x)
				      (let ((x 10))
					 x))))
	 10)
   (test "with-access.3" (eval '(let ((o (instantiate::object5-sans)))
				   (let ((x 10))
				      (with-access::object5-sans o (x)
					 x))))
	 1)
   (test "with-access.4" (eval '(let ((o (instantiate::object5-sans)))
				   (with-access::object5-sans o (x y)
				      (let ((y 10))
					 (+ x y)))))
	 11)
   (test "with-access.5" (eval '(let ((o (instantiate::object5-sans)))
				   (let ((y 10))
				      (with-access::object5-sans o (x y)
					 (+ x y)))))
	 2)
   (test "with-access.6" (eval '(let ((o (instantiate::object5-sans)))
				   (let ((y 10))
				      (with-access::object5-sans o (x)
					 (+ x y)))))
	 11)
   (test "with-access.7" (eval '(let ((o (instantiate::object5-sans)))
				   (let ((y 10))
				      (with-access::object5-sans o ((x1 x))
					 (+ x1 y)))))
	 11)
   (test "with-access.8" (eval '(let ((o (instantiate::object5-sans))
				      (o2 (instantiate::object5-sans (x 2))))
				   (with-access::object5-sans o ((x1 x))
				      (with-access::object5-sans o2 ((x2 x))
					 (cons x1 x2)))))
	 '(1 . 2))
   (test "with-access.9" (eval '(let ((o (instantiate::object5-sans))
				      (o2 (instantiate::object5-sans (x 2))))
				   (with-access::object5-sans o ((x1 x))
				      (let ((x1 3))
					 (with-access::object5-sans o2 ((x2 x))
					    (cons x1 x2))))))
	 '(3 . 2))
   (test "with-access.10" (eval '(let ((o (instantiate::object5-sans))
				       (o2 (instantiate::object5-sans (x 2))))
				    (with-access::object5-sans o ((x1 x))
				       (let* ((x1 3)
					      (y x1))
					  (with-access::object5-sans o2 ((x2 x))
					     (cons y x2))))))
	 '(3 . 2))
   (test "with-access.11" (eval '(let ((o (instantiate::object7-sans
					     (host 14))))
				    (when (object? o)
				       (with-access::object7-sans o (host)
					  host))))
	 14)
   (test "with-access.12" (begin
			     (eval '(define (with-access-12)
				       (let ((o (instantiate::object7-sans
						   (host 14))))
					  (when (object? o)
					     (with-access::object7-sans o (host)
						host)))))
			     (eval '(with-access-12)))
	 14)
   (test "with-access.13" (eval '(let ((o (instantiate::object7-sans
					     (host 15))))
				  (with-access::object7-sans o (host)
				     (when (fixnum? host)
					(set! host (+fx host 1)))
				     (set! host (+fx host 1))
				     host)))
	 17)
   (test "with-access.14" (eval '(let ((x (instantiate::object5-sans)))
				  (with-access::object5-sans x (x)
				     x)))
	 1)
   (test "with-access.15" (eval '(let ((z (instantiate::object5-sans
					     (x 3)
					     (z (instantiate::object5-sans (x 2))))))
				  (with-access::object5-sans z (x)
				     x)))
	 3)
   (test "with-access.16" (eval '(let ((z (instantiate::object5-sans
					     (x 3)
					     (z (instantiate::object5-sans
						   (x 2)
						   (z 4))))))
				  (with-access::object5-sans z (z)
				     (with-access::object5-sans z (z)
					z))))
	 4)
   (test "with-access.17" (let ((z (instantiate::object5-sans
				      (x 3)
				      (z (instantiate::object5-sans
					    (x 2)
					    (z 4))))))
			     (with-access::object5-sans z (z)
				(with-access::object5-sans z (z)
				   z)))
	 4)
   (test "-nil.1" (let ((o (instantiate::object6-sans)))
		     (with-access::object6-sans o (rec)
			(eq? rec (class-nil object6-sans))))
      #t)
   (test "-nil.2" (eval '(let ((o (instantiate::object6-sans)))
			  (with-access::object6-sans o (rec)
			     (eq? rec (class-nil object6-sans)))))
	 #t)
   (test "-nil.3" (isa? (instantiate::object8-sans (value 0)) object8-sans) #t)
   (test "-nil.4" (isa? (eval '(instantiate::object8-sans (value 0))) object8-sans) #t)
   (eval '(define-generic (show o) 1))
   (eval '(define-generic (show2 o) 0))
   (eval '(define-generic (show3 o . p) (apply + p)))
   (eval '(define-generic (show4 o q . p) (apply + p)))
   (eval '(define-method (show o::object5-sans) 5))
   (eval '(define-method (show o::object6-sans) 6))
   (eval '(define-method (show3 o::object6-sans . p) (+ 6 (car p))))
   (eval '(define-method (show4 o::object6-sans q . p) (+ 6 q (car p))))
   (test "eval.generic.1" (eval '(show (instantiate::object5-sans))) 5)
   (test "eval.generic.2" (eval '(show (instantiate::object6-sans))) 6)
   (test "eval.generic.3" (eval '(show #f)) 1)
   (test "eval.generic.4" (eval '(show3 #f)) 0)
   (test "eval.generic.5" (eval '(show3 #f 5)) 5)
   (test "eval.generic.6" (eval '(show3 (instantiate::object6-sans) 4)) 10)
   (test "eval.generic.7" (eval '(show4 #f #f)) 0)
   (test "eval.generic.8" (eval '(show4 #f #f 5)) 5)
   (test "eval.generic.9" (eval '(show4 (instantiate::object6-sans) 1 3)) 10)
   (test "eval.generic-apply.1"
	 (eval '(apply show (list #f))) 1)
   (test "eval.generic-apply.2"
	 (eval '(apply show3 (list #f))) 0)
   (test "eval.generic-apply.3"
	 (eval '(apply show3 (list #f 5))) 5)
   (test "eval.generic-apply.4"
	 (eval '(apply show3 (list (instantiate::object6-sans) 4))) 10)
   (test "eval.generic-apply.5"
	 (eval '(apply show4 (list #f #f))) 0)
   (test "eval.generic-apply.6"
	 (eval '(apply show4 (list #f #f 5))) 5)
   (test "eval.generic-apply.7"
	 (eval '(apply show4 (list (instantiate::object6-sans) 1 3))) 10)
   (test "recursive class" (object? (class-nil recursive-dog-sans)) #t)
   (eval '(define-method (show-sans o::object6b-sans #!optional a b)
	   (set! a 10)
	   (set! b 1)
	   (call-next-method)))
   (eval '(define-generic (disp-sans o::object6-sans #!optional a b)
	   (+fx a b)))
   (eval '(define-method (disp-sans o::object6b-sans #!optional a b)
	   (set! a 10)
	   (set! b 1)
	   (call-next-method)))
   (test "eval.method.1" (eval '(disp-sans (instantiate::object6b-sans))) 11)
   (test "eval.method.2" (eval '(show-sans (instantiate::object6b-sans))) 11))
      

