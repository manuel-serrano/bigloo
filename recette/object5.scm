;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/object5.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 24 13:29:40 2000                          */
;*    Last change :  Tue Feb  4 11:02:24 2025 (serrano)                */
;*    Copyright   :  2000-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Testing with-access and instantiate.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module object5
   (import  (main "main.scm"))
   (include "test.sch"
	    "object5.sch")
   (export (test-object5))
   (static (class object5 (x (default 1)) (y (default 1)) (z (default '()))))
   (export (class object6 (rec (default (object6-nil)))))
   (export (class object7 (host (default #t)))
	   (final-class object8 value::byte))
   (static (class recursive-dog name next::recursive-dog))
   (eval (export object6-rec)
         (export object6-nil))
   (eval (class object5)
	 (class object6)
	 (class object7)
	 (class object8)))

;*---------------------------------------------------------------------*/
;*    test-object5 ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-object5)
   (test-module "object5" "object5.scm")
   (test "instantiate" (object5? (eval '(instantiate::object5))) #t)
   (test "with-access.1" (eval '(let ((o (instantiate::object5)))
				 (with-access::object5 o (x y)
				    (+ x y))))
      2)
   (test "with-access.2" (eval '(let ((o (instantiate::object5)))
				 (with-access::object5 o (x)
				    (let ((x 10))
				       x))))
      10)
   (test "with-access.3" (eval '(let ((o (instantiate::object5)))
				 (let ((x 10))
				    (with-access::object5 o (x)
				       x))))
      1)
   (test "with-access.4" (eval '(let ((o (instantiate::object5)))
				 (with-access::object5 o (x y)
				    (let ((y 10))
				       (+ x y)))))
      11)
   (test "with-access.5" (eval '(let ((o (instantiate::object5)))
				 (let ((y 10))
				    (with-access::object5 o (x y)
				       (+ x y)))))
      2)
   (test "with-access.6" (eval '(let ((o (instantiate::object5)))
				 (let ((y 10))
				    (with-access::object5 o (x)
				       (+ x y)))))
      11)
   (test "with-access.7" (eval '(let ((o (instantiate::object5)))
				 (let ((y 10))
				    (with-access::object5 o ((x1 x))
				       (+ x1 y)))))
      11)
   (test "with-access.8" (eval '(let ((o (instantiate::object5))
				      (o2 (instantiate::object5 (x 2))))
				 (with-access::object5 o ((x1 x))
				    (with-access::object5 o2 ((x2 x))
				       (cons x1 x2)))))
      '(1 . 2))
   (test "with-access.9" (eval '(let ((o (instantiate::object5))
				      (o2 (instantiate::object5 (x 2))))
				 (with-access::object5 o ((x1 x))
				    (let ((x1 3))
				       (with-access::object5 o2 ((x2 x))
					  (cons x1 x2))))))
      '(3 . 2))
   (test "with-access.10" (eval '(let ((o (instantiate::object5))
				       (o2 (instantiate::object5 (x 2))))
				  (with-access::object5 o ((x1 x))
				     (let* ((x1 3)
					    (y x1))
					(with-access::object5 o2 ((x2 x))
					   (cons y x2))))))
      '(3 . 2))
   (test "with-access.11" (eval '(let ((o (instantiate::object7
					     (host 14))))
				  (when (object? o)
				     (with-access::object7 o (host)
					host))))
      14)
   (test "with-access.12" (begin
			     (eval '(define (with-access-12)
				     (let ((o (instantiate::object7
						 (host 14))))
					(when (object? o)
					   (with-access::object7 o (host)
					      host)))))
			     (eval '(with-access-12)))
      14)
   (test "with-access.13" (eval '(let ((o (instantiate::object7
					     (host 15))))
				  (with-access::object7 o (host)
				     (when (fixnum? host)
					(set! host (+fx host 1)))
				     (set! host (+fx host 1))
				     host)))
      17)
   (test "with-access.14" (eval '(let ((x (instantiate::object5)))
				  (with-access::object5 x (x)
				     x)))
      1)
   (test "with-access.15" (eval '(let ((z (instantiate::object5
					     (x 3)
					     (z (instantiate::object5 (x 2))))))
				  (with-access::object5 z (x)
				     x)))
      3)
   (test "with-access.16" (eval '(let ((z (instantiate::object5
					     (x 3)
					     (z (instantiate::object5
						   (x 2)
						   (z 4))))))
				  (with-access::object5 z (z)
				     (with-access::object5 z (z)
					z))))
      4)
   (test "with-access.17" (let ((z (instantiate::object5
				      (x 3)
				      (z (instantiate::object5
					    (x 2)
					    (z 4))))))
			     (with-access::object5 z (z)
				(with-access::object5 z (z)
				   z)))
      4)
   (test "-nil.1" (let ((o (instantiate::object6)))
		     (eq? (object6-rec o) (object6-nil)))
      #t)
   (test "-nil.2" (eval '(let ((o (instantiate::object6)))
			  (eq? (object6-rec o) (object6-nil))))
      #t)
   (test "-nil.3" (object8? (instantiate::object8 (value 0))) #t)
   (test "-nil.4" (object8? (eval '(instantiate::object8 (value 0)))) #t)
   (test "eval.generic.1" (begin (eval '(define-generic (show o) 1)) #t) #t)
   (test "eval.generic.2" (begin (eval '(define-generic (show2 o) 0)) #t) #t)
   (test "eval.generic.3" (begin (eval '(define-generic (show3 o . p) (apply + p))) #t) #t)
   (test "eval.generic.4" (begin (eval '(define-generic (show4 o q . p) (apply + p))) #t) #t)
   (test "eval.generic.5" (begin (eval '(define-method (show o::object5) 5)) #t) #t)
   (test "eval.generic.6" (begin (eval '(define-method (show o::object6) 6)) #t) #t)
   (test "eval.generic.7" (begin (eval '(define-method (show3 o::object6 . p) (+ 6 (car p)))) #t) #t)
   (test "eval.generic.8" (begin (eval '(define-method (show4 o::object6 q . p) (+ 6 q (car p)))) #t) #t)
   (test "eval.generic.9" (eval '(show (instantiate::object5))) 5)
   (test "eval.generic.10" (eval '(show (instantiate::object6))) 6)
   (test "eval.generic.11" (eval '(show #f)) 1)
   (test "eval.generic.12" (eval '(show3 #f)) 0)
   (test "eval.generic.13" (eval '(show3 #f 5)) 5)
   (test "eval.generic.14" (eval '(show3 (instantiate::object6) 4)) 10)
   (test "eval.generic.15" (eval '(show4 #f #f)) 0)
   (test "eval.generic.16" (eval '(show4 #f #f 5)) 5)
   (test "eval.generic.17" (eval '(show4 (instantiate::object6) 1 3)) 10)
   (test "eval.generic-apply.1"
      (eval '(apply show (list #f))) 1)
   (test "eval.generic-apply.2"
      (eval '(apply show3 (list #f))) 0)
   (test "eval.generic-apply.3"
      (eval '(apply show3 (list #f 5))) 5)
   (test "eval.generic-apply.4"
      (eval '(apply show3 (list (instantiate::object6) 4))) 10)
   (test "eval.generic-apply.5"
      (eval '(apply show4 (list #f #f))) 0)
   (test "eval.generic-apply.6"
      (eval '(apply show4 (list #f #f 5))) 5)
   (test "eval.generic-apply.7"
      (eval '(apply show4 (list (instantiate::object6) 1 3))) 10)
   (test "recursive class" (object? (recursive-dog-nil)) #t)
   (test "eval virtual field"
      (begin
	 (eval '(module test_getter_with_access
		 (export  (class xaudio-server
			     (music (get (lambda (o)
					    (with-access::xaudio-server o (%music) %music)))
				read-only)
			     (%music (default #f))))))
	 #t)
      #t))
      


