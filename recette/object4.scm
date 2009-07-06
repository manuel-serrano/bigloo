;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/object4.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 24 13:29:40 2000                          */
;*    Last change :  Mon Nov 21 14:03:43 2005 (serrano)                */
;*    Copyright   :  2000-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Testing the classes indexed slots and the introspection          */
;*    facilities.                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module object4
   (import  (main "main.scm"))
   (include "test.sch")
   (export (test-object4))
   (static (class c1
	      (* toto::long))
	   (class c2::c1
	      (bar (default #f))
	      (* tutu::char))
	   (abstract-class ca1
	      (x (default 1)))
	   (class ca2::ca1
	      (y (default 2)))
	   (class object4)
	   (final-class p1)
	   (wide-class p2::p1))
   (export (final-class person
              (name::string (default "Jones"))
              (sex read-only)
              (* children::person))
           (wide-class married-person::person
              mate::person)))

(define (create)
   (c1? (instantiate::c1
	   (toto-len 3)
	   (toto 1))))

(define (ref)
   (let ((p (instantiate::c1
	       (toto-len 3)
	       (toto 1))))
      (with-access::c1 p (toto)
	 (toto-ref 0))))

(define (set)
   (let ((p (instantiate::c1
	       (toto-len 3)
	       (toto 1))))
      (with-access::c1 p (toto)
	 (toto-set! 1 2)
	 (+ (toto-ref 0) (toto-ref 1)))))

(define (dup)
   (let* ((p1 (instantiate::c1
		 (toto-len 3)
		 (toto 3)))
	  (p2 (duplicate::c1 p1)))
      (with-access::c1 p2 (toto)
	 (eq? (toto-ref 0) (c1-toto-ref p1 2)))))

(define (find-field fields id)
   (let loop ((fields fields))
      (cond
	 ((null? fields)
	  #f)
	 ((eq? (class-field-name (car fields)) id)
	  (car fields))
	 (else
	  (loop (cdr fields))))))

(define (introspect)
   (let* ((tutu (find-class-field c2 'tutu))
	  (bar (find-class-field c2 'bar))
	  (fields (class-fields c2))
	  (toto (find-field (class-fields (class-super c2)) 'toto)))
      (and (class-field? tutu)
	   (class-field-indexed? tutu)
	   (class-field? bar)
	   (not (class-field-indexed? bar))
	   (class-field? toto)
	   (class-field-indexed? toto))))

(define (introspect2)
   (let ((p (instantiate::c1
	       (toto-len 3)
	       (toto 5))))
      (let* ((toto (find-field (class-fields c1) 'toto)))
	 (and (class-field? toto)
	      (class-field-indexed? toto)
	      (=fx ((class-field-len-accessor toto) p) 3)
	      (=fx ((class-field-accessor toto) p 0) 5)
	      (begin
		 ((class-field-mutator toto) p 1 6)
		 (=fx (c1-toto-ref p 1) 6))))))

(define (introspect3)
   (let ((o1 ((class-creator c1) 1 5))
	 (o2 (instantiate::c1
		(toto-len 1)
		(toto 5))))
      (equal? o1 o2)))

(define (subclass l1 l2)
   (let ((p (instantiate::c2
	       (toto-len l1)
	       (toto 3)
	       (tutu-len l2)
	       (tutu #\a))))
      (with-access::c2 p (toto tutu)
	 (+fx toto-len tutu-len))))

(define (abstract)
   (let ((o (instantiate::ca2)))
      (with-access::ca1 o (x)
	 (with-access::ca2 o (y)
	    (if (and (ca1? o) (ca2? o))
		(+ x y))))))

(define (foobar::obj x)
   (cond
      ((> x 10)
       1)
      ((> x 9)
       "toto")
      ((> x 8)
       '(1 2))
      ((> x 7)
       3.1)
      ((> x 6)
       (instantiate::p1))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    test-object4 ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-object4)
   (test-module "object4" "object4.scm")
   (test "class name mangling" (object4? (instantiate::object4)) #t)
   (test "create" (create) #t)
   (test "ref" (ref) 1)
   (test "set" (set) 3)
   (test "duplicate" (dup) #t)
   (test "subclass" (subclass 7 12) 19)
   (test "introspection" (introspect) #t)
   (test "introspection" (introspect2) #t)
   (test "introspection" (introspect3) #t)
   (let* ((o1 (instantiate::c2
		 (toto-len 12)
		 (toto 3)
		 (tutu-len 432)
		 (tutu #\a)))
	  (o2 (instantiate::c1
		 (toto-len 123)
		 (toto 34)))
	  (o (cons o1 o2))
	  (s (obj->string o)))
      (test "write-circle"
	    (begin
	       (with-output-to-string
		  (lambda ()
		     (write-circle o)))
	       #t)
	    #t)
      (test "intext" (string->obj s) o))
   (test "abstract" (abstract) 3)
   (test "illegal widen!" (try (widen!::p2 (foobar 0))
			       (lambda (e a b c)
				  (e 3.14)))
	 3.14)
   (test "with-access::indexed.1"
	 (let ((o (instantiate::c1
		     (toto-len 3)
		     (toto 1))))
	    (with-access::c1 o (toto)
	       (+ toto-len (toto-ref 1))))
	 4)
   (test "with-access::indexed.2"
	 (let ((o (instantiate::c1
		     (toto-len 3)
		     (toto 1))))
	    (with-access::c1 o ((tata toto))
	       (+ tata-len (tata-ref 1))))
	 4)
   (test "with-access::indexed.3"
	 (let ((o1 (instantiate::c1
		     (toto-len 3)
		     (toto 1)))
	       (o2 (instantiate::c1
		      (toto-len 4)
		      (toto 2))))
	    (with-access::c1 o1 ((toto1 toto))
	       (with-access::c1 o2 ((toto2 toto))
		  (+ toto1-len toto2-len (toto1-ref 0) (toto2-ref 0)))))
	 10))


