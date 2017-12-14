;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/object-sans.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 17 07:59:51 1996                          */
;*    Last change :  Tue Mar 20 14:10:50 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The object system tests                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module object-sans
   (import  (main "main.scm")
	    (object2-sans "object2-sans.scm")
	    (object1-sans "object1-sans.scm")
	    (object3-sans "object3-sans.scm"))
   (include "test.sch")
   (export  (test-object-sans))
   (static  (final-class foo-sans
	       x::long
	       (y::string (default "yoyo"))
	       (z::string (default "zozo")))
	    (class gee-sans
	       x y)
	    (wide-class foo/l-sans::foo-sans (dummy (default 'dummy)))
	    (class titi-sans
	       (x::int (default 666)))
	    (class toto-sans::titi-sans
	       y::char
	       (yy::char read-only)
	       z
	       t))
   (export (class value-sans)
	   (class fin-sans::value-sans x)
	   (class sfin-sans::fin-sans y)
	   (class cfin-sans::fin-sans z))
   (static (final-class point-sans (x (default 0)) (y (default 0)))
           (wide-class pointc-sans::point-sans (color (default 'black)))
           (wide-class point3-sans::point-sans (z (default 0)))
	   (class readc-sans (x read-only))
	   (class virtual-1-sans
	      (x (get (lambda (x) 'virtual-1)) read-only))
	   (class virtual-2-sans::virtual-1-sans
	      (z (default 0) read-only)
	      (t (default 0))
	      (w (set (lambda (x y) 0))
		 (get (lambda (x) 'virtual-2))
		 (default 0))
	      (x (get (lambda (x) 'virtual-2)) read-only)))
   (static (class rec-1-sans
	      f1::rec-2-sans)
	   (class rec-2-sans
	      f2::rec-1-sans))
   (export (class virtual-3-sans
	      (a (get (lambda (x) 'virtual-3-a)) read-only)
	      (x (get (lambda (x) 'virtual-3-x)) read-only))
	   (class virtual-4-sans::virtual-3-sans
	      (x (get (lambda (x) 'virtual-4-x)) read-only)
	      (z (get (lambda (x) 'virtual-4-z)) read-only)))
   (static (class class/constr-sans
	      (constructor)
	      (x (default 10)))
	   (class class2/constr-sans::class/constr-sans)))
 
;*---------------------------------------------------------------------*/
;*    *obj* ...                                                        */
;*---------------------------------------------------------------------*/
(define *obj* '())

;*---------------------------------------------------------------------*/
;*    constructor ...                                                  */
;*---------------------------------------------------------------------*/
(define (constructor o)
   (set! *obj* (cons o *obj*)))
 
;*---------------------------------------------------------------------*/
;*    access ...                                                       */
;*---------------------------------------------------------------------*/
(define (access)
   (let ((p (instantiate::point-sans (x 1) (y 2))))
      (with-access::point-sans p (x y)
	 (set! y (+fx y x))
	 (set! y (-fx y x))
	 (set! y (+fx y 100))
	 (-fx y x))))

;*---------------------------------------------------------------------*/
;*    access2 ...                                                      */
;*---------------------------------------------------------------------*/
(define (access2)
   (let ((o1 (instantiate::toto-sans
	       (y #\a)
	       (yy #\b)
	       (z 'toto)
	       (t 'tata)))
	 (o2 (instantiate::titi-sans))
	 (o3 (instantiate::toto-sans
		(x 4)
		(y #\a)
		(yy #\b)
		(z 'toto)
		(t 'tata)))
	 (o4 (instantiate::titi-sans
		(x 5))))
      (with-access::titi-sans o1 ((x1 x))
	 (with-access::titi-sans o2 ((x2 x))
	    (with-access::titi-sans o3 ((x3 x))
	       (with-access::titi-sans o4 ((x4 x))
		  (list x1 x2 x3 x4)))))))

;*---------------------------------------------------------------------*/
;*    wide-dispatch ...                                                */
;*---------------------------------------------------------------------*/
(define (wide-dispatch)
   (let ((p (instantiate::point-sans (x 1) (y 2))))
      (let ((r1 (do-point p)))
	 (widen!::pointc-sans p)
	 (let ((r2 (do-point p)))
	    (shrink! p)
	    (widen!::point3-sans p (z 4))
	    (let ((r3 (do-point p)))
	       (shrink! p)
	       (let ((r4 (do-point p)))
		  (list r1 r2 r3 r4)))))))

(define (do-point-point p)
   1) 

(define-generic (do-point p)
   (error "do-point" "no method for" 'p))

(define-method (do-point p::point-sans)
   (do-point-point p))

(define-method (do-point p::pointc-sans)
   (cons 2 (call-next-method)))

(define-method (do-point p::point3-sans)
   (cons 3 (call-next-method)))

(define-generic (inc-point p::point-sans)
   (with-access::point-sans p (x)
      x))

(define-method (inc-point p::point3-sans)
   (with-access::point3-sans p (x z)
      (+fx x z)))

(define-generic (inc-point2 p::point-sans t)
   (with-access::point-sans p (x)
      (+fx x t)))

(define-method (inc-point2 p::point3-sans t)
   (with-access::point3-sans p (x z)
      (+fx (+fx x z) t)))

;*---------------------------------------------------------------------*/
;*    predicat ...                                                     */
;*---------------------------------------------------------------------*/
(define (predicat)
   (let ((val (instantiate::value-sans))
	 (fin (instantiate::fin-sans (x 1)))
	 (sfin (instantiate::sfin-sans (x -1) (y 2)))
	 (cfin (instantiate::cfin-sans (x -1) (z 3))))
      (list (isa? val value-sans)
	    (isa? fin value-sans) 
	    (isa? sfin value-sans)
	    (isa? cfin value-sans)
	    (isa? val fin-sans)
	    (isa? fin fin-sans)
	    (isa? sfin fin-sans)
	    (isa? cfin fin-sans)
	    (isa? val sfin-sans)
	    (isa? fin sfin-sans)
	    (isa? sfin sfin-sans)
	    (isa? cfin sfin-sans))))

;*---------------------------------------------------------------------*/
;*    intern ...                                                       */
;*---------------------------------------------------------------------*/
(define (intern)
   (let* ((f1 (instantiate::foo-sans (x 1) (y "yuyu") (z "zuzu")))
	  (f2 (instantiate::foo-sans (x 2)))
	  (f3 (duplicate::foo-sans f2 (x 3) (z "zaza")))
	  (p  (cons 1 f3))
	  (f4 (instantiate::gee-sans (x p) (y p)))
	  (f5 (instantiate::foo-sans (x 3)))
	  (r  (instantiate::readc-sans (x 5))))
      (widen!::foo/l-sans f5 (dummy f4))
      (let ((obj (make-vector 8 f2))
	    (o2 (instantiate::foo/l-sans (x 2) (dummy f4))))
	 (vector-set! obj 0 (cons f1 f1))
 	 (vector-set! obj 3 f4)
	 (vector-set! obj 4 f5)
	 (vector-set! obj 5 f5)
	 (vector-set! obj 6 r)
	 (vector-set! obj 7 o2)
	 (let ((bis (string->obj (obj->string obj))))
	    ;; each test must be true
	    (and (eq? (car (vector-ref bis 0)) (cdr (vector-ref bis 0)))
		 (eq? (vector-ref bis 1) (vector-ref bis 2))
		 (with-access::gee-sans (vector-ref bis 3) (x y)
		    (eq? x y))
		 (with-access::gee-sans (vector-ref bis 3) (x)
		    (isa? (cdr x) foo-sans))
		 (with-access::gee-sans (vector-ref bis 3) (x y)
		    (eq? (cdr x) (cdr y)))
		 (with-access::foo/l-sans  (vector-ref bis 5) (dummy)
		    (eq? dummy (vector-ref bis 3)))
		 (eq? (vector-ref bis 4) (vector-ref bis 5))
		 (with-access::readc-sans (vector-ref obj 6) (x)
		    (eq? x x)))))))

;*---------------------------------------------------------------------*/
;*    test-equal? ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-equal?)
   (let ((o (instantiate::toto-sans
	       (y #\a)
	       (yy #\b)
	       (z 'toto)
	       (t 'tata)))
	 (o2 (instantiate::toto-sans
		(y #\a)
		(yy #\b)
		(z 'toto)
		(t 'tata)))
	 (o3 (instantiate::toto-sans
		(x 4)
		(y #\a)
		(yy #\b)
		(z 'toto)
		(t 'tata)))
	 (o4 (instantiate::toto-sans
		(y #\a)
		(yy #\b)
		(z 'totoi)
		(t 'tata))))
      (and (equal? o o2) (not (equal? o o3)) (not (equal? o o4)))))

;*---------------------------------------------------------------------*/
;*    test-with-access ...                                             */
;*---------------------------------------------------------------------*/
(define (test-with-access g u)
   (with-access::gee-sans u (x y)
      (define (hux x::bool)
	 x)
      (hux g)))

;*---------------------------------------------------------------------*/
;*    test-with-access-2 ...                                           */
;*---------------------------------------------------------------------*/
(define (test-with-access-2 u1 u2)
   (with-access::gee-sans u1 ((x1 x) y)
      (with-access::gee-sans u2 (x (y2 y))
	 (set! x1 x)
	 (+ x1 x y y2))))

;*---------------------------------------------------------------------*/
;*    test-with-access-3 ...                                           */
;*---------------------------------------------------------------------*/
(define (test-with-access-3 o)
   (with-access::gee-sans o (x (y1 y))
      (set! x (- x))
      (set! y1 (- y1))
      (cons x y1)))

;*---------------------------------------------------------------------*/
;*    *dump-object* ...                                                */
;*---------------------------------------------------------------------*/
(define *dump-object*
   (list (instantiate::foo-sans (x 1))
      (instantiate::gee-sans (x 1) (y 2))
      (instantiate::foo/l-sans (x 1))
      (vector 5 (instantiate::titi-sans))
      'runtime-os-version
      (get-hashnumber 'runtime-os-version)
      "runtime-os-version"
      (get-hashnumber "runtime-os-version")
      'OS_VERSION
      (get-hashnumber "OS_VERSION")
      'OS_VERSION
      (get-hashnumber "OS_VERSION")
      (instantiate::toto-sans
	 (y #\a)
	 (yy #\b)
	 (z (list "toto" '(1 . 2) '#(1 2 3 tutu)))
	 (t (list 1 ':foo)))
      (let ((table (make-hashtable)))
	 (hashtable-put! table 'toto (instantiate::foo-sans (x 2)))
	 (hashtable-put! table 'tutu (instantiate::foo-sans (x 3)))
	 table)))

;*---------------------------------------------------------------------*/
;*    dump-obj ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-obj fname)
   (let ((p (open-output-binary-file fname)))
      (output-obj p *dump-object*)
      (close-binary-port p)))

;*---------------------------------------------------------------------*/
;*    restore-obj ...                                                  */
;*---------------------------------------------------------------------*/
(define (restore-obj fname)
   (let* ((port (open-input-binary-file fname))
	  (res (input-obj port)))
      (close-binary-port port)
      res))

;*---------------------------------------------------------------------*/
;*    test-object-sans ...                                             */
;*---------------------------------------------------------------------*/
(define (test-object-sans)
   (test-module "object-sans" "object-sans.scm")
   (test "access" (access) 101)
   (test "access.2" (access2) '(666 666 4 5))
   (test "wide-dispatch" (wide-dispatch) '(1 (2 . 1) (3 . 1) 1))
   (test "predicat" (predicat) '(#t #t #t #t #f #t #t #t #f #f #t #f))
   (test "intern" (intern) #t)
   (test "with-access.1" (test-with-access #t (instantiate::gee-sans (x 4) (y 6))) #t)
   (test "with-access.2" (test-with-access-2
			  (instantiate::gee-sans (x 4) (y 6))
			  (instantiate::gee-sans (x 5) (y 7)))
	 23)
   (test "with-access.3" (test-with-access-3
			  (instantiate::gee-sans (x 4) (y 6)))
	 (cons -4 -6))
   (test "equal?" (test-equal?) #t)
   (test "import" (let ((f (instantiate::foo2-sans
			      (x 4)
			      (y 6))))
		     (with-access::foo2-sans f (x y)
			(+fx x y)))
	 10)
   (test "import" (let ((f (instantiate::bar2-sans
			      (a 4)
			      (b 6))))
		     (with-access::bar2-sans f (a b)
			(+fx a b)))
	 10)
   (test "virtual.1" (let ((v (instantiate::virtual-1-sans)))
			(with-access::virtual-1-sans v (x)
			   x))
	 'virtual-1)
   (test "virtual.2" (let ((v (instantiate::virtual-2-sans)))
			(with-access::virtual-1-sans v (x)
			   x))
	 'virtual-2)
   (test "virtual.3" (with-access::virtual-3-sans (instantiate::virtual-4-sans) (x) x) 'virtual-4-x)
   (test "virtual.4" (with-access::virtual-4-sans (instantiate::virtual-4-sans) (x) x) 'virtual-4-x)
   (test "virtual.5" (with-access::virtual-3-sans (instantiate::virtual-3-sans) (x) x) 'virtual-3-x)
   (test "virtual.5b" (try (with-access::virtual-4-sans (instantiate::virtual-3-sans) (x) x)
			 (lambda (esc obj proc msg)
			    (esc #f)))
	 #f)
   (test "virtual.6" (with-access::virtual-3-sans (instantiate::virtual-3-sans) (a) a) 'virtual-3-a)
   (test "virtual.7" (with-access::virtual-3-sans (instantiate::virtual-4-sans) (a) a) 'virtual-3-a)
   (test "virtual.8" (with-access::virtual-4-sans (instantiate::virtual-4-sans) (z) z) 'virtual-4-z)
   (test "field.1" (let loop ((fs (vector->list (class-all-fields virtual-2-sans))))
		      (cond
			 ((null? fs)
			  #f)
			 ((eq? (class-field-name (car fs)) 'w)
		 	  (class-field-virtual? (car fs)))
			 (else
			  (loop (cdr fs)))))
	 #t)
   (test "field.2" (let loop ((fs (vector->list (class-all-fields virtual-2-sans))))
		      (cond
			 ((null? fs)
			  #f)
			 ((eq? (class-field-name (car fs)) 'x)
			  (class-field-virtual? (car fs)))
			 (else
			  (loop (cdr fs)))))
	 #t)
   (test "field.3" (let loop ((fs (vector->list (class-all-fields virtual-2-sans))))
		      (cond
			 ((null? fs)
			  #f)
			 ((eq? (class-field-name (car fs)) 'z)
			  (class-field-virtual? (car fs)))
			 (else
			  (loop (cdr fs)))))
	 #f)
   (test "co-instantiate" (co-instantiate
				((o1 (instantiate::rec-1-sans (f1 o2)))
				 (o2 (instantiate::rec-2-sans (f2 o1))))
			     (with-access::rec-1-sans o1 (f1)
				(with-access::rec-2-sans o2 (f2)
				   (and (eq? f1 o2) (eq? f2 o1)))))
	 #t)
   (test "widening cast" (object? (3foo-sans
				     (instantiate::3point-sans
					(x 1)
					(y 2))))
	 #t)
   (test "widening import" (wxws-sans) 9)
   (test "introspection" (let ((f (find-class-field foo-sans 'x)))
			    (class-field-name f))
	 'x)
   (let ((c1 (instantiate::foo-sans (x 10))))
      (test "serialization.1" (string->obj (obj->string c1)) c1))
   (let ((c1/l (instantiate::foo/l-sans (x 10))))
      (test "serialization.2" (string->obj (obj->string c1/l)) c1/l))
   (let ((c2/l (instantiate::foo/l-sans (x 10))))
      (widen!::foo/l-sans c2/l (dummy 'glop))
      (test "serialization.2" (string->obj (obj->string c2/l)) c2/l)
      (test "serialization.3" (isa?  (string->obj (obj->string c2/l)) foo/l-sans) #t)
      (test "serialization.4" (with-access::foo/l-sans (string->obj (obj->string c2/l)) (dummy) dummy) 'glop))
   (let ((l (list (instantiate::point-sans (x 1))
		  (instantiate::point3-sans (x 2) (z 10))))
	 (p (list + inc-point inc-point2)))
      (test "generic.1" ((car p) ((cadr p) (car l)) ((cadr p) (cadr l))) 13)
      (test "generic.2" ((car p) ((caddr p) (car l) 1) ((caddr p) (cadr l) 2)) 16))
   (let ((i1 (instantiate::class/constr-sans))
	 (i2 (instantiate::class2/constr-sans)))
      (test "constructor" (length *obj*) 2)))
