;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/recette/object.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 17 07:59:51 1996                          */
;*    Last change :  Sun Feb 11 18:38:13 2018 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The object system tests                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module object
   (import  (main    "main.scm")
	    (object2 "object2.scm")
	    (object1 "object1.scm")
	    (object3 "object3.scm"))
   (include "test.sch"
	    "object.sch")
   (export  (test-object)
	    (dump-obj fname))
   (static  (final-class foo
	       x::long
	       (y::string (default "yoyo"))
	       (z::string (default "zozo")))
	    (class gee
	       x y)
	    (wide-class foo/l::foo (dummy (default 'dummy)))
	    (class titi
	       (x::int (default 666)))
	    (class toto::titi
	       y::char
	       (yy::char read-only)
	       z
	       t))
   (export (class value)
	   (class fin::value x)
	   (class sfin::fin y)
	   (class cfin::fin z))
   (static (final-class point (x (default 0)) (y (default 0)))
           (wide-class pointc::point (color (default 'black)))
           (wide-class point3::point (z (default 0)))
	   (class readc (x read-only))
	   (class virtual-1
	      (x (get (lambda (x) 'virtual-1)) read-only))
	   (class virtual-2::virtual-1
	      (z (default 0) read-only)
	      (t (default 0))
	      (w (set (lambda (x y) 0))
		 (get (lambda (x) 'virtual-2))
		 (default 0))
	      (x (get (lambda (x) 'virtual-2)) read-only)))
   (static (class rec-1
	      f1::rec-2)
	   (class rec-2
	      f2::rec-1))
   (export (class virtual-3
	      (a (get (lambda (x) 'virtual-3-a)) read-only)
	      (x (get (lambda (x) 'virtual-3-x)) read-only))
	   (class virtual-4::virtual-3
	      (x (get (lambda (x) 'virtual-4-x)) read-only)
	      (z (get (lambda (x) 'virtual-4-z)) read-only)))
   (static (class class/constr
	      (constructor)
	      (x (default 10)))
	   (class class2/constr::class/constr))
   (static (class def-foo
	      (x (default (cons 1 1))))
	   (final-class def-bar::def-foo
	      (y (default (cons 2 2))))
	   (wide-class def-gee::def-bar
	      (z (default (cons 3 3)))))
   (static (class deftest (x (default (counter))))
	   (counter))
   (export (generic dsssl-generic obj::object name::obj
	      throw::bool %this::object
	      cache::object
	      #!optional (point -1) (cspecs 0))))

;*---------------------------------------------------------------------*/
;*    *counter* ...                                                    */
;*---------------------------------------------------------------------*/
(define *counter* 0)

;*---------------------------------------------------------------------*/
;*    counter ...                                                      */
;*---------------------------------------------------------------------*/
(define (counter)
   (set! *counter* (+fx 1 *counter*))
   *counter*)

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
   (let ((p (instantiate::point (x 1) (y 2))))
      (with-access::point p (x y)
	 (set! y (+fx y x))
	 (set! y (-fx y x))
	 (set! y (+fx y 100))
	 (-fx y x))))

;*---------------------------------------------------------------------*/
;*    access2 ...                                                      */
;*---------------------------------------------------------------------*/
(define (access2)
   (let ((o1 (instantiate::toto
	       (y #\a)
	       (yy #\b)
	       (z 'toto)
	       (t 'tata)))
	 (o2 (instantiate::titi))
	 (o3 (instantiate::toto
		(x 4)
		(y #\a)
		(yy #\b)
		(z 'toto)
		(t 'tata)))
	 (o4 (instantiate::titi
		(x 5))))
      (with-access::titi o1 ((x1 x))
	 (with-access::titi o2 ((x2 x))
	    (with-access::titi o3 ((x3 x))
	       (with-access::titi o4 ((x4 x))
		  (list x1 x2 x3 x4)))))))

;*---------------------------------------------------------------------*/
;*    wide-dispatch ...                                                */
;*---------------------------------------------------------------------*/
(define (wide-dispatch)
   (let ((p (instantiate::point (x 1) (y 2))))
      (let ((r1 (do-point p)))
	 (widen!::pointc p)
	 (let ((r2 (do-point p)))
	    (shrink! p)
	    (widen!::point3 p (z 4))
	    (let ((r3 (do-point p)))
	       (shrink! p)
	       (let ((r4 (do-point p)))
		  (list r1 r2 r3 r4)))))))

(define (do-point-point p)
   1) 

(define-generic (do-point p)
   (error "do-point" "no method for" 'p))

(define-method (do-point p::point)
   (do-point-point p))

(define-method (do-point p::pointc)
   (cons 2 (call-next-method)))

(define-method (do-point p::point3)
   (cons 3 (call-next-method)))

(define-generic (inc-point p::point)
   (point-x p))

(define-method (inc-point p::point3)
   (+fx (point-x p) (point3-z p)))

(define-generic (inc-point2 p::point t)
   (+fx (point-x p) t))

(define-method (inc-point2 p::point3 t)
   (+fx (+fx (point-x p) (point3-z p)) t))

;*---------------------------------------------------------------------*/
;*    predicat ...                                                     */
;*---------------------------------------------------------------------*/
(define (predicat)
   (let ((val (instantiate::value))
	 (fin (instantiate::fin (x 1)))
	 (sfin (instantiate::sfin (x -1) (y 2)))
	 (cfin (instantiate::cfin (x -1) (z 3))))
      (list (value? val)
	    (value? fin) 
	    (value? sfin)
	    (value? cfin)
	    (fin? val)
	    (fin? fin)
	    (fin? sfin)
	    (fin? cfin)
	    (sfin? val)
	    (sfin? fin)
	    (sfin? sfin)
	    (sfin? cfin))))

;*---------------------------------------------------------------------*/
;*    intern ...                                                       */
;*---------------------------------------------------------------------*/
(define (intern)
   (let* ((f1 (make-foo 1 "yuyu" "zuzu"))
	  (f2 (instantiate::foo (x 2)))
	  (f3 (duplicate::foo f2 (x 3) (z "zaza")))
	  (p  (cons 1 f3))
	  (f4 (make-gee p p))
	  (f5 (instantiate::foo (x 3)))
	  (r  (instantiate::readc (x 5))))
      (widen!::foo/l f5 (dummy f4))
      (let ((obj (make-vector 8 f2))
	    (o2 (instantiate::foo/l (x 2) (dummy f4))))
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
		 (eq? (gee-x (vector-ref bis 3)) (gee-y (vector-ref bis 3)))
		 (foo? (cdr (gee-x (vector-ref bis 3))))
		 (eq? (cdr (gee-x (vector-ref bis 3)))
		      (cdr (gee-y (vector-ref bis 3))))
		 (eq? (foo/l-dummy (vector-ref bis 5))
		      (vector-ref bis 3))
		 (eq? (vector-ref bis 4) (vector-ref bis 5))
		 (eq? (readc-x (vector-ref obj 6))
		      (readc-x (vector-ref bis 6))))))))

;*---------------------------------------------------------------------*/
;*    test-equal? ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-equal?)
   (let ((o (instantiate::toto
	       (y #\a)
	       (yy #\b)
	       (z 'toto)
	       (t 'tata)))
	 (o2 (instantiate::toto
		(y #\a)
		(yy #\b)
		(z 'toto)
		(t 'tata)))
	 (o3 (instantiate::toto
		(x 4)
		(y #\a)
		(yy #\b)
		(z 'toto)
		(t 'tata)))
	 (o4 (instantiate::toto
		(y #\a)
		(yy #\b)
		(z 'totoi)
		(t 'tata))))
      (and (equal? o o2) (not (equal? o o3)) (not (equal? o o4)))))

;*---------------------------------------------------------------------*/
;*    test-with-access ...                                             */
;*---------------------------------------------------------------------*/
(define (test-with-access g u)
   (with-access::gee u (x y)
      (define (hux x::bool)
	 x)
      (hux g)))

;*---------------------------------------------------------------------*/
;*    test-with-access-2 ...                                           */
;*---------------------------------------------------------------------*/
(define (test-with-access-2 u1 u2)
   (with-access::gee u1 ((x1 x) y)
      (with-access::gee u2 (x (y2 y))
	 (set! x1 x)
	 (+ x1 x y y2))))

;*---------------------------------------------------------------------*/
;*    test-with-access-3 ...                                           */
;*---------------------------------------------------------------------*/
(define (test-with-access-3 o)
   (with-access::gee o (x (y1 y))
      (set! x (- x))
      (set! y1 (- y1))
      (cons x y1)))

;*---------------------------------------------------------------------*/
;*    *dump-object* ...                                                */
;*---------------------------------------------------------------------*/
(define *dump-object*
   (list (instantiate::foo (x 1))
	 (instantiate::gee (x 1) (y 2))
	 (instantiate::foo/l (x 1))
	 (vector 5 (instantiate::titi))
	 'runtime-os-version
	 (get-hashnumber 'runtime-os-version)
	 "runtime-os-version"
	 (get-hashnumber "runtime-os-version")
	 'OS_VERSION
	 (get-hashnumber "OS_VERSION")
	 'OS_VERSION
	 (get-hashnumber "OS_VERSION")
	 (instantiate::toto
	    (y #\a)
	    (yy #\b)
	    (z (list "toto" '(1 . 2) '#(1 2 3 tutu)))
	    (t (list 1 ':foo)))
	 (let ((table (make-hashtable)))
	    (hashtable-put! table 'toto (instantiate::foo (x 2)))
	    (hashtable-put! table 'tutu (instantiate::foo (x 3)))
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
;*    dsssl-method ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (dsssl-method o::titi #!key (x 10) a #!rest rest)
   (+ x (if a a 0) (length rest)))

(define-method (dsssl-method o::toto #!key (x 10) a #!rest rest)
   (- (call-next-method) (+ x (if a a 0) (length rest))))

;*---------------------------------------------------------------------*/
;*    dsssl-generic                                                    */
;*---------------------------------------------------------------------*/
(define-generic (dsssl-generic obj::object name::obj
		   throw::bool %this::object
		   cache::object
		   #!optional (point -1) (cspecs 0))
   (+ point cspecs))

(define-method (dsssl-generic obj::titi name::obj
		  throw::bool %this::object
		  cache::object
		  #!optional (point -1) (cspecs 0))
   (+fx 10 (-fx (call-next-method) point)))

(define-method (dsssl-generic obj::toto name::obj
		  throw::bool %this::object
		  cache::object
		  #!optional (point -1) (cspecs 0))
   (+fx 100 (-fx (call-next-method) cspecs)))

;*---------------------------------------------------------------------*/
;*    test-object ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-object)
   (test-module "object" "object.scm")
   (test "access" (access) 101)
   (test "access.2" (access2) '(666 666 4 5))
   (test "wide-dispatch" (wide-dispatch) '(1 (2 . 1) (3 . 1) 1))
   (test "predicat" (predicat) '(#t #t #t #t #f #t #t #t #f #f #t #f))
   (test "intern" (intern) #t)
   (test "with-access.1" (test-with-access #t (instantiate::gee (x 4) (y 6))) #t)
   (test "with-access.2" (test-with-access-2
			  (instantiate::gee (x 4) (y 6))
			  (instantiate::gee (x 5) (y 7)))
	 23)
   (test "with-access.3" (test-with-access-3
			  (instantiate::gee (x 4) (y 6)))
	 (cons -4 -6))
   (test "equal?" (test-equal?) #t)
   (test "import" (let ((f (instantiate::foo2
			      (x 4)
			      (y 6))))
		     (with-access::foo2 f (x y)
			(+fx x y)))
	 10)
   (test "import" (let ((f (instantiate::bar2
			      (a 4)
			      (b 6))))
		     (with-access::bar2 f (a b)
			(+fx a b)))
	 10)
   (test "virtual.1" (let ((v (instantiate::virtual-1)))
			(virtual-1-x v))
	 'virtual-1)
   (test "virtual.2" (let ((v (instantiate::virtual-2)))
			(virtual-1-x v))
	 'virtual-2)
   (test "virtual.3" (virtual-3-x (instantiate::virtual-4)) 'virtual-4-x)
   (test "virtual.4" (virtual-4-x (instantiate::virtual-4)) 'virtual-4-x)
   (test "virtual.5" (virtual-3-x (instantiate::virtual-3)) 'virtual-3-x)
   (test "virtual.5b" (try (virtual-4-x (instantiate::virtual-3))
			   (lambda (esc obj proc msg)
			      (esc #f)))
	 #f)
   (test "virtual.6" (virtual-3-a (instantiate::virtual-3)) 'virtual-3-a)
   (test "virtual.7" (virtual-3-a (instantiate::virtual-4)) 'virtual-3-a)
   (test "virtual.8" (virtual-4-z (instantiate::virtual-4)) 'virtual-4-z)
   (test "field.1" (let loop ((fs (vector->list (class-all-fields virtual-2))))
		      (cond
			 ((null? fs)
			  #f)
			 ((eq? (class-field-name (car fs)) 'w)
		 	  (class-field-virtual? (car fs)))
			 (else
			  (loop (cdr fs)))))
	 #t)
   (test "field.2" (let loop ((fs (vector->list (class-all-fields virtual-2))))
		      (cond
			 ((null? fs)
			  #f)
			 ((eq? (class-field-name (car fs)) 'x)
			  (class-field-virtual? (car fs)))
			 (else
			  (loop (cdr fs)))))
	 #t)
   (test "field.3" (let loop ((fs (vector->list (class-all-fields virtual-2))))
		      (cond
			 ((null? fs)
			  #f)
			 ((eq? (class-field-name (car fs)) 'z)
			  (class-field-virtual? (car fs)))
			 (else
			  (loop (cdr fs)))))
	 #f)
   (test "co-instantiate" (co-instantiate
				((o1 (instantiate::rec-1 (f1 o2)))
				 (o2 (instantiate::rec-2 (f2 o1))))
			     (and (eq? (rec-1-f1 o1) o2)
				  (eq? (rec-2-f2 o2) o1)))
	 #t)
   (test "widening cast" (object? (3foo (instantiate::3point
					   (x 1)
					   (y 2))))
	 #t)
   (test "widening import" (wxws) 9)
   (test "dump.c" (restore-obj "misc/dump.c") *dump-object*)
   (test "dump.jvm" (restore-obj "misc/dump.jvm") *dump-object*)
   (test "introspection" (let ((f (find-class-field foo 'x)))
			    (class-field-name f))
	 'x)
   (let ((c1 (instantiate::foo (x 10))))
      (test "serialization.1" (string->obj (obj->string c1)) c1))
   (let ((c1/l (instantiate::foo/l (x 10))))
      (test "serialization.2" (string->obj (obj->string c1/l)) c1/l))
   (let ((c2/l (instantiate::foo/l (x 10))))
      (widen!::foo/l c2/l (dummy 'glop))
      (test "serialization.2" (string->obj (obj->string c2/l)) c2/l)
      (test "serialization.3" (foo/l? (string->obj (obj->string c2/l))) #t)
      (test "serialization.4" (foo/l-dummy (string->obj (obj->string c2/l))) 'glop))
   (let ((l (list (instantiate::point (x 1))
		  (instantiate::point3 (x 2) (z 10))))
	 (p (list + inc-point inc-point2)))
      (test "generic.1" ((car p) ((cadr p) (car l)) ((cadr p) (cadr l))) 13)
      (test "generic.2" ((car p) ((caddr p) (car l) 1) ((caddr p) (cadr l) 2)) 16))
   (let ((i1 (instantiate::class/constr))
	 (i2 (instantiate::class2/constr)))
      (test "constructor" (length *obj*) 2))
   (let* ((i1 (instantiate::deftest))
	  (i2 (instantiate::deftest)))
      (with-access::deftest i1 ((x1 x))
	 (with-access::deftest i2 ((x2 x))
	    (test "default field value.1" (+fx x1 1) x2))))
   (let ((o1 (instantiate::def-foo)))
      (with-access::def-foo o1 (x)
	 (test "default field value.2" (car x) 1)))
   (let ((o2 (instantiate::def-bar)))
      (with-access::def-bar o2 (x y)
	 (test "default field value.3" (+fx (car x) 1) (car y)))
      (widen!::def-gee o2)
      (with-access::def-gee o2 (x y z)
	 (test "default field value.4" (+fx (car x) (car y)) (car z))))
   (let ((o3 (instantiate::def-gee)))
      (with-access::def-gee o3 (x y z)
	 (test "default field value.5" (+fx (car x) (car y)) (car z))))
   (let ((ta (instantiate::def-gee))
	 (ti (instantiate::titi))
	 (to (instantiate::toto (y #\c) (yy #\a) (z 10) (t 10))))
      (test "dsssl generic.1" (dsssl-method ti :x 20) 20)
      (test "dsssl generic.2" (dsssl-method ti :x 20 :a -10) 10)
      (test "dsssl generic.3" (dsssl-method ti :x 20 :a -10 :z 4 5 6) 14)
      (test "dsssl generic.4" (dsssl-method to :x 20) 0)
      (test "dsssl generic.5" (dsssl-method to :x 20 :a -10) 00)
      (test "dsssl generic.6" (dsssl-method to :x 20 :a -10 4 5 6) 0)
      (test "dsssl generic.10" (dsssl-generic ta "name" #f ta to -2 3) 1)
      (test "dsssl generic.11" (dsssl-generic ta "name" #f ta to -2) -2)
      (test "dsssl generic.12" (dsssl-generic ta "name" #f ta to) -1)
      (test "dsssl generic.10" (dsssl-generic ti "name" #f to to -2 3) 13)
      (test "dsssl generic.11" (dsssl-generic ti "name" #f to to -2) 10)
      (test "dsssl generic.12" (dsssl-generic ti "name" #f to to) 10)
      (test "dsssl generic.13" (dsssl-generic to "name" #f to to -2 3) 110)
      (test "dsssl generic.14" (dsssl-generic to "name" #f to to -2) 110)
      (test "dsssl generic.15" (dsssl-generic to "name" #f to to) 110)
      (let ((g (car (list dsssl-generic))))
	 (test "dsssl generic.16" (g to "name" #f to to -2 3) 110)
	 (test "dsssl generic.17" (g to "name" #f to to -2) 110)
	 (test "dsssl generic.18" (g to "name" #f to to) 110))))

      
