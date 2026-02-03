(module ex11
   (export (ex11-count *count*) C1 C2 ex11f ex11g))

(define-class C1 (ctor) (a (default 10)))
(define-class C2::C1 (b-of-C2::long (default 20)))
(define-class C3::C1)

(define *count* 0)

(define (ctor o::C1)
   (set! *count* (+fx *count* (-> o a))))

(define (ex11f)
   (let ((x (instantiate::C2)))
      (with-access::C1 x (a)
	 (with-access::C2 x (b-of-C2)
	    (list a b-of-C2 (typeof x))))))

(define (ex11g)
   (instantiate::C3 (a 30)))

