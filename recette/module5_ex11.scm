(module ex11
   (export C1 C2 ex11f))

(define-class C1 (a (default 10)))
(define-class C2::C1 (b-of-C2::long (default 20)))

(define (ex11f)
   (let ((x (instantiate::C2)))
      (with-access::C1 x (a)
	 (with-access::C2 x (b-of-C2)
	    (list a b-of-C2 (typeof x))))))
