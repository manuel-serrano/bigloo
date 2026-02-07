(module ex11
   (export (ex11-count *count*) C1 C2 ex11f ex11g ex11h))

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

(define-class V1
   (ctorv)
   (a (default 1))
   (b (default 2))
   (c (get (lambda (o) -1)) (set (lambda (o::V1 v) (set! (-> o a) v))))
   (d (default 3)))

(define-class V2::V1
   (c (get (lambda (o) -2)) (set (lambda (o::V1 v) (set! (-> o b) v))))
   (e (get (lambda (o) 0)) (set (lambda (o::V1 v) (set! (-> o a) v))))
   (f (default 4)))

(define *countv* 0)

(define (ctorv o::V1)
   (set! *countv* (+fx *countv* (-> o a))))

(define (ex11h)
   (let ((o1::V1 (instantiate::V1 (c 10)))
	 (o2::V2 (instantiate::V2)))
      (and (=fx (-> o1 a) 10)
	   (=fx (-> o1 b) 2)
	   (=fx (-> o1 c) -1)
	   (=fx (-> o2 a) 1)
	   (=fx (-> o2 b) 10)
	   (=fx (-> o2 c) -2)
	   (=fx *countv* 2))))
	   
