(module example
   (export (class point
	      (x (default 0))
	      (y (default 0)))
	   (class point-3d::point
	      (z (default 0)))
	   (generic display-point ::point)))

(define-generic (display-point p::point)
   (print 'point))

(define-method (display-point p::point)
   (with-access::point p (x y)
      (print 'point " x: " x " y: " y)))

(define-method (display-point p::point-3d)
   (call-next-method)
   (with-access::point-3d (z)
	 (print " z: " z)))

(let ((p (instantiate::point-3d (y 7.8))))
   (display-point p))
