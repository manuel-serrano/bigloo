(module main
   (java (class point
	    (constructor init-default-point ())
	    (field x::int "x")
	    (method show::void (::point) "show")
	    (method static pointstatistics::int () "PointStatistics")
	    "Point")
	 (class point
	    (field y::int "y")
	    (field static point-num::int "point_num")
	    (constructor init-point (::int ::int)))
	 (export callback "callback"))
   (export (callback::int ::int))
   (main main))

(define (main argv)
   (let loop ((num (if (null? (cdr argv))
		       10
		       (string->integer (cadr argv)))))
      (if (=fx num -1)
	  (let ((pt (init-default-point)))
	     (show pt)
	     (newline)
	     (print "Number of allocated points: " (PointStatistics))
	     (print "point-num: " point-num))
	  (let ((pt (init-point num num)))
	     (show pt)
	     (print "  <-->  Point: " pt " x:" (point-x pt) " y:" (point-y pt))
	     (newline)
	     (loop (-fx num 1))))))

(define (callback x)
   (+ 1 x))

