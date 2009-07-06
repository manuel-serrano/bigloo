(module dsssl
   (export (foo x y #!optional #!rest #!key)))

(define (foo x y #!optional z (zz 1) #!rest r #!key i (j 1))
   (let ((f (lambda (z y #!key (i 8)) (list z y i))))
      (labels ((g (a b #!rest l) (list a b l)))
	 (print "f: " (f 1 2))
	 (print "f: " (f 1 2 i: 9))
	 (print "g: " (g 1 2 3 4 5))
	 (print "r: " r)
	 (print "i: " i)
	 (print "j: " j)
	 (list x y z zz i: i j: j))))

(print "foo: " (foo 1 2 3 4 I: 5))
(print ((lambda (x y #!optional z #!rest r #!key i (j 1))
	   (list x y z i: i j: j))
	3 4 5 i: 6 i: 7))
