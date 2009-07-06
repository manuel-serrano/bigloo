(module example
   (main main))

(define (main x)
   (let ((p (point 2.9 (point-car (list 3.5)))))
      (print "point?: " (point? p))
      (print "point?: " (point? 4))
      (print-point p)
      (print (eval `(point? ,p)))
      (eval `(print-point ,p))
      (print "done...")))
