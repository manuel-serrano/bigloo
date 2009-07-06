(thread-start!
 (make-thread
  (lambda ()
     (print 1)
     (thread-yield!)
     (print 2)
     (thread-yield!)
     (print 3))))

(thread-start!
 (make-thread
  (lambda ()
     (print 'A)
     (thread-yield!)
     (print 'B)
     (thread-yield!)
     (print 'C))))

(thread-start!
 (make-thread
  (lambda () (let loop ((n 0))
		(thread-await! (make-sleep-signal 1000000))
		(print "n=" n)
		(thread-yield!)
		(loop (+fx n 1))))))
