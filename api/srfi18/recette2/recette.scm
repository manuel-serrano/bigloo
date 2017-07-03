(module foo
   (library pthread)
   (main main))

(define *res* -1)

(define (foo x)
   (lambda ()
      (let loop ((n 100000))
	 (if (= (remainder n 10000) 0)
	     (print n))
	 (bind-exit (skip)
	    (with-exception-handler
	       (lambda (e)
		  (skip e))
	       (lambda ()
		  (error x x x))))
	 (if (> n 0)
	     (loop (- n 1))
	     (if (=fx x 1)
		 (set! *res* n))))))

(define (main x)
   (let ((t1 (thread-start-joinable! (instantiate::pthread (body (foo 1)))))
	 (t2 (thread-start-joinable! (instantiate::pthread (body (foo 2))))))
      (thread-join! t1)
      (if (= *res* 0)
	  (print "ok, test succeeded....")
	  (print "error, test failed..."))))
