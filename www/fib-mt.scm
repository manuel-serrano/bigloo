(module fib-mt
   (extern (macro printf::int (::string ::string ::long) "printf"))
   (library pthread)
   (main main))

(define (main x)
   (let ((arg (if (pair? (cdr x)) (cadr x) "30")))
      (printf "fib(%s)=%ld\n" arg (fib-mt (string->integer arg)))))

(define (fib-mt x)
   (+fx (thread-join!
	   (thread-start-joinable!
	      (instantiate::pthread
		 (body (lambda () (fib (-fx x 1)))))))
      (thread-join!
	 (thread-start-joinable!
	    (instantiate::pthread
	       (body (lambda () (fib (-fx x 2)))))))))

(define (fib x)
   (if (<fx x 2)
       1
       (+fx (fib (-fx x 1)) (fib (-fx x 2)))))
