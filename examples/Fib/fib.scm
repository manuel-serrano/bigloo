
(module fib
   (main main))

(define (main argv)
   (let ((val (if (null? (cdr argv))
		  20
		  (string->integer (cadr argv)))))
      (print (fib val))))


(define (fib x)
   (if (<fx x 2)
       1
       (+fx (fib (-fx x 1)) (fib (-fx x 2)))))

      
