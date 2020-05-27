(module fib
   (extern (macro printf::int (::string ::string ::long) "printf"))
   (main main))

(define (main x)
   (let ((arg (if (pair? (cdr x)) (cadr x) "30")))
      (printf "fib(%s)=%ld\n" arg (fib (string->integer arg)))))

(define (fib x)
   (if (<fx x 2)
       1
       (+fx (fib (-fx x 1)) (fib (-fx x 2)))))
