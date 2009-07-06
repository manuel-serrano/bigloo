(module foo
   (java (array int* ::int)
	 (class bar
	    (method static hello::int (::int*) "hello")
	    "bar"))
   (main main))

(define (main argv)
   (let ((tab (make-int* 2)))
      (print "tab length: " (int*-length tab))
      (int*-set! tab 0 3)
      (int*-set! tab 1 6)
      (print (bar-hello tab))))
