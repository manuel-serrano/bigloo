(par '1 '2)

(let ( (x '1) (y '2) )
  (par x y) )

(begin (print 1) (print 2) (print 3) '4)

(par
 (begin (print 1) (stop) (print 2) 'ok1)
 (begin (print 3) (stop) (print 4) 'ok2) )

(par
 (begin
    (print 1)
     (new (begin (print 100)
                (stop)
                 (print 200)
                 'ok3))
    (print 2)
   (stop)
    (print 3)
   (stop)
    (print 4)
    'ok1 )
 (begin
   (print 10)
  (stop)
   (print 20)
  (stop)
   (print 30)
   'ok2) )

(par
 (begin (print 1) (wait 'a) (print 2) 'ok1)
 (begin (print 10) (emit 'a) (print 20) 'ok2) )

(par
 (begin (print 1) (wait 'a) (print 2) (stop) (wait 'b) 'ok1)
 (begin (print 10) (wait 'b) (print 20) (emit 'a) (stop) (emit 'b) 'ok2)
 (begin (print 100) (emit 'b) (stop) 'ok3) )

(par
 (begin (print 1))
 (begin (let ((x 1)) (print x))))

(par
 (begin
    (print 1)
    (stop)
    (print 2)
    (let ((f (lambda (x) (print x))))
       (f (call/cc (lambda (k)
		      (new (k 3))))))))

(par (begin
	(stop)
	(print 1)
	(call/cc (lambda (k) (emit 'kont k)))
	(print 2))
     (begin
	((wait 'kont) 4)))

(par (begin
	(stop)
	(print 1)
	(call-with-current-continuation (lambda (k) (emit 'kont k)))
	(print 2))
     (begin
	((wait 'kont) 4)))

(par (call/cc (lambda (kill)
		 (begin
		    (print 1)
		    (let ((f (lambda (x)
				(print 'f)
				(kill x))))
		       (f (call-with-current-continuation (lambda (k)
							     (emit 'kont k)))))
		    (print 2))))
     (begin
	(print 3)
	(stop)
	(print 4))
     (begin
	((wait 'kont) 4)))


(par (call/cc (lambda (exit)
		 (begin
		    (stop)
		    (exit 6)
		    (print 4)
		    (stop)))))

(par (begin
	(print 1)
	(call/cc (lambda (k)
		    (emit 'toto k)
		    (print 2)))
	(print (wait 'toto))
	(print 3)))


(begin (call/cc (lambda (exit)
		   (par (exit 1))
		   (print 3)))
       (print 'foo))

(let ((th (lambda (x)
	     (begin (print 1)
		    (stop)
		    (print 2)))))
   (par (th 1) (th 2)))
