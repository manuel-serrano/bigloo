(module client
   (main main))

(define (main argv)
   (if (null? (cdr argv))
       (begin
	  (print "usage: port-num [message]")
	  (exit 0))
       (let ((port-number (string->integer (cadr argv)))
	     (arg         (if (pair? (cddr argv))
			      (caddr argv)
			      "(Toto is not happy)")))
	  (let ((socket (make-client-socket "localhost" port-number)))
	     (print "socket: " socket)
	     (fprint (socket-output socket) arg)
	     (flush-output-port (socket-output socket))
	     (socket-shutdown socket)))))
