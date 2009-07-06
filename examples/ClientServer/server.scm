(module server)


(let ((socket (make-server-socket)))
   (print "socket: " socket)
   (print "port-number: " (socket-port-number socket))
   (let ((c (socket-accept socket)))
      (let ((obj (read (socket-input c))))
	 (print "read [" obj "]")
	 (socket-close c)
	 (socket-shutdown socket))))
