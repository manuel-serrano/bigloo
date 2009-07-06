(module socket)

(let ((s (make-server-socket)))
   (dynamic-wind 
      ;; Init: Launch an xterm with telnet running
      ;; on the s listening port and connect
      (lambda ()
	 (run-process "xterm" "-e" "telnet" "localhost" 
		      (number->string (socket-port-number s))))
      
      ;; Action: A toplevel like loop
      (lambda ()
	 (let ((c (socket-accept s)))
	    (display #"\nWelcome on the socket REPL.\n\n> " (socket-output c))
	    (flush-output-port (socket-output c))
	    (let loop ()
	       (fprint (socket-output c)
		       "; Result: "
		       (eval (read (socket-input c))))
	       (display "> " (socket-output c))
	       (flush-output-port (socket-output c))
	       (loop))))
      
      ;; Termination: We go here when 
      ;;     -a: an error occurs 
      ;;     -b: connection is closed
      (lambda ()
	 (print #"Shutdown ......\n")
	 (socket-shutdown s))))
