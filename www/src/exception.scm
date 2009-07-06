(module example
   (import some-module)
   (export (gee str)))

(define (gee str)
   (bind-exit (stop)
      (try (let ((port (open-input-file str)))
	      (unwind-protect 
		 (some-module-fun port stop)
		 (close-input-port port)))
	   (lambda (escape proc obj msg)
	      (notify-error proc obj msg)
	      (escape #f)))))

