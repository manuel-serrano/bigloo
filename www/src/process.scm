(module process-example
   (main main))

(define (main argv)
   (print "running: ./a-proc" argv)
   (let* ((proc (apply run-process "./a-proc"
                       env: "FOO=0" env: "BAR=1" env: "GEE=2"
                       error: pipe: output: pipe: argv))
          (out (process-output-port proc))
          (err (process-error-port proc)))
      (purge err)
      (purge out)))

(define (purge port)
   (let loop ((line (read-line port)))
      (if (eof-object? line)
          (close-input-port port)
          (begin
             (print line)
             (loop (read-line port))))))
