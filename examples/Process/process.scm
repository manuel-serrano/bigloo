;<font size="-3"><pre>
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Process/process.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Mar  6 09:57:09 1997                          */
;*    Last change :  Tue Dec 26 07:07:26 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Forking and waiting Unix processes                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module process-example
   (main main))

(define (main argv)
   (print "running: ./the-proc" argv)
   (let* ((proc (apply run-process "./the-proc"
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
;</pre></font>

