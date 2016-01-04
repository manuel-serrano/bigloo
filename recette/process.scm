;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/process.scm                  */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 28 10:52:56 1992                          */
;*    Last change :  Fri Jan  1 07:49:22 2016 (serrano)                */
;*                                                                     */
;*    Process tests                                                    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module process
  (import  (main "main.scm"))
  (include "test.sch")
  (export  (test-process)))

;*---------------------------------------------------------------------*/
;*    test-number ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-process)
   (test-module "process" "process.scm")
   (test "output: pipe:"
	 (let* ((proc (run-process *bigloo-path*
				   "-eval"
				   "(begin (print (+ 41 27)) (exit 1))"
				   wait: #t
				   output: pipe:))
		(proc-output (process-output-port proc))
		(line (read-line proc-output)))
	    (close-input-port proc-output)
	    (list (process-alive? proc)
		  (process-exit-status proc)
		  line))
	'(#f 1 "68"))
   (test "output: file"
         (let ((temp-filename "process.output.tmp"))
            (delete-file temp-filename)
            (unwind-protect
               (let ((proc (run-process *bigloo-path*
                                        "-eval"
                                        "(begin (print (- 27 95)) (exit 2))"
                                        wait: #t
                                        output: temp-filename)))
                  (call-with-input-file temp-filename
                     (lambda (port)
                        (list (process-alive? proc)
                              (process-exit-status proc)
                              (read-line port)))))
               (if (file-exists? temp-filename)
                   (delete-file temp-filename))))
           '(#f 2 "-68"))
   (test "error: pipe:"
	 (let* ((proc (run-process *bigloo-path*
				   "-eval"
				   "(begin (fprint (current-error-port) (* 68 2)) (exit 3))"
				   wait: #t
				   error: pipe:))
		(proc-error (process-error-port proc))
		(line (read-line proc-error)))
	    (close-input-port proc-error)
	    (list (process-alive? proc) (process-exit-status proc) line))
	 '(#f 3 "136"))
   (test "error: file"
	 (let ((temp-filename "process.error.tmp"))
	    (delete-file temp-filename)
	    (unwind-protect
	       (let ((proc (run-process *bigloo-path*
					"-eval"
					"(begin (fprint (current-error-port) (* 68 -2)) (exit 4))"
					wait: #t
					error: temp-filename)))
		  (call-with-input-file temp-filename
		     (lambda (port)
			(list (process-alive? proc)
			      (process-exit-status proc)
			      (read-line port)))))
	       (if (file-exists? temp-filename)
		   (delete-file temp-filename))))
	 '(#f 4 "-136"))
   (test "input: file"
	 (let ((temp-filename "process.input.tmp"))
	    (delete-file temp-filename)
	    (unwind-protect
	       (begin
		  (call-with-output-file temp-filename
		     (lambda (port)
			(fprint port "(* 68 -3)")))
		  (let* ((proc (run-process *bigloo-path*
					    "-eval"
					    "(begin (print (eval (read))) (exit 5))"
					    wait: #t
					    input: temp-filename
					    output: pipe:))
			 (proc-output (process-output-port proc))
			 (line (read-line proc-output))
			 (close-input-port proc-output))
		     (list (process-alive? proc)
			   (process-exit-status proc)
			   line)))
	       (if (file-exists? temp-filename)
		   (delete-file temp-filename))))
	 '(#f 5 "-204"))
   (test "input: pipe:"
	 (let* ((proc (run-process *bigloo-path*
				   "-eval" 
				   "(begin (print (eval (read))) (exit 6))"
				   wait: #f
				   input: pipe:
				   output: pipe:))
		(proc-input (process-input-port proc))
		(proc-output (process-output-port proc)))
	    (fprint proc-input "(* 68 3)")
	    (close-output-port proc-input)
	    (let ((line (read-line proc-output)))
	       (process-wait proc)
	       (close-input-port proc-output)
	       (list (process-alive? proc)
		     (process-exit-status proc)
		     line)))
	 '(#f 6 "204"))
   (cond-expand
      ;; can't set timeout on regular Java ports
      (bigloo-jvm #f)          
      (else (test "&io-timeout-error"
	       (let* ((proc  (run-process *bigloo-path* "-eval" "(sleep 4)"
				output: pipe:))
		      (output (process-output-port proc)))
		  (input-port-timeout-set! output 1000)
		  (let ((result (with-handler
				   (lambda (e)
				      (isa? e &io-timeout-error))
				   (begin
				      (read-lines output)
				      #f))))
		     (close-input-port output)
		     (process-wait proc)
		     result))
	       #t)))
   (cond-expand
      ;; no access to environment variables in Java
      (bigloo-jvm #f)          
      (else (test "env:"
		  (let* ((proc (run-process *bigloo-path*
					    "-eval"
					    "(begin (print (getenv \"foo\")) (exit 7))"
					    wait: #t
					    output: pipe:
					    env: "foo=bar"))
			 (proc-output (process-output-port proc))
			 (line (read-line proc-output)))
		     (close-input-port proc-output)
		     (list (process-alive? proc)
			   (process-exit-status proc)
			   line))
		  '(#f 7 "bar"))))
   (test "kill"
	 (let ((proc (run-process *bigloo-path*
				  "-eval"
				  "(define (loop n) (when (>fx n 0) (sleep 100) (loop (-fx n 1)))) (loop 100)"
				  wait: #f)))
	    (list (process-alive? proc)
		  (begin (sleep 500) (process-kill proc) (sleep 500) '-)
		  '-))
	 '(#t - -)))
