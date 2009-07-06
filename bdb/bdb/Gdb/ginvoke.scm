;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Gdb/ginvoke.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 13:11:10 1999                          */
;*    Last change :  Fri Aug  9 08:57:59 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Invoking GDB from BDB.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gdb_invoke
   (import gdb_proc
	   gdb_annotate
	   gdb_read
	   tools_speek
	   tools_tools
	   tools_io
	   engine_param
	   engine_repl
	   tools_error)
   (export (flush-gdb-pending-output ::bool)
	   (gdb-call->string . args)
	   (gdb-server->string . args)
	   (gdb-noerror-call->string . args)))

;*---------------------------------------------------------------------*/
;*    flush-until-prompt ...                                           */
;*    -------------------------------------------------------------    */
;*    This function stops reading when it encouters a prompt.          */
;*---------------------------------------------------------------------*/
(define (flush-until-prompt::bstring pin::input-port armed?)
   (let loop ((line (read-unbuffered-line pin #f #t))
	      (armed? armed?)
	      (lst '()))
      (bdb-log 5 flush-until-prompt:
	       (string-for-read (string-until line #\Newline 1)))
      (bdb-log 6 #"\narmed?: " armed? " parser: " (find-annotate-parser line))
      (bdb-log 6 " lst: " (with-output-to-string (lambda () (write lst))))
      (bdb-log 5 #\Newline)
      (cond
	 ((newline? line)
	  ;; we might be arming an annotate command
	  (loop (read-unbuffered-line pin #f #t) #t lst))
	 ((string-prefix? *gdb-prompt* line)
	  ;; we have found a prompt, we stop at once
	  (let loop ((lst lst)
		     (res ""))
	     (if (null? lst)
		 res
		 (loop (cdr lst)
		       (string-append (car lst) res)))))
	 (else
	  (let ((parser (find-annotate-parser line)))
	     (if (and armed? (procedure? parser))
		 ;; We have to parse with the parser then, we will
		 ;; keep going with the current parser. The result of this
		 ;; parser is ignored because the local parser is
		 ;; supposed to make a side effect with its input.
		 (let* ((parsed (parser line))
			(armed? (if (boolean? parsed)
				    parsed
				    #f))
			(new-lst (if (string? parsed)
				     (cons parsed lst)
				     lst)))
		    (bdb-log 6 #"***FLUSH-UNTIL-PROMPT:PARSER: "
			     armed? " string?: " (string? parsed)
			     " as bool: "
			     (if (boolean? parser) parser " <no-a-bool>")
			     #\Newline)
		    (loop (read-unbuffered-line pin #f #t)
			  armed?
			  new-lst))
		 ;; we are armed so we have to issue the previous newline
		 (let ((new-lst (if armed?
				    (cons* (string-until line #\Newline 0)
					   #"\n"
					   lst)
				    (cons (string-until line #\Newline 0)
					  lst))))
		    ;; then we loop to read again
		    (loop (read-unbuffered-line pin #f #t) #t new-lst))))))))
	      
;*---------------------------------------------------------------------*/
;*    flush-gdb-pending-output ...                                     */
;*    -------------------------------------------------------------    */
;*    We read everything until we find the gdb prompt.                 */
;*    -------------------------------------------------------------    */
;*    That is we read line by line the gdb output until we find the    */
;*    last lines that gdb prints for its prompt.                       */
;*    -------------------------------------------------------------    */
;*    If VERB? is #t error are reported. Otherwise they are ignored.   */
;*    In any case, the regular gdb outputs are displayed.              */
;*---------------------------------------------------------------------*/
(define (flush-gdb-pending-output verb?)
   (flush-until-prompt (process-output-port (get-gdb-process)) #t)
   (if (gdb-annotate-error?)
       (if verb?
	   (let ((err (gdb-read-error)))
	      ;; reads the error and displays it
	      (console-error err)
	      ;; we redisplay the console prompt
	      (console-prompt (bdb-repl-prompt)))
	   (gdb-read-error))))

;*---------------------------------------------------------------------*/
;*    *gdb-call-armed* ...                                             */
;*---------------------------------------------------------------------*/
(define *gdb-call-armed* #f)

;*---------------------------------------------------------------------*/
;*    echo-command? ...                                                */
;*---------------------------------------------------------------------*/
(define (echo-command? cmd)
   (cond
      ((string-prefix? "kill" cmd) #f)
      ((string-prefix? "server output" cmd) #f)
      (else #t)))

;*---------------------------------------------------------------------*/
;*    reentrant-command? ...                                           */
;*---------------------------------------------------------------------*/
(define (reentrant-command? cmd)
   (cond
      ((string-prefix? "kill" cmd) #t)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    gdb-call ...                                                     */
;*---------------------------------------------------------------------*/
(define (gdb-call cmd::bstring filter::procedure)
   (verbose 4 "sending: [" cmd "]" #\Newline)
   (cond
      ((and *gdb-call-armed* (not (reentrant-command? cmd)))
       (error "internal-error:gdb-call:Illegal re-entrant gdb-call"
	      *gdb-call-armed*
	      cmd))
      ((and (gdb-annotate-busy?) (not (reentrant-command? cmd)))
       (bdb-error "gdb-call"
		  "Can't issue command because GDB is busy"
		  cmd))
      (else
       ;; protect from re-entrant GDB-CALL invokations
       (set! *gdb-call-armed* cmd)
       (let* ((proc (get-gdb-process))
	      (pin  (process-input-port proc))
	      (pout (process-output-port proc)))
	  ;; reset the annotate registers before any invokation
	  (gdb-annotate-reset-registers!)
	  ;; send the command
	  (fprint pin cmd)
	  (flush-output-port pin)
	  ;; remove the echoed line
	  (let ((echo (gdb-read-line 0)))
	     (if (and (not (string=? echo ""))
		      (not (string=? echo cmd))
		      (not (string=? echo (string-for-read cmd)))
		      (echo-command? cmd))
		 (warning "internal-error:gdb-call"
			  "Illegal echo \"" echo "\" for command \""
			  cmd "\"")))
	  ;; we filter out the output of the command
	  (let ((res (filter pout)))
	     (set! *gdb-call-armed* #f)
	     ;; we run the gdb-call hooks
	     (for-each (lambda (h) (h)) *gdb-call-hook*)
	     ;; we are done
	     res)))))

;*---------------------------------------------------------------------*/
;*    gdb-call->string ...                                             */
;*    -------------------------------------------------------------    */
;*    Invokes a GDB command and prints out the result of the           */
;*    evaluation of that command and echo the GDB error messages.      */
;*---------------------------------------------------------------------*/
(define (gdb-call->string . args)
   ;; before calling we have to make a user reset
   (gdb-annotate-user-reset-registers!)
   ;; first, we construct the command line
   (let ((cmd (match-case args
		 (()
		  (error "internal-error:gdb-call-for-ouput"
			 "Illegal empty command"
			 args))
		 ((?str)
		  str)
		 (else
		  (let loop ((cmd (reverse args))
			     (res ""))
		     (if (null? cmd)
			 res
			 (loop (cdr cmd)
			       (string-append (car cmd) " " res))))))))
      ;; we we invoke the called
      (let ((res (gdb-call cmd (lambda (pin) (flush-until-prompt pin #t)))))
	 (if (gdb-annotate-error?)
	     (console-error (gdb-read-error)))
	 res)))

;*---------------------------------------------------------------------*/
;*    gdb-server->string ...                                           */
;*    -------------------------------------------------------------    */
;*    We prefix our command with GDB SERVER prefix and we don't        */
;*    reset user annotate registers (such as the source                */
;*    position).                                                       */
;*---------------------------------------------------------------------*/
(define (gdb-server->string . args)
   ;; first, we construct the command line
   (let* ((args (cons "server" args))
	  (cmd (let loop ((cmd (reverse args))
			  (res ""))
		  (if (null? cmd)
		      res
		      (loop (cdr cmd)
			    (string-append (car cmd) " " res))))))
      ;; we invoke gdb
      (let ((res (gdb-annotate-registers-protect
		  (lambda ()
		     (gdb-call cmd (lambda (pin)
				      (flush-until-prompt pin #f)))))))
	 (if (gdb-annotate-error?)
	     (console-error (gdb-read-error)))
	 res)))

;*---------------------------------------------------------------------*/
;*    gdb-noerror-call->string ...                                     */
;*    -------------------------------------------------------------    */
;*    Invokes a GDB command and prints out the result of the           */
;*    evaluation of that command. Purge the GDB error messages but     */
;*    display them.                                                    *
;*---------------------------------------------------------------------*/
(define (gdb-noerror-call->string . args)
   ;; first, we construct the command line
   (let* ((args (cons "server" args))
	  (cmd (let loop ((cmd (reverse args))
			  (res ""))
		  (if (null? cmd)
		      res
		      (loop (cdr cmd)
			    (string-append (car cmd) " " res))))))
      ;; we we invoke the called
      (let ((res (gdb-annotate-registers-protect
		  (lambda ()
		     (gdb-call cmd (lambda (pin)
				      (flush-until-prompt pin #t)))))))
	 (if (gdb-annotate-error?)
	     (gdb-read-error))
	 res)))
