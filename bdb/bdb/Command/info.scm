;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/info.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 12:51:32 1999                          */
;*    Last change :  Fri Dec  9 11:12:16 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The info command and sub-commands.                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_info
   (import tools_error
	   tools_tools
	   tools_io
	   patient_mangling
	   patient_invoke
	   patient_value
	   command_command
	   command_parse
	   command_backtrace
	   command_break
	   command_whatis
	   gdb_invoke
	   gdb_annotate
	   gdb_tools
	   engine_param
	   tools_speek))

;*---------------------------------------------------------------------*/
;*    display-var-value ...                                            */
;*---------------------------------------------------------------------*/
(define (display-var-value name type value)
   (console-echo name)
   (console-echo " = (")
   (console-echo type)
   (console-echo ") ")
   (console-echo value))
   
;*---------------------------------------------------------------------*/
;*    display-bigloo-locals ...                                        */
;*    -------------------------------------------------------------    */
;*    When parsing the local variable, we simple search for line       */
;*    such as:                                                         */
;*       c_ident = c_value                                             */
;*    If the variable is not a union object * then, we display it by   */
;*    means of the C printer. Otherwise, we use the Bigloo             */
;*    output_value facility.                                           */
;*---------------------------------------------------------------------*/
(define (display-bigloo-locals locals err-msg)
   (let ((port (open-input-string locals)))
      (verbose 5 "locals [" locals "]" #\Newline)
      (let loop ((line (read-line port))
		 (emit #f))
	 (verbose 6 "line: [" line "] : " (find-runtime-type line) #\Newline)
	 (if (string? line)
	     (if (or (=fx (string-length line) 0)
		     (char=? (string-ref line 0) #\space))
		 (loop (read-line port) emit)
		 (let ((exp (with-input-from-string line read-case-sensitive)))
		    (if (symbol? exp)
			;; we stop of the first non symbols because it
			;; means that we have parsed all the locals
			(let* ((ident (symbol->string exp))
			       (type  (gdb-typeof ident)))
			   (let ((scm-name (bdb-demangle2
					    (gdb-annotate-current-function)
					    ident)))
			      ;; if we are not able to find the Scheme name
			      ;; the variable has be hidden, we thus skip it
			      (if (string? scm-name)
				  (begin
				     ;; the newline for variable separation
				     (if emit (console-newline))
				     (if (bigloo-type? type)
					 ;; this is a Bigloo value
					 (let ((v (patient-value ident)))
					    (display-var-value scm-name
							       (car v)
							       (cdr v)))
					 ;; this is not a Bigloo value
					 ;; we display its C value.
					 (let ((v (string-from line #\= 2)))
					    (display-var-value scm-name
							       type
							       v)))
				     (loop (read-line port) #t)) 
				  (loop (read-line port) emit))))
			;; if nothing has be emitted we have to output
			;; a no locals message
			(if (not emit) (console-echo err-msg)))))
	     ;; if nothing has be emitted we have to output
	     ;; a no locals message
	     (if (not emit) (console-echo err-msg))))
      (close-input-port port)))

;*---------------------------------------------------------------------*/
;*    display-c-locals ...                                             */
;*---------------------------------------------------------------------*/
(define (display-c-locals locals)
   (let ((port (open-input-string locals)))
      (let loop ((line (read-line port))
		 (nl? #f))
	 (if (string? line)
	     (begin
		(if nl? (console-newline))
		(console-echo line)
		(loop (read-line port) #t))))
      (close-input-port port)))

;*---------------------------------------------------------------------*/
;*    info-args ...                                                    */
;*---------------------------------------------------------------------*/
(define (info-args _)
   (let* ((fun    (gdb-annotate-current-function))
	  (locals (gdb-call->string "info args")))
      (if (or (not (string? fun))
	      (not (bigloo-function? fun)))
	  ;; Easy job, this is not a Bigloo function...
	  (display-c-locals locals)
	  ;; We have to parse and display. 
	  (display-bigloo-locals locals "No args."))))

;*---------------------------------------------------------------------*/
;*    info-locals ...                                                  */
;*    -------------------------------------------------------------    */
;*    There is no otherway that requesting an "INFO LOCALS" and        */
;*    parse the result of that command :-(                             */
;*---------------------------------------------------------------------*/
(define (info-locals _)
   (let* ((fun    (gdb-annotate-current-function))
	  (locals (gdb-call->string "info locals")))
      (if (or (not (string? fun))
	      (not (bigloo-function? fun)))
	  ;; Easy job, this is not a Bigloo function...
	  (display-c-locals locals)
	  ;; We have to parse and display. 
	  (display-bigloo-locals locals "No locals."))))

;*---------------------------------------------------------------------*/
;*    info-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (info-parser cmd-list source level env)
   (match-case cmd-list
      (()
       (console-echo (info-parser-help))
       (console-newline))
      ((?id . ?rest)
       (let ((cmd (find-command id env)))
	  (if (not (isa? cmd command))
	      ;; if we do not override the command, simply calls gdb
	      (console-echo (gdb-call->string source))
	      (let ((cmd::command cmd))
		 ((-> cmd parser) rest
				  source
				  (+fx level 1)
				  (-> cmd env ))))))))

;*---------------------------------------------------------------------*/
;*    info-stack-parser ...                                            */
;*---------------------------------------------------------------------*/
(define (info-stack-parser cmd-list source level env)
   (match-case cmd-list
      (()
       (display-stack-frames source))
      ((?num)
       (display-stack-frames source))
      ((?- . ?rest)
       (parse-error "Junk at end of arguments." (car rest) source 2))))

;*---------------------------------------------------------------------*/
;*    info-break-parser ...                                            */
;*---------------------------------------------------------------------*/
(define (info-break-parser cmd-list source level env)
   (gdb-call->string source)
   (let ((table (gdb-annotate-breakpoint-table)))
      (display-breakpoint-table table)))

;*---------------------------------------------------------------------*/
;*    info-parser-help ...                                             */
;*---------------------------------------------------------------------*/
(define (info-parser-help)
   "Generic command for showing things about the program being debugged.

List of info subcommands:

info address -- Describe where symbol SYM is stored
info all-registers -- List of all registers and their contents
info architecture -- List supported target architectures
info args -- Argument variables of current stack frame
info breakpoints -- Status of user-settable breakpoints
info catch -- Exceptions that can be caught in the current stack frame
info common -- Print out the values contained in a Fortran COMMON block
info copying -- Conditions for redistributing copies of GDB
info dcache -- Print information on the dcache performance
info display -- Expressions to display when program stops
info files -- Names of targets and files being debugged
info float -- Print the status of the floating point unit
info frame -- All about selected stack frame
info functions -- All function names
info handle -- What debugger does when program gets various signals
info line -- Core addresses of the code for a source line
info locals -- Local variables of current stack frame
info program -- Execution status of the program
info registers -- List of integer registers and their contents
info scope -- List the variables local to a scope
info set -- Show all GDB settings
info sharedlibrary -- Status of loaded shared object libraries
info signals -- What debugger does when program gets various signals
info source -- Information about the current source file
info sources -- Source files in the program
info stack -- Backtrace of the stack
info symbol -- Describe what symbol is at location ADDR
info target -- Names of targets and files being debugged
info terminal -- Print inferior's saved terminal status
info threads -- IDs of currently known threads
info tracepoints -- Status of tracepoints
info types -- All type names
info udot -- Print contents of kernel ``struct user'' for current child
info variables -- All global and static variable names
info warranty -- Various kinds of warranty you do not have
info watchpoints -- Synonym for ``info breakpoints''

Type \"help info\" followed by info subcommand name for full documentation.
Command name abbreviations are allowed if unambiguous.")

;*---------------------------------------------------------------------*/
;*    The info command                                                 */
;*---------------------------------------------------------------------*/
(let ((info (bind-toplevel-command!
	     "info"
	     1
	     info-parser
	     (info-parser-help))))
   (bind-sub-command! info
		      "args"
		      3
		      (make-stop-parse info-args)
		      "Argument variables of current stack frame")
   (bind-sub-command! info
		      "locals"
		      2
		      (make-stop-parse info-locals)
		      "Local variables of current stack frame.")
   (bind-sub-command! info
		      "stack"
		      1
		      info-stack-parser
		      "Backtrace of the stack, or innermost COUNT frames.")
   (bind-sub-command! info
		      "breakpoint"
		      1
		      info-break-parser
		      "Status of user-settable breakpoints."))

;*---------------------------------------------------------------------*/
;*    The cinfo command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "cinfo"
			2
			(lambda (cmd source level env)
			   (display-c-locals
			    (gdb-call->string
			     (substring source 1 (string-length source)))))
			(info-parser-help))

   
