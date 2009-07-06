;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/break.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 16:25:36 1999                          */
;*    Last change :  Mon Oct 31 15:33:35 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    In this implementation of break, we assume that Scheme           */
;*    breakpoints are entered using upper case strings and C           */
;*    breakpoints are lower case strings.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_break
   (import tools_error
	   tools_tools
	   tools_io
	   patient_mangling
	   bee_etags
	   command_command
	   command_parse
	   command_expr
	   engine_param
	   gdb_invoke
	   gdb_annotate)
   (export (display-breakpoint-table table)
	   (compare-breakpoint-tables old new)
	   (break-condition ::pair-nil)
	   (break-footprint ::pair-nil)
	   (breakpoint-changed!)
	   (breakpoint-changed?::bool)))

;*---------------------------------------------------------------------*/
;*    *breakpoint-changed?* ...                                        */
;*    -------------------------------------------------------------    */
;*    This variable is set to #t when an break command is issued       */
;*    (e.g. delete, disable, enabled, break, ...).                     */
;*---------------------------------------------------------------------*/
(define *breakpoint-changed?* #f)

;*---------------------------------------------------------------------*/
;*    breakpoint-changed? ...                                          */
;*---------------------------------------------------------------------*/
(define (breakpoint-changed?::bool)
   *breakpoint-changed?*)

;*---------------------------------------------------------------------*/
;*    breakpoint-changed! ...                                          */
;*---------------------------------------------------------------------*/
(define (breakpoint-changed!)
   (set! *breakpoint-changed?* #t))

;*---------------------------------------------------------------------*/
;*    display-breakpoint-table ...                                     */
;*    -------------------------------------------------------------    */
;*    Prints out a breakpoint table has parsed by the --annotate       */
;*    parser. This function must implements some name demangling       */
;*    of the what field. That field display a summary of the           */
;*    breakpoint location. That description contains the function      */
;*    name :-(.                                                        */
;*---------------------------------------------------------------------*/
(define (display-breakpoint-table table)
   (define (get-tabbing-list)
      '(4 16 5 4 12 0 0 0 0 0 0 0 0 0 0))
   (match-case table
      ((?headers . ?records)
       (define (what-value value)
	  ;; This function computes the shape of the WHAT value of
	  ;; a breakpoint record. This require a little bit a name demangling.
	  (let* ((ident     (string-until-at value 3 #\space 0))
		 (scm-ident (bdb-demangle ident))
		 (start     (string-from-at value 3 #\space 1)))
	     (if (>=fx (string-length start) 3)
		 (let ((loc (substring start 3 (string-length start))))
		    (values
		     (if (string? scm-ident)
			 (string-append "in "
					scm-ident
					(string-from-at value 3 #\space 0))
			 value)
		     loc))
		 (values value #f))))
       (define (display/width string width)
	  (let ((add (-fx width (string-length string))))
	     (console-echo string)
	     (if (>fx add 0)
		 (console-echo (make-string add #\space)))))
       (define (display-breakpoint-table-headers headers)
	  (for-each (lambda (x w) (display/width x w))
		    headers
		    (get-tabbing-list))
	  (console-newline))
       (define (display-breakpoint-record-value/edit string edit width)
	  (let ((add (-fx width (string-length string))))
	     (console-echo-edit string edit)
	     (if (>fx add 0)
		 (console-echo (make-string add #\space)))))
       (define (display-breakpoint-record-value record tabbing)
	  (display/width record tabbing))
       (define (display-breakpoint-table-record record len)
	  ;; we display the first element with tabing
	  (let loop ((i       0)
		     (record  record)
		     (tabbing (get-tabbing-list)))
	     (cond
		((=fx i len)
		 (console-newline)
		 ;; we display all the other elements in separate lines
		 (for-each (lambda (x)
			      (console-echo x) (console-newline))
			   record))
		((=fx i (-fx len 1))
		 ;; this is the WHAT value, the one that has to be demangled
		 (let ((tab (if (pair? tabbing) (car tabbing) 0)))
		    (multiple-value-bind (value loc)
		       (what-value (car record))
		       (if loc
			   (display-breakpoint-record-value/edit value loc tab)
			   (display-breakpoint-record-value value tab)))
		    (loop (+fx i 1)
			  (cdr record)
			  (cdr tabbing))))
		(else
		 (let ((tab (if (pair? tabbing) (car tabbing) 0)))
		    (display-breakpoint-record-value (car record) tab)
		    (loop (+fx i 1)
			  (cdr record)
			  (cdr tabbing)))))))
       (define (display-breakpoint-table-records records len)
	  (for-each (lambda (x) (display-breakpoint-table-record x len))
		    records))
       (display-breakpoint-table-headers headers)
       (display-breakpoint-table-records records (length headers)))
      ((kwote no-breakpoint)
       ;; no breakpoint yet
       (console-echo #"No breakpoints or watchpoints.\n"))
      (else
       (error "display-breakpoint-table" "Illegal table" table))))

;*---------------------------------------------------------------------*/
;*    footprint-command-parse ...                                      */
;*    -------------------------------------------------------------    */
;*    This function is sent to BREAK-COMMAND-PARSE. Its argument       */
;*    is the rest of the list that is left unparsed by the break       */
;*    command.                                                         */
;*---------------------------------------------------------------------*/
(define (footprint-command-parse base cmd source::bstring level::int env)
   (define (obj->string o)
      (let ((p (open-output-string)))
	 (display o p)
	 (close-output-port p)))
   (let* ((src (string-append base (string-from source #\space 0)))
	  (msg (match-case cmd
		  (()
		   (break-command-parse base cmd src level env)
		   (symbol->string (gensym "footprint")))
		  ((?fun)
		   (break-command-parse base cmd src level env)
		   (obj->string fun))
		  ((?fun . ?rest)
		   (break-command-parse base (list fun) src level env)
		   (obj->string (car rest)))))
	  (msg    (if *bdb-fontification?*
		      (string-append *footprint-mark* msg)
		      msg))
	  (action (string-append #"commands\nsilent\ncall puts(\""
				 msg
				 #"\")\ncontinue\nend")))
      (if (not (gdb-annotate-error?))
	  (gdb-server->string action))))
		     
;*---------------------------------------------------------------------*/
;*    break-command-parse ...                                          */
;*    -------------------------------------------------------------    */
;*    If the argument of the breakpoint is an identifier and if that   */
;*    identifier the one of a Bigloo function we try successively to   */
;*    find out the source file position of that identifier using the   */
;*    read etags file. If this fail we try a name mangling. The name   */
;*    mangling succeeds if the program is already started. Otherwise   */
;*    it fails and the breakpoint will be rejected.                    */
;*---------------------------------------------------------------------*/
(define (break-command-parse base cmd source::bstring level::int env)
   (match-case cmd
      (()
       ;; mark the breakpoints as changed
       (breakpoint-changed!)
       ;; issue the command
       (console-echo (gdb-call->string "break")))
      ((?fun)
       ;; mark the breakpoints as changed
       (breakpoint-changed!)
       ;; this is a breakpoint to a function
       (if (bigloo-ident? fun)
	   (let ((fpos (bigloo-symbol-etags-source-location fun)))
	      ;; before operating a name mangling we check
	      ;; if we know the source file position of that identifier
	      (if (string? fpos)
		  ;; yes we do
		  (let ((cmd (string-append base " " fpos)))
		     (console-echo (gdb-call->string cmd)))
		  ;; no we don't thus we try some name mangling to get
		  ;; the C name associated to that function
		  (let ((c-name (bdb-mangle fun)))
		     (if (string? c-name)
			 (let ((cmd (string-append base " " c-name)))
			    (console-echo (gdb-call->string cmd)))
			 ;; we have found nothing the symbol is unbound
			 (bdb-error "break"
				    "Can't find Bigloo identifier"
				    fun)))))
	   ;; this is a plain C identifier or a source file position
	   (console-echo (gdb-call->string source))))
      ((?- . ?rest)
       (parse-error "Junk at end of arguments." (car rest) source 2))))
   
;*---------------------------------------------------------------------*/
;*    break-condition ...                                              */
;*    -------------------------------------------------------------    */
;*    Return the condition expression associated to a breakpoint       */
;*    records.                                                         */
;*---------------------------------------------------------------------*/
(define (break-condition records)
   (let* ((stop-markup     #"\tstop only if ")
	  (stop-markup-len (string-length stop-markup)))
      (let loop ((records records))
	 (cond
	    ((null? records)
	     #f)
	    ((substring=? stop-markup (car records) stop-markup-len)
	     (substring (car records)
			stop-markup-len
			(string-length (car records))))
	    (else
	     (loop (cdr records)))))))
    
;*---------------------------------------------------------------------*/
;*    break-footprint ...                                              */
;*    -------------------------------------------------------------    */
;*    Returns the expression associated with a footprint.              */
;*---------------------------------------------------------------------*/
(define (break-footprint records)
   (and (=fx (length records) 3)
	(let* ((start-markup    #"        silent")
	       (mid-markup      #"        call puts(\"")
	       (mid-markup-stop #"\")")
	       (mid-markup-len  (string-length mid-markup))
	       (stop-markup     #"        continue"))
	   ;; don't ask me why on condition gdb introduces #\tab characters
	   ;; and on commands, it introduces #\space characters!!!
	   (if (and (string=? (car records) start-markup)
		    (string=? (caddr records) stop-markup)
		    (substring=? mid-markup (cadr records) mid-markup-len))
	       (substring (cadr records)
			  mid-markup-len
			  (-fx (string-length (cadr records))
			       (string-length mid-markup-stop)))
	       #f))))
  
;*---------------------------------------------------------------------*/
;*    compare-breakpoint-tables ...                                    */
;*    -------------------------------------------------------------    */
;*    This function reset the *BREAKPOINT-CHANGED?* variable. That is  */
;*    after comparing to breakpoint tables, the breakpoints are as     */
;*    unchanged.                                                       */
;*---------------------------------------------------------------------*/
(define (compare-breakpoint-tables old new)
   (set! *breakpoint-changed?* #f)
   (cond
      ((not (pair? old))
       (if (pair? new)
	   (values '() '() (cdr new))
	   (values '() '() '())))
      ((not (pair? new))
       (values (cdr old) '() '()))
      (else
       (let loop ((orec    (cdr old))
		  (nrec    (cdr new))
		  (removed '())
		  (changed '())
		  (added   '()))
	  (cond
	     ((and (null? orec) (null? nrec))
	      (values removed changed added))
	     ((null? orec)
	      (values removed changed (append nrec added)))
	     ((null? nrec)
	      (values (append orec removed) changed added))
	     (else
	      (let* ((o (car orec))
		     (n (car nrec)))
		 (match-case o
		    ((?on ?- ?- ?oe . ?er)
		     (match-case n
			((?nn ?- ?- ?ne . ?or)
			 (let ((onum (string->integer on))
			       (nnum (string->integer nn)))
 			    (cond
			       ((and (=fx onum nnum)
				     (string=? oe ne)
				     (equal? (break-condition er)
					     (break-condition or)))
				;; nothing changed
				(loop (cdr orec)
				      (cdr nrec)
				      removed
				      changed
				      added))
			       ((=fx onum nnum)
				;; the state has changed
				(loop (cdr orec)
				      (cdr nrec)
				      removed
				      (cons n changed)
				      added))
			       ((<fx onum nnum)
				;; a breakpoint has been remove
				(loop (cdr orec)
				      nrec
				      (cons o removed)
				      changed
				      added))
			       ((>fx onum nnum)
				;; a breakpoint has been added
				(loop orec
				      (cdr nrec)
				      removed
				      changed
				      (cons n added))))))
			(else
			 (loop (cdr orec)
			       (cdr nrec)
			       removed
			       changed
			       added))))
		    (else
		     (loop (cdr orec)
			   (cdr nrec)
			   removed
			   changed
			   added))))))))))

;*---------------------------------------------------------------------*/
;*    condition-parser ...                                             */
;*---------------------------------------------------------------------*/
(define (condition-parser cmd-list source level end)
   (match-case cmd-list
      (() 
       (console-error "Argument required (breakpoint number)."))
      ((?num)
       (breakpoint-changed!)
       (console-echo (gdb-call->string source)))
      ((?num . ?-)
       (breakpoint-changed!)
       ;; we seek for the beginning of the expression
       (let* ((bdb-exp (string-from (string-from source #\space 1) #\space 1))
	      (gdb-exp (bdb-expr->gdb-expr bdb-exp
					   (gdb-annotate-current-function)))
	      (cmd     (string-append "cond " num " " gdb-exp)))
	  (console-echo "WARNING: THE CONDITION EXPRESSION OF THE BREAKPOINT")
	  (console-newline)
	  (console-echo "IS NOT DEMANGLED IN THE ENVIRONMENT OF THE BREAKPOINT")
	  (console-newline)
	  (console-echo "BUT THE CURRENT ENVIRONMENT. THIS WILL BE FIXED...")
	  (console-newline)
	  (console-echo (gdb-call->string cmd))))))
       
;*---------------------------------------------------------------------*/
;*    The break command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "break"
			1
			(lambda (cmd-list source level env)
			   (break-command-parse "break"
						cmd-list
						source
						level
						env))
"Set breakpoint at specified line or function.
Argument may be line number, function name, or \"*\" and an address.
If line number is specified, break at start of code for that line.
If function is specified, break at start of code for that function.
If an address is specified, break at that exact address.
With no arg, uses current execution address of selected stack frame.
This is useful for breaking on return to a stack frame.

If the function name is upper case, then a breakpoint to a Bigloo
function is set. Otherwise, the name is considered as a C identifier.

Multiple breakpoints at one place are permitted, and useful if conditional.

Do \"help breakpoints\" for info on other commands dealing with breakpoints.")

;*---------------------------------------------------------------------*/
;*    The break command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "tbreak"
			2
			(lambda (cmd-list source level env)
			   (break-command-parse "tbreak"
						cmd-list
						source
						level
						env))
			"Set a temporary breakpoint.  Args like \"break\" command.
Like \"break\" except the breakpoint is only temporary,
so it will be deleted when hit.  Equivalent to \"break\" followed
by using \"enable delete\" on the breakpoint number.")

;*---------------------------------------------------------------------*/
;*    generic-break-command-parse ...                                  */
;*    -------------------------------------------------------------    */
;*    This function is just a wrapper. It calls the real gdb command   */
;*    but it set the BREAKPOINT-CHANGED first.                         */
;*---------------------------------------------------------------------*/
(define (generic-break-command-parse cmd-list source::bstring level::int env)
   (breakpoint-changed!)
   (console-echo (gdb-call->string source)))

;*---------------------------------------------------------------------*/
;*    The delete command                                               */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "delete"
			1
			(lambda (cmd-list source level env)
			   (generic-break-command-parse cmd-list
							source
							level
							env))
			"Delete some breakpoints or auto-display expressions.
Arguments are breakpoint numbers with spaces in between.
To delete all breakpoints, give no argument.

Also a prefix command for deletion of other GDB objects.

List of delete subcommands:

delete breakpoints -- Delete some breakpoints or auto-display expressions
delete display -- Cancel some expressions to be displayed when program stops
delete tracepoints -- Delete specified tracepoints

Type \"help delete\" followed by delete subcommand name for full documentation.
Command name abbreviations are allowed if unambiguous.")

;*---------------------------------------------------------------------*/
;*    The enable command                                               */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "enable"
			2
			(lambda (cmd-list source level env)
			   (generic-break-command-parse cmd-list
							source
							level
							env))
			"Enable some breakpoints.
Give breakpoint numbers (separated by spaces) as arguments.
With no subcommand, breakpoints are enabled until you command otherwise.
This is used to cancel the effect of the \"disable\" command.
With a subcommand you can enable temporarily.

List of enable subcommands:

enable delete -- Enable breakpoints and delete when hit
enable display -- Enable some expressions to be displayed when program stops
enable once -- Enable breakpoints for one hit
enable tracepoints -- Enable specified tracepoints

Type \"help enable\" followed by enable subcommand name for full documentation.
Command name abbreviations are allowed if unambiguous.")

;*---------------------------------------------------------------------*/
;*    The disable command                                              */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "disable"
			3
			(lambda (cmd-list source level env)
			   (generic-break-command-parse cmd-list
							source
							level
							env))
			"Disable some breakpoints.
Give breakpoint numbers (separated by spaces) as arguments.
With no subcommand, breakpoints are disabled until you command otherwise.
This is used to cancel the effect of the \"disable\" command.
With a subcommand you can disable temporarily.

List of disable subcommands:

disable delete -- Disable breakpoints and delete when hit
disable display -- Disable some expressions to be displayed when program stops
disable once -- Disable breakpoints for one hit
disable tracepoints -- Disable specified tracepoints

Type \"help disable\" followed by disable subcommand name for full documentation.
Command name abbreviations are allowed if unambiguous.")

;*---------------------------------------------------------------------*/
;*    the condition command                                            */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "condition"
			4
			condition-parser
			"Specify breakpoint number N to break only if COND is true.
Usage is `condition N COND', where N is an integer and COND is an
expression to be evaluated whenever breakpoint N is reached.")

;*---------------------------------------------------------------------*/
;*    The break command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "footprint"
			3
			(lambda (cmd-list source level env)
			   (footprint-command-parse "break"
						    cmd-list
						    source
						    level
						    env))
			"Set footprint at specified line or function.
Argument may be line number, function name, or \"*\" and an address.
If line number is specified, break at start of code for that line.
If function is specified, footprint at start of code for that function.
If an address is specified, footprint at that exact address.
With no arg, uses current execution address of selected stack frame.
This is useful for footprinting on return to a stack frame.

If the function name is upper case, then a footprint to a Bigloo
function is set. Otherwise, the name is considered as a C identifier.")

;*---------------------------------------------------------------------*/
;*    The break command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "tfootprint"
			4
			(lambda (cmd-list source level env)
			   (footprint-command-parse "tbreak"
						    cmd-list
						    source
						    level
						    env))
			"Set a temporary footprint  Args like \"footprint\" command.
Like \"footprint\" except the footprint is only temporary,
so it will be deleted when hit.")

