;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/print.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug  4 07:54:28 1999                          */
;*    Last change :  Wed Aug  9 17:05:46 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The print (without format) command.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_print
   (import command_expr
	   tools_error
	   tools_tools
	   tools_io
	   patient_mangling
	   patient_invoke
	   patient_value
	   gdb_tools
	   gdb_invoke
	   gdb_annotate
	   engine_param
	   command_command)
   (export (gdb-value->bdb-value value)))

;*---------------------------------------------------------------------*/
;*    print-parser ...                                                 */
;*---------------------------------------------------------------------*/
(define (print-parser cmd-list source level env)
   (if (null? cmd-list)
       (console-echo (gdb-call->string source))
       (let* ((bdb-expr (string-from-at source 1 #\space 1))
	      (gdb-expr (bdb-expr->gdb-expr
			 bdb-expr
			 (gdb-annotate-current-function))))
	  ;; if bdb-expr and gdb-expr are the same it is a real printing...
	  (gdb-enable-print!)
	  (if (string=? bdb-expr gdb-expr)
	      (let ((cmd (string-append "print " gdb-expr)))
		 (console-echo (gdb-call->string cmd)))
	      ;; they differ, it is a Bigloo print
	      (begin
		 (gdb-enable-bigloo-print!)
		 (let* ((cmd (string-append "print (char *)bdb_print( "
					    gdb-expr
					    (if *write-circle?*
						", 1"
						", 0")
					    " )"))
			(out (gdb-call->string cmd)))
		    (console-echo out))))
	  (gdb-disable-print!))))
   
;*---------------------------------------------------------------------*/
;*    bprint-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (bprint-parser cmd-list source level env)
   (if (null? cmd-list)
       (console-echo (gdb-call->string source))
       (let* ((bdb-expr (string-from-at source 1 #\space 1))
	      (gdb-expr (bdb-expr->gdb-expr
			 bdb-expr
			 (gdb-annotate-current-function))))
	  ;; if bdb-expr and gdb-expr are the same it is a real printing...
	  (gdb-enable-print!)
	  (gdb-enable-bigloo-print!)
	  (let* ((cmd (string-append "print (char *)bdb_print( "
				     gdb-expr
				     (if *write-circle?*
					 ", 1"
					 ", 0")
				     " )"))
		 (out (gdb-call->string cmd)))
	     (console-echo out))
	  (gdb-disable-print!))))
   
;*---------------------------------------------------------------------*/
;*    gdb-value->bdb-value ...                                         */
;*    -------------------------------------------------------------    */
;*    When printing a value GDB has produced something like:           */
;*    $3 = 0x807beb8 "(\"/home/tahoe/serrano/prgm/project\")"          */
;*    We have to remove the pointles address and we have to read       */
;*    into the string to get a nice representation of the value.       */
;*---------------------------------------------------------------------*/
(define (gdb-value->bdb-value value)
   (let* ((value (string-from value #\Space 1))
	  (port  (open-input-string value))
	  (obj   (read port)))
      (close-input-port port)
      (let ((port (open-output-string)))
	 (display obj port)
	 (close-output-port port))))
   
;*---------------------------------------------------------------------*/
;*    *print-help* ...                                                 */
;*---------------------------------------------------------------------*/
(define *print-help*
   "Print value of expression EXP.
Variables accessible are those of the lexical environment of the selected
stack frame, plus all those whose scope is global or an entire file.")

;*---------------------------------------------------------------------*/
;*    *bprint-help* ...                                                */
;*---------------------------------------------------------------------*/
(define *bprint-help*
   "Print value of a Bigloo expression EXP.
Variables accessible are those of the lexical environment of the selected
stack frame, plus all those whose scope is global or an entire file.")

;*---------------------------------------------------------------------*/
;*    *cprint-help* ...                                                */
;*---------------------------------------------------------------------*/
(define *cprint-help*
   "Print value of expression EXP.
Variables accessible are those of the lexical environment of the selected
stack frame, plus all those whose scope is global or an entire file.

$NUM gets previous value number NUM.  $ and $$ are the last two values.
$$NUM refers to NUM'th value back from the last one.
Names starting with $ refer to registers (with the values they would have
if the program were to return to the stack frame now selected, restoring
all registers saved by frames farther in) or else to debugger
\"convenience\" variables (any such name not a known register).
Use assignment expressions to give values to convenience variables.

{TYPE}ADREXP refers to a datum of data type TYPE, located at address ADREXP.
@ is a binary operator for treating consecutive data objects
anywhere in memory as an array.  FOO@NUM gives an array whose first
element is FOO, whose second element is stored in the space following
where FOO is stored, etc.  FOO must be an expression whose value
resides in memory.

EXP may be preceded with /FMT, where FMT is a format letter
but no count or size letter (see \"x\" command).")
   
;*---------------------------------------------------------------------*/
;*    The print command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "print"
			1
			print-parser
			*print-help*)

;*---------------------------------------------------------------------*/
;*    The print command                                                */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "bprint"
			2
			bprint-parser
			*bprint-help*)

;*---------------------------------------------------------------------*/
;*    The cprint command                                               */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "cprint"
			2
			(lambda (cmd source level env)
			   (gdb-enable-print!)
			   (console-echo
			    (gdb-call->string
			     (substring source 1 (string-length source))))
			   (gdb-disable-print!))
			*cprint-help*)
