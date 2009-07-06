;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/mangle.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 09:29:59 1999                          */
;*    Last change :  Fri Aug  9 08:56:13 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The mangle/demangle user facility. We provide the use with this  */
;*    facility mostly for bdb debug purposes.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_mangle
   (import tools_error
	   tools_io
	   patient_mangling
	   patient_invoke
	   command_command
	   command_parse
	   gdb_invoke
	   gdb_annotate
	   engine_param))

;*---------------------------------------------------------------------*/
;*    mangle-command-parse ...                                         */
;*---------------------------------------------------------------------*/
(define (mangle-command-parse base cmd-list source::bstring level::int env)
   ;; we have to force a complete reset of the annotate registers
   ;; as for a plain GDB command
   (gdb-annotate-user-reset-registers!)
   ;; then we may emit are request
   (match-case cmd-list
      (()
       (bdb-error base "missing argument" source))
      ((?fun)
       (let* ((cmd (string-append "\"" fun "\""))
	      (mgl (patient-call base cmd)))
	  (print "PATIENT-CALL [" mgl "]")
	  (if mgl
	      (console-echo mgl)
	      (bdb-error base "Can't find variable" fun))))
      ((?fun ?local)
       (let* ((base (string-append base "2"))
	      (cmd  (string-append "\"" fun "\", \"" local "\""))
	      (mgl  (patient-call base cmd)))
	  (if mgl
	      (console-echo mgl)
	      (bdb-error base "Can't find variables " local))))
      ((?- ?- . ?rest)
       (parse-error "Junk at end of arguments." (car rest) source 2))))
 
;*---------------------------------------------------------------------*/
;*    The mangle command                                               */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "mangle"
			3
			(lambda (cmd-list source level env)
			   (mangle-command-parse "bdb_mangle"
						 cmd-list
						 source
						 level
						 env))
			"Display the C name for a Bigloo identifier.")
(bind-toplevel-command! "demangle"
			3
			(lambda (cmd-list source level env)
			   (mangle-command-parse "bdb_demangle"
						 cmd-list
						 source
						 level
						 env))
			"Display the Bigloo name for a C identifier.")

