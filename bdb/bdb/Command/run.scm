;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/run.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug  6 06:37:14 1999                          */
;*    Last change :  Wed Aug  9 09:43:15 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The run command                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_run
   (include "Engine/hooks.sch")
   (import  command_command
	    command_parse
	    engine_param
	    engine_engine
	    tools_io
	    patient_server
	    gdb_invoke)
   (export  (add-run-hook! ::procedure)))

;*---------------------------------------------------------------------*/
;*    *run-hooks* ...                                                  */
;*---------------------------------------------------------------------*/
(define *run-hooks* '())

;*---------------------------------------------------------------------*/
;*    add-run-hook! ...                                                */
;*---------------------------------------------------------------------*/
(define (add-run-hook! fun::procedure)
   (set! *run-hooks* (cons fun *run-hooks*)))

;*---------------------------------------------------------------------*/
;*    run-parser ...                                                   */
;*---------------------------------------------------------------------*/
(define (run-parser cmd-list source level env)
   ;; we have to reset the Bdb server
   (close-server!)
   ;; we have to execute all the run hooks
   (run-hooks *run-hooks*)
   ;; and then we can issue the command
   (console-echo (gdb-call->string source)))

;*---------------------------------------------------------------------*/
;*    *help-run* ...                                                   */
;*---------------------------------------------------------------------*/
(define *help-run*
   "Start debugged program.  You may specify arguments to give it.
Args may include \"*\", or \"[...]\"; they are expanded using \"sh\".
Input and output redirection with \">\", \"<\", or \">>\" are also allowed.

With no arguments, uses arguments last specified (with \"run\" or \"set args\").
To cancel previous arguments and run with no arguments,
use \"set args\" without arguments.")

;*---------------------------------------------------------------------*/
;*    the Quit command                                                 */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "run" 1 run-parser *help-run*)

