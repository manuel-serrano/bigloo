;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/quit.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 11:02:35 1999                          */
;*    Last change :  Wed Aug  9 18:40:40 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The quit & kill commands                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_quit
   (import command_command
	   command_parse
	   engine_param
	   engine_engine
	   tools_io
	   gdb_invoke
	   gdb_annotate))

;*---------------------------------------------------------------------*/
;*    the Quit command                                                 */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "quit"
			1
			(make-stop-parse
			 (lambda (_)
			    ;; we always kill the running process before...
			    (gdb-call->string "kill")
			    ;; quitting bdb
			    (bdb-exit 0)))
			"Exit Bdb.")
	 		
;*---------------------------------------------------------------------*/
;*    the Kill command                                                 */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "kill"
			1
			(lambda (cmd-list source level env)
			   (console-echo (gdb-call->string source))
			   (gdb-annotate-reset-stack-frames!))
			"Kill execution of program being debugged.")
