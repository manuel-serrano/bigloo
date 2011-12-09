;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/help.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 11:40:27 1999                          */
;*    Last change :  Fri Dec  9 11:12:03 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The help command (it is really necessary to bother with it...).  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_help
   (import command_command
	   gdb_invoke
	   engine_param
	   tools_io))

;*---------------------------------------------------------------------*/
;*    print-command-help ...                                           */
;*---------------------------------------------------------------------*/
(define (print-command-help cmd::command)
   (with-access::command cmd (name help)
      (console-echo name)
      (console-echo " -- ")
      (console-echo help)
      (console-newline)))
   
;*---------------------------------------------------------------------*/
;*    main-help-callback ...                                           */
;*---------------------------------------------------------------------*/
(define (main-help-callback)
   (lambda ()
      (console-echo (gdb-call->string "help"))))

;*---------------------------------------------------------------------*/
;*    gdb-help-callback ...                                            */
;*---------------------------------------------------------------------*/
(define (gdb-help-callback line)
   (lambda ()
      (gdb-call->string line)))

;*---------------------------------------------------------------------*/
;*    sub-help-command ...                                             */
;*---------------------------------------------------------------------*/
(define (sub-help-command sub-cmd)
   (lambda ()
      ;; display the information relative to that command
      (print-command-help sub-cmd)
      (with-access::command sub-cmd (name env)
	 (if (pair? env)
	     (begin
		;; they are subcommands
		(console-newline)
		(console-echo "List of ")
		(console-echo name)
		(console-echo " subcommands:")
		(console-newline)
		(console-newline)
		(for-each (lambda (cmd)
			     (with-access::command cmd (help)
				(console-echo help)
				(console-newline)))
			  env))))))

;*---------------------------------------------------------------------*/
;*    help-command-parse ...                                           */
;*---------------------------------------------------------------------*/
(define (help-command-parse cmd-list source::bstring level::int env)
   (let loop ((cmd-list  cmd-list)
	      (help      (main-help-callback))
	      (env       (toplevel-command-env))
	      (err-level level))
      (if (null? cmd-list)
	  (help)
	  (let* ((sub-cmd-id (car cmd-list))
		 (sub-cmd::command (find-command sub-cmd-id env)))
	     (if (not (isa? sub-cmd command))
		 (gdb-help-callback source)
		 (loop (cdr cmd-list)
		       (sub-help-command sub-cmd)
		       (-> sub-cmd env)
		       (+fx err-level 1)))))))

;*---------------------------------------------------------------------*/
;*    The help command                                                 */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "help"
   1
   help-command-parse
   "Print list of commands.")
			   
	   
