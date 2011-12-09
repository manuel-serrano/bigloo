;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/command.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 10:51:23 1999                          */
;*    Last change :  Sun Jul 30 09:04:00 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The implementation of the command themselves. A command contains */
;*    informations for its completer, its parser and a one line help   */
;*    description. In addition, a command contains an environment      */
;*    which is the list of subcommands it accepts.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_command
   (export (class command
	      ;; the name of the command
	      (name::bstring read-only)
	      ;; the number of character its name can be abbreviated
	      (abbrev-len::int read-only)
	      ;; the command parser
	      (parser::procedure read-only)
	      ;; the environment (the subcommands)
	      (env::pair-nil (default '()))
	      ;; help message
	      (help::bstring read-only)))
   (export (toplevel-command-env)
	   (bind-toplevel-command! ::bstring ::int ::procedure ::bstring)
	   (bind-sub-command! ::obj ::bstring ::int ::procedure ::bstring)
	   (find-command ::bstring env)))

;*---------------------------------------------------------------------*/
;*    toplevel-command-env ...                                         */
;*---------------------------------------------------------------------*/
(define (toplevel-command-env)
   *toplevel-env*)

;*---------------------------------------------------------------------*/
;*    *toplevel-env* ...                                               */
;*    -------------------------------------------------------------    */
;*    The list of all the top level commands (e.g. help, quit, ...).   */
;*---------------------------------------------------------------------*/
(define *toplevel-env* '())

;*---------------------------------------------------------------------*/
;*    bind-toplevel-command! ...                                       */
;*---------------------------------------------------------------------*/
(define (bind-toplevel-command! name abbrev-len parser help)
   (let ((cmd (instantiate::command
		 (name name)
		 (abbrev-len abbrev-len)
		 (parser parser)
		 (help help))))
      (set! *toplevel-env* (cons cmd *toplevel-env*))
      cmd))

;*---------------------------------------------------------------------*/
;*    bind-sub-command! ...                                            */
;*---------------------------------------------------------------------*/
(define (bind-sub-command! main name abbrev-len parser help)
   (if (not (isa?  main command))
       ;; this is an internal error, thus no need to call bdb-error
       (error "bind-sub-command!" "Illegal command" main)
       (let ((new (instantiate::command
		     (name name)
		     (abbrev-len abbrev-len)
		     (parser parser)
		     (help help))))
	  (with-access::command main (env)
	     (set! env (cons new env))
	     new))))

;*---------------------------------------------------------------------*/
;*    find-command ...                                                 */
;*    -------------------------------------------------------------    */
;*    Find a command in an environment. This is a smart name           */
;*    comparison because we have to handle here the abbreviations      */
;*    facility.                                                        */
;*---------------------------------------------------------------------*/
(define (find-command id::bstring env)
   (let ((len (string-length id)))
      (let loop ((env env))
	 (if (null? env)
	     #f
	     (with-access::command (car env) (abbrev-len name)
		(if (and (>=fx len abbrev-len)
			 (substring=? id name len))
		    (car env)
		    (loop (cdr env))))))))
