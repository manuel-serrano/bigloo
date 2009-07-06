;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/file.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 16:00:47 1999                          */
;*    Last change :  Sun Jul 30 09:03:50 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The file command implementation                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_file
   (import command_command
	   command_expr
	   gdb_invoke
	   gdb_annotate
	   engine_param
	   patient_mangling
	   tools_io
	   bee_etags)
   (export (set-previous-file! ::bstring)
	   (get-previous-file)))

;*---------------------------------------------------------------------*/
;*    *previous-file* ...                                              */
;*---------------------------------------------------------------------*/
(define *previous-file* #unspecified)

;*---------------------------------------------------------------------*/
;*    set-previous-file! ...                                           */
;*---------------------------------------------------------------------*/
(define (set-previous-file! file)
   (set! *previous-file* file))

;*---------------------------------------------------------------------*/
;*    get-previous-file ...                                            */
;*---------------------------------------------------------------------*/
(define (get-previous-file)
   *previous-file*)
	 
;*---------------------------------------------------------------------*/
;*    make-file-callback ...                                           */
;*---------------------------------------------------------------------*/
(define (make-file-callback file)
   ;; first, we register the file for a futur file command
   (set-previous-file! file)
   ;; then emit the command
   (console-echo (gdb-call->string (string-append "file " file)))
   ;; we reset the whole thing
   (gdb-annotate-reset-stack-frames!)
   ;; try to re-read the etags description
   (read-etags-file))

;*---------------------------------------------------------------------*/
;*    file-command-parse ...                                           */
;*---------------------------------------------------------------------*/
(define (file-command-parse cmd-list source::bstring level::int env)
   (match-case cmd-list
      (()
       ;; we cannot simply invoke the bdb command, we have to
       ;; check first if a file has already been loaded. If it
       ;; has, file with no argument means re-load that file
       (if (string? *previous-file*)
	   ;; a reload
	   (make-file-callback *previous-file*)
	   ;; file with no argument! After all, why not...
	   (begin
	      (console-echo (gdb-call->string "file"))
	      (gdb-annotate-reset-stack-frames!)
	      ;; try to re-read the etags description
	      (read-etags-file))))
      ((?file . ?-)
       ;; because this is a change of file, we have to forget
       ;; about everything we know of name demangling.
       (reset-demangling-cache!)
       ;; we also have to reset the expression table
       (reset-expression-table!)
       ;; we do invoke the gdb command
       (make-file-callback file))))

;*---------------------------------------------------------------------*/
;*    The file command                                                 */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "file"
			3
			file-command-parse
			"Use FILE as program to be debugged.
It is read for its symbols, for getting the contents of pure memory,
and it is the program executed when you use the `run' command.
If FILE cannot be found as specified, your execution directory path
($PATH) is searched for a command of that name.
No arg means to have no executable file and no symbols.")
