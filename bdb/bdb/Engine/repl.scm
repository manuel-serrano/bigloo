;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Engine/repl.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 10:01:33 1999                          */
;*    Last change :  Thu Aug  8 15:13:19 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bdb REPL.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_repl
   (include "Engine/hooks.sch")
   (import  engine_param
	    command_parse
	    gdb_annotate
	    gdb_proc
	    patient_mangling
	    tools_io
	    tools_version
	    tools_speek)
   (export  (bdb-repl)
	    (bdb-call ::bstring ::bool ::bool ::bool)
	    (bdb-repl-prompt)
	    (history-command-get ::int)
	    (all-history)
	    (reset-history!)
	    (add-history! line)))

;*---------------------------------------------------------------------*/
;*    bdb-repl ...                                                     */
;*    -------------------------------------------------------------    */
;*    A very simple REPL because there is nothing fancy here. We       */
;*    have supposed in that implementation of BDB that it is           */
;*    definitively useless to make use of tools such as readline       */
;*    because we think this is in charge of the tool that will         */
;*    spawn bdb to provide with a decent user interface. If we         */
;*    add a readline connection here we will just be adding an         */
;*    extra layer of cpu consumer.                                     */
;*---------------------------------------------------------------------*/
(define (bdb-repl)
   (if (>= *verbose* 0)
       (version))
    (let loop ((line (read-cmd-line)))
      (cond
	 ((eof-object? line)
	  'done)
	 ((not (procedure? line))
	  (loop (read-cmd-line)))
	 (else
	  (line) 
	  (loop (read-cmd-line))))))

;*---------------------------------------------------------------------*/
;*    bdb-repl-prompt ...                                              */
;*---------------------------------------------------------------------*/
(define (bdb-repl-prompt)
   (let* ((mfun (gdb-annotate-current-function))
	  (dfun (if (string? mfun) (bdb-demangle mfun) #unspecified))
	  (fun  (if (string? dfun) dfun mfun)))
      (if (and *active-prompt* (string? fun))
	  (string-append "(" (substring
			      *prompt*
			      1
			      (-fx (string-length *prompt*) 2))
			 ":" fun ") ")
	  *prompt*)))
   
;*---------------------------------------------------------------------*/
;*    read-cmd-line ...                                                */
;*    -------------------------------------------------------------    */
;*    This function reads a complete line, parse it, expanse it        */
;*    and return a thunk that is a sort of call back to be executed    */
;*    implementing the requested command.                              */
;*---------------------------------------------------------------------*/
(define (read-cmd-line)
   (console-prompt (bdb-repl-prompt))
   (console-flush)
   (let loop ((line (read-line *pin*)))
      (console-flush)
      (cond
	 ((eof-object? line)
	  line)
	 ((string=? line "")
	  (loop (history-command)))
	 (else
	  (let* ((port (open-input-string line))
		 (cmd  (port->string-list port)))
	     ;; store the line in the history buffer
	     (add-history! line)
	     (close-input-port port)
	     ;; before executing the line, we call the pre-hooks
	     (run-hooks *pre-command-hook*)
	     ;; parse and execute the command line
	     (command-parse cmd line)
	     ;; handle the gathered annotate information
	     (gdb-annotate-hook)
	     ;; before flushing the command output, we call
	     ;; all the command hooks
	     (run-hooks *post-command-hook*)
	     ;; flush to get messages as soon as possible
	     (console-flush)
	     ;; we read the process output
	     (let ((str (read-process-tty-output)))
		(if (string? str)
		    (console-output str))))))))

;*---------------------------------------------------------------------*/
;*    bdb-call                                                         */
;*    -------------------------------------------------------------    */
;*    Call the Bdb command CMD. If HIST? is #t, the command is saved   */
;*    in the command history. If the argument ECHO? is #t, the command */
;*    is displayed before being executed. if NL? is #t, a newline is   */
;*    inserted after the echo.                                         */
;*---------------------------------------------------------------------*/
(define (bdb-call line::bstring hist?::bool echo?::bool nl?::bool)
   ;; debugging
   (verbose 3 "bdb-call(" line", " hist? "," echo? "," nl? ") [" line #"]\n")
   ;; we display the command if necessary.
   (if echo?
       (begin
	  (console-echo line)
	  (if nl?
	      (console-newline))))
   (let ((line (if (string=? line "")
		   (history-command)
		   line)))
      (if (string? line)
	  ;; then we parse the command
	  (let* ((port (open-input-string line))
		 (cmd  (port->string-list port)))
	     (close-input-port port)
	     ;; we store the command in the history
	     (if hist?
		 (add-history! line)) 
	     ;; before executing the line, we call the pre-hooks
	     (run-hooks *pre-command-hook*)
	     ;; parse and execute the command line
	     (command-parse cmd line)
	     ;; we display the new source file location
	     (gdb-annotate-hook)
	     ;; before flushing the command output, we call
	     ;; all the command hooks
	     (run-hooks *post-command-hook*)))
      ;; flush to get messages as soon as possible
      (console-flush)))

;*---------------------------------------------------------------------*/
;*    port->string-list ...                                            */
;*---------------------------------------------------------------------*/
(define (port->string-list port::input-port)
   (let ((gram (regular-grammar ((blank  (in #\space #\tab #a012 #\Newline))
                                 (letter (out #\space #\tab #a012 #\Newline)))
                  ((+ blank)
                   (ignore))
                  ((+ letter)
		   (let ((str (the-string)))
		      (cons str (ignore))))
                  (else
                   '()))))
      (read/rp gram port)))

;*---------------------------------------------------------------------*/
;*    *command-history* ...                                            */
;*---------------------------------------------------------------------*/
(define *command-history* '())
(define *command-history-len* 0)

;*---------------------------------------------------------------------*/
;*    add-history! ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-history! line)
   (set! *command-history-len* (+fx 1 *command-history-len*))
   (set! *command-history* (cons line *command-history*)))

;*---------------------------------------------------------------------*/
;*    history-command ...                                              */
;*---------------------------------------------------------------------*/
(define (history-command)
   (if (pair? *command-history*)
       (car *command-history*)
       #f))
   
;*---------------------------------------------------------------------*/
;*    history-command-get ...                                          */
;*---------------------------------------------------------------------*/
(define (history-command-get num::int)
   (let ((offset (cond
		    ((<fx num 0)
		     0)
		    ((>=fx num *command-history-len*)
		     (-fx *command-history-len* 1))
		    (else
		     num))))
      (list-ref *command-history* offset)))

;*---------------------------------------------------------------------*/
;*    all-history ...                                                  */
;*---------------------------------------------------------------------*/
(define (all-history)
   (reverse *command-history*))

;*---------------------------------------------------------------------*/
;*    reset-history! ...                                               */
;*---------------------------------------------------------------------*/
(define (reset-history!)
   (set! *command-history* '())
   (set! *command-history-len* 0))
