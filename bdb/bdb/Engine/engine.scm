;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Engine/engine.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 09:27:53 1999                          */
;*    Last change :  Fri Aug  9 08:56:42 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bdb engine (i.e. whatever to be done before the repl).       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_engine
   (include "Engine/hooks.sch")
   (extern  (include "signal.h"))
   (import  engine_param
	    engine_repl
	    tools_error
	    gdb_proc
	    gdb_invoke
	    gdb_annotate
	    patient_server
	    bee_etags
	    bee_invoke
	    command_run
	    command_backtrace
	    patient_invoke
	    tools_speek)
   (export  (engine ::procedure)
	    (bdb-exit ::int)
	    (add-quit-hook! ::procedure)))

;*---------------------------------------------------------------------*/
;*    *quit-hooks* ...                                                 */
;*    -------------------------------------------------------------    */
;*    Hooks to be executed on bdb termination.                         */
;*---------------------------------------------------------------------*/
(define *quit-hooks* '())

;*---------------------------------------------------------------------*/
;*    add-quit-hook! ...                                               */
;*---------------------------------------------------------------------*/
(define (add-quit-hook! fun::procedure)
   (set! *quit-hooks* (cons fun *quit-hooks*)))

;*---------------------------------------------------------------------*/
;*    engine ...                                                       */
;*---------------------------------------------------------------------*/
(define (engine repl)
   ;; Bee acknowledgement
   (if *bee-client?* (bee-notify "INITIALIZATION"))
   ;; we setup the signal handling (that is C-c)
   (init-signal-handler!)
   ;; we have to register that each time a new run is commited, we have
   ;; to reset the gdb annotate register
   (add-run-hook! gdb-annotate-run-hook)
   ;; we have to check if the application has been linked againsted bdb lib.
   ;; if not we disbable scheme modes
   (add-run-hook! (lambda ()
		     (let* ((cmd "info address bdb_demangle")
			    (res (gdb-server->string cmd)))
			(if (or (string=? res "") (substring=? "No " res 3))
			    (begin
			       (set! *bdb-scheme-mode-enabled?* #f)
			       (if (not (eq? *bdb-mode* 'c))
				   (begin
				      (set! *bdb-mode* 'c)
				      (bdb-warning
				       #"Bdb facilities unavailable\n"
				       "Disabling `scheme' mode -- "
				       #"See -gbdb Bigloo option\n"))))
			    (set! *bdb-scheme-mode-enabled?* #t)))))
   ;; if we are debugging a Bigloo program, we add an run hook in order
   ;; to set a breakpoint in the error notifying function
   (add-run-hook! (lambda ()
		     (if (memq *bdb-mode* '(scheme mixte))
			 (let ((cli (string-append "clear " *bigloo-failure*))
			       (li (string-append "break " *bigloo-failure*)))
			    ;; we start erasing the break if already set
			    (gdb-noerror-call->string cli)
			    ;; we must not register that breakpoint in the
			    ;; history list because run always sets it!
			    (gdb-noerror-call->string li)))))
   ;; try to read the etags description
   (read-etags-file)
   ;; we prepare for the exit machinery
   (bind-exit (exit)
      (set! *bdb-exit* exit)
      ;; then we start the gdb process
      (if (gdb-start)
	  (unwind-protect
	     (begin
		;; before entering the repl, we have to execute
		;; all the pre hooks and the post hooks in order
		;; to ensure a correct initialization.
		(run-hooks *pre-command-hook*)
		(run-hooks *post-command-hook*)
		(repl))
	     (begin
		(process-send-signal (get-gdb-process) sigint)
		(gdb-call->string "kill")
		(gdb-stop)
		(run-hooks *quit-hooks*)
		(close-server!))))))

;*---------------------------------------------------------------------*/
;*    *bdb-exit* ...                                                   */
;*    -------------------------------------------------------------    */
;*    This variable is set when the repl is started. It is a pointer   */
;*    to a lexical escape from the repl or the Bigloo exit function.   */
;*---------------------------------------------------------------------*/
(define *bdb-exit* #f)

;*---------------------------------------------------------------------*/
;*    bdb-exit ...                                                     */
;*---------------------------------------------------------------------*/
(define (bdb-exit val)
   (close-bdb-log)
   (if (procedure? *bdb-exit*)
       (*bdb-exit* val)
       (*exit* val)))

;*---------------------------------------------------------------------*/
;*    init-signal-handler! ...                                         */
;*---------------------------------------------------------------------*/
(define (init-signal-handler!)
   (let ((kill-my-self (lambda (n)
                          (bdb-error "engine"
				     "Illegal signal caught, aborting"
				     n)
                          (exit -1))))
      (signal sigfpe kill-my-self)
      (signal sigill kill-my-self)
      (signal sigbus kill-my-self)
      (signal sigsegv kill-my-self)
      (signal sigint (lambda (signal) 'ignore))))
   
