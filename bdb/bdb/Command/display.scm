;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Command/display.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug  4 11:13:35 1999                          */
;*    Last change :  Thu Aug 10 10:28:48 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The handling of gdb DISPLAYs.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_display
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
	   command_command
	   command_print)
   (export (display-display-table! disp)
	   (display->string disp)
	   (display-number disp)
	   (set-display-display-function! ::procedure)))

;*---------------------------------------------------------------------*/
;*    bdb-print-expr? ...                                              */
;*---------------------------------------------------------------------*/
(define (bdb-print-expr? expr)
   ;; this is a rough implementation. we consider expr to be
   ;; a bdb expression as soon as it is a call to a static non
   ;; debugged function.
   (let ((pref "(char *) ({<text variable, no debug info>}"))
      (substring=? pref expr (string-length pref))))

;*---------------------------------------------------------------------*/
;*    bdb-print-expr ...                                               */
;*---------------------------------------------------------------------*/
(define (bdb-print-expr expr)
   ;; we have to fetch the expression inside the last parenthesis
   ;; and to demangle it. this is not 100% correct because we don't
   ;; make a specical case of strings.
   (let* ((len  (string-length expr))
	  (stop (let loop ((i (-fx len 1))
			   (clo -1))
		   ;; the format has changed in GDB 4.18 now, the expression
		   ;; is followed by a comma and the display number.
		   (cond
		      ((=fx i 0)
		       clo)
		      ((char=? (string-ref expr i) #\))
		       (loop (-fx i 1) (if (<fx clo 0) i clo)))
		      ((char=? (string-ref expr i) #\,)
		       i)
		      (else
		       (loop (-fx i 1) clo))))))
      (let loop ((i (-fx stop 1))
		 (p 1))
	 (cond
	    ((=fx i -1)
	     ;; strange. this should be an error...
	     expr)
	    ((char=? (string-ref expr i) #\()
	     (if (=fx p 1)
		 ;; we got it
		 (gdb-expr->bdb-expr (substring expr (+fx i 1) stop))
		 ;; not yet...
		 (loop (-fx i 1) (-fx p 1))))
	    ((char=? (string-ref expr i) #\))
	     (loop (-fx i 1) (+fx p 1)))
	    (else
	     (loop (-fx i 1) p))))))

;*---------------------------------------------------------------------*/
;*    *display-table* ...                                              */
;*---------------------------------------------------------------------*/
(define *display-table* '())

;*---------------------------------------------------------------------*/
;*    *display-register-mode* ...                                      */
;*---------------------------------------------------------------------*/
(define *display-register-mode* 'echo)

;*---------------------------------------------------------------------*/
;*    *display-display* ...                                            */
;*---------------------------------------------------------------------*/
(define *display-display* console-display-display)

;*---------------------------------------------------------------------*/
;*    set-display-display-function! ...                                */
;*---------------------------------------------------------------------*/
(define (set-display-display-function! fun)
   (set! *display-display* fun))

;*---------------------------------------------------------------------*/
;*    display-number ...                                               */
;*---------------------------------------------------------------------*/
(define (display-number disp)
   (match-case disp
      ((?num ?fmt ?expr ?value)
       (string->integer num))
      (else
       0)))
   
;*---------------------------------------------------------------------*/
;*    display->string ...                                              */
;*---------------------------------------------------------------------*/
(define (display->string disp)
   (match-case disp
      ((?num ?fmt ?expr ?value)
       (if (bdb-print-expr? expr)
	   (string-append num ": "
			  (bdb-print-expr expr) " = "
			  (gdb-value->bdb-value value))
	   (string-append num ": " expr " = " value)))
      (else
       "")))
   
;*---------------------------------------------------------------------*/
;*    console-display-display ...                                      */
;*---------------------------------------------------------------------*/
(define (console-display-display disp)
   (define (display-display disp)
      (console-echo (display->string disp))
      (console-newline))
   (for-each display-display disp))
	 
;*---------------------------------------------------------------------*/
;*    display-display-table! ...                                       */
;*---------------------------------------------------------------------*/
(define (display-display-table! disp)
   (case *display-register-mode*
      ((eager)
       (console-display-display disp))
      ((echo)
       (*display-display* disp))))

;*---------------------------------------------------------------------*/
;*    display-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (display-parser cmd-list source level env)
   (let ((mode *display-register-mode*))
      (set! *display-register-mode* 'eager)
      (if (null? cmd-list)
	  (console-echo (gdb-call->string source))
	  (let* ((bdb-expr (string-from-at source 1 #\space 1))
		 (gdb-expr (bdb-expr->gdb-expr
			    bdb-expr
			    (gdb-annotate-current-function))))
	     ;; if bdb-expr and gdb-expr are the same
	     ;; it is a real displaying...
	     (if (string=? bdb-expr gdb-expr)
		 (console-echo (gdb-call->string source))
		 ;; they differ, it is a Bigloo display
		 (let* ((cmd (string-append "display (char *)bdb_print( "
					    gdb-expr
					    (if *write-circle?*
						", 1"
						", 0")
					    " )")))
		    (console-echo (gdb-call->string cmd))))))
      (set! *display-register-mode* mode)))

;*---------------------------------------------------------------------*/
;*    *display-help* ...                                               */
;*---------------------------------------------------------------------*/
(define *display-help*
   "Print value of expression EXP each time the program stops.")

;*---------------------------------------------------------------------*/
;*    *cdisplay-help* ...                                              */
;*---------------------------------------------------------------------*/
(define *cdisplay-help*
   "Print value of expression EXP each time the program stops.
/FMT may be used before EXP as in the \"print\" command.
/FMT \"i\" or \"s\" or including a size-letter is allowed,
as in the \"x\" command, and then EXP is used to get the address to examine
and examining is done as in the \"x\" command.

With no argument, display all currently requested auto-display expressions.
Use \"undisplay\" to cancel display requests previously made.")
   
;*---------------------------------------------------------------------*/
;*    The display command                                              */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "display"
			4
			display-parser
			*display-help*)

;*---------------------------------------------------------------------*/
;*    The cdisplay command                                             */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "cdisplay"
			5
			(lambda (cmd source level env)
			   (console-echo
			    (gdb-call->string
			     (substring source 1 (string-length source)))))
			*cdisplay-help*)
