;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/whatis.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug  4 11:13:35 1999                          */
;*    Last change :  Wed Aug  9 17:06:38 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The handling of gdb DISPLAYs.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_whatis
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
   (export (bigloo-type?::bool ::bstring)
	   (update-kbdb-classes)))

;*---------------------------------------------------------------------*/
;*    update-kbdb-classes ...                                          */
;*---------------------------------------------------------------------*/
(define (update-kbdb-classes)
   ;; when running in Bigloo mode, after the run command, we have to
   ;; download the classes table in order to correctly display
   ;; object instances. This hook is called every time but the true body
   ;; of the test is exectuted at most once.
   (if (eq? *bigloo-classes* #unspecified)
       (if (memq *bdb-mode* '(scheme mixte))
	   (if (gdb-annotate-running?)
	       ;; we fetch the classes only when the program is running...
	       (set! *bigloo-classes* (patient-call "bdb_dump_classes")))
	   (set! *bigloo-classes* '()))))

;*---------------------------------------------------------------------*/
;*    bigloo-type? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Is TNAME a Bigloo type ?                                         */
;*---------------------------------------------------------------------*/
(define (bigloo-type? tname::bstring)
   (or (member tname *bigloo-type*)
       (member tname *bigloo-classes*)))
   
;*---------------------------------------------------------------------*/
;*    whatis-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (whatis-parser cmd-list source level env)
   (if (null? cmd-list)
       (console-echo (gdb-call->string source))
       (let* ((bdb-expr (string-from-at source 1 #\space 1))
	      (gdb-expr (bdb-expr->gdb-expr bdb-expr
					    (gdb-annotate-current-function))))
	  ;; if bdb-expr and gdb-expr are the same
	  ;; it is a real whatis
	  (if (string=? bdb-expr gdb-expr)
	      (console-echo (gdb-call->string source))
	      ;; we can't just ask the patient for the type of the expression
	      ;; because it could be that this expression is not an obj
	      ;; type. In consequence, we have to check this first
	      (let* ((cmd (string-append "whatis " gdb-expr))
		     (out (gdb-call->string cmd))
		     (sty (string-from out #\= 2))
		     (ty  (string-until sty #\Newline 0)))
		 (if (bigloo-type? ty)
		     ;; they differ, it is a Bigloo whatis
		     (let ((val (patient-call "bdb_whatis" gdb-expr)))
			(console-echo val)
			(console-newline))
		     (console-echo out)))))))

;*---------------------------------------------------------------------*/
;*    *whatis-help* ...                                                */
;*---------------------------------------------------------------------*/
(define *whatis-help*
   "Print data type of expression EXP.")
		  
;*---------------------------------------------------------------------*/
;*    The whatis command                                               */
;*---------------------------------------------------------------------*/
(bind-toplevel-command! "whatis"
			3
			whatis-parser
			*whatis-help*)
			      
	   
       
	   
   
