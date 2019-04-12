;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Tools/error.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 10:47:51 1994                          */
;*    Last change :  Fri Apr 12 11:04:42 2019 (serrano)                */
;*    Copyright   :  1994-2019 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Error utilities                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_error
   (include "Tools/location.sch"
	    "Ast/node.sch")
   (option  (set! *compiler-debug* 0))
   (import  engine_pass
	    engine_param
	    tools_location
	    tools_trace
	    tools_shape
	    init_main)
   (export  *nb-error-on-pass*
	    (internal-error <obj> <obj> <obj>)
	    (user-warning <obj> <obj> <obj>)
	    (user-warning/location ::obj <obj> <obj> <obj>)
	    (user-error-notify ::obj ::symbol)
	    (user-error <obj> <obj> <obj> . <obj>)
	    (user-error/location ::obj <obj> <obj> <obj> . <obj>)
	    (current-function)
	    (enter-function ::symbol)
	    (leave-function)
	    (no-warning thunk)))
 
;*---------------------------------------------------------------------*/
;*    *nb-error-on-pass* ...                                           */
;*---------------------------------------------------------------------*/
(define *nb-error-on-pass* 0)

;*---------------------------------------------------------------------*/
;*    internal-error ...                                               */
;*---------------------------------------------------------------------*/
(define (internal-error proc mes obj)
   (if (output-port? *trace-port*)
       (fprint *trace-port* "*** ERROR: " proc ":" mes ":" obj))
   (fprint (current-error-port)
	   "*** INTERNAL-ERROR in pass: " *current-pass*)
   (fprint (current-error-port)
	   "(Would you, please, send this error report and the source file to"
	   #\Newline
	   *bigloo-author* " [" *bigloo-email* "], thank you.)")
   (error proc mes obj)
   (compiler-exit 1))

;*---------------------------------------------------------------------*/
;*    with-dump-stack ...                                              */
;*---------------------------------------------------------------------*/
(define (with-dump-stack thunk)
   (if *compiler-stack-debug?*
       (thunk)
       (let ((st (bigloo-trace-stack-depth)))
	  (bigloo-trace-stack-depth-set! 0)
	  (unwind-protect
	     (thunk)
	     (bigloo-trace-stack-depth-set! st)))))
   
;*---------------------------------------------------------------------*/
;*    user-warning/location ...                                        */
;*---------------------------------------------------------------------*/
(define (user-warning/location loc proc mes obj)
   (when (>fx (bigloo-warning) 0)
      (with-dump-stack
	 (lambda ()
	    (if (not (location? loc))
		(warning proc mes " -- " obj)
		(warning/location (location-full-fname loc)
		   (location-pos loc) proc
		   mes " -- " obj))))))
   
;*---------------------------------------------------------------------*/
;*    user-warning ...                                                 */
;*---------------------------------------------------------------------*/
(define (user-warning proc mes obj)
   (user-warning/location (find-location obj) proc mes obj))

;*---------------------------------------------------------------------*/
;*    no-warning ...                                                   */
;*---------------------------------------------------------------------*/
(define (no-warning thunk)
   (let ((warning (bigloo-warning)))
      (bigloo-warning-set! 0)
      (let ((val (thunk)))
	 (bigloo-warning-set! warning)
	 val)))

;*---------------------------------------------------------------------*/
;*    user-error-notify ...                                            */
;*---------------------------------------------------------------------*/
(define (user-error-notify e proc)
   (when (isa? e &error)
      (with-dump-stack
	 (lambda ()
	    (with-access::&error e (obj (p proc))
	       (let ((loc (find-location obj)))
		  (set! *nb-error-on-pass* (+fx *nb-error-on-pass* 1))
		  (if (location? loc)
		      (error-notify (duplicate::&error e
				       (proc (or p proc))
				       (fname (location-full-fname loc))
				       (location (location-pos loc))))
		      (begin
			 (unless p (set! p proc))
			 (error-notify e)))))))))

;*---------------------------------------------------------------------*/
;*    user-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (user-error proc mes obj . continue)
   (if (pair? continue)
       (user-error/location (find-location obj) proc mes (shape obj)
			    (car continue))
       (user-error/location (find-location obj) proc mes (shape obj))))

;*---------------------------------------------------------------------*/
;*    user-error/location ...                                          */
;*---------------------------------------------------------------------*/
(define (user-error/location loc proc msg obj . continue)
   (with-dump-stack
      (lambda ()
	 (if (output-port? *trace-port*)
	     (fprint *trace-port*  "*** ERROR:" proc ":" msg ":" obj))
	 (set! *nb-error-on-pass* (+fx *nb-error-on-pass* 1))
	 (let* ((proc-string (cond
				((string? proc)
				 proc)
				((symbol? proc)
				 (symbol->string proc))
				(else
				 #f)))
		(fun-string (symbol->string (current-function)))
		(proc (if (and (string? proc-string)
			       (not (string=? proc-string fun-string)))
			  (string-append fun-string ":" proc-string)
			  fun-string))
		(objprn (let ((port (open-output-string)))
			   (display-circle obj port)
			   (let ((string (close-output-port port)))
			      (if (>fx (string-length string) 45)
				  (string-append (substring string 0 44) " ...")
				  string)))))
	    (bind-exit (skip)
	       (with-exception-handler
		  (lambda (e)
		     (error-notify e)
		     (if (pair? continue)
			 (skip (car continue))
			 (exit 1)))
		  (lambda ()
		     (if (location? loc)
			 (let ((fname (location-full-fname loc))
			       (pos (location-pos loc)))
			    (error/location proc msg objprn fname pos))
			 (error proc msg objprn)))))))))
		 
;*---------------------------------------------------------------------*/
;*    *sfun-stack*                                                     */
;*---------------------------------------------------------------------*/
(define *sfun-stack* '(top-level))

;*---------------------------------------------------------------------*/
;*    enter-function ...                                               */
;*---------------------------------------------------------------------*/
(define (enter-function var)
   (set! *sfun-stack* (cons var *sfun-stack*)))

;*---------------------------------------------------------------------*/
;*    leave-function ...                                               */
;*---------------------------------------------------------------------*/
(define (leave-function)
   (set! *sfun-stack* (cdr *sfun-stack*)))

;*---------------------------------------------------------------------*/
;*    current-function ...                                             */
;*---------------------------------------------------------------------*/
(define (current-function)
   (car *sfun-stack*))

;*---------------------------------------------------------------------*/
;*    sharp-symbol ...                                                 */
;*---------------------------------------------------------------------*/
(define sharp-symbol (string->symbol "#"))
