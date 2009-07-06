;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Command/backtrace.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 21:34:02 1999                          */
;*    Last change :  Thu Aug 10 08:12:08 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The backtrace command                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_backtrace
   (import tools_error
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
   (export (display-stack-frames ::bstring)
	   (frame=? ::obj ::obj)
	   (reset-gc-stacks!)
	   (add-gc-stack! stack)
	   (get-gc-stack num::int)))

;*---------------------------------------------------------------------*/
;*    *display-frame-armed?* ...                                       */
;*---------------------------------------------------------------------*/
(define *display-frame-armed?* #f)

;*---------------------------------------------------------------------*/
;*    display-stack-frames ...                                         */
;*---------------------------------------------------------------------*/
(define (display-stack-frames cmd)
   ;; before getting a new stack we have to reset the current one
   (gdb-annotate-reset-stack-frames!)
   ;; we store the current stack frame
   (let ((frame (gdb-annotate-current-frame))
	 (fun (gdb-annotate-current-function)))
      ;; then we emit the command
      (gdb-call->string cmd)
      (let ((stack (gdb-annotate-stack-frames)))
	 ;; and now we can display the stack
	 (if (not *display-frame-armed?*)
	     (begin
		(set! *display-frame-armed?* #t)
		(for-each (lambda (f)
			     (console-frame f 'long)
			     (console-newline))
			  (reverse (cdr stack)))
		(console-frame (car stack) 'long)
		(set! *display-frame-armed?* #f)))
	 ;; we have to restore the current stack frame
	 (gdb-annotate-current-function-set! fun)
	 (gdb-annotate-current-frame-set! frame))))
 
;*---------------------------------------------------------------------*/
;*    frame=? ...                                                      */
;*    -------------------------------------------------------------    */
;*    This function returns #t iff the two stack frames F1 and F2      */
;*    are the same stack frame                                         */
;*---------------------------------------------------------------------*/
(define (frame=? f1 f2)
   (equal? f1 f2))

;*---------------------------------------------------------------------*/
;*    *gc-stacks* ...                                                  */
;*    -------------------------------------------------------------    */
;*    All the following definitions are used to automatically          */
;*    get an excerpt of the execution stack during during each         */
;*    GCs. These functions are used by the heap statistics visualizer. */
;*---------------------------------------------------------------------*/
(define *gc-stacks* '())

;*---------------------------------------------------------------------*/
;*    reset-gc-stacks! ...                                             */
;*---------------------------------------------------------------------*/
(define (reset-gc-stacks!)
   (set! *gc-stacks* '()))

;*---------------------------------------------------------------------*/
;*    add-gc-stack! ...                                                */
;*---------------------------------------------------------------------*/
(define (add-gc-stack! stack)
   (set! *gc-stacks* (cons stack *gc-stacks*)))
   
;*---------------------------------------------------------------------*/
;*    get-gc-stack ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-gc-stack num::int)
   (list-ref *gc-stacks* (-fx (length *gc-stacks*) num)))
   
;*---------------------------------------------------------------------*/
;*    *backtrace-help* ...                                             */
;*---------------------------------------------------------------------*/
(define *backtrace-help* 
   "Print backtrace of all stack frames, or innermost COUNT frames.
With a negative argument, print outermost -COUNT frames.")

;*---------------------------------------------------------------------*/
;*    The backtrace command                                            */
;*---------------------------------------------------------------------*/
(let ((action (lambda (cmd source level env)
		 (display-stack-frames source))))
   ;; backtrace
   (bind-toplevel-command! "backtrace"
			   2
			   action
			   *backtrace-help*)
   ;; bt
   (bind-toplevel-command! "bt"
			   2
			   action
			   *backtrace-help*))

	   


