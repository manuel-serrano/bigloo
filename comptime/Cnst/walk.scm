;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cnst/walk.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb  3 09:46:40 1995                          */
;*    Last change :  Tue Jul 12 11:31:44 2011 (serrano)                */
;*    Copyright   :  1995-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `constant compilation'                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cnst_walk
   (include "Engine/pass.sch"
            "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    ast_var
	    ast_node
	    ast_remove
	    cnst_alloc
	    cnst_cache
	    cnst_node
	    cnst_initialize)
   (export  (cnst-walk! globals)))

;*---------------------------------------------------------------------*/
;*    cnst-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (cnst-walk! globals)
   (pass-prelude "Cnst" start-cnst-cache! start-cnst-alloc! unsafe!)
   (verbose 2 "      [" *init-mode* #\] #\Newline)
   (for-each (lambda (global)
		(trace cnst (shape global) #\Newline)
		(enter-function (global-id global))
		(let* ((fun (global-value global))
		       (new-body (cnst! (sfun-body fun))))
		   (sfun-body-set! fun new-body))
		(leave-function))
	     globals)
   (pass-postlude (remove-var 'cnst (append (initialize-ast) globals))
		  stop-cnst-alloc!
		  safe!))

;*---------------------------------------------------------------------*/
;*    unsafe! ...                                                      */
;*---------------------------------------------------------------------*/
(define (unsafe!)
   (set! *old-unsafe-type* *unsafe-type*)
   (set! *unsafe-type* #t)
   (set! *old-unsafe-arity* *unsafe-arity*)
   (set! *unsafe-arity* #t)
   (set! *old-unsafe-range* *unsafe-range*)
   (set! *unsafe-range* #t)
   (set! *old-unsafe-struct* *unsafe-struct*)
   (set! *unsafe-struct* #t)
   (set! *old-unsafe-version* *unsafe-version*)
   (set! *unsafe-version* #t))

;*---------------------------------------------------------------------*/
;*    safe! ...                                                        */
;*---------------------------------------------------------------------*/
(define (safe!)
   (set! *unsafe-type* *old-unsafe-type*)
   (set! *unsafe-arity* *old-unsafe-arity*)
   (set! *unsafe-range* *old-unsafe-range*)
   (set! *unsafe-struct* *old-unsafe-struct*)
   (set! *unsafe-version* *old-unsafe-version*))

;*---------------------------------------------------------------------*/
;*    old-unsafes                                                      */
;*---------------------------------------------------------------------*/
(define *old-unsafe-type* #f)
(define *old-unsafe-arity* #f)
(define *old-unsafe-range* #f)
(define *old-unsafe-struct* #f)
(define *old-unsafe-version* #f)

