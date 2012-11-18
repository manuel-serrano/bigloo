;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/node.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:38:02 2012                          */
;*    Last change :  Sun Nov 18 10:24:32 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    SYNC2NODE, this expands a SYNC node into a plain node using      */
;*    explicitly lock/unlock and push/pop operations. Used by the      */
;*    C backend. The JVM backend should, someday, compile directly     */
;*    a synchronize block and should not use this expansion.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sync_node
   
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    ast_app
	    ast_dump
	    effect_effect
	    backend_cplib
	    sync_failsafe)
   
   (export (sync->sequence::sequence ::sync)))

;*---------------------------------------------------------------------*/
;*    lock cache                                                       */
;*---------------------------------------------------------------------*/
(define mlock #f)
(define mulock #f)
(define mpush #f)
(define mpop #f)

;*---------------------------------------------------------------------*/
;*    init-sync! ...                                                   */
;*---------------------------------------------------------------------*/
(define (init-sync! loc)
   (unless mlock
      (set! mlock (sexp->node '$mutex-lock '() loc 'app))
      (set! mulock (sexp->node '$mutex-unlock '() loc 'app))
      (set! mpush (sexp->node '(@ exitd-push-mutex! __bexit) '() loc 'app))
      (set! mpop (sexp->node '(@ exitd-pop-mutex! __bexit) '() loc 'app))
      (set-variable-name! (var-variable mlock))
      (set-variable-name! (var-variable mulock))
      (set-variable-name! (var-variable mpush))
      (set-variable-name! (var-variable mpop))))

;*---------------------------------------------------------------------*/
;*    sync->sequence ...                                               */
;*    -------------------------------------------------------------    */
;*    This function performs the following expansion:                  */
;*                                                                     */
;*      (sync m body)                                                  */
;*    =>                                                               */
;*      (begin                                                         */
;*         ($mutex-lock m)                                             */
;*         ((@ exitd-push-mutex! __bexit) m)                           */
;*         (let ((tmp body))                                           */
;*    	      ((@ exitd-pop-mutex! __bexit) m)                         */
;*    	      ($mutex-unlock m)))                                      */
;*---------------------------------------------------------------------*/
(define (sync->sequence node::sync)
   
   (define (app expr loc)
      (application->node expr '() loc 'value))

   (with-access::sync node (loc nodes mutex type)
      (init-sync! loc)
      (let* ((tmp (make-local-svar (gensym 'tmp) type))
	     (lock (app `(,mlock ,mutex) loc))
	     (push (app `(,mpush ,mutex) loc))
	     (pop (app `(,mpop ,mutex) loc))
	     (unlock (app `(,mulock ,mutex) loc))
	     (fsafe (failsafe-sync? node))
	     (vref (instantiate::var
		      (loc loc)
		      (type type)
		      (variable tmp)))
	     (sbody (if (and (pair? nodes) (null? (cdr nodes)))
			(car nodes)
			(instantiate::sequence
			   (loc loc)
			   (type type)
			   (nodes nodes))))
	     (lbody (instantiate::let-var
		       (loc loc)
		       (type type)
		       (bindings (list (cons tmp sbody)))
		       (body (instantiate::sequence
				(loc loc)
				(type type)
				(nodes (if fsafe
					   (list unlock vref)
					   (list pop unlock vref))))))))
	 (instantiate::sequence
	    (loc loc)
	    (type type)
	    (nodes (if fsafe
		       (list lock lbody)
		       (list lock push lbody)))))))



