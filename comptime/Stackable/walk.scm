;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Stackable/walk.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 12 08:33:01 2020                          */
;*    Last change :  Sun Jul 12 08:33:02 2020 (serrano)                */
;*    Copyright   :  2020 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Mark "stackable" expressions, that is expression that can        */
;*    possibly be stack allocated.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module stackable_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_misc
	    type_env
	    object_class
	    ast_var
	    ast_node
	    ast_env
	    ast_dump
	    ast_walk
	    engine_param)
   (export  (stackable-walk! globals)))

;*---------------------------------------------------------------------*/
;*    stackable-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (stackable-walk! globals)
   (pass-prelude "Stackable")
   (when *optim-stackable?*
      (for-each (lambda (g)
		   (let ((sfun (variable-value g)))
		      (stackable (sfun-body sfun))))
	 globals))
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    stackable ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    stackable ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::app)
   (let ((f (variable-value (var-variable (app-fun node))))
	 (args (app-args node)))
      (if (isa? f fun)
	  (with-access::fun f (args-noescape)
	     (cond
		((eq? args-noescape '*)
		 (for-each (lambda (a) (mark-stackable! a #t)) args))
		((pair? args-noescape)
		 (let loop ((i 0)
			    (args args))
		    (when (pair? args)
		       (mark-stackable! (car args) (memq i args-noescape))
		       (loop (+fx i 1) (cdr args)))))
		(else
		 for-each (lambda (a) (mark-stackable! a #f)) args)))
	  (for-each (lambda (a) (mark-stackable! a #f)) args))))

;*---------------------------------------------------------------------*/
;*    stackable ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (stackable node::closure)
   (mark-stackable! node #f))

;*---------------------------------------------------------------------*/
;*    mark-stackable! ::node ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (mark-stackable! node::node stackablep::bool)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mark-stackable! ::closure ...                                    */
;*---------------------------------------------------------------------*/
(define-method (mark-stackable! node::closure stackablep)
   (let* ((v (var-variable node))
	  (f (variable-value v)))
      (with-access::sfun f (stackable)
	 (if stackablep
	     (set! stackable (and stackable stackablep))
	     (set! stackable #f)))))
