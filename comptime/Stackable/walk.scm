;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Stackable/walk.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 12 08:33:01 2020                          */
;*    Last change :  Sun Jul 12 08:33:02 2020 (serrano)                */
;*    Copyright   :  2020-21 Manuel Serrano                            */
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
	    ast_occur
	    engine_param)
   (export  (stackable-walk! globals)))

;*---------------------------------------------------------------------*/
;*    stackable-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (stackable-walk! globals)
   (if *optim-stackable?*
       (begin
	  (pass-prelude "Stackable")
	  (for-each (lambda (g)
		       (let ((sfun (variable-value g)))
			  (stackable (sfun-body sfun) '())))
	     globals)
	  (pass-postlude globals))
       globals))

;*---------------------------------------------------------------------*/
;*    stackable ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::node env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    stackable ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::app env)
   (let ((f (variable-value (var-variable (app-fun node))))
	 (args (app-args node)))
      (if (isa? f fun)
	  (with-access::fun f (args-noescape)
	     (cond
		((eq? args-noescape '*)
		 (for-each (lambda (a) (mark-stackable! a #t env)) args))
		((pair? args-noescape)
		 (let loop ((i 0)
			    (args args))
		    (when (pair? args)
		       (mark-stackable! (car args) (memq i args-noescape) env)
		       (loop (+fx i 1) (cdr args)))))
		(else
		 (for-each (lambda (a) (mark-stackable! a #f env)) args))))
	  (for-each (lambda (a) (mark-stackable! a #f env)) args))))

;*---------------------------------------------------------------------*/
;*    stackable ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (stackable node::closure env)
   (mark-stackable! node #f env))

;*---------------------------------------------------------------------*/
;*    stackable ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (stackable node::let-var env)
   
   (define (read-only-and-unique? binding)
      (with-access::local (car binding) (access occurrence)
	 (and (eq? access 'read) (=fx occurrence 1))))
      
   (with-access::let-var node (bindings body)
      (stackable body (append (filter read-only-and-unique? bindings) env))))

;*---------------------------------------------------------------------*/
;*    mark-stackable! ::node ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (mark-stackable! node::node stackablep::bool env)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    mark-stackable! ::closure ...                                    */
;*---------------------------------------------------------------------*/
(define-method (mark-stackable! node::closure stackablep env)
   (let* ((v (var-variable node))
	  (f (variable-value v)))
      (with-access::sfun f (stackable)
	 (if stackablep
	     (set! stackable (and stackable stackablep))
	     (set! stackable #f)))))

;*---------------------------------------------------------------------*/
;*    mark-stackable! ::var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (mark-stackable! node::var stackablep env)
   (with-access::var node (variable)
      (when (and stackablep (isa? variable local))
;* 	 (with-access::variable variable (occurrence access)           */
;* 	    (when (and (eq? access 'read) (=fx occurrence 1))          */
;* 	       (let ((bind (assq variable env)))                       */
;* 		  (tprint "v=" (shape variable) " occ=" occurrence " ax=" access */
;* 		     " b=" (typeof bind))                              */
;* 		  (when (pair? bind)                                   */
;* 		     (tprint (node->sexp (cdr bind)))))))              */
	 #unspecified)))
