;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/apply.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 17:21:26 1995                          */
;*    Last change :  Mon Mar 28 14:12:51 2011 (serrano)                */
;*    Copyright   :  1995-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `apply' coercion                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_apply
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import  engine_param
	    backend_backend
	    tools_shape
	    tools_location
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    ast_lvtype
	    coerce_coerce
	    coerce_convert))

;*---------------------------------------------------------------------*/
;*    coerce! ::app-ly ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::app-ly caller to safe)
   (trace coerce "coerce-apply!: " (shape node) #\Newline)
   (let ((error-msg (list 'quote (shape node))))
      ;; we coerce the arguments
      (app-ly-arg-set! node (coerce! (app-ly-arg node) caller *obj* safe))
      ;; we coerce the procedure
      (let ((c-fun (coerce! (app-ly-fun node) caller *procedure* safe)))
	 ;; we check arity
	 (if *unsafe-arity*
	     (begin
		(if (var? c-fun)
		    (begin
		       (app-ly-fun-set! node c-fun)
		       (convert! node *obj* to safe))
		    (let ((fun (make-local-svar 'fun *procedure*)))
		       (app-ly-fun-set! node (instantiate::var
						(loc (node-loc c-fun))
						(type *procedure*)
						(variable fun)))
		       (instantiate::let-var
			  (loc (node-loc node))
			  (type (strict-node-type to (node-type node)))
			  (bindings (list (cons fun c-fun)))
			  (body (convert! node *obj* to safe))))))
	     (let* ((fun (make-local-svar 'fun *procedure*))
		    (val (make-local-svar 'val *pair-nil*))
		    (loc (node-loc node))
		    (lval (lvtype-node (top-level-sexp->node `(length ,val) loc)))
		    (len (gensym 'len))
		    (body (lvtype-node
			   (top-level-sexp->node
			    `(let ((,(symbol-append len '::int)
				    ,(coerce! lval caller *int* safe)))
				(if (correct-arity? ,fun ,len)
				    ,(convert! node *obj* to safe)
				    ,(make-error-node error-msg
						      loc
						      caller 
						      to)))
			    loc)))
		    (lnode (instantiate::let-var
			      (loc loc)
			      (type (strict-node-type (node-type body) *obj*))
			      (bindings (list (cons fun c-fun)
					      (cons val (app-ly-arg node))))
			      (body body))))
		   ;; we set the new apply value
		(app-ly-fun-set! node (instantiate::var
					(loc loc)
					(type *procedure*)
					(variable fun)))
		(app-ly-arg-set! node (instantiate::var
					(loc loc)
					(type (strict-node-type
					       (variable-type val) *obj*))
					(variable val)))
		lnode)))))

;*---------------------------------------------------------------------*/
;*    make-error-node ...                                              */
;*---------------------------------------------------------------------*/
(define (make-error-node error-msg loc caller to)
   (let ((node (top-level-sexp->node 
		(if (and (or (and (>fx *bdb-debug* 0)
				  (memq 'bdb
					(backend-debug-support
					 (the-backend))))
			     (>fx *compiler-debug* 0))
			 (location? loc))
		    `(begin
			((@ error/location __error)
			 ,(list 'quote (current-function))
			 "Wrong number of arguments"
			 ,error-msg
			 ,(location-full-fname loc)
			 ,(location-pos loc))
			(failure '_ '_ '_))
		    `(failure ,(list 'quote (current-function))
			      "Wrong number of arguments"
			      ,error-msg))
		loc)))
      (coerce! node caller to #f)))

