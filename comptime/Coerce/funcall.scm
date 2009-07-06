;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/funcall.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 17:21:26 1995                          */
;*    Last change :  Fri Dec  9 11:33:26 2005 (serrano)                */
;*    Copyright   :  1995-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `funcall' coercion                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_funcall
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import  engine_param
	    backend_backend
	    tools_shape
	    tools_error
	    tools_location
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    ast_ident
	    coerce_coerce
	    coerce_convert))

;*---------------------------------------------------------------------*/
;*    coerce! ::funcall ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::funcall caller to safe)
   (trace coerce "coerce-funcall!: " (shape node) #\Newline)
   (let ((error-msg (list 'quote (shape node)))
	 (strength  (funcall-strength node)))
      ;; we coerce the arguments
      (coerce-funcall-args! node caller to safe)
      (if (memq strength '(light elight))
	  (convert! node *obj* to safe)
	  ;; we coerce the procedure
	  (let ((c-fun (coerce! (funcall-fun node) caller *procedure* safe)))
	     ;; we check arity
	     (if *unsafe-arity*
		 (begin
		    (funcall-fun-set! node c-fun)
		    (convert! node *obj* to safe))
		 (let* ((fun    (make-local-svar 'fun *procedure*))
			(loc    (node-loc node))
			(len    (instantiate::atom
				   (loc loc)
				   (type *int*)
				   (value (-fx (length (funcall-args node))
					       2))))
			(a-len  (mark-symbol-non-user! (gensym 'len)))
			(a-tlen (mark-symbol-non-user!
				 (symbol-append a-len '::int)))
			(lnode  (instantiate::let-var
				   (loc loc)
				   (type *obj*)
				   (bindings (list (cons fun c-fun)))
				   (body (top-level-sexp->node
					  `(let ((,a-tlen ,len))
					      (if (correct-arity? ,fun ,a-len)
						  ,(convert! node *obj* to safe)
						  ,(make-arity-error-node
						    fun
						    error-msg
						    loc
						    caller
						    to)))
					  loc)))))
		    (funcall-fun-set! node (instantiate::var
					      (loc loc)
					      (type *obj*)
					      (variable fun)))
		    lnode))))))

;*---------------------------------------------------------------------*/
;*    make-arity-error-node ...                                        */
;*---------------------------------------------------------------------*/
(define (make-arity-error-node fun error-msg loc caller to)
   (let ((node (top-level-sexp->node 
		(if (and (or (>fx *compiler-debug* 0)
			     (and (>fx *bdb-debug* 0)
				  (memq 'bdb
					(backend-debug-support
					 (the-backend)))))
			 (location? loc))
		    `(begin
			((@ error/location __error)
			 ,(string-append (symbol->string
					  (current-function))
					 ":Wrong number of arguments")
			 ,error-msg
			 ,fun
			 ,(location-full-fname loc)
			 ,(location-pos loc))
			(failure '_ '_ '_))
		    `(failure ,(string-append (symbol->string
					       (current-function))
					      ":Wrong number of arguments")
			      ,error-msg
			      ,fun))
		loc)))
      (coerce! node caller to #f)))

;*---------------------------------------------------------------------*/
;*    coerce-funcall-args! ...                                         */
;*---------------------------------------------------------------------*/
(define (coerce-funcall-args! node caller to safe)
   (if (null? (funcall-args node))
       (funcall-args-set! node (list (top-level-sexp->node
				      '__eoa__
				      (node-loc node))))
       (let loop ((actuals (funcall-args node))
		  (prev    'dummy))
	  (if (null? actuals)
	      (set-cdr! prev (list (top-level-sexp->node
				    '__eoa__
				    (node-loc node))))
	      (begin
		 (set-car! actuals (coerce! (car actuals) caller *obj* safe))
		 (loop (cdr actuals) actuals))))))
