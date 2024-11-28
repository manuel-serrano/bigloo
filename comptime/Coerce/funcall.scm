;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Coerce/funcall.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 17:21:26 1995                          */
;*    Last change :  Thu Nov 28 12:38:08 2024 (serrano)                */
;*    Copyright   :  1995-2024 Manuel Serrano, see LICENSE file        */
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
	    type_typeof
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    ast_ident
	    ast_lvtype
	    coerce_coerce
	    coerce_convert))

;*---------------------------------------------------------------------*/
;*    coerce! ::funcall ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::funcall caller to safe)
   (trace coerce "coerce-funcall!: " (shape node) #\Newline)
   (let ((error-msg (list 'quote (shape node)))
	 (strength  (funcall-strength node))
	 (nty (node-type node)))
;*       (tprint "COERCE-FUNCALL node=" (shape node) " " strength      */
;* 	 " nty=" (shape nty) " to=" (shape to)                         */
;* 	 " gettype=" (shape (get-type node #f)))                       */
      (case strength
	 ((elight)
	  (if *optim-cfa-unbox-closure-args*
	      (coerce-funcall-elight-args! node caller to safe)
	      (coerce-funcall-args! node caller to safe))
	  ;; convert the call
	  (convert! node nty to safe))
	 ((light)
	  ;; convert the arguments
	  (if *optim-cfa-unbox-closure-args*
	      (coerce-funcall-light-args! node caller to safe)
	      (coerce-funcall-args! node caller to safe))
	  ;; convert the call
	  (convert! node nty to safe))
	 (else
	  ;; we coerce the procedure
	  (let ((c-fun (coerce! (funcall-fun node) caller *procedure* safe)))
	     ;; we coerce the arguments
	     (coerce-funcall-args! node caller to safe)
	     ;; we check arity
	     (if *unsafe-arity*
		 (begin
		    (funcall-fun-set! node c-fun)
		    (convert! node nty to safe))
		 (let* ((fun    (make-local-svar 'fun *procedure*))
			(loc    (node-loc node))
			(len    (instantiate::literal
				   (loc loc)
				   (type *int*)
				   (value (-fx (length (funcall-args node)) 2))))
			(a-len  (mark-symbol-non-user! (gensym 'len)))
			(a-tlen (mark-symbol-non-user!
				   (symbol-append a-len '::int)))
			(lnode  (instantiate::let-var
				   (loc loc)
				   (type (strict-node-type to nty))
				   (bindings (list (cons fun c-fun)))
				   (body (top-level-sexp->node
					    `(let ((,a-tlen ,len))
						(if (correct-arity? ,fun ,a-len)
						    ,(cast-if-not-obj loc nty to
							(convert! node nty to safe))
						    ,(make-arity-error-node
							fun
							error-msg
							loc
							caller
							to)))
					    loc)))))
		    (funcall-fun-set! node
		       (instantiate::ref
			  (loc loc)
			  (type (strict-node-type
				   (variable-type fun) *obj*))
			  (variable fun)))
		    (lvtype-node! lnode)
		    lnode)))))))

;*---------------------------------------------------------------------*/
;*    cast-if-not-obj ...                                              */
;*---------------------------------------------------------------------*/
(define (cast-if-not-obj loc from to node)
   (if (eq? from *obj*)
       node
       (with-access::funcall node (type)
	  (set! type *obj*)
	  (instantiate::cast
	     (loc loc)
	     (type to)
	     (arg node)))))

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
      (lvtype-node! node)
      (coerce! node caller to #f)))

;*---------------------------------------------------------------------*/
;*    coerce-funcall-args! ...                                         */
;*---------------------------------------------------------------------*/
(define (coerce-funcall-args! node caller to safe)
   
   (define (toplevel-exp node)
      (let ((n (top-level-sexp->node '__eoa__ (node-loc node))))
	 (lvtype-node! n)
	 n))
   
   (if (null? (funcall-args node))
       (funcall-args-set! node (list (toplevel-exp node)))
       (let loop ((actuals (funcall-args node))
		  (prev    'dummy))
	  (if (null? actuals)
	      (set-cdr! prev (list (toplevel-exp node)))
	      (begin
		 (set-car! actuals (coerce! (car actuals) caller *obj* safe))
		 (loop (cdr actuals) actuals))))))

;*---------------------------------------------------------------------*/
;*    coerce-funcall-elight-args! ...                                  */
;*---------------------------------------------------------------------*/
(define (coerce-funcall-elight-args! node caller to safe)
   
   (define (toplevel-exp node)
      (let ((n (top-level-sexp->node '__eoa__ (node-loc node))))
	 (lvtype-node! n)
	 n))
   
   (if (null? (funcall-args node))
       (funcall-args-set! node (list (toplevel-exp node)))
       (let ((callee (var-variable (car (funcall-functions node)))))
;* 	  (tprint "N=" (shape node) " to=" (shape to))                 */
	  (let loop ((actuals (funcall-args node))
		     (formals (sfun-args (variable-value callee)))
		     (prev 'dummy))
	     (if (null? actuals)
		 (set-cdr! prev (list (toplevel-exp node)))
		 (begin
		    (set-car! actuals
		       (coerce! (car actuals) caller (local-type (car formals))
			  safe))
		    (loop (cdr actuals) (cdr formals) actuals)))))))

;*---------------------------------------------------------------------*/
;*    coerce-funcall-light-args! ...                                   */
;*---------------------------------------------------------------------*/
(define (coerce-funcall-light-args! node caller to safe)
   (coerce-funcall-elight-args! node caller to safe))
