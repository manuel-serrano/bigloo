;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/Globalize/loc2glo.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 27 11:39:39 1995                          */
;*    Last change :  Wed Jun 12 10:34:47 2019 (serrano)                */
;*    Copyright   :  1995-2019 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `local' -> `global' transformation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_local->global
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_args
	    backend_backend
	    engine_param
	    module_module
	    object_method
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_local
	    ast_glo-def
	    ast_sexp
	    ast_env
	    globalize_ginfo
	    globalize_node
	    globalize_free)
   (export  (local->global::global ::local)
	    (the-global::global ::local)))

;*---------------------------------------------------------------------*/
;*    default-type ...                                                 */
;*---------------------------------------------------------------------*/
(define (default-type)
   (if (and *optim-cfa-unbox-closure-args* (>=fx *optim* 2)) *_* *obj*))

;*---------------------------------------------------------------------*/
;*    local->global ...                                                */
;*---------------------------------------------------------------------*/
(define (local->global local)
   (trace (globalize 2) "local->global: " (shape local) #\Newline)
   (trace (globalize 3) "old body=" (shape (sfun-body (local-value local)))
	  #\Newline)
   (let* ((global    (the-global local))
	  (args      (sfun-args (local-value local)))
	  (info      (local-value local))
	  (new-body  (sfun/Ginfo-new-body info))
	  (kaptured  (sfun/Ginfo-kaptured info)))
      (trace (globalize 2) "local->global " (shape local) " escaping: "
	 (local/Ginfo-escape? local) #\Newline)
      (if (local/Ginfo-escape? local)
	  (fix-escaping-definition global local args kaptured new-body)
	  (fix-non-escaping-definition global local args kaptured new-body))
      global))

;*---------------------------------------------------------------------*/
;*    globalized-type ...                                              */
;*    -------------------------------------------------------------    */
;*    See (@ gobalize-type-id globalize_global-closure) for a          */
;*    similar function.                                                */
;*---------------------------------------------------------------------*/
(define (globalized-type type)
   (cond
      ((not *optim-cfa-unbox-closure-args*)
       ;; we set function types of the escaping function.
       ;; unless funcall-tracking optimization is enabled, these
       ;; function are _always_ of type procedure x obj x .. x obj -> obj
       ;; because type check cannot be performed on the call site.
       *obj*)
      (else
       ;; if funcall-tracking is enabled, the type of the global function
       ;; is the type of the local function
       type)))

;*---------------------------------------------------------------------*/
;*    fix-escaping-definition ...                                      */
;*---------------------------------------------------------------------*/
(define (fix-escaping-definition global local args kaptured body)
   (let* ((env (make-local-svar 'env *procedure*))
	  (new-free (map (lambda (old)
			    (let ((new (make-local-svar
					  (local-id old)
					  (default-type))))
			       (local-user?-set! new (local-user? old))
			       (widen!::local/Ginfo new)
			       (widen!::svar/Ginfo (local-value new)
				  (kaptured? #t))
			       (local-access-set! new (local-access old))
			       new))
			 kaptured))
	  (new-args (map (lambda (old)
			    (let ((new (make-local-svar
					  (local-id old)
					  (default-type))))
			       (local-user?-set! new (local-user? old))
			       (widen!::local/Ginfo new)
			       (widen!::svar/Ginfo (local-value new)
				  (kaptured? (svar/Ginfo-kaptured?
					      (local-value old))))
			       (local-access-set! new (local-access old))
			       new))
			 args))
	  (old-fun  (local-value local))
	  (new-fun  (duplicate::sfun old-fun
		       (arity (+-arity (sfun-arity old-fun) 1))
		       (args (cons env new-args)))))
      ;; we widen the new-fun
      (global/Ginfo-escape?-set! global #t)
      (widen!::sfun/Ginfo new-fun)
      ;; we must set now the info slot of env
      (widen!::svar/Ginfo (local-value env) (kaptured? #f))
      (global-type-set! global (globalized-type (local-type local)))
      ;; associate the closure entry point and the function
      (sfun-the-closure-global-set! new-fun local)
      (let ((nargs (cdr (sfun-args new-fun))))
	 (if (and (local-is-method? local)
		  (backend-typed-funcall (the-backend)))
	     (begin
		;; since the first argument (the procedure itself) has a
		;; correct (the type is the type of the `env' variable)
		;; type, we just skip it.
		(local-type-set! (car nargs) (local-type (car args)))
		(for-each (lambda (n o)
			     (if (and (bigloo-type? (local-type o))
				      (not (eq? (local-type o) *_*)))
				 (local-type-set! n (local-type o))
				 (local-type-set! n *obj*)))
		   (cdr nargs) (cdr args)))
	     (unless *optim-cfa-unbox-closure-args*
		(for-each (lambda (l)
			     (local-type-set! l *obj*))
		   nargs))))
      (global-value-set! global new-fun)
      (sfun-body-set!
       new-fun
       (make-escaping-body local global new-args new-free env
	  (node-globalize! body
	     local
	     (cons (cons (the-closure local #f) env)
		(map cons kaptured new-free)))))
      global))

;*---------------------------------------------------------------------*/
;*    make-escaping-body ...                                           */
;*---------------------------------------------------------------------*/
(define (make-escaping-body::node local global args kaptured env body)
   (let ((stack (list env))
	 (loc   (node-loc body)))
      (sfun-body-set! (local-value local) body)
      (instantiate::let-var
	 (loc loc)
	 (type (strict-node-type (variable-type local) *_*))
	 (bindings (let loop ((kaptured kaptured)
			      (num  0)
			      (res  '()))
		      (if (null? kaptured)
			  (reverse! res)
			  (loop (cdr kaptured)
				(+fx num 1)
				(cons (cons (car kaptured)
					    (sexp->node
					     `(procedure-ref env ,num)
					     stack
					     loc
					     'value))
				      res)))))
	 (body (instantiate::let-fun
		  (loc loc)
		  (type (strict-node-type (variable-type local) *_*))
		  (locals (list local))
		  (body (instantiate::app
			   (loc loc)
			   (type (strict-node-type (variable-type local) *_*))
			   (fun (instantiate::var
				   (loc loc)
				   (type (strict-node-type
					  (variable-type local) *_*))
				   (variable local)))
			   (args (map (lambda (v)
					 (instantiate::var
					    (loc loc)
					    (type (strict-node-type
						   (variable-type v) *_*))
					    (variable v)))
				      args)))))))))
					    
;*---------------------------------------------------------------------*/
;*    fix-non-escaping-definition ...                                  */
;*---------------------------------------------------------------------*/
(define (fix-non-escaping-definition global local args kaptured body)
   (let* ((add-args (map (lambda (old)
			    (trace (globalize 2) "arg=" (shape old)
			       " access=" (local-access old) #\Newline)
			    (let ((new (make-local-svar
					  (local-id old)
					  (local-type old))))
			       (local-user?-set! new (local-user? old))
			       (widen!::local/Ginfo new)
			       (widen!::svar/Ginfo (local-value new)
				  (kaptured? #t))
			       ;; MS: 24oct2016
			       (case (local-access old)
				  ((write)
				   ;; MS: 19Decl2017
				   (local-type-set! new *cell*)
				   (local-access-set! new 'cell-globalize))
				  ((cell-globalize)
				   ;; MS: 12Jun2019
				   (local-type-set! new *cell*)
				   (local-access-set! new 'cell-globalize)))
			       ;;(local-access-set! new (local-access old))
			       new))
			 kaptured))
	  (old-fun  (local-value local))
	  (new-fun  (duplicate::sfun old-fun
		       (arity (+-arity (sfun-arity old-fun) (length kaptured)))
		       (args (append (reverse add-args) args)))))
      (local/Ginfo-escape?-set! local #f)
      (global/Ginfo-escape?-set! global #t)
      (global-type-set! global (local-type local))
      (sfun-body-set! new-fun
		      (node-globalize! (sfun/Ginfo-new-body
					(local-value local))
				       local
				       (map cons kaptured add-args)))
      (global-value-set! global new-fun)
      global))

;*---------------------------------------------------------------------*/
;*    the-global ...                                                   */
;*    -------------------------------------------------------------    */
;*    The 'sfun' object allocated in this function is duplicated       */
;*    in the functions defined above.                                  */
;*---------------------------------------------------------------------*/
(define (the-global::global local::local)
   (let ((value (local-value local)))
      (if (global? (sfun/Ginfo-the-global value))
	  (sfun/Ginfo-the-global value)
	  (let* ((lid (symbol-append '& (local-id local)))
		 (id (if (global? (find-global/module lid *module*))
			 (gensym lid)
			 lid))
		 (global (def-global-sfun-no-warning! id
			    ;; we set dummy empty args-id 
			    ;; and dummy empty args because a new-fun
			    ;; will be allocated.
			    '()
			    '()
			    *module*
			    'static
			    'sfun
			    'now
			    #unspecified)))
	     ;; we have to propagate the location definition
	     ;; of the local variable
	     (sfun-loc-set! (global-value global) (sfun-loc value))
	     ;; we have to set the same type for the new global
	     (global-type-set! global (local-type local))
	     ;; we check if the function is a user one
	     (unless (local-user? local)
		(global-user?-set! global #f))
	     (widen!::global/Ginfo global)
	     (sfun-side-effect-set! (global-value global)
				    (sfun-side-effect value))
	     (sfun/Ginfo-the-global-set! value global)
	     global))))




