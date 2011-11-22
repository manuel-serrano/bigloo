;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/apply.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 21 09:34:48 1996                          */
;*    Last change :  Mon Nov 14 17:32:00 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The apply compilation                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_apply
   (include "Ast/node.sch")
   (import  engine_param
	    tools_error
	    tools_location
	    tools_shape
	    type_cache
 	    ast_sexp
	    ast_local
	    ast_app
	    ast_ident)
   (export  (applycation->node::node ::obj ::obj ::obj ::symbol)
	    (known-app-ly->node::node stack loc proc::node ::node ::symbol)))

;*---------------------------------------------------------------------*/
;*    applycation->node ...                                            */
;*---------------------------------------------------------------------*/
(define (applycation->node exp stack loc site)
   (match-case exp
      ((apply ?proc ?arg)
       (let* ((loc  (find-location/loc exp loc))
	      (proc (sexp->node proc
				stack
				(find-location/loc proc loc)
				'apply))
	      (arg  (sexp->node arg
				stack
				(find-location/loc arg loc)
				'value)))
	  (if (and (var? proc)
		   (fun? (variable-value (var-variable proc)))
		   (or (not (global? (var-variable proc)))
		       (and (not (global-optional? (var-variable proc)))
			    (not (global-key? (var-variable proc))))))
	      (known-app-ly->node stack loc proc arg site)
	      (instantiate::app-ly
		 (loc loc)
		 (type *_*)
		 (fun proc)
		 (arg arg)))))
      (else
       (error-sexp->node
	"Illegal `apply' form" exp (find-location/loc exp loc)))))

;*---------------------------------------------------------------------*/
;*    make-fun-frame ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (make-fun-frame fun::fun))

;*---------------------------------------------------------------------*/
;*    make-fun-frame ::sfun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (make-fun-frame fun::sfun)
   (let ((arity (sfun-arity fun)))
      (let loop ((formals (sfun-args fun))
		 (locals  '()))
	 (cond
	    ((null? formals)
	     (reverse! locals))
	    ((and (null? (cdr formals)) (<fx arity 0))
	     (reverse! locals))
	    (else
	     (loop (cdr formals)
		   (cons (make-local-svar (mark-symbol-non-user! (gensym 'aux))
					  (if (type? (car formals))
					      (car formals)
					      (local-type (car formals))))
			 locals)))))))

;*---------------------------------------------------------------------*/
;*    make-fun-frame ::cfun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (make-fun-frame fun::cfun)
   (let ((arity (cfun-arity fun)))
      (let loop ((types  (cfun-args-type fun))
		 (locals '()))
	 (cond
	    ((null? types)
	     (reverse! locals))
	    ((and (null? (cdr types)) (<fx arity 0))
	     (reverse! locals))
	    (else
	     (loop (cdr types)
		   (cons (make-local-svar (mark-symbol-non-user! (gensym 'aux))
					  (car types))
			 locals)))))))

;*---------------------------------------------------------------------*/
;*    known-app-ly->node ...                                           */
;*    -------------------------------------------------------------    */
;*    This function assumes that proc is not an opt- or key-           */
;*    function.                                                        */
;*---------------------------------------------------------------------*/
(define (known-app-ly->node stack loc proc arg site)
   (let* ((fun   (variable-value (var-variable proc)))
	  (arity (fun-arity fun))
	  (frame (make-fun-frame fun)))
      (assert (arity fun) (or (<=fx arity 0)
			      (and (not (sfun-optional? fun))
				   (not (sfun-key? fun)))))
      (cond
	 ((>fx arity 0)
	  (fx-known-app-ly->node stack loc proc arg frame site))
	 ((=fx arity 0)
	  (0-known-app-ly->node stack loc proc arg frame site))
	 (else
	  (va-known-app-ly->node stack loc proc arg frame site)))))

;*---------------------------------------------------------------------*/
;*    0-known-app-ly->node ...                                         */
;*---------------------------------------------------------------------*/
(define (0-known-app-ly->node stack loc proc arg frame site)
   (sexp->node (list proc) stack loc site))
   
;*---------------------------------------------------------------------*/
;*    fx-known-app-ly->node ...                                        */
;*    -------------------------------------------------------------    */
;*    We perform the following transformation (f is a known to be a    */
;*    fix-arity function).                                             */
;*                                                                     */
;*    (apply f exp)                                                    */
;*       -->                                                           */
;*    (let ((runner exp))                                              */
;*       (let ((a0 (car runner)))                                      */
;*          (set! runner (cdr runner))                                 */
;*          (let ((a1 (car runner)))                                   */
;*             (set! runner (cdr runner))                              */
;*             ...                                                     */
;*             (let ((an (car runner)))                                */
;*                (if (null? (cdr runner))                             */
;*                   (f a0 ... an)))))                                 */
;*                   (error)                                           */
;*---------------------------------------------------------------------*/
(define (fx-known-app-ly->node stack loc proc arg frame site)
   (let ((runner (make-local-svar (mark-symbol-non-user! (gensym 'runner))
				  *_*))
	 (type (node-type proc)))
      (local-access-set! runner 'write)
      (instantiate::let-var
	 (loc loc)
	 (type (strict-node-type *_* type))
	 (bindings (list (cons runner arg)))
	 (body (let loop ((locals frame))
		  (if (null? locals)
		      (let ((app (application->node
				  `(,proc
				    ,@(map (lambda (local)
					      (instantiate::var
						 (loc loc)
						 (type (strict-node-type *_* (local-type local)))
						 (variable local)))
					   frame))
				  stack
				  loc
				  'value)))
			 (if *unsafe-arity*
			     app
			     (sexp->node
			      `(if (null? (cdr
					   ,(instantiate::var
					       (loc loc)
					       (type (strict-node-type *_* (local-type runner)))
					       (variable runner))))
				   ,app
				   (failure "apply"
					    "Too many arguments provided"
					    ',(shape (var-variable proc))))
			      stack
			      loc
			      site)))
		      (instantiate::let-var
			 (loc loc)
			 (type (strict-node-type *_* type))
			 (bindings (list (cons
					  (car locals)
					  (sexp->node
					   `(car
					     ,(instantiate::var
						 (loc loc)
						 (type (strict-node-type *_* (local-type runner)))
						 (variable runner)))
					   stack
					   loc
					   'value))))
			 (body (if (null? (cdr locals))
				   (loop (cdr locals))
				   (sexp->node
				    `(begin
					(set! ,(instantiate::var
						  (loc loc)
						  (type (strict-node-type *_* (local-type runner)))
						  (variable runner))
					      (cdr
					       ,(instantiate::var
						   (loc loc)
						   (type (strict-node-type *_* (local-type runner)))
						   (variable runner))))
					,(loop (cdr locals)))
				    stack
				    loc
				    'value))))))))))
				     
;*---------------------------------------------------------------------*/
;*    va-known-app-ly->node ...                                        */
;*    -------------------------------------------------------------    */
;*    We perform the following transformation (f is a known to be a    */
;*    va-arity function).                                              */
;*                                                                     */
;*    (apply f exp)                                                    */
;*       -->                                                           */
;*    (let ((runner exp))                                              */
;*       (let ((a0 (car runner)))                                      */
;*          (set! runner (cdr runner))                                 */
;*          (let ((a1 (car runner)))                                   */
;*             (set! runner (cdr runner))                              */
;*             ...                                                     */
;*             (f a0 ... runner))))                                    */
;*---------------------------------------------------------------------*/
(define (va-known-app-ly->node stack loc proc arg frame site)
   (let ((runner (make-local-svar (mark-symbol-non-user! (gensym 'runner)) *_*))
	 (type   (strict-node-type *_* (node-type proc))))
      (when (pair? frame)
	 (local-access-set! runner 'write))
      (instantiate::let-var
	 (loc loc)
	 (type (strict-node-type *_* type))
	 (bindings (list (cons runner arg)))
	 (body (let loop ((locals frame)
			  (old    '()))
		  (if (null? locals)
		      (begin
			 (if (null? old)
			     (set! frame (list runner))
			     (set-cdr! old (cons runner '())))
			 (instantiate::app
			    (loc loc)
			    (type (strict-node-type *_* (variable-type (var-variable proc))))
			    (fun (duplicate::var proc))
			    (args (map (lambda (local)
					  (instantiate::var
					     (loc loc)
					     (type (strict-node-type *_* (local-type local)))
					     (variable local)))
				       frame))))
		      (instantiate::let-var
			 (loc loc)
			 (type (strict-node-type *_* type))
			 (bindings (list (cons
					  (car locals)
					  (sexp->node
					   `(car
					     ,(instantiate::var
						 (loc loc)
						 (type (strict-node-type *_* (local-type runner)))
						 (variable runner)))
					   stack
					   loc
					   'value))))
			 (body (sexp->node
				`(begin
				    (set! ,(instantiate::var
					      (loc loc)
					      (type (strict-node-type *_* (local-type runner)))
					      (variable runner))
					  (cdr ,(instantiate::var
						   (loc loc)
						   (type (strict-node-type *_* (local-type runner)))
						   (variable runner))))
				    ,(loop (cdr locals) locals))
				stack
				loc
				'value)))))))))
					 



