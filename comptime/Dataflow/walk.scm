;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Dataflow/walk.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 26 08:17:46 2010                          */
;*    Last change :  Sun Nov 28 08:13:20 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compute variable references according to dataflow tests. E.G.,   */
;*    for an expression such as (if (pair? x) then else), propagate    */
;*    x::pair in the the branch.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dataflow_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    ast_var
	    ast_node
	    effect_cgraph
	    effect_spread
	    effect_feffect
	    engine_param)
   (static  (wide-class local/value::local
	       (node::node read-only)))
   (export  (dataflow-walk! globals)))

;*---------------------------------------------------------------------*/
;*    dataflow-walk! ...                                               */
;*---------------------------------------------------------------------*/
(define (dataflow-walk! globals)
   (pass-prelude "Dataflow")
   (for-each dataflow-global! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    dataflow-global! ...                                             */
;*---------------------------------------------------------------------*/
(define (dataflow-global! global)
   (dataflow-node! (sfun-body (global-value global)) '()))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (dataflow-test-env node::node)
   '())

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::app ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::app)
   (with-access::app node (fun args)
      (if (and (fun? (variable-value (var-variable fun)))
	       (fun-predicate-of (variable-value (var-variable fun)))
	       (pair? args) (null? (cdr args))
	       (var? (car args)))
	  (let ((typ (fun-predicate-of (variable-value (var-variable fun))))
		(var (var-variable (car args))))
	     (list (cons var typ)))
	  '())))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::conditional ...                              */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::conditional)
   (with-access::conditional node (test true)
      (append (dataflow-test-env test) (dataflow-test-env true))))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::var)
   (with-access::var node (variable)
      (if (local/value? variable)
	  (dataflow-test-env (local/value-node variable))
	  '())))
	        
;*---------------------------------------------------------------------*/
;*    dataflow-node! ::node ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (dataflow-node!::pair-nil node::node env::pair-nil)
   env)

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::var env)
   (with-access::var node (type variable)
      (let ((b (assq variable env)))
	 (if (pair? b)
	     (begin
		(when (global? variable)
		   (tprint (shape node) " -> " (shape (cdr b))))
		(set! type (cdr b)))
	     (set! type (variable-type variable)))))
   env)

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::sequence ...                                    */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::sequence env)
   (let loop ((node* (sequence-nodes node))
	      (env env))
      (if (null? node*)
	  env
	  (loop (cdr node*) (dataflow-node! (car node*) env)))))
	  
;*---------------------------------------------------------------------*/
;*    dataflow-node! ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::app env)
   (with-access::app node (fun args)
      (dataflow-node*! args env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::app-ly ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::app-ly env)
   (with-access::app-ly node (fun arg)
      (dataflow-node! fun env)
      (dataflow-node! arg env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::funcall ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::funcall env)
   (with-access::funcall node (fun args)
      (dataflow-node! fun env)
      (dataflow-node*! args env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::extern ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::extern env)
   (let ((nodes (extern-expr* node)))
      (dataflow-node*! nodes env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::setq ...                                        */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::setq env)
   (with-access::setq node (var value)
      (dataflow-node! value env)
      (with-access::var var (variable)
	 (if (global? variable)
	     env
	     (let ((typ (get-type value)))
		(if (or (eq? typ *_*) (eq? typ *obj*))
		    env
		    (cons (cons variable typ) env)))))))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::conditional ...                                 */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::conditional env)
   (with-access::conditional node (test true false)
      (let ((true-env (dataflow-test-env test)))
	 (dataflow-node! false env)
	 (dataflow-node! true (append true-env env))
	 env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::fail ...                                        */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::fail env)
   (with-access::fail node (proc msg obj)
      (dataflow-node! proc env)
      (dataflow-node! msg env)
      (dataflow-node! obj env)
      env))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::select ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::select env)
   (with-access::select node (test clauses)
      (dataflow-node! test env)
      (for-each (lambda (clause)
		   (dataflow-node! (cdr clause) env))
		clauses)
      env))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::let-fun ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::let-fun env)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (dataflow-node! (sfun-body (local-value local)) env))
		locals)
      (dataflow-node! body env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::let-var ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::let-var env)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (dataflow-node! (cdr binding) env)
		   (let ((l (car binding)))
		      (when (eq? (variable-access l) 'read)
			 (widen!::local/value l
			    (node (cdr binding))))))
		bindings)
      (dataflow-node! body env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::set-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::set-ex-it env)
   (with-access::set-ex-it node (var)
      (dataflow-node! (set-ex-it-body node) env)
      '()))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::jump-ex-it ...                                  */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::jump-ex-it env)
   (with-access::jump-ex-it node (exit value)
      (dataflow-node! exit env)
      (dataflow-node! value env)
      '()))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::make-box ...                                    */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::make-box env)
   (dataflow-node! (make-box-value node) env))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::box-ref ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::box-ref env)
   (dataflow-node! (box-ref-var node) env))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::box-set! ...                                    */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::box-set! env)
   (with-access::box-set! node (var value)
      (dataflow-node! var env)
      (dataflow-node! value env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node*! ...                                              */
;*---------------------------------------------------------------------*/
(define (dataflow-node*! node* env)
   (for-each (lambda (n) (dataflow-node! n env)) node*)
   env)
