;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Dataflow/walk.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 26 08:17:46 2010                          */
;*    Last change :  Fri Apr 21 18:43:50 2017 (serrano)                */
;*    Copyright   :  2010-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute type variable references according to dataflow tests.    */
;*    For instance, for an expression such as (if (pair? x) then else),*/
;*    propagate x::pair in the the branch.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module dataflow_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch"
	    "Dataflow/walk.sch")
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
	    effect_cgraph
	    effect_spread
	    effect_feffect
	    engine_param)
   (static  (wide-class local/value::local
	       (stamp::int read-only)
	       (node::node read-only)))
   (export  (dataflow-walk! globals ::bstring)))

;*---------------------------------------------------------------------*/
;*    dataflow-walk! ...                                               */
;*---------------------------------------------------------------------*/
(define (dataflow-walk! globals name)
   (pass-prelude name)
   (set! *$null?* (find-global/module '$null? 'foreign))
   (set! *$pair?* (find-global/module '$pair? 'foreign))
   (set! *$epair?* (find-global/module '$epair? 'foreign))
   (for-each dataflow-global! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    type predicates                                                  */
;*---------------------------------------------------------------------*/
(define *$null?* #f)
(define *$pair?* #f)
(define *$epair?* #f)

;*---------------------------------------------------------------------*/
;*    dataflow-global! ...                                             */
;*---------------------------------------------------------------------*/
(define (dataflow-global! global)
   (dataflow-node! (sfun-body (global-value global)) '()))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::node ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (dataflow-node!::pair-nil node::node env::pair-nil)
   env)

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::var ...                                         */
;*    -------------------------------------------------------------    */
;*    This function sets the most specific type for the variable       */
;*    reference (computed according to the control flow). The          */
;*    stage globalize and integrate that introduce cells change        */
;*    the types of the boxed variable references (see globalize_node   */
;*    and integrate_node).                                             */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::var env)
   (with-access::var node (type variable)
      (with-access::variable variable ((vtype type))
	 (when (and (bigloo-type? vtype)
		    (or (not (tclass? vtype)) (type-subclass? vtype type)))
	    (let ((b (assq variable env))) 
	       (if (pair? b)
		   (set! type (cdr b))
		   (set! type (variable-type variable)))))))
   env)

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::closure ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::closure env)
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
;*    dataflow-node! ::sync ...                                        */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::sync env)
   (dataflow-node! (sync-mutex node) env)
   (dataflow-node! (sync-prelock node) env)
   (dataflow-node! (sync-body node) env)
   env)

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::app env)
   (with-access::app node (fun args)
      (let ((aenv (dataflow-node*! args env)))
	 (if (and (var? fun) (local? (var-variable fun)))
	     ;; when a local variable, all the locals that are not
	     ;; read-only have to be removed from the environment
	     ;; because they might be affected by the called function
	     (filter (lambda (b)
			(let ((v (car b)))
			   (or (global? v) (eq? (variable-access v) 'read))))
		     aenv)
	     aenv))))

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
   
   (define (remove-variable-from-env variable env)
      (filter (lambda (b)
		 (not (eq? (car b) variable)))
	 env))
   
   (with-access::setq node (var value)
      (let ((nenv (dataflow-node! value env)))
	 (with-access::var var (variable)
	    (if (global? variable)
		(remove-variable-from-env variable nenv)
		(let ((typ (get-type value #t)))
		   (if (or (eq? typ *_*) (eq? typ *obj*))
		       (remove-variable-from-env variable nenv)
		       (cons (cons variable typ) nenv))))))))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::conditional ...                                 */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::conditional env)
   
   (define (lub env1 env2)
      (filter (lambda (c)
		 (let* ((v (car c))
			(t (cdr c))
			(c2 (assq v env2)))
		    (and (pair? c2) (eq? (cdr c2) t))))
	 env1))

   (with-access::conditional node (test true false)
      (let* ((tenv (dataflow-node! test env))
	     (true-env (append (dataflow-test-env test) tenv))
	     (false-env (append (dataflow-test-false-env test) tenv)))
	 (let ((tenv (dataflow-node! false false-env))
	       (fenv (dataflow-node! true true-env)))
	    (if (abort? false)
		tenv
		(lub tenv fenv))))))

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
;*    dataflow-node! ::switch ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::switch env)
   (with-access::switch node (test clauses)
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
		   (dataflow-node! (sfun-body (local-value local)) '()))
		locals)
      (dataflow-node! body env)))

;*---------------------------------------------------------------------*/
;*    let-var-stamp ...                                                */
;*---------------------------------------------------------------------*/
(define let-var-stamp 0)

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::let-var ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::let-var env)
   (set! let-var-stamp (+fx 1 let-var-stamp))
   (with-access::let-var node (body bindings)
      (let* ((stamp let-var-stamp)
	     (env (let loop ((bindings bindings)
			     (env env))
		     (if (null? bindings)
			 env
			 (let* ((node (cdar bindings))
				(env (dataflow-node! node env)))
			    (let ((l (caar bindings)))
			       (when (eq? (variable-access l) 'read)
				  (widen!::local/value l
				     (stamp stamp)
				     (node node))))
			    (loop (cdr bindings) env))))))
	 (filter-map (lambda (b)
			(cond
			   ((or (not (local/value? (car b)))
				(<fx (local/value-stamp (car b)) stamp))
			    b)
			   ((var? (local/value-node (car b)))
			    (let ((v (var-variable (local/value-node (car b)))))
			       (when (eq? (variable-access v) 'read)
				  (cons v (cdr b)))))
			   (else
			    #f)))
		     (dataflow-node! body env)))))

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
   (with-access::box-ref node (var)
      env))

;*---------------------------------------------------------------------*/
;*    dataflow-node! ::box-set! ...                                    */
;*---------------------------------------------------------------------*/
(define-method (dataflow-node! node::box-set! env)
   (with-access::box-set! node (var value)
      (dataflow-node! value env)))

;*---------------------------------------------------------------------*/
;*    dataflow-node*! ...                                              */
;*---------------------------------------------------------------------*/
(define (dataflow-node*! node* env)
   (for-each (lambda (n) (dataflow-node! n env)) node*)
   env)

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (dataflow-test-env::pair-nil node::node)
   '())

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::app ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::app)
   (with-access::app node (fun args)
      (let* ((f (var-variable fun))
	     (funv (variable-value f)))
	 (cond
	    ((isa-of node)
	     =>
	     (lambda (ty)
		(if (var? (car args))
		    (list (cons (var-variable (car args)) ty))
		    '())))
	    ((and (fun? funv)
		  (fun-predicate-of funv)
		  (pair? args) (null? (cdr args))
		  (var? (car args))
		  (bigloo-type? (variable-type (var-variable (car args)))))
	     (list (cons (var-variable (car args)) (fun-predicate-of funv))))
	    (else
	     '())))))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::instanceof ...                               */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::instanceof)
   (with-access::instanceof node (expr* class)
      (if (var? (car expr*))
	  (list (cons (var-variable (car expr*)) class))
	  '())))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::conditional ...                              */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::conditional)
   (with-access::conditional node (test true false)
      (if (and (atom? false) (eq? (atom-value false) #f))
	  (append (dataflow-test-env test) (dataflow-test-env true))
	  '())))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::var)
   (with-access::var node (variable)
      (if (local/value? variable)
	  (dataflow-test-env (local/value-node variable))
	  '())))

;*---------------------------------------------------------------------*/
;*    dataflow-test-env ::let-var ...                                  */
;*    -------------------------------------------------------------    */
;*    We detect the two patterns:                                      */
;*      pattern 1:                                                     */
;*        (let ((tmp exp))                                             */
;*           (<predicate> tmp))                                        */
;*      pattern 2:                                                     */
;*        (let ((res::bool pattern1))                                  */
;*           res)                                                      */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-env node::let-var)
   (with-access::let-var node (bindings body)
      (if (and (pair? bindings) (null? (cdr bindings)))
	  (cond
	     ((var? (cdar bindings))
	      (let ((env (dataflow-test-env body)))
		 (if (and (pair? env)
			  (null? (cdr env))
			  (eq? (caar env) (caar bindings)))
		     (list (cons (var-variable (cdar bindings)) (cdar env)))
		     '())))
	     ((and (var? body)
		   (eq? (var-variable body) (caar bindings))
		   (eq? (variable-type (caar bindings)) *bool*))
	      (dataflow-test-env (cdar bindings)))
	     (else
	      '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    dataflow-test-false-env ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (dataflow-test-false-env::pair-nil node::node)
   '())

;*---------------------------------------------------------------------*/
;*    dataflow-test-false-env ::app ...                                */
;*---------------------------------------------------------------------*/
(define-method (dataflow-test-false-env node::app)
   (with-access::app node (fun args)
      (let* ((f (var-variable fun))
	     (funv (variable-value f)))
	 (if (and (pair? args) (null? (cdr args))
		  (var? (car args))
		  (eq? (variable-type (var-variable (car args))) *pair-nil*))
	     (cond
		((eq? f *$null?*)
		 (list (cons (var-variable (car args)) *pair*)))
		((or (eq? f *$pair?*) (eq? f *$epair?*))
		 (list (cons (var-variable (car args)) *bnil*)))
		(else
		 '()))
	     '()))))

;*---------------------------------------------------------------------*/
;*    abort? ::node ...                                                */
;*    -------------------------------------------------------------    */
;*    The predicate abort? returns #t for an expression iff the        */
;*    evaluation of that expression yields to evaluation a FAIL.       */
;*---------------------------------------------------------------------*/
(define-generic (abort? node::node)
   #f)

;*---------------------------------------------------------------------*/
;*    abort? ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (abort? node::fail)
   #t)

;*---------------------------------------------------------------------*/
;*    abort? ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (abort? node::sequence)
   (any abort? (sequence-nodes node)))

;*---------------------------------------------------------------------*/
;*    abort? ::sync ...                                                */
;*---------------------------------------------------------------------*/
(define-method (abort? node::sync)
   (or (abort? (sync-mutex node))
       (abort? (sync-prelock node))
       (abort? (sync-body node))))

;*---------------------------------------------------------------------*/
;*    abort? ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (abort? node::let-var)
   (with-access::let-var node (bindings body)
      (or (any (lambda (b) (abort? (cdr b))) bindings) (abort? body))))

;*---------------------------------------------------------------------*/
;*    abort? ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (abort? node::let-fun)
   (with-access::let-fun node (body)
      (abort? body)))

;*---------------------------------------------------------------------*/
;*    abort? ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abort? node::conditional)
   (with-access::conditional node (test true false)
      (or (abort? test) (and (abort? true) (abort? false)))))

;*---------------------------------------------------------------------*/
;*    abort? ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (abort? node::app)
   (with-access::app node (fun args)
      (or (abort? fun) (any abort? args))))

;*---------------------------------------------------------------------*/
;*    abort? ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (abort? node::funcall)
   (with-access::funcall node (fun args)
      (or (abort? fun) (any abort? args))))

;*---------------------------------------------------------------------*/
;*    abort? ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (abort? node::app-ly)
   (with-access::app-ly node (fun arg)
      (or (abort? fun) (abort? arg))))

