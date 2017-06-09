;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/sbeta.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  9 15:29:23 2000                          */
;*    Last change :  Fri Jun  9 10:19:08 2017 (serrano)                */
;*    Copyright   :  2000-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This stage implement a very straightforward beta-reduction. It   */
;*    is simpler than the 1occ stage. It apply the following           */
;*    transformation:                                                  */
;*       (let ((a1 exp1)...) (<fun> ... a1 ...))                       */
;*                         ==>                                         */
;*       (let (...) (<fun> ... <exp1> ...))                            */
;*                                                                     */
;*    In other word, we are not producing better code, we are simply   */
;*    helping the C compiler by doing some pre-register allocations.   */
;*    This optimization is important for the Jvm back-end. It helps    */
;*    producing better code.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_beta
   (include "Tools/trace.sch")
   (import   tools_shape
	     tools_speek
	     tools_error
	     backend_backend
	     type_type
	     type_cache
	     type_typeof
	     coerce_coerce
	     effect_effect
	     engine_param
	     ast_var
	     ast_env
	     ast_node
	     ast_lvtype
	     ast_occur)
   (export  (reduce-beta! globals)))

;*---------------------------------------------------------------------*/
;*    reduce-beta! ...                                                 */
;*---------------------------------------------------------------------*/
(define (reduce-beta! globals)
   (verbose 2 #"      simple beta reduction  ")
   ;; we have to recompute the occurrences because both `copy-propagation
   ;; and `cse' have changed the number of occurrence (in two directions).
   (occur-var globals)
   ;; then we start the 1-occurrence reduction. 
   (set! *removed* 0)
   ;; prepare the predicate beta reduction
   (set! *c-fixnum?* (find-global 'c-fixnum? 'foreign))
   (set! *c-flonum?* (find-global 'c-flonum? 'foreign))
   (set! *c-string-length* (find-global '$string-length 'foreign))
   (set! *predicates* (list *c-fixnum?* *c-flonum?*))
   ;; start reducing
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun))) 
		   (sfun-body-set! fun (node-beta! node))
		   #unspecified))
	     globals)
   ;; clean the local cache
   (set! *c-fixnum?* #unspecified)
   (set! *c-flonum?* #unspecified)
   (set! *c-string-length* #unspecified)
   (set! *predicates* '())
   ;; display statistics
   (verbose 2 "(removed: " *removed* #\) #\newline)
   globals)

;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *removed* 0)

;*---------------------------------------------------------------------*/
;*    Predicate optimization                                           */
;*---------------------------------------------------------------------*/
(define *c-fixnum?* #unspecified)
(define *c-flonum?* #unspecified)
(define *c-string-length* #unspecified)
(define *predicates* '())

;*---------------------------------------------------------------------*/
;*    node-beta! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node-beta!::node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    find-actual-expression ...                                       */
;*---------------------------------------------------------------------*/
(define (find-actual-expression::node body::node)
   (if (sequence? body)
       (with-access::sequence body (nodes meta)
	  (if (and (pair? nodes) (null? (cdr nodes)) (null? meta))
	      (find-actual-expression (car nodes))
	      body))
       body))

;*---------------------------------------------------------------------*/
;*    is-argument? ...                                                 */
;*---------------------------------------------------------------------*/
(define (is-argument? var::variable body)
   (cond
      ((same-variable? var body)
       #t)
      ((cast? body)
       (with-access::cast body (arg)
	  (same-variable? var arg)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    argument? ...                                                    */
;*---------------------------------------------------------------------*/
(define (argument? var::variable args::pair-nil)
   (let loop ((args args))
      (cond
	 ((null? args)
	  #f)
	 ((is-argument? var (car args))
	  #t)
	 (else
	  (loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    make-args-list ...                                               */
;*---------------------------------------------------------------------*/
(define (make-args-list bindings args)
   (map (lambda (a)
	   (let loop ((bindings bindings))
	      (cond
		 ((null? bindings)
		  a)
		 ((is-argument? (caar bindings) a)
		  (cdar bindings))
		 (else
		  (loop (cdr bindings))))))
	args))
   
;*---------------------------------------------------------------------*/
;*    dangerous? ...                                                   */
;*---------------------------------------------------------------------*/
(define (dangerous?::bool expr)
   (let ((expr (find-actual-expression expr)))
      (cond
	 ((or (var? expr) (atom? expr) (kwote? expr))
	  #f)
	 ((vref? expr)
	  (any dangerous? (vref-expr* expr)))
	 ((getfield? expr)
	  (any dangerous? (getfield-expr* expr)))
	 ((not (app? expr))
	  #t)
	 (else
	  (with-access::app expr (fun args)
	     (let* ((var (var-variable fun))
		    (val (variable-value var)))
		(if (and (global? var)
			 (cfun? val)
			 (cfun-macro? val)
			 (not (memq 'nesting (global-pragma var))))
		    #t
		    (any dangerous? args))))))))

;*---------------------------------------------------------------------*/
;*    side-effect-safe? ...                                            */
;*    -------------------------------------------------------------    */
;*    This returns #t for forms that may be declared as doing a        */
;*    side-effect (because for instance, it is an application          */
;*    for which one arguments is a variable that is set somewhere),    */
;*    that are known to be safe to reduce if they are an argument      */
;*    to a predicate.                                                  */
;*---------------------------------------------------------------------*/
(define (side-effect-safe?::bool expr)
   (let ((expr (find-actual-expression expr)))
      (cond
	 ((or (var? expr) (atom? expr) (kwote? expr))
	  #t)
	 ((vref? expr)
	  (every side-effect-safe? (vref-expr* expr)))
	 ((getfield? expr)
	  (every side-effect-safe? (getfield-expr* expr)))
	 ((not (app? expr))
	  #f)
	 (else
	  (with-access::app expr (fun args)
	     (if (not (fun-side-effect (variable-value (var-variable fun))))
		 (every side-effect-safe? args)
		 #f))))))

;*---------------------------------------------------------------------*/
;*    dangerous-binding? ...                                           */
;*---------------------------------------------------------------------*/
(define (dangerous-binding? binding)
   (dangerous? (cdr binding)))

;*---------------------------------------------------------------------*/
;*    same-variable? ...                                               */
;*---------------------------------------------------------------------*/
(define (same-variable?::bool var::variable node::node)
   (and (var? node) (eq? (var-variable node) var)))

;*---------------------------------------------------------------------*/
;*    node-beta! ::let-var ...                                         */
;*    -------------------------------------------------------------    */
;*    This is the only important node. The other node processing       */
;*    is pure graph traversal.                                         */
;*    -------------------------------------------------------------    */
;*    We have to be smart enough to correctly handle LET constructions */
;*    such as:                                                         */
;*       (let ((a1 exp1) (a2 exp2)...) (<fun1> a1 (<fun2> a2 ..) ...)) */
;*                 ==>                                                 */
;*       (<fun1> exp1 (<fun2> exp2 ...) ...)                           */
;*    We want to correctly handle these kind of transformation because */
;*    this situation arose because of the insertion of the type        */
;*    conversions.                                                     */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::let-var)
   (define (simple? e)
      (or (atom? e)
	  (var? e)
	  (and (vref? e) (every simple? (vref-expr* e)))
	  (and (app? e) (every simple? (app-args e)) (not (dangerous? e)))
	  (and (getfield? e) (every simple? (getfield-expr* e)))))
   (with-access::let-var node (body bindings removable?)
      (let ((abody (find-actual-expression body)))
	 ;; in any case, walk thru the body of the let construction
	 (set! body (node-beta! abody))
	 ;; and thru the bound variables
	 (for-each (lambda (binding)
		      (set-cdr! binding (node-beta! (cdr binding))))
		   bindings)
	 ;; then try some reductions
	 (cond
	    ((not removable?)
	     node)
	    ((null? bindings)
	     abody)
	    ((or (any (lambda (b)
			 ;; we don't optimize if a variable has several
			 ;; occurrences or if it is a user variable and
			 ;; we are compiling for debugging.
			 (or (>fx (local-occurrence (car b)) 1)
			     (and (local-user? (car b))
				  (memq 'bdb
				     (backend-debug-support (the-backend)))
				  (>fx *bdb-debug* 0))))
		       bindings)
		 (any dangerous-binding? bindings))
	     node)
	    ((and (pair? bindings)
		  (null? (cdr bindings))
		  (same-variable? (car (car bindings)) abody))
	     ;; (let ((var <expr>)) var) ==> <expr>
	     (set! *removed* (+fx *removed* 1))
	     (node-beta! (cdar bindings)))
	    ((and (conditional? abody)
		  (null? (cdr bindings))
		  (same-variable? (car (car bindings))
				  (conditional-test abody)))
	     ;; (let ((var <expr>)) (if var ... ...)) ==> (if <expr> ... ...)
	     (set! *removed* (+fx *removed* 1))
	     (let ((val (cdr (car bindings))))
		(conditional-test-set! abody val)
		(if (not (conditional-side-effect abody))
		    (conditional-side-effect-set! abody (side-effect? val))))
	     abody)
	    ((and (app? abody)
		  (every simple? (app-args abody))
		  (not (any side-effect? (app-args abody)))
		  (every (lambda (b)
			    (and (argument? (car b) (app-args abody))
				 ;; kwote has to be checked because sbeta
				 ;; could be first applied before constant
				 ;; compilation
				 (not (kwote? (cdr b)))))
			  bindings))
	     ;; (let ((v1 <expr1>) ... (vn <exprn>)) (f var1 ... varn))
	     ;;   ==> (let (...) (f <expr1> ... <exprn))
	     (set! *removed* (+fx *removed* (length bindings)))
	     (with-access::app abody (args)
		(let ((nargs (make-args-list bindings (app-args abody))))
		   (set! args nargs)
		   (if (not (app-side-effect abody))
		       (app-side-effect-set! abody (any side-effect? nargs)))))
	     abody)
	    ((and (extern? abody)
		  (every simple? (extern-expr* abody))
		  (every (lambda (b)
			    (and (argument? (car b) (extern-expr* abody))
				 ;; kwote has to be checked because sbeta
				 ;; could be first applied before constant
				 ;; compilation
				 (not (kwote? (cdr b)))))
			  bindings))
	     ;; (let ((v1 <expr1>) ... (vn <exprn>)) (extern var1 ... varn))
	     ;;   ==> (let (...) (extern <expr1> ... <exprn))
	     (set! *removed* (+fx *removed* (length bindings)))
	     (with-access::extern abody (expr*)
		(let ((nexpr* (make-args-list bindings (extern-expr* body))))
		   (extern-expr*-set! abody nexpr*)
		   (if (not (extern-side-effect abody))
		       (extern-side-effect-set! abody (any side-effect? nexpr*)))))
	     abody)
	    (else
	     node)))))

;*---------------------------------------------------------------------*/
;*    node-beta! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::sequence)
   (with-access::sequence node (nodes)
      (node-beta*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::sync)
   (with-access::sync node (body mutex prelock)
      (set! mutex (node-beta! mutex))
      (set! prelock (node-beta! prelock))
      (set! body (node-beta! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (node-beta! fun))
      (set! arg (node-beta! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::funcall)
   (with-access::funcall node (args)
      (node-beta*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::cast)
   (with-access::cast node (arg)
      (set! arg (node-beta! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::setq)
   (with-access::setq node (var value)
      (set! value (node-beta! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::conditional)
   (with-access::conditional node (test true false)
      (set! test (node-beta! test))
      (set! true (node-beta! true))
      (set! false (node-beta! false))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::switch)
   (with-access::switch node (clauses test)
      (set! test (node-beta! test))
      (let loop ((clauses clauses))
	 (if (null? clauses)
	     node
	     (let ((clause (car clauses)))
		(set-cdr! clause (node-beta! (cdr clause)))
		(loop (cdr clauses)))))))

;*---------------------------------------------------------------------*/
;*    node-beta! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::let-fun)
   (with-access::let-fun node (body locals)
      (set! body (node-beta! body))
      (let loop ((locals locals))
	 (if (null? locals)
	     node
	     (let* ((local (car locals))
		    (sfun  (local-value local)))
		(sfun-body-set! sfun (node-beta! (sfun-body sfun)))
		(loop (cdr locals)))))))

;*---------------------------------------------------------------------*/
;*    node-beta! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (node-beta! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (node-beta! exit))
      (set! value (node-beta! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::make-box)
   (with-access::make-box node (value)
      (set! value (node-beta! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::box-set!)
   (with-access::box-set! node (value)
      (set! value (node-beta! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::app)
   (with-access::app node (args)
      (node-beta*! args)
      (if (predicate? node)
	  (node-beta-predicate! node)
	  (node-beta-early-app! node))))

;*---------------------------------------------------------------------*/
;*    predicate? ...                                                   */
;*---------------------------------------------------------------------*/
(define (predicate? node::app)
   (with-access::app node (fun args)
      (and (pair? args)
	   (null? (cdr args))
	   (or (not (side-effect? (car args)))
	       (side-effect-safe? (car args)))
	   (memq (var-variable fun) *predicates*))))

;*---------------------------------------------------------------------*/
;*    node-beta-predicate! ...                                         */
;*---------------------------------------------------------------------*/
(define (node-beta-predicate! node)
   (with-access::app node (fun args loc type)
      (let ((vfun (var-variable fun))
	    (atype (get-type (car args) #f)))
	 (cond
	    ((eq? atype *obj*)
	     node)
	    ((eq? vfun *c-fixnum?*)
	     (set! *removed* (+fx *removed* 1))
	     (instantiate::literal
		(type type)
		(loc loc)
		(value (or (eq? atype *bint*)
			   (eq? atype *int*)
			   (eq? atype *long*)))))
	    ((eq? vfun *c-flonum?*)
	     (set! *removed* (+fx *removed* 1))
	     (instantiate::literal
		(type type)
		(loc loc)
		(value (or (eq? atype *real*)
			   (eq? atype *breal*)))))
	    (else
	     (internal-error "node-beta-predicate!"
			     "Illegal predicate"
			     (shape node)))))))
   
;*---------------------------------------------------------------------*/
;*    node-beta*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-beta*! nodes)
   (let loop ((nodes nodes))
      (if (null? nodes)
	  #unspecified
	  (begin
	     (set-car! nodes (node-beta! (car nodes)))
	     (loop (cdr nodes))))))

;*---------------------------------------------------------------------*/
;*    node-beta-early-app! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function implements straightfoward early reductions         */
;*    such as (string-length "foo") -> 3.                              */
;*---------------------------------------------------------------------*/
(define (node-beta-early-app!::node node::app)
   (with-access::app node (args fun)
      (if (every atom? args)
	  (if (and (=fx (length args) 1) (string? (atom-value (car args))))
	      (if (eq? (var-variable fun) *c-string-length*)
		  (instantiate::literal
		     (loc (node-loc node))
		     (type (node-type node))
		     (value (string-length (atom-value (car args)))))
		  node)
	      node)
	  node)))

   
