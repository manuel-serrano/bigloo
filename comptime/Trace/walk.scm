;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Trace/walk.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 13 13:53:58 1995                          */
;*    Last change :  Wed Jan  8 13:04:14 2020 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The introduction of trace in debugging mode.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module trace_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  ast_let
	    tools_shape
	    tools_error
	    tools_misc
	    tools_location
	    type_env
	    backend_backend
	    ast_sexp
	    ast_ident
	    module_module
	    engine_param
	    (mark-symbol-non-user! ast_ident)
	    trace_isloop
	    (find-global ast_env)
	    (find-location tools_location))
   (export  (trace-walk! tree)))

;*---------------------------------------------------------------------*/
;*    trace-walk! ...                                                  */
;*---------------------------------------------------------------------*/
(define (trace-walk! globals)
   (pass-prelude "Trace")
   ;; We make some extra tracing for top level set! expression. Top level
   ;; set! expressions that change global variables are traced. This enables
   ;; tracing of toplevel defined closures. This transformation is applied
   ;; before regular tracing because it only scans [begin] top level forms.
   ;; It stops before any nested expression.
   (let* ((id 'toplevel-init)
	  (glo (find-global id *module*)))
      (if (global? glo)
	  (with-access::sfun (global-value glo) (body)
	     (set! body (toplevel-trace-node body)))))
   ;; then, trace all functions (including the toplevel one)
   (for-each (lambda (v) (trace-fun! v '() *compiler-debug-trace*)) globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    trace-id ...                                                     */
;*    -------------------------------------------------------------    */
;*    To get the trace identifier we take the ident of the global      */
;*    function holding the function. There is one exception for        */
;*    the module [toplevel-init] function. Instead of using the        */
;*    rather anonymous [toplevel-init] ident, we use the name of       */
;*    the module.                                                      */
;*---------------------------------------------------------------------*/
(define (trace-id v)
   (cond
      ((and (global? v) (eq? (global-id v) 'toplevel-init))
       (symbol-append (string->symbol "%toplevel@") (global-module v)))
      ((and (global? v) (eq? (global-id v) 'imported-modules-init))
       (symbol-append (string->symbol "%import@") (global-module v)))
      ((global? v)
       (symbol-append (global-id v) '@ (global-module v)))
      (else
       (variable-id v))))

;*---------------------------------------------------------------------*/
;*    trace-fun! ...                                                   */
;*    -------------------------------------------------------------    */
;*    We don't trace predicates. It is useless and makes the code      */
;*    much bigger in safe modes.                                       */
;*---------------------------------------------------------------------*/
(define (trace-fun! var stack level)
   (let* ((fun  (variable-value var))
	  (body (sfun-body fun))
	  (lloc (if (global? var)
		    (find-location (find-last-sexp (global-src var)))
		    (node-loc (find-last-node body)))))
      (when (and (not (fun-predicate-of fun))
		 (not (memq 'no-trace (sfun-property fun)))
		 (user-symbol? (variable-id var)))
	 (enter-function (trace-id var))
	 (let* ((bd (if (or (>fx level 1)
			    (and (global? var)
				 (or (eq? (global-id var) 'toplevel-init)
				     (eq? (global-id var) 'method-init)
				     (eq? (global-id var) 'generic-init))))
			;; we always goes trough the first level
			;; (i.e. not the nested local functions)
			;; of the toplevel-init function even
			;; if [*compiler-debug-trace* < 2]. That way
			;; we are sure that global closures will
			;; be correctly traced and not labeled
			;; [toplevel-init].
			(if (or (local? var)
				(eq? (global-id var) 'method-init)
			        (eq? (global-id var) 'generic-init))
			    (trace-node body stack (-fx level 1))
			    (trace-node body (cons var stack) (-fx level 1)))
			body))
		(t (strict-node-type (node-type body) (variable-type var)))
		(id (trace-id var))
		(nbody (make-traced-node bd t id lloc stack)))
	    (sfun-body-set! fun nbody)
	    (leave-function)))))

;*---------------------------------------------------------------------*/
;*    find-last-sexp ...                                               */
;*    -------------------------------------------------------------    */
;*    Find the last sexp embedded in SEXP. The expression we are       */
;*    seeking is a LIST, not an atom. The function we are              */
;*    implementing could also be called something like LAST-LIST.      */
;*---------------------------------------------------------------------*/
(define (find-last-sexp sexp)
   (let loop ((sexp sexp)
	      (res sexp))
      (cond
	 ((not (pair? sexp))
	  res)
	 ((not (pair? (cdr sexp)))
	  (loop (car sexp) sexp))
	 (else
	  (loop (last-pair sexp) sexp)))))

;*---------------------------------------------------------------------*/
;*    find-last-node ::node ...                                        */
;*    -------------------------------------------------------------    */
;*    This function computes the same computation as FIND-LAST-SEXP    */
;*    but on NODEs instead of LISTs.                                   */
;*---------------------------------------------------------------------*/
(define-generic (find-last-node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    find-last-node ::sequence ...                                    */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::sequence)
   (with-access::sequence node (nodes)
      (if (pair? nodes)
	  (find-last-node (car (last-pair nodes)))
	  node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::sync ...                                        */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::sync)
   (with-access::sync node (mutex prelock body)
      (find-last-node body)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::app)
   (with-access::app node (args)
      (if (pair? args)
	  (find-last-node (car (last-pair args)))
	  node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::app-ly ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::app-ly)
   (find-last-node (app-ly-arg node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::funcall ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::funcall)
   (with-access::funcall node (fun args)
      (if (pair? args)
	  (find-last-node (car (last-pair args)))
	  (find-last-node fun))))

;*---------------------------------------------------------------------*/
;*    find-last-node ::extern ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::extern)
   (with-access::extern node (expr*)
      (if (pair? expr*)
	  (find-last-node (car (last-pair expr*)))
	  node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::setq ...                                        */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::setq)
   (find-last-node (setq-value node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::conditional ...                                 */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::conditional)
   (find-last-node (conditional-false node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::fail ...                                        */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::fail)
   (find-last-node (fail-obj node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::switch ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::switch)
   (with-access::switch node (clauses test)
      (if (pair? clauses)
	  (find-last-node (cdr (last-pair clauses)))
	  (find-last-sexp test))))

;*---------------------------------------------------------------------*/
;*    find-last-node ::let-var ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::let-var)
   (find-last-node (let-var-body node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::let-fun ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::let-fun)
   (find-last-node (let-fun-body node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::set-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::set-ex-it)
   (find-last-node (set-ex-it-body node)))

;*---------------------------------------------------------------------*/
;*    find-last-node ::jump-ex-it ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-last-node node::jump-ex-it)
   (find-last-node (jump-ex-it-value node)))

;*---------------------------------------------------------------------*/
;*    make-traced-node ...                                             */
;*---------------------------------------------------------------------*/
(define (make-traced-node::let-var node::node type::type symbol lloc stack)

   (define (fqname sym stack)
      (if (null? stack)
	  sym
	  (symbol-append sym ': (fqname (trace-id (car stack)) (cdr stack)))))
   
   (let* ((loc  (node-loc node))
	  (aux  (mark-symbol-non-user! (gensym 'aux)))
	  (taux (make-typed-ident aux (type-id type)))
	  (tmp1 (mark-symbol-non-user! (gensym 'name)))
	  (tmp2 (mark-symbol-non-user! (gensym 'loc)))
	  (tmp3 (mark-symbol-non-user! (gensym 'env)))
	  (sym  (if (and (pair? stack) (variable? (car stack)))
		    (fqname symbol stack)
		    ;;(symbol-append symbol ': (variable-id (car stack)))
		    symbol))
	  (l    (when (location? loc)
		   `(at ,(location-full-fname loc) ,(location-pos loc))))
	  (exp `(let ((,tmp1 ',sym)
		      (,tmp2 ',l)
		      (,(make-typed-ident tmp3 'dynamic-env) (current-dynamic-env)))
		   (let ()
		      ($env-push-trace ,tmp3 ,tmp1 ,tmp2)
		      (let ((,taux ,node))
			 ,(if (location? lloc)
			      (econs '$env-pop-trace
				     (econs tmp3 '() lloc)
				     lloc)
			      `($env-pop-trace ,tmp3))
			 ,aux))))
	  (nnode (sexp->node exp '() loc 'value)))
      (let-var-removable?-set! nnode (backend-remove-empty-let (the-backend)))
      nnode))

;*---------------------------------------------------------------------*/
;*    trace-node ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (trace-node::node node::node stack level)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::sequence stack level)
   (trace-node*! (sequence-nodes node) stack level)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::sync stack level)
   (with-access::sync node (mutex prelock body)
      (set! mutex (trace-node mutex stack level))
      (set! prelock (trace-node prelock stack level))
      (set! body (trace-node body stack level)))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::app stack level)
   (trace-node*! (app-args node) stack level)
   node)
 
;*---------------------------------------------------------------------*/
;*    trace-node ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::app-ly stack level)
   (with-access::app-ly node (fun arg)
      (set! fun (trace-node fun stack level))
      (set! arg (trace-node arg stack level))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::funcall stack level)
   (with-access::funcall node (fun args)
      (set! fun (trace-node fun stack level))
      (trace-node*! args stack level)
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::extern stack level)
   (trace-node*! (extern-expr* node) stack level)
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::cast stack level)
   (cast-arg-set! node (trace-node (cast-arg node) stack level))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::setq stack level)
   (setq-value-set! node (trace-node (setq-value node) stack level))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::conditional stack level)
   (with-access::conditional node (test true false)
       (set! test (trace-node test stack level))
       (set! true (trace-node true stack level))
       (set! false (trace-node false stack level))
       node))

;*---------------------------------------------------------------------*/
;*    trace-node ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::fail stack level)
   (with-access::fail node (proc msg obj)
      (set! proc (trace-node proc stack level))
      (set! msg (trace-node msg stack level))
      (set! obj (trace-node obj stack level))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::switch stack level)
   (with-access::switch node (clauses test)
      (set! test (trace-node test stack level))
      (for-each (lambda (clause)
		   (set-cdr! clause (trace-node (cdr clause) stack level)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::let-fun stack level)
   (if (isloop? node)
       (with-access::let-fun node (body locals loc)
	  (let* ((var (car locals))
		 (t (strict-node-type (node-type body) (variable-type var)))
		 (id (trace-id var)))
	     (set! body (make-traced-node body t id loc '())))
	  node)
       (with-access::let-fun node (body locals)
	  (for-each (lambda (v) (trace-fun! v stack level)) locals)
	  (set! body (trace-node body stack level))
	  node)))

;*---------------------------------------------------------------------*/
;*    trace-node ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::let-var stack level)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (trace-node (cdr binding) stack level)))
		bindings)
      (set! body (trace-node body stack level))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::set-ex-it stack level)
   (set-ex-it-body-set! node (trace-node (set-ex-it-body node) stack level))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::jump-ex-it stack level)
   (with-access::jump-ex-it node (exit value)
      (set! exit (trace-node exit stack level)) 
      (set! value (trace-node value stack level))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::make-box stack level)
   (make-box-value-set! node (trace-node (make-box-value node) stack level))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::box-ref stack level)
   (box-ref-var-set! node (trace-node (box-ref-var node) stack level))
   node)

;*---------------------------------------------------------------------*/
;*    trace-node ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (trace-node node::box-set! stack level)
   (with-access::box-set! node (var value)
      (set! var (trace-node var stack level))
      (set! value (trace-node value stack level))
      node))

;*---------------------------------------------------------------------*/
;*    trace-node*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (trace-node*! node* stack level)
   (unless (null? node*)
      (set-car! node* (trace-node (car node*) stack level))
      (trace-node*! (cdr node*) stack level)))
   
;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (toplevel-trace-node::node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ::sequence ...                               */
;*---------------------------------------------------------------------*/
(define-method (toplevel-trace-node node::sequence)
   (toplevel-trace-node*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ::sync ...                                   */
;*---------------------------------------------------------------------*/
(define-method (toplevel-trace-node node::sync)
   (sync-mutex-set! node (toplevel-trace-node (sync-mutex node)))
   (sync-prelock-set! node (toplevel-trace-node (sync-prelock node)))
   (sync-body-set! node (toplevel-trace-node (sync-body node)))
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node ::setq ...                                   */
;*    -------------------------------------------------------------    */
;*    This method instruments global variable affections provided      */
;*    those affections are:                                            */
;*      - top level                                                    */
;*      - they assign non trivial values (i.e., non literal values).   */
;*---------------------------------------------------------------------*/
(define-method (toplevel-trace-node node::setq)
   (with-access::setq node (var value loc)
      (with-access::var var (variable)
	 (when (and (global? variable)
		    (not (or (atom? value)
			     (var? value)
			     (kwote? value)
			     (pragma? value))))
	    (let* ((t (strict-node-type (node-type value) (global-type variable)))
		   (trace (make-traced-node value t (trace-id variable) loc '())))
	       (set! value trace)))))
   node)

;*---------------------------------------------------------------------*/
;*    toplevel-trace-node*! ...                                        */
;*---------------------------------------------------------------------*/
(define (toplevel-trace-node*! node*)
   (unless (null? node*)
      (set-car! node* (toplevel-trace-node (car node*)))
      (toplevel-trace-node*! (cdr node*))))
   




