;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/ftypec.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Fri Apr 21 18:43:09 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reduction of type checks.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_flow-typec
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    type_misc
	    coerce_coerce
	    effect_effect
	    ast_var
	    ast_node
	    ast_lvtype
	    object_class)
   (export  (reduce-flow-type-check! globals::pair-nil)))

;*---------------------------------------------------------------------*/
;*    reduce-flow-type-check! ...                                      */
;*    -------------------------------------------------------------    */
;*    This stage implements a simple data flow analysis which          */
;*    is intended to remove duplicated type checks. That is, it        */
;*    re-write:                                                        */
;*       (let ((x ...))                                                */
;*          ...                                                        */
;*          (if (pair? x)                                              */
;*              (begin ... (if (pair? x) then else) ...)))             */
;*    into:                                                            */
;*       (let ((x ...))                                                */
;*          ...                                                        */
;*          (if (pair? x)                                              */
;*              (begin ... then ...)))                                 */
;*                                                                     */
;*    provided x is a read-only variable.                              */
;*                                                                     */
;*    In order to acheive this, this pass implement a traversal        */
;*    that accumulates, in a stack, the types of checked variable.     */
;*    The main compuation is done on conditional node.                 */
;*---------------------------------------------------------------------*/
(define (reduce-flow-type-check! globals)
   (verbose 2 #"      flow type check        ")
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun))) 
		   (sfun-body-set! fun (node-typec! node '()))
		   #unspecified))
	     globals)
   (verbose 2 "(removed: " *type-checks-removed*
	    ", remaining: " *type-checks-remaining* #")\n")
   globals)


;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *type-checks-remaining* 0)
(define *type-checks-removed*   0)

;*---------------------------------------------------------------------*/
;*    node-typec! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (node-typec!::node node::node stack)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::sequence stack)
   (with-access::sequence node (nodes)
      (node-typec*! nodes stack)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::sync stack)
   (with-access::sync node (body mutex prelock)
      (set! mutex (node-typec! mutex stack))
      (set! prelock (node-typec! prelock stack))
      (set! body (node-typec! body stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::app-ly stack)
   (with-access::app-ly node (fun arg)
      (set! fun (node-typec! fun stack))
      (set! arg (node-typec! arg stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::funcall stack)
   (with-access::funcall node (fun args)
      (set! fun (node-typec! fun stack))
      (node-typec*! args stack)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::extern stack)
   (with-access::extern node (expr* type)
      (node-typec*! expr* stack)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::cast stack)
   (with-access::cast node (arg)
      (node-typec! arg stack)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::setq stack)
   (with-access::setq node (var value)
      (set! value (node-typec! value stack))
      (set! var (node-typec! var stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::conditional stack)
   (define (type-checked node)
      (when (app? node)
	 (cond
	    ((isa-of node)
	     =>
	     (lambda (t)
		(with-access::app node (args)
		   (when (var? (car args))
		      t))))
	    (else
	     (with-access::app node (fun args)
		(and (pair? args)
		     (null? (cdr args))
		     (var? (car args))
		     (eq? (variable-access (var-variable (car args))) 'read)
		     (let* ((fun (app-fun node))
			    (val (variable-value (var-variable fun)))
			    (typec (fun-predicate-of val)))
			typec)))))))
   (define (test-static-value node)
      (and (app? node)
	   (with-access::app node (fun args)
	      (and (pair? args)
		   (null? (cdr args))
		   (or (var? (car args)) (atom? (car args)))
		   (let* ((fun (app-fun node))
			  (val (variable-value (var-variable fun)))
			  (typec (fun-predicate-of val))
			  (typev (get-type (car args) #f)))
		      (cond
			 ((not (type? typec))
			  ;; this is not a predicate
			  #f)
			 ((eq? typev *obj*)
			  ;; we have not idea of the result of the type
			  #f)
			 ((type-less-specific? typec typev)
			  'true)
			 ((type-disjoint? typec typev)
			  'false)
			 (else
			  #f)))))))
   (with-access::conditional node (test true false type)
      (set! test (node-typec! test stack))
      (let ((typec (type-checked test)))
	 (if (not (type? typec))
	     (begin
		(set! true (node-typec! true stack))
		(set! false (node-typec! false stack))
		node)
	     (let* ((args (app-args test))
		    (var (var-variable (car args)))
		    (types (assq var stack))
		    (t-stack (cons (cons var (cons typec #t)) stack))
		    (f-stack (cons (cons var (cons typec #f)) stack)))
		(if (or (not types) (not (eq? (cadr types) typec)))
		    (begin
		       (set! *type-checks-remaining*
			     (+fx 1 *type-checks-remaining*))
		       (set! true (node-typec! true t-stack))
		       (set! false (node-typec! false f-stack))
		       node)
		    (begin
		       (set! *type-checks-removed*
			     (+fx 1 *type-checks-removed*))
		       (if (cddr types)
			   (node-typec! true t-stack)
			   (node-typec! false f-stack)))))))))

;*---------------------------------------------------------------------*/
;*    node-typec! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::fail stack)
   (with-access::fail node (type proc msg obj)
      (set! proc (node-typec! proc stack))
      (set! msg (node-typec! msg stack))
      (set! obj (node-typec! obj stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::switch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::switch stack)
   (with-access::switch node (clauses test)
      (set! test (node-typec! test stack))
      (for-each (lambda (clause)
		   (set-cdr! clause (node-typec! (cdr clause) stack)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::let-fun stack)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (node-typec! (sfun-body fun) stack))))
		locals)
      (set! body (node-typec! body stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::let-var stack)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (node-typec! (cdr binding) stack)))
		bindings)
      (set! body (node-typec! body stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::set-ex-it stack)
   (with-access::set-ex-it node (var body)
      (set! body (node-typec! body stack))
      (set! var (node-typec! var stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::jump-ex-it stack)
   (with-access::jump-ex-it node (exit value)
      (set! exit (node-typec! exit stack))
      (set! value (node-typec! value stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::make-box stack)
   (with-access::make-box node (value)
      (set! value (node-typec! value stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::box-set! stack)
   (with-access::box-set! node (var value)
      (set! var (node-typec! var stack))
      (set! value (node-typec! value stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::box-ref stack)
   (with-access::box-ref node (var)
      (set! var (node-typec! var stack))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (node-typec*! node* stack)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (node-typec! (car node*) stack))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    node-typec! ::app ...                                            */
;*    -------------------------------------------------------------    */
;*    The subtype relationship between nil, pair, epair and pair-nil   */
;*    is hard coded into that function.                                */
;*    @label pair-nil subtyping@                                       */
;*    @ref ../../runtime/Llib/type.scm:pair-nil subtyping@             */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::app stack)
   (with-access::app node (fun args loc)
      (node-typec*! args stack)
      node))

