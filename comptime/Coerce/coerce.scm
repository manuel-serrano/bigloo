;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/coerce.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:57:49 1995                          */
;*    Last change :  Mon Nov 29 09:01:18 2010 (serrano)                */
;*    Copyright   :  1995-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We coerce an Ast                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_coerce
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    tools_error
	    type_type
	    type_cache
	    type_coercion
	    type_typeof
	    type_misc
	    object_class
	    ast_var
	    ast_node
	    coerce_pproto
	    coerce_convert
	    coerce_app
	    coerce_apply
	    coerce_funcall
	    effect_effect)
   (export  (coerce-function! ::variable ::bool)
	    (generic coerce!::node ::node ::obj ::type ::bool)))

;*---------------------------------------------------------------------*/
;*    the-coerced-function ...                                         */
;*---------------------------------------------------------------------*/
(define the-coerced-function #unspecified)

;*---------------------------------------------------------------------*/
;*    coerce-function! ...                                             */
;*---------------------------------------------------------------------*/
(define (coerce-function! variable type-safe)
   (trace coerce #"\ncoerce-function!: " (shape variable) #"\n")
   (enter-function (variable-id variable))
   (let* ((fun  (variable-value variable))
	  (body (sfun-body fun))
	  (tres (variable-type variable))
	  (clo (sfun-the-closure-global fun))
	  (type-safety-enforced (and (not *unsafe-eval*)
				     (global? variable)
				     (global? clo)
				     (global-evaluable? clo)
				     (global-user? clo)))
	  (type-safe (or type-safe type-safety-enforced)))
      (if (global? variable)
	  (trace (coerce 2) "  type-safe=" type-safe
		 " global=" (global? variable)
		 " evaluable=" (global-evaluable? variable)
		 " user=" (global-user? variable)
		 " clo=" (global? (sfun-the-closure-global fun))
		 "\n")
	  (trace (coerce 2) "  type-safe=" type-safe "\n"))
      (pfunction-proto 3 variable)
      (set! the-coerced-function variable)
      (sfun-body-set! fun (coerce! body variable tres type-safe))
      (leave-function)))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (coerce!::node node::node caller to::type safe::bool))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::atom caller to safe)
   (convert! node (get-type node) to safe))
 
;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::kwote caller to safe)
   (convert! node (get-type node) to safe))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*    -------------------------------------------------------------    */
;*    The coerce transformation on variable references has been        */
;*    made more complex with the introduction of the type dataflow     */
;*    analysis. With this new analysis, a variable is of a certain     */
;*    type, that it not necessarily the type returned by GET-TYPE      */
;*    which might produce a more precise type (for instance, a         */
;*    variable of the type obj might be used as a pair on a particular */
;*    reference). Hence, it is required here to distinguish between    */
;*    the static type and the reference type.                          */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::var caller to safe)
   (with-access::var node (variable)
      (let ((ntype (get-type node))
	    (vtype (variable-type variable)))
	 (cond
	    ((eq? vtype to)
	     node)
	    ((eq? ntype vtype)
	     (convert! node ntype to safe))
	    (else
	     (convert! (convert! node vtype ntype #f) ntype to safe))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::closure ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::closure caller to safe)
   (internal-error "coerce!" "Unexepected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    coerce! ::sequence ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::sequence caller to safe)
   (with-access::sequence node (nodes)
      (let loop ((hook nodes))
	 (if (null? (cdr hook))
	     (begin
		(set-car! hook (coerce! (car hook) caller to safe))
		node)
	     (begin
		;; yes, it is strange, we coerce to the type of
		;; the expression !
		(set-car! hook (coerce! (car hook)
					caller
					(get-type (car hook))
					safe))
		(loop (cdr hook)))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::extern ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::extern caller to safe)
   (with-access::extern node (expr*)
      (let loop ((values expr*))
	 (if (null? values)
	     (convert! node (get-type node) to safe)
	     (begin
		(set-car! values (coerce! (car values)
					  caller
					  (get-type (car values))
					  safe))
		(loop (cdr values)))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::getfield ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::getfield caller to safe)
   (with-access::getfield node (expr* ftype otype)
      ;; there is no need to type check the argument because since
      ;; getfield/setfield forms are introduced by the compiler they
      ;; are always type safe
      (set-car! expr* (coerce! (car expr*) caller *obj* safe))
      (convert! node ftype to safe)))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::setfield ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::setfield caller to safe)
   (with-access::setfield node (expr* type ftype otype)
      (set-car! expr* (coerce! (car expr*) caller *obj* safe))
      (set-car! (cdr expr*) (coerce! (cadr expr*) caller ftype safe))
      (convert! node *unspec* to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::new ...                                                */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::new caller to safe)
   (with-access::new node (expr* type)
      (let loop ((l expr*))
	 (if (null? l)
	     (convert! node type to safe)
	     (begin
		(set-car! l (coerce! (car l)
				     caller
				     (get-type (car l))
				     safe))
		(loop (cdr l)))))
      (convert! node type to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::valloc ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::valloc caller to safe)
   (with-access::valloc node (type otype expr*)
      (set-car! expr* (coerce! (car expr*) caller otype safe))
      (convert! node type to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::vref ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::vref caller to safe)
   (with-access::vref node (expr* vtype ftype otype)
      ;; when compiling for vector optimization, it may happens
      ;; that the ftype of the node VREF is still *_* (because the cfa
      ;; has not been able to unpatch it). This situation arises for non
      ;; optimized vectors that are accessed by the C function C-VECTOR-REF
      ;; instead of the regular function VECTOR-REF). So we have to make
      ;; a special rule in the typing system.
      (let ((ftype (if (eq? ftype *_*) *obj* ftype)))
	 (set-car! expr* (coerce! (car expr*) caller vtype safe))
	 (set-car! (cdr expr*) (coerce! (cadr expr*) caller otype safe))
	 (convert! node ftype to safe))))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::vset! ...                                              */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::vset! caller to safe)
   (with-access::vset! node (expr* type vtype ftype otype)
      ;; same remark as for VREF.
      (let ((ftype (if (eq? ftype *_*) *obj* ftype)))
	 (set-car! expr* (coerce! (car expr*) caller vtype safe))
	 (set-car! (cdr expr*) (coerce! (cadr expr*) caller otype safe))
	 (set-car! (cddr expr*) (coerce! (caddr expr*) caller ftype safe))
	 (convert! node type to safe))))

;*---------------------------------------------------------------------*/
;*    coerce! ::vlength ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::vlength caller to safe)
   (with-access::vlength node (expr* type vtype)
      (set-car! expr* (coerce! (car expr*) caller vtype safe))
      (convert! node type to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::cast ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::cast caller to safe)
   (with-access::cast node (arg type)
      (trace (coerce 2) "coerce-cast!: " (shape node) " -> " (shape to)
	     #\Newline)
      (set! arg (coerce! arg caller to safe))
      (convert! node type to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::setq ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::setq caller to safe)
   (with-access::setq node (var value)
      (set! value (coerce! value
			   caller
			   (variable-type (var-variable var))
			   safe))
      (convert! node *unspec* to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::conditional ...                                        */
;*    -------------------------------------------------------------    */
;*    We play here a special trick in order to avoid reporting         */
;*    "illegal" type errors because of a lack of data flow             */
;*    information propagation. Basically what is done here is          */
;*    the following:                                                   */
;*     - the test is typed.                                            */
;*     - if the test if statically recognize as #t or #f               */
;*          then only on branch is compiled.                           */
;*          else the whole expression is compiled.                     */
;*    Of course, all this is very bad because it makes strong          */
;*    assumption on the shape of the test. If this shape changes,      */
;*    the compiler will, one more time, erroneously report "false"     */
;*    type errors.                                                     */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::conditional caller to safe)
   (define (test-static-app node)
      (with-access::app node (fun args)
	 (and (pair? args)
	      (null? (cdr args))
	      (not (side-effect? (car args)))
	      (let* ((fun (app-fun node))
		     (val (variable-value (var-variable fun)))
		     (typec (fun-predicate-of val))
		     (typev (get-type (car args))))
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
		     #f))))))
   (define (test-static-let-var node)
      (with-access::let-var node (bindings body)
	 (and (pair? bindings)
	      (null? (cdr bindings))
	      (app? body)
	      (let* ((fun (app-fun body))
		     (var (let ((args (app-args body)))
			     (and (pair? args)
				  (null? (cdr args))
				  (var? (car args))
				  (var-variable (car args)))))
		     (val (variable-value (var-variable fun)))
		     (typec (fun-predicate-of val))
		     (typep (variable-type (car (car bindings))))
		     (typev (if (eq? typep *obj*)
				(get-type (cdr (car bindings)))
				typep)))
		 (cond
		    ((not (type? typec))
		     ;; this is not a predicate
		     #f)
		    ((not (eq? var (car (car bindings))))
		     ;; the predicate does not concern the let-var variable
		     #f)
		    ((eq? typev *obj*)
		     ;; we have not idea of the result of the type
		     #f)
		    ((type-less-specific? typec typev)
		     'true)
		    ((type-disjoint? typec typev)
		     'false)
		    (else
		     #f))))))
   (define (test-static-value node)
      (cond
	 ((app? node)
	  (test-static-app node))
	 ((let-var? node)
	  (test-static-let-var node))
	 (else
	  #f)))
   (with-access::conditional node (test true false type)
      (set! test (coerce! test caller *bool* safe))
      (case (test-static-value test)
	 ((true)
	  (coerce! true caller to safe))
	 ((false)
	  (coerce! false caller to safe))
	 (else
	  (set! true (coerce! true caller to safe))
	  (set! false (coerce! false caller to safe))
	  (set! type to)
	  node))))

;*---------------------------------------------------------------------*/
;*    coerce! ::fail ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::fail caller to safe)
   (with-access::fail node (proc msg obj)
      (set! proc (coerce! proc caller *obj* safe))
      (set! msg (coerce! msg caller *obj* safe))
      (set! obj (coerce! obj caller *obj* safe)) 
      (convert! node *magic* to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::select ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::select caller to safe)
   (with-access::select node (loc clauses test type)
      (set! type to)
      (let ((clauses        clauses)
	    (test-type      (select-item-type node))
	    (test-node-type (get-type test)))
	 ;; select constructions are normalized: the test should have
	 ;; been placed in a variable. That's why this test below should
	 ;; work. This test may fails (in strange cases that I'm currently
	 ;; ignoring) and then, it may happens that some `correct' select
	 ;; construction could be rejected. These forms are those where the
	 ;; else clause trap objects of different types from the one tested
	 ;; in the clauses.
	 (if (not (coercer-exists? test-node-type test-type))
	     (coerce! (runtime-type-error loc (type-id test-type) test)
		      caller
		      to
		      safe)
	     (begin
		(select-test-set! node (coerce! test caller test-type safe))
		(for-each (lambda (clause)
			     (set-cdr! clause (coerce! (cdr clause)
						       caller
						       to
						       safe)))
			  clauses)
		node)))))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::let-fun ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::let-fun caller to safe)
   (with-access::let-fun node (body locals)
      (inc-ppmarge!)
      (for-each (lambda (f) (coerce-function! f (not *unsafe-type*))) locals)
      (set! body (coerce! body caller to safe))
      (dec-ppmarge!)
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::let-var caller to safe)
   (trace (coerce 3) "coercer ::let-var: " (shape node) " -> " (shape to)
	  #\Newline)
   (with-access::let-var node (body bindings)
      (inc-ppmarge!)
      (for-each (lambda (binding)
		   (pvariable-proto 3 (car binding))
		   (set-cdr! binding (coerce! (cdr binding)
					      caller 
					      (local-type (car binding))
					      safe)))
		bindings)
      (set! body (coerce! body caller to safe))
      (dec-ppmarge!)
      node))
 
;*---------------------------------------------------------------------*/
;*    coerce! ::set-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::set-ex-it caller to safe)
   (with-access::set-ex-it node (var body)
      (set! var (coerce! var caller *exit* safe))
      (pvariable-proto 3 (var-variable var))
      (set! body (coerce! body caller to safe))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::jump-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::jump-ex-it caller to safe)
   (with-access::jump-ex-it node (exit value)
      (set! exit (coerce! exit caller *exit* safe))
      (set! value (coerce! value caller to safe))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::make-box ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::make-box caller to safe)
   (with-access::make-box node (value)
      (set! value (coerce! value caller *obj* safe))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::box-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::box-ref caller to safe)
   (with-access::box-ref node (var)
      (convert! node *obj* to safe)))

;*---------------------------------------------------------------------*/
;*    coerce! ::box-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::box-set! caller to safe)
   (with-access::box-set! node (var value)
      (local-type-set! (var-variable var) *obj*)
      (set! value (coerce! value caller *obj* safe))
      (convert! node *unspec* to safe)))

