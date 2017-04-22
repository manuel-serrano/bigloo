;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/coerce.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:57:49 1995                          */
;*    Last change :  Fri Apr 21 18:46:24 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
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
	    ast_dump
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
      (let ((notify *notify-type-test*))
	 (set! *notify-type-test*
	    (and (variable-user? variable)
		 (not (global? (sfun-the-closure-global fun)))))
	 (pfunction-proto 3 variable)
	 (set! the-coerced-function variable)
	 (sfun-body-set! fun (coerce! body variable tres type-safe))
	 (set! *notify-type-test* notify))
      (leave-function)))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (coerce!::node node::node caller to::type safe::bool))

;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::atom caller to safe)
   (convert! node (get-type node #f) to safe))
 
;*---------------------------------------------------------------------*/
;*    coerce! ...                                                      */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::kwote caller to safe)
   (convert! node (get-type node #f) to safe))

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
   (with-access::var node (type variable)
      (let ((ntype (get-type node #f))
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
   (with-access::sequence node (type nodes unsafe)
      (let ((s (and (not unsafe) safe)))
	 (let loop ((nodes nodes))
	    (let ((n (car nodes)))
	       (if (null? (cdr nodes))
		   (begin
		      (set-car! nodes (coerce! n caller to s))
		      (set! type (strict-node-type to type))
		      node)
		   (begin
		      (set-car! nodes (coerce! n caller (get-type n #f) s))
		      (loop (cdr nodes)))))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::sync ...                                               */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::sync caller to safe)
   (with-access::sync node (type body mutex prelock)
      (set! mutex (coerce! mutex caller *mutex* safe))
      (set! prelock (coerce! prelock caller *pair-nil* safe))
      (set! body (coerce! body caller to safe))
      (set! type (strict-node-type to type))
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::extern ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::extern caller to safe)
   (with-access::extern node (expr*)
      (let loop ((values expr*))
	 (if (null? values)
	     (convert! node (get-type node #f) to safe)
	     (let* ((v (car values))
		    (nv (coerce! v caller (get-type v #f) safe)))
		(set-car! values nv)
		(loop (cdr values)))))))

;*---------------------------------------------------------------------*/
;*    coerce! ::widening ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::widening caller to safe)
   ;; MS CARE 16nov2011, NOT USED YET
   (with-access::widening node (expr* otype)
      ;; the object might have been shrunk
      (let ((super (tclass-its-super otype)))
	 (set-car! expr* (coerce! (car expr*) caller super safe))
      (convert! node *obj* to safe))))

;*---------------------------------------------------------------------*/
;*    coerce! ::getfield ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::getfield caller to safe)
   (with-access::getfield node (expr* ftype otype)
      (set-car! expr* (coerce! (car expr*) caller otype safe))
      (convert! node ftype to safe)))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::setfield ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::setfield caller to safe)
   (with-access::setfield node (expr* type ftype otype)
      (set-car! expr* (coerce! (car expr*) caller otype safe))
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
	     (let* ((v (car l))
		    (nv (coerce! v caller (get-type v #f) safe)))
		(set-car! l nv)
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
      (set! arg (coerce! arg caller (get-type arg #f) safe))
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
	      (let* ((typec (app-predicate-of node))
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
		     #f))))))
   (define (test-static-let-var node)
      (with-access::let-var node (bindings body)
	 (when (and (pair? bindings) (null? (cdr bindings)))
	    (cond
	       ((app? body)
		(let* ((var (let ((args (app-args body)))
			       (cond
				  ((not (pair? args))
				   #f)
				  ((and (null? (cdr args)) (var? (car args)))
				   ;; predicates
				   (var-variable (car args)))
				  ((not (pair? (cdr args)))
				   #f)
				  ((and (null? (cddr args)) (var? (car args)))
				   ;; isa
				   (var-variable (car args)))
				  (else
				   #f))))
		       (typec (app-predicate-of body))
		       (typep (variable-type (car (car bindings))))
		       (typev (if (eq? typep *obj*)
				  (get-type (cdr (car bindings)) #f)
				  typep)))
		   (cond
		      ((not (type? typec))
		       ;; this is not a predicate
		       #f)
		      ((not (eq? var (caar bindings)))
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
		       #f))))
	       ((and (var? body) (eq? (var-variable body) (caar bindings)))
		(test-static-value (cdar bindings)))
	       (else
		#f)))))
   (define (test-static-isa node typec)
      (with-access::app node (args)
	 (let ((typev (cond
			 ((var? (car args))
			  (get-type (car args) #f))
			 ((cast? (car args))
			  (with-access::cast (car args) (type arg)
			     (when (and (eq? type *obj*) (var? arg))
				(get-type arg #f)))))))
	    (cond
	       ((not (type? typev))
		#f)
	       ((type-less-specific? typec typev)
		'true)
	       ((type-disjoint? typec typev)
		'false)
	       (else
		#f)))))
   (define (test-static-value node)
      (cond
	 ((isa-of node)
	  =>
	  (lambda (t)
	     (test-static-isa node t)))
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
;*    coerce! ::switch ...                                             */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::switch caller to safe)
   (with-access::switch node (loc clauses test type)
      (set! type to)
      (let ((clauses clauses)
	    (test-type (switch-item-type node))
	    (test-node-type (get-type test #f)))
	 (unless (coercer-exists? test-node-type test-type)
	    (tprint "switch test-type=" (shape test-type)
	       " test-node-type=" (shape test-node-type)))
	 ;; switch constructions are normalized: the test should have
	 ;; been placed in a variable. That's why this test below should
	 ;; work. This test may fail (in strange cases that I'm currently
	 ;; ignoring) and then, it may happen that some `correct' switch
	 ;; forms could be rejected. These forms are those where the
	 ;; else clause trap objects of different types from the one tested
	 ;; in the clauses.
	 (if (coercer-exists? test-node-type test-type)
	     (begin
		(switch-test-set! node (coerce! test caller test-type safe))
		(for-each (lambda (clause)
			     (set-cdr! clause
				(coerce! (cdr clause) caller to safe)))
		   clauses)
		node)
	     (coerce! (runtime-type-error loc (type-id test-type) test)
		caller to safe)))))
      
;*---------------------------------------------------------------------*/
;*    coerce! ::let-fun ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::let-fun caller to safe)
   (with-access::let-fun node (type body locals)
      (inc-ppmarge!)
      (for-each (lambda (f) (coerce-function! f (not *unsafe-type*))) locals)
      (set! body (coerce! body caller to safe))
      (set! type (strict-node-type (node-type body) type))
      (dec-ppmarge!)
      node))

;*---------------------------------------------------------------------*/
;*    coerce! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::let-var caller to safe)
   (trace (coerce 3) "coercer ::let-var: " (shape node) " -> " (shape to)
	  #\Newline)
   (with-access::let-var node (type body bindings)
      (inc-ppmarge!)
      (for-each (lambda (binding)
		   (pvariable-proto 3 (car binding))
		   (set-cdr! binding (coerce! (cdr binding)
					      caller 
					      (local-type (car binding))
					      safe)))
		bindings)
      (set! body (coerce! body caller to safe))
      (set! type (strict-node-type (node-type body) type))
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
   (with-access::jump-ex-it node (exit value type)
      (set! exit (coerce! exit caller *exit* safe))
      (set! value (coerce! value caller (get-type value #f) safe))
      (convert! node type to safe)))

;*---------------------------------------------------------------------*/
;*    cast-obj ...                                                     */
;*---------------------------------------------------------------------*/
(define (cast-node node to)
   (with-access::node node (loc)
      (instantiate::cast
	 (loc loc)
	 (type to)
	 (arg node))))

;*---------------------------------------------------------------------*/
;*    coerce! ::make-box ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::make-box caller to safe)
   (with-access::make-box node (value vtype)
      (let ((nodev (coerce! value caller vtype safe)))
	 (if (tclass? (get-type nodev #f))
	     (set! value (cast-node nodev *obj*))
	     (set! value nodev)))
      node))
;
;*---------------------------------------------------------------------*/
;*    coerce! ::box-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::box-ref caller to safe)
   (with-access::box-ref node (var type vtype loc)
      (let ((cnode (convert! node vtype to safe)))
	 (if (tclass? to)
	     (cast-node cnode to)
	     cnode))))

;*---------------------------------------------------------------------*/
;*    coerce! ::box-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (coerce! node::box-set! caller to safe)
   (with-access::box-set! node (var value vtype type)
      (let ((vt (coerce! value caller vtype safe)))
	 (set! value (coerce! vt caller *obj* safe)))
      (convert! node type to safe)))

