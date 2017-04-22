;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/type.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 27 10:33:17 1996                          */
;*    Last change :  Fri Apr 21 18:47:05 2017 (serrano)                */
;*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the obvious type election (taking care of tvectors).     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_type
   (include "Tools/trace.sch")
   (import  type_type
	    type_cache
	    type_typeof
	    tools_shape
	    tools_error
	    engine_param
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_set
	    cfa_tvector
	    cfa_closure
	    tvector_tvector
	    object_class)
   (export  (type-settings! globals)
	    (get-approx-type ::approx ::obj)))

;*---------------------------------------------------------------------*/
;*    type-settings! ...                                               */
;*---------------------------------------------------------------------*/
(define (type-settings! globals)
   (trace (cfa 2) "====== type-settings! ======================\n")
   ;; set a polymorphic type for the non-optimized procedures
   (type-closures!)
   ;; type the functions body
   (for-each type-fun! globals))

;*---------------------------------------------------------------------*/
;*    type-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-fun! var::variable)
   (let ((fun (variable-value var)))
      (trace (cfa 2) "type-fun! var=" (shape var) " "
	 (typeof fun) #\Newline)
      (cond
	 ((intern-sfun/Cinfo? fun)
	  ;; if it is not an `intern-sfun/Cinfo', it means that the
	  ;; procedure is unreachable and then we can ignore it.
	  (with-access::intern-sfun/Cinfo fun (body args approx)
	     ;; the formals
	     (for-each (lambda (var)
			  (trace (cfa 3) "  formal " (shape var)
			     " " (typeof (local-value var)) #\Newline)
			  (type-variable! (local-value var) var))
		       args)
	     ;; and the function result
	     (set-variable-type! var (get-approx-type approx var))
	     (shrink! fun)
	     ;; the body
	     (set! body (type-node! body))))
	 ((sfun? fun)
	  (with-access::sfun fun (body)
	     (set! body (type-node! body))))
	 (else
	  (internal-error "type-fun!" "Unknown value" (shape var))))))

;*---------------------------------------------------------------------*/
;*    type-fun-node! ...                                               */
;*---------------------------------------------------------------------*/
(define (type-fun-node! v::var)
   (let* ((var (var-variable v))
	  (fun (variable-value var)))
      (when (intern-sfun/Cinfo? fun)
	 (with-access::intern-sfun/Cinfo fun (approx)
	    (set-variable-type! var (get-approx-type approx v))))))

;*---------------------------------------------------------------------*/
;*    get-approx-type ...                                              */
;*---------------------------------------------------------------------*/
(define (get-approx-type approx node)
   (let ((type (approx-type approx))
	 (alloc-list (set->list (approx-allocs approx))))
      (cond
	 ((not (pair? alloc-list))
	  type)
	 ((approx-top? approx)
	  *obj*)
	 ((every make-vector-app? alloc-list)
	  (let* ((app (car alloc-list))
		 (tv-type (get-vector-item-type app))
		 (value-approx (make-vector-app-value-approx app))
		 (item-type (approx-type value-approx))
		 (tv (type-tvector item-type)))
	     (cond
		((type? tv) tv)
		((eq? type *_*) *vector*)
		(else type))))
	 ((every valloc/Cinfo+optim? alloc-list)
	  (if (not (tvector-optimization?))
	      (if (eq? type *_*)
		  *vector*
		  type)
	      (let* ((app (car alloc-list))
		     (tv-type (get-vector-item-type app))
		     (value-approx (valloc/Cinfo+optim-value-approx app))
		     (item-type (approx-type value-approx))
		     (tv (type-tvector item-type)))
		 (cond
		    ((type? tv) tv)
		    ((eq? type *_*) *vector*)
		    (else type)))))
	 (else
	  type))))
	 
;*---------------------------------------------------------------------*/
;*    type-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (type-variable! value::value variable::variable)
   (let ((type (variable-type variable)))
      (cond
	 ((not (type? type))
	  (set-variable-type! variable (get-default-type)))
	 ((and (eq? type *_*) (not *optim-cfa?*))
	  (set-variable-type! variable (get-default-type))))))

;*---------------------------------------------------------------------*/
;*    type-variable! ::svar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-variable! value::svar/Cinfo variable)
   (with-access::svar/Cinfo value (approx)
      (let ((typ (get-approx-type approx value)))
	 (trace (cfa 4) "   type-variable " (shape variable) " -> " (shape typ)
	    #\Newline)
	 (set-variable-type! variable typ))))
   
;*---------------------------------------------------------------------*/
;*    type-variable! ::scnst ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-variable! value::scnst/Cinfo variable)
   'nothing)
   
;*---------------------------------------------------------------------*/
;*    type-variable! ::cvar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-variable! value::cvar/Cinfo variable)
    (with-access::cvar/Cinfo value (approx)
      (set-variable-type! variable (get-approx-type approx value))))

;*---------------------------------------------------------------------*/
;*    type-variable! ::sexit ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-variable! value::sexit/Cinfo variable)
   'nothing)

;*---------------------------------------------------------------------*/
;*    type-variable! ::intern-sfun/Cinfo ...                           */
;*    -------------------------------------------------------------    */
;*    We reach this method each time a `make-procedure' is scanned.    */
;*    This node has no effect. Its value is never used. It can be      */
;*    ignore but for the typing system, it as to be an `approx'.       */
;*---------------------------------------------------------------------*/
(define-method (type-variable! value::intern-sfun/Cinfo variable)
   'nothing)
   
;*---------------------------------------------------------------------*/
;*    set-variable-type! ...                                           */
;*    -------------------------------------------------------------    */
;*    We set the type of the variable in two different circumstances:  */
;*      1- the variable has no type yet (*_* type)                     */
;*      2- the variable has the type *vector* but this variable        */
;*         holds actually optimized tvectors.                          */
;*---------------------------------------------------------------------*/
(define (set-variable-type! variable::variable type::type)
   (let ((ntype (if (eq? type *_*) *obj* type))
	 (otype (variable-type variable)))
      (cond
	 ((eq? otype *_*)
	  (if (and (global? variable)
		   (not (fun? (global-value variable)))
		   (not (or *unsafe-type* (eq? (global-init variable) #t)))
		   (not (eq? (type-class ntype) 'bigloo)))
	      ;; due to the pbm of the initialization order, global variables
	      ;; have to be checked before read. This means that it is
	      ;; mandatory that there type is checkable and then, it means
	      ;; that there type is a Bigloo type.
	      (variable-type-set! variable *obj*)
	      (variable-type-set! variable ntype)))
	 ((and (eq? otype *vector*) (tvec? ntype))
	  (variable-type-set! variable ntype)))))

;*---------------------------------------------------------------------*/
;*    type-node! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (type-node!::node node::node))

;*---------------------------------------------------------------------*/
;*    type-node! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    type-node! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::kwote)
   (with-access::kwote node (type value)
      (when (eq? type *_*)
	 (set! type (get-type-kwote value))))
   node)

;*---------------------------------------------------------------------*/
;*    type-node! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::var)
   
   (define (type-more-specific? ntype vtype)
      (or (not (bigloo-type? vtype))
	  (eq? ntype *obj*)
	  (or (eq? vtype *pair*) (eq? vtype *epair*) )
	  (and (tclass? vtype) (tclass? ntype) (type-subclass? vtype ntype))))
   
   (with-access::var node (variable type)
      (when (and (global? variable) (eq? (global-import variable) 'static))
	 (type-variable! (global-value variable) variable))
      (when (or (eq? type *_*)
		(eq? type *vector*)
		(type-more-specific? type (variable-type variable)))
	 (set! type (variable-type variable))))
   node)

;*---------------------------------------------------------------------*/
;*    type-node! ::closure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::closure)
   (internal-error "type-node!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    type-node! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::sequence)
   (with-access::sequence node (type nodes)
      (type-node*! nodes)
      (when (eq? type *_*)
	 (if (pair? nodes)
	     (set! type (get-type (car (last-pair nodes)) #f))
	     (set! type *unspec*)))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::sync)
   (with-access::sync node (type body mutex prelock)
      (set! mutex (type-node! mutex))
      (set! prelock (type-node! prelock))
      (set! body (type-node! body))
      (when (eq? type *_*)
	 (set! type (get-type body #f)))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app)
   (with-access::app node (type fun args)
      (type-node*! args)
      (type-fun-node! fun)
      (set! type (strict-node-type (variable-type (var-variable fun)) type))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::arithmetic-app ...                                  */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::arithmetic-app)
   
   (define (cleanup-type t)
      (cond
	 ((type? t)
	  (if (eq? t *_*) *obj* t))
	 ((local? t)
	  (when (eq? (local-type t) *_*)
	     (local-type-set! t *obj*))
	  t)
	 (else
	  t)))
   
   (call-next-method)
   
   (with-access::app node (fun type args)
      ;; cleanup the function type
      (let* ((v (var-variable fun))
	     (val (variable-value v)))
	 (when (eq? (variable-type v) *_*)
	    (variable-type-set! v *obj*))
	 ;; cleanup the arguments type
	 (cond
	    ((sfun? val)
	     (map! cleanup-type (sfun-args val)))
	    ((cfun? val)
	     (map! cleanup-type (cfun-args-type val)))))
      ;; cleanup the node type
      (when (eq? type *_*)
	 (set! type *obj*)))
   
   node)

;*---------------------------------------------------------------------*/
;*    get-procedure-approx-type ...                                    */
;*---------------------------------------------------------------------*/
(define (get-procedure-approx-type approx node)
   (if (approx-procedure-el? approx)
       *procedure-el*
       (get-approx-type approx node)))
   
;*---------------------------------------------------------------------*/
;*    type-node! ::procedure-ref-app ...                               */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::procedure-ref-app)
   (with-access::procedure-ref-app node (args approx type fun)
      (call-next-method)
      (if *optim-cfa-free-var-tracking?*
	  (let ((atype (get-procedure-approx-type approx node)))
	     (if (and (bigloo-type? atype)
		      (not (eq? atype *_*))
		      (not (eq? atype (get-type node #f))))
		 (instantiate::cast
		    (type atype)
		    (arg node))
		 node))
	  node)))

;*---------------------------------------------------------------------*/
;*    type-node! ::procedure-set!-app ...                              */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::procedure-set!-app)
   (with-access::procedure-set!-app node (args approx vapprox type fun)
      (call-next-method)
      (when *optim-cfa-free-var-tracking?*
	 (let ((atype (get-procedure-approx-type vapprox node)))
	    (when (and (bigloo-type? atype)
		       (not (eq? atype *_*))
		       (not (eq? atype (get-type (caddr args) #f))))
	       (set-car! (cddr args)
			 (instantiate::cast
			    (type *obj*)
			    (arg (caddr args)))))))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::cons-ref-app ...                                    */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::cons-ref-app)
   (with-access::cons-ref-app node (args approx type)
      (call-next-method)
      (let ((atype (get-approx-type approx node)))
	 (unless (eq? atype *_*)
	    (set! type atype)))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app-ly)
   (when *optim-cfa?* (error "type-node!" "unexpected node" (shape node)))
   (with-access::app-ly/Cinfo node (fun arg type)
      (set! fun (type-node! fun))
      (set! arg (type-node! arg))
      (set! type *obj*)
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::app-ly/Cinfo ...                                    */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app-ly/Cinfo)
   (with-access::app-ly/Cinfo node (fun arg type approx)
      (set! fun (type-node! fun))
      (set! arg (type-node! arg))
      (if *optim-cfa-unbox-closure-args*
	  (set! type (get-approx-type approx node))
	  (set! type *obj*))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::funcall)
   (when *optim-cfa?* (error "type-node!" "unexpected node" (shape node)))
   (with-access::funcall node (fun args type)
      (set! fun (type-node! fun))
      (type-node*! args)
      (set! type *obj*)
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::funcall/Cinfo ...                                   */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::funcall/Cinfo)
   (with-access::funcall/Cinfo node (fun args type approx)
      (set! fun (type-node! fun))
      (type-node*! args)
      (if *optim-cfa-unbox-closure-args*
	  (let ((typ (get-approx-type approx node)))
	     (if (eq? typ *_*)
		 ;; the function is actually never called,
		 ;; the funcall node is removed
		 (begin
		    (set! type *obj*)
		    node)
		 (begin
		    ;; check type-closures! (loc2glo.scm), non optimized closures
		    ;; must return bigloo boxed types
		    (if (memq (funcall-strength node) '(light elight))
			(set! type typ)
			(set! type (get-bigloo-type typ)))
		    node)))
	  (begin
	     (set! type *obj*)
	     node))))

;*---------------------------------------------------------------------*/
;*    type-node! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::extern)
   (with-access::extern node (expr*)
      (type-node*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::valloc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::valloc)
   (call-next-method)
   (with-access::valloc node (expr* type ftype)
      (type-node*! expr*)
      (set! type (strict-node-type (if (eq? type *_*) *vector* type) type))
      (set! ftype (if (eq? ftype *_*) *obj* ftype))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::vref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::vref)
   (call-next-method)
   (with-access::vref node (ftype type)
      (when (eq? ftype *_*)
	 (set! ftype *obj*))
      (set! type ftype))
   node)
      
;*---------------------------------------------------------------------*/
;*    type-node! ::vset! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::vset!)
   (call-next-method)
   (with-access::vset! node (ftype)
      (when (eq? ftype *_*)
	 (set! ftype *obj*)))
   node)
      
;*---------------------------------------------------------------------*/
;*    type-node! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::cast)
   (with-access::cast node (arg)
      (set! arg (type-node! arg))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::setq)
   (with-access::setq node (var value)
      (set! value (type-node! value))
      (set! var (type-node! var))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::conditional ...                                     */
;*    -------------------------------------------------------------    */
;*    This computes the lub                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::conditional)
   (with-access::conditional node (type test true false)
       (set! test (type-node! test))
       (set! true (type-node! true))
       (set! false (type-node! false))
       (set! type (get-type node #f))
       node))

;*---------------------------------------------------------------------*/
;*    type-node! ::conditional/Cinfo ...                               */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::conditional/Cinfo)
   (call-next-method)
   node)

;*---------------------------------------------------------------------*/
;*    type-node! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::fail)
   (with-access::fail node (type proc msg obj)
      (set! proc (type-node! proc))
      (set! msg (type-node! msg))
      (set! obj (type-node! obj))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::switch)
   (with-access::switch node (type clauses test)
      (set! test (type-node! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (type-node! (cdr clause))))
		clauses)
      ;; we cannot use the type of the approximation here because the
      ;; approximated type might be more precise that the actual
      ;; code generation (for instance when some procedure-el are applied)
      (set! type (get-type node #f))
      node))

;* {*---------------------------------------------------------------------*} */
;* {*    type-node! ::switch ...                                          *} */
;* {*---------------------------------------------------------------------*} */
;* (define-method (type-node! node::switch/Cinfo)                      */
;*    (call-next-method)                                               */
;*    (unless *strict-node-type*                                       */
;*       (with-access::switch/Cinfo node (type approx)                 */
;* 	 (set! type (get-approx-type approx node))))                        */
;*    node)                                                            */

;*---------------------------------------------------------------------*/
;*    type-node! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::let-fun)
   (with-access::let-fun node (type body locals)
      (for-each type-fun! locals)
      (set! body (type-node! body))
      (set! type (node-type body))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::let-var)
   (with-access::let-var node (type body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (set-cdr! binding (type-node! val))
		      (type-variable! (local-value var) var)))
		bindings)
      (set! body (type-node! body))
      (set! type (node-type body))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (let ((v (var-variable var)))
	 (type-variable! (local-value v) v))
      (set! body (type-node! body))
      (set! var (type-node! var))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (type-node! exit) )
      (set! value (type-node! value))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::make-box)
   (with-access::make-box node (value)
      (set! value (type-node! value))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (type-node! var))
      (set! value (type-node! value))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::box-ref)
   (with-access::box-ref node (var type)
      (set! var (type-node! var))
      ;; MS: 24oct2016
      ;;(set! type (get-type var #f))
      node))

;*---------------------------------------------------------------------*/
;*    type-node*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-node*! node*)
   (map! type-node! node*))


		
	    
