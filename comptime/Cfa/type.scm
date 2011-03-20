;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/type.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 27 10:33:17 1996                          */
;*    Last change :  Sun Mar 20 06:51:52 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the obvious type election (taking care of tvectors).     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_type
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
	    tvector_tvector)
   (export  (type-settings! globals)
	    (get-approx-type ::approx)))

;*---------------------------------------------------------------------*/
;*    type-settings! ...                                               */
;*---------------------------------------------------------------------*/
(define (type-settings! globals)
   (for-each type-fun! globals))

;*---------------------------------------------------------------------*/
;*    type-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (type-fun! var::variable)
   (let ((fun (variable-value var)))
      (when (intern-sfun/Cinfo? fun)
	 ;; if it is not an `intern-sfun/Cinfo', it means that the
	 ;; procedure is unreachable and then we can ignore it.
	 (with-access::intern-sfun/Cinfo fun (body args approx)
	    ;; the formals
	    (for-each (lambda (var)
			 (type-variable! (local-value var) var))
		      args)
	    ;; the body
	    (set! body (type-node! body))
	    ;; and the function result
	    (set-variable-type! var (get-approx-type approx))))))

;*---------------------------------------------------------------------*/
;*    get-approx-type ...                                              */
;*---------------------------------------------------------------------*/
(define (get-approx-type approx)
   (let ((type       (approx-type approx))
	 (alloc-list (set->list (approx-allocs approx))))
      (cond
	 ((not (pair? alloc-list))
	  type)
	 ((not (tvector-optimization?))
	  type)
	 ((make-vector-app? (car alloc-list))
	  (let* ((app          (car alloc-list))
		 (tv-type      (get-vector-item-type app))
		 (value-approx (make-vector-app-value-approx app))
		 (item-type    (approx-type value-approx))
		 (tv           (type-tvector item-type)))
	     (if (type? tv)
		 tv
		 type)))
	 ((valloc/Cinfo+optim? (car alloc-list))
	  (let* ((app          (car alloc-list))
		 (tv-type      (get-vector-item-type app))
		 (value-approx (valloc/Cinfo+optim-value-approx app))
		 (item-type    (approx-type value-approx))
		 (tv           (type-tvector item-type)))
	     (if (type? tv)
		 tv
		 type)))
	 (else
	  type))))
	 
;*---------------------------------------------------------------------*/
;*    type-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (type-variable! value::value variable::variable)
   (let ((type (variable-type variable)))
      (if (type? type)
	  'nothing
	  (set-variable-type! variable (get-default-type)))))

;*---------------------------------------------------------------------*/
;*    type-variable! ::svar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-variable! value::svar/Cinfo variable)
   (with-access::svar/Cinfo value (approx)
      (set-variable-type! variable (get-approx-type approx))))
   
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
      (set-variable-type! variable (get-approx-type approx))))

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
   node)

;*---------------------------------------------------------------------*/
;*    type-node! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::var)
   (with-access::var node (variable type)
      (when (and (global? variable) (eq? (global-import variable) 'static))
	 (type-variable! (global-value variable) variable))
      (when (or (eq? type *_*) (eq? type *vector*))
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
   (with-access::sequence node (nodes)
      (type-node*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app)
   (with-access::app node (type fun args)
      (type-node*! args)
      node))

;*---------------------------------------------------------------------*/
;*    get-procedure-approx-type ...                                    */
;*---------------------------------------------------------------------*/
(define (get-procedure-approx-type approx)
   (cond
      ((not (approx-procedure-el? approx)) (approx-type approx))
      ((approx-procedure-el1? approx) *procedure-el1*)
      (else *procedure-el*)))
   
;*---------------------------------------------------------------------*/
;*    type-node! ::procedure-ref-app ...                               */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::procedure-ref-app)
   (with-access::procedure-ref-app node (args approx type fun)
      (call-next-method)
      (if *optim-cfa-free-var-tracking?*
	  (let ((atype (get-procedure-approx-type approx)))
	     (if (and (bigloo-type? atype)
		      (not (eq? atype *_*))
		      (not (eq? atype (get-type node))))
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
	 (let ((atype (get-procedure-approx-type vapprox)))
	    (when (and (bigloo-type? atype)
		       (not (eq? atype *_*))
		       (not (eq? atype (get-type (caddr args)))))
	       (set-car! (cddr args)
			 (instantiate::cast
			    (type *obj*)
			    (arg (caddr args)))))))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app-ly/Cinfo)
   (with-access::app-ly/Cinfo node (fun arg type approx)
      (set! fun (type-node! fun))
      (set! arg (type-node! arg))
      (if *optim-cfa-funcall-tracking?*
	  (set! type (approx-type approx))
	  (set! type *obj*))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::funcall/Cinfo)
   (with-access::funcall/Cinfo node (fun args type approx)
      (set! fun (type-node! fun))
      (type-node*! args)
      (if *optim-cfa-funcall-tracking?*
	  (begin
	     (let ((typ (approx-type approx)))
		(if (eq? typ *_*)
		    (internal-error "cfa!" "Illegal type _ for funcall" (shape node))
		    (set! type typ))))
	  (set! type *obj*))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::extern)
   (with-access::extern node (expr* type)
      (type-node*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::vref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::vref)
   (call-next-method)
   (with-access::vref node (ftype)
      (when (eq? ftype *_*)
	 (set! ftype *obj*)))
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
;*---------------------------------------------------------------------*/
(define-method (type-node! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (type-node! test))
       (set! true (type-node! true))
       (set! false (type-node! false))
       node))

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
;*    type-node! ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::select)
   (with-access::select node (clauses test)
      (set! test (type-node! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (type-node! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each type-fun! locals)
      (set! body (type-node! body))
      node))

;*---------------------------------------------------------------------*/
;*    type-node! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (set-cdr! binding (type-node! val))
		      (type-variable! (local-value var) var)))
		bindings)
      (set! body (type-node! body))
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
   (with-access::box-ref node (var)
      (set! var (type-node! var))
      node))

;*---------------------------------------------------------------------*/
;*    type-node*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-node*! node*)
   (map! type-node! node*))


		
	    
