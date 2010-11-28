;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/type.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 27 10:33:17 1996                          */
;*    Last change :  Sun Nov 28 09:37:04 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the obvious type election (taking care of tvectors).     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_type
   (import  type_type
	    type_cache
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
	    (type-node! body)
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
;*    We set the type of the variable in two different circontances:   */
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
		   (not *unsafe-type*)
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
(define-generic (type-node! node::node))

;*---------------------------------------------------------------------*/
;*    type-node! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::atom)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    type-node! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::kwote)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    type-node! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::var)
   (with-access::var node (variable type)
      (when (and (global? variable) (eq? (global-import variable) 'static))
	 (type-variable! (global-value variable) variable))
      (when (or (eq? type *_*) (eq? type *vector*))
	 (set! type (variable-type variable)))))

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
      (type-node*! nodes)))

;*---------------------------------------------------------------------*/
;*    type-node! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app)
   (with-access::app node (type fun args)
      (type-node*! args)))

;*---------------------------------------------------------------------*/
;*    type-node! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (type-node! fun)
      (type-node! arg)))

;*---------------------------------------------------------------------*/
;*    type-node! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::funcall)
   (with-access::funcall node (fun args)
      (type-node! fun)
      (type-node*! args)))

;*---------------------------------------------------------------------*/
;*    type-node! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::extern)
   (with-access::extern node (expr* type)
      (type-node*! expr*)))

;*---------------------------------------------------------------------*/
;*    type-node! ::vref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::vref)
   (call-next-method)
   (with-access::vref node (ftype)
      (if (eq? ftype *_*)
	  (set! ftype *obj*))))
      
;*---------------------------------------------------------------------*/
;*    type-node! ::vset! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::vset!)
   (call-next-method)
   (with-access::vset! node (ftype)
      (if (eq? ftype *_*)
	  (set! ftype *obj*))))
      
;*---------------------------------------------------------------------*/
;*    type-node! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::cast)
   (with-access::cast node (arg)
      (type-node! arg)))

;*---------------------------------------------------------------------*/
;*    type-node! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::setq)
   (with-access::setq node (var value)
      (type-node! value)
      (type-node! var)))

;*---------------------------------------------------------------------*/
;*    type-node! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::conditional)
   (with-access::conditional node (test true false)
       (type-node! test)
       (type-node! true)
       (type-node! false)))

;*---------------------------------------------------------------------*/
;*    type-node! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::fail)
   (with-access::fail node (type proc msg obj)
      (type-node! proc)
      (type-node! msg)
      (type-node! obj)))

;*---------------------------------------------------------------------*/
;*    type-node! ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::select)
   (with-access::select node (clauses test)
      (type-node! test)
      (for-each (lambda (clause)
		   (type-node! (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    type-node! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each type-fun! locals)
      (type-node! body)))

;*---------------------------------------------------------------------*/
;*    type-node! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (type-node! val)
		      (type-variable! (local-value var) var)))
		bindings)
      (type-node! body)))

;*---------------------------------------------------------------------*/
;*    type-node! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (let ((v (var-variable var)))
	 (type-variable! (local-value v) v))
      (type-node! body)
      (type-node! var)))

;*---------------------------------------------------------------------*/
;*    type-node! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (type-node! exit) 
      (type-node! value)))

;*---------------------------------------------------------------------*/
;*    type-node! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::make-box)
   (with-access::make-box node (value)
      (type-node! value)))

;*---------------------------------------------------------------------*/
;*    type-node! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::box-set!)
   (with-access::box-set! node (var value)
      (type-node! var)
      (type-node! value)))

;*---------------------------------------------------------------------*/
;*    type-node! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (type-node! node::box-ref)
   (with-access::box-ref node (var)
      (type-node! var)))

;*---------------------------------------------------------------------*/
;*    type-node*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-node*! node*)
   (for-each type-node! node*))


		
	    
