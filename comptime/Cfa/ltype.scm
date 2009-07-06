;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/ltype.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 27 10:33:17 1996                          */
;*    Last change :  Thu Sep 25 20:39:30 2003 (serrano)                */
;*    Copyright   :  1996-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the obvious type election (taking care of tvectors).     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_ltype
   (import  type_type
	    type_cache
	    tools_shape
	    tools_error
	    tools_shape
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
	    cnst_node)
   (export  (light-type! globals)))

;*---------------------------------------------------------------------*/
;*    light-type! ...                                                  */
;*---------------------------------------------------------------------*/
(define (light-type! globals)
   (for-each light-type-fun! globals))

;*---------------------------------------------------------------------*/
;*    light-type-fun! ...                                              */
;*---------------------------------------------------------------------*/
(define (light-type-fun! var::variable)
   (let ((fun (variable-value var)))
      (if (intern-sfun/Cinfo? fun)
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
   (if (approx-procedure-el? approx)
       (if (approx-procedure-el1? approx)
	   *procedure-el1*
	   *procedure-el*)
       *_*))

;*---------------------------------------------------------------------*/
;*    type-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (type-variable! value::value variable::variable)
   (let ((type (variable-type variable)))
      (if (not (eq? type *procedure*))
	  'nothing
	  (set-variable-type! variable type))))

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
;*---------------------------------------------------------------------*/
(define (set-variable-type! variable::variable type::type)
   (if (not (eq? type *_*))
       ;; we have a true type so we change the variable
       (variable-type-set! variable type)))

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
      (if (and (global? variable) (eq? (global-import variable) 'static))
	  (type-variable! (global-value variable) variable))))

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
      (for-each light-type-fun! locals)
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


		
	    
