;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Cfa/setup.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 14:08:53 1996                          */
;*    Last change :  Wed Mar 14 19:22:42 2018 (serrano)                */
;*    Copyright   :  1996-2018 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We setup the ast for the Cfa.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_setup
   (include "Tools/trace.sch")
   (import  engine_param
	    type_type
	    type_cache
	    type_typeof
	    module_module
	    tools_shape
	    tools_error
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_approx
	    cfa_pair)
   (export  (set-initial-approx! globals)
	    (generic node-setup! ::node)
	    (node-setup*! node*)))

;*---------------------------------------------------------------------*/
;*    set-initial-approx! ...                                          */
;*---------------------------------------------------------------------*/
(define (set-initial-approx! globals)
   (trace cfa "================== initial ===========================\n")   
   (for-each (lambda (global)
		(trace (cfa 5) "set-initial-approx!: " (shape global)
		       #\Newline)
		(let ((fun (global-value global)))
		   (fun-setup! fun global)
		   (for-each (lambda (local)
				(widen!::reshaped-local local)
				(variable-value-setup! (local-value local)
						       local))
			     (sfun-args fun)))
		(node-setup! (sfun-body (global-value global))))
	     globals))

;*---------------------------------------------------------------------*/
;*    node-setup! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (node-setup! node::node))

;*---------------------------------------------------------------------*/
;*    node-setup! ::literal ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::literal)
   (with-access::literal node (value)
      (widen!::literal/Cinfo node
	 (approx (make-type-approx (get-type-atom value))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::patch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::patch)
   (with-access::patch node (value type)
      (node-setup! value)
      (widen!::patch/Cinfo node
	 (approx (make-type-approx type)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::kwote ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::kwote)
   (with-access::kwote node (value)
      (let ((approx (make-type-approx (get-type-kwote value))))
	 (approx-top?-set! approx #t)
	 (widen!::kwote/Cinfo node
	    (approx approx)))))
   
;*---------------------------------------------------------------------*/
;*    node-setup! ::kwote/node ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-setup! kwote::kwote/node)
   (with-access::kwote/node kwote (node value)
      (node-setup! node)))
   
;*---------------------------------------------------------------------*/
;*    node-setup! ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::var) 
   (with-access::var node (variable)
      (variable-value-setup! (variable-value variable) variable)
      ;; this widen! is only for the nice pretting of cfa_show
      (cond
	 ((and (local? variable) (not (reshaped-local? variable)))
	  (widen!::reshaped-local variable))
	 ((and (global? variable) (not (reshaped-global? variable)))
	  (widen!::reshaped-global variable)))))

;*---------------------------------------------------------------------*/
;*    alloc-type? ...                                                  */
;*    -------------------------------------------------------------    */
;*    This predicate returns #t for all types denoting data            */
;*    structures approximated by the cfa.                              */
;*---------------------------------------------------------------------*/
(define (alloc-type? type)
   (cond
      ((eq? type *vector*) #t)
      ((eq? type *procedure*) #t)
      ((eq? type *struct*) #t)
      ((eq? type *pair*) (pair-optim?))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (variable-value-setup! value::value variable::variable))

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::sfun ...                                 */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::sfun var)
   ;; we reach this method when setting up a `make-procedure' call
   ;; on the second argument of this call.
   #unspecified)

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::svar ...                                 */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::svar var::variable)
   (let ((typ (variable-type var)))
      (if (global? var)
	  (let ((value (widen!::svar/Cinfo value
			  (approx (make-type-approx typ)))))
	     (if (and (not (eq? (global-import var) 'static)) (alloc-type? typ))
		 (approx-set-top! (svar/Cinfo-approx value))))
	  (widen!::svar/Cinfo value
	     (approx (make-type-approx typ))))))

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::pre-clo-env ...                          */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::pre-clo-env var)
   (trace (cfa 5) "Je set un pre-clo-env..." (shape var) #\Newline)
   (call-next-method)
   (svar/Cinfo-clo-env?-set! (local-value var) #t))

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::sexit ...                                */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::sexit var)
   (widen!::sexit/Cinfo value
      (approx (make-type-approx (variable-type var)))))

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::scnst ...                                */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::scnst/Cinfo var)
   'already-done)
   
;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::scnst ...                                */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::scnst var)
   (trace (cfa 5) "Je setup une scnst: " (shape var) " "
	  (shape (scnst-node value)) #\Newline)
   (if (global? var)
       (if (and (eq? (global-module var) *module*)
		(memq (scnst-class value) '(sfun sgfun))
		(pre-make-procedure-app? (scnst-node value)))
	   ;; this variable holds a closure
	   (let ((node (scnst-node value)))
	      (trace (cfa 5) "    et en plus, c'est une closure" #\Newline)
	      (node-setup! node)
	      (widen!::scnst/Cinfo value
		 (approx (make-procedure-app-approx node))))
	   (let ((value (widen!::scnst/Cinfo value
			   (approx (make-type-approx (variable-type var))))))
	      (approx-set-top! (scnst/Cinfo-approx value))))
       (widen!::scnst/Cinfo value
	  (approx (make-type-approx (variable-type var))))))

;*---------------------------------------------------------------------*/
;*    variable-value-setup! ::cvar ...                                 */
;*---------------------------------------------------------------------*/
(define-method (variable-value-setup! value::cvar var)
   (widen!::cvar/Cinfo value
      (approx (make-type-approx (variable-type var))))
   (when (alloc-type? (variable-type var))
      (approx-set-top! (cvar/Cinfo-approx value))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::closure ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::closure)
   (internal-error "node-setup!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    node-setup! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::sequence)
   (with-access::sequence node (nodes)
      (node-setup*! nodes)))

;*---------------------------------------------------------------------*/
;*    node-setup! ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::sync)
   (with-access::sync node (body mutex prelock)
      (node-setup! mutex)
      (node-setup! prelock)
      (node-setup! body)))

;*---------------------------------------------------------------------*/
;*    node-setup! ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::app)
   (trace (cfa 5) "Je setup une app: " (shape node) #\Newline)
   (with-access::app node (fun args)
      (node-setup*! args)
      (let ((variable (var-variable fun)))
	 (fun-setup! (variable-value variable) variable))))

;*---------------------------------------------------------------------*/
;*    fun-setup! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (fun-setup! fun::fun var)
   (if (and (global? var) (not (reshaped-global? var)))
       (widen!::reshaped-global var))
   #unspecified) 

;*---------------------------------------------------------------------*/
;*    fun-setup! ::sfun ...                                            */
;*---------------------------------------------------------------------*/
(define-method (fun-setup! fun::sfun var)
   (if (and (global? var) (not (reshaped-global? var)))
       (widen!::reshaped-global var))
   (if (and (global? var) (eq? (global-import var) 'import))
       (let ((approx (make-type-approx (global-type var))))
	  (if (or #t (sfun-top? fun)) (approx-set-top! approx))
	  (widen!::extern-sfun/Cinfo fun
	     (approx approx)))
       (let ((approx (make-type-approx (variable-type var))))
	  (when *optim-cfa-force-loose-local-function?*
	     (approx-set-top! approx))
	  (widen!::intern-sfun/Cinfo fun
	     (approx approx)))))

;*---------------------------------------------------------------------*/
;*    fun-setup! ::cfun ...                                            */
;*---------------------------------------------------------------------*/
(define-method (fun-setup! fun::cfun var)
   (if (not (reshaped-global? var))
       (widen!::reshaped-global var))
   (let ((approx (make-type-approx (global-type var))))
      (if (or #t (cfun-top? fun)) (approx-set-top! approx))
      (widen!::cfun/Cinfo fun
	 (approx approx))))
 
;*---------------------------------------------------------------------*/
;*    node-setup! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::app-ly)
   (with-access::app-ly node (fun arg)
      (node-setup! fun)
      (node-setup! arg)
      (if *optim-cfa-apply-tracking?*
	  (widen!::app-ly/Cinfo node
	     (approx (make-empty-approx)))
	  (begin
	     (widen!::app-ly/Cinfo node
		(approx (make-type-approx *obj*)))
	     (approx-set-top! (app-ly/Cinfo-approx node))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::funcall)
   (with-access::funcall node (fun args)
      (node-setup! fun)
      (node-setup*! args)
      (if *optim-cfa-unbox-closure-args*
	  (widen!::funcall/Cinfo node
	     (approx (make-empty-approx))
	     (va-approx (make-empty-approx)))
	  (begin
	     (widen!::funcall/Cinfo node
		(approx (make-type-approx *obj*))
		(va-approx (make-type-approx *obj*)))
	     (approx-set-top! (funcall/Cinfo-va-approx node))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pragma ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pragma)
   (with-access::pragma node (expr* type)
      (node-setup*! expr*)
      (widen!::pragma/Cinfo node
	 (approx (make-type-approx type)))
      (approx-set-top! (pragma/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::genpatchid ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::genpatchid)
   (with-access::genpatchid node (expr*)
      (node-setup*! expr*)
      (widen!::genpatchid/Cinfo node 
	 (approx (make-type-approx *long*)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::getfield ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::getfield)
   (with-access::getfield node (expr* type)
      (node-setup*! expr*)
      (widen!::getfield/Cinfo node
	 (approx (make-type-approx type)))
      (approx-set-top! (getfield/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::setfield ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::setfield)
   (with-access::setfield node (expr* type)
      (node-setup*! expr*)
      (widen!::setfield/Cinfo node
	 (approx (make-type-approx type)))
      (approx-set-top! (setfield/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::new ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::new)
   (with-access::new node (expr* type)
      (node-setup*! expr*)
      (widen!::new/Cinfo node
	 (approx (make-type-approx type)))
      (approx-set-top! (new/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::instanceof ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::instanceof)
   (with-access::instanceof node (type expr*)
      (node-setup*! expr*)
      (widen!::instanceof/Cinfo node
	 (approx (make-type-approx type)))
      (approx-set-top! (instanceof/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::cast-null ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::cast-null)
   (with-access::cast-null node (type)
      (widen!::cast-null/Cinfo node
	 (approx (make-type-approx type)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::cast)
   (with-access::cast node (arg)
      (node-setup! arg)))

;*---------------------------------------------------------------------*/
;*    node-setup! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::setq)
   (trace (cfa 5) "Je setup un setq: " (shape node) #\Newline)
   (with-access::setq node (var value)
      (node-setup! value)
      (node-setup! var)
      (widen!::setq/Cinfo node
	 (approx (make-type-approx *unspec*)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::conditional)
   (with-access::conditional node (test true false)
       (node-setup! test)
       (node-setup! true)
       (node-setup! false)
       (widen!::conditional/Cinfo node
	  (approx (make-empty-approx)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::fail)
   (with-access::fail node (type proc msg obj)
      (node-setup! proc)
      (node-setup! msg)
      (node-setup! obj)
      (widen!::fail/Cinfo node
	 (approx (make-type-approx *obj*)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::switch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::switch)
   (with-access::switch node (clauses test)
      (node-setup! test)
      (for-each (lambda (clause)
		   (node-setup! (cdr clause)))
		clauses)
      (widen!::switch/Cinfo node
	 (approx (make-empty-approx)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (l)
		   (widen!::reshaped-local l)
		   (let ((fun (local-value l)))
		      (for-each (lambda (local)
				   (widen!::reshaped-local local)
				   (variable-value-setup! (local-value local)
							  local))
				(sfun-args fun))
		      (node-setup! (sfun-body fun))))
		locals)
      (node-setup! body)))

;*---------------------------------------------------------------------*/
;*    node-setup! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::let-var)
   (with-access::let-var node (body bindings)
      (trace (cfa 5) "let-var setup: " (shape node) #\Newline)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (variable-value-setup! (local-value var) var)
		      ;; if the variable is read-only we set it a binding
		      ;; value to improve the approximations which require
		      ;; offset (such as make-procedure, procedure-ref)
		      (node-setup! val)
		      (widen!::reshaped-local var
			 (binding-value (if (eq? (local-access var) 'read)
					    val
					    #f)))
		      (trace (cfa 5) "~~~ let-var setup " (shape val)
			     " " (find-runtime-type val)
			     #\Newline)
		      (trace (cfa 5) "  " (shape var) " ... " #\Newline)))
		bindings)
      (trace (cfa 5) "let-var body: " (shape body) #\Newline)
      (node-setup! body)
      (trace (cfa 5) "<<< let-var setup...\n")))
 
;*---------------------------------------------------------------------*/
;*    node-setup! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (node-setup! body)
      (node-setup! var)
      (widen!::reshaped-local (var-variable var))
      (widen!::set-ex-it/Cinfo node
	 (approx (make-type-approx *obj*)))
      (approx-set-top! (set-ex-it/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (node-setup! exit) 
      (node-setup! value)
      (widen!::jump-ex-it/Cinfo node
	 (approx (make-type-approx *obj*)))
      (approx-set-top! (jump-ex-it/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (node-setup*! node*)
   (for-each node-setup! node*))


