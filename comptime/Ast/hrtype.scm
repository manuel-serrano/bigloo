;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Ast/hrtype.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  3 11:58:06 1996                          */
;*    Last change :  Mon Oct 20 11:01:20 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This function hrtype-node! is used for inlined functions         */
;*    that are restored from additional heap. These bodies still       */
;*    contain references to their old definition types. New pointers   */
;*    have to be restored.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_hrtype
   (import  type_type
	    type_env
	    tools_shape
	    tools_error
	    module_module
	    ast_var
	    ast_env
	    ast_node
	    ast_dump)
   (export  (generic hrtype-node! ::node ::obj)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (hrtype-node! node::node env)
   (with-access::node node (type)
      (when (type? type)
	 (set! type (find-type (type-id type))))))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::var env)
   (with-access::var node (variable)
      ;; MS 15 apr 2010. It might be that a body contain a reference
      ;; to an unbound global variable. For instance, the camloo raise
      ;; function is defined as:
      ;; 
      ;;    (define-inline (raise value)
      ;;       (set! *try* 1)
      ;;       (if (eq? current_handler #f)
      ;;           (error "Fatal error" "uncaught exception." value)
      ;;           (begin
      ;;              (invoke-continuation value)
      ;;              (unspecified))))
      ;;
      ;; when used in client code, this function will be inlined.
      ;; It turns out that (unspecified) is also an inline function, defined
      ;; in the standard library, defined as:
      ;;
      ;;    (define-inline (unspecified) __unspec__)
      ;;
      ;; Hence, inlining raised in a client code introduces a
      ;; reference to the version of __unspec__ that was used when generating
      ;; the camloo heap, which is not the same one as the one used when
      ;; compiling the client code. This global has to be "rebound", this is
      ;; the purpose of this patch
      (when (and (global? variable)
		 (global-bucket-position env
		    (global-id variable)
		    (global-module variable)))
	 (let ((n (find-global/module env
		     (global-id variable)
		     (global-module variable))))
	    (when (global? n)
	       (set! variable n)))))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::sequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::sequence env)
   (with-access::sequence node (nodes)
      (hrtype-node*! nodes env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::sync ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::sync env)
   (with-access::sync node (mutex prelock body)
      (hrtype-node! mutex env)
      (hrtype-node! prelock env)
      (hrtype-node! body env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::app env)
   (with-access::app node (args fun type loc)
      (with-access::var fun (variable)
	 (when (global? variable)
	    (let ((value (variable-value variable)))
	       (cond
		  ((cfun? value)
		   (let ((g (find-global env (global-id variable)
			       (global-module variable))))
		      ;; MS 8dec2020: This correct a bug observed in
		      ;; Hop hopscript library. An inlined hopscript function
		      ;; was using $VECTOR?. The type of the parameter not
		      ;; not properly restored and left as "_". This was
		      ;; responsible for coerce to fail to convert something
		      ;; into a "_" type. As foreign variable are supposed
		      ;; to be globally unique, I think it's safer to
		      ;; changer the restored foreign for the one pre-existing.
		      (if g
			  (set! variable g)
			  (restore-global! variable env))))
		  ((sfun? value)
		   (if (and (eq? (sfun-class value) 'sifun)
			    ;; @label already-restored@
			    (not (already-restored? variable)))
		       ;; because of possible recursive inlined functions
		       ;; we have to prevent the compiler from looping
		       ;; when clearing inlined called functions
		       (restore-global! variable env)
		       ;; if we are calling a global variable from a
		       ;; library inlined function, we have to substitute
		       ;; the reference to the body with the reference of
		       ;; the variable that is in the current Bigloo
		       ;; environment.
		       (let ((g (find-global env
				   (global-id variable)
				   (global-module variable))))
			  (if (not (variable? g))
			      (let ((new-g (find-global env
					      (global-id variable)
					      *module*)))
				 (if (not (and (global? new-g)
					       (eq? (global-import new-g) 'static)))
				     (error "heap"
					"Can't find library variable"
					(global-id variable))
				     (set! variable new-g)))
			      (set! variable g)))))))))
      (hrtype-node*! args env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::app-ly env)
   (with-access::app-ly node (fun arg)
      (hrtype-node! fun env)
      (hrtype-node! arg env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::funcall env)
   (with-access::funcall node (fun args)
      (hrtype-node! fun env)
      (hrtype-node*! args env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::extern ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::extern env)
   (with-access::extern node (expr*)
      (hrtype-node*! expr* env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::getfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::getfield env)
   (with-access::getfield node (ftype otype)
      (set! ftype (find-type (type-id ftype)))
      (set! otype (find-type (type-id otype)))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::setfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::setfield env)
   (with-access::setfield node (ftype otype)
      (set! ftype (find-type (type-id ftype)))
      (set! otype (find-type (type-id otype)))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::widening ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::widening env)
   (with-access::widening node (otype)
      (set! otype (find-type (type-id otype)))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::new ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::new env)
   (with-access::new node (args-type)
      (set! args-type (map! (lambda (t) (find-type (type-id t))) args-type))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::valloc ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::valloc env)
   (with-access::valloc node (ftype otype)
      (set! ftype (find-type (type-id ftype)))
      (set! otype (find-type (type-id otype)))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::vref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::vref env)
   (with-access::vref node (ftype otype vtype)
      (set! ftype (find-type (type-id ftype)))
      (set! otype (find-type (type-id otype)))
      (set! vtype (find-type (type-id vtype)))
      (call-next-method)))
      
;*---------------------------------------------------------------------*/
;*    hrtype-node! ::vset! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::vset! env)
   (with-access::vset! node (ftype otype vtype)
      (set! ftype (find-type (type-id ftype)))
      (set! otype (find-type (type-id otype)))
      (set! vtype (find-type (type-id vtype)))
      (call-next-method)))
      
;*---------------------------------------------------------------------*/
;*    hrtype-node! ::vlength ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::vlength env)
   (with-access::vlength node (ftype otype vtype)
      (set! vtype (find-type (type-id vtype)))
      (call-next-method)))
      
;*---------------------------------------------------------------------*/
;*    hrtype-node! ::instanceof ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::instanceof env)
   (with-access::instanceof node (class)
      (set! class (find-type (type-id class))))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::cast env)
   (with-access::cast node (arg type)
      (hrtype-node! arg env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::setq env)
   (with-access::setq node (var value)
      (hrtype-node! value env)
      (hrtype-node! var env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::conditional env)
   (with-access::conditional node (test true false)
       (hrtype-node! test env)
       (hrtype-node! true env)
       (hrtype-node! false env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::fail env)
   (with-access::fail node (type proc msg obj)
      (hrtype-node! proc env)
      (hrtype-node! msg env)
      (hrtype-node! obj env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::switch ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::switch env)
   (with-access::switch node (clauses test item-type)
      (set! item-type (find-type (type-id item-type)))
      (hrtype-node! test env)
      (for-each (lambda (clause)
		   (hrtype-node! (cdr clause) env))
		clauses))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::let-fun env)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((sfun (local-value local)))
		      (let loop ((args (sfun-args sfun)))
			 (if (pair? args)
			     (let ((arg (car args)))
				(cond
				   ((type? arg)
				    (set-car! args (find-type (type-id arg))))
				   ((local? arg)
				    (restore-variable-type! arg))
				   (else
				    (error "hrtype-node!"
					   "Illegal argument"
					   (shape arg))))
				(loop (cdr args)))))
		      (restore-variable-type! local)
		      (hrtype-node! (sfun-body sfun) env)))
		locals)
      (hrtype-node! body env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::let-var env)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (hrtype-node! val env)
		      (restore-variable-type! var)))
		bindings)
      (hrtype-node! body env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::set-ex-it env)
   (with-access::set-ex-it node (var body onexit)
      (restore-variable-type! (var-variable var))
      (hrtype-node! body env)
      (hrtype-node! onexit env)
      (hrtype-node! var env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::jump-ex-it env)
   (with-access::jump-ex-it node (exit value)
      (hrtype-node! exit env) 
      (hrtype-node! value env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::make-box env)
   (with-access::make-box node (value)
      (hrtype-node! value env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::box-set! env)
   (with-access::box-set! node (var value)
      (hrtype-node! var env)
      (hrtype-node! value env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::box-ref env)
   (with-access::box-ref node (var)
      (hrtype-node! var env))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    hrtype-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (hrtype-node*! node* env)
   (for-each (lambda (n) (hrtype-node! n env)) node*))
   
;*---------------------------------------------------------------------*/
;*    restore-variable-type! ...                                       */
;*---------------------------------------------------------------------*/
(define (restore-variable-type! variable::variable)
   (let ((ty (variable-type variable)))
      (when (type? ty)
	 (variable-type-set! variable (find-type (type-id ty))))))

