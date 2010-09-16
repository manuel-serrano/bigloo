;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/hrtype.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  3 11:58:06 1996                          */
;*    Last change :  Wed Sep  8 08:20:05 2010 (serrano)                */
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
	    ast_node)
   (export  (generic hrtype-node! ::node)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (hrtype-node! node::node))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::atom ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::atom)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::kwote)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::var)
   (with-access::var node (type variable)
      ;; MS 15 apr 2010. It might be that a body contains a reference
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
		 (global-bucket-position (global-id variable)
					 (global-module variable)))
	 (let ((n (find-global/module (global-id variable)
				      (global-module variable))))
	    (when (global? n)
	       (set! variable n))))
      (if (type? type)
	  (set! type (find-type (type-id type)))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::closure ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::closure)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::sequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::sequence)
   (with-access::sequence node (nodes)
      (hrtype-node*! nodes)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::app)
   (with-access::app node (args fun type loc)
      (when (type? type)
	 (set! type (find-type (type-id type))))
      (with-access::var fun (variable)
	 (when (global? variable)
	    (let ((value (variable-value variable)))
	       (cond
		  ((cfun? value)
		   (restore-global! variable))
		  ((sfun? value)
		   (if (and (eq? (sfun-class value) 'sifun)
			    ;; @label already-restored@
			    (not (already-restored? variable)))
		       ;; because of possible recursive inlined functions
		       ;; we have to prevent the compiler from looping
		       ;; when clearing inlined called functions
		       (restore-global! variable)
		       ;; if we are calling a global variable from a
		       ;; library inlined function, we have to substitute
		       ;; the reference to the body with the reference of
		       ;; the variable that is in the current Bigloo
		       ;; environment.
		       (let ((g (find-global (global-id variable)
					     (global-module variable))))
			  (if (not (variable? g))
			      (let ((new-g (find-global (global-id variable)
							*module*)))
				 (if (not (and (global? new-g)
					       (eq? (global-import new-g) 'static)))
				     (error "heap"
					    "Can't find library variable"
					    (global-id variable))
				     (set! variable new-g)))
			      (set! variable g)))))))))
      (hrtype-node*! args)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (hrtype-node! fun)
      (hrtype-node! arg)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::funcall)
   (with-access::funcall node (fun args)
      (hrtype-node! fun)
      (hrtype-node*! args)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::extern ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::extern)
   (with-access::extern node (expr* type)
      (if (type? type)
	  (set! type (find-type (type-id type))))
      (hrtype-node*! expr*)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::cast)
   (with-access::cast node (arg type)
      (hrtype-node! arg)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::setq)
   (with-access::setq node (var value)
      (hrtype-node! value)
      (hrtype-node! var)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::conditional)
   (with-access::conditional node (test true false)
       (hrtype-node! test)
       (hrtype-node! true)
       (hrtype-node! false)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::fail)
   (with-access::fail node (type proc msg obj)
      (hrtype-node! proc)
      (hrtype-node! msg)
      (hrtype-node! obj)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::select ...                                        */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::select)
   (with-access::select node (clauses test)
      (hrtype-node! test)
      (for-each (lambda (clause)
		   (hrtype-node! (cdr clause)))
		clauses)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::let-fun)
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
		      (hrtype-node! (sfun-body sfun))))
		locals)
      (hrtype-node! body)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (hrtype-node! val)
		      (restore-variable-type! var)))
		bindings)
      (hrtype-node! body)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (restore-variable-type! (var-variable var))
      (hrtype-node! body)
      (hrtype-node! var)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (hrtype-node! exit) 
      (hrtype-node! value)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::make-box)
   (with-access::make-box node (value)
      (hrtype-node! value)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::box-set!)
   (with-access::box-set! node (var value)
      (hrtype-node! var)
      (hrtype-node! value)))

;*---------------------------------------------------------------------*/
;*    hrtype-node! ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (hrtype-node! node::box-ref)
   (with-access::box-ref node (var)
      (hrtype-node! var)))

;*---------------------------------------------------------------------*/
;*    hrtype-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (hrtype-node*! node*)
   (for-each hrtype-node! node*))
   
;*---------------------------------------------------------------------*/
;*    restore-variable-type! ...                                       */
;*---------------------------------------------------------------------*/
(define (restore-variable-type! variable::variable)
   (let ((type (variable-type variable)))
      (if (type? type)
	  (variable-type-set! variable (find-type (type-id type))))))

