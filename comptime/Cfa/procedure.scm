;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/procedure.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 12:08:59 1996                          */
;*    Last change :  Sat Aug 25 07:16:06 2012 (serrano)                */
;*    Copyright   :  1996-2012 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The procedure approximation management                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_procedure
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_error
	    tools_shape
	    type_type
	    type_cache
	    ast_var 
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_loose
	    cfa_setup
	    cfa_approx
	    cfa_cfa
	    cfa_iterate
	    cfa_closure)
   (export  (disable-X-T! approx::approx ::bstring)
	    (set-procedure-approx-polymorphic! ::make-procedure-app)))

;*---------------------------------------------------------------------*/
;*    disable-X-T! ...                                                 */
;*---------------------------------------------------------------------*/
(define (disable-X-T! approx reason)
   
   (define (disable-alloc! app)
      (when (make-procedure-app? app)
	 (trace (cfa 3)
	    "!!! disable X-T: " (shape app)
	    " because " reason
	    #\Newline)
	 (with-access::make-procedure-app app (X-T?)
	    (set! X-T? #f))
	 (set-procedure-approx-polymorphic! app)
	 (when *optim-cfa-unbox-closure-args*
	    (let* ((callee (car (make-procedure-app-args app)))
		   (v (var-variable callee))
		   (fun (variable-value v)))
	       (for-each loose-arg! (cdr (sfun-args fun)))))))
   
   (for-each-approx-alloc disable-alloc! approx))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-make-procedure-app ...                         */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-make-procedure-app)
   (define (make-obj-approx-vector len)
      (let ((res (make-vector len)))
	 (let loop ((i 0))
	    (if (=fx i len)
		res
		(begin
		   (vector-set! res i (make-type-approx *obj*))
		   (loop (+fx i 1)))))))
   (define (make-empty-approx-vector len)
      (let ((res (make-vector len)))
	 (let loop ((i 0))
	    (if (=fx i len)
		res
		(begin
		   (vector-set! res i (make-empty-approx))
		   (loop (+fx i 1)))))))
   (with-access::pre-make-procedure-app node (fun args)
      (add-make-procedure! node)
      (node-setup*! args)
      (let* ((owner (pre-make-procedure-app-owner node))
	     (node  (shrink! node)))
	 (let ((proc-size (get-node-atom-value (caddr args)))
	       (proc      (car args)))
	    (if (and (fixnum? proc-size)
		     (var? proc)
		     (fun? (variable-value (var-variable proc))))
		(let* ((vapprox (if *optim-cfa-free-var-tracking?*
				    (make-empty-approx-vector proc-size)
				    (make-obj-approx-vector proc-size)))
		       (node (widen!::make-procedure-app node
				(owner owner)
				(approx (make-empty-approx))
				(values-approx vapprox))))
		   (trace (cfa 5)
			  " make-procedure-app: " (shape node) #\Newline
			  "          proc-size: " proc-size #\Newline)
		   (make-procedure-app-approx-set!
		    node
		    (make-type-alloc-approx *procedure* node))
		   ;; we mark that first argument (or we set) is
		   ;; bound to a closure
		   (let* ((clo (car (sfun-args
				     (variable-value
				      (var-variable proc)))))
			  (vclo (local-value clo)))
		      (if (svar/Cinfo? vclo)
			  (begin
			     (trace (cfa 5) "Je set un pre-clo-env..."
				    (shape clo) #\Newline)
			     (svar/Cinfo-clo-env?-set! vclo #t))
			  (widen!::pre-clo-env vclo))))
		;; if the size is not a fixnum, we treat this make-procedure
		;; as a regular function call
		(call-next-method))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-procedure-ref-app ...                          */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-procedure-ref-app)
   (add-procedure-ref! node)
   (with-access::pre-procedure-ref-app node (fun args)
      (node-setup*! args)
      (let ((node (shrink! node)))
	 (widen!::procedure-ref-app node
	    (approx (if *optim-cfa-free-var-tracking?*
			(make-empty-approx)
			(make-type-approx *obj*)))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-procedure-set!-app ...                         */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-procedure-set!-app)
   (add-procedure-ref! node)
   (with-access::pre-procedure-set!-app node (fun args)
      (node-setup*! args)
      (let ((node (shrink! node)))
	 (widen!::procedure-set!-app node
	    (vapprox (make-empty-approx))
	    (approx (make-type-approx *unspec*))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::make-procedure-app ...                                    */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::make-procedure-app)
   (with-access::make-procedure-app node (approx args)
      ;; the first argument of the procedure is the
      ;; procedure itself, we set this manually
      (let* ((proc (car args))
	     (fun  (variable-value (var-variable proc)))
	     (env  (car (sfun-args fun))))
	 (trace (cfa 4)
		"make-procedure-app(je set l'env): (union "
		(shape (svar/Cinfo-approx (local-value env)))
		" "
		(shape approx)
		#\Newline)
	 (union-approx! (svar/Cinfo-approx (local-value env)) approx))
      ;; and we process the argument of the make-procedure
      (for-each cfa! args)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::procedure-ref-app ...                                     */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::procedure-ref-app)
   (with-access::procedure-ref-app node (args approx)
      
      (let ((proc-approx (cfa! (car args)))
	    (offset      (get-node-atom-value (cadr args))))
	 (cfa! (cadr args))
	 (trace (cfa 4) " procedure-ref: " (shape node) #\Newline
		"        offset: " offset #\Newline
		"        approx: " (shape approx) #\Newline)
	 ;; we check for top
	 (when (approx-top? proc-approx)
	    (approx-set-top! approx))
	 (if (fixnum? offset)
	     ;; if the offset is a fixnum, we compute an accurate approx
	     (for-each-approx-alloc
	      (lambda (app)
		 (if (and (make-procedure-app? app)
			  (<fx offset
			       (vector-length
				(make-procedure-app-values-approx app))))
		     (union-approx! approx
				    (vector-ref 
				     (make-procedure-app-values-approx app)
				     offset))
		     ;; We are out the procedure. its an error (or a
		     ;; reference to a generic function).
		     (approx-set-top! approx)))
	      proc-approx)
	     ;; is the offset is not a fixnum, we compute a merging approx
	     (for-each-approx-alloc
	      (lambda (app)
		 (if (make-procedure-app? app)
		     (let ((len (vector-length
				 (make-procedure-app-values-approx app))))
			(let loop ((i 0))
			   (if (<fx i len)
			       (begin
				  (union-approx!
				   approx
				   (vector-ref 
				    (make-procedure-app-values-approx app)
				    i))
				  (loop (+fx i 1))))))))
	      proc-approx))
	 (trace (cfa 4) "  <- " (shape approx) #\Newline)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::procedure-set!-app ...                                    */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::procedure-set!-app)
   (with-access::procedure-set!-app node (args approx vapprox)
      (let ((proc-approx (cfa! (car args)))
	    (offset      (get-node-atom-value (cadr args))))
	 (trace (cfa 4) " procedure-set!: " (shape node) #\Newline
		"         offset: " offset #\Newline
		"         val-ap: " (shape vapprox) #\Newline)
	 (cfa! (cadr args))
	 (union-approx! vapprox (cfa! (caddr args)))
	 ;; do we have top in the proc approximation ?
	 (if (approx-top? proc-approx)
	     ;; yes, we have, hence we loose every thing.
	     (loose! vapprox 'all)
	     (if (fixnum? offset)
		 ;; if the offset is a fixnum, we compute an accurate approx
		 (for-each-approx-alloc
		  (lambda (app)
		     (if (and (make-procedure-app? app)
			      (<fx offset
				   (vector-length
				    (make-procedure-app-values-approx app))))
			 (begin
			    (trace (cfa 5) "         app: "
				   (shape app)
				   " vapprox[" offset "]: "
				   (shape
				    (vector-ref
				     (make-procedure-app-values-approx app)
				     offset))
				   #\Newline)
			    (union-approx! (vector-ref
					    (make-procedure-app-values-approx app)
					    offset)
					   vapprox))
			 (approx-set-top! vapprox)))
		  proc-approx)
		 ;; if the offset is not a fixnum, we compute a merging approx
		 (for-each-approx-alloc
		  (lambda (app)
		     (if (make-procedure-app? app)
			 (let ((len (vector-length
				     (make-procedure-app-values-approx app))))
			    (let loop ((i 0))
			       (if (<fx i len)
				   (begin
				      (union-approx!
				       (vector-ref 
					(make-procedure-app-values-approx
					 app)
					i)
				       vapprox)
				      (loop (+fx i 1))))))
			 (approx-set-top! vapprox)))
		  proc-approx)))
	 approx)))

;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a procedure only means that the result of the procedure  */
;*    is lost and all formals can be bound to top. It does not mean    */
;*    anything about the values closed by the procedure (contrary to   */
;*    vectors).                                                        */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::make-procedure-app)
   (with-access::make-procedure-app alloc (lost-stamp)
      (unless (=fx lost-stamp *cfa-stamp*)
	 (trace (cfa 2) "     loose-alloc::make-procedure-app: " (shape alloc) #\Newline)
	 (set! lost-stamp *cfa-stamp*)
	 (set-procedure-approx-polymorphic! alloc)
	 (let* ((callee (car (make-procedure-app-args alloc)))
		(v (var-variable callee))
		(fun (variable-value v)))
	    (cfa-export-var! fun v)
	    (when *optim-cfa-unbox-closure-args*
	       (for-each loose-arg! (cdr (sfun-args fun))))))))

;*---------------------------------------------------------------------*/
;*    set-procedure-approx-polymorphic! ...                            */
;*    -------------------------------------------------------------    */
;*    When on a funcall not all the closures return the same type,     */
;*    all the closures have to use a polymorphic representation of     */
;*    their values, that is a Bigloo type. This function is in charge  */
;*    of setting a Bigloo type for all the closures potentially        */
;*    called on a funcall site.                                        */
;*---------------------------------------------------------------------*/
(define (set-procedure-approx-polymorphic! app::make-procedure-app)
   (let* ((callee (car (make-procedure-app-args app)))
	  (v (var-variable callee))
	  (fun (variable-value v)))
      (trace (cfa 2) "     set-polymorphic!: " (shape v) " " (typeof fun) #\Newline)
      (cond
	 ((intern-sfun/Cinfo? fun)
	  (unless (intern-sfun/Cinfo-polymorphic? fun)
	     (continue-cfa! "polymorphic intern fun")
	     (intern-sfun/Cinfo-polymorphic?-set! fun #t)))
	 ((extern-sfun/Cinfo-approx fun)
	  (unless (extern-sfun/Cinfo-polymorphic? fun)
	     (continue-cfa! "polymorphic extern fun")
	     (extern-sfun/Cinfo-polymorphic?-set! fun #t))))))

;*---------------------------------------------------------------------*/
;*    loose-arg! ...                                                   */
;*---------------------------------------------------------------------*/
(define (loose-arg! a)
   (local-type-set! a *obj*)
   (when (reshaped-local? a)
      (let ((value (variable-value a)))
	 (if (svar/Cinfo? value)
	     (with-access::approx (svar/Cinfo-approx value) (type)
		(set! type *obj*))
	     (tprint "INTERNAL CFA ERROR: " (typeof value))))))
