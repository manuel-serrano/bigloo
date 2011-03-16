;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/procedure.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 12:08:59 1996                          */
;*    Last change :  Wed Mar 16 17:06:27 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
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
   (export  (disable-X-T! approx::approx)))

;*---------------------------------------------------------------------*/
;*    disable-X-T! ...                                                 */
;*---------------------------------------------------------------------*/
(define (disable-X-T! approx)
   (for-each-approx-alloc (lambda (app)
			     (trace (cfa 3)
				    "!!! Je disable X-T pour: " (shape app)
				    #\Newline)
			     (if (make-procedure-app? app)
				 (make-procedure-app-X-T?-set! app #f)))
			  approx))

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
		   (trace (cfa 3)
			  " make-procedure-app: " (shape node) #\Newline
			  "          proc-size: " proc-size #\Newline)
		   (make-procedure-app-approx-set! node
						   (make-type-alloc-approx
						    *procedure*
						    node))
		   ;; we mark that first argument (or we set) is
		   ;; bound to a closure
		   (let* ((clo (car (sfun-args
				     (variable-value
				      (var-variable proc)))))
			  (vclo (local-value clo)))
		      (if (svar/Cinfo? vclo)
			  (begin
			     (trace (cfa 2) "Je set un pre-clo-env..."
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
      (cfa! (cadr args))
      (let ((proc-approx (cfa! (car args)))
	    (offset      (get-node-atom-value (cadr args))))
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
      (cfa! (cadr args))
      (let ((proc-approx (cfa! (car args)))
	    (offset      (get-node-atom-value (cadr args))))
	 (set! vapprox (cfa! (caddr args)))
	 (trace (cfa 4) " procedure-set!: " (shape node) #\Newline
		"         offset: " offset #\Newline
		"         val-ap: " (shape vapprox) #\Newline)
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
			 (union-approx! (vector-ref
					 (make-procedure-app-values-approx
					  app)
					 offset)
					vapprox)))
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
				      (loop (+fx i 1))))))))
		  proc-approx)))
	 approx)))

;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a procedure only means that the result of the procedure  */
;*    is lost and all formals can be bound to top. It does not mean    */
;*    anything about the values closed by the procedure (contrarily to */
;*    vectors).                                                        */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::make-procedure-app)
     (trace (cfa 4) " *** loose-alloc::make-procedure-app: " *cfa-stamp* " "
	    (shape alloc) #\Newline)
    (with-access::make-procedure-app alloc (lost-stamp)
      (if (=fx lost-stamp *cfa-stamp*)
	  #unspecified
	  (begin
	     (trace (cfa 2) " *** loose: " (shape alloc) #\Newline)
	     (set! lost-stamp *cfa-stamp*)
	     (let* ((callee (car (make-procedure-app-args alloc)))
		    (v      (var-variable callee))
		    (fun    (variable-value v)))
		(cfa-export-var! fun v))))))



   



