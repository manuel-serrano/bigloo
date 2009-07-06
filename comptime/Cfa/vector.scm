;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/vector.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  5 18:06:51 1995                          */
;*    Last change :  Sat Jul  7 08:41:55 2001 (serrano)                */
;*    Copyright   :  1995-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The vector approximation managment                               */
;*    -------------------------------------------------------------    */
;*    All vectors fields approximation are merger into on single set.  */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_vector
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    engine_param
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_loose
	    cfa_iterate
	    cfa_cfa
	    cfa_setup
	    cfa_approx
	    cfa_tvector)
   (export  (vector-optim?::bool)))

;*---------------------------------------------------------------------*/
;*    vector-optim? ...                                                */
;*---------------------------------------------------------------------*/
(define (vector-optim?)
   (>=fx *optim* 2))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-make-vector-app ...                            */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-make-vector-app)
   (add-make-vector! node)
   (with-access::pre-make-vector-app node (fun args)
      (node-setup*! args)
      (let* ((owner (pre-make-vector-app-owner node))
	     (node  (shrink! node)))
	 (let ((wnode (widen!::make-vector-app node
			 (owner owner)
			 (approx (make-empty-approx))
			 (value-approx (make-empty-approx)))))
	    (trace (cfa 3) "    make-vector-app: " (shape node) #\Newline)
	    (make-vector-app-approx-set! wnode
					 (make-type-alloc-approx *vector*
								 node))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-valloc/Cinfo ...                               */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-valloc/Cinfo)
   (add-make-vector! node)
   (with-access::pre-valloc/Cinfo node (expr* owner type)
      (node-setup*! expr*)
      (let* ((owner owner)
	     (node (shrink! node)))
	 (let ((wnode (widen!::valloc/Cinfo+optim node
			 (owner owner)
			 (approx (make-empty-approx))
			 (value-approx (make-empty-approx)))))
	    (trace (cfa 3) "  valloc(optim): " (shape node) #\Newline)
	    (valloc/Cinfo+optim-approx-set!
	     wnode
	     (make-type-alloc-approx type node))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::valloc ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::valloc)
   (with-access::valloc node (expr* type)
      (node-setup*! expr*)
      (trace (cfa 3) "  valloc(plain): " (shape node) #\Newline)
      (let* ((approx (make-type-approx type))
	     (wnode (widen!::valloc/Cinfo node
		       (approx approx))))
	 (approx-set-top! approx)
	 wnode)))

;*---------------------------------------------------------------------*/
;*    node-setup! ::vlength ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::vlength)
   (with-access::vlength node (expr* type vtype)
      (node-setup! (car expr*))
      (widen!::vlength/Cinfo node
	 (approx (make-type-approx type))
	 (tvector? (tvector? vtype)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::vref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::vref)
   (with-access::vref node (expr* ftype)
      (node-setup*! expr*)
      (let* ((tvector? (not (or (eq? ftype *_*) (eq? ftype *obj*))))
	     (approx (cond
			(tvector?
			 (make-type-approx ftype))
			((vector-optim?)
			 (make-empty-approx))
			(else
			 (let ((approx (make-type-approx *obj*)))
			    (approx-set-top! approx)
			    approx)))))
	 (widen!::vref/Cinfo node
	    (approx approx)
	    (tvector? tvector?)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::vset! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::vset!)
   (with-access::vset! node (expr* ftype)
      (node-setup*! expr*)
      (widen!::vset!/Cinfo node
	 (approx (make-type-approx *unspec*))
	 (tvector? (not (or (eq? ftype *_*) (eq? ftype *obj*)))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::make-vector-app ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::make-vector-app)
   (with-access::make-vector-app node (args value-approx approx)
      (trace (cfa 4) "   make-vector: " (shape node) #\Newline)
      (cfa! (car args))
      (let ((init-value-approx (cfa! (cadr args))))
	 (union-approx! value-approx init-value-approx)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::valloc/Cinfo+optim ...                                    */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::valloc/Cinfo+optim)
   (with-access::valloc/Cinfo+optim node (expr* approx)
      (trace (cfa 4) " create-vector(optim): " (shape node) #\Newline)
      (for-each cfa! expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::valloc/Cinfo ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::valloc/Cinfo)
   (with-access::valloc/Cinfo node (expr* approx)
      (trace (cfa 4) " create-vector: " (shape node) #\Newline)
      (for-each (lambda (x) (loose! (cfa! x) 'all)) expr*)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa!::approx ::vlength/Cinfo ...                                 */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::vlength/Cinfo)
   (with-access::vlength/Cinfo node (expr* approx)
      (trace (cfa 4) " vector-length: " (shape node) #\Newline)
      (let ((vec-approx (cfa! (car expr*))))
	 (if (not (vector-optim?))
	     (loose! vec-approx 'all)))
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::vref/Cinfo ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::vref/Cinfo)
   (with-access::vref/Cinfo node (expr* approx ftype tvector?)
      ;; even if we don't use it we must walk on the offset
      ;; argument of a vector ref.
      (cfa! (cadr expr*))
      (let ((vec-approx (cfa! (car expr*))))
	 (cond
	    (tvector?
	     approx)
	    ((vector-optim?)
	     (trace (cfa 4) ">>> vector-ref: " (shape node) " "
		    (shape vec-approx) " currently: " (shape approx)
		    #\Newline)
	     ;; we check the type...
	     (if (or (not (eq? (approx-type vec-approx) *vector*))
		     (approx-top? vec-approx))
		 (approx-set-type! approx *obj*))
	     ;; and top
	     (if (approx-top? vec-approx)
		 (approx-set-top! approx))
	     ;; then we scan the allocations.
	     (for-each-approx-alloc
	      (lambda (app)
		 (cond
		    ((make-vector-app? app)
		     (with-access::make-vector-app app (value-approx seen?)
			(set! seen? #t)
			(union-approx! approx value-approx)
			(approx-set-type! value-approx
					  (approx-type approx))))
		    ((valloc/Cinfo+optim? app)
		     (with-access::valloc/Cinfo+optim app (value-approx seen?)
			(set! seen? #t)
			(union-approx! approx value-approx)
			(approx-set-type! value-approx
					  (approx-type approx))))))
	      vec-approx))
	    (else
	     (loose! vec-approx 'all)))
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::vset!/Cinfo ...                                           */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::vset!/Cinfo)
   (with-access::vset!/Cinfo node (expr* approx ftype tvector?)
      ;; even if we don't use it we must walk on the offset
      ;; argument of a vector ref.
      (cfa! (cadr expr*))
      (let ((vec-approx (cfa! (car expr*)))
	    (val-approx (cfa! (caddr expr*))))
	 (cond
	    (tvector?
	     'nothing)
	    ((vector-optim?)
	     (trace (cfa 4) "   vector-set!: " (shape node) " "
		    (shape vec-approx) #\Newline)
	     ;; we check the type...
	     (if (not (eq? (approx-type vec-approx) *vector*))
		 (approx-set-type! val-approx *obj*))
	     ;; we check if we have top on the vector
	     (if (approx-top? vec-approx)
		 ;; yes, we have, hence we loose every thing.
		 (loose! val-approx 'all)
		 ;; no, then we scan the allocations.
		 (for-each-approx-alloc
		  (lambda (app)
		     (cond
			((make-vector-app? app)
			 (with-access::make-vector-app app (value-approx seen?)
			    (set! seen? #t)
			    (union-approx! value-approx val-approx)))
			((valloc/Cinfo+optim? app)
			 (with-access::valloc/Cinfo+optim app (value-approx seen?)
			    (set! seen? #t)
			    (union-approx! value-approx val-approx)))))
		  vec-approx)))
	    (else
		(loose! vec-approx 'all)
		(loose! val-approx 'all))))
      approx))
      
;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a vector means that vector-ref now returns `obj' and     */
;*    `top' and that all contained allocation are also lost but        */
;*    it does not change anything on the allocation itself.            */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::make-vector-app)
   (with-access::make-vector-app alloc (lost-stamp value-approx)
      (if (=fx lost-stamp *cfa-stamp*)
	  #unspecified
	  (begin
	     (trace (cfa 2) " *** loose: " (shape alloc) #\Newline)
	     (set! lost-stamp *cfa-stamp*)
	     (for-each-approx-alloc loose-alloc! value-approx)
	     (approx-set-type! value-approx *obj*)
	     (approx-set-top! value-approx)))))
   
;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a vector means that vector-ref now returns `obj' and     */
;*    `top' and that all contained allocation are also lost but        */
;*    it does not change anything on the allocation itself.            */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::valloc/Cinfo+optim)
   (with-access::valloc/Cinfo+optim alloc (lost-stamp value-approx)
      (if (=fx lost-stamp *cfa-stamp*)
	  #unspecified
	  (begin
	     (trace (cfa 2) " *** loose: " (shape alloc) #\Newline)
	     (trace (cfa 3) "     value: " (shape value-approx) #\Newline)
	     (set! lost-stamp *cfa-stamp*)
	     (for-each-approx-alloc loose-alloc! value-approx)
	     (approx-set-type! value-approx *obj*)
	     (approx-set-top! value-approx)))))
   

   


 
