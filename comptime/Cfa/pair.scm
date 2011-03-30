;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/pair.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 30 08:11:10 2011                          */
;*    Last change :  Wed Mar 30 09:42:19 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The pair approximation manager                                   */
;*    -------------------------------------------------------------    */
;*    Pair are approximated by two values, the car and the cdr.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_pair
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
   (export  (pair-optim?::bool)))

;*---------------------------------------------------------------------*/
;*    pair-optim? ...                                                  */
;*---------------------------------------------------------------------*/
(define (pair-optim?)
   (and (>=fx *optim* 2) *optim-cfa-pair?*))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-cons-app ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-cons-app)
   (with-access::pre-cons-app node (fun args)
      (node-setup*! args)
      (let* ((owner (pre-cons-app-owner node))
	     (node  (shrink! node))
	     (wnode (widen!::cons-app node
		       (owner owner)
		       (approxes (cons (make-empty-approx) (make-empty-approx)))
		       (approx (make-empty-approx)))))
	 (trace (cfa 3) "    cons-app: " (shape node) #\Newline)
	 (cons-app-approx-set! wnode (make-type-alloc-approx *pair* node)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-cons-ref-app ...                               */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-cons-ref-app)
   (with-access::pre-cons-ref-app node (fun args get)
      (node-setup*! args)
      (let ((node (shrink! node)))
	 (widen!::cons-ref-app node
	    (get get)
	    (approx (make-empty-approx))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-cons-set!-app ...                              */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-cons-set!-app)
   (with-access::pre-cons-set!-app node (fun args get)
      (node-setup*! args)
      (let ((node (shrink! node)))
	 (widen!::cons-set!-app node
	    (get get)
	    (approx (make-type-approx *unspec*))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::cons-app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::cons-app)
   (with-access::cons-app node (args approxes approx)
      (trace (cfa 4) "   cons: " (shape node) #\Newline)
      (cfa! (car args))
      (let ((car-approx (cfa! (cadr args)))
	    (cdr-approx (cfa! (caddr args))))
	 (union-approx! (car approxes) car-approx)
	 (union-approx! (cdr approxes) cdr-approx)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::cons-ref-app ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::cons-ref-app)
   (with-access::cons-ref-app node (args approx get)
      (let ((cons-approx (cfa! (car args))))
	 (trace (cfa 4) ">>> cons-ref: " (shape node) " "
		(shape cons-approx) " currently: " (shape approx)
		#\Newline)
	 ;; we check for top
	 (when (approx-top? cons-approx)
	    (approx-set-top! approx))
	 ;; then we scan the allocations.
	 (for-each-approx-alloc
	  (lambda (app)
	     (when (cons-app? app)
		(with-access::cons-app app (approxes seen?)
		    (set! seen? #t)
		    (union-approx! approx (get approxes))
		    (approx-set-type! (get approxes) (approx-type approx)))))
	  cons-approx))
	 approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::cons-set!-app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::cons-set!-app)
   (with-access::cons-set!-app node (args approx get)
      (let ((cons-approx (cfa! (car args)))
	    (val-approx (cfa! (cadr args))))
	 (trace (cfa 4) "   set-car!: " (shape node) " "
		(shape cons-approx) #\Newline)
	 ;; we check the type...
	 (unless (eq? (approx-type cons-approx) *pair*)
	    (approx-set-type! val-approx *obj*))
	 ;; we check if we have top on the vector
	 (if (approx-top? cons-approx)
	     ;; yes, we have, hence we loose every thing.
	     (loose! val-approx 'all)
	     ;; no, then we scan the allocations.
	     (for-each-approx-alloc
	      (lambda (app)
		 (when (cons-app? app)
		    (with-access::cons-app app (approxes seen?)
		       (set! seen? #t)
		       (union-approx! (get approxes) val-approx))))
	      cons-approx)))
      approx))
      
;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a pair means that cons-ref now returns `obj' and         */
;*    `top' and that all contained allocation are also lost but        */
;*    it does not change anything on the allocation itself.            */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::cons-app)
   (with-access::cons-app alloc (lost-stamp approxes)
      (unless (=fx lost-stamp *cfa-stamp*)
	 (trace (cfa 2) " *** loose(make-cons-app): " (shape alloc) #\Newline)
	 (set! lost-stamp *cfa-stamp*)
	 (for-each-approx-alloc loose-alloc! (car approxes))
	 (for-each-approx-alloc loose-alloc! (cdr approxes))
	 (approx-set-type! (car approxes) *obj*)
	 (approx-set-type! (cdr approxes) *obj*)
	 (approx-set-top! (car approxes))
	 (approx-set-top! (cdr approxes)))))
