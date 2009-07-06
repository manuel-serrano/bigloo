;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/struct.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  5 18:06:51 1995                          */
;*    Last change :  Sat Jul  7 08:41:48 2001 (serrano)                */
;*    Copyright   :  1995-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The struct approximation management                              */
;*    -------------------------------------------------------------    */
;*    All structs fields approximation are merged into on single set.  */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_struct
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_loose
	    cfa_iterate
	    cfa_cfa
	    cfa_setup
	    cfa_approx))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-make-struct-app ...                            */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-make-struct-app)
   (with-access::pre-make-struct-app node (fun args)
      (node-setup*! args)
      (let* ((owner (pre-make-struct-app-owner node))
	     (node  (shrink! node)))
	 (let ((wnode (widen!::make-struct-app node
			 (owner owner)
			 (approx (make-empty-approx))
			 (value-approx (make-empty-approx)))))
	    (trace (cfa 3) "    make-struct-app: " (shape node) #\Newline)
	    (make-struct-app-approx-set! wnode
					 (make-type-alloc-approx *struct*
								 node))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-struct-ref-app ...                             */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-struct-ref-app)
   (with-access::pre-struct-ref-app node (fun args)
      (node-setup*! args)
      (let ((node (shrink! node)))
	 (widen!::struct-ref-app node (approx (make-type-approx *obj*))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-struct-set!-app ...                            */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-struct-set!-app)
   (with-access::pre-struct-set!-app node (fun args)
      (node-setup*! args)
      (let ((node (shrink! node)))
	 (widen!::struct-set!-app node (approx (make-type-approx *unspec*))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::make-struct-app ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::make-struct-app)
   (with-access::make-struct-app node (args value-approx approx)
      (trace (cfa 4) "   make-struct: " (shape node) #\Newline)
      (cfa! (car args))
      (let ((init-value-approx (cfa! (cadr args))))
	 (union-approx! value-approx init-value-approx)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::struct-ref-app ...                                        */
;*    -------------------------------------------------------------    */
;*    This function differs from the vector-ref one in the several     */
;*    points. In particular we don't have to take care to add *obj*    */
;*    in the approximation of struct-ref because the type of           */
;*    struct-ref _is_ *obj*.                                           */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::struct-ref-app)
   (with-access::struct-ref-app node (args approx)
      ;; even if we don't use it we must walk on the 2nd
      ;; argument of a struct ref.
      (cfa! (cadr args))
      (let ((struct-approx (cfa! (car args))))
	 (trace (cfa 4) "    struct-ref: " (shape node) #\Newline)
	 ;; we check for top
	 (if (approx-top? struct-approx)
	     (approx-set-top! approx))
	 (for-each-approx-alloc
	  (lambda (app)
	     (if (make-struct-app? app)
		 (with-access::make-struct-app app (value-approx)
		    (union-approx! approx value-approx)
		    (approx-set-type! value-approx (approx-type approx)))))
	  struct-approx))
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::struct-set!-app ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::struct-set!-app)
   (with-access::struct-set!-app node (args approx)
      ;; even if we don't use it we must walk on the 2nd
      ;; argument of a struct ref.
      (cfa! (cadr args))
      (let ((struct-approx (cfa! (car args)))
	    (val-approx (cfa! (caddr args))))
	 (trace (cfa 4) "   struct-set!: " (shape node) #\Newline)
	 ;; we check if we have top on the structure
	 (if (approx-top? struct-approx)
	     ;; yes, we have, hence we loose every thing.
	     (loose! val-approx 'all)
	     (for-each-approx-alloc
	      (lambda (app)
		 (if (make-struct-app? app)
		     (with-access::make-struct-app app (value-approx)
			(union-approx! value-approx val-approx))))
	      struct-approx)))
      approx))
      
;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a struct means that struct-ref now returns `obj' and     */
;*    `top' and that all contained allocation are also lost but        */
;*    it does not change anything on the allocation itself.            */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::make-struct-app)
   (with-access::make-struct-app alloc (lost-stamp value-approx)
      (if (=fx lost-stamp *cfa-stamp*)
	  #unspecified
	  (begin
	     (trace (cfa 2) " *** loose: " (shape alloc) #\Newline)
	     (set! lost-stamp *cfa-stamp*)
	     (for-each-approx-alloc loose-alloc! value-approx)
	     (approx-set-type! value-approx *obj*)
	     (approx-set-top! value-approx)))))
   
   
