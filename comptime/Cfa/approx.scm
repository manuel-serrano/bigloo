;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/approx.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 12:32:06 1996                          */
;*    Last change :  Sun Mar 20 08:16:29 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The approximation manipulations.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_approx
   (import  type_type
	    type_cache
	    tools_shape
	    tools_error
	    tools_misc
	    ast_var
	    ast_node
	    ast_ident
	    cfa_info
	    cfa_info2
	    cfa_collect
	    cfa_set
	    cfa_iterate
	    cfa_loose
	    cfa_procedure
	    object_class)
   (export  (inline node-key ::node/effect)
	    (inline node-key-set! ::node/effect ::obj)
	    (declare-approx-sets!)
	    (union-approx!::approx ::approx ::approx)
	    (approx-set-type! ::approx ::type)
	    (approx-set-top! ::approx)
	    (make-empty-approx::approx)
	    (make-type-approx::approx ::type)
	    (make-alloc-approx::approx ::app)
	    (make-type-alloc-approx::approx ::type ::node)
	    (for-each-approx-alloc ::procedure ::approx)
	    (empty-approx-alloc? ::approx)
	    (generic get-node-atom-value ::node)))
 
;*---------------------------------------------------------------------*/
;*    The sets                                                         */
;*---------------------------------------------------------------------*/
(define *alloc-set* #unspecified)
				
;*---------------------------------------------------------------------*/
;*    declare-approx-sets! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-approx-sets!)
   (set! *alloc-set* (declare-set! (list->vector (get-allocs)))))

;*---------------------------------------------------------------------*/
;*    node-key ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (node-key node::node/effect)
   (node/effect-key node))

;*---------------------------------------------------------------------*/
;*    node-key-set! ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (node-key-set! node::node/effect key)
   (node/effect-key-set! node key))

;*---------------------------------------------------------------------*/
;*    union-approx! ...                                                */
;*---------------------------------------------------------------------*/
(define (union-approx!::approx dst::approx src::approx)
   ;; we make the union of the type
   (approx-set-type! dst (approx-type src))
   ;; we check *obj* to prevent closure optimizations
   (when (not (or (eq? (approx-type dst) *procedure*)
		  (eq? (approx-type dst) *_*)))
      (disable-X-T! src "dst is not a procedure"))
   ;; of the alloc/top 
   (when (approx-top? src)
      (approx-set-top! dst))
   ;; and we make the union of approximations
   (when (set-union! (approx-allocs dst) (approx-allocs src))
       (continue-cfa! 'union))
   ;; and we return the dst
   dst)

;*---------------------------------------------------------------------*/
;*    approx-set-type! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function computes the smallest subtype of dst and type.     */
;*    Subtyping rules may apply to integers, nil/pair/epair/pair-nil,  */
;*    and classes.                                                     */
;*---------------------------------------------------------------------*/
(define (approx-set-type! dst::approx type::type)
   (let ((dtype (approx-type dst)))
      (cond
	 ((approx-type-locked? dst)
	  #f)
	 ((eq? type *_*)
	  #f)
	 ((eq? dtype type)
	  #f)
	 ((or (and (eq? dtype *long*) (eq? type *int*))
	      (and (eq? dtype *int*) (eq? type *long*)))
	  ;; integer equivalence
	  #f)
	 ((and (eq? dtype *epair*) (eq? type *pair*))
	  ;; pair-nil subtyping 1
	  (approx-type-set! dst *pair*)
	  (continue-cfa! 'approx-set-type!))
	 ((and (eq? dtype *pair-nil*)
	       (or (eq? type *bnil*) (eq? type *pair*) (eq? type *epair*)))
	  ;; pair-nil subtyping 2
	  #f)
	 ((and (or (eq? dtype *bnil*) (eq? dtype *pair*) (eq? dtype *epair*))
	       (or (eq? type *bnil*)
		   (eq? type *pair*) (eq? type *epair*) (eq? type *pair-nil*)))
	  ;; pair-nil subtyping 3
	  (approx-type-set! dst *pair-nil*)
	  (continue-cfa! 'approx-set-type!))
	 ((and (tclass? dtype) (tclass? type))
	  (let ((super (find-common-super-class dtype type)))
	     (cond
		((eq? super dtype)
		 #f)
		(super
		 ;; class subtyping
		 (approx-type-set! dst super)
		 (continue-cfa! 'approx-set-type!))
		(else
		 (approx-type-set! dst *obj*)
		 (continue-cfa! 'approx-set-type!)))))
	 ((eq? dtype *obj*)
	  #f)
	 ((eq? dtype *_*)
	  (approx-type-set! dst type)
	  (continue-cfa! 'approx-set-type!))
	 (else
	  (approx-type-set! dst *obj*)
	  (continue-cfa! 'approx-set-type!)))))

;*---------------------------------------------------------------------*/
;*    approx-set-top! ...                                              */
;*---------------------------------------------------------------------*/
(define (approx-set-top! dst::approx)
   (if (not (approx-top? dst))
       (begin
	  (approx-top?-set! dst #t)
	  (continue-cfa! 'approx-set-top!))))

;*---------------------------------------------------------------------*/
;*    make-empty-approx ...                                            */
;*---------------------------------------------------------------------*/
(define (make-empty-approx::approx)
   (let ((allocs (make-set! *alloc-set*)))
      (instantiate::approx
	 (type *_*)
	 (allocs allocs))))
		 
;*---------------------------------------------------------------------*/
;*    make-type-approx ...                                             */
;*---------------------------------------------------------------------*/
(define (make-type-approx type)
   (let ((allocs (make-set! *alloc-set*)))
      (instantiate::approx
	 (type-locked? (not (eq? type *_*)))
	 (type type)
	 (allocs allocs))))
		 
;*---------------------------------------------------------------------*/
;*    make-alloc-approx ...                                            */
;*---------------------------------------------------------------------*/
(define (make-alloc-approx alloc)
   (let ((allocs (make-set! *alloc-set*)))
      (set-extend! allocs alloc)
      (instantiate::approx
	 (type type)
	 (allocs allocs))))
		 
;*---------------------------------------------------------------------*/
;*    make-type-alloc-approx ...                                       */
;*---------------------------------------------------------------------*/
(define (make-type-alloc-approx type alloc)
   (let ((allocs (make-set! *alloc-set*)))
      (set-extend! allocs alloc)
      (instantiate::approx
	 (type-locked? (not (eq? type *_*)))
	 (type type)
	 (allocs allocs))))
		 
;*---------------------------------------------------------------------*/
;*    shape ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (shape exp::approx)
   (with-access::approx exp (top? type allocs)
      (let ((type-id (make-typed-formal (type-id type))))
	 (let* ((keys   (set->vector allocs))
		(len    (vector-length keys))
		(slen   (if top? (+fx len 1) len))
		(struct (make-struct type-id slen #unspecified)))
	    (if top? (struct-set! struct 0 'top))
	    (let loop ((r (if top? 1 0))
		       (w 0))
	       (if (=fx w len)
		   struct
		   (begin 
		      (struct-set! struct r (node-key (vector-ref keys w)))
		      (loop (+fx r 1) (+fx w 1)))))))))

;*---------------------------------------------------------------------*/
;*    for-each-approx-alloc ...                                        */
;*---------------------------------------------------------------------*/
(define (for-each-approx-alloc proc::procedure approx::approx)
   (with-access::approx approx (allocs)
      (set-for-each proc allocs)))

;*---------------------------------------------------------------------*/
;*    empty-approx-alloc? ...                                          */
;*---------------------------------------------------------------------*/
(define (empty-approx-alloc? approx::approx)
   (=fx (set-length (approx-allocs approx)) 0))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ...                                          */
;*    -------------------------------------------------------------    */
;*    To return false, we just return a non-atomic value.              */
;*---------------------------------------------------------------------*/
(define-generic (get-node-atom-value node::node)
   '(no-atom-value))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ::atom ...                                   */
;*---------------------------------------------------------------------*/
(define-method (get-node-atom-value node::atom)
   (atom-value node))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ::var ...                                    */
;*---------------------------------------------------------------------*/
(define-method (get-node-atom-value node::var)
   (let ((v (var-variable node)))
      (if (and (reshaped-local? v) (node? (reshaped-local-binding-value v)))
	  (get-node-atom-value (reshaped-local-binding-value v))
	  '(no-atom-value))))

