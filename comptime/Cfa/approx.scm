;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/approx.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 12:32:06 1996                          */
;*    Last change :  Sat Mar 18 21:03:34 2017 (serrano)                */
;*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The approximation manipulations.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_approx
   (include "Tools/trace.sch")
   (import  type_type
	    type_cache
	    type_coercion
	    type_misc
	    tools_shape
	    tools_error
	    tools_misc
	    ast_var
	    ast_node
	    ast_ident
	    cfa_info
	    cfa_info2
	    cfa_info3
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
	    (union-approx-filter!::approx ::approx ::approx)
	    (approx-set-type! ::approx ::type)
	    (approx-set-top! ::approx)
	    (make-empty-approx::approx)
	    (make-type-approx::approx ::type)
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
   (trace cfa "================== declare ===========================\n")
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
   (approx-set-type! dst (get-src-approx-type src))
   ;; check the consistency with *_* type
   (unless (eq? (approx-type dst) *_*)
      (when (any vector-approx? (set->list (approx-allocs src)))
	 (approx-set-type! dst *vector*)))
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
;*    union-approx/type! ...                                           */
;*---------------------------------------------------------------------*/
(define (union-approx/type! dst::approx src::approx ty::type)
   
   (define (set-union/type! dst src pred)
      ;; as set-union! but only copy values satisfying pred
      (let ((res #f)
	    (set (approx-allocs dst)))
	 (for-each (lambda (v)
		      (when (and (pred v) (not (set-member? set v)))
			 (set-extend! set v)
			 (set! res #t)))
	    (set->list (approx-allocs src)))
	 res))
   
   (when (approx-top? src)
      (approx-set-top! dst))
   (when (bigloo-type? ty)
      (cond
	 ((eq? ty *vector*)
	  (when (set-union/type! dst src vector-approx?)
	     (continue-cfa! 'union)))
	 ((eq? ty *procedure*)
	  (when (set-union/type! dst src procedure-approx?)
	     (continue-cfa! 'union)))
	 ((or (eq? ty *pair*)
	      (eq? ty *epair*)
	      (eq? ty *pair-nil*)
	      (eq? ty *list*))
	  (when (set-union/type! dst src cons-approx?)
	     (continue-cfa! 'union)))
	 ((eq? ty *struct*)
	  (when (set-union/type! dst src struct-approx?)
	     (continue-cfa! 'union)))
	 ((not (or (eq? ty *symbol*)
		   (eq? ty *keyword*)
		   (eq? ty *unspec*)
		   (eq? ty *bstring*)
		   (eq? ty *bint*)
		   (eq? ty *belong*)
		   (eq? ty *bllong*)
		   (eq? ty *bignum*)
		   (eq? ty *real*)
		   (eq? ty *bchar*)
		   (eq? ty *nil*)
		   (eq? ty *bbool*)))
	  (when (set-union! (approx-allocs dst) (approx-allocs src))
	     (continue-cfa! 'union)))))
   dst)

;*---------------------------------------------------------------------*/
;*    union-approx-filter! ...                                         */
;*    -------------------------------------------------------------    */
;*    This unification is used for function that are pre-assigned a    */
;*    type. Because the type will be enforced by latter stage, the     */
;*    union here must only copy values that are compatible with that   */
;*    destination type. See cfa-intern-sfun! and cfa! ::let-var        */
;*---------------------------------------------------------------------*/
(define (union-approx-filter!::approx dst::approx src::approx)
   (let ((ty (approx-type dst)))
      (if (or (not (approx-type-locked? dst)) (eq? ty *_*) (eq? ty *obj*))
	  (union-approx! dst src)
	  (union-approx/type! dst src ty))))

;*---------------------------------------------------------------------*/
;*    vector-approx? ...                                               */
;*---------------------------------------------------------------------*/
(define (vector-approx? x)
   (or (make-vector-app? x) (valloc/Cinfo+optim? x)))

;*---------------------------------------------------------------------*/
;*    procedure-approx? ...                                            */
;*---------------------------------------------------------------------*/
(define (procedure-approx? x)
   (make-procedure-app? x))

;*---------------------------------------------------------------------*/
;*    cons-approx? ...                                                 */
;*---------------------------------------------------------------------*/
(define (cons-approx? x)
   (cons-app? x))

;*---------------------------------------------------------------------*/
;*    struct-approx? ...                                               */
;*---------------------------------------------------------------------*/
(define (struct-approx? x)
   (make-struct-app? x))

;*---------------------------------------------------------------------*/
;*    get-src-approx-type ...                                          */
;*    -------------------------------------------------------------    */
;*    Vectors are handled specially. They are assigned the type _      */
;*    because they may be turned in tvector. This function handles     */
;*    the special case where vectors are mixed up with other types.    */
;*---------------------------------------------------------------------*/
(define (get-src-approx-type a::approx)
   (let ((type (approx-type a)))
      (cond
	 ((eq? type *_*)
	  type)
	 ((=fx (set-length (approx-allocs a)) 0)
	  type)
	 (else
	  (let ((allocs (set->list (approx-allocs a))))
	     (if (any vector-approx? allocs)
		 (if (and (eq? type *vector*) (every vector-approx? allocs))
		     *vector*
		     *obj*)
		 type))))))
	     
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
	      (and (eq? dtype *int*) (eq? type *long*))
	      (and (eq? dtype *bint*) (or (eq? type *int*) (eq? type *long*))))
	  ;; integer equivalence
	  #f)
	 ((and (or (eq? dtype *long*) (eq? dtype *int*)) (eq? type *bint*))
	  (approx-type-set! dst *bint*)
	  #t)
	 ((and (eq? dtype *epair*) (eq? type *pair*))
	  ;; pair-nil subtyping 1
	  (approx-type-set! dst *pair*)
	  (continue-cfa! 'approx-set-type!))
	 ((and (eq? dtype *pair-nil*)
	       (or (eq? type *bnil*)
		   (eq? type *pair*) (eq? type *epair*)
		   (eq? type *list*)))
	  ;; pair-nil subtyping 2
	  #f)
	 ((and (bigloo-type? dtype) (eq? dtype (get-bigloo-type type)))
	  ;; C / bigloo mapping

	  #t)
	 ((or (and (bigloo-type? dtype) (eq? dtype (get-bigloo-type type)))
	      (and (bigloo-type? type) (eq? type (get-bigloo-type dtype))))
	  ;; C / bigloo mapping
	  (when (eq? type *obj*)
	     (approx-type-set! dst type))
	  #t)
	 ((and (or (eq? dtype *bnil*) (eq? dtype *pair*) (eq? dtype *epair*))
	       (or (eq? type *bnil*)
		   (eq? type *pair*) (eq? type *epair*)
		   (eq? type *pair-nil*)
		   (eq? type *list*)))
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
	 ((and (not (bigloo-type? dtype)) (not (bigloo-type? type)))
	  (cond
	     ((c-subtype? type dtype)
	      #f)
	     ((c-subtype? dtype type)
	      (approx-type-set! dst type)
	      (continue-cfa! 'approx-set-type!))
	     (else
	      (approx-type-set! dst *obj*)
	      (continue-cfa! 'approx-set-type!))))
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
   (with-access::approx dst (top? dup)
      (unless top?
	 (approx-top?-set! dst #t)
	 (continue-cfa! 'approx-set-top!))
      (when (approx? dup)
	 (approx-set-top! dup))))

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
      (let* ((keys   (set->vector allocs))
	     (len    (vector-length keys))
	     (slen   (if top? (+fx len 1) len))
	     (struct (make-struct 'approx (+fx 1 slen) #unspecified)))
	 (struct-set! struct 0 (type-id type))
	 (if top? (struct-set! struct 1 'top))
	 (let loop ((r (if top? 2 1))
		    (w 0))
	    (if (=fx w len)
		struct
		(begin 
		   (struct-set! struct r (node-key (vector-ref keys w)))
		   (loop (+fx r 1) (+fx w 1))))))))

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

