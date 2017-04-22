;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/typeof.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:25:05 1996                          */
;*    Last change :  Fri Apr 21 18:39:45 2017 (serrano)                */
;*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The type of the things                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_typeof
   (include "Tvector/tvector.sch")
   (import  type_type
	    type_cache
	    type_coercion
	    type_misc
	    engine_param
	    ast_node
	    ast_var
	    tools_shape
	    tools_speek
	    object_class)
   (export  (is-subtype? t1 t2)
	    (get-type-atom::type <atom>)
	    (get-type-kwote::type <kwote>)
	    (generic get-type::type ::node ::bool)))

;*---------------------------------------------------------------------*/
;*    get-type-atom ...                                                */
;*---------------------------------------------------------------------*/
(define (get-type-atom atom)
   (cond
      ((null? atom) *bnil*)
      ((fixnum? atom) *long*)
      ((bignum? atom) *bignum*)
      ((real? atom) *real*)
      ((boolean? atom) *bool*)
      ((char? atom) *char*)
      ((string? atom) *bstring*)
      ((eq? atom #unspecified) *unspec*)
      ((elong? atom) *elong*)
      ((llong? atom) *llong*)
      ((int8? atom) *int8*)
      ((uint8? atom) *uint8*)
      ((int16? atom) *int16*)
      ((uint16? atom) *uint16*)
      ((int32? atom) *int32*)
      ((uint32? atom) *uint32*)
      ((int64? atom) *int64*)
      ((uint64? atom) *uint64*)
      ((keyword? atom) *keyword*)
      (else *obj*)))

;*---------------------------------------------------------------------*/
;*    get-type-kwote ...                                               */
;*---------------------------------------------------------------------*/
(define (get-type-kwote kwote)
   (cond
      ((symbol? kwote) *symbol*)
      ((keyword? kwote) *keyword*)
      ((pair? kwote) *pair*)
      ((null? kwote) *bnil*)
      ((vector? kwote) *vector*)
      ((a-tvector? kwote) (a-tvector-type kwote))
      (else *obj*)))

;*---------------------------------------------------------------------*/
;*    pair-nil? ...                                                    */
;*---------------------------------------------------------------------*/
(define (pair-nil? t)
   (or (eq? t *pair*) (eq? t *epair*) (eq? t *bnil*) (eq? t *pair-nil*)))

;*---------------------------------------------------------------------*/
;*    get-type ...                                                     */
;*    -------------------------------------------------------------    */
;*    The function GET-TYPE finds a type for a node. The general       */
;*    idea is to look at the TYPE field of each node. If that          */
;*    particular node has not been given a type yet (the TYPE field    */
;*    is equal to *_*) then, a recursive descent search for the        */
;*    type of node.                                                    */
;*---------------------------------------------------------------------*/
(define-generic (get-type::type node::node strict)
   (node-type node))

;*---------------------------------------------------------------------*/
;*    get-type ::var ...                                               */
;*    -------------------------------------------------------------    */
;*    Computing the value of a variable reference is complex. It used  */
;*    to consist in getting the variable type. Since the dataflow      */
;*    analysis has been added, this is no longer that simple because   */
;*    the variable reference type depends on its context. For instance */
;*    under the "then" branch of a type predicat the reference is of   */
;*    a more precise type that the variable itself.                    */
;*                                                                     */
;*    To make the thing even more complex, the type of the reference   */
;*    might be not correct with respect to boxing/unboxing invariant.  */
;*    For instance, after an expression such as (set! x 1), the        */
;*    variable X will be considered as ::int. However if the global    */
;*    analysis fails to unbox X type it as to be ::obj, then the       */
;*    expression must be compiled as  X = BINT( 1 ) and not X = 1.     */
;*    The predicate TYPE-MORE-SPECIFIC? handles is used to detect      */
;*    these cases.                                                     */
;*---------------------------------------------------------------------*/
(define-method (get-type node::var strict)
   
   (define (verbose-type typen typev)
      (unless (or (eq? typen *obj*)
		  (eq? typen *_*)
		  (eq? typen typev))
	 (verbose 3 "   refining type " (shape node) ": "
	    (shape typev) " -> " (shape typen))))
   
   (define (type-more-specific? ntype vtype)
      (or (and (eq? vtype *obj*) (bigloo-type? ntype) (not (eq? ntype *_*)))
	  (and (eq? vtype *pair-nil*) (eq? ntype *pair*))
	  (and (tclass? vtype) (tclass? ntype) (type-subclass? ntype vtype))))
   
   (with-access::var node (variable type)
      (with-access::variable variable ((vtype type))
	 (cond
	    ((eq? type *_*) vtype)
	    ((and (tclass? vtype) (type-subclass? type vtype)) vtype)
	    (else type)))))

;*---------------------------------------------------------------------*/
;*    get-type ::sequence ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::sequence strict)
   (with-access::sequence node (type)
      (if (eq? type *_*)
	  (with-access::sequence node (nodes)
	     (get-type (car (last-pair nodes)) strict))
	  type)))

;*---------------------------------------------------------------------*/
;*    get-type ::sync ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::sync strict)
   (with-access::sync node (type)
      (if (eq? type *_*)
	  (with-access::sync node (body)
	     (get-type body strict))
	  type)))

;*---------------------------------------------------------------------*/
;*    get-type ::conditional ...                                       */
;*---------------------------------------------------------------------*/
(define-method (get-type node::conditional strict)
   (with-access::conditional node (type test true false)
      (if (eq? type *_*)
	  (with-access::conditional node (test true false)
	     (let ((ttrue (get-type true strict))
		   (tfalse (get-type false strict)))
		(cond
		   ((or (eq? ttrue tfalse) (eq? tfalse *magic*)) ttrue)
		   ((eq? ttrue *magic*) tfalse)
		   ((and (pair-nil? ttrue) (pair-nil? tfalse)) *pair-nil*)
		   ((is-strict-subtype? ttrue tfalse strict) tfalse)
		   ((is-strict-subtype? tfalse ttrue strict) ttrue)
		   (else *obj*))))
	  type)))

;*---------------------------------------------------------------------*/
;*    is-strict-subtype? ...                                           */
;*---------------------------------------------------------------------*/
(define (is-strict-subtype? t1 t2 strict)
   (when (is-subtype? t1 t2)
      (or (not strict) (and (not (eq? t1 *_*)) (not (eq? t2 *_*))))))

;*---------------------------------------------------------------------*/
;*    is-subtype? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Is t1 a subtype of t2?                                           */
;*---------------------------------------------------------------------*/
(define (is-subtype? t1 t2)
   (cond
      ((eq? t1 t2)
       #t)
      ((or (eq? t1 *magic*) (eq? t2 *magic*))
       #t)
      ((or (eq? t2 *_*) (eq? t1 *_*))
       #t)
      ((eq? t2 *pair-nil*)
       (or (eq? t1 *pair*) (eq? t1 *epair*) (eq? t1 *nil*)))
      ((or (tclass? t1) (tclass? t2))
       (type-subclass? t1 t2))
      ((and (not (bigloo-type? t1)) (not (bigloo-type? t2)))
       (c-subtype? t1 t2))
      ((and (eq? t2 *bint*) (or (eq? type *int*) (eq? type *long*)))
       #t)
      (else
       (eq? t2 *obj*))))

;*---------------------------------------------------------------------*/
;*    get-type ::switch ...                                            */
;*---------------------------------------------------------------------*/
(define-method (get-type node::switch strict)
   (with-access::switch node (type clauses test)
      (if (eq? type *_*)
	  (let loop ((clauses (cdr clauses))
		     (type (get-type (cdr (car clauses)) strict)))
	     (if (null? clauses)
		 type
		 (let ((ntype (get-type (cdr (car clauses)) strict)))
		    (cond
		       ((eq? ntype type)
			(loop (cdr clauses) type))
		       ((eq? type *magic*)
			(loop (cdr clauses) ntype))
		       ((or (eq? ntype type) (eq? ntype *magic*))
			(loop (cdr clauses) type))
		       ((and (pair-nil? type) (pair-nil? ntype))
			(loop (cdr clauses) *pair-nil*))
		       (else *obj*)))))
	  type)))

;*---------------------------------------------------------------------*/
;*    get-type ::let-fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::let-fun strict)
   (with-access::let-fun node (type body)
      (if (eq? type *_*)
	  (get-type body strict)
	  type)))

;*---------------------------------------------------------------------*/
;*    get-type ::let-var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::let-var strict)
   (with-access::let-var node (type body)
      (if (eq? type *_*)
	  (get-type body strict)
	  type)))
 
;*---------------------------------------------------------------------*/
;*    get-type ::app ...                                               */
;*---------------------------------------------------------------------*/
(define-method (get-type node::app strict)
   (with-access::app node (type fun)
      (if (eq? type *_*)
	  (variable-type (var-variable fun))
	  type)))
