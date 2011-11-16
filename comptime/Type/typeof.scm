;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/typeof.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:25:05 1996                          */
;*    Last change :  Wed Nov 16 08:07:01 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
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
   (export  (get-type-atom::type <atom>)
	    (get-type-kwote::type <kwote>)
	    (generic get-type::type ::node)))

;*---------------------------------------------------------------------*/
;*    get-type-atom ...                                                */
;*---------------------------------------------------------------------*/
(define (get-type-atom atom)
   (cond
      ((null? atom)
       *bnil*)
      ((fixnum? atom)
       *long*)
      ((bignum? atom)
       *bignum*)
      ((real? atom)
       *real*)
      ((boolean? atom)
       *bool*)
      ((char? atom)
       *char*)
      ((string? atom)
       *bstring*)
      ((eq? atom #unspecified)
       *unspec*)
      ((elong? atom)
       *elong*)
      ((llong? atom)
       *llong*)
      ((keyword? atom)
       *keyword*)
      (else
       *obj*)))

;*---------------------------------------------------------------------*/
;*    get-type-kwote ...                                               */
;*---------------------------------------------------------------------*/
(define (get-type-kwote kwote)
   (cond
      ((symbol? kwote)
       *symbol*)
      ((keyword? kwote)
       *keyword*)
      ((pair? kwote)
       *pair*)
      ((null? kwote)
       *bnil*)
      ((vector? kwote)
       *vector*)
      ((a-tvector? kwote)
       (a-tvector-type kwote))
      (else
       *obj*)))

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
(define-generic (get-type::type node::node)
   (if *strict-node-type*
       (node-type node)
       (error "get-type" "No method for this object" node)))

;*---------------------------------------------------------------------*/
;*    get-type ::atom ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::atom)
   (if *strict-node-type*
       (call-next-method)
       (with-access::atom node (value)
	  (get-type-atom value))))
 
;*---------------------------------------------------------------------*/
;*    get-type ::kwote ...                                             */
;*---------------------------------------------------------------------*/
(define-method (get-type node::kwote)
   (if *strict-node-type*
       (call-next-method)
       (with-access::kwote node (value)
	  (get-type-kwote value))))

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
(define-method (get-type node::var)
   
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
	 (if *strict-node-type*
	     (cond
		((eq? type *_*)
		 vtype)
		((and (tclass? vtype) (type-subclass? type vtype))
		 vtype)
		(else
		 type))
	     (let ((value (variable-value variable)))
		(cond
		   ((sfun? value)
		    *procedure*)
		   ((cfun? value)
		    *obj*)
		   (else
		    (if (and *optim-dataflow-types?*
			     (type-more-specific? type vtype))
			(begin
			   (verbose-type type vtype)
			   type)
			vtype))))))))

;*---------------------------------------------------------------------*/
;*    get-type ::closure ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::closure)
   ;; don't call call-next-method because a closure < var
   (if *strict-node-type*
       (call-next-method)
       *procedure*))

;*---------------------------------------------------------------------*/
;*    get-type ::sequence ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::sequence)
   (if *strict-node-type*
       (with-access::sequence node (type)
	  (if (eq? type *_*)
	      (with-access::sequence node (nodes)
		 (get-type (car (last-pair nodes))))
	      type))
       (with-access::sequence node (nodes)
	  (get-type (car (last-pair nodes))))))

;*---------------------------------------------------------------------*/
;*    get-type ::setq ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::setq)
   (if *strict-node-type*
       (call-next-method)
       *unspec*))

;*---------------------------------------------------------------------*/
;*    get-type ::conditional ...                                       */
;*---------------------------------------------------------------------*/
(define-method (get-type node::conditional)
   (if *strict-node-type*
       (with-access::conditional node (type test true false)
	  (if (eq? type *_*)
	      (with-access::conditional node (test true false)
		 (let ((ttrue (get-type true))
		       (tfalse (get-type false)))
		    (cond
		       ((or (eq? ttrue tfalse) (eq? tfalse *magic*)) ttrue)
		       ((eq? ttrue *magic*) tfalse)
		       ((and (pair-nil? ttrue) (pair-nil? tfalse)) *pair-nil*)
		       ((subtype? ttrue tfalse) tfalse)
		       ((subtype? tfalse ttrue) ttrue)
		       (else *obj*))))
	      type))
       (with-access::conditional node (test true false)
	  (let ((ttrue (get-type true))
		(tfalse (get-type false)))
	     (cond ((or (eq? ttrue tfalse) (eq? tfalse *magic*)) ttrue)
		   ((eq? ttrue *magic*) tfalse)
		   (else *obj*))))))

;*---------------------------------------------------------------------*/
;*    subtype? ...                                                     */
;*    -------------------------------------------------------------    */
;*    Is t1 a subtype of t2?                                           */
;*---------------------------------------------------------------------*/
(define (subtype? t1 t2)
   
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
;*    get-type ::fail ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::fail)
   (if *strict-node-type*
       (call-next-method)
       *magic*))

;*---------------------------------------------------------------------*/
;*    get-type ::select ...                                            */
;*---------------------------------------------------------------------*/
(define-method (get-type node::select)
   (if *strict-node-type*
       (with-access::select node (type clauses test)
	  (if (eq? type *_*)
	      (let loop ((clauses (cdr clauses))
			 (type (get-type (cdr (car clauses)))))
		 (if (null? clauses)
		     type
		     (let ((ntype (get-type (cdr (car clauses)))))
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
	      type))
       (with-access::select node (clauses test)
	  (let loop ((clauses (cdr clauses))
		     (type (get-type (cdr (car clauses)))))
	     (if (null? clauses)
		 type
		 (let ((ntype (get-type (cdr (car clauses)))))
		    (cond
		       ((eq? type *magic*)
			(loop (cdr clauses) ntype))
		       ((or (eq? ntype type) (eq? ntype *magic*))
			(loop (cdr clauses) type))
		       (else *obj*))))))))

;*---------------------------------------------------------------------*/
;*    get-type ::let-fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::let-fun)
   (if *strict-node-type*
       (with-access::let-fun node (type body)
	  (if (eq? type *_*)
	      (get-type body)
	      type))
       (with-access::let-fun node (body)
	  (get-type body))))

;*---------------------------------------------------------------------*/
;*    get-type ::let-var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::let-var)
   (if *strict-node-type*
       (with-access::let-var node (type body)
	  (if (eq? type *_*)
	      (get-type body)
	      type))
       (with-access::let-var node (body)
	  (get-type body))))
 
;*---------------------------------------------------------------------*/
;*    get-type ::set-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (get-type node::set-ex-it)
   (if *strict-node-type*
       (call-next-method)
       *obj*))

;*---------------------------------------------------------------------*/
;*    get-type ::jump-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (get-type node::jump-ex-it)
   (if *strict-node-type*
       (call-next-method)
       *obj*))

;*---------------------------------------------------------------------*/
;*    get-type ::make-box ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::make-box)
   (if *strict-node-type*
       (call-next-method)
       *obj*))

;*---------------------------------------------------------------------*/
;*    get-type ::box-ref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::box-ref)
   (if *strict-node-type*
       (call-next-method)
       *obj*))

;*---------------------------------------------------------------------*/
;*    get-type ::box-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::box-set!)
   (if *strict-node-type*
       (call-next-method)
       *unspec*))

;*---------------------------------------------------------------------*/
;*    get-type ::app ...                                               */
;*---------------------------------------------------------------------*/
(define-method (get-type node::app)
   (if *strict-node-type*
       (with-access::app node (type fun)
	  (if (eq? type *_*)
	      (variable-type (var-variable fun))
	      type))
       (with-access::app node (fun)
	  (variable-type (var-variable fun)))))
