;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/typeof.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:25:05 1996                          */
;*    Last change :  Mon Nov 29 05:47:36 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
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
	    engine_param
	    ast_node
	    ast_var
	    tools_shape
	    tools_speek
	    object_class)
   (export  (get-type-atom::type  <atom>)
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
;*    get-type ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (get-type::type node::node))

;*---------------------------------------------------------------------*/
;*    get-type ::atom ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::atom)
   (with-access::atom node (value)
      (get-type-atom value)))
 
;*---------------------------------------------------------------------*/
;*    get-type ::kwote ...                                             */
;*---------------------------------------------------------------------*/
(define-method (get-type node::kwote)
   (with-access::kwote node (value)
      (get-type-kwote value)))

;*---------------------------------------------------------------------*/
;*    get-type ::var ...                                               */
;*    -------------------------------------------------------------    */
;*    Computing the value of a variable reference is complex. It used  */
;*    to consist in getting the variable type. Since the dataflow      */
;*    analysis has been added, this is more complex because the type   */
;*    of a variable reference depends on its context. For instance     */
;*    under the then branch of a type predicat the reference is of     */
;*    a more precise type that the variable itself.                    */
;*                                                                     */
;*    To make the thing even more complex, the type of the reference   */
;*    might be not correct with respect to boxing/unboxing invariant.  */
;*    For instance, after an expression such as (set! x 1), the        */
;*    variable X will be considered as ::int. However is the global    */
;*    analysis fails to unbox X types it as ::obj, then the expression */
;*    must be compiled as  X = BINT( 1 ) and not X = 1. The predicate  */
;*    TYPE-MORE-SPECIFIC? deals with these cases.                      */
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
      (let ((value (variable-value variable)))
	 (cond
	    ((sfun? value)
	     *procedure*)
	    ((cfun? value)
	     *obj*)
	    (else
	     (if (and *optim-dataflow-types?*
		      (type-more-specific? type (variable-type variable)))
		 (begin
		    (verbose-type type (variable-type variable))
		    type)
		 (variable-type variable)))))))

;*---------------------------------------------------------------------*/
;*    get-type ::closure ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::closure)
   *procedure*)

;*---------------------------------------------------------------------*/
;*    get-type ::sequence ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::sequence)
   (with-access::sequence node (nodes)
      (get-type (car (last-pair nodes)))))

;*---------------------------------------------------------------------*/
;*    get-type ::extern ...                                            */
;*---------------------------------------------------------------------*/
(define-method (get-type node::extern)
   (with-access::extern node (type)
      type))

;*---------------------------------------------------------------------*/
;*    get-type ::cast ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::cast)
   (with-access::cast node (type)
      type))

;*---------------------------------------------------------------------*/
;*    get-type ::setq ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::setq)
   *unspec*)

;*---------------------------------------------------------------------*/
;*    get-type ::conditional ...                                       */
;*---------------------------------------------------------------------*/
(define-method (get-type node::conditional)
   (with-access::conditional node (test true false)
      (let ((ttrue (get-type true))
	    (tfalse (get-type false)))
	 (cond ((or (eq? ttrue tfalse) (eq? tfalse *magic*)) ttrue)
	       ((eq? ttrue *magic*) tfalse)
	       (else *obj*)))))

;*---------------------------------------------------------------------*/
;*    get-type ::fail ...                                              */
;*---------------------------------------------------------------------*/
(define-method (get-type node::fail)
   *magic*)

;*---------------------------------------------------------------------*/
;*    get-type ::select ...                                            */
;*---------------------------------------------------------------------*/
(define-method (get-type node::select)
   (with-access::select node (clauses test)
      (let loop ((clauses (cdr clauses))
		 (type    (get-type (cdr (car clauses)))))
	 (if (null? clauses)
	     type
	     (let ((ntype (get-type (cdr (car clauses)))))
		(cond
		   ((eq? type *magic*)
		    (loop (cdr clauses) ntype) )
		   ((or (eq? ntype type) (eq? ntype *magic*))
		    (loop (cdr clauses) type))
		   (else *obj*)))))))

;*---------------------------------------------------------------------*/
;*    get-type ::let-fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::let-fun)
   (with-access::let-fun node (body)
      (get-type body)))

;*---------------------------------------------------------------------*/
;*    get-type ::let-var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::let-var)
   (with-access::let-var node (body)
      (get-type body)))
 
;*---------------------------------------------------------------------*/
;*    get-type ::set-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (get-type node::set-ex-it)
   *obj*)

;*---------------------------------------------------------------------*/
;*    get-type ::jump-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (get-type node::jump-ex-it)
   *obj*)

;*---------------------------------------------------------------------*/
;*    get-type ::make-box ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::make-box)
   *obj*)

;*---------------------------------------------------------------------*/
;*    get-type ::box-ref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::box-ref)
   *obj*)

;*---------------------------------------------------------------------*/
;*    get-type ::box-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (get-type node::box-set!)
   *unspec*)

;*---------------------------------------------------------------------*/
;*    get-type ::app-ly ...                                            */
;*---------------------------------------------------------------------*/
(define-method (get-type node::app-ly)
   *obj*)

;*---------------------------------------------------------------------*/
;*    get-type ::funcall ...                                           */
;*---------------------------------------------------------------------*/
(define-method (get-type node::funcall)
   *obj*)

;*---------------------------------------------------------------------*/
;*    get-type ::app ...                                               */
;*---------------------------------------------------------------------*/
(define-method (get-type node::app)
   (with-access::app node (fun)
      (variable-type (var-variable fun))))
