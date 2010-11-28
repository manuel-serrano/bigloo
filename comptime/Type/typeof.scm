;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/typeof.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:25:05 1996                          */
;*    Last change :  Sun Nov 28 07:11:38 2010 (serrano)                */
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
	    ast_node
	    ast_var
	    tools_shape)
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
;*---------------------------------------------------------------------*/
(define-method (get-type node::var)
   (with-access::var node (variable)
      (let ((value (variable-value variable)))
	 (cond
	    ((sfun? value)
	     *procedure*)
	    ((cfun? value)
	     *obj*)
	    (else
	     (unless (eq? (variable-type variable) (node-type node))
		(tprint "get-type: " (shape node) " vtype="
			(shape (variable-type variable))
			" ntype=" (shape (node-type node))))
	     (variable-type variable))))))

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
