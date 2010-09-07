;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/tools.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 18 12:52:24 1996                          */
;*    Last change :  Tue Sep  7 08:53:49 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Some tools for builing the class accessors                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_tools
   (import  tools_misc
	    backend_backend
	    type_type
	    type_env
	    type_cache
	    type_tools
	    ast_var
	    ast_node
	    ast_private
	    ast_ident
	    object_class
	    object_slots
	    engine_param
	    tools_error)
   (export  (class->obj-id::symbol ::symbol)
	    (obj->class-id::symbol ::symbol)
	    (class?-id::symbol ::symbol)
	    (class->super-id::symbol ::symbol ::symbol)
	    (super->class-id::symbol ::symbol ::symbol)
	    (make-pragma-direct-set! ::type ::slot obj val)
	    (make-pragma-direct-ref/widening ::type ::slot obj obj)
	    (make-pragma-direct-set!/widening ::type ::slot obj val w)
	    (make-pragma-indexed-ref/widening ::type ::slot obj index ::obj)
	    (make-pragma-indexed-init-set! ::type ::slot obj val)
	    (make-pragma-indexed-set! ::type ::slot obj val index)
	    (make-pragma-indexed-set!/widening ::type ::slot obj val index ::obj)))

;*---------------------------------------------------------------------*/
;*    class->obj-id ...                                                */
;*---------------------------------------------------------------------*/
(define (class->obj-id id)
   (symbol-append id '->obj))
	   
;*---------------------------------------------------------------------*/
;*    obj->class-id ...                                                */
;*---------------------------------------------------------------------*/
(define (obj->class-id id)
   (symbol-append 'obj-> id))

;*---------------------------------------------------------------------*/
;*    class?-id ...                                                    */
;*---------------------------------------------------------------------*/
(define (class?-id id)
   (symbol-append id '?))

;*---------------------------------------------------------------------*/
;*    class->super-id ...                                              */
;*---------------------------------------------------------------------*/
(define (class->super-id class super)
   (symbol-append class '-> super))

;*---------------------------------------------------------------------*/
;*    super->class-id ...                                              */
;*---------------------------------------------------------------------*/
(define (super->class-id super class)
   (symbol-append super '-> class))

;*---------------------------------------------------------------------*/
;*    make-pragma-direct-ref ...                                       */
;*---------------------------------------------------------------------*/
(define (make-pragma-direct-ref type slot obj)
   (if (not (backend-pragma-support (the-backend)))
       (make-private-sexp 'getfield
			  (type-id (slot-type slot))
			  (type-id (real-slot-class-owner slot))
			  (slot-name slot) obj)
       `(,(make-typed-ident 'free-pragma/effect (type-id (slot-type slot)))
	 (effect (read (,(symbol-append (type-id type) '- (slot-id slot)))))
	 ,(string-append "(((" (type-class-name type) ")CREF($1))->"
			 (slot-name slot) ")")
	 ,obj)))

;*---------------------------------------------------------------------*/
;*    make-pragma-direct-ref/widening ...                              */
;*---------------------------------------------------------------------*/
(define (make-pragma-direct-ref/widening type slot obj widening)
   (if (not widening)
       (make-pragma-direct-ref type slot obj)
       (make-pragma-direct-ref type slot `(object-widening ,obj))))

;*---------------------------------------------------------------------*/
;*    make-pragma-direct-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (make-pragma-direct-set! type slot obj val)
   (if (not (backend-pragma-support (the-backend)))
       (make-private-sexp 'setfield
			  (type-id (slot-type slot))
			  (type-id (real-slot-class-owner slot))
			  (slot-name slot) obj val)
       `(pragma/effect::obj
	 (effect (write (,(symbol-append (type-id type) '- (slot-id slot)))))
	 ,(string-append "((((" (type-class-name type) ")CREF($1))->" (slot-name slot)
			 ") = ((" (type-class-name (slot-type slot)) ")$2), BUNSPEC)")
	 ,obj
	 ,val)))

;*---------------------------------------------------------------------*/
;*    make-pragma-direct-set!/widening ...                             */
;*---------------------------------------------------------------------*/
(define (make-pragma-direct-set!/widening type slot obj val widening)
   (if (not widening)
       (make-pragma-direct-set! type slot obj val)
       (make-pragma-direct-set! type slot `(object-widening ,obj) val)))
   
;*---------------------------------------------------------------------*/
;*    make-pragma-indexed-ref ...                                      */
;*---------------------------------------------------------------------*/
(define (make-pragma-indexed-ref type slot obj index)
   (define (pragma-index)
      `(,(make-typed-ident 'free-pragma (type-id (slot-type slot)))
	,(string-append "(((" (type-class-name type) ")CREF($1))->"
			(slot-name slot)
			")[ $2 ]")
	,obj
	,index))
   (define (nopragma-index)
      (with-access::slot slot (name indexed class-owner)
	 (let* ((ftype-id (type-id indexed))
		(field (make-private-sexp 'getfield
					  ftype-id
					  (type-id class-owner)
					  name
					  obj)))
	    (make-private-sexp 'vref-ur
			       ftype-id
			       (type-id (slot-type slot))
			       (type-id *int*)
			       ""
			       field
			       index))))
   (if (backend-pragma-support (the-backend))
       (pragma-index)
       (nopragma-index)))

;*---------------------------------------------------------------------*/
;*    make-pragma-indexed-ref/widening ...                             */
;*---------------------------------------------------------------------*/
(define (make-pragma-indexed-ref/widening type slot obj index widening)
   (if (not widening)
       (make-pragma-indexed-ref type slot obj index)
       (make-pragma-indexed-ref type slot `(object-widening ,obj) index)))

;*---------------------------------------------------------------------*/
;*    c-allocate-indexed-slot ...                                      */
;*---------------------------------------------------------------------*/
(define (c-allocate-indexed-slot type len)
   (let ((tid  (type-id type))
	 (tname (string-sans-$ (type-class-name type)))
	 (sizeof (if (string? (type-size type))
		     (type-size type)
		     (type-class-name type))))
      `(,(make-typed-ident 'free-pragma tid)
	,(string-append "GC_MALLOC( sizeof(" sizeof ") * $1 )") ,len)))

;*---------------------------------------------------------------------*/
;*    jvm-allocate-indexed-slot ...                                    */
;*---------------------------------------------------------------------*/
(define (jvm-allocate-indexed-slot vtype-id ftype-id len)
   (make-private-sexp 'valloc vtype-id ftype-id (type-id *int*) "" "" #f len))
   
;*---------------------------------------------------------------------*/
;*    make-pragma-indexed-init-set! ...                                */
;*---------------------------------------------------------------------*/
(define (make-pragma-indexed-init-set! type slot obj len)
   (define (pragma-index)
      `(pragma::obj
	,(string-append "((((" (type-class-name type) ")CREF($1))->"
			(slot-name slot)
			") = ((" (type-class-name (slot-type slot))
			" *)$2), BUNSPEC)")
	,obj
	,(c-allocate-indexed-slot type len)))
   (define (nopragma-index)
      (let ((ftype (type-id (slot-indexed slot)))
	    (otype (slot-type slot))
	    (name (slot-name slot)))
	 (make-private-sexp 'setfield
			    ftype
			    (type-id (real-slot-class-owner slot))
			    name
			    obj
			    (jvm-allocate-indexed-slot ftype
						       (type-id otype)
						       len))))
   (if (backend-pragma-support (the-backend))
       (pragma-index)
       (nopragma-index)))

;*---------------------------------------------------------------------*/
;*    make-pragma-indexed-set! ...                                     */
;*---------------------------------------------------------------------*/
(define (make-pragma-indexed-set! type slot obj val index)
   (define (pragma-index)
      `(pragma::obj
	,(string-append "((((" (type-class-name type) ")CREF($1))->"
			(slot-name slot)
			")[ $2 ] = ((" (type-class-name (slot-type slot))
			")$3), BUNSPEC)")
	,obj
	,index
	,val))
   (define (nopragma-index)
      (let* ((ftype-id (type-id (slot-indexed slot)))
	     (gfield (make-private-sexp 'getfield
					ftype-id
					(type-id (real-slot-class-owner slot))
					(slot-name slot)
					obj)))
	 (make-private-sexp 'vset-ur!
			    ftype-id
			    (type-id (slot-type slot))
			    (type-id *int*)
			    ""
			    gfield
			    index
			    val)))
   (if (backend-pragma-support (the-backend))
       (pragma-index)
       (nopragma-index)))

;*---------------------------------------------------------------------*/
;*    real-slot-class-owner ...                                        */
;*---------------------------------------------------------------------*/
(define (real-slot-class-owner slot)
   (let ((t (slot-class-owner slot)))
      (if (and *saw* (wide-class? t))
	  (find-type (saw-wide-class-id (tclass-id t)))
	  t)))

;*---------------------------------------------------------------------*/
;*    make-pragma-indexed-set!/widening ...                            */
;*---------------------------------------------------------------------*/
(define (make-pragma-indexed-set!/widening type slot obj val index widening)
   (if (not widening)
       (make-pragma-indexed-set! type slot obj val index)
       (make-pragma-indexed-set! type slot `(object-widening ,obj) val index)))
