;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/tools.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 18 12:52:24 1996                          */
;*    Last change :  Thu Nov  3 11:08:16 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
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
	    (make-direct-set! ::type ::slot obj val)
	    (make-direct-ref/widening ::type ::slot obj obj)
	    (make-direct-set!/widening ::type ::slot obj val w)
	    (make-indexed-ref/widening ::type ::slot obj index ::obj)
	    (make-indexed-init-set! ::type ::slot obj val)
	    (make-indexed-set! ::type ::slot obj val index)
	    (make-indexed-set!/widening ::type ::slot obj val index ::obj)
	    (real-slot-class-owner::type ::slot)
	    (find-class-slot::obj ::type ::symbol)))

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
;*    make-direct-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (make-direct-ref type slot obj)
   (let* ((otype (real-slot-class-owner slot))
	  (fname (slot-name slot))
	  (tname (type-name otype))
	  (fmt (format "(((~a)CREF($1))->~a)" tname fname)))
      (make-private-sexp 'getfield
			 (type-id (slot-type slot))
			 (type-id otype)
			 fname
			 fmt
			 obj)))

;*---------------------------------------------------------------------*/
;*    make-direct-ref/widening ...                                     */
;*---------------------------------------------------------------------*/
(define (make-direct-ref/widening type slot obj widening)
   (if (not widening)
       (make-direct-ref type slot obj)
       (make-direct-ref type slot `(object-widening ,obj))))

;*---------------------------------------------------------------------*/
;*    make-direct-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (make-direct-set! type slot obj val)
   (let* ((otype (real-slot-class-owner slot))
	  (fname (slot-name slot))
	  (tname (type-name otype))
	  (fmt (format "((((~a)CREF($1))->~a)=((~a)$2),BUNSPEC)" tname fname
		       (type-name (slot-type slot)))))
      (make-private-sexp 'setfield
			 (type-id (slot-type slot))
			 (type-id otype)
			 (slot-name slot)
			 fmt
			 obj val)))

;*---------------------------------------------------------------------*/
;*    make-direct-set!/widening ...                                    */
;*---------------------------------------------------------------------*/
(define (make-direct-set!/widening type slot obj val widening)
   (if (not widening)
       (make-direct-set! type slot obj val)
       (make-direct-set! type slot `(object-widening ,obj) val)))
   
;*---------------------------------------------------------------------*/
;*    make-indexed-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (make-indexed-ref type slot obj index)
   (with-access::slot slot (name indexed class-owner)
      (let* ((ftype-id (type-id indexed))
	     (otype (real-slot-class-owner slot))
	     (tname (type-name otype))
	     (fmt (string-append "(((" tname ")CREF($1))->" name ")"))
	     (field (make-private-sexp 'getfield
				       ftype-id
				       (type-id otype)
				       name
				       fmt
				       obj)))
	 (make-private-sexp 'vref-ur
			    ftype-id
			    (type-id (slot-type slot))
			    (type-id *int*)
			    "($1)[$2]"
			    field
			    index))))

;*---------------------------------------------------------------------*/
;*    make-indexed-ref/widening ...                                    */
;*---------------------------------------------------------------------*/
(define (make-indexed-ref/widening type slot obj index widening)
   (if (not widening)
       (make-indexed-ref type slot obj index)
       (make-indexed-ref type slot `(object-widening ,obj) index)))
   
;*---------------------------------------------------------------------*/
;*    make-indexed-init-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (make-indexed-init-set! type slot obj len)
   
   (define (allocate-indexed-slot type vtype-id ftype-id len)
      (let* ((tname (string-sans-$ (type-class-name type)))
	     (sizeof (if (string? (type-size type))
			 (type-size type)
			 (type-class-name type)))
	     (fmt (format "(sizeof(~a)*$1)" sizeof)))
	 (make-private-sexp 'valloc vtype-id ftype-id (type-id *int*)
			    (string-append "GC_MALLOC" fmt)
			    (string-append "alloca" fmt)
			    #f len)))
   
   (let* ((ftype (type-id (slot-indexed slot)))
	  (otype (real-slot-class-owner slot))
	  (stype (slot-type slot))
	  (tname (type-name otype))
	  (name (slot-name slot))
	  (fmt (format "((((~a)CREF($1))->~a)=((~a *)$2), BUNSPEC)"
		       tname
		       (slot-name slot)
		       (type-name (slot-type slot)))))
      (make-private-sexp 'setfield
			 ftype
			 (type-id (real-slot-class-owner slot))
			 name
			 fmt
			 obj
			 (allocate-indexed-slot type ftype (type-id stype) len))))

;*---------------------------------------------------------------------*/
;*    make-indexed-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (make-indexed-set! type slot obj val index)
   (let* ((ftype-id (type-id (slot-indexed slot)))
	  (otype (real-slot-class-owner slot))
	  (tname (type-name otype))
	  (name (slot-name slot))
	  (fmt (string-append "(((" tname ")CREF($1))->" name ")"))
	  (gfield (make-private-sexp 'getfield
				     ftype-id
				     (type-id otype)
				     name
				     fmt
				     obj)))
      (make-private-sexp 'vset-ur!
			 ftype-id
			 (type-id (slot-type slot))
			 (type-id *int*)
			 "($1[$2]=($3),BUNSPEC)"
			 gfield
			 index
			 val)))

;*---------------------------------------------------------------------*/
;*    real-slot-class-owner ...                                        */
;*---------------------------------------------------------------------*/
(define (real-slot-class-owner slot)
   (let ((t (slot-class-owner slot)))
      (if (and *saw* (wide-class? t))
	  (find-type (saw-wide-class-id (tclass-id t)))
	  t)))

;*---------------------------------------------------------------------*/
;*    make-indexed-set!/widening ...                                   */
;*---------------------------------------------------------------------*/
(define (make-indexed-set!/widening type slot obj val index widening)
   (if (not widening)
       (make-indexed-set! type slot obj val index)
       (make-indexed-set! type slot `(object-widening ,obj) val index)))

;*---------------------------------------------------------------------*/
;*    find-class-slot ...                                              */
;*---------------------------------------------------------------------*/
(define (find-class-slot klass id)
   (let loop ((slots (tclass-slots klass)))
      (cond
	 ((null? slots)
	  (when (tclass-widening klass)
	     (find-class-slot (tclass-its-super klass) id)))
	 ((eq? (slot-id (car slots)) id)
	  (car slots))
	 (else
	  (loop (cdr slots))))))
