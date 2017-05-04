;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/tools.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 18 12:52:24 1996                          */
;*    Last change :  Thu May  4 07:47:17 2017 (serrano)                */
;*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
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
	    (make-class-ref ::type ::slot ::obj)
	    (make-class-set! ::type ::slot ::obj ::obj)
	    (make-direct-set! ::type ::slot obj val)
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
;*    make-class-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (make-class-ref type slot obj)
   (let* ((klass (slot-class-owner slot))
	  (widening (and (tclass? klass) (tclass-widening klass))))
      (if (not widening)
	  (make-direct-ref type slot obj)
	  (make-direct-ref (tclass-wide-type type) slot `(object-widening ,obj)))))

;*---------------------------------------------------------------------*/
;*    make-class-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (make-class-set! type slot obj val)
   (let* ((klass (slot-class-owner slot))
	  (widening (and (tclass? klass) (tclass-widening klass))))
      (if (not widening)
	  (make-direct-set! type slot obj val)
	  (make-direct-set! (tclass-wide-type type) slot `(object-widening ,obj) val))))

;*---------------------------------------------------------------------*/
;*    make-direct-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (make-direct-ref type slot obj)
   (let* ((fname (slot-name slot))
	  (tname (type-name type))
	  (fmt (format "(((~a)COBJECT($1))->~a)" tname fname)))
      (make-private-sexp 'getfield
			 (type-id (slot-type slot))
			 (type-id type)
			 fname
			 fmt
			 obj)))

;*---------------------------------------------------------------------*/
;*    make-direct-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (make-direct-set! type slot obj val)
   (let* ((fname (slot-name slot))
	  (tname (type-name type))
	  (fmt (format "((((~a)COBJECT($1))->~a)=((~a)$2),BUNSPEC)" tname fname
		       (type-name (slot-type slot)))))
      (make-private-sexp 'setfield
			 (type-id (slot-type slot))
			 (type-id type)
			 (slot-name slot)
			 fmt
			 obj val)))

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
   
