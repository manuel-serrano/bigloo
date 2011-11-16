;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Foreign/jtype.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  2 10:58:27 2001                          */
;*    Last change :  Mon Nov 14 17:00:12 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The JVM array type managment. I have tried to reuse as much as   */
;*    as possible of the C foreign type machinery. This is why this    */
;*    module may seems to produce C types.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_jtype

   (include "Tools/trace.sch"
	    "Foreign/jtype.sch")

   (import  type_type
	    type_env
	    type_cache
	    type_tools
	    tools_error
	    tools_args
	    tools_misc
	    tools_location
	    tools_shape
	    module_module
	    ast_ident
	    ast_private
	    foreign_ctype
	    foreign_access
	    foreign_library)
   
   (export  (wide-class jarray::type
	       (item-type::type read-only))

	    (declare-jvm-type!::type ::symbol ::symbol ::pair)))

;*---------------------------------------------------------------------*/
;*    declare-jvm-type! ...                                            */
;*---------------------------------------------------------------------*/
(define (declare-jvm-type!::type id::symbol of::symbol src::pair)
   (trace (expand 3) "declare-jvm-type: " id " " of #\Newline)
   (let* ((pointed (use-type! of (find-location src)))
	  (pointer (type-pointed-to-by pointed))
	  (name (symbol->string id))
	  (obj->id (symbol-append 'obj-> id)))
      ;; is there already a type to `exp' we just define an alias ...
      (cond
	 ((type? pointer)
	  (declare-c-alias! id of name))
	 (else
	  ;; otherwise, we define a new type ...
	  (let* ((type (declare-subtype! id name '(obj) 'bigloo)))
	     (produce-module-clause!
	      `(type
		(coerce ,id obj () ())
		(coerce obj ,id (,(symbol-append id '?)) ())))
	     ;; we mark the relation between the two types and we return
	     (type-pointed-to-by-set! pointed type)
	     (widen!::jarray type
		(item-type pointed))
	     type)))))

;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::jarray ...                                */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::jarray who::type loc)
   (trace (expand 3) "make-ctype-accesses(cptr): " (shape what) " " (shape who)
	  #\Newline)
   (let* ((id (jarray-id what))
	  (oid (make-typed-ident 'o id))
	  (tid (make-typed-formal id))
	  (id->obj (symbol-append id '->obj))
	  (obj->id (symbol-append 'obj-> id))
	  (tobj->id (make-typed-ident obj->id id))
	  (id? (symbol-append id '?))
	  (tid? (make-typed-ident id? 'bool))
	  (id-length (symbol-append id '-length))
	  (tid-length (make-typed-ident id-length 'int))
	  (item-type (jarray-item-type what))
	  (item-type-id (type-id item-type))
	  (fitem-type-id (make-typed-formal item-type-id))
	  (oitem-type-id (make-typed-ident 'val item-type-id))
	  (make-id (symbol-append 'make- id))
	  (tmake-id (make-typed-ident make-id id))
	  (id-ref (symbol-append id '-ref))
	  (tid-ref (make-typed-ident id-ref item-type-id))
	  (id-set! (symbol-append id '-set!)))
      
      ;; the coercers
      (define (mk-id->obj)
	 `(define-inline (,id->obj ,oid)
	     ,(make-private-sexp 'cast 'object 'o)))
      
      (define (mk-obj->id)
	 `(define-inline (,tobj->id o)
	     ,(make-private-sexp 'cast id 'o)))
      
      ;; the predicate
      (define (mk-id?)
	 `(define-inline (,tid? o::obj)
	     ,(make-private-sexp 'instanceof id 'o)))
      
      ;; the length
      (define (mk-id-length)
	 `(define-inline (,tid-length ,oid)
	     ,(make-private-sexp 'vlength id item-type-id 'int
				 "" 'o)))
      
      ;; the user allocation form without initialization
      (define (mk-make-id)
	 `(define-inline (,tmake-id len::int)
	     ,(make-private-sexp 'valloc id item-type-id 'int
				 "" ""
				 #f 'len)))
      
      ;; the getter
      (define (mk-id-ref)
	 `(define-inline (,tid-ref ,oid offset::int)
	     ,(make-private-sexp 'vref id item-type-id 'int
				 "" 'o 'offset)))
      ;; the setter
      (define (mk-id-set!)
	 `(define-inline (,id-set! ,oid offset::int ,oitem-type-id)
	     ,(make-private-sexp 'vset! id item-type-id 'int
				 "" 'o 'offset 'val)))
      
      ;; we register all the foreign identifiers
      (register-foreign-access-idents! id? id-length make-id id-ref id-set!)
      
      ;; and the predicate
      (produce-module-clause! `(static
				(inline ,id->obj ,tid)
				(inline ,tobj->id ::obj)
				(inline ,tid? ::obj)
				(inline ,tid-length ,tid)
				(inline ,tmake-id ::int)
				(inline ,tid-ref ,tid ::int)
				(inline ,id-set! ,tid ::int ,fitem-type-id)))
      
      (list (mk-id->obj)
	    (mk-obj->id)
	    (mk-id?)
	    (mk-id-length)
	    (mk-make-id)
	    (mk-id-ref)
	    (mk-id-set!))))
      
