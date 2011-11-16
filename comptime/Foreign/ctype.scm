;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Foreign/ctype.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 18:57:02 1994                          */
;*    Last change :  Mon Nov 14 16:59:34 2011 (serrano)                */
;*    Copyright   :  1994-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C type managment                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_ctype

   (include "Tools/trace.sch"
	    "Foreign/ctype.sch")
   
   (import  type_type
	    type_env
	    type_cache
	    type_tools
	    tools_error
	    tools_args
	    tools_misc
	    tools_location
	    module_module
	    ast_ident
	    engine_param)
   
   (export  (wide-class calias::type
	       ;; are we computing an alias for an array ?
	       (array?::bool (default #f)))

	    (wide-class cenum::type
	       ;; a pointer to the associated Bigloo type
	       (btype::type read-only)
	       ;; the literal of the enum type
	       (literals read-only))

	    (wide-class copaque::type
	       ;; a pointer to the associated Bigloo type
	       (btype::type read-only))

	    (wide-class cfunction::type
	       ;; a pointer to the associated Bigloo type
	       (btype::type read-only)
	       ;; the arity of the function
	       (arity::long read-only)
	       ;; the result type
	       (type-res::type read-only)
	       ;; the type of the arguments
	       (type-args read-only))

	    (wide-class cptr::type
	       ;; a pointer to the associated Bigloo type
	       (btype::type read-only)
	       ;; the pointed type
	       (point-to::type read-only)
	       ;; array or pointer ?
	       (array?::bool read-only (default #t)))
	    
	    (wide-class cstruct::type
	       ;; struct or union ?
	       (struct?::bool read-only (default #t))
	       ;; fields
	       (fields read-only)
	       ;; the cstruct* associated
	       (cstruct* (default #f)))

	    (wide-class cstruct*::type
	       ;; a pointer to the associated Bigloo type
	       (btype::type read-only)
	       ;; the pointed structure
	       (cstruct::cstruct read-only))

	    (ctype?::bool ::obj)
	    (declare-c-type!::obj ::obj ::symbol ::obj ::bstring)
	    (declare-c-alias!::type id::symbol exp::symbol name::bstring)))

;*---------------------------------------------------------------------*/
;*    ctype? ...                                                       */
;*---------------------------------------------------------------------*/
(define (ctype? obj::obj)
   (or (calias? obj)
       (cenum? obj)
       (copaque? obj)
       (cfunction? obj)
       (cptr? obj)
       (cstruct? obj)
       (cstruct*? obj)))

;*---------------------------------------------------------------------*/
;*    declare-c-type! ...                                              */
;*---------------------------------------------------------------------*/
(define (declare-c-type! ct-def ct-id ct-exp ct-name)
   (cond
      ((type-exists? ct-id)
       (unless *allow-type-redefinition*
	  (warning "declare-c-type!" "Type redefinition -- " ct-id))
       #unspecified)
      ((symbol? ct-exp)
       (declare-c-alias! ct-id ct-exp ct-name))
      ((pair? ct-exp)
       (case (car ct-exp)
	  ((enum)
	   (declare-c-enum! ct-id ct-exp ct-name))
	  ((opaque)
	   (declare-c-opaque! ct-id ct-exp ct-name))
	  ((function)
	   (declare-c-function! ct-id ct-exp ct-name))
	  ((pointer array)
	   (declare-c-pointer! ct-id ct-exp ct-name))
	  ((struct union)
	   (declare-c-struct! ct-id ct-exp ct-name))
	  ((struct* union*)
	   (declare-c-struct*! ct-id ct-exp ct-name))
	  (else
	   (internal-error "declare-c-type!"
			   "Illegal c type declaration"
			   ct-def))))
      (else
       (internal-error "declare-c-type!"
		       "Illegal c type declaration"
		       ct-def))))
 
;*---------------------------------------------------------------------*/
;*    declare-c-alias! ...                                             */
;*---------------------------------------------------------------------*/
(define (declare-c-alias!::type id::symbol exp::symbol name::bstring)
   (trace (expand 3) "declare-c-alias: " id " " exp #\Newline)
   (let ((atype (get-aliased-type (find-type exp))))
      (if (eq? id (type-id atype))
	  atype
	  (let ((ctype (declare-aliastype! id name 'C atype)))
	     (widen!::calias ctype)
	     ctype))))

;*---------------------------------------------------------------------*/
;*    declare-c-enum! ...                                              */
;*---------------------------------------------------------------------*/
(define (declare-c-enum!::type id::symbol exp::pair name::bstring)
   (trace (expand 3) "declare-c-enum: " id " " exp #\Newline)
   (let* ((cobj    *cobj*)
	  (obj     *obj*)
	  (bid     (symbol-append 'b id))
	  (ctype   (declare-subtype! id name '(cobj) 'C))
	  (btype   (declare-subtype! bid (type-name obj) '(obj) 'bigloo)))
      ;; we declare the coercions
      (produce-module-clause! (make-foreign-coercers (string-sans-$ name)
						     id
						     bid))
      ;; and we return the cenum type
      (widen!::cenum ctype (btype btype) (literals (cdr exp)))
      ctype))
	  
;*---------------------------------------------------------------------*/
;*    declare-c-opaque! ...                                            */
;*---------------------------------------------------------------------*/
(define (declare-c-opaque!::type id::symbol exp::pair name::bstring)
   (trace (expand 3) "declare-c-opaque: " id " " exp #\Newline)
   (let* ((cobj    *cobj*)
	  (obj     *obj*)
	  (bid     (symbol-append 'b id))
	  (ctype   (declare-subtype! id name '(cobj) 'C))
	  (btype   (declare-subtype! bid (type-name obj) '(obj) 'bigloo)))
      ;; we declare the coercions
      (produce-module-clause! (make-foreign-coercers (string-sans-$ name)
						     id
						     bid))
      ;; and we return the cenum type
      (widen!::copaque ctype (btype btype))
      ctype))
	  
;*---------------------------------------------------------------------*/
;*    declare-c-function! ...                                          */
;*---------------------------------------------------------------------*/
(define (declare-c-function!::type id::symbol exp::pair name::bstring)
   (trace (expand 3) "declare-c-function: " id " " exp #\Newline)
   (let* ((cobj     *cobj*)
	  (obj      *obj*)
	  (bid      (symbol-append 'b id))
	  (ctype    (declare-subtype! id name '(cobj) 'C))
	  (btype    (declare-subtype! bid (type-name obj) '(obj) 'bigloo))
	  (arity    (foreign-arity (caddr exp)))
	  (tres-id  (cadr exp))
	  (targs-id (args*->args-list (caddr exp)))
	  (loc      (find-location exp)))
      ;; we declare the coercions
      (produce-module-clause! (make-foreign-coercers (string-sans-$ name)
						     id
						     bid))
      ;; and we return the cfunction type
      (widen!::cfunction ctype
	 (btype btype)
	 (arity arity)
	 (type-res (use-type! tres-id loc))
	 (type-args (map (lambda (a) (use-type! a loc)) targs-id)))
      ctype))

;*---------------------------------------------------------------------*/
;*    declare-c-pointer! ...                                           */
;*---------------------------------------------------------------------*/
(define (declare-c-pointer!::type id::symbol exp::pair name::bstring)
   (trace (expand 3) "declare-c-ptr: " id " " exp #\Newline)
   (let* ((pointed (find-type (cadr exp)))
	  (pointer (type-pointed-to-by pointed)))
      ;; is there already a type to `exp' we just define an alias ...
      (cond
	 ((type? pointer)
	  (let ((alias (declare-c-alias! id
					 (type-id pointer)
					 (make-pointer-to-name pointed))))
	     (if (and (eq? (car exp) 'array) (not (cptr-array? pointer)))
		 (begin
		    (type-size-set! alias (string-sans-$ name))
		    (calias-array?-set! alias #t)))
	     alias))
	 (else
	  ;; otherwise, we define a new type ...
	  (let* ((cobj     *cobj*)
		 (obj      *obj*)
		 (nobj     (type-name obj))
		 (bid      (symbol-append 'b id))
		 (tname    (make-pointer-to-name pointed))
		 (ctype    (declare-subtype! id tname '(cobj) 'C))
		 (btype    (declare-subtype! bid nobj '(obj) 'bigloo)))
	     ;; we set the sizeof field
	     (type-size-set! ctype (string-sans-$ name))
	     ;; we declare the coercions
	     (produce-module-clause! (make-foreign-coercers
				      (string-sans-$ name)
				      id
				      bid))
	     ;; we mark the relation between the two types and we return
	     (type-pointed-to-by-set! pointed ctype)
	     (widen!::cptr ctype
		(btype btype)
		(point-to pointed)
		(array? (eq? (car exp) 'array)))
	     ctype)))))
   
;*---------------------------------------------------------------------*/
;*    declare-c-struct! ...                                            */
;*---------------------------------------------------------------------*/
(define (declare-c-struct!::type id::symbol exp::pair name::bstring)
   (trace (expand 3) "declare-c-struct: " id " " exp #\Newline)
   (let* ((cobj  *cobj*)
	  (obj   *obj*)
	  (bid   (symbol-append 'b id))
	  (ctype (declare-subtype! id name '(cobj) 'C)))
      ;; we set the sizeof field
      (type-size-set! ctype name)
      (widen!::cstruct ctype
	 (struct? (eq? (car exp) 'struct))
	 (fields (cdr exp)))
      ;; we automatically declare a type `pointer to' the structure
      (let ((ptr-id (symbol-append id '*)))
	 (declare-c-struct*! ptr-id
			     `(,(car exp) ,id)
			     (make-pointer-to-name ctype)))))

;*---------------------------------------------------------------------*/
;*    declare-c-struct*! ...                                           */
;*---------------------------------------------------------------------*/
(define (declare-c-struct*!::type id::symbol exp::pair name::bstring)
   (trace (expand 3) "declare-c-struct*: " id " " exp " size: " #\Newline)
   (let* ((struct  (find-type (cadr exp)))
	  (struct* (cstruct-cstruct* struct)))
      ;; is there is already a type to `exp' we just define an alias ...
      (cond
	 ((cstruct*? struct*)
	  (declare-c-alias! id (type-id struct*) name))
	 (else
	  (let* ((cobj  *cobj*)
		 (obj   *obj*)
		 (nobj  (type-name obj))
		 (bid   (symbol-append 'b id))
		 (ctype (declare-subtype! id name '(cobj) 'C))
		 (btype (declare-subtype! bid nobj '(obj) 'bigloo)))
	     ;; we set the sizeof field
	     (type-size-set! ctype
			     (string-append "*((" (string-sans-$ name) ") 0)"))
	     ;; we declare the coercions
	     (produce-module-clause! (make-foreign-coercers
				      (string-sans-$ name)
				      id
				      bid))
	     ;; we mark the relation between the two types and we return
	     (cstruct-cstruct*-set! struct ctype)
	     ;; and now we are done
	     (widen!::cstruct* ctype
		(btype btype)
		(cstruct struct))
	     ctype)))))

;*---------------------------------------------------------------------*/
;*    make-foreign-coercers ...                                        */
;*---------------------------------------------------------------------*/
(define (make-foreign-coercers name id bid)
   (let ((id?      (symbol-append id '?))
	 (id->bid  (symbol-append id '-> bid))
	 (bid->id  (symbol-append bid '-> id)))
      (let ((res `(type
		   (coerce ,id cobj () ())
		   (coerce cobj ,id () ())
		   (coerce ,bid foreign () ())
		   (coerce foreign ,bid (,id?) ())
		   (coerce ,id ,bid
			   () ((lambda (,(make-typed-ident 'x id))
				  (,id->bid ',bid x)))) 
		   (coerce ,id foreign
			   () ((lambda (,(make-typed-ident 'x id))
				  (,id->bid ',bid x))))
		   (coerce ,bid    ,id () (,bid->id))
		   (coerce foreign ,id (,id?) (,bid->id)))) )
	 (trace (expand 3) "make-foreign-coercers: " res #\Newline)
	 res)))
