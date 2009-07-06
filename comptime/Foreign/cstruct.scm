;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Foreign/cstruct.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  6 12:23:13 1996                          */
;*    Last change :  Mon Jul 17 11:15:43 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C ptr accessors creations                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_cstruct
   (include "Tools/trace.sch")
   (import  type_tools
	    type_type
	    type_tools
	    type_env
	    tools_shape
	    tools_misc
	    foreign_ctype
	    foreign_access
	    foreign_library
	    module_module
	    ast_ident))
   
;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::cstruct ...                               */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::cstruct who::type loc)
   '())

;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::cstruct* ...                              */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::cstruct* who::type loc)
   (trace (expand 3) "make-ctype-accesses(cstruct*): " (shape what)
	  " " (shape who) #\Newline)
   (let* ((btype          (cstruct*-btype what))
	  (id             (type-id who))
	  (wid            (type-id what))
	  (bid            (type-id btype))
	  (id->bid        (symbol-append id '-> bid))
	  (bid->id        (symbol-append bid '-> id))
	  (bid?           (symbol-append id '?))
	  (bid?-bool      (symbol-append bid? '::bool))
	  (name           (type-name who))
	  (name-sans-$    (string-sans-$ name))
	  (cstruct        (cstruct*-cstruct what))
	  (cstruct-fields (cstruct-fields cstruct))
	  (sizeof         (string-append "*((" name-sans-$ ") 0)")))
      
      ;; the two conversion allocation fonctions (they are not
      ;; simple coercion because the first one allocate and the
      ;; second one destructurate).
      (define (mk-id->bid)
	 `(macro ,bid ,id->bid (symbol ,id) "cobj_to_foreign"))
      
      (define (mk-bid->id)
	 (let ((mname (string-append "(" name-sans-$ ")FOREIGN_TO_COBJ")))
	    `(macro ,id ,bid->id (,bid) ,mname)))
      
      ;; the predicate
      (define (mk-bid?)
	 (make-define-inline
	  `(,bid?-bool o::obj)
	  `(if (foreign? o)
	       (eq? (foreign-id o) ',bid)
	       #f)))
      
      ;; equality (using ==)
      (define (mk-=id)
	 (make-define-inline
	  `(,(symbol-append '= id '?::bool)
	    ,(make-typed-ident 'o1 id)
	    ,(make-typed-ident 'o2 id))
	  '(pragma::bool "($1 == $2)" o1 o2)))
      
      ;; id-null?
      (define (mk-id-null?)
	 (make-define-inline
	  `(,(symbol-append id '-null?::bool)
	    ,(make-typed-ident 'o id))
	  `(pragma::bool ,(string-append "($1 == (" name-sans-$ ")0L)")
			 o)))
      
      ;; make-id-null
      (define (mk-id-null)
	 (make-define-inline
	  `(,(make-typed-ident (symbol-append 'make-null- id) id))
	  `(,(symbol-append 'pragma:: id)
	    ,(string-append "((" name-sans-$ ")0L)"))))
      
      ;; the user allocation form without initialization
      (define (mk-make-id)
	 (make-define-inline
	  `(,(make-typed-ident (symbol-append 'make- id) id))
	  `(,(make-typed-ident 'pragma id)
	    ,(string-append "(" name-sans-$ ")GC_MALLOC( sizeof( "
			    sizeof
			    " ) )"))))
      
      ;; the user allocation form with initialization
      (define (mk-id)
	 (let ((formals-typed (map (lambda (f)
				      (let* ((f-id      (cadr f))
					     (f-type-id (car f))
					     (f-type    (use-type! f-type-id
								   loc))
					     (af-type   (get-aliased-type
							 f-type)))
					 (if (cstruct? af-type)
					     (make-typed-ident
					      f-id
					      (symbol-append f-type-id '*))
					     (make-typed-ident f-id
							       f-type-id))))
				   cstruct-fields))
	       (new           (gensym 'new)))
	    (make-define-inline
	     `(,(make-typed-ident id id) ,@formals-typed)
	     `(let ((,(make-typed-ident new id)
		     (,(symbol-append 'make- id))))
		 ,@(map (lambda (f)
			   (let ((f-id (cadr f)))
			      `(,(symbol-append id '- f-id '-set!) ,new
								   ,f-id)))
			cstruct-fields)
		 ,new))))
      
      ;; getter and setter
      (define (getter-&-setter field)
	 (let* ((f-type-id       (car field))
		(f-type          (use-type! f-type-id loc))
		(af-type         (get-aliased-type f-type))
		(f-id            (cadr field))
		(f-name          (caddr field))
		(get-name        (symbol-append id '- f-id))
		(set-name        (symbol-append id '- f-id '-set!))
		(get-type-id     (if (cstruct? af-type)
				     (symbol-append f-type-id '*)
				     f-type-id))
		(struct-ref-fmt  (if (cstruct? af-type)
				     (string-append "&((((" name-sans-$
						    ")$1)->" f-name "))")
				     (string-append "(((" name-sans-$
						    ")$1)->" f-name ")")))
		(struct-set-fmt  (string-append "((((" name-sans-$
						")$1)->" f-name ")"))
		(struct-setv-fmt (if (cstruct? af-type)
				     " = (*($2)), BUNSPEC)"
				     " = ($2), BUNSPEC)")))
	    (register-foreign-access-idents! get-name set-name)
	    (list
	     (make-define-inline `(,(make-typed-ident get-name get-type-id)
				   ,(make-typed-ident 'o id))
				 `(,(make-typed-ident 'pragma get-type-id)
				   ,struct-ref-fmt o))
	     (make-define-inline `(,(symbol-append set-name '::obj)
				   ,(make-typed-ident 'o id)
				   ,(make-typed-ident 'v get-type-id))
				 `(pragma
				   ,(string-append struct-set-fmt struct-setv-fmt)
				   o
				   v)))))
      
      ;; we register all the foreign identifiers
      (register-foreign-access-idents!
       (symbol-append 'make- id)
       id
       (symbol-append '= id '?)
       bid?
       (symbol-append id '-null?)
       (symbol-append 'make-null- id))
      
      ;; getters and setters
      (define (getters-&-setters)
	 (let loop ((fields cstruct-fields)
		    (res   '()))
	    (if (null? fields)
		res
		(loop (cdr fields)
		      (append (getter-&-setter (car fields)) res)))))
      
      ;; we declare the coercion operations ...
      (produce-module-clause! `(foreign ,(mk-id->bid) ,(mk-bid->id)))
      ;; and the predicate
      (produce-module-clause! `(static
				,(make-proto-inline `(,bid?-bool ::obj))))
      (produce-module-clause! `(pragma (,bid? (predicate-of ,wid))))
      
      (cons* (mk-make-id)
	     (mk-id)
	     (mk-=id)
	     (mk-id-null?)
	     (mk-id-null)
	     (mk-bid?)
	     (getters-&-setters))))
      



