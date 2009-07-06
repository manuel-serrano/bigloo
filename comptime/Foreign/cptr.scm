;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Foreign/cptr.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  6 12:23:13 1996                          */
;*    Last change :  Wed Feb  4 15:36:02 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C ptr accessors creations                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_cpointer
   (include "Tools/trace.sch")
   (import  type_tools
	    type_type
	    tools_shape
	    tools_misc
	    engine_param
	    foreign_ctype
	    foreign_access
	    foreign_library
	    module_module
	    ast_ident))
   
;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::cptr ...                                  */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::cptr who::type loc)
   (trace (expand 3) "make-ctype-accesses(cptr): " (shape what) " " (shape who)
	  #\Newline)
   (let* ((btype                (cptr-btype what))
	  (id                   (type-id who))
	  (wid                  (type-id what))
	  (bid                  (type-id btype))
	  (id->bid              (symbol-append id '-> bid))
	  (bid->id              (symbol-append bid '-> id))
	  (bid?                 (symbol-append id '?))
	  (bid?-bool            (symbol-append bid? '::bool))
	  (name                 (type-name who))
	  (name-sans-$          (string-sans-$ name))
	  (point-to             (cptr-point-to what))
	  (point-to-id          (type-id point-to))
	  (point-to-name        (type-name point-to))
	  (point-to-name-sans-$ (string-sans-$ point-to-name)))
      
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
	 (make-define-inline `(,bid?-bool o::obj)
			     `(if (foreign? o)
				  (eq? (foreign-id o) ',bid)
				  #f)))

      ;; equality (using ==)
      (define (mk-=id)
	 (make-define-inline `(,(symbol-append '= id '?::bool)
			      ,(make-typed-ident 'o1 id)
			      ,(make-typed-ident 'o2 id))
	     '(free-pragma::bool "($1 == $2)" o1 o2)))

      ;; null-test
      (define (mk-id-null?)
	 (make-define-inline `(,(symbol-append id '-null?::bool)
			       ,(make-typed-ident 'o id))
			     `(free-pragma::bool
			       ,(string-append "($1 == (" name-sans-$ ")0L)") o)))

      ;; null creator
      (define (mk-id-null)
	 (make-define-inline
	  `(,(make-typed-ident (symbol-append 'make-null- id) id))
	  `(,(symbol-append 'free-pragma:: id)
	    ,(string-append "((" name-sans-$ ")0L)"))))
      
      ;; the user allocation form without initialization
      (define (make-make-id)
	 (define (make-cptr-make-id)
	    (make-define-inline
	     `(,(make-typed-ident (symbol-append 'make- id) id) len::long)
	     `(,(make-typed-ident 'free-pragma id)
	       ,(string-append "(" name-sans-$ ")GC_MALLOC( "
			       "sizeof( " point-to-name-sans-$ " )"
			       " * $1 )")
	       len)))
	 (define (make-carray-make-id)
	    (make-define-inline
	     `(,(make-typed-ident (symbol-append 'make- id) id))
	     `(,(make-typed-ident 'free-pragma id)
	       ,(string-append "(" name-sans-$ ")GC_MALLOC( "
			       "sizeof( " (type-size who) " ))"))))
	 (if (or (and (cptr-array? what) (eq? who what))
		 (and (calias? who) (calias-array? who)))
	     (make-carray-make-id) 
	     (make-cptr-make-id)))

      ;; the getter and setter
      (define (getter-&-setter)
	 (let ((ref-id      (symbol-append id '-ref))
	       (set-id      (symbol-append id '-set!))
	       (ref-type-id (if (cstruct? point-to)
				(symbol-append point-to-id '*)
				point-to-id))
	       (ref-fmt     (if (cstruct? point-to)
				(string-append "&(((" name-sans-$
					       ")($1))[ $2 ])")
				(string-append "((" name-sans-$
					       ")($1))[ $2 ]")))
	       (set-fmt     (if (cstruct? point-to)
				(string-append
				 "(((" name-sans-$
				 ")($1))[ $2 ] = *($3), BUNSPEC)")
				(string-append
				 "(((" name-sans-$
				 ")($1))[ $2 ] = $3, BUNSPEC)"))))
	    (list
	     (make-define-inline
	      `(,(make-typed-ident (symbol-append ref-id) ref-type-id)
		,(make-typed-ident 'o id)
		i::long)
	      `(,(make-typed-ident (symbol-append 'free-pragma) ref-type-id)
		,ref-fmt
		o
		i))
	     (make-define-inline
	      `(,(symbol-append set-id '::obj)
		,(make-typed-ident 'o id)
		i::long
		,(make-typed-ident 'v ref-type-id))
	      `(pragma
		,set-fmt
		o
		i
		v)))))

      ;; we register all the foreign identifiers
      (register-foreign-access-idents!
       bid?
       (symbol-append '= id '?)
       (symbol-append 'make-null- id)
       (symbol-append id '-null?)
       (symbol-append 'make- id)
       (symbol-append id '-ref)
       (symbol-append id '-set!))
      
      ;; we declare the coercion operations ...
      (produce-module-clause! `(foreign ,(mk-id->bid) ,(mk-bid->id)))
      ;; and the predicate
      (produce-module-clause! `(static
				,(make-proto-inline `(,bid?-bool ::obj))))
      (produce-module-clause! `(pragma (,bid? (predicate-of ,wid))))
      
      (cons* (make-make-id)
	     (mk-=id)
	     (mk-id-null?)
	     (mk-id-null)
	     (mk-bid?)
	     (getter-&-setter))))
      
