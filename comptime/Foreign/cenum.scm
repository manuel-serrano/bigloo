;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Foreign/cenum.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  6 12:23:13 1996                          */
;*    Last change :  Mon Jul 17 11:07:57 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C enum accessors creations                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_cenum
   (import tools_misc
	   type_tools
	   type_type
	   foreign_ctype
	   foreign_access
	   foreign_library
	   module_module
	   ast_ident))
   
;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::cenum ...                                 */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::cenum who::type loc)
   (let* ((btype       (cenum-btype what))
	  (id          (type-id who))
	  (wid         (type-id what))
	  (bid         (type-id btype))
	  (id->bid     (symbol-append id '-> bid))
	  (bid->id     (symbol-append bid '-> id))
	  (bid?        (symbol-append id '?))
	  (bid?-bool   (symbol-append bid? '::bool))
	  (name        (type-name who))
	  (name-sans-$ (string-sans-$ name))
	  (literals    (cenum-literals what)))
      
      ;; the two conversion allocation fonctions (they are not
      ;; simple coercion because the first one allocate and the
      ;; second one destructurate).
      (define (mk-id->bid)
	 `(macro ,bid ,id->bid (symbol ,id) "CENUM_TO_FOREIGN"))
      
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
      
      ;; literals accessors
      (define (literal-accessors)
	 (let loop ((literals literals)
		    (res      '()))
	    (if (null? literals)
		res
		(let* ((literal      (car literals))
		       (literal-id   (car literal))
		       (literal-name (cadr literal))
		       (access-id    (symbol-append id '- literal-id))
		       (access       (make-define-inline
				      `(,(make-typed-ident access-id wid))
				      `(,(make-typed-ident 'pragma wid)
					,literal-name))))
		   (register-foreign-access-idents! access-id)
		   (loop (cdr literals)
			 (cons access res))))))
      
      ;; we register the foreign identifiers
      (register-foreign-access-idents!
       bid?
       (symbol-append '= id '?))
      
      ;; we declare the coercion operations ...
      (produce-module-clause! `(foreign ,(mk-id->bid) ,(mk-bid->id)))
      ;; and the predicate
      (produce-module-clause! `(static
				,(make-proto-inline `(,bid?-bool ::obj))))
      (produce-module-clause! `(pragma (,bid? (predicate-of ,wid))))
      
      ;; and we return the built code
      (cons* (mk-=id) (mk-bid?) (literal-accessors))))


