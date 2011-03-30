;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Foreign/copaque.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  6 12:23:13 1996                          */
;*    Last change :  Wed Mar 30 21:10:39 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C opaque accessors creations                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_copaque
   (import tools_misc
	   type_tools
	   type_type
	   foreign_ctype
	   foreign_access
	   foreign_library
	   module_module
	   ast_ident))
   
;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::copaque ...                               */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::copaque who::type loc)
   (let* ((btype       (copaque-btype what))
	  (id          (type-id who))
	  (wid         (type-id what))
	  (bid         (type-id btype))
	  (id->bid     (symbol-append id '-> bid))
	  (bid->id     (symbol-append bid '-> id))
	  (bid?        (symbol-append id '?))
	  (bid?-bool   (symbol-append bid? '::bool))
	  (name        (type-name who))
	  (name-sans-$ (string-sans-$ name)))

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
	    ,(make-typed-ident 'o1 'void*)
	    ,(make-typed-ident 'o2 'void*))
	  '(pragma::bool "($1 == $2)" o1 o2)))

      ;; we register all the foreign identifiers
      (register-foreign-access-idents!
       bid?
       (symbol-append '= id '?))
       
      ;; we declare the coercion operations ...
      (produce-module-clause! `(foreign ,(mk-id->bid) ,(mk-bid->id)))
      ;; and the predicate
      (produce-module-clause! `(static
				,(make-proto-inline `(,bid?-bool ::obj))))
      (produce-module-clause! `(pragma (,bid? (predicate-of ,bid))))

      ;; and we return the built code
      (list (mk-=id) (mk-bid?))))


