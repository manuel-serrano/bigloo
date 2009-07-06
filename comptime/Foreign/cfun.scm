;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Foreign/cfun.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  6 12:23:13 1996                          */
;*    Last change :  Mon Jul 17 11:12:41 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The C function accessors creations                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foreign_cfunction
   (import tools_error
	   tools_misc
	   type_tools
	   type_type
	   foreign_ctype
	   foreign_access
	   foreign_library
	   module_module
	   engine_param
	   ast_ident))
   
;*---------------------------------------------------------------------*/
;*    make-ctype-accesses! ::cfunction ...                             */
;*---------------------------------------------------------------------*/
(define-method (make-ctype-accesses! what::cfunction who::type loc)
   (let* ((btype       (cfunction-btype what))
	  (id          (type-id who))
	  (wid         (type-id what))
	  (bid         (type-id btype))
	  (call-id     (symbol-append id '-call))
	  (id->bid     (symbol-append id '-> bid))
	  (bid->id     (symbol-append bid '-> id))
	  (bid?        (symbol-append id '?))
	  (bid?-bool   (symbol-append bid? '::bool))
	  (name        (type-name who))
	  (name-sans-$ (string-sans-$ name))
	  (type-res    (cfunction-type-res what))
	  (type-args   (cfunction-type-args what))
	  (arity       (cfunction-arity what))
	  (nb-args     (integer->string arity)))
      
      ;; the two conversion allocation functions (they are not
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
      
      (define (mk-c-call-id)
	 (let* ((tres-id     (type-id type-res))
		(targs-id    (map type-id type-args))
		(caller-name (string-append "C_FUNCTION_CALL_" nb-args))
		(c-call-id   (symbol-append 'c- call-id)))
	    `(macro ,tres-id ,c-call-id ,(cons id targs-id) ,caller-name)))
      
      ;; the caller
      (define (mk-call-id)
	 (cond
	    ((>=fx arity *max-c-foreign-arity*)
	     (user-error id
			 (string-append
			  "Too large arity for a foreign function (max"
			  (integer->string *max-c-foreign-arity*)
			  ")")
			 (string-append nb-args " args provided")))
	    ((>=fx arity 0)
	     (fix-args-call-id))
	    (else
	     (user-error "bigloo"
			 "Can't manage pointers on C multiple arity function"
			 id))))
      
      (define (fix-args-call-id)
	 (let* ((tres-id   (type-id type-res))
		(targs-id  (map type-id type-args))
		(args      (map gensym targs-id))
		(c-call-id (symbol-append 'c- call-id)))	  
	    (make-define-inline
	     `(,(make-typed-ident call-id tres-id)
	       ,(make-typed-ident 'f id)
	       ,@(map (lambda (arg type)
			 (make-typed-ident arg type))
		      args targs-id))
	     `(,c-call-id f ,@args))))
      
      ;; we register all the foreign identifiers
      (register-foreign-access-idents!
       bid?
       (symbol-append '= id '?)
       call-id)
      
      ;; we declare the coercion operations ...
      (produce-module-clause! `(foreign ,(mk-id->bid)
					,(mk-bid->id)
					,(mk-c-call-id)))
      ;; and the predicate
      (produce-module-clause! `(static
				,(make-proto-inline `(,bid?-bool ::obj))))
      (produce-module-clause! `(pragma (,bid? (predicate-of ,wid))))
      
      ;; and we return the built code
      (list (mk-bid?) (mk-=id) (mk-call-id))))
      
      
   
