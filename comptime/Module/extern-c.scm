;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/Module/extern-c.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:51:54 2026                          */
;*    Last change :  Mon Jun 15 07:48:50 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 extern plugins                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_extern-c
   
   (import engine_param
	   tools_error
	   tools_shape
 	   tools_location
	   tools_misc
	   read_jvm
	   backend_backend
	   module_module
	   module_class
	   module_checksum
	   module_pragma
	   module_foreign
	   module_java
	   module_type
	   module_eval
	   module_extern5
	   heap_restore
	   expand_eps
	   expand_object
	   expand_assert
	   ast_node
	   ast_var
	   ast_env
	   ast_glo-decl
	   ast_ident
	   ast_toplevel
	   ast_build
	   ast_sexp
	   ast_private
	   ast_walk
	   type_type
	   type_env
	   type_cache
	   object_class
	   object_slots
	   object_coercion
	   foreign_jtype)

   (export (module5-extern-plugin-c ::Module ::pair)
	   (module4-extern-plugin-c ::Module ::pair)))

;*---------------------------------------------------------------------*/
;*    parse-extern-c-clause ...                                        */
;*---------------------------------------------------------------------*/
(define (parse-extern-c-clause version::long clause mod::Module x::pair)
   
   (define (parse-include string clause mod::Module)
      (unless (member string *include-foreign*)
	 (set! *include-foreign* (cons string *include-foreign*))))
   
   (define (illegal-args args src mod)
      (let loop ((args args))
	 (cond
	    ((null? args)
	     #f)
	    ((symbol? args)
	     (multiple-value-bind (id type)
		(parse-ident args)
		(unless (string? type)
		   args)))
	    ((not (pair? args))
	     args)
	    ((not (symbol? (car args)))
	     args)
	    (else
	     (multiple-value-bind (id type)
		(parse-ident (car args))
		(if (string? type)
		    (loop (cdr args))
		    args))))))
   
   (define (parse-function macro infix ident args name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident)
	 (cond
	    ((not (string? type))
	     (error/loc mod "Missing C type" ident clause))
	    ((illegal-args args clause mod)
	     =>
	     (lambda (args) (error/loc mod "Illegal C args" args clause)))
	    (else
	     (co-instantiate
		   ((def (instantiate::CDef
			    (id id)
			    (type (string->symbol type))
			    (kind 'c-function)
			    (expr clause)
			    (ronly #t)
			    (decl decl)
			    (args args)
			    (name name)
			    (macro macro)
			    (infix infix)
			    (module 'foreign)))
		    (decl (instantiate::Decl
			     (id id)
			     (alias id)
			     (mod mod)
			     (expr clause)
			     (ronly #t)
			     (scope 'extern)
			     (def def))))
		(with-access::Module mod (decls defs exports)
		   (hashtable-put! exports (symbol->string! id) decl)
		   (hashtable-put! decls (symbol->string! id) decl)
		   (hashtable-put! defs (symbol->string! id) def)))))))
   
   (define (parse-variable macro ident name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident)
	 (cond
	    ((not (string? type))
	     (error/loc mod "Missing C type" ident clause))
	    (else
	     (co-instantiate
		   ((def (instantiate::CDef
			    (id id)
			    (type (string->symbol type))
			    (kind 'c-variable)
			    (expr clause)
			    (ronly #f)
			    (decl decl)
			    (args '())
			    (name name)
			    (macro macro)
			    (infix #f)
			    (module 'foreign)))
		    (decl (instantiate::Decl
			     (id id)
			     (alias id)
			     (mod mod)
			     (expr clause)
			     (ronly #f)
			     (scope 'extern)
			     (def def))))
		(with-access::Module mod (decls defs exports)
		   (hashtable-put! exports (symbol->string! id) decl)
		   (hashtable-put! decls (symbol->string! id) decl)
		   (hashtable-put! defs (symbol->string! id) def)))))))
   
   (define (parse-type id name clause mod::Module)
      (co-instantiate
	    ((def (instantiate::TDef
		     (id id)
		     (kind 'c-type)
		     (expr clause)
		     (ronly #t)
		     (expr clause)
		     (decl decl)
		     (name name)))
	     (decl (instantiate::Decl
		      (id id)
		      (alias id)
		      (mod mod)
		      (expr clause)
		      (ronly #t)
		      (scope 'extern)
		      (def def))))
	 (with-access::Module mod (decls defs exports)
	    (hashtable-put! exports (symbol->string! id) decl)
	    (hashtable-put! decls (symbol->string! id) decl)
	    (hashtable-put! defs (symbol->string! id) def))
	 (parse-c-foreign-type clause)))

   (define (cigloo file x mod)
      (module5-extern-plugin-preprocessor "cigloo" file x mod))
   
   (define (parse-args id::symbol args::pair-nil mod clause x)
      (cond
	 ((null? args)
	  (values '() (symbol->name id clause mod)))
	 ((not (list? args))
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))
	 ((string? (car (last-pair args)))
	  (let* ((name (car (last-pair args)))
		 (args (drop-last args 1)))
	     (if (every symbol? args)
		 (values args name)
		 (error/loc mod "Illegal extern \"C\" module clause" clause x))))
	 (else
	  (values args (symbol->name id clause mod)))))

   (define (symbol->name ident::symbol src mod)
      (multiple-value-bind (id type)
	 (parse-ident ident)
	 (symbol->string id)))

   (define (parse5 clause)
      (with-trace 'module_module5 "parse-extern-c-clause"
	 (trace-item "clause=" clause)
	 (match-case clause
	    ((include (and (? string?) ?string))
	     (parse-include string clause mod))
	    ((import (and (? string?) ?string))
	     (module5-extern-plugin-c mod
		(localize clause `(include ,string)))
	     (module5-extern-plugin-c mod
		(call-with-input-file (cigloo string clause mod)
		   read)))
	    ((export . ?-)
	     (parse-c-foreign-export clause #t))
	    ((type (and (? symbol?) ?id))
	     (parse-type id (symbol->string id) clause mod))
	    ((type (and (? symbol?) ?id) (and (? string?) ?name))
	     (parse-type id name clause mod))
	    ((macro (and (? symbol?) ?ident) . ?args)
	     (multiple-value-bind (args name)
		(parse-args ident args mod clause x)
		(parse-function #t #f ident args name clause mod)))
	    ((infix macro (and (? symbol?) ?ident) . ?args)
	     (multiple-value-bind (args name)
		(parse-args ident args mod clause x)
		(parse-function #t #t ident args name clause mod)))
	    ((cnst macro (and (? symbol?) ?ident))
	     (parse-variable #t ident (symbol->name ident clause mod) clause mod))
	    ((cnst macro (and (? symbol?) ?ident) (and (? string?) ?name))
	     (parse-variable #t ident name clause mod))
	    ((variable (and (? symbol?) ?ident))
	     (parse-variable #f ident (symbol->name ident x mod) clause mod))
	    ((variable (and (? symbol?) ?ident) (and (? string?) ?name))
	     (parse-variable #f ident name clause mod))
	    (((and (? symbol?) ?ident) . ?args)
	     (multiple-value-bind (args name)
		(parse-args ident args mod clause x)
		(parse-function #f #f ident args name clause mod)))
	    ((and (? symbol?) ?ident)
	     (parse-variable #f ident (symbol->name ident x mod) clause mod))
	    (else
	     (error/loc mod "Illegal extern \"C\" module clause" clause x)))))

   (define (parse4 clause)
      (match-case clause
	 ((include (and (? string?) ?string))
	  (parse-include string clause mod))
	 ((type (and (? symbol?) ?id) (and (? string?) ?name))
	  (parse-type id name clause mod))
	 ((type (and (? symbol?) ?id) ?- (and (? string?) ?name))
	  (parse-type id name clause mod))
	 ((macro (and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #t #f ident args name clause mod))
	 ((infix macro (and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #t #t ident args name clause mod))
	 ((macro (and (? symbol?) ?ident) (and (? string?) ?name))
	  (parse-variable #t ident name clause mod))
	 ((export . ?-)
	  (parse-c-foreign-export clause #t))
	 (((and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #f #f ident args name clause mod))
	 (((and (? symbol?) ?ident) (and (? string?) ?name))
	  (parse-variable #f ident name clause mod))
	 (else
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))))

   (if (=fx version 5)
       (parse5 clause)
       (parse4 clause)))
   
;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-c mod::Module x::pair)
   (with-trace 'module_module5 "module5-extern-plugin-c"
      (trace-item "x=" x)
      (when (memq 'extern (backend-foreign-clause-support (the-backend)))
	 (for-each (lambda (c) (parse-extern-c-clause 5 c mod x)) (cddr x)))
      '()))

;*---------------------------------------------------------------------*/
;*    module4-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module4-extern-plugin-c mod::Module x::pair)
   (when (memq 'extern (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-extern-c-clause 4 c mod x)) (cdr x)))
   '())



