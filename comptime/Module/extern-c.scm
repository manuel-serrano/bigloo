;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/Module/extern-c.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:51:54 2026                          */
;*    Last change :  Fri Jul 10 09:04:03 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 extern plugins                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_extern-c

   (include "Type/coercer.sch")
   
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
	   type_coercion
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
   
   (define (parse-function macro infix ident args name clause mod::Module)
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
   
   (define (parse-type id affinity::symbol name clause mod::Module)

      (define (add-affinity-coercion! from to c)
	 (if (coercer? c)
	     (let ((check (caar (coercer-check-op c)))
		   (conv (caar (coercer-coerce-op c))))
		(let ((nx (cond
			     ((and (eq? check #t) (eq? conv #t))
			      `(coerce ,from ,to () ()))
			     ((eq? check #t)
			      `(coerce ,from ,to () (,conv)))
			     ((eq? conv #t)
			      `(coerce ,from ,to (,check) ()))
			     (else
			      `(coerce ,from ,to (,check) (,conv))))))
		   (type-parser #t (localize clause nx) (list clause))))
	     (error/loc mod
		(format "Impossible type affinity from ~a to ~a"  from to)
		clause x)))
      
      (define (add-affinity! affinity::symbol)
	 (if (type-exists? affinity)
	     (let* ((aff (find-type affinity))
		    (obj (find-type 'obj))
		    (->c (find-coercer obj aff))
		    (c-> (find-coercer aff obj)))
		(add-affinity-coercion! id 'obj c->)
		(add-affinity-coercion! 'obj id ->c)
		(type-parser #t
		   (localize clause
		      `(coerce ,id ,affinity () ())) (list clause))
		(type-parser #t
		   (localize clause
		      `(coerce ,affinity ,id () ())) (list clause)))
	     (error/loc mod "Affinity type unknown" clause x)))

      (unless (type-exists? id)
	 ;; MS 25jun2026, for now C type can override any previously
	 ;; defined type without any check, this should be fixed in the future
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
	    (let ((clause4 (localize clause `(type ,id ,name))))
	       (parse-c-foreign-type clause4))
	    (unless (eq? affinity 'opaque)
	       (add-affinity! affinity)))))

   (define (cigloo file x mod)
      (module5-extern-plugin-preprocessor "cigloo" file x mod))

   (define (build-args args::pair-nil mod::Module clause)
      (let loop ((args args))
	 (match-case args
	    (()
	     '())
	    (((and (? symbol?) ?arg) (kwote ...))
	     (multiple-value-bind (id type)
		(parse-ident arg)
		(if (string? type)
		    arg
		    (error/loc mod "Illegal C args" arg clause))))
	    (((and (? symbol?) ?arg) . ?-)
	     (multiple-value-bind (id type)
		(parse-ident arg)
		(if (string? type)
		    (cons arg (loop (cdr args)))
		    (error/loc mod "Illegal C args" arg clause))))
	    (else
	     (error/loc mod "Illegal C args" args clause)))))
   
   (define (parse-args id::symbol args::pair-nil mod clause x)
      (cond
	 ((null? args)
	  (values '() (symbol->name id clause mod)))
	 ((not (list? args))
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))
	 ((string? (car (last-pair args)))
	  (let* ((name (car (last-pair args))))
	     (values (build-args (drop-last args 1) mod clause) name)))
	 (else
	  (values (build-args args mod clause) (symbol->name id clause mod)))))

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
	    ((type (and (? symbol?) ?id) (and (? string?) ?name))
	     (parse-type id 'opaque name clause mod))
	    ((type (and (? symbol?) ?id) :affinity ?aff (and (? string?) ?name))
	     (parse-type id aff name clause mod))
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
	  (parse-type id 'none name clause mod))
	 ((type (and (? symbol?) ?id) ?affinity (and (? string?) ?name))
	  (parse-type id affinity name clause mod))
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



