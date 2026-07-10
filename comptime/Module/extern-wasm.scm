;*=====================================================================*/
;*    .../project/bigloo/5.0.x/comptime/Module/extern-wasm.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:51:54 2026                          */
;*    Last change :  Thu Jul  9 15:22:51 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 extern plugins                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_extern-wasm
   
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

   (export (module5-extern-plugin-wasm ::Module ::pair)))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-wasm ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-wasm mod::Module expr::pair)
   
   (define (parse-clause clause mod::Module)
      (match-case clause
	 (((and (? string?) ?mod) (and (? symbol?) ?ident) . ?args)
	  (multiple-value-bind (id type)
	     (parse-ident ident)
	     (let* ((nc `(,ident ,args ,(symbol->string id)))
		    (g (extern-parser nc #f)))
		(global-module-set! g (string->symbol mod))
		g)))
	 (((and (? string?) ?mod) (and (? string?) ?cn) ?id . ?args)
	  (let* ((nc `(,id ,args ,cn))
		 (g (extern-parser nc #f)))
	     (global-module-set! g (string->symbol mod))
	     g))
	 (((and (? symbol?) ?id) ?proto (and (? string?) ?cn))
	  (extern-parser clause #f))
	 (((and (? symbol?) ?id) ?proto (and (? string?) ?cn) (and (? symbol?) ?mod))
	  (let* ((nc `(,id ,proto ,cn))
		 (g (extern-parser nc #f)))
	     (tprint "c=" nc " " (typeof g))
	     (global-module-set! g mod)
	     g))
	 (((and (? symbol?) ?ident) (and (? string?) ?name) . ?deps)
	  (multiple-value-bind (id type)
	     (parse-ident ident)
	     (let ((decl (hashtable-get (-> mod decls) (symbol->string! id))))
		(if (isa? decl Decl)
		    (with-access::Decl decl (attributes)
		       (if (pair? deps)
			   (set! attributes
			      (cons* (cons 'wasm deps)
				 ;(cons 'qualified-type-name name)
				 attributes))
			   '(set! attributes
			      (cons (cons 'qualified-type-name name)
				 attributes))))
		    (error/loc "mod" "Cannot find declaration" clause expr)))))
	 
	 (else
	  (error/loc mod "Illegal extern \"wasm\" module clause" clause expr))))
   
   (when (memq 'wasm (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))
   '())

