;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Module/module5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Tue Sep 16 12:32:53 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Compilation of the a Module5 clause.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_module5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
   (import engine_param
	   tools_error
	   module_module
	   expand_eps
	   ast_node
	   ast_var
	   ast_glo-decl
	   type_type)
   
   (export (module5-expand ::pair-nil)
	   (module5-ast::pair-nil ::Module)
	   (module5-imported-unit ::Module)))

;*---------------------------------------------------------------------*/
;*    module5-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (module5-expand x)
   (module5-expander x initial-expander))

;*---------------------------------------------------------------------*/
;*    module5-ast ...                                                  */
;*---------------------------------------------------------------------*/
(define (module5-ast mod::Module)

   (define (procedure-args src)
      (match-case src
	 ((define (?- . ?args) . ?-) args)
	 ((define-inline (?- . ?args) . ?-) args)
	 ((define-method (?- . ?args) . ?-) args)
	 ((define-generic (?- . ?args) . ?-) args)
	 ((define (and (? symbol?)) (lambda ?args . ?-)) args)
	 (else (error "module5-ast" "Illegal procedure source" src))))
   
   (with-access::Module mod (defs (mid id))
      (open-string-hashtable-map defs
	 (lambda (k d)
	    (with-access::Def d (src kind id decl ronly)
	       (let ((alias (if (isa? decl Decl)
				(with-access::Decl decl (alias) alias)
				id))
		     (scope (if (isa? decl Decl)
				(with-access::Decl decl (scope) scope)
				'static)))
		  (case kind
		     ((variable)
		      (declare-global-svar! id alias
			 mid scope src src))
		     ((procedure)
		      (declare-global-sfun! id alias (procedure-args src)
			 mid scope 'sfun src src))
		     ((inline)
		      (declare-global-sfun! id alias (procedure-args src)
			 mid scope 'sifun src src))
		     ((generic)
		      (declare-global-sfun! id alias (procedure-args src)
			 mid scope 'sgfun src src))
		     ((class)
		      (tprint "CLASS"))
		     (else (error "module5-ast"
			      (format "Unsupported definition kind \"~a\"" kind)
			      id)))))))))

;*---------------------------------------------------------------------*/
;*    module5-imported-unit ...                                        */
;*---------------------------------------------------------------------*/
(define (module5-imported-unit mod::Module)
   (with-access::Module mod (inits)
      (let ((body (map (lambda (imod)
			  (with-access::Module imod (id checksum)
			     (module5-checksum! imod)
			     `((@ module-initialize ,id) checksum)))
		     inits)))
	 (unit 'imported-modules 12 body #t #f))))

   

   
