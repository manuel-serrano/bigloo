;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Module/module5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Sun Sep 14 18:27:13 2025 (serrano)                */
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
	   ast_node
	   ast_var
	   ast_glo-decl
	   type_type)
   
   (export (module5-ast::pair-nil ::Module)))

;*---------------------------------------------------------------------*/
;*    module5-ast ...                                                  */
;*---------------------------------------------------------------------*/
(define (module5-ast mod::Module)
   (with-access::Module mod (defs (mid id))
      (hashtable-for-each defs (lambda (k d) (print "k=" k)))
      (open-string-hashtable-map defs
	 (lambda (k d)
	    (with-access::Def d (src kind id decl ronly)
	       (let ((alias (if (isa? decl Decl)
				(with-access::Decl decl (alias) alias)
				id))
		     (scope (if (isa? decl Decl)
				(with-access::Decl decl (scope) scope)
				'static)))
		  (tprint "id=" id " kind=" kind " ronly=" ronly)
		  (case kind
		     ((variable)
		      (declare-global-svar! id alias
			 mid scope src src))
		     ((procedure)
		      (tprint "procedure"))
		     ((inline)
		      (tprint "inline"))
		     ((generic)
		      (tprint "generic"))
		     (else (error "module5-ast"
			      (format "Unsupported definition type ~s" kind)
			      id)))))))))

   
