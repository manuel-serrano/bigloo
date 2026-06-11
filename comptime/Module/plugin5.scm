;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/Module/plugin5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:54:30 2026                          */
;*    Last change :  Thu Jun 11 08:57:52 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 plugins.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_plugin5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
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

   (export (module5-plugin-pragma ::Module ::pair)
	   (module5-plugin-eval ::Module ::pair)
	   (module4-plugin-eval ::Module ::pair)
	   (module4-plugin-type ::Module ::pair)
	   (module4-plugin-pragma ::Module ::pair)))

;*---------------------------------------------------------------------*/
;*    module5-plugin-pragma ...                                        */
;*---------------------------------------------------------------------*/
(define (module5-plugin-pragma mod::Module expr::pair)

   (define (parse-clause clause::pair mod::Module)
      (match-case clause
	 ((?id . ?props)
	  (let ((decl (module5-get-decl mod id clause)))
	     (with-access::Decl decl ((dmod mod) attributes)
		(cond
		   ((not (eq? dmod mod))
		    (error/loc mod
		       (format "\"~a\" is not defined in module" id)
		       clause mod))
		   (else
		    (set! attributes (append attributes props)))))))
	 (else
	  (error/loc mod "Illegal pragma clause" clause expr))))
   
   (for-each (lambda (c) (parse-clause c mod)) (cdr expr)))

;*---------------------------------------------------------------------*/
;*    module5-plugin-eval ...                                          */
;*---------------------------------------------------------------------*/
(define (module5-plugin-eval mod::Module expr::pair)
   (if (eq? (-> mod id) *module*)
       (for-each (lambda (c) (parse-eval c expr)) (cdr expr))
       (with-remembered-eval
	  (lambda ()
	     (for-each (lambda (c) (parse-eval c expr)) (cdr expr))))))

;*---------------------------------------------------------------------*/
;*    module4-plugin-eval ...                                          */
;*---------------------------------------------------------------------*/
(define (module4-plugin-eval mod::Module expr::pair)
   (module5-plugin-eval mod expr)
   '())

;*---------------------------------------------------------------------*/
;*    module4-plugin-type ...                                          */
;*---------------------------------------------------------------------*/
(define (module4-plugin-type mod::Module x::pair)
   (for-each (lambda (c) (type-parser #f c x)) (cdr x))
   '())

;*---------------------------------------------------------------------*/
;*    module4-plugin-pragma ...                                        */
;*---------------------------------------------------------------------*/
(define (module4-plugin-pragma mod::Module x::pair)
   (for-each (lambda (c) (pragma-parser c (-> mod id) x)) (cdr x))
   '())

