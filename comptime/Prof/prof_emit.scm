;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Prof/prof_emit.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  8 17:32:59 1998                          */
;*    Last change :  Thu Nov  3 14:28:38 2011 (serrano)                */
;*    Copyright   :  1998-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of the Bdb identifier translation table.            */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module prof_emit
   (include "Ast/node.sch"
	    "Ast/unit.sch"
	    "Tools/location.sch")
   (import  tools_shape
	    tools_error
	    tools_misc
	    type_env
	    type_cache
	    object_class
	    object_slots
	    ast_sexp
	    ast_env
	    ast_ident
	    ast_unit
	    module_module
	    module_include
	    engine_param
	    backend_c_prototype
	    backend_cplib)
   (export (emit-prof-info globals ::output-port)))

;*---------------------------------------------------------------------*/
;*    emit-prof-info ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-prof-info globals port)
   
   (define (emit-global! global)
      (let ((sfun (global-value global)))
	 (set-variable-name! global)
	 (let ((id (global-id global))
	       (c-name (global-name global))
	       (is-alloc (and (sfun? sfun) (memq 'allocator (sfun-property sfun))))
	       (loc    (and (sfun? sfun) (sfun-loc sfun))))
	    (if (location? loc)
		(fprint port
			"      fputs( \"((\\\""
			id "\\\" "
			"\\\"" (location-fname loc) "\\\" "
			(location-pos loc)
			") \\\""
			c-name
			"\\\""
			(if is-alloc " allocator" "")
			")\\n\", (FILE *)bprof_port );")
		(fprint port
			"      fputs( \"(\\\""
			id "\\\" \\\""
			c-name
			"\\\""
			(if is-alloc " allocator" "")
			")\\n\", (FILE *)bprof_port );")))))
   
   ;; the declaration of the association table
   (newline port)
   (newline port)
   ;; prof demangling table
   (fprint port "/* prof association table */")
   (fprint port "static obj_t write_bprof_table() {")
   (fprint port "   extern obj_t bprof_port;")
   (fprint port "   if( bprof_port == BUNSPEC ) bprof_port = (obj_t)fopen( \""
	   *prof-table-name* "\", \"w\" );")
   (fprint port "   if( bprof_port ) {")
   ;; the library functions dump
   (for-each-global! (lambda (g)
			(when (and (or (>fx (global-occurrence g) 0)
				       (eq? (global-module g) 'foreign))
				   (global-library g)
				   (global-user? g))
			   (emit-global! g))))
   ;; and then the non function global variables.
   (for-each (lambda (global)
		(when (global-user? global)
		   (emit-global! global)))
	     globals)
   ;; in addition we write that make_pair is CONS and some
   ;; other builtins
   (for-each (lambda (scm-c)
		(let ((scm (car scm-c))
		      (c (cdr scm-c)))
		   (fprint port
			   "      fputs( \"(\\\""
			   scm "\\\" \\\""
			   c
			   "\\\""
			   ")\\n\", (FILE *)bprof_port );")))
	     *builtin-allocators*)
   (fprint port "   }")
   (fprint port "   return BUNSPEC;")
   (fprint port #"}\n"))

