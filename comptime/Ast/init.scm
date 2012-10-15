;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/init.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 19 14:33:36 2005                          */
;*    Last change :  Mon Oct 15 08:12:08 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The initialization part of the AST                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_init
   
   (include "Engine/pass.sch"
	    "Ast/unit.sch"
	    "Tools/trace.sch")
   
   (import  tools_shape
	    tools_error
	    engine_param
	    engine_configure
	    module_module
	    module_library
	    type_type
	    type_cache
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_occur
	    ast_build
	    ast_env
	    ast_lvtype
	    object_class
	    bdb_emit
	    prof_emit
	    backend_init
	    backend_c_emit
	    backend_c_prototype
	    backend_c_main)
   
   (export (ast-initializers::pair-nil)))
   
;*---------------------------------------------------------------------*/
;*    ast-initializers ...                                             */
;*    -------------------------------------------------------------    */
;*    This function builds the list of global variables used           */
;*    for initializing the application (e.g. module-initialization,    */
;*    cnst-initialization, ...).                                       */
;*---------------------------------------------------------------------*/
(define (ast-initializers::pair-nil)
   (let* ((lib-unit (library-finalizer))
	  (lib-init (if (unit? lib-unit)
			(let ((vars (build-ast-sans-remove (list lib-unit))))
			   (for-each (lambda (g)
					(let ((n (sfun-body (global-value g))))
					   (occur-node-in! n g)
					   (lvtype-node! n)))
				     vars)
			   vars)
			'()))
	  (mod-init (get-module-init)))
      (cons mod-init
	    (if (and (or *main* (memq *pass* '(ld distrib)))
		     (not (eq? *main* 'imported)))
		(cons (make-bigloo-main) lib-init)
		lib-init))))

