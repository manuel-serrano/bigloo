;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/BackEnd/c_main.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 17:59:38 1995                          */
;*    Last change :  Thu Jul 18 13:09:53 2024 (serrano)                */
;*    Copyright   :  1995-2024 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We produce a Bigloo's `main' function.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c_main
   (import  engine_param
	    module_module
	    type_type
	    type_cache
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_env
	    ast_sexp
	    ast_local
	    ast_glo-def
	    ast_lvtype
	    tools_shape
	    backend_c_prototype
	    backend_cplib)
   (export  (make-bigloo-main)))

;*---------------------------------------------------------------------*/
;*    make-bigloo-main ...                                             */
;*---------------------------------------------------------------------*/
(define (make-bigloo-main)
   (let* ((args (list (make-local-svar 'argv *pair*)))
	  (main-body (if (global? *main*)
			 `(begin
			     (,(module-initialization-id *module*)
			      0
			      ,(symbol->string *module*))
			     ((@ bigloo-initialized! __param))
			     (%exit ((@ ,(global-id *main*)
					,(global-module *main*))
				     argv)))
			 `(begin
			     (,(module-initialization-id *module*)
			      0
			      ,(symbol->string *module*))
			     ((@ bigloo-initialized! __param))
			     (let ((z::bint ($long->bint 0)))
				(%exit z))
			     #unspecified)))
	  (node (let ((node (sexp->node main-body args #f 'value)))
		   (lvtype-node! node)
		   node))
	  (bigloo-main (def-global-sfun! 'bigloo_main::obj
			  '(argv::obj)
			  args
			  *module*
			  'sfun
			  'bigloo-main-procedure
			  'now
			  node)))
      (global-import-set! bigloo-main 'export)
      (global-name-set! bigloo-main "bigloo_main")
      bigloo-main))
