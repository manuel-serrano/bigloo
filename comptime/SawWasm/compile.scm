;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/SawWasm/compile.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Fri May 22 07:50:58 2026 (serrano)                */
;*    Copyright   :  1995-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `C generation' pass.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_wasm_compile
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
	    ast_var
	    ast_node
	    ast_occur
	    ast_build
	    ast_env
	    object_class
	    bdb_emit
	    prof_emit
	    backend_backend
	    backend_init
	    backend_cvm
	    backend_wasm
	    backend_c_emit
	    backend_c_prototype
	    backend_c_main
	    backend_cplib
	    object_class
	    object_slots
	    saw_defs
	    saw_wasm_gen))

;*---------------------------------------------------------------------*/
;*    backend-compile-functions ::wasm ...                             */
;*---------------------------------------------------------------------*/
(define-method (backend-compile-functions me::wasm)
	;; mangle names
   (for-each-global! (get-genv) set-variable-name!)
   (let ((globals (cvm-functions me)))
      ;; we now emit the code for all the Scheme functions
      (map (lambda (v) (wasm-gen me v)) globals)))
