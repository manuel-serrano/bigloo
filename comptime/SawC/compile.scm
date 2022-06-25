;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/SawC/compile.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Fri Jun 24 20:28:16 2022 (serrano)                */
;*    Copyright   :  1995-2022 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `C generation' pass.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_c_compile
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
	    backend_c_emit
	    backend_c_prototype
	    backend_c_main
	    backend_cplib
	    object_class
	    object_slots
	    saw_c_code))

;*---------------------------------------------------------------------*/
;*    backend-compile-functions ::sawc ...                             */
;*---------------------------------------------------------------------*/
(define-method (backend-compile-functions me::sawc)
   (for-each-global! set-variable-name!)
   (let ((globals (cvm-functions me)))
      ;; we now emit the code for all the Scheme functions
      (saw-cheader)
      (for-each (lambda (v) (saw-cgen me v)) globals)
      (saw-cepilogue)))
