;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tlift/types.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 17 14:33:21 2013                          */
;*    Last change :  Sun Nov 17 17:38:46 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wide types used by the type lifting                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tlift_types
   (import tools_shape
	   type_type
	   type_typeof
	   type_cache
	   type_env
	   ast_var
	   ast_node
	   ast_env
	   module_module
	   engine_param)
   (export (wide-class local/tlift::local
	      (ltype::type (default *_*)))))
