;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/types.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov  9 08:19:20 2013                          */
;*    Last change :  Fri Nov 15 06:57:42 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wide types used by the narrow stage                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narrow_types
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
   (export (wide-class local/narrow::local
	       (%count::int (default 0))
	       (%count2::int (default 0))
	       (binder::int (default 0)))))
    
