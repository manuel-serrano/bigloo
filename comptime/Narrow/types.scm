;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/types.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov  9 08:19:20 2013                          */
;*    Last change :  Fri Apr 21 18:45:13 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
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
	       (%count::int (default 0)))

	   (wide-class sequence/narrow::sequence
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class app/narrow::app
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class app-ly/narrow::app-ly
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class funcall/narrow::funcall
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class setq/narrow::setq
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class conditional/narrow::conditional
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class fail/narrow::fail
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class switch/narrow::switch
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class set-ex-it/narrow::set-ex-it
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class jump-ex-it/narrow::jump-ex-it
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class box-set!/narrow::box-set!
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class sync/narrow::sync
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class let-fun/narrow::let-fun
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class let-var/narrow::let-var
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))))

	   
	      
    
