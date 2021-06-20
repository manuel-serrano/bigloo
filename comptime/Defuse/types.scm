;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Defuse/types.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov  9 08:19:20 2013                          */
;*    Last change :  Sat Jun 19 08:04:45 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Wide types used to annotate the AST with a defuse property.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module defuse_types
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
   (export (wide-class local/defuse::local
	       (%count::int (default 0)))

	   (wide-class sequence/defuse::sequence
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class app/defuse::app
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class app-ly/defuse::app-ly
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class funcall/defuse::funcall
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class setq/defuse::setq
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class conditional/defuse::conditional
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class fail/defuse::fail
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class switch/defuse::switch
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class set-ex-it/defuse::set-ex-it
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class jump-ex-it/defuse::jump-ex-it
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class box-set!/defuse::box-set!
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class sync/defuse::sync
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class let-fun/defuse::let-fun
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))

	   (wide-class let-var/defuse::let-var
	      (def::pair-nil read-only)
	      (use::pair-nil read-only))))

	   
	      
    
