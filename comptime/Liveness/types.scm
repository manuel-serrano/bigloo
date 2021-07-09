;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Liveness/types.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov  9 08:19:20 2013                          */
;*    Last change :  Fri Jul  9 09:59:39 2021 (serrano)                */
;*    Copyright   :  2013-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Wide types used to annotate the AST with a liveness property.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module liveness_types
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
   (export (wide-class local/liveness::local
	      (%count::int (default 0)))
	   
	   (wide-class literal/liveness::literal
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class ref/liveness::ref
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class closure/liveness::closure
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))

	   (wide-class kwote/liveness::kwote
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class sequence/liveness::sequence
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class app/liveness::app
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class app-ly/liveness::app-ly
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class funcall/liveness::funcall
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))

	   (wide-class pragma/liveness::pragma
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))

	   (wide-class getfield/liveness::getfield
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class setfield/liveness::setfield
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class widening/liveness::widening
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class new/liveness::new
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class valloc/liveness::valloc
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class vref/liveness::vref
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class vset!/liveness::vset!
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class vlength/liveness::vlength
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class instanceof/liveness::instanceof
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class cast-null/liveness::cast-null
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class cast/liveness::cast
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class setq/liveness::setq
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class conditional/liveness::conditional
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class fail/liveness::fail
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class switch/liveness::switch
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class let-fun/liveness::let-fun
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class let-var/liveness::let-var
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class set-ex-it/liveness::set-ex-it
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class jump-ex-it/liveness::jump-ex-it
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class sync/liveness::sync
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class retblock/liveness::retblock
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class return/liveness::return
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class make-box/liveness::make-box
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class box-ref/liveness::box-ref
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))
	   
	   (wide-class box-set!/liveness::box-set!
	      (def::pair-nil (default '()))
	      (use::pair-nil (default '()))
	      (in::pair-nil (default '()))
	      (out::pair-nil (default '())))))

	   
	      
    
