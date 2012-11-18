;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/failsafe.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 18 08:49:33 2012                          */
;*    Last change :  Sun Nov 18 08:50:32 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The property FAILSAFE for a node is true, IFF that node cannot   */
;*    raise an exception or invoke an exit.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sync_failsafe

   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_tools
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    ast_app
	    ast_dump
	    effect_effect
	    backend_cplib)
   
   (export (generic failsafe?::bool ::node)))

;*---------------------------------------------------------------------*/
;*    failsafe? ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (failsafe? n::node)
   #f)
