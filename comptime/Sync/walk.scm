;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Sync/walk.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:25:23 1995                          */
;*    Last change :  Fri Nov 16 16:50:15 2012 (serrano)                */
;*    Copyright   :  1995-2012 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The expansion of the synchronize node.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sync_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    ast_var
	    ast_node
	    engine_param)
   (export  (sync-walk! globals)))

;*---------------------------------------------------------------------*/
;*    sync-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (sync-walk! globals)
   (pass-prelude "Sync")
   (pass-postlude globals))
	   
