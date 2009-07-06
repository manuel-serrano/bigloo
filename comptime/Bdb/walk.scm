;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Bdb/walk.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr  8 17:21:15 1998                          */
;*    Last change :  Mon May 15 07:29:48 2000 (serrano)                */
;*    Copyright   :  1998-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The debugging information code production.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdb_walk
   (include "Engine/pass.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    ast_var
	    ast_node
	    ast_remove
	    bdb_initialize)
   (export  (bdb-walk! globals)))

;*---------------------------------------------------------------------*/
;*    bdb-walk! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdb-walk! globals)
   (pass-prelude "Bdb")
   (pass-postlude (append (initialize-ast) globals)))


