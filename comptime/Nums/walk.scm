;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Nums/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Mon Jun 30 08:37:00 2025 (serrano)                */
;*    Copyright   :  2010-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Remove uless cell that has been introduced for bind-exit forms   */
;*    that have finally been transformed into returns and gotos.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module nums_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    engine_param
	    backend_backend)
   (export  (nums-walk! globals))
   (static  (wide-class local/info::local
	       (escape::bool (default #f)))))

;*---------------------------------------------------------------------*/
;*    nums-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (nums-walk! globals)
   (pass-prelude "Nums")
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    nums-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (nums-fun! var)
   (enter-function (variable-id var))
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (nums! body)
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    nums! ...                                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (nums! node::node)
   (call-default-walker))

