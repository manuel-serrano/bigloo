;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/patch.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 31 10:22:17 2017                          */
;*    Last change :  Wed May 31 10:50:22 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Patch management                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_patch
   (include "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_error
	    type_cache
	    ast_sexp
	    ast_local
	    ast_apply
	    ast_app
	    ast_dump)
   (export  (get-patch-index::long #!optional idx exp loc)))

;*---------------------------------------------------------------------*/
;*    *patch-index* ...                                                */
;*---------------------------------------------------------------------*/
(define *patch-index* 0)
(define *patches* '#())

;*---------------------------------------------------------------------*/
;*    get-patch-index ...                                              */
;*---------------------------------------------------------------------*/
(define (get-patch-index #!optional idx exp loc)
   (let ((idx (or idx *patch-index*)))
      (set! *patch-index* (+fx 1 *patch-index*))
      (let ((len (vector-length *patches*)))
	 (when (>=fx *patch-index* len)
	    (set! *patches* (copy-vector *patches* (*fx len 2)))
	    (vector-fill! *patches* #f len)))
      (if (vector-ref *patches* idx)
	  (if (and exp loc)
	      (error-sexp->node "Duplicated patch" exp loc)
	      (error "get-patch-index" "Duplicated patch" idx)))
      (vector-set! *patches* idx #t)
      idx))
