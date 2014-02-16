;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:25:23 1995                          */
;*    Last change :  Sat Feb 15 14:00:10 2014 (serrano)                */
;*    Copyright   :  1995-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The Reduction optimizations                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_walk
   (include "Engine/pass.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    ast_var
	    ast_node
	    reduce_copy
	    reduce_cse
	    reduce_cond
	    reduce_typec
	    reduce_flow-typec
	    reduce_1occ
	    reduce_beta
	    ast_remove)
   (export  (reduce-walk! globals::pair-nil msg::bstring . obj)))

;*---------------------------------------------------------------------*/
;*    reduce-walk ...                                                  */
;*---------------------------------------------------------------------*/
(define (reduce-walk! globals msg . type-unsafe)
   (pass-prelude msg)
   (cond
      ((and (pair? type-unsafe) (car type-unsafe))
       (reduce-1occ! globals)
       (reduce-type-check! globals)
       (reduce-conditional! globals)
       (pass-postlude (remove-var 'reduce globals)))
      (*optim-dataflow?*
       (reduce-copy! globals)
       (when (>=fx *optim* 2) (reduce-cse! globals))
       (reduce-type-check! globals)
       (reduce-copy! globals)
       (reduce-conditional! globals)
       (reduce-1occ! globals)
       (when *optim-reduce-beta?* (reduce-beta! globals))
       (reduce-flow-type-check! globals)
       (pass-postlude (remove-var 'reduce globals)))
      (else
       (remove-var 'reduce globals))))
