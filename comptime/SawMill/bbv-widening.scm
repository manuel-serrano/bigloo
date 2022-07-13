;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-widening.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Wed Jul 13 13:32:38 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV widening                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-widening
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    tools_shape
	    tools_speek
	    backend_backend
	    saw_lib
	    saw_defs
	    saw_regset
	    saw_bbv-types
	    saw_bbv-utils)

   (export  (mark-widener! ::blockV)))

;*---------------------------------------------------------------------*/
;*    mark-widener! ...                                                */
;*    -------------------------------------------------------------    */
;*    Mark the BB where widening is allowed. These blocks are the      */
;*    loop heads.                                                      */
;*---------------------------------------------------------------------*/
(define (mark-widener! block)
   (let loop ((block block)
	      (stack '()))
      (with-access::block block (succs)
	 (let liip ((succs succs))
	    (when (pair? succs)
	       (let ((n (car succs)))
		  (with-access::blockV n (widener)
		     (cond
			(widener #unspecified)
			((memq n stack) (set! widener #t))
			(else (loop n (cons block stack))))
		     (liip (cdr succs)))))))))
