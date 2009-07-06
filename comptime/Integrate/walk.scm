;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/walk.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Mon Sep 15 11:16:03 2008 (serrano)                */
;*    Copyright   :  1995-2008 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `integration' pass.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_walk
   (include "Engine/pass.sch")
   (import  tools_error
	    engine_pass
	    type_type
	    ast_var
	    ast_remove
	    ast_node
	    integrate_definition)
   (export  (integrate-walk! globals)))

;*---------------------------------------------------------------------*/
;*    integrate-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (integrate-walk! globals)
   (pass-prelude "Integration")
   (let loop ((old globals)
	      (new '()))
      (if (null? old)
	  (pass-postlude (remove-var '(integrate cfa) new))
	  (let ((global (car old)))
	     (enter-function (global-id global))
	     (loop (cdr old)
		   (append (integrate-definition! global) new))))))

