;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Integrate/walk.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Mon Dec 30 06:37:38 2024 (serrano)                */
;*    Copyright   :  1995-2024 Manuel Serrano, see LICENSE file        */
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
	    backend_backend
	    integrate_definition
	    integrate_volatile)
   (export  (integrate-walk! globals)))

;*---------------------------------------------------------------------*/
;*    integrate-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (integrate-walk! globals)
   (pass-prelude "Integration")
   (let loop ((old globals)
	      (new '()))
      (if (null? old)
	  (let ((vars (if (backend-local-exit (the-backend))
			  (map volatile! new)
			  new)))
	     (pass-postlude (remove-var '(integrate cfa) vars)))
	  (let ((global (car old)))
	     (enter-function (global-id global))
	     (loop (cdr old) (append (integrate-definition! global) new))))))

