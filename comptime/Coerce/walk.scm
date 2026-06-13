;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/comptime/Coerce/walk.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 09:43:37 1995                          */
;*    Last change :  Sat Jun 13 08:23:15 2026 (serrano)                */
;*    Copyright   :  1995-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We perform now coercions.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    the module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_walk
   (include "Engine/pass.sch")
   (import  tools_speek
	    tools_shape
	    tools_error
	    type_type
	    type_cache
	    foreign_jtype
	    engine_param
	    ast_var
	    ast_env
	    ast_node
	    ast_remove
	    coerce_pproto
	    coerce_coerce
	    coerce_convert
	    coerce_app)
   (export  (coerce-walk! ast)))

;*---------------------------------------------------------------------*/
;*    coerce-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (coerce-walk! ast)
   (pass-prelude "Coercions & Checks")
   (init-app-cache!)
   (for-each (lambda (global)
		(reset-ppmarge!)
		(enter-function (global-id global))
		(coerce-function! global (not *unsafe-type*))
		(leave-function))
	     ast)
   (reset-ppmarge!)
   (for-each-global! (get-genv)
      (lambda (global)
	 (if (and (not (fun? (global-value global)))
		  (or (eq? (global-import global) 'static)
		      (eq? (global-import global) 'export)))
	     (pvariable-proto 3 global))))
   (verbose 2 "      type tests introduced: " (get-stack-check) #\Newline)
   (pass-postlude (remove-var 'coerce ast)))

