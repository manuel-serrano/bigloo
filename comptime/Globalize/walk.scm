;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Globalize/walk.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 26 14:25:07 1995                          */
;*    Last change :  Wed Jan  8 13:49:53 2020 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `globalization' stage                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    globalize_escape
	    globalize_globalize
	    (foreign-closures globalize_global-closure)
	    ast_remove)
   (export  (globalize-walk! <global>* ::symbol)))

;*---------------------------------------------------------------------*/
;*    globalize-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (globalize-walk! globals remove)
   (pass-prelude "Closure (globalize)")
   ;; compute the escape! property for all functions (locals and globals)
   (for-each escape-fun! globals)
   ;; we perform the globalization
   (let loop ((globals globals)
	      (new-globals '()))
      (trace (globalize 3) "globalize-walk!, new-globals="
	 (map shape new-globals) #\Newline)
      (if (null? globals)
	  (pass-postlude
	     (remove-var remove (append new-globals (foreign-closures))))
	  (loop (cdr globals)
	     (append (globalize! (car globals)) new-globals)))))



 
