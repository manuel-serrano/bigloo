;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Module/module5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Sat Sep 13 06:38:01 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Compilation of the a Module5 clause.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_module5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
   (import engine_param
	   tools_error
	   module_module)
   
   (export (produce-module5! ::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    produce-module5! ...                                             */
;*---------------------------------------------------------------------*/
(define (produce-module5! mod path)
   (pass-prelude "Module5")
   (let ((mod (module5-parse mod path :lib-path *lib-dir*)))
      (with-access::Module mod (id)
	 (set! *module* id))
      (list (unit 'toplevel 100 '() #t #f))))


   
