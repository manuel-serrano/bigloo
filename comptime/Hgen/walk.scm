;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Hgen/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Fri Nov 17 19:24:00 2023 (serrano)                */
;*    Copyright   :  1995-2023 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `C generation' pass.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hgen_walk
   (include "Engine/pass.sch")
   (import  tools_error
	    type_type
	    object_class
	    ast_var
	    ast_node
	    module_module
	    backend_c_emit
	    backend_c_prototype)
   (export  (hgen-walk)))

;*---------------------------------------------------------------------*/
;*    hgen-walk ...                                                    */
;*---------------------------------------------------------------------*/
(define (hgen-walk)
   (pass-prelude "C headers generation" (lambda () (start-emission! ".h")))
   
   ;; a very little comment 
   (emit-header)
   ;; we emit the generated type for the classes
   (let ((clist (filter (lambda (c)
			   (and (tclass? c)
				(eq? (global-module (tclass-holder c))
				   *module*)))
		   (get-class-list))))
      (let ((name (module->id *module*)))
	 (newline *c-port*)
	 (fprintf *c-port* "#ifndef __BGL_~a_H\n" name)
	 (fprintf *c-port* "#define __BGL_~a_H\n" name)
	 (emit-class-types clist *c-port*)
	 (newline *c-port*)
	 (fprintf *c-port* "#endif // __BGL_~a_H\n" name)))
   (stop-emission!))

;*---------------------------------------------------------------------*/
;*    module->id ...                                                   */
;*---------------------------------------------------------------------*/
(define (module->id mod)
   (let ((name (string-upcase (symbol->string! mod))))
      (if (bigloo-need-mangling? name)
	  (bigloo-mangle name)
	  name)))
