;*=====================================================================*/
;*    serrano/bigloo/5.0a/comptime/BackEnd/walk.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:12:02 2003                          */
;*    Last change :  Fri Feb 13 09:32:09 2026 (serrano)                */
;*    Copyright   :  2003-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Drivers for code generator and linker                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_walk
   (import engine_param
	   tools_shape
	   module_module
	   read_jvm
	   type_type
	   ast_var
	   backend_backend)
   (export (backend-walk ast)))

;*---------------------------------------------------------------------*/
;*    backend-walk ...                                                 */
;*---------------------------------------------------------------------*/
(define (backend-walk functions)
   (let ((backend (the-backend)))
      (backend-init backend functions)
      (backend-link backend (backend-compile backend))
      backend))

;*---------------------------------------------------------------------*/
;*    backend-init ...                                                 */
;*---------------------------------------------------------------------*/
(define (backend-init backend functions)
   (backend-functions-set! backend functions) )


