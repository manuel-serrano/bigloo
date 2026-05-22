;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/comptime/BackEnd/walk.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug  4 14:12:02 2003                          */
;*    Last change :  Fri May 22 08:06:46 2026 (serrano)                */
;*    Copyright   :  2003-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Drivers for code generator and linker                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_walk
   (import engine_param
	   engine_pass
	   tools_shape
	   tools_error
	   module_module
	   read_jvm
	   type_type
	   ast_var
	   write_ast
	   backend_backend)
   (include "BackEnd/backend.sch"
	    "Engine/pass.sch")
   (export (backend-walk ast)))

;*---------------------------------------------------------------------*/
;*    backend-walk ...                                                 */
;*---------------------------------------------------------------------*/
(define (backend-walk functions)
   (let ((backend (the-backend)))
      (backend-init backend functions)
      (stop-on-pass 'sawmill
	 (lambda ()
	    (pass-prelude "Sawmill")
	    (let ((f (ast-filename)))
	       (when (string? f)
		  (call-with-output-file f
		     (lambda (port)
			(fprintf port ";; backend: ~a (~a)\n"
			   (backend-name backend)
			   (backend-language backend))
			(fprintf port ";; src-files: ~(, )\n" *src-files*)
			(fprintf port ";; bigloo: ~a\n"
			   (bigloo-config 'release-number))
			(fprintf port ";; date: ~a\n\n" (date)))))
	       (backend-compile backend))))
      (backend-link backend (backend-compile backend))
      backend))

;*---------------------------------------------------------------------*/
;*    backend-init ...                                                 */
;*---------------------------------------------------------------------*/
(define (backend-init backend functions)
   (backend-functions-set! backend functions))


