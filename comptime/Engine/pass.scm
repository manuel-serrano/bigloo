;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/comptime/Engine/pass.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 10:49:57 1994                          */
;*    Last change :  Tue May 19 09:51:46 2026 (serrano)                */
;*    Copyright   :  1994-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The pass tools                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_pass
   (import engine_param
	   init_main
	   module_module
	   write_ast)
   (export *current-pass*
	   (stop-on-pass ::symbol ::procedure)
	   (write-pass-result ::obj)))

;*---------------------------------------------------------------------*/
;*    *current-pass* ...                                               */
;*---------------------------------------------------------------------*/
(define *current-pass* '())

;*---------------------------------------------------------------------*/
;*    stop-on-pass ...                                                 */
;*---------------------------------------------------------------------*/
(define (stop-on-pass pass thunk)
   (when (eq? *pass* pass)
      (thunk)
      (compiler-exit 0)))

;*---------------------------------------------------------------------*/
;*    write-pass-result ...                                            */
;*---------------------------------------------------------------------*/
(define (write-pass-result ast)
   (if (equal? *pass-dump* "module")
       (dump-module *module-mod*)
       (write-ast ast)))
   
