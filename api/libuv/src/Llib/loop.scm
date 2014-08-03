;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/loop.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 11:51:22 2014                          */
;*    Last change :  Fri Aug  1 19:59:23 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV loops                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_loop

   (option (set! *dlopen-init-gc* #t))

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-default-loop::UvLoop)
	   (uv-run ::UvLoop #!optional mode)
	   (uv-stop ::UvLoop)
	   (uv-loop-alive?::bool ::UvLoop)
	   %uv-mutex)

   (extern (export %uv-mutex "bgl_uv_mutex")))

;*---------------------------------------------------------------------*/
;*    default-loop ...                                                 */
;*---------------------------------------------------------------------*/
(define default-loop #f)

;*---------------------------------------------------------------------*/
;*    gc-loops ...                                                     */
;*---------------------------------------------------------------------*/
(define gc-loops '())
(define %uv-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvLoop ...                                            */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvLoop)
   (with-access::UvLoop o ($builtin)
      (when ($uv_handle_nilp $builtin)
	 (set! $builtin ($uv-handle-t ($uv_loop_new))))
      o))

;*---------------------------------------------------------------------*/
;*    uv-default-loop ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-default-loop)
   (unless default-loop
      (set! default-loop
	 (instantiate::UvLoop
	    ($builtin ($uv-handle-t ($uv_default_loop))))))
   default-loop)

;*---------------------------------------------------------------------*/
;*    uv-run ...                                                       */
;*---------------------------------------------------------------------*/
(define (uv-run loop::UvLoop #!optional mode)
   (with-access::UvLoop loop ($builtin)
      (unwind-protect
	 (begin
	    (set! gc-loops (cons loop gc-loops))
	    ($uv-run ($uv-loop-t $builtin) (or mode $UV_RUN_DEFAULT)))
	 (synchronize %uv-mutex
	    (set! gc-loops (remq! loop gc-loops))))
      loop))

;*---------------------------------------------------------------------*/
;*    uv-stop ...                                                      */
;*---------------------------------------------------------------------*/
(define (uv-stop loop::UvLoop)
   (with-access::UvLoop loop ($builtin)
      (synchronize %uv-mutex
	 (set! gc-loops (remq! loop gc-loops)))
      ($uv-stop ($uv-loop-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-loop-alive? ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-loop-alive? loop::UvLoop)
   (with-access::UvLoop loop ($builtin)
      ($uv-loop-alive? ($uv-loop-t $builtin))))
