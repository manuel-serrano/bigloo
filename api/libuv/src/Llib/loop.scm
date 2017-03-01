;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/loop.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 11:51:22 2014                          */
;*    Last change :  Wed Mar  1 10:13:51 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
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
	   (uv-run::int ::UvLoop #!optional mode)
	   (uv-stop ::UvLoop)
	   (uv-loop-alive?::bool ::UvLoop)
	   (uv-update-time ::UvLoop)
	   (uv-now::uint64 ::UvLoop)
;* 	   (uv-queue-work::UvWork ::procedure ::procedure              */
;* 	      #!key (loop (uv-default-loop)))                          */
;* 	   (uv-cancel ::UvWork)                                        */
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
	    (set! gc-loops (remq! loop gc-loops))))))

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

;*---------------------------------------------------------------------*/
;*    uv-update-time ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-update-time loop::UvLoop)
   (with-access::UvLoop loop ($builtin)
      ($uv-update-time ($uv-loop-t $builtin))
      loop))
   
;*---------------------------------------------------------------------*/
;*    uv-now ...                                                       */
;*---------------------------------------------------------------------*/
(define (uv-now loop::UvLoop)
   (with-access::UvLoop loop ($builtin)
      ($uv-now ($uv-loop-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-queue-work ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-queue-work work-cb after-cb #!key (loop (uv-default-loop)))
   (tprint "BROKEN AS LIBUV uses its own thread (and not Bigloo threads)")
   (let ((w (instantiate::UvWork
	       (%work-cb work-cb)
	       (%after-cb after-cb))))
      (cond
	 ((not (correct-arity? work-cb 0))
	  (error "uv-queue-work" "wrong work callback arity" work-cb))
	 ((not (correct-arity? after-cb 1))
	  (error "uv-queue-work" "wrong after callback arity" after-cb))
	 (else
	  ($bgl_uv_queue_work w loop)
	  w))))

;*---------------------------------------------------------------------*/
;*    uv-cancel ...                                                    */
;*---------------------------------------------------------------------*/
(define (uv-cancel w::UvWork)
   (with-access::UvWork w ($builtin)
      ($uv_cancel ($uv-req-t $builtin))
      w))
