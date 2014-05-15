;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/timer.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Wed May 14 09:46:47 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV timers                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_timer

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-timer-start ::UvTimer ::uint64 ::uint64)
	   (uv-timer-stop ::UvTimer)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvTimer ...                                           */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvTimer)
   (with-access::UvTimer o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_timer_new o loop)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-timer-start ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-timer-start o::UvTimer t::uint64 r::uint64)
   (with-access::UvTimer o ($builtin loop %proc)
      (with-access::UvLoop loop (%mutex %gcmarks)
	 (synchronize %mutex
	    ;; store in the loop for the GC
	    (set! %gcmarks (cons o %gcmarks))
	    ;; force Bigloo to add the extern clause for bgl_uv_timer_cb
	    (when (null? %gcmarks) ($bgl_uv_timer_cb $uv_timer_nil 0))))
      ($uv_timer_start ($uv-timer-t $builtin) $BGL_UV_TIMER_CB t r)))

;*---------------------------------------------------------------------*/
;*    uv-timer-stop ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-timer-stop o::UvTimer)
   (with-access::UvTimer o ($builtin)
      ($uv_timer_stop ($uv-timer-t $builtin))))
      
