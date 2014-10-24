;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/timer.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Fri Oct 17 08:00:20 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV timers                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_timer

   (include "uv.sch")

   (import __libuv_types
	   __libuv_handle)
   
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
   (with-access::UvTimer o ($builtin loop repeat ref)
      (set! repeat r)
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
   (with-access::UvTimer o ($builtin count)
      ($uv_timer_stop ($uv-timer-t $builtin))))
      
;*---------------------------------------------------------------------*/
;*    uv-ref ::UvTimer ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uv-ref o::UvTimer)
   (with-access::UvTimer o (ref)
      (set! ref #t)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    uv-unref ::UvTimer ...                                           */
;*---------------------------------------------------------------------*/
(define-method (uv-unref o::UvTimer)
   (with-access::UvTimer o (ref)
      (set! ref #f)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    uv-has-ref? ::UvTimer ...                                        */
;*---------------------------------------------------------------------*/
(define-method (uv-has-ref? o::UvTimer)
   (with-access::UvTimer o (ref)
      ref))
