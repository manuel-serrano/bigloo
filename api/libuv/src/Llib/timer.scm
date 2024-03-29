;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/libuv/src/Llib/timer.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Tue May  9 09:50:06 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
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
	   (uv-timer-stop ::UvTimer)
	   (uv-hrtime::uint64)))
   

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
   (with-access::UvTimer o (cb repeat ref)
      (set! repeat r)
      ($uv_timer_start o cb t r)))

;*---------------------------------------------------------------------*/
;*    uv-timer-stop ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-timer-stop o::UvTimer)
   (with-access::UvTimer o (loop)
      ($uv_timer_stop o)))
      
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

;*---------------------------------------------------------------------*/
;*    uv-hrtime ...                                                    */
;*---------------------------------------------------------------------*/
(define (uv-hrtime)
   ($uv-hrtime))
