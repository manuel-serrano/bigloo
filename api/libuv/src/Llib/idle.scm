;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/idle.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Wed Mar  1 10:23:10 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV idle callback                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_idle

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-idle-start ::UvIdle)
	   (uv-idle-stop ::UvIdle)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvIdle ...                                            */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvIdle)
   (with-access::UvIdle o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_idle_new o loop)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-idle-start ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-idle-start o::UvIdle)
   (with-access::UvIdle o ($builtin loop cb %gcmarks)
      (if (not (and (procedure? cb) (correct-arity? cb 1)))
	  (error "uv-idle-start" "wrong callback" o)
	  (begin
	     ;; store in the loop for the GC
	     (uv-push-gcmark! loop o)
	     (uv-push-gcmark! o cb)
	     ;; force Bigloo to add the extern clause for bgl_uv_idle_cb
	     (when (uv-gcmarks-empty? loop) ($bgl_uv_idle_cb $uv_idle_nil 0))
	     ($uv_idle_start ($uv-idle-t $builtin) $BGL_UV_IDLE_CB)))))

;*---------------------------------------------------------------------*/
;*    uv-idle-stop ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-idle-stop o::UvIdle)
   (with-access::UvIdle o ($builtin loop cb)
      (with-access::UvLoop loop (%mutex)
	 (synchronize %mutex
	    ;; remove in the loop for the GC
	    (uv-pop-gcmark! loop o)))
      ($uv_idle_stop ($uv-idle-t $builtin))
      (uv-pop-gcmark! o cb)))
      
