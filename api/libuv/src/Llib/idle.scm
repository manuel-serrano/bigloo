;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/idle.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Tue Jul 29 05:45:12 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
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
   (with-access::UvIdle o ($builtin loop %proc cb)
      (if (not (and (procedure? cb) (correct-arity? cb 1)))
	  (error "uv-idle-start" "wrong callback" o)
	  (with-access::UvLoop loop (%mutex %gcmarks)
	     (synchronize %mutex
		;; store in the loop for the GC
		(set! %gcmarks (cons o %gcmarks))
		;; force Bigloo to add the extern clause for bgl_uv_idle_cb
		(when (null? %gcmarks) ($bgl_uv_idle_cb $uv_idle_nil 0)))
	     ($uv_idle_start ($uv-idle-t $builtin) $BGL_UV_IDLE_CB)))))

;*---------------------------------------------------------------------*/
;*    uv-idle-stop ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-idle-stop o::UvIdle)
   (with-access::UvIdle o ($builtin)
      ($uv_idle_stop ($uv-idle-t $builtin))))
      
