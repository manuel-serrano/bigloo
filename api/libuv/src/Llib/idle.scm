;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/libuv/src/Llib/idle.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Sat May  6 08:55:14 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
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
	   (inline uv-idle-stop ::UvIdle)))

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
   (with-access::UvIdle o ($builtin cb %data)
      (if (not (and (procedure? cb) (correct-arity? cb 1)))
	  (error "uv-idle-start" "wrong callback" o)
	  ($uv_idle_start o cb))))

;*---------------------------------------------------------------------*/
;*    uv-idle-stop ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (uv-idle-stop o::UvIdle)
   ($uv_idle_stop o))
      
