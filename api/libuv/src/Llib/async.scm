;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/libuv/src/Llib/async.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Thu May  4 18:51:32 2023 (serrano)                */
;*    Copyright   :  2014-23 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV asyncs                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_async

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-async-send ::UvAsync)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvAsync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvAsync)
   (with-access::UvAsync o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_async_new o loop)))
      (with-access::UvLoop loop (%mutex)
	 (synchronize %mutex
	    ;; store in the loop for the GC
	    (uv-push-gcmark! loop o "%uv-init")))
      o))

;*---------------------------------------------------------------------*/
;*    uv-async-send ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-async-send o::UvAsync)
   (with-access::UvAsync o ($builtin)
      ($uv_async_send ($uv-async-t $builtin))))

