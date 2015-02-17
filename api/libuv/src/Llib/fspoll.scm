;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fspoll.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Fri Feb  6 11:28:54 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV fspoll                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_fs-poll

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-fs-poll-start ::UvFsPoll ::procedure ::bstring ::int)
	   (uv-fs-poll-stop ::UvFsPoll)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvFsPoll ...                                          */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvFsPoll)
   (with-access::UvFsPoll o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_fs_poll_new o loop)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-fs-poll-start ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-fs-poll-start o::UvFsPoll proc path::bstring interval)
   (with-access::UvFsPoll o ($builtin loop cb)
      (with-access::UvLoop loop (%mutex %gcmarks)
	 (synchronize %mutex
	    ;; store in the loop for the GC
	    (set! %gcmarks (cons o %gcmarks))
	    ;; force Bigloo to add the extern clause for bgl_uv_fs_poll_cb
	    (when (null? %gcmarks)
	       ($bgl_uv_fs_poll_cb $uv_fs_poll_nil 0
		  $uv-stat-nil $uv-stat-nil))))
      (if (correct-arity? proc 4)
	  (begin
	     (set! cb proc)
	     ($uv_fs_poll_start ($uv-fs-poll-t $builtin) $BGL_UV_FS_POLL_CB
		path interval))
	  (error "uv-fs-poll-start" "wrong procedure arity" proc))))

;*---------------------------------------------------------------------*/
;*    uv-fs-poll-stop ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-fs-poll-stop o::UvFsPoll)
   (with-access::UvFsPoll o ($builtin)
      ($uv_fs_poll_stop ($uv-fs-poll-t $builtin))))
