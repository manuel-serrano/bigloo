;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fspoll.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Wed Mar  1 10:25:31 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
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
      (with-access::UvLoop loop (%mutex)
	 (synchronize %mutex
	    ;; store in the loop for the GC
	    (uv-push-gcmark! loop o)
	    ;; force Bigloo to add the extern clause for bgl_uv_fs_poll_cb
	    (when (uv-gcmarks-empty? loop)
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
   (with-access::UvFsPoll o ($builtin loop)
      (let ((r ($uv_fs_poll_stop ($uv-fs-poll-t $builtin))))
	 (uv-pop-gcmark! loop o)
	 r)))

;*---------------------------------------------------------------------*/
;*    uv-fs-get-path ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-fs-get-path o::UvFsPoll)
   (with-access::UvFsPoll o ($builtin)
      ($bgl_uv_fs_poll_getpath $builtin)))
