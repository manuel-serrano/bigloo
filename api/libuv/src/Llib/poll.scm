;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/poll.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Fri Apr 21 15:10:27 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV fspoll                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_poll

   (include "uv.sch")

   (import __libuv_types)

   (extern (export uv-events->list "bgl_uv_events_to_list"))
   
   (export (uv-poll-start ::UvPoll ::pair-nil ::procedure)
	   (uv-poll-stop ::UvPoll)
	   (uv-events->list::pair-nil ::int)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvPoll ...                                            */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvPoll)
   (with-access::UvPoll o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_poll_new o loop)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-poll-start ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-poll-start o::UvPoll events proc)
   
   (define (poll-list->events lst)
      (let loop ((lst lst)
		 (r 0))
	 (if (null? lst)
	     r
	     (loop (cdr lst)
		(bit-or r
		   (case (car lst)
		      ((UV_READABLE) $uv_readable)
		      ((UV_WRITABLE) $uv_writable)
		      ;;((UV_DISCONNECT) $uv_disconnect)
		      (else (error "uv-poll-start" "Illegal uv-poll-start" lst))))))))
   
   (with-access::UvPoll o ($builtin loop cb)
      (with-access::UvLoop loop (%mutex)
	 (synchronize %mutex
	    ;; store in the loop for the GC
	    (uv-push-gcmark! loop o)
	    ;; force Bigloo to add the extern clause for bgl_uv_poll_cb
	    (when (uv-gcmarks-empty? loop)
	       ($bgl_uv_poll_cb $uv_poll_nil 0 0))))
      (if (correct-arity? proc 3)
	  (begin
	     (set! cb proc)
	     ($uv_poll_start ($uv-poll-t $builtin)
		(poll-list->events events) $BGL_UV_POLL_CB))
	  (error "uv-poll-start" "wrong procedure arity" proc))))

;*---------------------------------------------------------------------*/
;*    uv-poll-stop ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-poll-stop o::UvPoll)
   (with-access::UvPoll o ($builtin loop)
      (let ((r ($uv_poll_stop ($uv-poll-t $builtin))))
	 (uv-pop-gcmark! loop o)
	 r)))

;*---------------------------------------------------------------------*/
;*    uv-events->list ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-events->list events::int)
   (let ((r '()))
      (when (= (bit-and events $uv_readable) $uv_readable)
	 (set! r (cons 'UV_READABLE r)))
      (when (= (bit-and events $uv_writable) $uv_writable)
	 (set! r (cons 'UV_WRITABLE r)))
      ;;(when (= (bit-and events $uv_disconnect) $uv_disconnect) (set! r (cons 'UV_DISCONNECT r)))
      r))
      
