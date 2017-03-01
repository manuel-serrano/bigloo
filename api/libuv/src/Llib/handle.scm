;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/handle.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 11:51:22 2014                          */
;*    Last change :  Wed Mar  1 10:30:41 2017 (serrano)                */
;*    Copyright   :  2014-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV handles                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_handle

   (include "uv.sch")

   (import __libuv_types)
   
   (export (generic uv-ref ::UvHandle)
	   (generic uv-unref ::UvHandle)
	   (generic uv-has-ref? ::UvHandle)
	   (generic uv-close ::UvHandle #!optional callback)
	   (uv-active?::bool ::UvHandle)))

;*---------------------------------------------------------------------*/
;*    uv-ref ::UvHandle ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (uv-ref o::UvHandle)
   (with-access::UvHandle o ($builtin)
      ($uv-handle-ref $builtin)))

;*---------------------------------------------------------------------*/
;*    uv-unref ::UvHandle ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (uv-unref o::UvHandle)
   (with-access::UvHandle o ($builtin)
      ($uv-handle-unref $builtin)))

;*---------------------------------------------------------------------*/
;*    uv-has-ref? ::UvHandle ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (uv-has-ref? o::UvHandle)
   (with-access::UvHandle o ($builtin)
      ($uv-handle-has-ref? $builtin)))

;*---------------------------------------------------------------------*/
;*    uv-close ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (uv-close o::UvHandle #!optional callback)
   ;; force Bigloo to add the extern clause for bgl_uv_timer_cb
   (with-access::UvHandle o ($builtin %onclose closed)
      (when (procedure? callback)
	 (unless (correct-arity? callback 0)
	    (error "uv-close" "wrong procedure arity" callback))
	 (uv-push-gcmark! o callback)
	 (set! %onclose (lambda ()
			   (let ((r (callback)))
			      (uv-pop-gcmark! o callback)
			      r))))
      (when ($uv_handle_nilp $builtin) ($bgl_uv_close_cb $builtin))
      (unless closed
	 (set! closed #t)
	 ($uv-handle-close $builtin $BGL_UV_CLOSE_CB))))

;*---------------------------------------------------------------------*/
;*    uv-active? ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-active? o::UvHandle)
   (with-access::UvHandle o ($builtin)
      ($uv-handle-active? $builtin)))
