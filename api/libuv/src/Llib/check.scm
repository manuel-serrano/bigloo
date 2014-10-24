;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/check.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:27:21 2014                          */
;*    Last change :  Wed Oct 15 15:46:50 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV check callback                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_check

   (include "uv.sch")

   (import __libuv_types)
   
   (export (uv-check-start ::UvCheck)
	   (uv-check-stop ::UvCheck)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvCheck ...                                           */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvCheck)
   (with-access::UvCheck o ($builtin loop)
      (set! $builtin ($uv-handle-t ($bgl_uv_check_new o loop)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-check-start ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-check-start o::UvCheck)
   (with-access::UvCheck o ($builtin loop cb)
      (if (not (and (procedure? cb) (correct-arity? cb 1)))
	  (error "uv-check-start" "wrong callback" o)
	  (with-access::UvLoop loop (%mutex %gcmarks)
	     (synchronize %mutex
		;; store in the loop for the GC
		(set! %gcmarks (cons o %gcmarks))
		;; force Bigloo to add the extern clause for bgl_uv_check_cb
		(when (null? %gcmarks) ($bgl_uv_check_cb $uv_check_nil 0)))
	     ($uv_check_start ($uv-check-t $builtin) $BGL_UV_CHECK_CB)))))

;*---------------------------------------------------------------------*/
;*    uv-check-stop ...                                                */
;*---------------------------------------------------------------------*/
(define (uv-check-stop o::UvCheck)
   (with-access::UvCheck o ($builtin loop)
      (with-access::UvLoop loop (%mutex %gcmarks)
	 (synchronize %mutex
	    ;; remove in the loop for the GC
	    (set! %gcmarks (remq! o %gcmarks))))
      ($uv_check_stop ($uv-check-t $builtin))))
      
