;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Tue Jul 22 14:51:50 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV fs                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_fs

   (include "uv.sch")

   (import  __libuv_types
	    __libuv_loop)

   (export  (uv-rename-file ::bstring ::bstring ::procedure
	       #!optional (loop (uv-default-loop)))
	    (uv-open-input-file ::bstring #!key (bufinfo #t) callback)
	    (uv-fs-read ::input-port ::bstring ::long ::procedure
	       #!key (offset 0) (position -1) (loop (uv-default-loop)))))

;*---------------------------------------------------------------------*/
;*    uv-rename-file ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-rename-file old new cb #!optional (loop (uv-default-loop)))
   ($uv-rename-file old new cb loop))

;*---------------------------------------------------------------------*/
;*    uv-open-input-file ...                                           */
;*---------------------------------------------------------------------*/
(define (uv-open-input-file name #!key (bufinfo #t) callback)
   (let ((buf (get-port-buffer "uv-open-input-file" bufinfo c-default-io-bufsiz)))
      (cond
	 ((not callback)
	  ($uv-open-input-file name buf callback))
	 ((not (procedure? callback))
	  (error "uv-open-input-file" "wrong callback" callback))
	 ((not (correct-arity? callback 2))
	  (error "uv-open-input-file" "wrong callback arity" callback))
	 (else
	  ($uv-open-input-file name buf callback)))))

;*---------------------------------------------------------------------*/
;*    uv-fs-read ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-fs-read fd buffer length callback
	   #!key (offset 0) (position -1) (loop (uv-default-loop)))
   (if (correct-arity? callback 1)
       ($uv-fs-read fd buffer offset length position callback loop)
       (error "uv-fs-read" "wrong callback arity" callback)))
