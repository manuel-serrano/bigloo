;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Mon Jul 21 09:51:11 2014 (serrano)                */
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
	    (uv-open-input-file ::bstring #!key (bufinfo #t) callback)))

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
      ($uv-open-input-file name buf callback)))
