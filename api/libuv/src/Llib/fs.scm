;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/fs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Thu Jul 10 15:17:19 2014 (serrano)                */
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
	       #!optional (loop (uv-default-loop)))))

;*---------------------------------------------------------------------*/
;*    uv-rename-file ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-rename-file old new cb #!optional (loop (uv-default-loop)))
   ($uv-rename-file old new cb loop))
