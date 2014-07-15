;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Misc/make_lib.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Thu Jul 10 12:25:15 2014 (serrano)                */
;*    Copyright   :  2001-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_makelib

   (import __libuv_types
	   __libuv_loop
	   __libuv_timer
	   __libuv_handle
	   __libuv_async
	   __libuv_fs)

   (eval   (export-all)

           (class %Uv)
	   (class UvLoop)
	   (class UvWatcher)
	   (class UvTimer)
	   (class UvAsync)))

