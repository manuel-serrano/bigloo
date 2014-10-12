;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Misc/make_lib.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sat Oct 11 15:34:23 2014 (serrano)                */
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
	   __libuv_idle
	   __libuv_handle
	   __libuv_async
	   __libuv_fs
	   __libuv_os
	   __libuv_net
	   __libuv_fs-event)
   
   (eval   (export-all)
      
      (class %Uv)
      (class UvLoop)
      (class UvWatcher)
      (class UvTimer)
      (class UvIdle)
      (class UvAsync)
      (class UvFsEvent)))

