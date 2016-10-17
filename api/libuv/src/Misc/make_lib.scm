;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Misc/make_lib.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Fri Feb  6 11:17:11 2015 (serrano)                */
;*    Copyright   :  2001-15 Manuel Serrano                            */
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
	   __libuv_pipe
	   __libuv_fs-event
	   __libuv_fs-poll
	   __libuv_poll
	   __libuv_check
	   __libuv_process)
   
   (eval   (export-all)
      
      (class %Uv)
      (class UvLoop)
      (class UvHandle)
      (class UvStream)
      (class UvTcp)
      (class UvPipe)
      (class UvTimer)
      (class UvIdle)
      (class UvAsync)
      (class UvFsEvent)
      (class UvCheck)
      (class UvProcess)
      (class UvProcessOptions)))

