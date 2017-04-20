;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Thu Apr 20 13:18:18 2017 (serrano)                */
;*    Copyright   :  2001-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_makelib

   (import __pth_backend
	   __pth_thread
	   __pth_mutex
	   __pth_condvar
	   __pth_semaphore)

   (eval   (export-all)

	   (class pthread)
	   (class &thread-error)
	   (class uncaught-exception)
	   (class terminated-thread-exception)))
