;*=====================================================================*/
;*    .../project/bigloo/5.0a/api/pthread/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Mon Mar  9 18:13:29 2026 (serrano)                */
;*    Copyright   :  2001-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_makelib

   (import __pth_thread
	   __pth_mutex
	   __pth_condvar
	   __pth_semaphore)

   (eval   (export-all)

	   (class pthread)
	   (class &thread-error)
	   (class uncaught-exception)
	   (class terminated-thread-exception)))
