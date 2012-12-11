;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Dec 11 15:17:24 2012 (serrano)                */
;*    Copyright   :  2001-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_makelib

   (library pthread)
   
   (import __srfi18_backend
	   __srfi18_thread
	   __srfi18_mutex
	   __srfi18_condvar)

   (eval   (export-all)

	   (class srfi18thread)
	   (class &thread-error)
	   (class uncaught-exception)
	   (class terminated-thread-exception)))
