;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Mon Jun 29 10:06:21 2009 (serrano)                */
;*    Copyright   :  2001-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __fthread_makelib

   (library pthread)

   (import __ft_types
	   __ft_thread
	   __ft_scheduler
	   __ft_env
	   __ft_env2d
	   __ft_time
	   __ft_async

	   __ft_%types
	   __ft_%env
	   __ft_signal)

   (eval   (export-all)

	   (class fthread)
	   (class join-timeout-exception)
	   (class scheduler)
	   (class ftenv)))

;*---------------------------------------------------------------------*/
;*    %fthread-eval ...                                                */
;*---------------------------------------------------------------------*/
(define (%fthread-eval)
   #unspecified)
