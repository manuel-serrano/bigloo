;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pthread/src/Llib/pthread.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Tue Apr 17 16:49:13 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for threads                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives 
   
   (java   (class $pthread
	      (constructor new (::procedure))
	      (method static setup::void () "setup")
	      (method static get-nil::$pthread () "nil")
	      (method start!::void (::$pthread ::obj ::bool) "start")
	      (method static terminate!::bool (::$pthread) "terminate")
	      (method static join!::void (::$pthread ::obj) "dojoin")
	      (method static current-thread::obj () "current_thread")
	      (method static sched-yield::int () "sched_yield")
	      (method specific::obj (::$pthread) "SPECIFIC")
	      (method specific-set!::void (::$pthread ::obj) "SPECIFIC_SET")
	      (method cleanup::obj (::$pthread) "CLEANUP")
	      (method cleanup-set!::void (::$pthread ::obj) "CLEANUP_SET")
	      "bigloo.pthread.bglpthread"))
   
   (extern (include "bglpthread.h")
	   (type $pthread void* "void *")
	   
	   ($pthread-new::$pthread (::procedure) "bglpth_thread_new")
	   (infix macro $pthread-get-nil::$pthread () "0L")
	   ($pthread-start!::void (::$pthread ::obj ::bool) "bglpth_thread_start")
	   ($pthread-join!::void (::$pthread ::obj) "bglpth_thread_join")
	   ($pthread-terminate!::bool (::$pthread) "bglpth_thread_terminate")
	   ($pthread-current-thread::obj () "bglpth_current_thread")
	   ($pthread-sched-yield::int () "sched_yield")
	   (macro $pthread-specific::obj (::$pthread) "BGLPTH_THREAD_SPECIFIC")
	   (macro $pthread-specific-set!::void (::$pthread ::obj) "BGLPTH_THREAD_SPECIFIC_SET")
	   (macro $pthread-cleanup::obj (::$pthread) "BGLPTH_THREAD_CLEANUP")
	   (macro $pthread-cleanup-set!::void (::$pthread ::obj) "BGLPTH_THREAD_CLEANUP_SET")
	   (macro $pthread-term-sig::int "BGL_PTHREAD_TERM_SIG")))


	   


