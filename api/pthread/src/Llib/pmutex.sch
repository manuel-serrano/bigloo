;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pthread/src/Llib/pmutex.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Fri Nov 23 17:38:45 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for mutexes                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives 
   
   (java   (class $pmutex
	      (method static make::mutex (::obj) "bglpth_make_mutex")
	      (method static specific::obj (::obj) "SPECIFIC")
	      (method static specific-set!::void (::obj ::obj) "SPECIFIC_SET")
	      "bigloo.pthread.bglpmutex"))
   
   (extern (include "bglpthread.h")
	   (macro $pmutex-make::mutex (::obj) "bglpth_make_mutex")
   	   (macro $pmutex-specific::obj (::mutex) "BGLPTH_MUTEX_SPECIFIC")
	   (macro $pmutex-specific-set!::void (::mutex ::obj) "BGLPTH_MUTEX_SPECIFIC_SET")))
   
