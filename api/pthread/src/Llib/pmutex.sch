;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pthread/src/Llib/pmutex.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Sat May  3 06:14:16 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for mutexes                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives 
   
   (java   (class $pmutex
	      (method static specific::obj (::obj) "SPECIFIC")
	      (method static specific-set!::void (::obj ::obj) "SPECIFIC_SET")
	      "bigloo.pthread.bglpmutex"))
   
   (extern (include "bglpthread.h")
   	   (macro $pmutex-specific::obj (::mutex) "BGLPTH_MUTEX_SPECIFIC")
	   (macro $pmutex-specific-set!::void (::mutex ::obj) "BGLPTH_MUTEX_SPECIFIC_SET")))
   
