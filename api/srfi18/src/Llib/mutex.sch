;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/mutex.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Tue Dec 11 18:18:58 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for mutexes                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives 
   
   (java (class $srfi18mutex
	    (method static make::mutex (::obj) "make_jmutex")
	    "bigloo.srfi18.jmutex"))
   
   (extern (include "srfi18.h")
	   (macro $srfi18mutex-make::mutex (::obj) "srfi18_make_mutex")))
   
