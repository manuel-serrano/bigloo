;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/srfi18.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Tue Dec 11 20:16:11 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for threads                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives 
   
   (java (class $srfi18thread::$pthread
	    (constructor new (::procedure))
	    (method static setup::void () "setup")
	    (method static get-nil::$srfi18thread () "nil")
	    (method start!::void (::$srfi18thread ::obj ::bool) "start")
	    "bigloo.srfi18.jthread"))
   
   (extern (include "srfi18.h")
	   (type $srfi18thread void* "void *")
	   
	   ($srfi18thread-new::$srfi18thread (::procedure) "srfi18_thread_new")
	   ($srfi18thread-start!::void (::$srfi18thread ::obj ::bool) "srfi18_thread_start")
	   (infix macro $srfi18-get-nil::$srfi18thread () "0L")))
