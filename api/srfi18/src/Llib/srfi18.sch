;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0a/api/srfi18/src/Llib/srfi18.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Mon Mar  9 18:25:56 2026 (serrano)                */
;*    Copyright   :  2005-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for SRFI18 threads                         */
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
