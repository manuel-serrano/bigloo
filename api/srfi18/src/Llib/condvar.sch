;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/condvar.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Tue Dec 11 18:17:08 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for condvars                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (java (class $srfi18condvar
	    (method static make::condvar (::obj) "make_condvar")
	    "bigloo.srfi18.jcondvar"))
   
   (extern (include "srfi18.h")
	   (macro $srfi18condvar-make::condvar (::obj) "srfi18_make_condvar")))

   
