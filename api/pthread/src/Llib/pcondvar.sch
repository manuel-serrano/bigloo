;*=====================================================================*/
;*    serrano/prgm/project/bigloo/pthread/src/Llib/pcondvar.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Tue Mar  8 15:30:07 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The native interfaces for condvars                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (java   (class $pcondvar
	      (method static specific::obj (::obj) "SPECIFIC")
	      (method static specific-set!::void (::obj ::obj) "SPECIFIC_SET")
	      "bigloo.pthread.bglpcondvar"))
   
   (extern (include "bglpthread.h")
   	   (macro $pcondvar-specific::obj (::condvar) "BGLPTH_CONDVAR_SPECIFIC")
	   (macro $pcondvar-specific-set!::void (::condvar ::obj) "BGLPTH_CONDVAR_SPECIFIC_SET")))
   
