;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/pcondvar.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar  5 14:48:55 2005                          */
;*    Last change :  Fri Nov 23 17:37:55 2012 (serrano)                */
;*    Copyright   :  2005-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces for condvars                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (java   (class $pcondvar
	      (method static make::condvar (::obj) "bglpth_make_condvar")
	      (method static specific::obj (::obj) "SPECIFIC")
	      (method static specific-set!::void (::obj ::obj) "SPECIFIC_SET")
	      "bigloo.pthread.bglpcondvar"))
   
   (extern (include "bglpthread.h")
	   (macro $pcondvar-make::condvar (::obj) "bglpth_make_condvar")
   	   (macro $pcondvar-specific::obj (::condvar) "BGLPTH_CONDVAR_SPECIFIC")
	   (macro $pcondvar-specific-set!::void (::condvar ::obj) "BGLPTH_CONDVAR_SPECIFIC_SET")))
   
