;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pkgcomp/src/Llib/interface.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 11 14:45:17 2006                          */
;*    Last change :  Mon Mar  5 14:23:22 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The interface compiler (expander).                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module @interface

   (include "configure.sch"
	    "srfi89.sch"
	    "interface.sch"
	    "expanders.sch"
	    "class.sch")
   
   (export  (@interface-expander::pair ::obj ::procedure)))

