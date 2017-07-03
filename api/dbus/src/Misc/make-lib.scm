;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/dbus/src/Misc/make-lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 16:57:32 2009                          */
;*    Last change :  Tue Nov  3 19:17:28 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The module used to build the DBUS heap file.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __dbus_makelib
   
   (import __dbus)
   
   (eval (export-all)
	 
	 (class dbus-connection)))
