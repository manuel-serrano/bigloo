;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Wed Apr  4 08:33:23 2012 (serrano)                */
;*    Copyright   :  2001-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_makelib

   (include "pdg.sch")
   
   (import  __phidget_types
	    __phidget_phidget
	    __phidget_event
	    __phidget_logging
	    __phidget_manager
	    __phidget_ifkit
	    __phidget_spatial
	    __phidget_servo
	    __phidget_advanced-servo)

   (eval    (export-all)

	    (class &phidget-error)
	    (class &phidget-log-error)
	   
	    (class phidget-manager)
	    (class phidget)

	    (class phidget-event)
	    (class phidget-error-event)
	    (class phidget-attach-event)
	    (class phidget-detach-event)
	    (class phidget-change-event)
	    (class phidget-inputchange-event)
	    (class phidget-outputchange-event)
	    (class phidget-sensorchange-event)
	    (class phidget-serverconnect-event)
	    (class phidget-serverdisconnect-event)
	    (class phidget-spatialdata-event)

	    (class phidget-ifkit)
	    (class phidget-spatial)
	    (class phidget-servo)
	    (class phidget-advanced-servo)))
