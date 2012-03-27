;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Mar 27 09:12:13 2012 (serrano)                */
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
	    __phidget_spatial)

   (eval    (export-all)

	    (class &phidget-error)
	    (class &phidget-log-error)
	   
	    (class %phidget)
	    (class phidget-manager)
	    (class phidget)

	    (class %phidget-event)

	    (class phidget-ifkit)
	    (class phidget-spatial)))
