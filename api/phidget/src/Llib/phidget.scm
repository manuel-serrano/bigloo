;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phidget/src/Llib/phidget.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 08:29:37 2007                          */
;*    Last change :  Fri Dec 13 12:10:09 2013 (serrano)                */
;*    Copyright   :  2007-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init and cleanup of PHIDGET applications.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget

   (option (set! *dlopen-init-gc* #t))
   
   (include "pdg.sch")

   (import __phidget_types
	   __phidget_event)

   (extern (include "bglphidget_config.h")
	   (macro $configure-phidget-version::string "BGL_PHIDGET_VERSION")
	   (export phidget-error "bgl_phidget_error"))
	   
   (export (phidget-init!)
	   (phidget-version::string)
	   (phidget-strerror::bstring ::int)
	   (phidget-error ::string ::int ::obj)))

;*---------------------------------------------------------------------*/
;*    phidget-initializedp ...                                         */
;*---------------------------------------------------------------------*/
(define phidget-initializedp #f)
(define phidget-lock (make-mutex "phidget"))

;*---------------------------------------------------------------------*/
;*    phidget-init! ...                                                */
;*---------------------------------------------------------------------*/
(define (phidget-init!)
   (synchronize phidget-lock
      (unless phidget-initializedp
	 (set! phidget-initializedp #t)
	 (bigloo-configuration-add-entry!
	    'phidget-version $configure-phidget-version)
	 ($pdg-init!)
	 (%phidget-thread-init!))))

;*---------------------------------------------------------------------*/
;*    phidget-version ...                                              */
;*---------------------------------------------------------------------*/
(define (phidget-version)
   $configure-phidget-version)

;*---------------------------------------------------------------------*/
;*    phidget-strerror ...                                             */
;*---------------------------------------------------------------------*/
(define (phidget-strerror status)
   (cond
      ((=fx status $pdg-notfound)
       "A Phidget matching the type and or serial number could not be found")
      ((=fx status $pdg-nomemory)
       "Memory could not be allocated")
      ((=fx status $pdg-unexpected)
       "Unexpected Error. Contact Phidgets Inc. for support")
      ((=fx status $pdg-invalidarg)	
       "Invalid argument passed to function")
      ((=fx status $pdg-notattached)
       "Phidget not physically attached")
      ((=fx status $pdg-interrupted)
       "Read/Write operation was interrupted")
      ((=fx status $pdg-invalid)
       "The Error Code is not defined")
      ((=fx status $pdg-network)
       "Network Error")
      ((=fx status $pdg-unknownval)
       "Value is Unknown (State not yet received from device, or not yet set by user)")
      ((=fx status $pdg-badpassword)
       "Bad password")
      ((=fx status $pdg-unsupported)
       "Not Supported")
      ((=fx status $pdg-duplicate)
       "Duplicated request")
      ((=fx status $pdg-timeout)
       "Given timeout has been exceeded")
      ((=fx status $pdg-outofbounds)
       "Index out of Bounds")
      ((=fx status $pdg-event)
       "A non-null error code was returned from an event handler")
      ((=fx status $pdg-network-notconnected)
       "A connection to the server does not exist")
      ((=fx status $pdg-wrongdevice)
       "Function is not applicable for this device")
      ((=fx status $pdg-closed)
       "Phidget handle was closed")
      ((=fx status $pdg-badversion)
       "Bad version")
      ((=fx status $pdg-_network)
       "Nework Error (asynchronous)")
      ((=fx status $pdg-_badpassword)
       "Authorization Failed")
      ((=fx status $pdg-_badversion)
       "Webservice and Client protocol versions don't match")
      ((=fx status $pdg-_overrun)
       "A sampling overrun happend in firmware")
      ((=fx status $pdg-_packetlost)
       "One or more packets were lost")
      ((=fx status $pdg-_wrap)
       "A variable has wrapped around")
      ((=fx status $pdg-_overtemp)
       "Overtemperature condition detected")
      ((=fx status $pdg-_overcurrent)
       "Overcurrent condition detected")
      ((=fx status $pdg-_outofrange)
       "Out of range condition detected")
      ((=fx status $pdg-_badpower)
       "Power supply problem detected")
      (else
       (integer->string status))))

;*---------------------------------------------------------------------*/
;*    phidget-error ...                                                */
;*---------------------------------------------------------------------*/
(define (phidget-error proc status obj)
   (raise
      (instantiate::&phidget-error
	 (proc proc)
	 (msg (phidget-strerror status))
	 (obj obj))))
