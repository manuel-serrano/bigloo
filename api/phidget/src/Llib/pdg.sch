;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phidget/src/Llib/pdg.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 15:28:51 2007                          */
;*    Last change :  Tue Mar 27 11:14:28 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Direct use of PHIDGET functions                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directive                                                    */
;*---------------------------------------------------------------------*/
(directives
   
   (extern
    (include "bglphidget_config.h")
    (include "bglphidget.h")

    ;; error codes
    (macro $pdg-ok::int "EPHIDGET_OK")
    (macro $pdg-notfound::int "EPHIDGET_NOTFOUND")
    (macro $pdg-nomemory::int "EPHIDGET_NOMEMORY")
    (macro $pdg-unexpected::int "EPHIDGET_UNEXPECTED")
    (macro $pdg-invalidarg::int "EPHIDGET_INVALIDARG")
    (macro $pdg-notattached::int "EPHIDGET_NOTATTACHED")
    (macro $pdg-interrupted::int "EPHIDGET_INTERRUPTED")
    (macro $pdg-invalid::int "EPHIDGET_INVALID")
    (macro $pdg-network::int "EPHIDGET_NETWORK")
    (macro $pdg-unknownval::int "EPHIDGET_UNKNOWNVAL")
    (macro $pdg-badpassword::int "EPHIDGET_BADPASSWORD")
    (macro $pdg-unsupported::int "EPHIDGET_UNSUPPORTED")
    (macro $pdg-duplicate::int "EPHIDGET_DUPLICATE")
    (macro $pdg-timeout::int "EPHIDGET_TIMEOUT")
    (macro $pdg-outofbounds::int "EPHIDGET_OUTOFBOUNDS")
    (macro $pdg-event::int "EPHIDGET_EVENT")
    (macro $pdg-network-notconnected::int "EPHIDGET_NETWORK_NOTCONNECTED")
    (macro $pdg-wrongdevice::int "EPHIDGET_WRONGDEVICE")
    (macro $pdg-closed::int "EPHIDGET_CLOSED")
    (macro $pdg-badversion::int "EPHIDGET_BADVERSION")
    (macro $pdg-_network::int "EEPHIDGET_NETWORK")
    (macro $pdg-_badpassword::int "EEPHIDGET_BADPASSWORD")
    (macro $pdg-_badversion::int "EEPHIDGET_BADVERSION")
    (macro $pdg-_overrun::int "EEPHIDGET_OVERRUN")
    (macro $pdg-_packetlost::int "EEPHIDGET_PACKETLOST")
    (macro $pdg-_wrap::int "EEPHIDGET_WRAP")
    (macro $pdg-_overtemp::int "EEPHIDGET_OVERTEMP")
    (macro $pdg-_overcurrent::int "EEPHIDGET_OVERCURRENT")
    (macro $pdg-_outofrange::int "EEPHIDGET_OUTOFRANGE")
    (macro $pdg-_badpower::int "EEPHIDGET_BADPOWER")

    ;; misc
    (macro $pdg-init!::void () "bgl_phidget_init")
    (macro $pdg-invoke-callbacks::void () "bgl_phidget_invoke_callbacks")
    
    ;; logging
    (type $pdg-log-level long "CPhidgetLog_level")
    (macro $pdg-log-critical::$pdg-log-level
       "PHIDGET_LOG_CRITICAL")
    (macro $pdg-log-error::$pdg-log-level
       "PHIDGET_LOG_ERROR")
    (macro $pdg-log-warning::$pdg-log-level
       "PHIDGET_LOG_WARNING")
    (macro $pdg-log-debug::$pdg-log-level
       "PHIDGET_LOG_DEBUG")
    (macro $pdg-log-info::$pdg-log-level
       "PHIDGET_LOG_INFO")
    (macro $pdg-log-verbose::$pdg-log-level
       "PHIDGET_LOG_VERBOSE")
    
    (macro $pdg-log-string-null::string
       "0L")

    (macro $pdg-enable-logging::int
       (::$pdg-log-level ::string) "CPhidget_enableLogging")
    (macro $pdg-disable-logging::int
       () "CPhidget_disableLogging")
    (macro $pdg-log::int
       (::$pdg-log-level ::string) "CPhidget_disableLogging")

    ;; manager
    (type $pdg-manager void* "CPhidgetManagerHandle")
    (type $pdg-manager* void* "CPhidgetManagerHandle *")
    (infix macro $pdg-manager-nil::$pdg-manager () "0L")
    
    (macro $pdg-manager-create::int (::$pdg-manager*)
	   "CPhidgetManager_create")
    (macro $pdg-manager-delete::int (::$pdg-manager)
	   "CPhidgetManager_delete")
    (macro $pdg-manager-open::int (::$pdg-manager)
	   "CPhidgetManager_open")
    (macro $pdg-manager-close::int (::$pdg-manager)
	   "CPhidgetManager_close")

    (macro $pdg-manager-add-event-listener!::int
       (::$pdg-manager ::string ::obj ::procedure)
       "bgl_phidget_manager_add_event_listener")

    ;; phidget
    (type $pdg-phidget void* "CPhidgetHandle")
    (infix macro $pdg-phidget-nil::$pdg-phidget () "0L")

    (macro $pdg-phidget-get-device-name::bstring (::$pdg-phidget)
	   "bgl_phidget_get_device_name")
    (macro $pdg-phidget-get-serial-number::int (::$pdg-phidget)
	   "bgl_phidget_get_serial_number")
    (macro $pdg-phidget-get-device-version::int (::$pdg-phidget)
	   "bgl_phidget_get_device_version")
    (macro $pdg-phidget-get-device-type::obj (::$pdg-phidget)
	   "bgl_phidget_get_device_type")
    (macro $pdg-phidget-get-device-id::obj (::$pdg-phidget)
	   "bgl_phidget_get_device_id")
    (macro $pdg-phidget-get-server-id::obj (::$pdg-phidget)
	   "bgl_phidget_get_server_id")

    (macro $pdg-phidget-phidget-add-event-listener!::int
       (::$pdg-phidget ::string ::obj ::procedure)
       "bgl_phidget_phidget_add_event_listener")

    (macro $pdg-phidget-open::int (::$pdg-phidget ::int)
	   "CPhidget_open")
    (macro $pdg-phidget-close::int (::$pdg-phidget)
	   "CPhidget_close")
    (macro $pdg-phidget-wait-for-attachment::int (::$pdg-phidget ::int)
	   "CPhidget_waitForAttachment")

    ;; ifkit
    (type $pdg-ifkit void* "CPhidgetInterfaceKitHandle")
    (type $pdg-ifkit* void* "CPhidgetInterfaceKitHandle *")
    (macro $pdg-ifkit-create::int (::$pdg-ifkit*)
	   "CPhidgetInterfaceKit_create")

    (macro $pdg-ifkit->phidget::$pdg-phidget (::$pdg-ifkit)
	   "(CPhidgetHandle)")
    (macro $pdg-phidget->ifkit::$pdg-ifkit (::$pdg-phidget)
	   "(CPhidgetInterfaceKitHandle)")
    
    (macro $pdg-phidget-ifkit-add-event-listener!::int
       (::$pdg-ifkit ::string ::obj ::procedure)
       "bgl_phidget_ifkit_add_event_listener")

    ;; spatial
    (type $pdg-spatial void* "CPhidgetSpatialHandle")
    (type $pdg-spatial* void* "CPhidgetSpatialHandle *")
    (macro $pdg-spatial-create::int (::$pdg-spatial*)
	   "CPhidgetSpatial_create")

    (macro $pdg-spatial->phidget::$pdg-phidget (::$pdg-spatial)
	   "(CPhidgetHandle)")
    (macro $pdg-phidget->spatial::$pdg-spatial (::$pdg-phidget)
	   "(CPhidgetSpatialHandle)")

    (macro $pdg-phidget-spatial-add-event-listener!::int
       (::$pdg-spatial ::string ::obj ::procedure)
       "bgl_phidget_spatial_add_event_listener")

    (macro $pdg-phidget-spatial-get-datarate::int (::$pdg-phidget)
	   "bgl_phidget_spatial_get_datarate")
    (macro $pdg-phidget-spatial-set-datarate!::int (::$pdg-phidget ::int)
	   "CPhidgetSpatial_setDataRate")
    (macro $pdg-phidget-spatial-get-datarate-min::int (::$pdg-phidget)
	   "bgl_phidget_spatial_get_datarate_min")
    (macro $pdg-phidget-spatial-get-datarate-max::int (::$pdg-phidget)
	   "bgl_phidget_spatial_get_datarate_max")

    ))

;*---------------------------------------------------------------------*/
;*    phidget-return ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (phidget-return status proc obj)
   `(unless (=fx ,status $pdg-ok)
       (raise (instantiate::&phidget-error
		 (proc ,proc)
		 (msg (phidget-strerror ,status))
		 (obj ,obj)))))
