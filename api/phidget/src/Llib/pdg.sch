;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phidget/src/Llib/pdg.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 15:28:51 2007                          */
;*    Last change :  Thu Oct  4 18:07:52 2012 (serrano)                */
;*    Copyright   :  2007-13 Inria                                     */
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
      (macro $pdg-true::int "PTRUE")
      (macro $pdg-false::int "PFALSE")
      
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
      (macro $pdg-phidget-open-remote::int (::$pdg-phidget ::int ::string ::string)
	     "CPhidget_openRemote")
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
      
      (macro $pdg-phidget-spatial-get-datarate::int (::$pdg-phidget ::obj)
	     "bgl_phidget_spatial_get_datarate")
      (macro $pdg-phidget-spatial-set-datarate!::int (::$pdg-phidget ::int)
	     "CPhidgetSpatial_setDataRate")
      (macro $pdg-phidget-spatial-get-datarate-min::int (::$pdg-phidget ::obj)
	     "bgl_phidget_spatial_get_datarate_min")
      (macro $pdg-phidget-spatial-get-datarate-max::int (::$pdg-phidget ::obj)
	     "bgl_phidget_spatial_get_datarate_max")
      
      ;; servo
      (type $pdg-servo void* "CPhidgetServoHandle")
      (type $pdg-servo* void* "CPhidgetServoHandle *")
      (macro $pdg-servo-create::int
	 (::$pdg-servo*)
	 "CPhidgetServo_create")
      
      (macro $pdg-servo->phidget::$pdg-phidget
	 (::$pdg-servo)
	 "(CPhidgetHandle)")
      (macro $pdg-phidget->servo::$pdg-servo
	 (::$pdg-phidget)
	 "(CPhidgetServoHandle)")
      
      (macro $pdg-phidget-servo-get-motor-count::int
	 (::$pdg-servo ::obj)
	 "bgl_phidget_servo_get_motor_count")
      
      ($pdg-phidget-servo-get-position::double
	 (::$pdg-servo ::int ::obj)
	 "bgl_phidget_servo_get_position")
      (macro $pdg-phidget-servo-set-position!::int
	 (::$pdg-servo ::int ::double)
	 "CPhidgetServo_setPosition")
      ($pdg-phidget-servo-get-position-max::double
	 (::$pdg-servo ::int ::obj)
	 "bgl_phidget_servo_get_position_max")
      ($pdg-phidget-servo-get-position-min::double
	 (::$pdg-servo ::int ::obj)
	 "bgl_phidget_servo_get_position_min")
      
      ($pdg-phidget-servo-get-engaged::int
	 (::$pdg-servo ::int ::obj)
	 "bgl_phidget_servo_get_engaged")
      (macro $pdg-phidget-servo-set-engaged!::int
	 (::$pdg-servo ::int ::int)
	 "CPhidgetServo_setEngaged")
      
      (macro $pdg-phidget-servo-set-parameters!::int
	 (::$pdg-servo ::int ::double ::double ::double)
	 "CPhidgetServo_setServoParameters")
      
      ($pdg-phidget-servo-get-servo-type::int
	 (::$pdg-servo ::int ::obj)
	 "bgl_phidget_servo_get_servo_type")

      (macro $pdg-phidget-servo-set-servo-type!::int
	 (::$pdg-servo ::int ::$pdg-servo-type)
	 "CPhidgetServo_setServoType")

      ($pdg-phidget-servo-add-event-listener!::int
	 (::$pdg-servo ::string ::obj ::procedure)
	 "bgl_phidget_servo_add_event_listener")
      
      ;; advanced-advanced-servo
      (type $pdg-advanced-servo void* "CPhidgetAdvancedServoHandle")
      (type $pdg-advanced-servo* void* "CPhidgetAdvancedServoHandle *")
      (macro $pdg-advanced-servo-create::int (::$pdg-advanced-servo*)
	     "CPhidgetAdvancedServo_create")
      
      (macro $pdg-advanced-servo->phidget::$pdg-phidget
	 (::$pdg-advanced-servo)
	 "(CPhidgetHandle)")
      (macro $pdg-phidget->advanced-servo::$pdg-advanced-servo
	 (::$pdg-phidget)
	 "(CPhidgetAdvancedServoHandle)")
      
      ($pdg-phidget-advanced-servo-get-motor-count::int
	 (::$pdg-advanced-servo ::obj)
	 "bgl_phidget_advanced_servo_get_motor_count")
      
      ($pdg-phidget-advanced-servo-get-acceleration::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_acceleration")
      ($pdg-phidget-advanced-servo-set-acceleration!::int
	 (::$pdg-advanced-servo ::int ::double)
	 "CPhidgetAdvancedServo_setAcceleration")
      ($pdg-phidget-advanced-servo-get-acceleration-max::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_acceleration_max")
      ($pdg-phidget-advanced-servo-get-acceleration-min::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_acceleration_min")
      
      ($pdg-phidget-advanced-servo-get-velocity-limit::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_velocity_limit")
      (macro $pdg-phidget-advanced-servo-set-velocity-limit!::int
	 (::$pdg-advanced-servo ::int ::double)
	 "CPhidgetAdvancedServo_setVelocityLimit")
      ($pdg-phidget-advanced-servo-get-velocity::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_velocity")
      ($pdg-phidget-advanced-servo-get-velocity-max::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_velocity_max")
      ($pdg-phidget-advanced-servo-get-velocity-min::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_velocity_min")
      
      ($pdg-phidget-advanced-servo-get-position::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_position")
      (macro $pdg-phidget-advanced-servo-set-position!::int
	 (::$pdg-advanced-servo ::int ::double)
	 "CPhidgetAdvancedServo_setPosition")
      ($pdg-phidget-advanced-servo-get-position-max::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_position_max")
      ($pdg-phidget-advanced-servo-get-position-min::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_position_min")
      
      ($pdg-phidget-advanced-servo-get-speed-ramping-on::int
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_speed_ramping_on")
      (macro $pdg-phidget-advanced-servo-set-speed-ramping-on!::int
	 (::$pdg-advanced-servo ::int ::int)
	 "CPhidgetAdvancedServo_setSpeedRampingOn")
      
      ($pdg-phidget-advanced-servo-get-engaged::int
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_engaged")
      (macro $pdg-phidget-advanced-servo-set-engaged!::int
	 (::$pdg-advanced-servo ::int ::int)
	 "CPhidgetAdvancedServo_setEngaged")
      
      ($pdg-phidget-advanced-servo-get-stopped::int
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_stopped")
      
      (macro $pdg-phidget-advanced-servo-set-parameters!::int
	 (::$pdg-advanced-servo ::int ::double ::double ::double ::double)
	 "CPhidgetAdvancedServo_setServoParameters")

      ($pdg-phidget-advanced-servo-get-current::double
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_current")
      
      ($pdg-phidget-advanced-servo-add-event-listener!::int
	 (::$pdg-advanced-servo ::string ::obj ::procedure)
	 "bgl_phidget_advanced_servo_add_event_listener")

      ($pdg-phidget-advanced-servo-get-servo-type::int
	 (::$pdg-advanced-servo ::int ::obj)
	 "bgl_phidget_advanced_servo_get_servo_type")

      (macro $pdg-phidget-advanced-servo-set-servo-type!::int
	 (::$pdg-advanced-servo ::int ::$pdg-servo-type)
	 "CPhidgetAdvancedServo_setServoType")

      ;; servo types
      (type $pdg-servo-type long "CPhidget_ServoType")
      (macro $pdg-phidget-servo-default::$pdg-servo-type
	 "PHIDGET_SERVO_DEFAULT")
      (macro $pdg-phidget-servo-raw-us-mode::$pdg-servo-type
	 "PHIDGET_SERVO_RAW_us_MODE")
      (macro $pdg-phidget-servo-hitec-hs322hd::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HS322HD")
      (macro $pdg-phidget-servo-hitec-hs5245mg::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HS5245MG")
      (macro $pdg-phidget-servo-hitec-805bb::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_805BB")
      (macro $pdg-phidget-servo-hitec-hs422::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HS422")
      (macro $pdg-phidget-servo-towerpro-mg90::$pdg-servo-type
	 "PHIDGET_SERVO_TOWERPRO_MG90")
      (macro $pdg-phidget-servo-hitec-hsr1425cr::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HSR1425CR")
      (macro $pdg-phidget-servo-hitec-hs785hb::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HS785HB")
      (macro $pdg-phidget-servo-hitec-hs485hb::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HS485HB")
      (macro $pdg-phidget-servo-hitec-hs645mg::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_HS645MG")
      (macro $pdg-phidget-servo-hitec-815bb::$pdg-servo-type
	 "PHIDGET_SERVO_HITEC_815BB")
      (macro $pdg-phidget-servo-firgelli-l12-30-50-06-r::$pdg-servo-type
	 "PHIDGET_SERVO_FIRGELLI_L12_30_50_06_R")
      (macro $pdg-phidget-servo-firgelli-l12-50-100-06-r::$pdg-servo-type
	 "PHIDGET_SERVO_FIRGELLI_L12_50_100_06_R")
      (macro $pdg-phidget-servo-firgelli-l12-50-210-06-r::$pdg-servo-type
	 "PHIDGET_SERVO_FIRGELLI_L12_50_210_06_R")
      (macro $pdg-phidget-servo-firgelli-l12-100-50-06-r::$pdg-servo-type
	 "PHIDGET_SERVO_FIRGELLI_L12_100_50_06_R")
      (macro $pdg-phidget-servo-firgelli-l12-100-100-06-r::$pdg-servo-type
	 "PHIDGET_SERVO_FIRGELLI_L12_100_100_06_R")
      (macro $pdg-phidget-servo-springrc-sm-s2313m::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S2313M")
      (macro $pdg-phidget-servo-springrc-sm-s3317m::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S3317M")
      (macro $pdg-phidget-servo-springrc-sm-s3317sr::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S3317SR")
      (macro $pdg-phidget-servo-springrc-sm-s4303r::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S4303R")
      (macro $pdg-phidget-servo-springrc-sm-s4315m::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S4315M")
      (macro $pdg-phidget-servo-springrc-sm-s4315r::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S4315R")
      (macro $pdg-phidget-servo-springrc-sm-s4505b::$pdg-servo-type
	 "PHIDGET_SERVO_SPRINGRC_SM_S4505B")

      ;; stepper
      (type $pdg-stepper void* "CPhidgetStepperHandle")
      (type $pdg-stepper* void* "CPhidgetStepperHandle *")
      (macro $pdg-stepper-create::int
	 (::$pdg-stepper*)
	 "CPhidgetStepper_create")

      (macro $pdg-stepper->phidget::$pdg-phidget
	 (::$pdg-stepper)
	 "(CPhidgetHandle)")
      (macro $pdg-phidget->stepper::$pdg-stepper
	 (::$pdg-phidget)
	 "(CPhidgetStepperHandle)")

      ($pdg-phidget-stepper-get-input-count::int
	 (::$pdg-stepper ::obj)
	 "bgl_phidget_stepper_get_input_count")
      ($pdg-phidget-stepper-get-input-state::int
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_input_state")
      ($pdg-phidget-stepper-get-motor-count::int
	 (::$pdg-stepper ::obj)
	 "bgl_phidget_stepper_get_motor_count")

      ($pdg-phidget-stepper-get-acceleration::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_acceleration")
      ($pdg-phidget-stepper-set-acceleration!::int
	 (::$pdg-stepper ::int ::double)
	 "CPhidgetStepper_setAcceleration")
      ($pdg-phidget-stepper-get-acceleration-max::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_acceleration_max")
      ($pdg-phidget-stepper-get-acceleration-min::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_acceleration_min")

      ($pdg-phidget-stepper-get-velocity-limit::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_velocity_limit")
      (macro $pdg-phidget-stepper-set-velocity-limit!::int
	 (::$pdg-stepper ::int ::double)
	 "CPhidgetStepper_setVelocityLimit")
      ($pdg-phidget-stepper-get-velocity::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_velocity")
      ($pdg-phidget-stepper-get-velocity-max::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_velocity_max")
      ($pdg-phidget-stepper-get-velocity-min::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_velocity_min")

      ($pdg-phidget-stepper-get-target-position::llong
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_target_position")
      (macro $pdg-phidget-stepper-set-target-position!::int
	 (::$pdg-stepper ::int ::llong)
	 "CPhidgetStepper_setTargetPosition")
      ($pdg-phidget-stepper-get-current-position::llong
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_current_position")
      (macro $pdg-phidget-stepper-set-current-position!::int
	 (::$pdg-stepper ::int ::llong)
	 "CPhidgetStepper_setCurrentPosition")
      ($pdg-phidget-stepper-get-position-max::llong
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_position_max")
      ($pdg-phidget-stepper-get-position-min::llong
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_position_min")

      ($pdg-phidget-stepper-get-current-limit::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_current_limit")
      (macro $pdg-phidget-stepper-set-current-limit!::int
	 (::$pdg-stepper ::int ::double)
	 "CPhidgetStepper_setCurrentLimit")
      ($pdg-phidget-stepper-get-current::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_current")
      ($pdg-phidget-stepper-get-current-max::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_current_max")
      ($pdg-phidget-stepper-get-current-min::double
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_current_min")

      ($pdg-phidget-stepper-get-engaged::int
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_engaged")
      (macro $pdg-phidget-stepper-set-engaged!::int
	 (::$pdg-stepper ::int ::int)
	 "CPhidgetStepper_setEngaged")

      ($pdg-phidget-stepper-get-stopped::int
	 (::$pdg-stepper ::int ::obj)
	 "bgl_phidget_stepper_get_stopped")

      ($pdg-phidget-stepper-add-event-listener!::int
	 (::$pdg-stepper ::string ::obj ::procedure)
	 "bgl_phidget_stepper_add_event_listener")

      ;; motor control
      (type $pdg-motor-control void* "CPhidgetMotorControlHandle")
      (type $pdg-motor-control* void* "CPhidgetMotorControlHandle *")
      (macro $pdg-motor-control-create::int (::$pdg-motor-control*)
             "CPhidgetMotorControl_create")

      (macro $pdg-motor-control->phidget::$pdg-phidget
	 (::$pdg-motor-control)
	 "(CPhidgetHandle)")
      (macro $pdg-phidget->motor-control::$pdg-motor-control
	 (::$pdg-phidget)
	 "(CPhidgetMotorControlHandle)")
      (macro $pdg-phidget-motor-control-add-event-listener!::int
	 (::$pdg-motor-control ::string ::obj ::procedure)
	 "bgl_phidget_motor_control_add_event_listener")

      ($pdg-phidget-motor-control-get-motor-count::int
	 (::$pdg-motor-control ::obj)
	 "bgl_phidget_motor_control_get_motor_count")
      ($pdg-phidget-motor-control-get-acceleration::double
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_acceleration")
      (macro $pdg-phidget-motor-control-set-acceleration!::int
         (::$pdg-motor-control ::int ::double)
         "CPhidgetMotorControl_setAcceleration")
      ($pdg-phidget-motor-control-get-acceleration-min::double
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_acceleration_min")
      ($pdg-phidget-motor-control-get-acceleration-max::double
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_acceleration_max")
      ($pdg-phidget-motor-control-get-velocity::double
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_velocity")
      (macro $pdg-phidget-motor-control-set-velocity!::int
	 (::$pdg-motor-control ::int ::double)
	 "CPhidgetMotorControl_setVelocity")
      ($pdg-phidget-motor-control-encoder-count::int
         (::$pdg-motor-control ::obj)
         "bgl_phidget_motor_control_get_encoder_count")
      ($pdg-phidget-motor-control-get-encoder-position::int
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_encoder_position")
      (macro $pdg-phidget-motor-control-set-encoder-position!::int
         (::$pdg-motor-control ::int ::int)
         "CPhidgetMotorControl_setEncoderPosition")
      ($pdg-phidget-motor-control-get-braking::double
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_braking")
      (macro $pdg-phidget-motor-control-set-braking!::int
         (::$pdg-motor-control ::int ::double)
         "CPhidgetMotorControl_setBraking")
      ($pdg-phidget-motor-control-get-current::double
         (::$pdg-motor-control ::int ::obj)
         "bgl_phidget_motor_control_get_current")

      ;; encoder
      (type $pdg-encoder void* "CPhidgetEncoderHandle")
      (type $pdg-encoder* void* "CPhidgetEncoderHandle *")
      (macro $pdg-encoder-create::int (::$pdg-encoder*)
             "CPhidgetEncoder_create")

      (macro $pdg-encoder->phidget::$pdg-phidget (::$pdg-encoder)
             "(CPhidgetHandle)")
      (macro $pdg-phidget->encoder::$pdg-encoder (::$pdg-phidget)
             "(CPhidgetEncoderHandle)")
      (macro $pdg-phidget-encoder-add-event-listener!::int
         (::$pdg-encoder ::string ::obj ::procedure)
         "bgl_phidget_encoder_add_event_listener")

      ($pdg-phidget-encoder-get-encoder-count::int (::$pdg-encoder ::obj)
         "bgl_phidget_encoder_get_encoder_count")
      ($pdg-phidget-encoder-get-input-count::int (::$pdg-encoder ::obj)
         "bgl_phidget_encoder_get_input_count")
      ($pdg-phidget-encoder-get-input-enabled?::bool (::$pdg-encoder ::int ::obj)
         "bgl_phidget_encoder_get_input_enabled")
      ($pdg-phidget-encoder-get-position::int (::$pdg-encoder ::int ::obj)
         "bgl_phidget_encoder_get_position")
      (macro $pdg-phidget-encoder-set-position!::int
         (::$pdg-encoder ::int ::int)
         "CPhidgetEncoder_setPosition")
      ($pdg-phidget-encoder-get-index-position::int
         (::$pdg-encoder ::int ::obj)
         "bgl_phidget_encoder_get_index_position")

      ($pdg-phidget-encoder-get-enabled?::bool (::$pdg-encoder ::int ::obj)
         "bgl_phidget_encoder_get_enabled")
      (macro $pdg-phidget-encoder-enable!::int (::$pdg-encoder ::int)
             "bgl_phidget_encoder_enable")
      (macro $pdg-phidget-encoder-disable!::int (::$pdg-encoder ::int)
             "bgl_phidget_encoder_disable")))

;*---------------------------------------------------------------------*/
;*    phidget-return ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (phidget-return status proc obj)
   `(unless (=fx ,status $pdg-ok)
       (raise (instantiate::&phidget-error
		 (proc ,proc)
		 (msg (phidget-strerror ,status))
		 (obj ,obj)))))
