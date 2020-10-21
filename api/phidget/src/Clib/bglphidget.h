/*=====================================================================*/
/*    .../prgm/project/bigloo/api/phidget/src/Clib/bglphidget.h        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec 30 08:37:50 2007                          */
/*    Last change :  Wed Apr  4 10:20:35 2012 (serrano)                */
/*    Copyright   :  2007-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    The source file for phidget.sch.                                 */
/*=====================================================================*/
#ifndef BGLPHIDGET_H 
#define BGLPHIDGET_H

#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    C functions and macros                                           */
/*---------------------------------------------------------------------*/
#define BGL_PHIDGET_MANAGER_BUILTIN( o ) \
   (&(((BgL_phidgetzd2managerzd2_bglt)o)->BgL_z42builtinz42))

#define BGL_PHIDGET_IFKIT_BUILTIN( o ) \
   ((CPhidgetInterfaceKitHandle *)(&(((BgL_phidgetzd2ifkitzd2_bglt)o)->BgL_z42builtinz42)))

#define BGL_PHIDGET_SPATIAL_BUILTIN( o ) \
   ((CPhidgetSpatialHandle *)(&(((BgL_phidgetzd2spatialzd2_bglt)o)->BgL_z42builtinz42)))

#define BGL_PHIDGET_SERVO_BUILTIN( o ) \
   ((CPhidgetServoHandle *)(&(((BgL_phidgetzd2servozd2_bglt)o)->BgL_z42builtinz42)))

#define BGL_PHIDGET_ADVANCED_SERVO_BUILTIN( o ) \
   ((CPhidgetAdvancedServoHandle *)(&(((BgL_phidgetzd2advancedzd2servoz00_bglt)o)->BgL_z42builtinz42)))

#define BGL_PHIDGET_STEPPER_BUILTIN( o ) \
   ((CPhidgetStepperHandle *)(&(((BgL_phidgetzd2stepperzd2_bglt)o)->BgL_z42builtinz42)))

#define BGL_PHIDGET_MOTOR_CONTROL_BUILTIN( o ) \
   ((CPhidgetMotorControlHandle *)(&(((BgL_phidgetzd2motorzd2controlz00_bglt)o)->BgL_z42builtinz42)))

#define BGL_PHIDGET_ENCODER_BUILTIN( o ) \
   ((CPhidgetEncoderHandle *)(&(((BgL_phidgetzd2encoderzd2_bglt)o)->BgL_z42builtinz42)))

extern void bgl_phidget_init();

extern obj_t bgl_phidget_error( char *, int, obj_t );

extern void bgl_phidget_invoke_callbacks( void );

extern int bgl_phidget_manager_add_event_listener(
   CPhidgetManagerHandle, char *, obj_t, obj_t );

extern int bgl_phidget_phidget_add_event_listener(
   CPhidgetHandle, char *, obj_t, obj_t );

extern int bgl_phidget_ifkit_add_event_listener(
   CPhidgetInterfaceKitHandle, char *, obj_t, obj_t );

extern obj_t bgl_phidget_event_error_new( obj_t, int, char * );
extern obj_t bgl_phidget_event_attach_new( obj_t, CPhidgetHandle );
extern obj_t bgl_phidget_event_detach_new( obj_t, CPhidgetHandle );
extern obj_t bgl_phidget_event_inputchange_new( obj_t, int, int );
extern obj_t bgl_phidget_event_outputchange_new( obj_t, int, int );
extern obj_t bgl_phidget_event_sensorchange_new( obj_t, int, int );
extern obj_t bgl_phidget_event_serverconnect_new( obj_t, CPhidgetHandle );
extern obj_t bgl_phidget_event_serverdisconnect_new( obj_t, CPhidgetHandle );
extern obj_t bgl_phidget_event_spatialdata_new( obj_t, int, int,
						double, double, double,
						double, double, double,
						double, double, double );
extern obj_t bgl_phidget_event_servoposition_new( obj_t, int, double );
extern obj_t bgl_phidget_event_servovelocity_new( obj_t, int, double );
extern obj_t bgl_phidget_event_servocurrent_new( obj_t, int, double );
extern obj_t bgl_phidget_event_stepperinput_new( obj_t, int, int);
extern obj_t bgl_phidget_event_steppervelocity_new( obj_t, int, double);
extern obj_t bgl_phidget_event_stepperposition_new( obj_t, int, BGL_LONGLONG_T);
extern obj_t bgl_phidget_event_steppercurrent_new( obj_t, int, double);
extern obj_t bgl_phidget_event_motorcontrolvelocity_new( obj_t, int, double);
extern obj_t bgl_phidget_event_motorcontrolcurrent_new( obj_t, int, double);
extern obj_t bgl_phidget_event_encoderinput_new( obj_t, int, int );
extern obj_t bgl_phidget_event_encoderindex_new( obj_t, int, int );
extern obj_t bgl_phidget_event_encoderposition_new( obj_t, int, int, int );

extern obj_t bgl_phidget_get_device_name( CPhidgetHandle );
extern obj_t bgl_phidget_get_device_type( CPhidgetHandle );
extern obj_t bgl_phidget_get_device_id( CPhidgetHandle );
extern obj_t bgl_phidget_get_server_id( CPhidgetHandle );
extern int bgl_phidget_get_serial_number( CPhidgetHandle );
extern int bgl_phidget_get_device_version( CPhidgetHandle );

extern int bgl_phidget_spatial_add_event_listener( CPhidgetSpatialHandle,
						   char *, obj_t, obj_t );
extern int bgl_phidget_spatial_get_datarate( CPhidgetSpatialHandle, obj_t );
extern int bgl_phidget_spatial_get_datarate_min( CPhidgetSpatialHandle, obj_t );
extern int bgl_phidget_spatial_get_datarate_max( CPhidgetSpatialHandle, obj_t );

extern int bgl_phidget_servo_get_motor_count( CPhidgetServoHandle, obj_t );

extern int bgl_phidget_motor_control_add_event_listener(
   CPhidgetMotorControlHandle, char *, obj_t, obj_t );

extern int bgl_phidget_encoder_add_event_listener( CPhidgetEncoderHandle,
						   char *, obj_t, obj_t );
extern void bgl_phidget_encoder_enable( CPhidgetEncoderHandle, int );
extern void bgl_phidget_encoder_disable( CPhidgetEncoderHandle, int );
extern int bgl_phidget_encoder_get_input_count( CPhidgetEncoderHandle, obj_t );
extern int bgl_phidget_encoder_get_encoder_count( CPhidgetEncoderHandle, obj_t );

/*---------------------------------------------------------------------*/
/*    Bigloo functions                                                 */
/*---------------------------------------------------------------------*/
extern obj_t bgl_phidget_lock();
extern obj_t bgl_phidget_unlock();
extern obj_t bgl_phidget_signal();

extern obj_t bgl_phidget_new( CPhidgetHandle );

#endif
