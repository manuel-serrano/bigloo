/*=====================================================================*/
/*    .../prgm/project/bigloo/api/phidget/src/Clib/bglphidget.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Sep 21 12:08:37 2010                          */
/*    Last change :  Thu Nov 15 12:41:52 2012 (serrano)                */
/*    Copyright   :  2010-12 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo wrapper for the widget library                            */
/*=====================================================================*/
#include "bglphidget_config.h"
#include "bglphidget.h"
#include <string.h>
#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    CHECK_PROCEDURE                                                  */
/*---------------------------------------------------------------------*/
#define CHECK_PROCEDURE( proc, arity ) \
   if( !PROCEDURE_CORRECT_ARITYP( proc, arity ) ) { \
      char buf[ 80 ]; \
      sprintf( buf, "wrong number of arguments for callback (%d expected)", arity ); \
      C_SYSTEM_FAILURE( BGL_ERROR, "phidget-add-event-listener", buf, proc ); \
   }

/*---------------------------------------------------------------------*/
/*    struct handler                                                   */
/*    -------------------------------------------------------------    */
/*    handler is a local data type used to bind bigloo objects and     */
/*    bigloo procedures so that phidget events can retreive the        */
/*    owner of the handler. In addition, the handlers global variable  */
/*    plays the role of a ROOT for the GC.                             */
/*---------------------------------------------------------------------*/
struct handler {
   int evtype;
   obj_t obj;
   obj_t proc;
};

#define EVENT_ERROR 1
#define EVENT_ATTACH 2
#define EVENT_DETACH 3
#define EVENT_INPUTCHANGE 4
#define EVENT_OUTPUTCHANGE 5
#define EVENT_SENSORCHANGE 6
#define EVENT_SERVERCONNECT 7
#define EVENT_SERVERDISCONNECT 8
#define EVENT_SPATIALDATA 9
#define EVENT_SERVOPOSITION 10
#define EVENT_SERVOVELOCITY 11
#define EVENT_SERVOCURRENT 12
#define EVENT_STEPPERINPUT 13
#define EVENT_STEPPERVELOCITY 14
#define EVENT_STEPPERPOSITION 15
#define EVENT_STEPPERCURRENT 16
#define EVENT_MOTORCONTROLVELOCITY 17
#define EVENT_MOTORCONTROLCURRENT 18
#define EVENT_ENCODERINPUT 19
#define EVENT_ENCODERPOSITION 20
#define EVENT_ENCODERINDEX 21

#define INITIAL_MAX_HANDLER 40
static struct handler *handlers;
static int handler_length = INITIAL_MAX_HANDLER;
static int handler_index = 0;

/*---------------------------------------------------------------------*/
/*    struct callback                                                  */
/*    -------------------------------------------------------------    */
/*    The callback machinery is used for one purpose. Phidget          */
/*    threads cannot invoke Bigloo code because the GC gets            */
/*    confused when alloc and collection functions are called from     */
/*    non-Bigloo thread. The callback is used to register the          */
/*    callbacks that are invoked by a dedicated Bigloo thread.         */
/*---------------------------------------------------------------------*/
struct callback {
   struct handler *handler;
   union {
      struct {
	 int code;
	 char *msg;
      } error;
      struct {
	 CPhidgetHandle id;
      } phidget;
      struct {
	 int index;
	 int istate;
      } change;
      struct {
	 int seconds;
	 int microseconds;
	 double acceleration[ 3 ];
	 double angularRate[ 3 ];
	 double magneticField[ 3 ];
      } spatial;
      struct {
	 int index;
	 double position;
      } servoposition;
      struct {
	 int index;
	 double velocity;
      } servovelocity;
      struct {
	 int index;
	 double current;
      } servocurrent;
      struct {
	 int index;
	 BGL_LONGLONG_T position;
      } stepperposition;
      struct {
	 int index;
	 double velocity;
      } motorcontrolvelocity;
      struct {
	 int index;
	 double current;
      } motorcontrolcurrent;
      struct {
	 int index;
	 int state;
      } encoderinput;
      struct {
	 int index;
	 int time;
	 int position;
      } encoderposition;
      struct {
	 int index;
	 int position;
      } encoderindex;
   } event;
};

#define INITIAL_MAX_CALLBACK 40
static struct callback *callbacks;
static int callback_length = INITIAL_MAX_CALLBACK;
static int callback_index = 0;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_phidget_init ...                                             */
/*---------------------------------------------------------------------*/
void bgl_phidget_init() {
   /* allocated the callbacks array */
   callbacks = calloc( sizeof( struct callback ), callback_length );
   /* allocated the handlers array */
   handlers = (void *)GC_MALLOC( sizeof( struct handler ) * handler_length );
}

/*---------------------------------------------------------------------*/
/*    ENLARGE_ARRAY ...                                                */
/*---------------------------------------------------------------------*/
#define ENLARGE_ARRAY( type ) { \
   struct type *n##type##s; \
   int osize = type##_length * sizeof( struct type ); \
   \
   type##_length *= 2; \
   n##type##s = malloc( osize * 2 ); \
   memcpy( n##type##s, type##s, osize ); \
   \
   free( type##s ); \
   type##s = n##type##s; \
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    enlarge_callback_array ...                                       */
/*---------------------------------------------------------------------*/
static void enlarge_callback_array() {
   ENLARGE_ARRAY( callback );
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    enlarge_handler_array ...                                        */
/*---------------------------------------------------------------------*/
static void enlarge_handler_array() {
   ENLARGE_ARRAY( handler );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_phidget_invoke_callbacks ...                                 */
/*    -------------------------------------------------------------    */
/*    This function is invoked by a Scheme code when it has been       */
/*    signaled that a phidget event has been fired.                    */
/*---------------------------------------------------------------------*/
void bgl_phidget_invoke_callbacks() {
   while( callback_index > 0 ) {
      struct callback *cb = &callbacks[ --callback_index ];
      struct handler *hdl = cb->handler;
      obj_t event;

      switch( hdl->evtype ) {
	 case EVENT_ERROR:
	    event = bgl_phidget_event_error_new(
	       hdl->obj,
	       cb->event.error.code,
	       cb->event.error.msg );
	    break;
	 case EVENT_ATTACH:
	    event = bgl_phidget_event_attach_new(
	       hdl->obj,
	       cb->event.phidget.id );
	    break;
	 case EVENT_DETACH:
	    event = bgl_phidget_event_detach_new(
	       hdl->obj,
	       cb->event.phidget.id );
	    break;
	 case EVENT_INPUTCHANGE:
	    event = bgl_phidget_event_inputchange_new(
	       hdl->obj,
	       cb->event.change.index,
	       cb->event.change.istate == PTRUE );
	    break;
	 case EVENT_OUTPUTCHANGE:
	    event = bgl_phidget_event_outputchange_new(
	       hdl->obj,
	       cb->event.change.index,
	       cb->event.change.istate == PTRUE );
	    break;
	 case EVENT_SENSORCHANGE:
	    event = bgl_phidget_event_sensorchange_new(
	       hdl->obj,
	       cb->event.change.index,
	       cb->event.change.istate );
	    break;
	 case EVENT_SERVERCONNECT:
	    event = bgl_phidget_event_serverconnect_new(
	       hdl->obj,
	       cb->event.phidget.id );
	    break;
	 case EVENT_SERVERDISCONNECT:
	    event = bgl_phidget_event_serverdisconnect_new(
	       hdl->obj,
	       cb->event.phidget.id );
	    break;
	 case EVENT_SPATIALDATA: {
	    event = bgl_phidget_event_spatialdata_new(
	       hdl->obj,
	       cb->event.spatial.seconds,
	       cb->event.spatial.microseconds,
	       cb->event.spatial.acceleration[ 0 ],
	       cb->event.spatial.acceleration[ 1 ],
	       cb->event.spatial.acceleration[ 2 ],
	       cb->event.spatial.angularRate[ 0 ],
	       cb->event.spatial.angularRate[ 1 ],
	       cb->event.spatial.angularRate[ 2 ],
	       cb->event.spatial.magneticField[ 0 ],
	       cb->event.spatial.magneticField[ 1 ],
	       cb->event.spatial.magneticField[ 2 ] );
	    break;
	 }
	 case EVENT_SERVOPOSITION: {
	    event = bgl_phidget_event_servoposition_new(
	       hdl->obj,
	       cb->event.servoposition.index,
	       cb->event.servoposition.position );
	    break;
	 }
	 case EVENT_SERVOVELOCITY: {
	    event = bgl_phidget_event_servovelocity_new(
	       hdl->obj,
	       cb->event.servovelocity.index,
	       cb->event.servovelocity.velocity );
	    break;
	 }
	 case EVENT_SERVOCURRENT: {
	    event = bgl_phidget_event_servocurrent_new(
	       hdl->obj,
	       cb->event.servocurrent.index,
	       cb->event.servocurrent.current );
	    break;
	 }
	 case EVENT_STEPPERINPUT: {
	    event = bgl_phidget_event_stepperinput_new(
	       hdl->obj,
	       cb->event.change.index,
	       cb->event.change.istate == PTRUE );
	    break;
	 }
	 case EVENT_STEPPERVELOCITY: {
	    event = bgl_phidget_event_steppervelocity_new(
	       hdl->obj,
	       cb->event.servovelocity.index,
	       cb->event.servovelocity.velocity );
	    break;
	 }
	 case EVENT_STEPPERPOSITION: {
	    event = bgl_phidget_event_stepperposition_new(
	       hdl->obj,
	       cb->event.stepperposition.index,
	       cb->event.stepperposition.position );
	    break;
	 }
	 case EVENT_STEPPERCURRENT: {
	    event = bgl_phidget_event_steppercurrent_new(
	       hdl->obj,
	       cb->event.servocurrent.index,
	       cb->event.servocurrent.current );
	    break;
	 }
	 case EVENT_MOTORCONTROLVELOCITY: {
	    event = bgl_phidget_event_motorcontrolvelocity_new(
	       hdl->obj,
	       cb->event.motorcontrolvelocity.index,
	       cb->event.motorcontrolvelocity.velocity );
	    break;
	 }
	 case EVENT_MOTORCONTROLCURRENT: {
	    event = bgl_phidget_event_motorcontrolcurrent_new(
	       hdl->obj,
	       cb->event.motorcontrolcurrent.index,
	       cb->event.motorcontrolcurrent.current );
	    break;
	 }
	 case EVENT_ENCODERINPUT: {
	    event = bgl_phidget_event_encoderinput_new(
	       hdl->obj,
	       cb->event.encoderinput.index,
	       cb->event.encoderinput.state );
	    break;
	 }
	 case EVENT_ENCODERINDEX: {
	    event = bgl_phidget_event_encoderindex_new(
	       hdl->obj,
	       cb->event.encoderindex.index,
	       cb->event.encoderindex.position );
	    break;
	 }
	 case EVENT_ENCODERPOSITION: {
	    event = bgl_phidget_event_encoderposition_new(
	       hdl->obj,
	       cb->event.encoderposition.index,
	       cb->event.encoderposition.time,
	       cb->event.encoderposition.position );
	    break;
	 }

	 default:
	    event = BUNSPEC;
	    C_SYSTEM_FAILURE(
	       BGL_ERROR, "phidget", "Unknown event", BINT( hdl->evtype ) );
      }

      PROCEDURE_ENTRY( hdl->proc )( hdl->proc, event, BEOA ); 
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_add_handler ...                                              */
/*---------------------------------------------------------------------*/
static struct handler *bgl_add_handler( obj_t obj, obj_t proc, int evtype ) {
   struct handler *hdl;
   
   CHECK_PROCEDURE( proc, 1 );
   
   bgl_phidget_lock();

   if( handler_index == handler_length ) enlarge_handler_array();
   
   handlers[ handler_index ].evtype = evtype;
   handlers[ handler_index ].proc = proc;
   handlers[ handler_index ].obj = obj;
   hdl = &handlers[ handler_index++ ];
   
   bgl_phidget_unlock();

   return hdl;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_phidget_handler ...                                          */
/*---------------------------------------------------------------------*/
static int bgl_phidget_handler( CPhidgetHandle phid, void *ptr ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.phidget.id = phid;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_change_handler ...                                           */
/*---------------------------------------------------------------------*/
static int bgl_change_handler( CPhidgetInterfaceKitHandle id, void *ptr, int index, int istate ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.change.index = index;
   callbacks[ callback_index ].event.change.istate = istate;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_spatial_handler ...                                          */
/*---------------------------------------------------------------------*/
static int bgl_spatial_handler( CPhidgetSpatialHandle id, void *ptr, CPhidgetSpatial_SpatialEventDataHandle *data, int count ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.spatial.seconds =
      data[ 0 ]->timestamp.seconds;
   callbacks[ callback_index ].event.spatial.microseconds =
      data[ 0 ]->timestamp.microseconds;
   callbacks[ callback_index ].event.spatial.acceleration[ 0 ] =
      data[ 0 ]->acceleration[ 0 ];
   callbacks[ callback_index ].event.spatial.acceleration[ 1 ] =
      data[ 0 ]->acceleration[ 1 ];
   callbacks[ callback_index ].event.spatial.acceleration[ 2 ] =
      data[ 0 ]->acceleration[ 2 ];
   callbacks[ callback_index ].event.spatial.angularRate[ 0 ] =
      data[ 0 ]->angularRate[ 0 ];
   callbacks[ callback_index ].event.spatial.angularRate[ 1 ] =
      data[ 0 ]->angularRate[ 1 ];
   callbacks[ callback_index ].event.spatial.angularRate[ 2 ] =
      data[ 0 ]->angularRate[ 2 ];
   callbacks[ callback_index ].event.spatial.magneticField[ 0 ] =
      data[ 0 ]->magneticField[ 0 ];
   callbacks[ callback_index ].event.spatial.magneticField[ 1 ] =
      data[ 0 ]->magneticField[ 1 ];
   callbacks[ callback_index ].event.spatial.magneticField[ 2 ] =
      data[ 0 ]->magneticField[ 2 ];
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_servoposition_handler ...                                    */
/*---------------------------------------------------------------------*/
static int
bgl_servoposition_handler( CPhidgetServoHandle id, void *ptr, int index, double position ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servoposition.index = index;
   callbacks[ callback_index ].event.servoposition.position = position;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_advanced_servoposition_handler ...                           */
/*---------------------------------------------------------------------*/
static int
bgl_advanced_servoposition_handler( CPhidgetAdvancedServoHandle id, void *ptr, int index, double position ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servoposition.index = index;
   callbacks[ callback_index ].event.servoposition.position = position;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_advanced_servovelocity_handler ...                           */
/*---------------------------------------------------------------------*/
static int
bgl_advanced_servovelocity_handler( CPhidgetAdvancedServoHandle id, void *ptr, int index, double velocity ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servovelocity.index = index;
   callbacks[ callback_index ].event.servovelocity.velocity = velocity;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_advanced_servocurrent_handler ...                            */
/*---------------------------------------------------------------------*/
static int
bgl_advanced_servocurrent_handler( CPhidgetAdvancedServoHandle id, void *ptr, int index, double current ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servocurrent.index = index;
   callbacks[ callback_index ].event.servocurrent.current = current;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_stepperinput_handler ...                                     */
/*---------------------------------------------------------------------*/
static int
bgl_stepperinput_handler( CPhidgetStepperHandle id, void *ptr, int index, int istate ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.change.index = index;
   callbacks[ callback_index ].event.change.istate = istate;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_steppervelocity_handler ...                                  */
/*---------------------------------------------------------------------*/
static int
bgl_steppervelocity_handler( CPhidgetStepperHandle id, void *ptr, int index, double velocity ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servovelocity.index = index;
   callbacks[ callback_index ].event.servovelocity.velocity = velocity;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_stepperposition_handler ...                                  */
/*---------------------------------------------------------------------*/
static int
bgl_stepperposition_handler( CPhidgetStepperHandle id, void *ptr, int index, BGL_LONGLONG_T position ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.stepperposition.index = index;
   callbacks[ callback_index ].event.stepperposition.position = position;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_steppercurrent_handler ...                                   */
/*---------------------------------------------------------------------*/
static int
bgl_steppercurrent_handler( CPhidgetStepperHandle id, void *ptr, int index, double current ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servocurrent.index = index;
   callbacks[ callback_index ].event.servocurrent.current = current;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_error_handler ...                                            */
/*---------------------------------------------------------------------*/
static int bgl_error_handler( void *id, void *ptr, int ecode, const char *emsg ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.error.code = ecode;
   callbacks[ callback_index ].event.error.msg = strdup( emsg );
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_phidget_add_event_listener ...                       */
/*---------------------------------------------------------------------*/
int bgl_phidget_phidget_add_event_listener( CPhidgetHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "attach" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ATTACH );
      
      return CPhidget_set_OnAttach_Handler(
	 id, &bgl_phidget_handler, hdl );
   } else if( !strcmp( event, "detach" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_DETACH );
      
      return CPhidget_set_OnDetach_Handler(
	 id, &bgl_phidget_handler, hdl );
   } else if( !strcmp( event, "error" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ERROR );

      return CPhidget_set_OnError_Handler(
	 id, (int(*)(CPhidgetHandle, void *, int, const char *))&bgl_error_handler, hdl );
   } else if( !strcmp( event, "serverconnect" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SERVERCONNECT );
      
      return CPhidget_set_OnServerConnect_Handler(
	 id, &bgl_phidget_handler, hdl );
   } else if( !strcmp( event, "serverdisconnect" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SERVERDISCONNECT );
      
      return CPhidget_set_OnServerDisconnect_Handler(
	 id, &bgl_phidget_handler, hdl );
   }
      

   return EPHIDGET_INVALIDARG;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_manager_add_event_listener ...                       */
/*---------------------------------------------------------------------*/
int bgl_phidget_manager_add_event_listener( CPhidgetManagerHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "attach" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ATTACH );
      
      return CPhidgetManager_set_OnAttach_Handler(
	 id, (int (*)())(&bgl_phidget_handler), hdl );
   } else if( !strcmp( event, "detach" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_DETACH );
      
      return CPhidgetManager_set_OnDetach_Handler(
	 id, (int (*)())&bgl_phidget_handler, hdl );
   } else if( !strcmp( event, "error" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ERROR );
      
      return CPhidgetManager_set_OnError_Handler(
	 id, (int(*)(CPhidgetManagerHandle, void *, int, const char *))&bgl_error_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_ifkit_add_event_listener ...                         */
/*---------------------------------------------------------------------*/
int bgl_phidget_ifkit_add_event_listener( CPhidgetInterfaceKitHandle id, char *event, obj_t obj, obj_t proc ) {
   
   if( !strcmp( event, "inputchange" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_INPUTCHANGE );
      
      return CPhidgetInterfaceKit_set_OnInputChange_Handler(
	 id, &bgl_change_handler, hdl );
   } else if( !strcmp( event, "outputchange" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_OUTPUTCHANGE );
      
      return CPhidgetInterfaceKit_set_OnOutputChange_Handler(
	 id, &bgl_change_handler, hdl );
   } else if( !strcmp( event, "sensorchange" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SENSORCHANGE );
      
      return CPhidgetInterfaceKit_set_OnSensorChange_Handler(
	 id, &bgl_change_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_spatial_add_event_listener ...                       */
/*---------------------------------------------------------------------*/
int
bgl_phidget_spatial_add_event_listener( CPhidgetSpatialHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "spatialdata" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SPATIALDATA );
      
      return CPhidgetSpatial_set_OnSpatialData_Handler(
	 id, &bgl_spatial_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_servo_add_event_listener ...                         */
/*---------------------------------------------------------------------*/
int
bgl_phidget_servo_add_event_listener( CPhidgetServoHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "position" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SERVOPOSITION );
      
      return CPhidgetServo_set_OnPositionChange_Handler(
	 id, &bgl_servoposition_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_advanced_servo_add_event_listener ...                */
/*---------------------------------------------------------------------*/
int
bgl_phidget_advanced_servo_add_event_listener( CPhidgetAdvancedServoHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "position" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SERVOPOSITION );
      
      return CPhidgetAdvancedServo_set_OnPositionChange_Handler(
	 id, &bgl_advanced_servoposition_handler, hdl );
   }
   if( !strcmp( event, "velocity" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SERVOVELOCITY );
      
      return CPhidgetAdvancedServo_set_OnVelocityChange_Handler(
	 id, &bgl_advanced_servovelocity_handler, hdl );
   }
   if( !strcmp( event, "current" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_SERVOCURRENT );
      
      return CPhidgetAdvancedServo_set_OnCurrentChange_Handler(
	 id, &bgl_advanced_servocurrent_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_stepper_add_event_listener ...                       */
/*---------------------------------------------------------------------*/
int
bgl_phidget_stepper_add_event_listener( CPhidgetStepperHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "input" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_STEPPERINPUT );

      return CPhidgetStepper_set_OnInputChange_Handler(
	 id, &bgl_stepperinput_handler, hdl );
   }
   if( !strcmp( event, "velocity" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_STEPPERVELOCITY );

      return CPhidgetStepper_set_OnVelocityChange_Handler(
	 id, &bgl_steppervelocity_handler, hdl );
   }
   if( !strcmp( event, "position" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_STEPPERPOSITION );

      return CPhidgetStepper_set_OnPositionChange_Handler(
	 id, &bgl_stepperposition_handler, hdl );
   }
   if( !strcmp( event, "current" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_STEPPERCURRENT );

      return CPhidgetStepper_set_OnCurrentChange_Handler(
	 id, &bgl_steppercurrent_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}

/*---------------------------------------------------------------------*/
/*    FIELD_GET_STRING ...                                             */
/*---------------------------------------------------------------------*/
#define FIELD_GET_STRING( phid, field ) { \
   char *name; \
   int i = CPhidget_get##field( phid, (const char **)&name ); \
 \
   return i ? BUNSPEC : string_to_bstring( name ); \
}
   
/*---------------------------------------------------------------------*/
/*    FIELD_GET_INT ...                                                */
/*---------------------------------------------------------------------*/
#define FIELD_GET_INT( phid, field ) { \
   int i; \
   int r = CPhidget_get##field( phid, &i ); \
 \
   return r ? -1 : i; \
}
   
/*---------------------------------------------------------------------*/
/*    FIELD_GET                                                        */
/*---------------------------------------------------------------------*/
#define FIELD_GET( _phid, _field, _atype, _rtype, _o ) { \
   _rtype _v; \
   int _r = _atype##_get##_field( _phid, &_v ); \
   return _r == EPHIDGET_OK ? \
      _v : (bgl_phidget_error( "" # _field "", _r, _o ), _v); \
}
   
/*---------------------------------------------------------------------*/
/*    FIELD_INDEXED_GET                                                */
/*---------------------------------------------------------------------*/
#define FIELD_INDEXED_GET( _phid, _field, _atype, _rtype, _i, _o ) { \
   _rtype _v; \
   int _r = _atype##_get##_field( _phid, _i, &_v ); \
   return _r == EPHIDGET_OK ? \
      _v : (bgl_phidget_error( "" # _field "", _r, _o ), _v); \
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_phidget_get_device_name ...                                  */
/*---------------------------------------------------------------------*/
obj_t bgl_phidget_get_device_name( CPhidgetHandle phid ) {
   FIELD_GET_STRING( phid, DeviceName );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_get_serial_number ...                                */
/*---------------------------------------------------------------------*/
int bgl_phidget_get_serial_number( CPhidgetHandle phid ) {
   FIELD_GET_INT( phid, SerialNumber );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_get_device_version ...                               */
/*---------------------------------------------------------------------*/
int bgl_phidget_get_device_version( CPhidgetHandle phid ) {
   FIELD_GET_INT( phid, DeviceVersion );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_phidget_get_device_type ...                                  */
/*---------------------------------------------------------------------*/
obj_t bgl_phidget_get_device_type( CPhidgetHandle phid ) {
   FIELD_GET_STRING( phid, DeviceType );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_phidget_get_device_id ...                                    */
/*---------------------------------------------------------------------*/
obj_t bgl_phidget_get_device_id( CPhidgetHandle phid ) {
   CPhidget_DeviceID id;
   int i = CPhidget_getDeviceID( phid, &id );

   if( i ) {
      return BUNSPEC;
   } else {
      switch( i ) {
	 case PHIDID_ACCELEROMETER_3AXIS:
	    return string_to_bstring( "Phidget 3-axis Accelerometer (1059)" );
	 case PHIDID_ADVANCEDSERVO_1MOTOR:
	    return string_to_bstring( "Phidget 1 Motor Advanced Servo (1066)" );
	 case PHIDID_ADVANCEDSERVO_8MOTOR:
	    return string_to_bstring( "Phidget 8 Motor Advanced Servo (1061)" );
	 case PHIDID_BIPOLAR_STEPPER_1MOTOR:
	    return string_to_bstring( "Phidget 1 Motor Bipolar Stepper Controller with 4 Digital Inputs (1063)" );
	 case PHIDID_ENCODER_1ENCODER_1INPUT:
	    return string_to_bstring( "Phidget Encoder - Mechanical (1052)" );
	 case PHIDID_ENCODER_HS_1ENCODER:
	    return string_to_bstring( "Phidget High Speed Encoder (1057)" );
	 case PHIDID_ENCODER_HS_4ENCODER_4INPUT:
	    return string_to_bstring( "Phidget High Speed Encoder - 4 Encoder (1047)" );
	 case PHIDID_INTERFACEKIT_0_0_4:
	    return string_to_bstring( "Phidget Interface Kit 0/0/4 (1014)" );
	 case PHIDID_INTERFACEKIT_0_0_8:
	    return string_to_bstring( "Phidget Interface Kit 0/0/8 (1017)" );
	 case PHIDID_INTERFACEKIT_0_16_16:
	    return string_to_bstring( "Phidget Interface Kit 0/16/16 (1012)" );
	 case PHIDID_INTERFACEKIT_8_8_8:
	    return string_to_bstring( "Phidget Interface Kit 8/8/8 (1013, 1018, 1019)" );
	 case PHIDID_INTERFACEKIT_8_8_8_w_LCD:
	    return string_to_bstring( "Phidget Interface Kit 8/8/8 with TextLCD (1201, 1202, 1203)" );
	 case PHIDID_IR:
	    return string_to_bstring( "Phidget IR Receiver Transmitter (1055)" );
	 case PHIDID_LED_64:
	    return string_to_bstring( "Phidget LED 64 (1030)" );
	 case PHIDID_LED_64_ADV:
	    return string_to_bstring( "Phidget LED 64 Advanced (1031)" );
	 case PHIDID_LINEAR_TOUCH:
	    return string_to_bstring( "Phidget Linear Touch (1015)" );
	 case PHIDID_MOTORCONTROL_HC_2MOTOR:
	    return string_to_bstring( "Phidget 2 Motor High Current Motor Controller (1064)" );
	 case PHIDID_MOTORCONTROL_LV_2MOTOR_4INPUT:
	    return string_to_bstring( "Phidget 2 Motor Low Voltage Motor Controller with 4 Digital Inputs (1060)" );
	 case PHIDID_PHSENSOR:
	    return string_to_bstring( "Phidget PH Sensor (1058)" );
	 case PHIDID_RFID_2OUTPUT:
	    return string_to_bstring( "Phidget RFID with Digital Outputs and Onboard LED (1023)" );
	 case PHIDID_ROTARY_TOUCH:
	    return string_to_bstring( "Phidget Rotary Touch (1016)" );
	 case PHIDID_SERVO_1MOTOR:
	    return string_to_bstring( "Phidget 1 Motor Servo Controller (1000)" );
	 case PHIDID_SPATIAL_ACCEL_3AXIS:
	    return string_to_bstring( "Phidget Spatial 3-axis accel (1049)" );
	 case PHIDID_SPATIAL_ACCEL_GYRO_COMPASS:
	    return string_to_bstring( "Phidget Spatial 3/3/3 (1056)" );
	 case PHIDID_TEMPERATURESENSOR:
	    return string_to_bstring( "Phidget Temperature Sensor (1051)" );
	 case PHIDID_TEMPERATURESENSOR_4:
	    return string_to_bstring( "Phidget Temperature Sensor 4-input (1048)" );
	 case PHIDID_TEXTLCD_2x20_w_8_8_8:
	    return string_to_bstring( "Phidget TextLCD with Interface Kit 8/8/8 (1201, 1202, 1203)" );
	 case PHIDID_UNIPOLAR_STEPPER_4MOTOR:
	    return string_to_bstring( "Phidget 4 Motor Unipolar Stepper Controller (1062)" );
	 case PHIDID_ACCELEROMETER_2AXIS:
	    return string_to_bstring( "Phidget 2-axis Accelerometer (1053, 1054)" );
	 case PHIDID_INTERFACEKIT_0_8_8_w_LCD:
	    return string_to_bstring( "Phidget Interface Kit 0/8/8 with TextLCD (1219, 1220, 1221)" );
	 case PHIDID_INTERFACEKIT_4_8_8:
	    return string_to_bstring( "Phidget Interface Kit 4/8/8" );
	 case PHIDID_RFID:
	    return string_to_bstring( "Phidget RFID without Digital Outputs" );
	 case PHIDID_SERVO_1MOTOR_OLD:
	    return string_to_bstring( "Phidget 1 Motor Servo Controller - Old Version" );
	 case PHIDID_SERVO_4MOTOR:
	    return string_to_bstring( "Phidget 4 Motor Servo Controller (1001)" );
	 case PHIDID_SERVO_4MOTOR_OLD:
	    return string_to_bstring( "Phidget 4 Motor Servo Controller - Old Version" );
	 case PHIDID_TEXTLCD_2x20:
	    return string_to_bstring( "Phidget TextLCD without Interface Kit (1210)" );
	 case PHIDID_TEXTLCD_2x20_w_0_8_8:
	    return string_to_bstring( "Phidget TextLCD with Interface Kit 0/8/8 (1219, 1220, 1221)" );
	 case PHIDID_TEXTLED_1x8:
	    return string_to_bstring( "Phidget TextLED 1x8" );
	 case PHIDID_TEXTLED_4x8:
	    return string_to_bstring( "Phidget TextLED 4x8 (1040)" );
	 case PHIDID_WEIGHTSENSOR:
	    return string_to_bstring( "Phidget Weight Sensor (1050) " );
	 default: return BUNSPEC;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_phidget_get_server_id ...                                    */
/*---------------------------------------------------------------------*/
obj_t bgl_phidget_get_server_id( CPhidgetHandle phid ) {
   FIELD_GET_STRING( phid, ServerID );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_spatial_get_datarate ...                             */
/*---------------------------------------------------------------------*/
int
bgl_phidget_spatial_get_datarate( CPhidgetSpatialHandle phid, obj_t o ) {
   FIELD_GET( phid, DataRate, CPhidgetSpatial, int, o );
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_spatial_get_datarate_min ...                         */
/*---------------------------------------------------------------------*/
int
bgl_phidget_spatial_get_datarate_min( CPhidgetSpatialHandle phid, obj_t o ) {
   FIELD_GET( phid, DataRateMin, CPhidgetSpatial, int, o );
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_spatial_get_datarate_max ...                         */
/*---------------------------------------------------------------------*/
int
bgl_phidget_spatial_get_datarate_max( CPhidgetSpatialHandle phid, obj_t o ) {
   FIELD_GET( phid, DataRateMax, CPhidgetSpatial, int, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_servo_get_motor_count ...                            */
/*---------------------------------------------------------------------*/
int
bgl_phidget_servo_get_motor_count( CPhidgetServoHandle phid , obj_t o ) {
   FIELD_GET( phid, MotorCount, CPhidgetServo, int, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_servo_get_position ...                               */
/*---------------------------------------------------------------------*/
double bgl_phidget_servo_get_position( CPhidgetServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Position, CPhidgetServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_servo_get_position_max ...                           */
/*---------------------------------------------------------------------*/
double bgl_phidget_servo_get_position_max( CPhidgetServoHandle phid, int i, obj_t o ) { 
   FIELD_INDEXED_GET( phid, PositionMax, CPhidgetServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_servo_get_position_min ...                           */
/*---------------------------------------------------------------------*/
double bgl_phidget_servo_get_position_min( CPhidgetServoHandle phid, int i, obj_t o ) { 
   FIELD_INDEXED_GET( phid, PositionMin, CPhidgetServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_servo_get_engaged ...                                */
/*---------------------------------------------------------------------*/
int bgl_phidget_servo_get_engaged( CPhidgetServoHandle phid, int i, obj_t o ) { 
   FIELD_INDEXED_GET( phid, Engaged, CPhidgetServo, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    CPhidget_ServoType                                               */
/*    bgl_phidget_servo_get_servo_type ...                             */
/*---------------------------------------------------------------------*/
CPhidget_ServoType
bgl_phidget_servo_get_servo_type( CPhidgetServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, ServoType, CPhidgetServo, CPhidget_ServoType, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_advanced_servo_get_motor_count ...                   */
/*---------------------------------------------------------------------*/
int
bgl_phidget_advanced_servo_get_motor_count( CPhidgetAdvancedServoHandle phid, obj_t o ) {
   FIELD_GET( phid, MotorCount, CPhidgetAdvancedServo, int, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_acceleration ...                  */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_acceleration( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Acceleration, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_acceleration_max ...              */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_acceleration_max( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, AccelerationMax, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_acceleration_min ...              */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_acceleration_min( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, AccelerationMin, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_velocity_limit ...                */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_velocity_limit( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, VelocityLimit, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_velocity ...                      */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_velocity( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Velocity, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_velocity_max ...                  */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_velocity_max( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, VelocityMax, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_velocity_min ...                  */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_velocity_min( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, VelocityMin, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_position ...                      */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_position( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Position, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_position_max ...                  */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_position_max( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, PositionMax, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_position_min ...                  */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_position_min( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, PositionMin, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_advanced_servo_get_current ...                       */
/*---------------------------------------------------------------------*/
double
bgl_phidget_advanced_servo_get_current( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Current, CPhidgetAdvancedServo, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_advanced_servo_get_engaged ...                       */
/*---------------------------------------------------------------------*/
int
bgl_phidget_advanced_servo_get_engaged( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Engaged, CPhidgetAdvancedServo, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_advanced_servo_get_stopped ...                       */
/*---------------------------------------------------------------------*/
int
bgl_phidget_advanced_servo_get_stopped( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Stopped, CPhidgetAdvancedServo, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_advanced_servo_get_speed_ramping_on ...              */
/*---------------------------------------------------------------------*/
int
bgl_phidget_advanced_servo_get_speed_ramping_on( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, SpeedRampingOn, CPhidgetAdvancedServo, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    CPhidget_ServoType                                               */
/*    bgl_phidget_advanced_servo_get_servo_type ...                    */
/*---------------------------------------------------------------------*/
CPhidget_ServoType
bgl_phidget_advanced_servo_get_servo_type( CPhidgetAdvancedServoHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, ServoType, CPhidgetAdvancedServo, CPhidget_ServoType, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_stepper_get_input_count ...                          */
/*---------------------------------------------------------------------*/
int
bgl_phidget_stepper_get_input_count( CPhidgetStepperHandle phid, obj_t o ) {
   FIELD_GET( phid, InputCount, CPhidgetStepper, int, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_stepper_get_input_state ...                          */
/*---------------------------------------------------------------------*/
int
bgl_phidget_stepper_get_input_state( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, InputState, CPhidgetStepper, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_stepper_get_motor_count ...                          */
/*---------------------------------------------------------------------*/
int
bgl_phidget_stepper_get_motor_count( CPhidgetStepperHandle phid, obj_t o ) {
   FIELD_GET( phid, MotorCount, CPhidgetStepper, int, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_acceleration ...                         */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_acceleration( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Acceleration, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_acceleration_max ...                     */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_acceleration_max( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, AccelerationMax, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_acceleration_min ...                     */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_acceleration_min( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, AccelerationMin, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_velocity_limit ...                       */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_velocity_limit( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, VelocityLimit, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_velocity ...                             */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_velocity( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Velocity, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_velocity_max ...                         */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_velocity_max( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, VelocityMax, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_velocity_min ...                         */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_velocity_min( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, VelocityMin, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_phidget_stepper_get_target_position ...                      */
/*---------------------------------------------------------------------*/
BGL_LONGLONG_T
bgl_phidget_stepper_get_target_position( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, TargetPosition, CPhidgetStepper, BGL_LONGLONG_T, i, o );
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_phidget_stepper_get_current_position ...                     */
/*---------------------------------------------------------------------*/
BGL_LONGLONG_T
bgl_phidget_stepper_get_current_position( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, CurrentPosition, CPhidgetStepper, BGL_LONGLONG_T, i, o );
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_phidget_stepper_get_position_max ...                         */
/*---------------------------------------------------------------------*/
BGL_LONGLONG_T
bgl_phidget_stepper_get_position_max( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, PositionMax, CPhidgetStepper, BGL_LONGLONG_T, i, o );
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_phidget_stepper_get_position_min ...                         */
/*---------------------------------------------------------------------*/
BGL_LONGLONG_T
bgl_phidget_stepper_get_position_min( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, PositionMin, CPhidgetStepper, BGL_LONGLONG_T, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_current_limit ...                        */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_current_limit( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, CurrentLimit, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_current ...                              */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_current( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Current, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_current_max ...                          */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_current_max( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, CurrentMax, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    bgl_phidget_stepper_get_current_min ...                          */
/*---------------------------------------------------------------------*/
double
bgl_phidget_stepper_get_current_min( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, CurrentMin, CPhidgetStepper, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_stepper_get_engaged ...                              */
/*---------------------------------------------------------------------*/
int
bgl_phidget_stepper_get_engaged( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Engaged, CPhidgetStepper, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_stepper_get_stopped ...                              */
/*---------------------------------------------------------------------*/
int
bgl_phidget_stepper_get_stopped( CPhidgetStepperHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Stopped, CPhidgetStepper, int, i, o );
}


/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_motor_controlvelocity_handler ...                            */
/*---------------------------------------------------------------------*/
static int
bgl_motor_controlvelocity_handler( CPhidgetMotorControlHandle id, void *ptr, int index, double velocity ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servovelocity.index = index;
   callbacks[ callback_index ].event.servovelocity.velocity = velocity;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_motor_controlcurrent_handler ...                             */
/*---------------------------------------------------------------------*/
static int
bgl_motor_controlcurrent_handler( CPhidgetMotorControlHandle id, void *ptr, int index, double current ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.servocurrent.index = index;
   callbacks[ callback_index ].event.servocurrent.current = current;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_add_event_listener ...                 */
/*---------------------------------------------------------------------*/
int
bgl_phidget_motor_control_add_event_listener( CPhidgetMotorControlHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "velocity" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_MOTORCONTROLVELOCITY );

      return CPhidgetMotorControl_set_OnVelocityChange_Handler(
	 id, &bgl_motor_controlvelocity_handler, hdl );
   }
   if( !strcmp( event, "current" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_MOTORCONTROLCURRENT );

      return CPhidgetMotorControl_set_OnCurrentChange_Handler(
	 id, &bgl_motor_controlcurrent_handler, hdl );
   } else {
      return bgl_phidget_phidget_add_event_listener(
	 (CPhidgetHandle)id, event, obj, proc );
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_motor_count ...                    */
/*---------------------------------------------------------------------*/
int
bgl_phidget_motor_control_get_motor_count( CPhidgetMotorControlHandle phid, obj_t o ) {
  int err, count;

  err = CPhidgetMotorControl_getMotorCount( phid, &count );

  return err == EPHIDGET_OK
    ? count
    : (bgl_phidget_error( "MotorCount", err, o ), count);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_encoder_count ...                  */
/*---------------------------------------------------------------------*/
int
bgl_phidget_motor_control_get_encoder_count( CPhidgetMotorControlHandle phid, obj_t o ) {
  int err, count;

  err = CPhidgetMotorControl_getEncoderCount( phid, &count );

  return err == EPHIDGET_OK
    ? count
    : (bgl_phidget_error( "EncoderCount", err, o ), count);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_acceleration ...                   */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_get_acceleration( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Acceleration, CPhidgetMotorControl, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_velocity ...                       */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_get_velocity( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Velocity, CPhidgetMotorControl, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_acceleration_min ...               */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_get_acceleration_min( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, AccelerationMin, CPhidgetMotorControl, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_acceleration_max ...               */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_get_acceleration_max( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, AccelerationMax, CPhidgetMotorControl, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_encoder_position ...               */
/*---------------------------------------------------------------------*/
int
bgl_phidget_motor_control_get_encoder_position( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, EncoderPosition, CPhidgetMotorControl, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_braking ...                        */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_get_braking( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Braking, CPhidgetMotorControl, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_set_braking ...                        */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_set_braking( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Braking, CPhidgetMotorControl, double, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_motor_control_get_current ...                        */
/*---------------------------------------------------------------------*/
double
bgl_phidget_motor_control_get_current( CPhidgetMotorControlHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Current, CPhidgetMotorControl, double, i, o );
}



/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_encoderinput_handler ...                                     */
/*---------------------------------------------------------------------*/
static int
bgl_encoderinput_handler( CPhidgetEncoderHandle id, void *ptr, int index, int state ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.encoderinput.index = index;
   callbacks[ callback_index ].event.encoderinput.state = state;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_encoderposition_handler ...                                  */
/*---------------------------------------------------------------------*/
static int
bgl_encoderposition_handler( CPhidgetEncoderHandle id, void *ptr, int index, int time, int pos ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.encoderposition.index = index;
   callbacks[ callback_index ].event.encoderposition.time = time;
   callbacks[ callback_index ].event.encoderposition.position = pos;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_encoderindex_handler ...                                     */
/*---------------------------------------------------------------------*/
static int
bgl_encoderindex_handler( CPhidgetEncoderHandle id, void *ptr, int index, int position ) {
   bgl_phidget_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index ].event.encoderindex.index = index;
   callbacks[ callback_index ].event.encoderindex.position = position;
   callbacks[ callback_index++ ].handler = (struct handler *)ptr;

   bgl_phidget_signal();
   bgl_phidget_unlock();

   return 0;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_add_event_listener ...                       */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_add_event_listener( CPhidgetEncoderHandle id, char *event, obj_t obj, obj_t proc ) {
   if( !strcmp( event, "input" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ENCODERINPUT );

      return CPhidgetEncoder_set_OnInputChange_Handler(
	 id, &bgl_encoderinput_handler, hdl );
   }
   else if( !strcmp( event, "position" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ENCODERPOSITION );

      return CPhidgetEncoder_set_OnPositionChange_Handler(
	 id, &bgl_encoderposition_handler, hdl );
   }
   else if( !strcmp( event, "index" ) ) {
      struct handler *hdl = bgl_add_handler( obj, proc, EVENT_ENCODERINDEX );

      return CPhidgetEncoder_set_OnIndex_Handler(
	 id, &bgl_encoderindex_handler, hdl );
   }
   return -1;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_get_input_count ...                          */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_get_input_count( CPhidgetEncoderHandle phid, obj_t o ) {
  int err, count;

  err = CPhidgetEncoder_getInputCount( phid, &count);
  return err == EPHIDGET_OK
    ? count
    : (bgl_phidget_error( "EncoderCount", err, o ), count);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_get_encoder_count ...                        */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_get_encoder_count( CPhidgetEncoderHandle phid, obj_t o ) {
  int err, count;

  err = CPhidgetEncoder_getEncoderCount( phid, &count);
  return err == EPHIDGET_OK
    ? count
    : (bgl_phidget_error( "EncoderCount", err, o ), count);
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_get_input_enabled ...                        */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_get_input_enabled( CPhidgetEncoderHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, InputState, CPhidgetEncoder, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_get_position ...                             */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_get_position( CPhidgetEncoderHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Position, CPhidgetEncoder, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_get_index_position ...                       */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_get_index_position( CPhidgetEncoderHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, IndexPosition, CPhidgetEncoder, int, i, o );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_get_enabled ...                             */
/*---------------------------------------------------------------------*/
int
bgl_phidget_encoder_get_enabled( CPhidgetEncoderHandle phid, int i, obj_t o ) {
   FIELD_INDEXED_GET( phid, Enabled, CPhidgetEncoder, int, i, o );
}


/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_enable ...                                   */
/*---------------------------------------------------------------------*/
void
bgl_phidget_encoder_enable( CPhidgetEncoderHandle phid, int i ) {
   CPhidgetEncoder_setEnabled( phid, i, PTRUE );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_phidget_encoder_disable ...                                  */
/*---------------------------------------------------------------------*/
void
bgl_phidget_encoder_disable( CPhidgetEncoderHandle phid, int i ) {
   CPhidgetEncoder_setEnabled( phid, i, PFALSE );
}
