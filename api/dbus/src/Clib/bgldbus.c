/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/dbus/src/Clib/bgldbus.c          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov  3 17:45:55 2009                          */
/*    Last change :  Sun Nov  8 14:46:44 2009 (serrano)                */
/*    Copyright   :  2009 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    The C part of the Bigloo dbus binding.                           */
/*=====================================================================*/
#include <dbus/dbus.h>
#include <dbus/dbus-shared.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <bigloo.h>
#include "bgldbus.h"

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    DBUS_CHECK_NULL ...                                              */
/*---------------------------------------------------------------------*/
#define DBUS_CHECK_NULL( conn, proc, obj ) \
   if( !conn ) bgl_dbus_error( proc, "Illegal NULL connection", obj )

/*---------------------------------------------------------------------*/
/*    DBusConnection *                                                 */
/*    bgl_dbus_bus_get_private ...                                     */
/*---------------------------------------------------------------------*/
DBusConnection *
bgl_dbus_bus_get_private( DBusBusType type ) {
   DBusConnection *conn;
   
   DBusError err;
   dbus_error_init( &err );

   conn = dbus_bus_get_private( type, &err );

   if( dbus_error_is_set( &err ) ) {
      bgl_dbus_error( "dbus-connect",
		      err.message,
		      type == DBUS_BUS_SESSION ? "session" : "system" );
   }

   return conn;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_dbus_bus_name_has_owner ...                                  */
/*---------------------------------------------------------------------*/
bool_t
bgl_dbus_bus_name_has_owner( DBusConnection *conn, char *name, obj_t obj ) {
   DBusError err;
   
   DBUS_CHECK_NULL( conn, "dbus-bus-has-owner?", obj );

   dbus_error_init( &err );

   if( dbus_bus_name_has_owner( conn, name, &err ) ) {
      return 1;
   } else {
      if( dbus_error_is_set( &err ) ) {
	 bgl_dbus_error( "dbus-bus-has-owner?", err.message, obj );
      }

      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_dbus_bus_set_name ...                                        */
/*---------------------------------------------------------------------*/
bool_t
bgl_dbus_bus_set_name( DBusConnection *conn, char *name, obj_t obj ) {
   DBusError err;

   DBUS_CHECK_NULL( conn, "dbus-bus-name-set!", obj );
   
   dbus_error_init( &err );

   TODO TODO
      
   if( dbus_bus_release_name( conn, name, &err ) ) {
      return 1;
   } else {
      if( dbus_error_is_set( &err ) ) {
	 bgl_dbus_error( "dbus-bus-name-set!", err.message, obj );
      }

      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_dbus_bus_release_name ...                                    */
/*---------------------------------------------------------------------*/
bool_t
bgl_dbus_bus_release_name( DBusConnection *conn, char *name, obj_t obj ) {
   DBusError err;

   DBUS_CHECK_NULL( conn, "dbus-bus-name-release!", obj );
   
   dbus_error_init( &err );

   if( dbus_bus_release_name( conn, name, &err ) ) {
      return 1;
   } else {
      if( dbus_error_is_set( &err ) ) {
	 bgl_dbus_error( "dbus-bus-name-release!", err.message, obj );
      }

      return 0;
   }
}
