/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/dbus/src/Clib/bgldbus.h          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Nov  3 17:26:48 2009                          */
/*    Last change :  Sun Nov  8 14:33:22 2009 (serrano)                */
/*    Copyright   :  2009 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    The C header file for bigloodbus.                                */
/*=====================================================================*/
#ifndef BGLDBUS_H 
#define BGLDBUS_H

#include <bigloo.h>

/*---------------------------------------------------------------------*/
/*    Exports                                                          */
/*---------------------------------------------------------------------*/
extern DBusConnection *bgl_dbus_bus_get_private( DBusBusType );
extern bool_t bgl_dbus_bus_name_has_owner( DBusConnection *, char *, obj_t );
extern bool_t bgl_dbus_bus_release_name( DBusConnection *, char *, obj_t );

#endif
