#include "bglconnection.h"

obj_t setPathConnection (BUS* bus, const char* path) {
  if (bus != NULL && path != NULL) {
    bus->path=getCopyString(path);
    if (bus->path == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating path string"),
		string_to_bstring(""));
      return BFALSE;
    }
    return BTRUE;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"), string_to_bstring(""));
    return BFALSE;
  }
}

obj_t setInterfaceConnection (BUS* bus, const char* interface) {
  if (bus != NULL && interface != NULL) {
    bus->interface=getCopyString(interface);
    if (bus->interface == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating interface string"),
		string_to_bstring((char *)interface));
      return BFALSE;
    }
    return BTRUE;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"), BNIL);
    return BFALSE;
  }
}

// connexion au bus suivant le type
BUS* connectBus (dbusType type) {
  BUS* res;
  DBusError* err=initError();
  res = (BUS*)GC_MALLOC (sizeof(BUS));
  if (res==NULL) {
    bgl_error(string_to_bstring("Error occured while allocating the bus"),
	      string_to_bstring(""));
    return NULL;
  }
  //connexion au bus avec authentification (permet de recreer la connexion si elle existe deja)

  if (type == SESSION)
    res->conn = dbus_bus_get_private(DBUS_BUS_SESSION, err);
  else
    res->conn = dbus_bus_get_private(DBUS_BUS_SYSTEM, err);

  if(dbus_error_is_set(err)) {
    bgl_error(string_to_bstring((char *)err->message), string_to_bstring(""));
    return NULL;
  }

  res->uniqueName = getCopyString (dbus_bus_get_unique_name (res->conn));
  if (res->uniqueName==NULL) {
   bgl_error(string_to_bstring("Error occured while allocating the unique name string"),
	     string_to_bstring(""));
    return NULL;
  }
  return res;
}

BUS* connectRemoteBus (const char * addr) {
  if (addr != NULL) {
    BUS* res;
    DBusError* err = initError();
    res = (BUS*)GC_MALLOC (sizeof(BUS));
    
    if (res==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the bus"),
		string_to_bstring((char *)addr));
      return NULL;
    }
    
    //connexion au bus avec authentification (permet de recreer la connexion si elle existe deja)
    res->conn=dbus_connection_open_private(addr, err);
    
    if(dbus_error_is_set(err)) {
      bgl_error(string_to_bstring((char *)err->message), string_to_bstring(""));
      return NULL;
    }
    
    res->uniqueName = getCopyString (dbus_bus_get_unique_name (res->conn));
    if (res->uniqueName==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the unique name string"),
		string_to_bstring(""));
      return NULL;
    }
    return res;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"),
	      string_to_bstring(""));
    return NULL;
  }
}

void closeBus (BUS* conn) {
  if (conn != NULL) {
    // connexion au bus avec authentification
    // (permet de recreer la connexion si elle existe deja)
    dbus_connection_close(conn->conn);
  }
  else bgl_error(string_to_bstring("Parameter bus  was NULL"), 
		 string_to_bstring(""));
}

obj_t isConnected (BUS* conn) {
  if (conn != NULL) {
    if ( dbus_connection_get_is_connected(conn->conn) )
      return BTRUE;
    else
      return BFALSE;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"),  string_to_bstring(""));
    return BFALSE;
  }
}

obj_t serviceNameHasOwner(BUS* conn, char* name) {
  DBusError* err = initError();
  int res;
  if (conn != NULL) {
    res = dbus_bus_name_has_owner(conn->conn, name, err);
    if (dbus_error_is_set (err)) {
      bgl_error(string_to_bstring((char *)err->message),  string_to_bstring(""));
      return BFALSE;
    }
    else {
      if (res)
	return BTRUE;
      else
	return BFALSE;
    }
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"),  string_to_bstring(""));
    return BFALSE;
  }
}

// assigne un serviceName au bus
obj_t setServiceName (BUS* bus, char * serviceName, unsigned int flagparam) {
  int flag;
  DBusError* err = initError();
  switch (flagparam) {
  case 1: flag = DBUS_NAME_FLAG_ALLOW_REPLACEMENT; break;
  case 2: flag = DBUS_NAME_FLAG_DO_NOT_QUEUE; break;
  case 4: flag = DBUS_NAME_FLAG_REPLACE_EXISTING; break;
  case 3: flag = DBUS_NAME_FLAG_DO_NOT_QUEUE | DBUS_NAME_FLAG_ALLOW_REPLACEMENT; break;
  case 5: flag = DBUS_NAME_FLAG_REPLACE_EXISTING | DBUS_NAME_FLAG_ALLOW_REPLACEMENT; break;
  case 6: flag = DBUS_NAME_FLAG_REPLACE_EXISTING | DBUS_NAME_FLAG_DO_NOT_QUEUE; break;
  case 7: flag = DBUS_NAME_FLAG_ALLOW_REPLACEMENT
	    | DBUS_NAME_FLAG_DO_NOT_QUEUE
	    | DBUS_NAME_FLAG_REPLACE_EXISTING; break;
  }
  if (bus!=NULL && serviceName!=NULL) {
    int res;
    res = dbus_bus_request_name(bus->conn, serviceName, flag, err);
    if (dbus_error_is_set (err)) {
      bgl_error(string_to_bstring((char *)err->message),  string_to_bstring(""));
      return BFALSE;
    }
    bus->serviceName=getCopyString(serviceName);
    if (bus->serviceName==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the service name string"),
		string_to_bstring((char *)serviceName));
      return BFALSE;
    }
    if ( res == -1 )
      return BFALSE;
    else
      return BTRUE;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"),  string_to_bstring(""));
    return BFALSE;
  }
}

// libere le serviceName du bus
obj_t releaseServiceName (BUS* bus) {
  DBusError* err = initError();
  if (bus!=NULL) {
    int res;
    res = dbus_bus_release_name(bus->conn, bus->serviceName, err);
    if (dbus_error_is_set(err)) {
      bgl_error(string_to_bstring((char *)err->message),  string_to_bstring(""));
      return BFALSE;
    }
    if ( res == -1 )
      return BFALSE;
    else
      return BTRUE;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"),  string_to_bstring(""));
    return BFALSE;
  }
}

SERVICE* handleService (BUS* bus, char* serviceName) {
  if (serviceName!=NULL && bus != NULL) {
    SERVICE* res;
    res = (SERVICE*) GC_MALLOC (sizeof(SERVICE));
    
    if (res==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating structure service"), 
		string_to_bstring(""));
      return NULL;
    }
    res->bus=bus;
    res->serviceName = getCopyString(serviceName);
    if (res->serviceName==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the service name string"),
		string_to_bstring(serviceName));
      return NULL;
    }
    return res;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"),  string_to_bstring(""));
    return NULL;
  }
}

OBJECT* handleObject (SERVICE* service, char* path, char* interface) {
  OBJECT* res;
  if (path!=NULL && interface !=NULL && service != NULL) {
    res = (OBJECT*) GC_MALLOC (sizeof(OBJECT));
    
    if (res==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the object"),
		string_to_bstring(""));
      return NULL;
    }
    
    res->service=service;
    res->path = getCopyString(path);
    if (res->path==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the path string"),
		string_to_bstring(path));
      return NULL;
    }
    res->interface = getCopyString(interface);
    if (res->interface==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the interface string"),
		string_to_bstring(interface));
      return NULL;
    }
    
    return res;
  }
  else {
    bgl_error(string_to_bstring("One of the parameters was NULL"),  string_to_bstring(""));
    return NULL;
  }
}


obj_t subscribe(const char* rule, BUS* bus) {
  if (rule != NULL && bus != NULL) {

    // abonnement aux messages correspondant a la regle
    DBusError* err = initError();

    dbus_bus_add_match(bus->conn, rule, err);
    
    if (dbus_error_is_set (err)) {
      bgl_error(string_to_bstring((char *)err->message),  string_to_bstring("")); 
      return BFALSE;
    }
    
    dbus_connection_flush(bus->conn);
    
    return BTRUE;
  }
  else {
    bgl_error(string_to_bstring("One of the parameter was NULL"),  string_to_bstring(""));
    return BFALSE;
  }
}


obj_t unsubscribe(const char* rule, BUS* bus) {
  if (rule != NULL && bus != NULL) {
    DBusError* err = initError();

    // suppression de l'abonnement
    dbus_bus_remove_match(bus->conn, rule, err);
    if (dbus_error_is_set (err)) {
      bgl_error(string_to_bstring((char *)err->message),  string_to_bstring("")); 
      return BFALSE;
    }
    
    dbus_connection_flush(bus->conn);
    return BTRUE;
  }
  else {
    bgl_error(string_to_bstring("One of the paramters was NULL"),  string_to_bstring(""));
    return BFALSE;
  }
}


char *getCopyString (const char *str) {
  if (str != NULL) { 
    char *res;
    
    res = (char *)GC_MALLOC (sizeof (char) * strlen (str) + 1);
    
    if (res == NULL) {
      return NULL;
    }

    return strcpy(res,str);
  }
  else {
    return "";
  }
}

DBusError *initError (void) {
  DBusError *res = (DBusError *) GC_MALLOC (sizeof (DBusError));
  dbus_error_init (res);
  return res;
}
