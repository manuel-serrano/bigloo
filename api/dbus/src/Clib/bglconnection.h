#ifndef _CONNECTIONH_
#define _CONNECTIONH_
#include <dbus/dbus.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <bigloo.h>

typedef enum {SESSION, SYSTEM} dbusType;


typedef struct {
  DBusConnection* conn;
  char* serviceName;
  char* path;
  char* interface;
  char* uniqueName;
} BUS;

typedef struct {
  BUS* bus;
  char* serviceName;
} SERVICE;

typedef struct {
  SERVICE* service;
  char* path;
  char* interface;
} OBJECT;

obj_t setPathConnection (BUS* , const char*);

obj_t setInterfaceConnection (BUS* , const char*);

BUS* connectBus (dbusType);

BUS* connectRemoteBus (const char*);

void closeBus (BUS*);

obj_t isConnected (BUS*);

obj_t serviceNameHasOwner(BUS*, char*);

SERVICE* handleService (BUS*, char*);

OBJECT* handleObject (SERVICE*, char*, char*);

obj_t unsubscribe(const char*, BUS *);
obj_t subscribe(const char*, BUS *);
obj_t releaseServiceName (BUS*);
obj_t setServiceName (BUS*, char*, unsigned int);

char *getCopyString (const char*);

DBusError *initError (void);

#endif
