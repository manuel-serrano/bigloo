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

int setPathConnection (BUS* , const char*);

int setInterfaceConnection (BUS* , const char*);

BUS* connectBus (dbusType);

BUS* connectRemoteBus (const char*);

void closeBus (BUS*);

int isConnected (BUS*);

int serviceNameHasOwner(BUS*, char*);

SERVICE* handleService (BUS*, char*);

OBJECT* handleObject (SERVICE*, char*, char*);

int unsubscribe(const char*, BUS *);

int subscribe(const char*, BUS *);

int releaseServiceName (BUS*);

int setServiceName (BUS*, char*, unsigned int);

char *getCopyString (const char*);

DBusError *initError (void);

#endif
