#ifndef _MESSAGEH_
#define _MESSAGEH_

#include "bglconnection.h"


typedef struct {
  const char *sender;
  const char *path;
  const char *interface;
  int type;
  DBusMessage* message;
} MESSAGE;


void appendArgsFromScmList ( DBusMessage*, obj_t );
void appendArgsFromScmListWithIter ( DBusMessageIter*, obj_t );

obj_t getScmListFromIter ( DBusMessageIter* );
obj_t getArgsMessage ( MESSAGE* );

MESSAGE* receiveMessageFromObject ( OBJECT*, int);
MESSAGE* receiveMessageFromBus ( BUS*, int);
MESSAGE* receiveMessageWithRule ( BUS*, int, const char *);

int sendSignal ( BUS*, const char *, obj_t);

obj_t getMessageType ( MESSAGE* );
obj_t getMessagePath ( MESSAGE* );

extern obj_t bgl_list_to_vector ( obj_t );
extern obj_t bgl_get_signature ( obj_t );
extern obj_t bgl_list_to_dbus_struct ( obj_t );
extern obj_t bgl_extract_list_from_struct ( obj_t );
extern obj_t bgl_get_sign_from_empty_arr ( obj_t );
extern obj_t bgl_is_dbus_structure ( obj_t );
extern obj_t bgl_is_dbus_empty_array (obj_t );
#endif
