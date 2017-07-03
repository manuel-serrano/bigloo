#ifndef _SERVICEH_
#define _SERVICEH_
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "bglmessage.h"

obj_t getMethodObject( OBJECT*, int);
int sendMethodCallReturn (BUS*, MESSAGE*, obj_t);
int sendError (BUS*, MESSAGE*, const char *, const char *, obj_t);
MESSAGE *sendMethodCall (OBJECT*, const char *, int, int, obj_t);
int sendIntrospectReturn (BUS*, const char*, MESSAGE*);
obj_t getMember (MESSAGE*);
obj_t getSender (MESSAGE*);

#endif
