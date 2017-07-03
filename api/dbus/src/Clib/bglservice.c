#include "bglservice.h"
#include <bigloo.h>

obj_t getMethodObject(OBJECT* object, int timeout) {
  if (object != NULL) {
    char * saveInterface;
    MESSAGE* reply;
    obj_t item;

    saveInterface = object->interface;
    
    object->interface = getCopyString ("org.freedesktop.DBus.Introspectable");
    if (object->interface==NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the interface string"),
		string_to_bstring("org.freedesktop.DBus.Introspect"));
      return NULL;
    }

    reply = sendMethodCall (object, "Introspect", timeout, 1, BNIL);
    object->interface=saveInterface;
 
    if (reply==NULL) {
      bgl_error(string_to_bstring("Error occured while sending the method call"),
		string_to_bstring(""));
      return NULL;
    }
    
    item = getArgsMessage (reply);

    if (! NULLP(item)) {
      return CAR (item);
    }
    else {
      bgl_error(string_to_bstring("Error occured while getting arguments from the reply message"),
		item);
      return NULL;
    }
  }
  else {
    bgl_error(string_to_bstring("Parameter object was NULL"),  string_to_bstring(""));
    return NULL;
  }
  
}

int sendMethodCallReturn (BUS *bus, MESSAGE *replyTo, obj_t args) {
  if (bus != NULL && replyTo != NULL) {
    DBusMessage *msg;
    int serial=0;
    
    /* dbus_message_new_method_return (DBusMessage *reply_to) */
    msg = dbus_message_new_method_return (replyTo->message);

    if (msg == NULL) {
      bgl_error(string_to_bstring("Error occured while creating the return message"),
		string_to_bstring(""));
      return 0;
    }

    /* Ajout des arguments au message */
    if ( ! NULLP(args) ) {
      appendArgsFromScmList (msg, args);
    }

    /* Envoi du message */
    if (!dbus_connection_send (bus->conn, msg, &serial)) {
      bgl_error(string_to_bstring("Error occured while sending the return message"),
		string_to_bstring(""));
      return 0;
    }

    dbus_connection_flush(bus->conn);
  
    return serial;
  }
  else {
    bgl_error(string_to_bstring("One of the parameters the was NULL"),
	      string_to_bstring(""));
    return 0;
  }
}


int sendError (BUS *bus, MESSAGE *replyTo, const char *errorType, const char *errorMsg, obj_t args) {
  if (bus != NULL && replyTo != NULL && errorType != NULL && errorMsg != NULL) {
    DBusMessage *msg;
    int serial=0;
    
    msg = dbus_message_new_error (replyTo->message, errorType, errorMsg);

    if (msg == NULL) { 
      bgl_error(string_to_bstring("Error occured while creating the error message"),
		string_to_bstring(""));
      return 0;
    }

    /* Ajout des arguments au message */
    if (! NULLP(args)) {
      appendArgsFromScmList (msg, args);
    }

    /* Envoi du message */
    if (!dbus_connection_send (bus->conn, msg, &serial)) {
      bgl_error(string_to_bstring("Error occured while sending the error message"),
		string_to_bstring(""));
      return 0;
    }

    dbus_connection_flush(bus->conn);
  
    return serial;
  }
  else {
    bgl_error(string_to_bstring("One of the parameters was NULL"),
	      string_to_bstring(""));
    return 0;
  }
}

MESSAGE *sendMethodCall (OBJECT *obj, const char *method, int timeout, int rep, obj_t args) {
  if (obj != NULL && method != NULL) {
    DBusMessage *msg;
    DBusMessage *reply;
    MESSAGE *res;
    DBusError *err = initError();
    
    msg = dbus_message_new_method_call (obj->service->serviceName, obj->path, obj->interface, method);
    
    if (msg == NULL) {
      bgl_error(string_to_bstring("Error occured while creating the method call message"), 
		string_to_bstring((char *)method)); 
      return NULL;
    }
    
    if (!rep)
      dbus_message_set_no_reply (msg, TRUE);
    
    /* Ajout des arguments au message */
    if (! NULLP(args)) {
      appendArgsFromScmList (msg, args);
    }
    if (rep) {

      reply = dbus_connection_send_with_reply_and_block (obj->service->bus->conn, msg, timeout, err);

      if (dbus_error_is_set(err)) {
	bgl_error(string_to_bstring((char*)err->message),  string_to_bstring(""));
	return NULL;
      }
      
    } else {
      if (!dbus_connection_send (obj->service->bus->conn, msg, NULL)) {
	bgl_error(string_to_bstring((char*)err->message),  string_to_bstring(""));
	return NULL;
      }
      return NULL;
    }
    
    
    res = (MESSAGE *) GC_MALLOC (sizeof (MESSAGE));
    
    if (res==NULL){
      bgl_error(string_to_bstring("Error occured while allocating the response message"),
		string_to_bstring(""));
      return NULL;
    }
    
    res->message = reply;
    
    if (getCopyString (dbus_message_get_path (res->message)) != NULL) {
      
      
      res->path = getCopyString (dbus_message_get_path (res->message));
      if (res->path == NULL) {
	bgl_error(string_to_bstring("Error occured while allocating the path string"),
		  string_to_bstring(""));
	return NULL;
      }
      
    }
    
    if (getCopyString (dbus_message_get_interface (res->message))) {
      
      res->interface = getCopyString (dbus_message_get_interface (res->message));
      if (res->interface == NULL) {
	bgl_error(string_to_bstring("Error occured while allocating the interface string"),
		  string_to_bstring(""));
	return NULL;
      }
      
    }
    
    if (getCopyString (dbus_message_get_sender (res->message))) {
      res->sender = getCopyString (dbus_message_get_sender (res->message));
      if (res->sender == NULL) {
	bgl_error(string_to_bstring("Error occured while allocating the sender string"),
		  string_to_bstring(""));
	return NULL;
      }
    }
    dbus_connection_flush(obj->service->bus->conn);
    
    return res;
  }
  else {
    bgl_error(string_to_bstring("One of the parameters was NULL"),  string_to_bstring(""));
  }
}


int sendIntrospectReturn (BUS *bus, const char* path, MESSAGE *replyTo) {
  if (bus != NULL && path != NULL && replyTo != NULL) {
    DBusMessage *msg;
    int serial=0;
    int length;
    
    /* dbus_message_new_method_return (DBusMessage *reply_to) */
    msg = dbus_message_new_method_return (replyTo->message);

    if (msg == NULL) {
      bgl_error(string_to_bstring("Error occured while creating the method return message"),
		string_to_bstring(""));
      return 0;
    }
    
    FILE* fd=fopen(path, "r");
    if (!fd) {
      bgl_error(string_to_bstring("Error occured while opening XML file"),
		string_to_bstring((char *)path));
      return 0;
    }
    if (fseek(fd,0L,SEEK_END)==-1) {
      fclose(fd);
      bgl_error(string_to_bstring("Error fseek"),  string_to_bstring(""));
      return 0;
    }
    length = ftell(fd);
    if (fseek(fd,0L,SEEK_SET)==-1) {
      fclose(fd);
      bgl_error(string_to_bstring("Error fseek"),  string_to_bstring(""));
      return 0;
    }

    char* buff=(char*)malloc(length);
    if (buff==NULL) {
      fclose(fd);
      bgl_error(string_to_bstring("Error occured while allocating the buffer string"),
		string_to_bstring(""));
    return 0;
    }
    fread(buff, length, 1, fd);
    fclose (fd);
    
    appendArgsFromScmList (msg, make_pair (string_to_bstring (buff), BNIL));

    /* Envoi du message */
    if (!dbus_connection_send (bus->conn, msg, &serial)) {
      bgl_error(string_to_bstring("Error occured while sending method return message"),  string_to_bstring(""));
      return 0;
    }
    
    dbus_connection_flush(bus->conn);
    
    return serial;
  }
  else {
    bgl_error(string_to_bstring("One of the parameters was NULL"),  string_to_bstring(""));
    return 0;
  }
}

obj_t getMember (MESSAGE *msg) {
  const char *tmp = dbus_message_get_member (msg->message);
  if (tmp == NULL)
    return BFALSE;
  return string_to_bstring ((char *)tmp);
}

obj_t getSender (MESSAGE *msg) {
  const char *tmp = dbus_message_get_sender (msg->message);
  if (tmp == NULL)
    return BFALSE;
  return string_to_bstring ((char *)tmp);
}

