#include "bglmessage.h"

obj_t getArgsMessage (MESSAGE* message) {

  if (message == NULL) {
    bgl_error(string_to_bstring("Parameter message is NULL"), BNIL);
    return BFALSE;
  }

  DBusMessage *mess = message->message;
  DBusMessageIter iter;
  obj_t res;

  dbus_message_iter_init (mess, &iter);

  res = getScmListFromIter (&iter);

  return res;
}



obj_t getBasicScmObjFromIter (DBusMessageIter *iter) {

  int type;
  type = dbus_message_iter_get_arg_type (iter);

  switch (type)
    {

    case DBUS_TYPE_BYTE:
      {
	char tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return BINT (tmp);
      }

    case DBUS_TYPE_DOUBLE:
      {
	double tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return make_real (tmp);
      }

    case DBUS_TYPE_INT16:
      {
	dbus_int16_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return BINT (tmp);
      }

    case DBUS_TYPE_UINT16:
      {
	dbus_uint16_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return BINT (tmp);
      }

    case DBUS_TYPE_INT32:
      {
	dbus_int32_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return BINT (tmp);
      }

    case DBUS_TYPE_UINT32:
      {
	dbus_uint32_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return BINT (tmp);
      }

    case DBUS_TYPE_INT64:
      {
	dbus_int64_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return LONG_TO_BLLONG (tmp);
      }

    case DBUS_TYPE_UINT64:
      {
	dbus_uint64_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return LONG_TO_BLLONG (tmp);
      }

    case DBUS_TYPE_STRING:
      {
	char* tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return string_to_bstring (tmp);
      }

    case DBUS_TYPE_SIGNATURE:
      {
	char* tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return string_to_bstring (tmp);
      }

    case DBUS_TYPE_OBJECT_PATH:
      {
	char* tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	return string_to_bstring (tmp);
      }

    case DBUS_TYPE_BOOLEAN:
      {
	dbus_bool_t tmp;
	dbus_message_iter_get_basic (iter, &tmp);
	if (tmp)
	  return BTRUE;
	else
	  return BFALSE;
      }

    case DBUS_TYPE_ARRAY:
      {
	DBusMessageIter subIter;
	dbus_message_iter_recurse (iter, &subIter);
	obj_t tmp =  getScmListFromIter (&subIter);
	return bgl_list_to_vector (tmp);
      }
      
    case DBUS_TYPE_DICT_ENTRY:
      {
	DBusMessageIter subIter;
	dbus_message_iter_recurse (iter, &subIter);
	return getScmListFromIter (&subIter);
      }
      
    case DBUS_TYPE_STRUCT:
      {
	DBusMessageIter subIter;
	dbus_message_iter_recurse (iter, &subIter);
	obj_t tmp =  getScmListFromIter (&subIter);
	return bgl_list_to_dbus_struct (tmp);
      }
      
    case DBUS_TYPE_VARIANT:
      {
	DBusMessageIter subIter;
	dbus_message_iter_recurse (iter, &subIter);
	return CAR ( getScmListFromIter (&subIter) );
      }
      
    }

  return BNIL;
}


obj_t getScmListFromIter (DBusMessageIter *iter) {

  if (dbus_message_iter_get_arg_type (iter) == DBUS_TYPE_INVALID)
    return BNIL;

  obj_t tmp = getBasicScmObjFromIter (iter);

  dbus_message_iter_next (iter);

  return make_pair ( tmp, getScmListFromIter (iter) );

}


void appendArgsFromScmList (DBusMessage *message, obj_t lst) {
  DBusMessageIter iter;
  dbus_message_iter_init_append (message, &iter);
  appendArgsFromScmListWithIter (&iter, lst);
}

void appendArgsFromScmListWithIter (DBusMessageIter *iter, obj_t lst) {

  if (NULLP (lst)) return;

  obj_t toAdd = CAR (lst);

  if (BOOLEANP (toAdd))
    {
      dbus_bool_t tmp;
      if (TRUEP (toAdd))
	tmp = TRUE;
      else
	tmp = FALSE;
      dbus_message_iter_append_basic (iter, DBUS_TYPE_BOOLEAN, &tmp);
    }

  else if (INTEGERP (toAdd))
    {
      dbus_int32_t tmp = CINT (toAdd);
      dbus_message_iter_append_basic (iter, DBUS_TYPE_INT32, &tmp);
    }

  else if (REALP (toAdd))
    {
      double tmp = REAL_TO_DOUBLE (toAdd);
      dbus_message_iter_append_basic (iter, DBUS_TYPE_DOUBLE, &tmp);
    }

  else if (STRINGP (toAdd))
    {
      char *tmp = BSTRING_TO_STRING (toAdd);
      dbus_message_iter_append_basic (iter, DBUS_TYPE_STRING, &tmp);
    }

  else if (bgl_is_dbus_structure (toAdd) == BTRUE)
    {
      DBusMessageIter subIter;
      char *sign = BSTRING_TO_STRING (bgl_get_signature (toAdd));
      obj_t list = bgl_extract_list_from_struct (toAdd);
      dbus_message_iter_open_container (iter, DBUS_TYPE_STRUCT, sign, &subIter);
      appendArgsFromScmListWithIter (&subIter, list);
      dbus_message_iter_close_container (iter, &subIter);
    }

  else if (bgl_is_dbus_empty_array (toAdd) == BTRUE)
    {
      DBusMessageIter subIter;
      char *sign = BSTRING_TO_STRING (bgl_get_sign_from_empty_arr (toAdd));
      dbus_message_iter_open_container (iter, DBUS_TYPE_ARRAY, sign, &subIter);
      dbus_message_iter_close_container (iter, &subIter);
    }

  else if (PAIRP (toAdd))
    {
      if (SYMBOLP (CAR (toAdd))) { // Conversion

	char *symb = BSTRING_TO_STRING (SYMBOL_TO_STRING (CAR (toAdd)));

	if (strcmp (symb, "byt") == 0) {
	  unsigned char tmp = CINT (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_BYTE, &tmp);
	}

	else if (strcmp (symb, "i16") == 0) {
	  dbus_int16_t tmp = (dbus_int16_t) CINT (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_INT16, &tmp);
	}

	else if (strcmp (symb, "u16") == 0) {
	  dbus_uint16_t tmp = (dbus_uint16_t) CINT (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT16, &tmp);
	}

	else if (strcmp (symb, "u32") == 0) {
	  dbus_uint32_t tmp = (dbus_uint32_t) CINT (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT32, &tmp);
	}

	else if (strcmp (symb, "i64") == 0) {
	  dbus_int64_t tmp = (dbus_int64_t) CINT (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_INT64, &tmp);
	}

	else if (strcmp (symb, "u64") == 0) {
	  dbus_uint64_t tmp = (dbus_uint64_t) CINT (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_UINT64, &tmp);
	}

	else if (strcmp (symb, "op") == 0) {
	  char *tmp = BSTRING_TO_STRING (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_OBJECT_PATH, &tmp);
	}

	else if (strcmp (symb, "sig") == 0) {
	  char *tmp = BSTRING_TO_STRING (CDR (toAdd));
	  dbus_message_iter_append_basic (iter, DBUS_TYPE_SIGNATURE, &tmp);
	}

	else if (strcmp (symb, "var") == 0) {
	  DBusMessageIter subIter;
	  char *sign = BSTRING_TO_STRING (bgl_get_signature (CDR (toAdd)));
	  dbus_message_iter_open_container (iter, DBUS_TYPE_VARIANT, sign, &subIter);
	  appendArgsFromScmListWithIter (&subIter, make_pair ( CDR (toAdd),
							       BNIL ));
	  dbus_message_iter_close_container (iter, &subIter);
	}

	else {
	  bgl_error(string_to_bstring("Unknown type of argument"),
		    toAdd);
	}
      }

      else if (NULLP (CDR (CDR (toAdd)))) { // Dict entry...
	DBusMessageIter subIter;
	char *sign = BSTRING_TO_STRING (bgl_get_signature (toAdd));
	dbus_message_iter_open_container (iter, DBUS_TYPE_DICT_ENTRY, sign, &subIter);
	appendArgsFromScmListWithIter (&subIter, toAdd);
	dbus_message_iter_close_container (iter, &subIter);
      }

    }

  else if (VECTORP (toAdd))
    {
      DBusMessageIter subIter;
      int i, size = VECTOR_LENGTH (toAdd);
      if (size <= 0)
	bgl_error (string_to_bstring("Empty array detected !"), toAdd);
      char *sign = BSTRING_TO_STRING (bgl_get_signature (VECTOR_REF (toAdd, 0)));
      dbus_message_iter_open_container (iter, DBUS_TYPE_ARRAY, sign, &subIter);
      for (i = 0; i < size; i++)
	appendArgsFromScmListWithIter (&subIter, make_pair ( VECTOR_REF (toAdd, i),
							     BNIL ));
      dbus_message_iter_close_container (iter, &subIter);
    }

  else
    {
      bgl_error(string_to_bstring("Unknown type of argument"),
		toAdd);
    }

  appendArgsFromScmListWithIter (iter, CDR (lst));

}


/* Permet de recevoir un message depuis un object et son interface
   cette fonction s'abonne le temps de recevoir un message au bus en
   question et le supprime ensuite */
MESSAGE* receiveMessageFromObject (OBJECT* object, int timeout)
{

  MESSAGE* res;

  res = (MESSAGE*) GC_MALLOC (sizeof(MESSAGE));
  if (res==NULL) {
    bgl_error(string_to_bstring("Error occured while allocating structure message"), BNIL);
    return NULL;
  }


  char *rule = (char *) GC_MALLOC (sizeof (char) * (strlen
						 ("sender='',path='',interface=''") +
						 strlen (object->service->serviceName) +
						 strlen (object->path) +
						 strlen (object->interface)));
  if (rule==NULL) {
       bgl_error(string_to_bstring("Error occured while allocating rule string"), BNIL);
    return NULL;
  }

  sprintf (rule, "sender='%s',path='%s',interface='%s'",
	   object->service->serviceName,
	   object->path, object->interface);

  if(0==subscribe (rule, object->service->bus)) {
    bgl_error(string_to_bstring("Error occured while subscribing the rule"), BNIL);
    return NULL;
  }

  // lecture du message
  dbus_connection_read_write (object->service->bus->conn, timeout);
  res->message = dbus_connection_pop_message (object->service->bus->conn);

  if(0==unsubscribe(rule, object->service->bus)) {
    bgl_error(string_to_bstring("Error occured while unsubscribing the rule"), BNIL);
    return NULL;
  }

  if (res->message == NULL) {
    return NULL;
  }

  res->path = getCopyString (dbus_message_get_path (res->message));
  if(res->path == NULL) {
    bgl_error(string_to_bstring("Error occured while allocating path string"), BNIL);
    return NULL;
  }



  res->interface = getCopyString (dbus_message_get_interface (res->message));
  if(res->interface == NULL) {
    bgl_error(string_to_bstring("Error occured while allocating interface string"), BNIL);
    return NULL;
  }

  //printf("sender (%s)\n", dbus_message_get_sender(res->message));
  res->sender = getCopyString (dbus_message_get_sender (res->message));
  if(res->sender ==NULL) {
    bgl_error(string_to_bstring("Error ocured while allocating sender string"), BNIL);
    return NULL;
  }

  res->type = dbus_message_get_type (res->message);

  return res;
}


MESSAGE* receiveMessageFromBus (BUS* bus,int timeout) {
  if (bus != NULL) {
    
    MESSAGE* res;
    res = (MESSAGE*) GC_MALLOC (sizeof(MESSAGE));
    
    if (res == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the message"), BNIL);
    }
    
    char* rule = (char*) GC_MALLOC (sizeof (char) * (strlen (bus->uniqueName) + strlen ("destination=''")));
    
    if (rule == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating the rule string"), BNIL);
      return NULL;
    }
    
    sprintf(rule, "destination='%s'", bus->uniqueName);
    
    
    if (subscribe(rule, bus)==0) {
      bgl_error(string_to_bstring("Error occured while subscribing the rule"), BNIL);
      return NULL;
    }
    
    
    // lecture du message
    dbus_connection_read_write (bus->conn, timeout);
    res->message=dbus_connection_pop_message (bus->conn);
    
    if (unsubscribe(rule, bus)==0) {
      bgl_error(string_to_bstring("Error occured while unsubscribing the rule"), BNIL);
      return NULL;
    }
    
    if (res->message == NULL) {
      return NULL;
    }
    
    res->path = getCopyString (dbus_message_get_path (res->message));
    if (res->path == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating path string"), BNIL);
      return NULL;
    }
    res->interface = getCopyString (dbus_message_get_interface (res->message));
    if (res->interface == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating interface string"), BNIL);
      return NULL;
    }
    res->sender = getCopyString (dbus_message_get_sender (res->message));
    if (res->sender == NULL) {
      bgl_error(string_to_bstring("Error occured while allocating sender string"), BNIL);
      return NULL;
    }
    res->type = dbus_message_get_type (res->message);
    
    return res;
  }
  else {
    bgl_error(string_to_bstring("Parameter bus was NULL"), BNIL);
    return NULL;
  }
}
  

int sendSignal (BUS *conn, const char *signalName, obj_t args) {

  DBusMessage *msg;
  DBusMessageIter iter;
  dbus_uint32_t serial = 0;

  /* dbus_message_new_signal (const char *path, const char *interface, const
     char *name) */
  msg = dbus_message_new_signal (conn->path, conn->interface, signalName);

  if (msg == NULL) {
    bgl_error(string_to_bstring("Error occured while creating the signal"),
	      string_to_bstring((char *)signalName));
    return 0;
  }

  dbus_message_iter_init_append(msg, &iter);

  /* Ajout des arguments au message */
  if (! NULLP(args)) {
    appendArgsFromScmList (msg, args);
  }
  
  /* Envoi du message */
  if (!dbus_connection_send(conn->conn, msg, &serial)) {
    bgl_error(string_to_bstring("Error occured while sending the signal on bus"),
	      string_to_bstring((char *)signalName));
    return 0;
  }

  dbus_connection_flush(conn->conn);

  return serial;
}


MESSAGE* receiveMessageWithRule (BUS* bus, int timeout, const char *rule) {

  MESSAGE* res;
  res = (MESSAGE*) GC_MALLOC (sizeof(MESSAGE));

  // abonnement aux messages correspondant a la regle
  if (subscribe(rule, bus)==0) {
    bgl_error(string_to_bstring("Error occured while subscribing the rule"),
	      string_to_bstring((char *)rule));
    return NULL;
  }
  
  // lecture du message
  dbus_connection_read_write(bus->conn, timeout);
  res->message = dbus_connection_pop_message(bus->conn);
  
  // abonnement aux messages correspondant a la regle
  if (unsubscribe(rule, bus)==0) {
    bgl_error(string_to_bstring("Error occured while subscribing the rule"),
	      string_to_bstring((char *)rule));
    return NULL;
  }
  
  if (res->message == NULL) {
    return NULL;
  }

  res->path = getCopyString (dbus_message_get_path (res->message));
  if(res->path == NULL) {
    bgl_error(string_to_bstring("Error occured while allocating the path string"),
	      string_to_bstring((char *)dbus_message_get_path (res->message)));
    return NULL;
  }
  
  res->interface = getCopyString (dbus_message_get_interface (res->message));
  if(res->interface == NULL) {
    bgl_error(string_to_bstring("Error occured while allocating the interface string"),
	      string_to_bstring((char *)dbus_message_get_interface (res->message)));
    return NULL;
  }
  
  res->sender = getCopyString (dbus_message_get_sender (res->message));
  if(res->sender ==NULL) {
    bgl_error(string_to_bstring("Error occured while allocating sender bstring"),
	      string_to_bstring((char *)dbus_message_get_sender (res->message)));
    return NULL;
  }
  
  res->type = dbus_message_get_type (res->message);
 
  return res;
}


obj_t getMessageType (MESSAGE* msg) {
  int type = dbus_message_get_type (msg->message);
  switch (type) {
  case DBUS_MESSAGE_TYPE_METHOD_CALL:
    return string_to_symbol ( "dbus-method-call" );
  case DBUS_MESSAGE_TYPE_METHOD_RETURN:
    return string_to_symbol ( "dbus-method-return" );
  case DBUS_MESSAGE_TYPE_ERROR:
    return string_to_symbol ( "dbus-error" );
  case DBUS_MESSAGE_TYPE_SIGNAL:
    return string_to_symbol ( "dbus-signal" );
  case DBUS_MESSAGE_TYPE_INVALID:
    return string_to_symbol ( "dbus-invalid" );
  }
  bgl_error(string_to_bstring("Error occured while getting the message type"),
	    string_to_bstring("Unknown type"));
  return BFALSE;
}


obj_t getMessagePath (MESSAGE *msg) {
  if (msg->path != NULL)
     return string_to_bstring ((char *)(msg->path));
  return BFALSE;
}
