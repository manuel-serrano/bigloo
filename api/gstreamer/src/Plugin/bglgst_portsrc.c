/*=====================================================================*/
/*    .../project/bigloo/api/gstreamer/src/Plugin/bglgst_portsrc.c     */
/*    -------------------------------------------------------------    */
/*    Author      :  Cyprien Nicolas                                   */
/*    Creation    :  Wed Jul 23 07:11:37 2008                          */
/*    Last change :  Wed Feb 13 16:11:29 2013 (serrano)                */
/*    Copyright   :  2008-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo INPUT-PORT plugin.                                        */
/*    -------------------------------------------------------------    */
/*    GStreamer                                                        */
/*    Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>      */
/*                       2000 Wim Taymans <wim@fluendo.com>            */
/*                                                                     */
/*    This library is free software; you can redistribute it and/or    */
/*    modify it under the terms of the GNU Library General Public      */
/*    License as published by the Free Software Foundation; either     */
/*    version 2 of the License, or (at your option) any later version. */
/*---------------------------------------------------------------------*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "bglgst_port.h"
#include "bglgst_config.h"

#define BGL_DEBUG 1
#undef BGL_DEBUG

/*---------------------------------------------------------------------*/
/*    Bigloo imports                                                   */
/*---------------------------------------------------------------------*/
extern obj_t bgl_close_input_port( obj_t );
extern obj_t bgl_input_port_seek( obj_t, long );
extern obj_t bglgst_open_input_file( char *file );
extern obj_t bglgst_register_port( obj_t );
extern obj_t bglgst_unregister_port( obj_t );

static GstStaticPadTemplate srctemplate =
   GST_STATIC_PAD_TEMPLATE( "src",
			    GST_PAD_SRC,
			    GST_PAD_ALWAYS,
			    GST_STATIC_CAPS_ANY );

GST_DEBUG_CATEGORY_STATIC( bgl_gst_port_src_debug );

#define GST_CAT_DEFAULT bgl_gst_port_src_debug


/*---------------------------------------------------------------------*/
/*    PortSrc signals and args                                         */
/*---------------------------------------------------------------------*/
enum {
  SIGNAL_HANDOFF,
  LAST_SIGNAL
};

#define DEFAULT_SIZEMIN           0
#define DEFAULT_SIZEMAX           4096
#define DEFAULT_DATARATE          0
#define DEFAULT_SYNC              FALSE
#define DEFAULT_EOS               FALSE
#define DEFAULT_SIGNAL_HANDOFFS   FALSE
#define DEFAULT_SILENT            FALSE
#define DEFAULT_DUMP              TRUE
#define DEFAULT_PARENTSIZE        4096*10
#define DEFAULT_CAN_ACTIVATE_PULL TRUE
#define DEFAULT_CAN_ACTIVATE_PUSH TRUE
#define DEFAULT_FORMAT            GST_FORMAT_BYTES
#define DEFAULT_PORT              BFALSE
#define DEFAULT_URI               0

enum {
   PROP_0,
   PROP_SIZEMIN,
   PROP_SIZEMAX,
   PROP_DATARATE,
   PROP_SYNC,
   PROP_EOS,
   PROP_SIGNAL_HANDOFFS,
   PROP_SILENT,
   PROP_DUMP,
   PROP_PARENTSIZE,
   PROP_LAST_MESSAGE,
   PROP_CAN_ACTIVATE_PULL,
   PROP_CAN_ACTIVATE_PUSH,
   PROP_IS_LIVE,
   PROP_FORMAT,
   PROP_LAST,
   PROP_PORT,
   PROP_URI,
};


/*---------------------------------------------------------------------*/
/*    Backward gstreamer compatibility                                 */
/*---------------------------------------------------------------------*/
#if( !BGL_GSTREAMER_HAVE_PARSE_INFO )
#define gst_buffer_try_new_and_alloc( s ) __gst_buffer_try_new_and_alloc( s )

static GstBuffer *__gst_buffer_try_new_and_alloc( guint size ) {
   GstBuffer *newbuf;
   guint8 *malloc_data;

   malloc_data = g_try_malloc( size );

   if( G_UNLIKELY (malloc_data == NULL && size != 0) ) {
      return NULL;
   }

   /* FIXME: there's no g_type_try_create_instance() in GObject yet, so this
    * will still abort if a new GstBuffer structure can't be allocated */
   newbuf = gst_buffer_new();

   GST_BUFFER_MALLOCDATA( newbuf ) = malloc_data;
   GST_BUFFER_DATA( newbuf ) = malloc_data;
   GST_BUFFER_SIZE( newbuf ) = size;

   return newbuf;
}
#endif

/*---------------------------------------------------------------------*/
/*    Boilerplate                                                      */
/*---------------------------------------------------------------------*/
#define _do_init( _ ) \
  GST_DEBUG_CATEGORY_INIT( bgl_gst_port_src_debug, \
			   "bglportsrc", \
			   0, \
			   "bglportsrc element" );

GST_BOILERPLATE_FULL( BglPortSrc,
		      bgl_gst_port_src,
		      GstBaseSrc,
		      GST_TYPE_BASE_SRC,
		      _do_init );

/*---------------------------------------------------------------------*/
/*    Local functions                                                  */
/*---------------------------------------------------------------------*/
static guint bgl_gst_port_src_signals[ LAST_SIGNAL ] = { 0 };

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_set_port ...                                    */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_set_port( BglPortSrc *src, gpointer new_port, char *new_uri ) {
#if( defined( BGL_DEBUG ) )   
   fprintf( stderr, "bgl_gst_port_src_set_port(%s:%d)\n  new_port=%p src->port=%p closed:%d filepos=%d\n", __FILE__, __LINE__, new_port, src->port, INPUT_PORT_CLOSEP( new_port ), INPUT_PORT_FILEPOS( new_port ) );
#endif
   
   if( !INPUT_PORTP( new_port ) ) {
      C_SYSTEM_FAILURE( BGL_TYPE_ERROR,
			"bglportsrc",
			"Illegal input-port",
			new_port );
   }

   if( INPUT_PORTP( src->port ) ) {
      if( src->uri ) bgl_close_input_port( src->port );
      bglgst_unregister_port( src->port );
   }

   bglgst_register_port( new_port );
   src->port = (obj_t)new_port;
   src->uri = new_uri;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_set_uri ...                                     */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_set_uri( BglPortSrc *src, char *new_uri ) {
   obj_t new_port = bglgst_open_input_file( new_uri );

#if( defined( BGL_DEBUG ) )
   fprintf( stderr, "bgl_gst_port_src_set_uri(%s:%d)\n   uri=%s pos=%d\n",
	    __FILE__, __LINE__, 
	    new_uri, INPUT_PORT_FILEPOS( new_port ) );
#endif
   
   if( !INPUT_PORTP( new_port ) ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"bglportsrc",
			"Cannot open uri",
			string_to_bstring( new_uri ) );
   }

   return bgl_gst_port_src_set_port( src, new_port, new_uri );
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_src_get_size ...                                    */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_src_get_size( GstBaseSrc *basesrc, guint64 *size ) {
   BglPortSrc *src = BGL_GST_PORT_SRC( basesrc );
   obj_t port = src->port;

   if( !INPUT_PORTP( port ) ) return FALSE;
   
   switch( (long)PORT( port ).kindof ) {
      case (long) KINDOF_FILE:
	 *size = bgl_file_size( BSTRING_TO_STRING( PORT( port ).name ) );
	 return TRUE;
	 
      case (long) KINDOF_STRING:
      case (long) KINDOF_SOCKET:
	 *size = BGL_INPUT_PORT_LENGTH( port );
	 return TRUE;
	 
      default:
	 return FALSE;
   }
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_src_is_seekable ...                                 */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_src_is_seekable( GstBaseSrc *basesrc ) {
   BglPortSrc *src = BGL_GST_PORT_SRC( basesrc );
   obj_t port = src->port;

   if( !INPUT_PORTP( port ) ) return FALSE;
   
   switch( (long)PORT( port ).kindof ) {
      case (long) KINDOF_FILE:
      case (long) KINDOF_STRING:
	 return TRUE;
	 
      default:
	 return FALSE;
   }
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_src_start ...                                       */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_src_start( GstBaseSrc *basesrc ) {
   BglPortSrc *src = BGL_GST_PORT_SRC( basesrc );

   src->buffer_count = 0;
   src->bytes_sent = 0;

   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_src_stop ...                                        */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_src_stop( GstBaseSrc *basesrc ) {
   BglPortSrc *src;

   src = BGL_GST_PORT_SRC( basesrc );

   GST_OBJECT_LOCK( src );
   
   if( src->parent ) {
      gst_buffer_unref( src->parent );
      src->parent = NULL;
   }
   
   g_free( src->last_message );
   src->last_message = NULL;
   
   GST_OBJECT_UNLOCK( src );

   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_finalize ...                                    */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_finalize( GObject * object ) {
   BglPortSrc *src;

#if( defined( BGL_DEBUG ) )   
   fprintf( stderr, "%s:%d bgl_gst_port_src_finalize: %p\n",
	    __FILE__, __LINE__, object );
#endif
   
   src = BGL_GST_PORT_SRC( object );

   g_free( src->last_message );
   
   /* MS: the parent field actually refers to buffer! */
   if( src->parent ) {
      gst_buffer_unref( src->parent );
      src->parent = NULL;
   }

   /* Unregister the underlying Bigloo port */
   if( INPUT_PORTP( src->port ) ) {
      if( src->uri ) bgl_close_input_port( src->port );
      bglgst_unregister_port( src->port );
      
      src->port = BFALSE;
      src->uri = 0;
   }
   
   G_OBJECT_CLASS( parent_class )->finalize( object );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_set_property ...                                */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_set_property( GObject *object,
			       guint prop_id,
			       const GValue *value,
			       GParamSpec *pspec ) {
   BglPortSrc *src = BGL_GST_PORT_SRC( object );
   GstBaseSrc *basesrc = GST_BASE_SRC( object );

   switch( prop_id ) {
      case PROP_SIZEMIN:
	 src->sizemin = g_value_get_int( value );
	 break;
      case PROP_SIZEMAX:
	 src->sizemax = g_value_get_int( value );
	 break;
      case PROP_PARENTSIZE:
	 src->parentsize = g_value_get_int( value );
	 break;
      case PROP_DATARATE:
	 src->datarate = g_value_get_int( value );
	 break;
      case PROP_SYNC:
	 src->sync = g_value_get_boolean( value );
	 break;
      case PROP_SILENT:
	 src->silent = g_value_get_boolean( value );
	 break;
      case PROP_SIGNAL_HANDOFFS:
	 src->signal_handoffs = g_value_get_boolean( value );
	 break;
      case PROP_DUMP:
	 src->dump = g_value_get_boolean( value );
	 break;
      case PROP_CAN_ACTIVATE_PUSH:
	 g_return_if_fail( !GST_OBJECT_FLAG_IS_SET( object, GST_BASE_SRC_STARTED) );
	 GST_BASE_SRC( src )->can_activate_push = g_value_get_boolean( value );
	 break;
      case PROP_CAN_ACTIVATE_PULL:
	 g_return_if_fail( !GST_OBJECT_FLAG_IS_SET( object, GST_BASE_SRC_STARTED ) );
	 src->can_activate_pull = g_value_get_boolean( value );
	 break;
      case PROP_IS_LIVE:
	 gst_base_src_set_live( basesrc, g_value_get_boolean( value ) );
	 break;
      case PROP_FORMAT:
	 src->format = g_value_get_enum( value );
	 break;
      case PROP_PORT:
	 fprintf( stderr, "bgl_gst_port_set_property src=%p\n", src );
	 bgl_gst_port_src_set_port( src, g_value_get_pointer( value ), 0 );
	 break;
      case PROP_URI:
	 bgl_gst_port_src_set_uri( src, (char *)g_value_get_string( value ) );
	 break;
      default:
	 G_OBJECT_WARN_INVALID_PROPERTY_ID( object, prop_id, pspec );
	 break;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_src_get_property ...                                     */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_get_property( GObject *object,
			       guint prop_id,
			       GValue *value,
			       GParamSpec *pspec ) {
   BglPortSrc *src;
   GstBaseSrc *basesrc;

   g_return_if_fail( GST_IS_BGL_GST_PORT_SRC( object ) );

   src = BGL_GST_PORT_SRC( object );
   basesrc = GST_BASE_SRC( object );

   switch( prop_id ) {
      case PROP_SIZEMIN:
	 g_value_set_int( value, src->sizemin );
	 break;
      case PROP_SIZEMAX:
	 g_value_set_int( value, src->sizemax );
	 break;
      case PROP_PARENTSIZE:
	 g_value_set_int( value, src->parentsize );
	 break;
      case PROP_DATARATE:
	 g_value_set_int( value, src->datarate );
	 break;
      case PROP_SYNC:
	 g_value_set_boolean( value, src->sync );
	 break;
      case PROP_SILENT:
	 g_value_set_boolean( value, src->silent );
	 break;
      case PROP_SIGNAL_HANDOFFS:
	 g_value_set_boolean( value, src->signal_handoffs );
	 break;
      case PROP_DUMP:
	 g_value_set_boolean( value, src->dump );
	 break;
      case PROP_LAST_MESSAGE:
	 GST_OBJECT_LOCK( src );
	 g_value_set_string( value, src->last_message );
	 GST_OBJECT_UNLOCK( src );
	 break;
      case PROP_CAN_ACTIVATE_PUSH:
	 g_value_set_boolean( value, GST_BASE_SRC( src )->can_activate_push );
	 break;
      case PROP_CAN_ACTIVATE_PULL:
	 g_value_set_boolean( value, src->can_activate_pull );
	 break;
      case PROP_IS_LIVE:
	 g_value_set_boolean( value, gst_base_src_is_live( basesrc ) );
	 break;
      case PROP_FORMAT:
	 g_value_set_enum( value, src->format );
	 break;
      case PROP_PORT:
	 g_value_set_pointer( value, src->port );
	 break;
      case PROP_URI:
	 if( src->port == BFALSE )
	    g_value_set_string( value, NULL );
	 else
	    g_value_set_string( value,
				BSTRING_TO_STRING( INPUT_PORT_NAME( src->port ) ) );
	 break;
      default:
	 G_OBJECT_WARN_INVALID_PROPERTY_ID( object, prop_id, pspec );
	 break;
   }
}
   
/*---------------------------------------------------------------------*/
/*    static GstFlowReturn                                             */
/*    bgl_gst_port_src_create ...                                      */
/*---------------------------------------------------------------------*/
static GstFlowReturn
bgl_gst_port_src_create( GstBaseSrc *basesrc,
			 guint64 offset,
			 guint length,
			 GstBuffer **ret ) {
   BglPortSrc *src;
   GstBuffer *buf;
   guint8 *readstr;
   guint readlen;
  
   src = BGL_GST_PORT_SRC( basesrc );
   
#if( defined( BGL_DEBUG ) )   
   fprintf( stderr, "bgl_gst_port_src_create: ret=%p *ret=%p length=%d\n",
	    ret, *ret, length );
   fprintf( stderr, "bgl_gst_port_src_create: src=%p obj=%p pos=%d offset=%ld\n", src, src->port, INPUT_PORT_FILEPOS( src->port ), offset );
#endif

   /* Check that a Bigloo input port is indeed associated to the element */
   if( src->port == BFALSE ) {
      GST_ELEMENT_ERROR( src,
			 RESOURCE,
			 NOT_FOUND,
			 ("no input-port provided\n"),
			 ("no input-port provided for object %p\n", src) );
      return GST_FLOW_ERROR;
   }

   /* Check the non emptyness of the buffer */
   if( length == 0 ) {
      return GST_FLOW_UNEXPECTED;
   }

/*    if( (src->parentsize == length) && src->parent ) {               */
/*       buf = src->parent;                                            */
/*    }                                                                */

   /* Seek to the correct position */
   if( INPUT_PORT( src->port ).filepos != offset ) {
      if( (offset > 0) && (bgl_gst_port_src_is_seekable( basesrc ) == TRUE) ) {
	 bgl_input_port_seek( src->port, offset );
      }
   }
   
   /* WARNING!!! get a new buffer for getting the read characters.    */
   /* The function rgc_blit_string adds and extra 0 after             */
   /* the read chars, so we have to allocate a buffer of size         */
   /* length + 1, the same thing apply in the g_malloc0 call.         */
   if( !(buf = gst_buffer_try_new_and_alloc( length + 1 ) ) ) {
      /* create a brand new buffer */
      if( !(buf = gst_buffer_new()) ) {
	 GST_ELEMENT_ERROR( src, CORE, FAILED,
			    ("Could not allocate buffer.\n"),
			    ("Could not allocate buffer for object %p\n", src));
	 
	 return GST_FLOW_ERROR;
      } else {
	 /* allocate a char array */
	 if( !(readstr = g_malloc0( length + 1 ) ) ) {;
	    gst_buffer_unref( buf );

	    GST_ELEMENT_ERROR( src, CORE, FAILED,
			       ("Could not allocate char array.\n"),
			       ("Could not allocate buffer for object %p\n", src) );
	    return GST_FLOW_ERROR;
	 } else {
	    GST_BUFFER_MALLOCDATA( buf ) = readstr;
	 }
      }
   } else {
      readstr = GST_BUFFER_MALLOCDATA( buf );
      
/*       if( src->parent != buf ) {                                    */
/* 	 if( src->parent ) gst_buffer_unref( src->parent );            */
/* 	                                                               */
/* 	 src->parentsize = length;                                     */
/* 	 src->parent = buf;                                            */
/* 	 gst_buffer_ref( buf );                                        */
/*       }                                                             */
   }

#if( defined( BGL_DEBUG ) )
   fprintf( stderr, "bgl_rgc_blit_string( %p, %p, 0, %d )\n",
	    src->port, readstr, length );
#endif
   if( !(readlen = bgl_rgc_blit_string( src->port, readstr, 0, length )) ) {
      /* end of file */
      gst_buffer_unref( buf );
      return GST_FLOW_UNEXPECTED;
   }
#if( defined( BGL_DEBUG ) )      
   fprintf( stderr, "bgl_gst_port_src_create(%s:%d)\n  readlen=%d\n  length=%d\n  offset=%d\n",
	    __FILE__, __LINE__, readlen, length, offset );
   
   if( BGL_DEBUG > 0 ) {
      int i;
      fprintf( stderr, "  readstr=" );
      for( i = 0; i < readlen; i +=2 ) {
	 if( (i % 16) == 0 ) fprintf( stderr, "\n    %08x ", i  );
	 fprintf( stderr, "%02x%02x ", readstr[ i + 1 ], readstr[ i ] );
      }
      fprintf( stderr, "\n" );
   }
#endif

   GST_BUFFER_SIZE( buf ) = readlen;
   GST_BUFFER_OFFSET( buf ) = offset;
   GST_BUFFER_OFFSET_END( buf ) = offset + readlen;

   GST_BUFFER_TIMESTAMP( buf ) = GST_CLOCK_TIME_NONE;
   GST_BUFFER_DURATION( buf ) = GST_CLOCK_TIME_NONE;
  
   if( src->signal_handoffs ) {
      GST_LOG_OBJECT( src, "pre handoff emit" );
      g_signal_emit( G_OBJECT( src ),
		     bgl_gst_port_src_signals[ SIGNAL_HANDOFF ], 0,
		     buf, basesrc->srcpad );
      GST_LOG_OBJECT( src, "post handoff emit" );
   }
  
   src->bytes_sent += GST_BUFFER_SIZE( buf );
   src->buffer_count++;
    
   *ret = buf;
  
   return GST_FLOW_OK;
}


/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    marshal_VOID__MINIOBJECT_OBJECT ...                              */
/*---------------------------------------------------------------------*/
static void
marshal_VOID__MINIOBJECT_OBJECT( GClosure *closure,
				 GValue *return_value,
				 guint n_param_values,
				 const GValue *param_values,
				 gpointer invocation_hint,
				 gpointer marshal_data ) {
   typedef void (*marshalfunc_VOID__MINIOBJECT_OBJECT)
      (gpointer obj, gpointer arg1, gpointer arg2, gpointer data2);
   
   marshalfunc_VOID__MINIOBJECT_OBJECT callback;
   GCClosure *cc = (GCClosure *)closure;
   gpointer data1, data2;

   g_return_if_fail( n_param_values == 3 );

   if( G_CCLOSURE_SWAP_DATA( closure ) ) {
      data1 = closure->data;
      data2 = g_value_peek_pointer( param_values + 0 );
   } else {
      data1 = g_value_peek_pointer( param_values + 0 );
      data2 = closure->data;
   }
   
   callback =
      (marshalfunc_VOID__MINIOBJECT_OBJECT)
      (marshal_data ? marshal_data : cc->callback);

   callback( data1,
	     gst_value_get_mini_object (param_values + 1),
	     g_value_get_object( param_values + 2 ),
	     data2);
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    bgl_gst_port_src_event_handler ...                               */
/*---------------------------------------------------------------------*/
static gboolean
bgl_gst_port_src_event_handler( GstBaseSrc * basesrc, GstEvent * event ) {
   BglPortSrc *src = BGL_GST_PORT_SRC( basesrc );

   if( !src->silent ) {
      const GstStructure *s;
      gchar *sstr;

      GST_OBJECT_LOCK( src );
      g_free( src->last_message );

      if( (s = gst_event_get_structure( event ) ) )
	 sstr = gst_structure_to_string( s );
      else
	 sstr = g_strdup( "" );

      src->last_message =
	 g_strdup_printf( "event   ******* E (type: %d, %s) %p",
			  GST_EVENT_TYPE (event), sstr, event );
      g_free( sstr );
      GST_OBJECT_UNLOCK( src );

      g_object_notify( G_OBJECT( src ), "last_message" );
   }

   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_get_times ...                                   */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_get_times( GstBaseSrc *basesrc,
			    GstBuffer *buffer,
			    GstClockTime *start,
			    GstClockTime *end) {
   BglPortSrc *src = BGL_GST_PORT_SRC( basesrc );

   /* sync on the timestamp of the buffer if requested. */
   if( src->sync ) {
      GstClockTime timestamp = GST_BUFFER_TIMESTAMP( buffer );

      if( GST_CLOCK_TIME_IS_VALID( timestamp ) ) {
	 /* get duration to calculate end time */
	 GstClockTime duration = GST_BUFFER_DURATION( buffer );

	 if( GST_CLOCK_TIME_IS_VALID( duration ) ) {
	    *end = timestamp + duration;
	 }
	 *start = timestamp;
      }
   } else {
      *start = -1;
      *end = -1;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_base_init ...                                   */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_base_init( gpointer g_class ) {
   static GstElementDetails element_details = {
      "Bigloo input-port source",
      "Source",
      "Get data from a Bigloo input port",
      "Cyprien Nicolas <Cyprien.Nicolas@sophia.inria.fr>"
   };
   GstElementClass *element_class = GST_ELEMENT_CLASS( g_class );

   gst_element_class_add_pad_template(
      element_class, gst_static_pad_template_get( &srctemplate ) );
   
   gst_element_class_set_details(
      element_class, &element_details );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_class_init ...                                  */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_class_init( BglPortSrcClass * klass ) {
  GObjectClass *gobject_class = G_OBJECT_CLASS( klass );
  GstElementClass *gstelement_class = GST_ELEMENT_CLASS( klass );
  GstBaseSrcClass *gstbase_src_class = GST_BASE_SRC_CLASS( klass );

  gobject_class->finalize =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_finalize );
  gobject_class->set_property =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_set_property );
  gobject_class->get_property =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_get_property );

#ifndef G_PARAM_STATIC_STRINGS
#  define G_PARAM_STATIC_STRINGS 0
#endif
  
  g_object_class_install_property( gobject_class, PROP_SIZEMIN,
      g_param_spec_int( "sizemin", "sizemin", "Minimum buffer size", 0,
          G_MAXINT, DEFAULT_SIZEMIN,
          G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_SIZEMAX,
      g_param_spec_int( "sizemax", "sizemax", "Maximum buffer size", 0,
          G_MAXINT, DEFAULT_SIZEMAX,
          G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_PARENTSIZE,
      g_param_spec_int( "parentsize", "parentsize",
          "Size of parent buffer for sub-buffered allocation", 0, G_MAXINT,
          DEFAULT_PARENTSIZE, G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_DATARATE,
      g_param_spec_int( "datarate", "Datarate",
          "Timestamps buffers with number of bytes per second (0 = none)", 0,
          G_MAXINT, DEFAULT_DATARATE,
          G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_SYNC,
      g_param_spec_boolean( "sync", "Sync", "Sync to the clock to the datarate",
          DEFAULT_SYNC, G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_LAST_MESSAGE,
      g_param_spec_string( "last-message", "last-message",
          "The last status message", NULL,
          G_PARAM_READABLE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_SILENT,
      g_param_spec_boolean( "silent", "Silent",
          "Don't produce last_message events", DEFAULT_SILENT,
          G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_SIGNAL_HANDOFFS,
      g_param_spec_boolean( "signal-handoffs", "Signal handoffs",
          "Send a signal before pushing the buffer", DEFAULT_SIGNAL_HANDOFFS,
          G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_CAN_ACTIVATE_PUSH,
      g_param_spec_boolean( "can-activate-push", "Can activate push",
          "Can activate in push mode", DEFAULT_CAN_ACTIVATE_PUSH,
          G_PARAM_READWRITE | G_PARAM_CONSTRUCT | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_CAN_ACTIVATE_PULL,
      g_param_spec_boolean( "can-activate-pull", "Can activate pull",
          "Can activate in pull mode", DEFAULT_CAN_ACTIVATE_PULL,
          G_PARAM_READWRITE | G_PARAM_CONSTRUCT | G_PARAM_STATIC_STRINGS ) );
  g_object_class_install_property( gobject_class, PROP_IS_LIVE,
      g_param_spec_boolean( "is-live", "Is this a live source",
          "True if the element cannot produce data in PAUSED", FALSE,
          G_PARAM_READWRITE | G_PARAM_CONSTRUCT | G_PARAM_STATIC_STRINGS) );
  /**
   * BglPortSrc:format
   *
   * Set the format of the newsegment events to produce.
   *
   * Since: 0.10.20
   */
  g_object_class_install_property( gobject_class, PROP_FORMAT,
      g_param_spec_enum( "format", "Format",
          "The format of the segment events", GST_TYPE_FORMAT,
          DEFAULT_FORMAT, G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );

  /**
   * BglPortSrc::handoff:
   * @portsrc: the portsrc instance
   * @buffer: the buffer that will be pushed
   * @pad: the pad that will sent it
   *
   * This signal gets emitted before sending the buffer.
   */
  bgl_gst_port_src_signals[ SIGNAL_HANDOFF ] =
      g_signal_new( "handoff", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST,
      G_STRUCT_OFFSET (BglPortSrcClass, handoff), NULL, NULL,
      marshal_VOID__MINIOBJECT_OBJECT, G_TYPE_NONE, 2, GST_TYPE_BUFFER,
      GST_TYPE_PAD );

  
  g_object_class_install_property( gobject_class, PROP_PORT,
      g_param_spec_pointer( "port", "input-port",
			    "input-port to get data from",
			    G_PARAM_READWRITE ) );

  g_object_class_install_property( gobject_class, PROP_URI,
      g_param_spec_string( "uri", "uri",
			   "open an input-port from a uri to get data from",
			   NULL,
			   G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS ) );

  gstbase_src_class->is_seekable =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_is_seekable );
  gstbase_src_class->start =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_start ) ;
  gstbase_src_class->stop =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_stop );
  gstbase_src_class->event =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_event_handler );
  gstbase_src_class->get_times =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_get_times );
  gstbase_src_class->get_size =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_get_size );
  gstbase_src_class->create =
     GST_DEBUG_FUNCPTR( bgl_gst_port_src_create );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_port_src_init ...                                        */
/*---------------------------------------------------------------------*/
static void
bgl_gst_port_src_init( BglPortSrc *portsrc, BglPortSrcClass *g_class) {
   portsrc->buffer_count = 0;
   portsrc->silent = DEFAULT_SILENT;
   portsrc->signal_handoffs = DEFAULT_SIGNAL_HANDOFFS;
   portsrc->dump = DEFAULT_DUMP;
   portsrc->sizemin = DEFAULT_SIZEMIN;
   portsrc->sizemax = DEFAULT_SIZEMAX;
   portsrc->parent = NULL;
   portsrc->parentsize = DEFAULT_PARENTSIZE;
   portsrc->last_message = NULL;
   portsrc->datarate = DEFAULT_DATARATE;
   portsrc->sync = DEFAULT_SYNC;
   portsrc->format = DEFAULT_FORMAT;
   portsrc->port = DEFAULT_PORT;
   portsrc->uri = DEFAULT_URI;
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    plugin_init ...                                                  */
/*---------------------------------------------------------------------*/
static gboolean
plugin_init( GstPlugin * plugin ) {
   GST_DEBUG_CATEGORY_INIT( bgl_gst_port_src_debug,
			    "bigloo",
			    0,
                            "Bigloo port plugin" );

   return gst_element_register( plugin, "bglportsrc",
                                GST_RANK_NONE,
                                GST_TYPE_BGL_GST_PORT_SRC );
}

/*---------------------------------------------------------------------*/
/*    gboolean                                                         */
/*    bgl_gst_plugin_port_src_init ...                                 */
/*---------------------------------------------------------------------*/
gboolean
bgl_gst_plugin_port_src_init() {
#if( defined( GST_PLUGIN_DEFINE_STATIC ) )
   GST_PLUGIN_DEFINE( GST_VERSION_MAJOR,
		      GST_VERSION_MINOR,
		      "bglportsrc",
		      "Bigloo Port Plugin",
		      plugin_init,
		      PLUGIN_VERSION,
		      PLUGIN_LICENSE,
		      PLUGIN_PACKAGE,
		      PLUGIN_URL );

   _gst_plugin_register_static( &gst_plugin_desc );

   return TRUE;
#else      
   gst_plugin_register_static( GST_VERSION_MAJOR,
			       GST_VERSION_MINOR,
			       "bglportsrc",
			       "Bigloo Port Plugin",
			       plugin_init,
			       PLUGIN_VERSION,
			       PLUGIN_LICENSE,
			       PLUGIN_PACKAGE,
			       PLUGIN_URL );
#endif
}
