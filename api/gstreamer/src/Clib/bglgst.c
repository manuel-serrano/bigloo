/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/gstreamer/src/Clib/bglgst.c      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec 30 08:32:57 2007                          */
/*    Last change :  Wed Feb 13 15:07:26 2013 (serrano)                */
/*    Copyright   :  2007-13 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Misc GSTREAMER wrappers.                                         */
/*=====================================================================*/
#if( BGL_GC == BGL_BOEHM_GC )
#  include <glib.h>
#endif
#include <gst/gst.h>
#include <string.h>
#include <locale.h>
#include "bglgst_config.h"
#include "bglgst.h"
#include "../Plugin/bglgst_port.h"

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DECL obj_t void_star_to_obj();
extern obj_t bgl_gst_bin_elements_set( obj_t, obj_t );

/*---------------------------------------------------------------------*/
/*    Threads                                                          */
/*    -------------------------------------------------------------    */
/*    Enabling/disabling Bigloo threads for gstreamer impacts the      */
/*    overall implementation. In particular, using threads greatly     */
/*    simplifies the callbacks machinery. Without threads, callbacks   */
/*    are not allowed to allocate Bigloo objects (the GC requires      */
/*    to create all allocating threads), this requires complex         */
/*    workaround that are no longer needed when threads are enabled.   */
/*    -------------------------------------------------------------    */
/*    The current implementation that uses no thread is broken.        */
/*    For a reason I (MS) cannot understand, the call back machinery   */
/*    require threads. I think some objects are collected (why,        */
/*    please, tell me why, the gstreamer guys have found smart to      */
/*    use reference counting, it is known for decades that ref-        */
/*    counting is a bad-idea).                                         */
/*---------------------------------------------------------------------*/
static int bglgst_use_threads = BGL_GSTREAMER_USE_THREADS;

/*---------------------------------------------------------------------*/
/*    CHECK_PROCEDURE                                                  */
/*---------------------------------------------------------------------*/
#define CHECK_PROCEDURE( proc, arity ) \
   if( !PROCEDURE_CORRECT_ARITYP( proc, arity ) ) { \
      char buf[ 80 ]; \
      sprintf( buf, "wrong number of arguments for callback (%d expected)", arity ); \
      C_SYSTEM_FAILURE( BGL_ERROR, "gst-object-connect", buf, proc ); \
   }

/*---------------------------------------------------------------------*/
/*    struct callback                                                  */
/*    -------------------------------------------------------------    */
/*    The callback machinery is used for one purpose. GStreamer        */
/*    threads cannot invoke Bigloo code because the GC gets            */
/*    confused when alloc and collection functions are called from     */
/*    non-Bigloo thread. The callback is used to register the          */
/*    callbacks that are invoked by a dedicated Bigloo thread.         */
/*---------------------------------------------------------------------*/
typedef struct callback {
   obj_t (*convert)( void *, obj_t );
   obj_t proc;
   int arity;
   void *args[ 1 ];
} *callback_t;

/*---------------------------------------------------------------------*/
/*    callback_t                                                       */
/*    callbacks ...                                                    */
/*---------------------------------------------------------------------*/
#define INITIAL_MAX_CALLBACK 40
static callback_t *callbacks;
static int callback_length = INITIAL_MAX_CALLBACK;
static int callback_index = 0;

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bglgst_use_threadsp ...                                          */
/*---------------------------------------------------------------------*/
bool_t
bglgst_use_threadsp() {
   return bglgst_use_threads;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gobject_boehm_alloc_init ...                                 */
/*---------------------------------------------------------------------*/
static gpointer bgl_gst_alloc( gsize n ) {
   fprintf( stderr, "GC_MALLOC n=%" G_GSIZE_FORMAT "\n", n );
   return GC_MALLOC( n );
}

static gpointer bgl_gst_realloc( gpointer ptr, gsize n ) {
   fprintf( stderr, "GC_REALLOC ptr=%p n=%" G_GSIZE_FORMAT "\n", ptr, n );
   return GC_REALLOC( ptr, n );
}

static void bgl_gst_free( gpointer ptr ) {
   fprintf( stderr, "GC_FREE ptr=%p\n", ptr );
   return GC_FREE( ptr );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gst_init ...                                                 */
/*---------------------------------------------------------------------*/
void
bgl_gst_init( obj_t args ) {
   if( !PAIRP( args ) && !NULLP( args ) ) {
      C_SYSTEM_FAILURE( BGL_TYPE_ERROR,
			"bgl_gst_init",
			"list expected",
			args );
   } else {
      int argc;
      char **argv;
      int len = bgl_list_length( args );
      char *locale = setlocale( LC_ALL, 0 );

      /* convert scheme vector to an char*[] for gst_init */
      argv = alloca( sizeof( char * ) * len );

      argc = 0;
      while( PAIRP( args ) ) {
	 argv[ argc++ ] = BSTRING_TO_STRING( CAR( args ) );
	 args = CDR( args );
      }

      /* initialize GStreamer */
      gst_init( &argc, &argv );

      /* WARNING: restore the previous locale that is changed by gst_init! */
      setlocale( LC_ALL, locale );

      /* allocate the callback array */
      callbacks = g_malloc( sizeof( callback_t ) * callback_length );

      /* bind the bigloo plugins */
      bgl_gst_plugin_port_init();
   }
}
 
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_finalize ...                                             */
/*---------------------------------------------------------------------*/
static void
bgl_gst_finalize( obj_t obj, obj_t proc ) {
   PROCEDURE_ENTRY( proc )( proc, obj, BEOA );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gst_add_finalizer ...                                        */
/*---------------------------------------------------------------------*/
void
bgl_gst_add_finalizer( obj_t obj, obj_t proc ) {
   GC_register_finalizer( obj,
			  (GC_finalization_proc)&bgl_gst_finalize,
			  proc, 0L, 0L );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_object_to_obj ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_object_to_obj( GstObject *obj, obj_t ref ) {
   if( !obj ) return BFALSE;

   //if( ref ) fprintf( stderr, "gst_object_to_obj(%s:%d), ref: builtin=%p refcount=%d\n", __FILE__, __LINE__, obj, obj->ref_count );
   if( GST_IS_PAD( obj ) ) {
      if( ref == BTRUE ) gst_object_ref( obj );
      return bgl_gst_pad_new( GST_PAD( obj ), BTRUE );
   } else if( GST_IS_ELEMENT_FACTORY( obj ) ) {
      if( ref == BTRUE ) gst_object_ref( obj );
      return bgl_gst_element_factory_new(
	 GST_ELEMENT_FACTORY( obj ), BTRUE );
   } else if( GST_IS_PLUGIN_FEATURE( obj ) ) {
      if( ref == BTRUE ) gst_object_ref( obj );
      return bgl_gst_plugin_feature_new(
	 GST_PLUGIN_FEATURE( obj ), BTRUE );
   } else if( GST_IS_PIPELINE( obj ) ) {
      if( ref == BTRUE ) gst_object_ref( obj );
      return bgl_gst_pipeline_new(
	 GST_PIPELINE( obj ), BTRUE );
   } else if( GST_IS_ELEMENT( obj ) ) {
      if( ref == BTRUE ) gst_object_ref( obj );
      return bgl_gst_element_new( GST_ELEMENT( obj ), BTRUE );
   } else if( GST_IS_BIN( obj ) ) {
      if( ref == BTRUE ) gst_object_ref( obj );
      return bgl_gst_bin_new( GST_BIN( obj ), BTRUE );
   } else {
      fprintf( stderr, "WARNING: unmatched type %s (%s:%d)\n",
	       (char *)g_type_name( G_OBJECT_TYPE( obj ) ),
	       __FILE__, __LINE__ );
      
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_g_value_to_obj ...                                           */
/*---------------------------------------------------------------------*/
static obj_t
bgl_g_value_to_obj( const GValue *gval, int doref, int doobj ) {
   switch( G_VALUE_TYPE( gval ) ) {
      case G_TYPE_STRING:
	 if( doobj ) 
	    return string_to_bstring( (char *)g_value_get_string( gval ) );
	 else
	    return BUNSPEC;

      case G_TYPE_BOOLEAN:
	 return BBOOL( g_value_get_boolean( gval ) );
	 
      case G_TYPE_INT:
	 return BINT( g_value_get_int( gval ) );
	 
      case G_TYPE_UINT:
	 return BINT( g_value_get_uint( gval ) );
	 
      case G_TYPE_LONG:
	 return BINT( g_value_get_long( gval ) );

      case G_TYPE_ULONG:
	 return doobj ? make_bllong( g_value_get_ulong( gval ) ) : BUNSPEC;

      case G_TYPE_INT64:
	 return doobj ? make_bllong( g_value_get_int64( gval ) ) : BUNSPEC;

      case G_TYPE_UINT64:
	 return doobj ? make_bllong( g_value_get_uint64( gval ) ) : BUNSPEC;

      case G_TYPE_DOUBLE:
	 return doobj ? make_real( g_value_get_double( gval ) ) : BUNSPEC;

      case G_TYPE_POINTER:
	 if( doobj ) 
	    return void_star_to_obj( g_value_get_pointer( gval ) );
	 else
	    BUNSPEC;

      case G_TYPE_OBJECT: {
	 GObject *obj = g_value_get_object( gval );

	 fprintf( stderr, "G_TYPE_OBJECT not implemented yet %p %s:%d\n",
		  obj, __FILE__, __LINE__ );
	 
	 return BUNSPEC;
      }

      case G_TYPE_ENUM: {
	 long obj = g_value_get_enum( gval );

	 fprintf( stderr, "G_TYPE_ENUM not implemented yet %ld (%s:%d)\n",
		  obj, __FILE__, __LINE__ );

	 return BUNSPEC;
      }

      default: {
	 if( GST_VALUE_HOLDS_CAPS( gval ) ) {
	    GstCaps *caps = GST_CAPS( gst_value_get_caps( gval ) );

	    if( doref ) gst_caps_ref( caps );
	    return doobj ? bgl_gst_caps_new( caps, BTRUE ) : BUNSPEC;
	 }
	 
	 if( GST_VALUE_HOLDS_BUFFER( gval ) ) {
	    GstBuffer *buffer = gst_value_get_buffer( gval );

	    if( buffer ) {
	       if( doref ) gst_buffer_ref( buffer );
	       return doobj ? bgl_gst_buffer_new( buffer, BTRUE ) : BUNSPEC;
	    } else {
	       return BUNSPEC;
	    }
	 }
	 
	 if( GST_VALUE_HOLDS_LIST( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_LIST not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }
	 
	 if( GST_VALUE_HOLDS_INT_RANGE( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_INT_RANGE not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( GST_VALUE_HOLDS_DOUBLE_RANGE( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_DOUBLE_RANGE not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( GST_VALUE_HOLDS_FRACTION_RANGE( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_FRACTION_RANGE not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( GST_VALUE_HOLDS_ARRAY( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_ARRAY not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( GST_VALUE_HOLDS_FRACTION( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_FRACTION not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }
	 
	 if( G_VALUE_HOLDS( gval, G_TYPE_DATE ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_DATE not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( GST_VALUE_HOLDS_STRUCTURE( gval ) ) {
	    fprintf( stderr, "GST_VALUE_HOLDS_STRUCTURE not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( G_VALUE_HOLDS_BOXED( gval ) ) {
	    fprintf( stderr, "G_VALUE_HOLDS_BOXED not implemented yet %s:%d\n",
		     __FILE__, __LINE__ );
	    return BUNSPEC;
	 }

	 if( G_VALUE_HOLDS_OBJECT( gval ) ) {
	    GstObject *obj = (GstObject *)g_value_get_object( gval );
	    if( doref ) gst_object_ref( obj );
	    return doobj ? bgl_gst_object_to_obj( obj, 0 ) : BUNSPEC;
	 } else {
	    char *name = (char *)g_type_name( G_VALUE_TYPE( gval ) );

	    if( name && !strcmp( name, "GstState" ) ) {
	       return bgl_gst_state_to_obj( (GstState)g_value_get_enum( gval ) );
	    } else {
	       return BUNSPEC;
	    }
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_g_value_to_obj_sans_ref ...                                  */
/*---------------------------------------------------------------------*/
static obj_t
bgl_g_value_to_obj_sans_ref( const GValue *gval, obj_t ref ) {
   obj_t obj = bgl_g_value_to_obj( gval, 0, 1 );

   g_free( (gpointer)gval );
   return obj;
}

/*---------------------------------------------------------------------*/
/*    static GValue *                                                  */
/*    bgl_g_value_markandcopy ...                                      */
/*---------------------------------------------------------------------*/
static GValue *
bgl_g_value_markandcopy( const GValue *gval ) {
   GValue *gnew = g_malloc0( sizeof( GValue ) );

   g_value_init( gnew, G_VALUE_TYPE( gval ) );
   g_value_copy( gval, gnew );

   /* mark the object */
   bgl_g_value_to_obj( gnew, 1, 0 );

   return gnew;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    enlarge_callback_array ...                                       */
/*---------------------------------------------------------------------*/
static void
enlarge_callback_array() {
   callback_t *ncallbacks;
   int osize = callback_length * sizeof( callback_t );

   callback_length *= 2;
   ncallbacks = g_malloc( osize * 2 );
   memcpy( ncallbacks, callbacks, osize );
   
   g_free( callbacks );
   callbacks = ncallbacks;
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_gst_register_async_callback ...                              */
/*---------------------------------------------------------------------*/
static void
bgl_gst_register_async_callback( callback_t cb ) {
   /* signal the callback */
   bgl_gst_lock();

   if( callback_index == callback_length ) enlarge_callback_array();

   callbacks[ callback_index++ ] = cb;

   bgl_gst_signal();
   bgl_gst_unlock();
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gst_invoke_callbacks ...                                     */
/*---------------------------------------------------------------------*/
void
bgl_gst_invoke_callbacks() {
   while( callback_index > 0 ) {
      callback_t cb = callbacks[ --callback_index ];
      obj_t proc = cb->proc;
      obj_t (*convert)( void *, obj_t ) = cb->convert;

      CHECK_PROCEDURE( proc, cb->arity );

      switch( cb->arity ) {
	 case 0:
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 BEOA );
	    break;
	    
	 case 1:
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 convert( cb->args[ 0 ], BTRUE ),
		 BEOA );
	    break;
	    
	 case 2:
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 convert( cb->args[ 0 ], BTRUE ),
		 convert( cb->args[ 1 ], BTRUE ),
		 BEOA );
	    break;
	    
	 case 3:
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 convert( cb->args[ 0 ], BTRUE ),
		 convert( cb->args[ 1 ], BTRUE ),
		 convert( cb->args[ 2 ], BTRUE ),
		 BEOA );
	    break;
	    
	 case 4:
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 convert( cb->args[ 0 ], BTRUE ),
		 convert( cb->args[ 1 ], BTRUE ),
		 convert( cb->args[ 2 ], BTRUE ),
		 convert( cb->args[ 3 ], BTRUE ),
		 BEOA );
	    break;
      }

      g_free( cb );
   }
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    gst_registry_list_to_obj ...                                     */
/*---------------------------------------------------------------------*/
static obj_t
gst_registry_list_to_obj( GList *glist, obj_t (*constr)() ) {
   obj_t res = BNIL;
   obj_t last = 0L;
   GList *gl = glist;
   
   while( gl ) {
      GstObject *factory = GST_OBJECT( gl->data );
      obj_t p = MAKE_PAIR( constr( factory, BTRUE ), BNIL );

      gst_object_ref( factory );
      
      if( last ) {
	 SET_CDR( last, p );
	 last = p;
      } else {
	 res = p;
	 last = p;
      }
      
      gl = g_list_next( gl );
   }

   gst_plugin_feature_list_free( glist );

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_registry_get_element_factory_list ...                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_registry_get_element_factory_list( GstRegistry *reg ) {
   return gst_registry_list_to_obj(
      gst_registry_get_feature_list( reg, GST_TYPE_ELEMENT_FACTORY ),
      &bgl_gst_element_factory_new );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    gst_feature_to_obj ...                                           */
/*---------------------------------------------------------------------*/
static obj_t
gst_feature_to_obj( GObject *obj, obj_t finalizer ) {
   return bgl_gst_object_to_obj( (GstObject *)obj, 0 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_registry_get_feature_list_by_plugin ...                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_registry_get_feature_list_by_plugin( GstRegistry *reg, char *name ) {
   return gst_registry_list_to_obj(
      gst_registry_get_feature_list_by_plugin( reg, name ),
      &gst_feature_to_obj );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_registry_get_plugin_list ...                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_registry_get_plugin_list( GstRegistry *reg ) {
   return gst_registry_list_to_obj(
      gst_registry_get_plugin_list( reg ),
      &bgl_gst_plugin_new );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_element_factory_get_uri_protocols ...                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_element_factory_get_uri_protocols( GstElementFactory *factory ) {
   const gchar * const *array = gst_element_factory_get_uri_protocols( factory );

   if( !array ) {
      return BNIL;
   } else {
      obj_t res = MAKE_PAIR( BUNSPEC, BNIL );
      obj_t last = res;

      while( *array ) {
	 obj_t pair = MAKE_PAIR( string_to_bstring( *((char **)array) ), BNIL );
	 SET_CDR( last, pair );
      
	 array++;
	 last = pair;
      }

      return CDR( res );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_element_factory_get_static_pad_templates ...             */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_element_factory_get_static_pad_templates( GstElementFactory *factory ) {
   obj_t res = BNIL;
   obj_t last = 0L;
   GList *gl = (GList *)gst_element_factory_get_static_pad_templates( factory );
   
   while( gl ) {
      GstStaticPadTemplate *tpl = (GstStaticPadTemplate *)( gl->data );
      obj_t p = MAKE_PAIR( bgl_gst_static_pad_template_new( tpl ), BNIL );
      if( last ) {
	 SET_CDR( last, p );
	 last = p;
      } else {
	 res = p;
	 last = p;
      }
      
      gl = g_list_next( gl );
   }

   return res;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gst_invoke_finalizers ...                                    */
/*---------------------------------------------------------------------*/
void
bgl_gst_invoke_finalizers() {
#if( !BGL_AUTO_FINALIZER )
#  if( !defined( BGL_FORBID_GC_COLLECT ) )
   GC_COLLECT();
#  endif   
   GC_invoke_finalizers();
#endif
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    get_property ...                                                 */
/*---------------------------------------------------------------------*/
static obj_t
get_property( GstObject *obj, char *prop, GParamSpec *gspec ) {
   GValue gval = {0,};
   obj_t res;

   g_value_init( &gval, G_PARAM_SPEC_VALUE_TYPE( gspec ) );
   g_object_get_property( G_OBJECT( obj ), prop, &gval );

   res = bgl_g_value_to_obj( &gval, 1, 1 );
   
   g_value_unset( &gval );
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_object_get_property ...                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_object_get_property( GstObject *obj, char *prop ) {
   GObjectClass *klass = G_OBJECT_GET_CLASS( G_OBJECT( obj ) );
   GParamSpec *gspec = g_object_class_find_property( klass, prop );
   
   if( gspec && ((gspec->flags & G_PARAM_READABLE) == G_PARAM_READABLE) ) {
      return get_property( obj, prop, gspec );
   } else {
      C_SYSTEM_FAILURE( BGL_ERROR,
			"gst-object-property",
			"unreadable property",
			string_to_bstring( prop ) );
      return BFALSE;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_object_property_list ...                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_object_property_list( GstObject *obj ) {
   guint n;
   GObjectClass *klass = G_OBJECT_GET_CLASS( G_OBJECT( obj ) );
   GParamSpec **array = g_object_class_list_properties( klass, &n );
   obj_t res = BNIL;

   while( n-- > 0 ) {
      GParamSpec *gspec = array[ n ];
      
      if( (gspec->flags & G_PARAM_READABLE) == G_PARAM_READABLE ) {
	 char *prop = (char *)g_param_spec_get_name( gspec );

	 res = MAKE_PAIR( get_property( obj, prop, gspec ), res );
	 res = MAKE_PAIR( string_to_keyword( prop ), res );
      }
   }

   return res;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_g_value_init ...                                             */
/*---------------------------------------------------------------------*/
static void
bgl_g_value_init( GValue *gval, obj_t val ) {
   if( STRINGP( val ) ) {
      g_value_init( gval, G_TYPE_STRING );
      g_value_set_string( gval, BSTRING_TO_STRING( val ) );
      return;
   }

   if( BOOLEANP( val ) ) {
      g_value_init( gval, G_TYPE_BOOLEAN );
      g_value_set_boolean( gval, CBOOL( val ) );
      return;
   }
   
   if( INTEGERP( val ) ) {
      g_value_init( gval, G_TYPE_INT );
      g_value_set_int( gval, CINT( val ) );
      return;
   }

   if( REALP( val ) ) {
      g_value_init( gval, G_TYPE_DOUBLE );
      g_value_set_double( gval, REAL_TO_DOUBLE( val ) );
      return;
   }
 
   if( INPUT_PORTP( val ) || OUTPUT_PORTP( val ) ) {
      g_value_init( gval, G_TYPE_POINTER );
      g_value_set_pointer( gval, val );
      return;
   }

   if( bgl_gst_objectp( val ) ) {
      g_value_init( gval, G_TYPE_POINTER );
      g_value_set_pointer( gval, bgl_gst_object_to_gstobject( val ) );
      return;
   }
      
   if( PAIRP( val ) && 
       SYMBOLP( CAR( val ) ) && 
       PAIRP( CDR( val ) ) && 
       INTEGERP( CAR( CDR( val ) ) ) ) {
      char *s = BSTRING_TO_STRING( SYMBOL_TO_STRING( CAR( val ) ) );

      if( !strcmp( s, "int" ) ) {
	 g_value_init( gval, G_TYPE_INT );
	 g_value_set_int( gval, CINT( CAR( CDR( val ) ) ) );
	 return;
      }
      if( !strcmp( s, "long" ) ) {
	 g_value_init( gval, G_TYPE_LONG );
	 g_value_set_long( gval, CINT( CAR( CDR( val ) ) ) );
	 return;
      }
      if( !strcmp( s, "uint" ) ) {
	 g_value_init( gval, G_TYPE_UINT );
	 g_value_set_uint( gval, CINT( CAR( CDR( val ) ) ) );
	 return;
      }
      if( !strcmp( s, "ulong" ) ) {
	 g_value_init( gval, G_TYPE_ULONG );
	 g_value_set_ulong( gval, CINT( CAR( CDR( val ) ) ) );
	 return;
      }
      if( !strcmp( s, "fraction" ) ) {
	 g_value_init( gval, GST_TYPE_FRACTION );
	 gst_value_set_fraction( gval,
				 CINT( CAR( CDR( val ) ) ),
				 CINT( CAR( CDR( CDR( val ) ) ) ) );
	 return;
      }
      C_SYSTEM_FAILURE( BGL_TYPE_ERROR,
			"bgl_g_value_init",
			"Illegal cast type",
			val );
   }
   
   C_SYSTEM_FAILURE( BGL_TYPE_ERROR,
		     "bgl_g_value_init",
		     "Illegal obj type",
		     val );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_object_set_property ...                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_object_set_property( GstObject *obj, char *prop, obj_t val ) {
   GValue gval = {0,};

   bgl_g_value_init( &gval, val );
   
   g_object_set_property( G_OBJECT( obj ), prop, &gval );

   g_value_unset( &gval );
   
   return val;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    closure_finalize_notify ...                                      */
/*---------------------------------------------------------------------*/
static void
closure_finalize_notify( gpointer data, GClosure *closure ) {
   fprintf( stderr, "%s:%d, closure finalize...: %p\n", __FILE__, __LINE__, data );
/*    bgl_closure_gcunmark( data );                                    */
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    closure_marshal ...                                              */
/*---------------------------------------------------------------------*/
void
closure_marshal( GClosure *closure,
		 GValue *return_value,
		 guint n_param_values,
		 const GValue *param_values,
		 gpointer invocation_hint,
		 gpointer marshal_data ) {
   obj_t proc = (obj_t)closure->data;
   callback_t cb = g_malloc( sizeof( struct callback ) +
			     (n_param_values - 1) * sizeof( void * ) );
   cb->convert = (obj_t (*)( void *, obj_t ))bgl_g_value_to_obj_sans_ref;
   cb->proc = proc;
   cb->arity = n_param_values;

   CHECK_PROCEDURE( proc, n_param_values );
   
   switch( n_param_values ) {
      case 0: {
	 if( bglgst_use_threadsp() ) {
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 BEOA );
	 } else {
	    bgl_gst_register_async_callback( cb );
	 }
	 return;
      }

      case 1: {
	 if( bglgst_use_threadsp() ) {
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 bgl_g_value_to_obj( param_values + 0, 1, 1 ),
		 BEOA );
	 } else {
	    cb->args[ 0 ] = bgl_g_value_markandcopy( param_values + 0 );
	    bgl_gst_register_async_callback( cb );
	 }
	 return;
      }

      case 2: {
	 if( bglgst_use_threadsp() ) {
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 bgl_g_value_to_obj( param_values + 0, 1, 1 ),
		 bgl_g_value_to_obj( param_values + 1, 1, 1 ),
		 BEOA );
	 } else {
	    cb->args[ 0 ] = bgl_g_value_markandcopy( param_values + 0 );
	    cb->args[ 1 ] = bgl_g_value_markandcopy( param_values + 1 );
	    bgl_gst_register_async_callback( cb );
	 }
	 return;
      }

      case 3: {
	 if( bglgst_use_threadsp() ) {
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 bgl_g_value_to_obj( param_values + 0, 1, 1 ),
		 bgl_g_value_to_obj( param_values + 1, 1, 1 ),
		 bgl_g_value_to_obj( param_values + 2, 1, 1 ),
		 BEOA );
	 } else {
	    cb->args[ 0 ] = bgl_g_value_markandcopy( param_values + 0 );
	    cb->args[ 1 ] = bgl_g_value_markandcopy( param_values + 1 );
	    cb->args[ 2 ] = bgl_g_value_markandcopy( param_values + 2 );
	    bgl_gst_register_async_callback( cb );
	 }
	 return;
      }

      case 4: {
	 if( bglgst_use_threadsp() ) {
	    PROCEDURE_ENTRY( proc )
	       ( proc,
		 bgl_g_value_to_obj( param_values + 0, 1, 1 ),
		 bgl_g_value_to_obj( param_values + 1, 1, 1 ),
		 bgl_g_value_to_obj( param_values + 2, 1, 1 ),
		 bgl_g_value_to_obj( param_values + 3, 1, 1 ),
		 BEOA );
	 } else {
	    cb->args[ 0 ] = bgl_g_value_markandcopy( param_values + 0 );
	    cb->args[ 1 ] = bgl_g_value_markandcopy( param_values + 1 );
	    cb->args[ 2 ] = bgl_g_value_markandcopy( param_values + 2 );
	    cb->args[ 3 ] = bgl_g_value_markandcopy( param_values + 3 );
	    bgl_gst_register_async_callback( cb );
	 }
	 return;
      }

      default:
	 fprintf( stderr, "closure_marshall: %d %p\n", n_param_values, proc );
   }

}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_object_connect ...                                       */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_object_connect( GstObject *obj, char *signal, obj_t proc ) {
   GClosure *closure;

   if( !PROCEDUREP( proc ) ) {
      C_SYSTEM_FAILURE( BGL_TYPE_ERROR,
			"gst-object-connect",
			"Illegal procedure",
			proc );
   }

//   bgl_closure_gcmark( proc );
   closure = g_closure_new_simple( sizeof( GClosure ), proc );
/*    g_closure_add_finalize_notifier( closure, (void *)closure, &closure_finalize_notify ); */
/*    g_closure_add_invalidate_notifier( closure, (void *)closure, &closure_finalize_notify ); */
/*                                                                     */
   g_closure_set_marshal( closure, closure_marshal );
   g_signal_connect_closure( obj, signal, closure, 0 );
   
   return proc;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_gsttag_value_to_obj ...                                      */
/*---------------------------------------------------------------------*/
static obj_t
bgl_gsttag_value_to_obj( const GstTagList *list, const gchar *tag ) {
   
   switch( gst_tag_get_type( tag ) ) {
      case G_TYPE_STRING: {
	 char *str;
	 obj_t res;
	 
	 gst_tag_list_get_string( list, tag, &str );
	 res = string_to_bstring( (char *)str );
	 g_free( str );
	 
	 return res;
      }

      case G_TYPE_BOOLEAN: {
	 gboolean b;

	 gst_tag_list_get_boolean( list, tag, &b );
	 
	 return BBOOL( b );
      }
	 

      case G_TYPE_CHAR: {
	 gint c;

	 gst_tag_list_get_int( list, tag, &c );
	 
	 return BCHAR( c );
      }
	 

      case G_TYPE_INT: {
	 gint i;

	 gst_tag_list_get_int( list, tag, &i );
	 
	 return BINT( i );
      }

      case G_TYPE_UINT: {
	 guint i;

	 gst_tag_list_get_uint( list, tag, &i );
	 
	 return make_belong( i );
      }

      case G_TYPE_LONG:
      case G_TYPE_INT64: {
	 gint64 i;

	 gst_tag_list_get_int64( list, tag, &i );
	 
	 return make_bllong( (BGL_LONGLONG_T)i );
      }

      case G_TYPE_ULONG:
      case G_TYPE_UINT64: {
	 guint64 i;

	 gst_tag_list_get_uint64( list, tag, &i );
	 
	 return make_bllong( (BGL_LONGLONG_T)i );
      }

      default: {
	 char *tname = (char *)g_type_name( gst_tag_get_type( tag ) );
	 if( !strcmp( tname, "GstDate" ) ) {
	    GDate *date;

	    if( gst_tag_list_get_date( list, tag, &date ) )
	       return bgl_make_date( 0, 1, 1, 1,
				     date->day, date->month, date->year,
				     0, 0, 0 );
	    else
	       return BUNSPEC;
	 } if( !strcmp( tname, "GstBuffer" ) ) {
	    return BUNSPEC;
	 } if( !strcmp( tname, "gdouble" ) ) {
	    gdouble d;
	    return make_real( gst_tag_list_get_double( list, tag, &d ) );
	 } else {
	    fprintf( stderr, "WARNING: bgl_gsttag_value_to_obj (%s:%d), unknown tag type %s \n",
		     __FILE__, __LINE__, 
		     (char *)g_type_name( gst_tag_get_type( tag ) ) );
	    
	    return BUNSPEC;
	 }
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    gst_tag_fun ...                                                  */
/*---------------------------------------------------------------------*/
static void
gst_tag_fun( const GstTagList *list, const gchar *tag, gpointer data ) {
   obj_t res = (obj_t)data;
   obj_t cell = MAKE_PAIR( string_to_bstring( (char *)tag ),
			   bgl_gsttag_value_to_obj( list, tag ) );
   
   SET_CDR( res, MAKE_PAIR( cell, CDR( res ) ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_buffer_get_string ...                                    */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_buffer_get_string( GstBuffer *buf )
{
   GstMapInfo info;
   obj_t str;

   if ( !gst_buffer_map( buf, &info, GST_MAP_READ ))
      str = BNIL;
   else {
      str = string_to_bstring_len( (char *)info.data, (int)info.size );
      gst_buffer_unmap( buf, &info );
   }
   return str;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gst_buffer_set_string ...                                    */
/*---------------------------------------------------------------------*/
void
bgl_gst_buffer_set_string( GstBuffer *buf, obj_t str )
{
   GstMapInfo info;
   long len = STRING_LENGTH (str);

   gst_buffer_set_size( buf, (gssize)len );
   if ( gst_buffer_map( buf, &info, GST_MAP_WRITE ))
   {
      memcpy( info.data, BSTRING_TO_STRING( str ), (size_t)len );
      gst_buffer_unmap( buf, &info );
   }
}

/*---------------------------------------------------------------------*/
/*    static char *                                                    */
/*    bgl_gst_message_error_parser ...                                 */
/*---------------------------------------------------------------------*/
char *
bgl_gst_message_error_parser( GstMessage *msg, void (*parser)() ) {
   GError *err;
   char *debug;
   char *str;
   
   parser( msg, &err, &debug );

   if( *debug ) {
      str = (char *)GC_MALLOC_ATOMIC( strlen( err->message ) +
				      strlen( debug ) +
				      2 );
      sprintf( str, "%s\n%s", err->message, debug );
   } else {
      str = (char *)GC_MALLOC_ATOMIC( strlen( err->message ) + 1 );
      strcpy( str, err->message );
   }
      
   g_free( debug );
   g_error_free( err );

   return str;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_gst_message_error_string ...                                 */
/*---------------------------------------------------------------------*/
char *
bgl_gst_message_error_string( GstMessage *msg ) {
   return bgl_gst_message_error_parser( msg, &gst_message_parse_error );
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_gst_message_info_string ...                                  */
/*---------------------------------------------------------------------*/
char *
bgl_gst_message_info_string( GstMessage *msg ) {
   return bgl_gst_message_error_parser( msg, &gst_message_parse_info );
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_gst_message_warning_string ...                               */
/*---------------------------------------------------------------------*/
char *
bgl_gst_message_warning_string( GstMessage *msg ) {
   return bgl_gst_message_error_parser( msg, &gst_message_parse_warning );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_message_tag_list ...                                     */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_message_tag_list( GstMessage *msg ) {
   GstTagList *tag_list;
   obj_t res = MAKE_PAIR( BNIL, BNIL );
   
   gst_message_parse_tag( msg, &tag_list );

   gst_tag_list_foreach( tag_list, &gst_tag_fun, (gpointer)res );
   gst_tag_list_free( tag_list );
   
   return CDR( res );
}

/*---------------------------------------------------------------------*/
/*    GstState                                                         */
/*    bgl_gst_message_new_state ...                                    */
/*---------------------------------------------------------------------*/
GstState
bgl_gst_message_new_state( GstMessage *msg ) {
   GstState ostate, nstate, pending;
   
   gst_message_parse_state_changed( msg, &ostate, &nstate, &pending );

   return nstate;
}

/*---------------------------------------------------------------------*/
/*    GstState                                                         */
/*    bgl_gst_message_old_state ...                                    */
/*---------------------------------------------------------------------*/
GstState
bgl_gst_message_old_state( GstMessage *msg ) {
   GstState ostate, nstate, pending;
   
   gst_message_parse_state_changed( msg, &ostate, &nstate, &pending );

   return ostate;
}

/*---------------------------------------------------------------------*/
/*    GstState                                                         */
/*    bgl_gst_message_pending_state ...                                */
/*---------------------------------------------------------------------*/
GstState
bgl_gst_message_pending_state( GstMessage *msg ) {
   GstState ostate, nstate, pending;
   
   gst_message_parse_state_changed( msg, &ostate, &nstate, &pending );

   return pending;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_message_get_src ...                                      */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_message_get_src( GstMessage *msg ) {
   return bgl_gst_object_to_obj( (GstObject *)GST_MESSAGE_SRC( msg ), BTRUE );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_gst_message_stream_status_type ...                           */
/*---------------------------------------------------------------------*/
int
bgl_gst_message_stream_status_type( GstMessage *msg ) {
   GstStreamStatusType type;
   GstElement *el;
   gst_message_parse_stream_status( msg, &type, &el );
   
   return (int)type;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_element_interface_list ...                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_element_interface_list( GstElement *el ) {
   GType type = G_OBJECT_TYPE( el );
   guint n_ifaces;
   GType *iface, *ifaces = g_type_interfaces( type, &n_ifaces );

   if( ifaces ) {
      if( n_ifaces ) {
	 obj_t res = BNIL;
	 
	 iface = ifaces;
	 while( *iface ) {
	    res = MAKE_PAIR( string_to_bstring( (char *)g_type_name( *iface ) ),
			     res );
	    iface++;
	 }
	 g_free( ifaces );

	 return res;
      }

      return BNIL;
   } else {
      return BNIL;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_gst_element_query_position ...                               */
/*---------------------------------------------------------------------*/
BGL_LONGLONG_T
bgl_gst_element_query_position( GstElement *el ) {
   gint64 res;
   
   if( gst_element_query_position( el, GST_FORMAT_TIME, &res ) )
      return (BGL_LONGLONG_T)res;
   else
      return (BGL_LONGLONG_T)-1;
}

/*---------------------------------------------------------------------*/
/*    BGL_LONGLONG_T                                                   */
/*    bgl_gst_element_query_duration ...                               */
/*---------------------------------------------------------------------*/
BGL_LONGLONG_T
bgl_gst_element_query_duration( GstElement *el ) {
   gint64 res;
   
   if( gst_element_query_duration( el, GST_FORMAT_TIME, &res ) )
      return (BGL_LONGLONG_T)res;
   else
      return (BGL_LONGLONG_T)-1;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_structure_get_property ...                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_structure_get_property( GstStructure *obj, char *prop ) {
   return bgl_g_value_to_obj( gst_structure_get_value( obj, prop ), 1, 1 );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_structure_set_property ...                               */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_structure_set_property( GstStructure *obj, char *prop, obj_t val ) {
   GValue gval;

   bgl_g_value_init( &gval, val );
   gst_structure_set_value( obj, prop, &gval );
   return val;
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    gst_structure_fun ...                                            */
/*---------------------------------------------------------------------*/
static gboolean
gst_structure_fun( GQuark field_id, const GValue *gval, gpointer data ) {
   obj_t res = (obj_t)data;
   char *name = (char *)g_quark_to_string( field_id );
   obj_t cell = MAKE_PAIR( string_to_bstring( name ),
			   bgl_g_value_to_obj( gval, 1, 1 ) );
   
   SET_CDR( res, MAKE_PAIR( cell, CDR( res ) ) );

   return TRUE;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_structure_property_list ...                              */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_structure_property_list( GstStructure *obj ) {
   obj_t res = MAKE_PAIR( BNIL, BNIL );
   
   gst_structure_foreach( obj, &gst_structure_fun, (gpointer)res );

   return CDR( res );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_caps_new_simple ...                                      */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_caps_new_simple( obj_t media_type, obj_t pair, obj_t finalizer ) {
   GstCaps *caps = gst_caps_new_empty();
   GstStructure *s = gst_structure_new_empty( BSTRING_TO_STRING( media_type ) );
   
   while( PAIRP( pair ) ) {
      char *fid = BSTRING_TO_STRING( KEYWORD_TO_STRING( CAR( pair ) ) );
      GValue gval = {0,};
      
      bgl_g_value_init( &gval, CAR( CDR( pair ) ) );
      
      gst_structure_set_value( s, fid, &gval );

      pair = CDR( CDR( pair ) );
			       
   }
   
   gst_caps_append_structure( caps, s );

   return bgl_gst_caps_new( caps, finalizer );
}


/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    make_bin ...                                                     */
/*---------------------------------------------------------------------*/
static obj_t
make_bin( GstElement *el ) {
   obj_t obj = bgl_gst_object_to_obj( (GstObject *)el, 0 );
   
   GList *gl = GST_BIN_CHILDREN( el );
   obj_t bl = MAKE_PAIR( BNIL, BNIL );
   obj_t last = bl;

   while( gl ) {
      obj_t o = bgl_gst_object_to_obj( (GstObject *)gl->data, BTRUE );
      obj_t p = MAKE_PAIR( o, BNIL );

      SET_CDR( last, p );
      last = p;

      gl = g_list_next( gl );
   }

   bgl_gst_bin_elements_set( obj, CDR( bl ) );
   
   return obj;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_parse_launch ...                                         */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_parse_launch( char *descr ) {
   GError *err = NULL;
   GstElement *el = gst_parse_launch( descr, &err );

   if( !el ) {
      obj_t msg = string_to_bstring( err->message );
      C_SYSTEM_FAILURE( BGL_ERROR,
			"gst-parse-launch",
			"Cannot construct pipeline",
			msg );

      return BUNSPEC;
   } else {
      if( err ) {
	 fprintf( stderr, "*** WARNING: %s\n", err->message );
      }

      return make_bin( el );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gst_parse_launchv ...                                        */
/*---------------------------------------------------------------------*/
obj_t
bgl_gst_parse_launchv( obj_t args ) {
   GError *err = NULL;
   GstElement *el;
   int argc;
   char **argv;
   int len = bgl_list_length( args );

   argv = alloca( sizeof( char * ) * (len + 1) );

   argc = 0;
   while( PAIRP( args ) ) {
      argv[ argc++ ] = BSTRING_TO_STRING( CAR( args ) );
      args = CDR( args );
   }

   argv[ argc ] = 0;
   
   el = gst_parse_launchv( (const gchar **)argv, &err );

   if( !el ) {
      obj_t msg = string_to_bstring( err->message );
      C_SYSTEM_FAILURE( BGL_ERROR,
			"gst-parse-launch",
			"Cannot construct pipeline",
			msg );
      return BUNSPEC;
   } else {
      if( err ) {
	 fprintf( stderr, "*** WARNING: %s\n", err->message );
      }

      return make_bin( el );
   }
}

/*---------------------------------------------------------------------*/
/*    static gboolean                                                  */
/*    buffer_probe_call ...                                            */
/*---------------------------------------------------------------------*/
static GstPadProbeReturn
buffer_probe_call( GstPad *pad, GstPadProbeInfo *info, gpointer data ) {
   obj_t proc = (obj_t)data;
   GstBuffer *buffer = (GstBuffer *)info->data;
   return (CBOOL (PROCEDURE_ENTRY( proc )( proc,
					   bgl_gst_buffer_new( buffer, BFALSE ),
					   BEOA )))
      ? GST_PAD_PROBE_OK
      : GST_PAD_PROBE_DROP;
}
   
/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_gst_pad_add_probe ...                                        */
/*---------------------------------------------------------------------*/
int
bgl_gst_pad_add_probe( GstPad *pad, GstPadProbeType mask, obj_t proc ) {
   /* protect the closure from GC reclaim */
   /* CARE: when should it be freed? */
   bgl_closure_gcmark( proc );
   return gst_pad_add_probe( pad,
			     mask,
			     buffer_probe_call,
			     (gpointer)proc,
			     NULL );
}

/*---------------------------------------------------------------------*/
/*    gboolean                                                         */
/*    bgl_gst_pad_set_caps ...                                         */
/*---------------------------------------------------------------------*/
gboolean
bgl_gst_pad_set_caps( GstPad *pad, GstCaps *caps ) {
   gboolean res;

   if (caps != NULL && gst_caps_is_fixed (caps))
   {
      res = FALSE;
   }
   else
   {
      GstEvent *event = gst_event_new_caps (caps);
      if (GST_PAD_IS_SRC (pad))
	 res = gst_pad_push_event (pad, event);
      else
	 res = gst_pad_send_event (pad, event);
   }
   return res;
}

/*---------------------------------------------------------------------*/
/*    static GstBusSyncReply                                           */
/*    bus_sync_handler ...                                             */
/*---------------------------------------------------------------------*/
static GstBusSyncReply
bus_sync_handler( GstBus *bus, GstMessage *msg, gpointer data ) {
   obj_t proc = (obj_t)data;
   callback_t cb = g_malloc( sizeof( struct callback ) );

   cb->convert = (obj_t (*)( void *, obj_t ))bgl_gst_message_new;
   cb->proc = proc;
   cb->arity = 1;
   cb->args[ 0 ] = msg;

   bgl_gst_register_async_callback( cb );
   return GST_BUS_DROP;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_gst_bus_set_sync_handler ...                                 */
/*---------------------------------------------------------------------*/
void
bgl_gst_bus_set_sync_handler( GstBus *bus, obj_t proc ) {
   /* protect the closure from GC reclaim */
   /* CARE: when should it be freed? */
   bgl_closure_gcmark( proc );

   return gst_bus_set_sync_handler( bus,
				    &bus_sync_handler,
				    (gpointer)proc,
				    NULL );
}
