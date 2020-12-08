/*=====================================================================*/
/*    .../prgm/project/bigloo/bigloo/api/avahi/src/Clib/bglavahi.c     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 20 14:50:56 2011                          */
/*    Last change :  Wed Apr  1 19:29:25 2020 (serrano)                */
/*    Copyright   :  2011-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    avahi Bigloo binding                                             */
/*    avahi documentation available at:                                */
/*    http://avahi.sourcearchive.com/documentation/0.6.25-1/main.html  */
/*=====================================================================*/
#include <bigloo.h>

#include <avahi-client/client.h>
#include <avahi-client/lookup.h>
#include <avahi-client/publish.h>

#include <avahi-common/simple-watch.h>
#include <avahi-common/thread-watch.h>
#include <avahi-common/error.h>
#include <avahi-common/timeval.h>

#include "bglavahi.h"
#include "bavahi.h"

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t bgl_avahi_error( char *, char *, obj_t, int );
extern obj_t bgl_avahi_client_state_to_symbol( AvahiClientState );
extern obj_t bgl_avahi_entry_group_state_to_symbol( AvahiEntryGroupState );
extern obj_t bgl_avahi_protocol_to_symbol( AvahiProtocol );
extern obj_t bgl_avahi_browser_event_to_symbol( AvahiBrowserEvent );
extern obj_t bgl_avahi_resolver_event_to_symbol( AvahiResolverEvent );
extern AvahiProtocol bgl_avahi_symbol_to_protocol();
extern obj_t bgl_avahi_lock();
extern obj_t bgl_avahi_unlock();
extern obj_t bgl_avahi_signal();

/*---------------------------------------------------------------------*/
/*    Strings                                                          */
/*---------------------------------------------------------------------*/
#define BGL_STRING_TO_STRING( o ) \
   (STRING_LENGTH( o ) == 0 ? 0L : (const char *)(BSTRING_TO_STRING( o )))

/*---------------------------------------------------------------------*/
/*    avahi_XXX_poll bigloo object                                     */
/*---------------------------------------------------------------------*/
#define bgl_avahi_poll_t BgL_avahizd2pollzd2_bglt
#define BGL_AVAHI_POLL_BUILTIN( o )		\
   (((bgl_avahi_poll_t)CREF(o))->BgL_z42builtinz42)

#define BGL_AVAHI_POLL_CTYPE( o ) \
   (((bgl_avahi_poll_t)CREF(o))->BgL_z42ctypez42)

#define bgl_avahi_simple_poll_t BgL_avahizd2simplezd2pollz00_bglt
#define BGL_AVAHI_SIMPLE_POLL_BUILTIN( o ) \
   (((bgl_avahi_simple_poll_t)CREF(o))->BgL_z42builtinz42)

#define bgl_avahi_threaded_poll_t BgL_avahizd2threadedzd2pollz00_bglt
#define BGL_AVAHI_THREADED_POLL_BUILTIN( o ) \
   (((bgl_avahi_threaded_poll_t)CREF(o))->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    avahi_client                                                     */
/*---------------------------------------------------------------------*/
#define bgl_avahi_client_t BgL_avahizd2clientzd2_bglt

#define BGL_AVAHI_CLIENT_BUILTIN( o ) \
   (((bgl_avahi_client_t)CREF(o))->BgL_z42builtinz42)
#define BGL_AVAHI_CLIENT_PROC( o ) \
   (((bgl_avahi_client_t)CREF(o))->BgL_procz00)
#define BGL_AVAHI_CLIENT_POLL( o ) \
   (((bgl_avahi_client_t)CREF(o))->BgL_pollz00)

/*---------------------------------------------------------------------*/
/*    avahi_entry_group                                                */
/*---------------------------------------------------------------------*/
#define bgl_avahi_entry_group_t BgL_avahizd2entryzd2groupz00_bglt

#define BGL_AVAHI_ENTRY_GROUP_BUILTIN( o ) \
   (((bgl_avahi_entry_group_t)CREF(o))->BgL_z42builtinz42)
#define BGL_AVAHI_ENTRY_GROUP_CLIENT( o ) \
   (((bgl_avahi_entry_group_t)CREF(o))->BgL_clientz00)
#define BGL_AVAHI_ENTRY_GROUP_PROC( o ) \
   (((bgl_avahi_entry_group_t)CREF(o))->BgL_procz00)

/*---------------------------------------------------------------------*/
/*    avahi_service_browser                                            */
/*---------------------------------------------------------------------*/
#define bgl_avahi_service_browser_t BgL_avahizd2servicezd2browserz00_bglt

#define BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) \
   (((bgl_avahi_service_browser_t)CREF(o))->BgL_z42builtinz42)
#define BGL_AVAHI_SERVICE_BROWSER_CLIENT( o ) \
   (((bgl_avahi_service_browser_t)CREF(o))->BgL_clientz00)
#define BGL_AVAHI_SERVICE_BROWSER_PROC( o ) \
   (((bgl_avahi_service_browser_t)CREF(o))->BgL_procz00)
#define BGL_AVAHI_SERVICE_BROWSER_TYPE( o ) \
   (((bgl_avahi_service_browser_t)CREF(o))->BgL_typez00)
#define BGL_AVAHI_SERVICE_BROWSER_DOMAIN( o ) \
   (((bgl_avahi_service_browser_t)CREF(o))->BgL_domainz00)

/*---------------------------------------------------------------------*/
/*    avahi_service_type_browser                                       */
/*---------------------------------------------------------------------*/
#define bgl_avahi_service_type_browser_t BgL_avahizd2servicezd2typezd2browserzd2_bglt

#define BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) \
   (((bgl_avahi_service_type_browser_t)CREF(o))->BgL_z42builtinz42)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_CLIENT( o ) \
   (((bgl_avahi_service_type_browser_t)CREF(o))->BgL_clientz00)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_PROC( o ) \
   (((bgl_avahi_service_type_browser_t)CREF(o))->BgL_procz00)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_TYPE( o ) \
   (((bgl_avahi_service_type_browser_t)CREF(o))->BgL_typez00)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_DOMAIN( o ) \
   (((bgl_avahi_service_type_browser_t)CREF(o))->BgL_domainz00)

/*---------------------------------------------------------------------*/
/*    avahi_domain_browser                                             */
/*---------------------------------------------------------------------*/
#define bgl_avahi_domain_browser_t BgL_avahizd2domainzd2browserz00_bglt

#define BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) \
   (((bgl_avahi_domain_browser_t)CREF(o))->BgL_z42builtinz42)
#define BGL_AVAHI_DOMAIN_BROWSER_CLIENT( o ) \
   (((bgl_avahi_domain_browser_t)CREF(o))->BgL_clientz00)
#define BGL_AVAHI_DOMAIN_BROWSER_PROC( o ) \
   (((bgl_avahi_domain_browser_t)CREF(o))->BgL_procz00)
#define BGL_AVAHI_DOMAIN_BROWSER_DOMAIN( o ) \
   (((bgl_avahi_domain_browser_t)CREF(o))->BgL_domainz00)

/*---------------------------------------------------------------------*/
/*    avahi_service_resolver                                           */
/*---------------------------------------------------------------------*/
#define bgl_avahi_service_resolver_t BgL_avahizd2servicezd2resolverz00_bglt

#define BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_z42builtinz42)
#define BGL_AVAHI_SERVICE_RESOLVER_CLIENT( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_clientz00)
#define BGL_AVAHI_SERVICE_RESOLVER_PROC( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_procz00)
#define BGL_AVAHI_SERVICE_RESOLVER_TYPE( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_typez00)
#define BGL_AVAHI_SERVICE_RESOLVER_NAME( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_namez00)
#define BGL_AVAHI_SERVICE_RESOLVER_DOMAIN( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_domainz00)
#define BGL_AVAHI_SERVICE_RESOLVER_INTERFACE( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_interfacez00)
#define BGL_AVAHI_SERVICE_RESOLVER_PROTOCOL( o ) \
   (((bgl_avahi_service_resolver_t)CREF(o))->BgL_protocolz00)

/*---------------------------------------------------------------------*/
/*    static bool_t                                                    */
/*    bgl_avahi_threaded_pollp ...                                     */
/*---------------------------------------------------------------------*/
static bool_t
bgl_avahi_threaded_pollp( obj_t o ) {
   return BGL_AVAHI_POLL_CTYPE( (bgl_avahi_poll_t)o ) == 2;
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
struct callback_conv {
   void *value;
   obj_t (*convert)( void * );
};
   
typedef struct callback {
   obj_t proc;
   int arity;
   struct callback_conv args[ 1 ];
} *callback_t;

/*---------------------------------------------------------------------*/
/*    CHECK_PROCEDURE                                                  */
/*---------------------------------------------------------------------*/
#define CHECK_PROCEDURE( proc, arity, name ) \
   if( !PROCEDURE_CORRECT_ARITYP( proc, arity ) ) { \
      char buf[ 80 ]; \
      sprintf( buf, "Wrong number of arguments for %s callback (%d expected)", name, arity ); \
      C_SYSTEM_FAILURE( BGL_ERROR, "avahi", buf, proc ); \
   }

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    bgl_avahi_int ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
bgl_avahi_int( void *x ) {
   return BINT( (long)x );
}

/*---------------------------------------------------------------------*/
/*    bgl_avahi_identity                                               */
/*---------------------------------------------------------------------*/
static obj_t
bgl_avahi_identity( void *x ) {
   return x;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_avahi_string_to_bstring ...                                  */
/*---------------------------------------------------------------------*/
obj_t
bgl_avahi_string_to_bstring( char *s ) {
   if( s ) {
      obj_t bs = string_to_bstring( s );

      free( s );
      return bs;
   } else {
      return string_to_bstring( "" );
   }
}

/*---------------------------------------------------------------------*/
/*    STRDUP ...                                                       */
/*---------------------------------------------------------------------*/
#define STRDUP( v ) ((v == 0) ? v : strdup( v ))

/*---------------------------------------------------------------------*/
/*    callback_t                                                       */
/*    callbacks ...                                                    */
/*---------------------------------------------------------------------*/
#define INITIAL_MAX_CALLBACK 16

static callback_t *callbacks;
static int callback_length = 0;
static int callback_index = 0;

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    enlarge_callback_array ...                                       */
/*---------------------------------------------------------------------*/
static void
enlarge_callback_array() {
   callback_t *ncallbacks;
   int osize = callback_length * sizeof( callback_t );

   callback_length *= 2;
   ncallbacks = malloc( osize * 2 );
   memcpy( ncallbacks, callbacks, osize );

   free( callbacks );
   callbacks = ncallbacks;
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_apply_callback ...                                     */
/*---------------------------------------------------------------------*/
void
bgl_avahi_apply_callback( callback_t cb ) {
   obj_t proc = cb->proc;
	 
   switch( cb->arity ) {
      case 0:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      BEOA );
	 break;
	    
      case 1:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      BEOA );
	 break;
	    
      case 2:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      cb->args[ 1 ].convert( cb->args[ 1 ].value ),
	      BEOA );
	 break;
	    
      case 6:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      cb->args[ 1 ].convert( cb->args[ 1 ].value ),
	      cb->args[ 2 ].convert( cb->args[ 2 ].value ),
	      cb->args[ 3 ].convert( cb->args[ 3 ].value ),
	      cb->args[ 4 ].convert( cb->args[ 4 ].value ),
	      cb->args[ 5 ].convert( cb->args[ 5 ].value ),
	      BEOA );
	 break;
	    
      case 7:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      cb->args[ 1 ].convert( cb->args[ 1 ].value ),
	      cb->args[ 2 ].convert( cb->args[ 2 ].value ),
	      cb->args[ 3 ].convert( cb->args[ 3 ].value ),
	      cb->args[ 4 ].convert( cb->args[ 4 ].value ),
	      cb->args[ 5 ].convert( cb->args[ 5 ].value ),
	      cb->args[ 6 ].convert( cb->args[ 6 ].value ),
	      BEOA );
	 break;
	    
      case 8:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      cb->args[ 1 ].convert( cb->args[ 1 ].value ),
	      cb->args[ 2 ].convert( cb->args[ 2 ].value ),
	      cb->args[ 3 ].convert( cb->args[ 3 ].value ),
	      cb->args[ 4 ].convert( cb->args[ 4 ].value ),
	      cb->args[ 5 ].convert( cb->args[ 5 ].value ),
	      cb->args[ 6 ].convert( cb->args[ 6 ].value ),
	      cb->args[ 7 ].convert( cb->args[ 7 ].value ),
	      BEOA );
	 break;
	    
      case 11:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      cb->args[ 1 ].convert( cb->args[ 1 ].value ),
	      cb->args[ 2 ].convert( cb->args[ 2 ].value ),
	      cb->args[ 3 ].convert( cb->args[ 3 ].value ),
	      cb->args[ 4 ].convert( cb->args[ 4 ].value ),
	      cb->args[ 5 ].convert( cb->args[ 5 ].value ),
	      cb->args[ 6 ].convert( cb->args[ 6 ].value ),
	      cb->args[ 7 ].convert( cb->args[ 7 ].value ),
	      cb->args[ 8 ].convert( cb->args[ 8 ].value ),
	      cb->args[ 9 ].convert( cb->args[ 9 ].value ),
	      cb->args[ 10 ].convert(cb->args[ 10 ].value ),
	      BEOA );
	 break;
	    
      case 12:
	 PROCEDURE_ENTRY( proc )
	    ( proc,
	      cb->args[ 0 ].convert( cb->args[ 0 ].value ),
	      cb->args[ 1 ].convert( cb->args[ 1 ].value ),
	      cb->args[ 2 ].convert( cb->args[ 2 ].value ),
	      cb->args[ 3 ].convert( cb->args[ 3 ].value ),
	      cb->args[ 4 ].convert( cb->args[ 4 ].value ),
	      cb->args[ 5 ].convert( cb->args[ 5 ].value ),
	      cb->args[ 6 ].convert( cb->args[ 6 ].value ),
	      cb->args[ 7 ].convert( cb->args[ 7 ].value ),
	      cb->args[ 8 ].convert( cb->args[ 8 ].value ),
	      cb->args[ 9 ].convert( cb->args[ 9 ].value ),
	      cb->args[ 10 ].convert(cb->args[ 10 ].value ),
	      cb->args[ 11 ].convert( cb->args[ 11 ].value ),
	      BEOA );
	 break;

      default:
	 bgl_avahi_error( "avahi-callback",
			  "illegal callback",
			  (obj_t)proc,
			  AVAHI_ERR_FAILURE );
	 break;
   }
}   

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_invoke_callbacks ...                                   */
/*---------------------------------------------------------------------*/
void
bgl_avahi_invoke_callbacks() {
   callback_t *tmpcb;
   int index = callback_index;
   int size = index * sizeof( callback_t );
   
   bgl_avahi_lock();

   tmpcb = alloca( size );
   memcpy( tmpcb, callbacks, size );

   callback_index = 0;
   
   bgl_avahi_unlock();
   
   while( index > 0 ) {
      bgl_avahi_apply_callback( tmpcb[ --index ] );
      free( tmpcb[ index ] );
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_avahi_register_async_callback ...                            */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_register_async_callback( callback_t cb ) {
   /* signal the callback */
   bgl_avahi_lock();
   
   if( callback_index == callback_length ) {
      if( callback_length == 0 ) {
	 callback_length = INITIAL_MAX_CALLBACK;
	 callbacks = malloc( sizeof( callback_t ) * callback_length );
      } else {
	 enlarge_callback_array();
      }
   }

   callbacks[ callback_index++ ] = cb;

   bgl_avahi_signal();
   bgl_avahi_unlock();
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_avahi_call_or_register_callback ...                          */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_call_or_register_callback( bgl_avahi_client_t o, callback_t cb ) {
   if( bgl_avahi_threaded_pollp( (obj_t)BGL_AVAHI_CLIENT_POLL( o ) ) ){
      /* multi-threaded */
      bgl_avahi_register_async_callback( cb );
   } else {
      /* single-threaded */
      bgl_avahi_apply_callback( cb );
      free( cb );
   }
}
   
/*---------------------------------------------------------------------*/
/*    static callback_t                                                */
/*    make_callback ...                                                */
/*---------------------------------------------------------------------*/
static callback_t
make_callback( obj_t proc, int arity, char *name ) {
   callback_t cb =
      malloc( sizeof( struct callback ) +
	      ((arity - 1) * sizeof( struct callback_conv )) );

   CHECK_PROCEDURE( proc, arity, name );

   cb->proc = proc;
   cb->arity = arity;

   return cb;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_avahi_string_list_to_list ...                                */
/*---------------------------------------------------------------------*/
static obj_t
bgl_avahi_string_list_to_list( AvahiStringList *list ) {
   obj_t hd = MAKE_PAIR( BNIL, BNIL ), tl = hd;
   AvahiStringList *l = list;
   
   while( l ) {
      obj_t s = string_to_bstring( avahi_string_list_get_text( l ) );
      SET_CDR( tl, MAKE_PAIR( s, BNIL ) );
      tl = CDR( tl );
      l = avahi_string_list_get_next( l );
   }

   avahi_string_list_free( list );
   
   return CDR( hd );
}

/*---------------------------------------------------------------------*/
/*    AvahiStringList *                                                */
/*    bgl_avahi_list_to_string_list ...                                */
/*---------------------------------------------------------------------*/
AvahiStringList *
bgl_avahi_list_to_string_list( obj_t p ) {
   // MS: 28 feb 2017
   // AvahiStringList *l = avahi_string_list_new( "", NULL );
   AvahiStringList *l = NULL;

   while( PAIRP( p ) ) {
      l = avahi_string_list_add( l, BSTRING_TO_STRING( CAR( p ) ) );
      p = CDR( p );
   }

   return l;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_simple_poll_new ...                                    */
/*---------------------------------------------------------------------*/
void
bgl_avahi_simple_poll_new( bgl_avahi_simple_poll_t o ) {
   AvahiSimplePoll *simple_poll = avahi_simple_poll_new();

   if( !simple_poll ) {
      bgl_avahi_error( "avahi-simple-poll",
		       "Cannot create simple poll object",
		       (obj_t)o,
		       AVAHI_ERR_FAILURE );
   } else {
      BGL_AVAHI_SIMPLE_POLL_BUILTIN( o ) = simple_poll;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    simple_poll_timeout_callback ...                                 */
/*---------------------------------------------------------------------*/
static void
simple_poll_timeout_callback( AvahiTimeout *e, void *udata ) {
   callback_t cb = make_callback( (obj_t)udata, 0, "timeout" );
   bgl_avahi_apply_callback( cb );
   free( cb );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_simple_poll_timeout ...                                */
/*---------------------------------------------------------------------*/
void
bgl_avahi_simple_poll_timeout( AvahiSimplePoll *o, long t, obj_t proc ) {
   struct timeval tv;
   const AvahiPoll *poll = avahi_simple_poll_get( o );

   poll->timeout_new( poll,
		      avahi_elapse_time( &tv, t, 0 ),
		      simple_poll_timeout_callback,
		      proc );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_simple_poll_close ...                                  */
/*---------------------------------------------------------------------*/
void
bgl_avahi_simple_poll_close( bgl_avahi_simple_poll_t o ) {
   if( BGL_AVAHI_SIMPLE_POLL_BUILTIN( o ) ) {
      avahi_simple_poll_free( BGL_AVAHI_SIMPLE_POLL_BUILTIN( o ) );
      BGL_AVAHI_SIMPLE_POLL_BUILTIN( o ) = 0L;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_threaded_poll_new ...                                  */
/*---------------------------------------------------------------------*/
void
bgl_avahi_threaded_poll_new( bgl_avahi_threaded_poll_t o ) {
   AvahiThreadedPoll *threaded_poll = avahi_threaded_poll_new();

   if( !threaded_poll ) {
      bgl_avahi_error( "avahi-threaded-poll",
		       "Cannot create threaded poll object",
		       (obj_t)o,
		       AVAHI_ERR_FAILURE );
   } else {
      BGL_AVAHI_THREADED_POLL_BUILTIN( o ) = threaded_poll;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    threaded_poll_timeout_callback ...                               */
/*---------------------------------------------------------------------*/
static void
threaded_poll_timeout_callback( AvahiTimeout *e, void *udata ) {
   callback_t cb = make_callback( (obj_t)udata, 0, "timeout" );
   
   bgl_avahi_register_async_callback( cb );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_threaded_poll_timeout ...                              */
/*---------------------------------------------------------------------*/
void
bgl_avahi_threaded_poll_timeout( AvahiThreadedPoll *o, long t, obj_t proc ) {
   struct timeval tv;
   const AvahiPoll *poll = avahi_threaded_poll_get( o );

   poll->timeout_new( poll,
		      avahi_elapse_time( &tv, t, 0 ),
		      threaded_poll_timeout_callback,
		      proc );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_threaded_poll_close ...                                */
/*---------------------------------------------------------------------*/
void
bgl_avahi_threaded_poll_close( bgl_avahi_threaded_poll_t o ) {
   if( BGL_AVAHI_THREADED_POLL_BUILTIN( o ) ) {
      avahi_threaded_poll_free( BGL_AVAHI_THREADED_POLL_BUILTIN( o ) );
      BGL_AVAHI_THREADED_POLL_BUILTIN( o ) = 0L;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_client_callback ...                                    */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_client_callback( AvahiClient *client,
			   AvahiClientState state,
			   void *udata ) {
   obj_t o = (obj_t)udata;
   callback_t cb = make_callback( BGL_AVAHI_CLIENT_PROC( o ), 2, "client" );

   if( !BGL_AVAHI_CLIENT_BUILTIN( o ) )
      BGL_AVAHI_CLIENT_BUILTIN( o ) = client;

   cb->args[ 0 ].convert = &bgl_avahi_identity;
   cb->args[ 0 ].value = o;
   
   cb->args[ 1 ].convert = (obj_t (*)(void*))&bgl_avahi_client_state_to_symbol;
   cb->args[ 1 ].value = (void *)state;

   bgl_avahi_call_or_register_callback( (bgl_avahi_client_t)o, cb );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_client_new ...                                         */
/*---------------------------------------------------------------------*/
void
bgl_avahi_client_new( bgl_avahi_client_t o ) {
   int error;
   bgl_avahi_poll_t bpoll = BGL_AVAHI_CLIENT_POLL( o );
   AvahiClient *client;
   const AvahiPoll *poll;
   
   if( bgl_avahi_threaded_pollp( (obj_t)bpoll ) ) {
      poll = avahi_threaded_poll_get( BGL_AVAHI_THREADED_POLL_BUILTIN( bpoll ) );
   } else {
      poll = avahi_simple_poll_get( BGL_AVAHI_SIMPLE_POLL_BUILTIN( bpoll ) );
   }

   client = avahi_client_new( poll, AVAHI_CLIENT_NO_FAIL,
			      bgl_avahi_client_callback, o, &error );

   if( !client ) {
      bgl_avahi_error( "avahi-client-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
   } else {
      BGL_AVAHI_CLIENT_BUILTIN( o ) = client;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_client_close ...                                       */
/*---------------------------------------------------------------------*/
void
bgl_avahi_client_close( bgl_avahi_client_t o ) {
   if( BGL_AVAHI_CLIENT_BUILTIN( o ) ) {
      avahi_client_free( BGL_AVAHI_CLIENT_BUILTIN( o ) );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_entry_group_callback ...                               */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_entry_group_callback( AvahiEntryGroup *group,
				AvahiEntryGroupState state,
				void *udata ) {
   obj_t o = (obj_t)udata;
   callback_t cb = make_callback( BGL_AVAHI_ENTRY_GROUP_PROC( o ), 2, "group" );

   cb->args[ 0 ].convert = &bgl_avahi_identity;
   cb->args[ 0 ].value = o;
   
   cb->args[ 1 ].convert = (obj_t (*)(void*))&bgl_avahi_entry_group_state_to_symbol;
   cb->args[ 1 ].value = (void *)state;

   bgl_avahi_call_or_register_callback(
      BGL_AVAHI_ENTRY_GROUP_CLIENT( o ), cb );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_entry_group_new ...                                    */
/*---------------------------------------------------------------------*/
void
bgl_avahi_entry_group_new( bgl_avahi_entry_group_t o ) {
   int error;
   AvahiClient *client =
      BGL_AVAHI_CLIENT_BUILTIN( BGL_AVAHI_ENTRY_GROUP_CLIENT( o ) );
   AvahiEntryGroup *group =
      avahi_entry_group_new(
	 client,
	 bgl_avahi_entry_group_callback,
	 o );

   if( !group ) {
      error = avahi_client_errno( client );
      bgl_avahi_error( "avahi-entry-group-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
   } else {
      BGL_AVAHI_ENTRY_GROUP_BUILTIN( o ) = group;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_entry_group_close ...                                  */
/*---------------------------------------------------------------------*/
void
bgl_avahi_entry_group_close( bgl_avahi_entry_group_t o ) {
   if( BGL_AVAHI_ENTRY_GROUP_BUILTIN( o ) ) {
      avahi_entry_group_free( BGL_AVAHI_ENTRY_GROUP_BUILTIN( o ) );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_browser_callback ...                           */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_service_browser_callback( AvahiServiceBrowser *browser,
				    AvahiIfIndex interface,
				    AvahiProtocol protocol,
				    AvahiBrowserEvent event,
				    const char *name,
				    const char *type,
				    const char *domain,
				    AvahiLookupResultFlags flags,
				    void *udata ) {
   obj_t o = (obj_t)udata;
   callback_t cb = make_callback( BGL_AVAHI_SERVICE_BROWSER_PROC( o ), 8, "service-browser" );

   if( !BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) )
      BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) = browser;

   cb->args[ 0 ].convert = &bgl_avahi_identity;
   cb->args[ 0 ].value = o;
   
   cb->args[ 1 ].convert = &bgl_avahi_int;
   cb->args[ 1 ].value = (void *)interface;

   cb->args[ 2 ].convert = (obj_t (*)(void*))&bgl_avahi_protocol_to_symbol;
   cb->args[ 2 ].value = (void *)protocol;

   cb->args[ 3 ].convert = (obj_t (*)(void*))&bgl_avahi_browser_event_to_symbol;
   cb->args[ 3 ].value = (void *)event;

   cb->args[ 4 ].convert = (obj_t (*)(void*))bgl_avahi_string_to_bstring;
   cb->args[ 4 ].value = (void *)STRDUP( name );

   cb->args[ 5 ].convert = (obj_t (*)(void*))bgl_avahi_string_to_bstring;
   cb->args[ 5 ].value = (void *)STRDUP( type );

   cb->args[ 6 ].convert = (obj_t (*)(void*))bgl_avahi_string_to_bstring;
   cb->args[ 6 ].value = (void *)STRDUP( domain );

   cb->args[ 7 ].convert = &bgl_avahi_int;
   cb->args[ 7 ].value = (void *)flags;

   bgl_avahi_call_or_register_callback(
       BGL_AVAHI_SERVICE_BROWSER_CLIENT( o ), cb );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_browser_new ...                                */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_browser_new( bgl_avahi_service_browser_t o ) {
   int error;
   AvahiClient *client =
      BGL_AVAHI_CLIENT_BUILTIN( BGL_AVAHI_SERVICE_BROWSER_CLIENT( o ) );
   AvahiServiceBrowser *browser =
      avahi_service_browser_new(
	 client,
	 AVAHI_IF_UNSPEC,
	 AVAHI_PROTO_UNSPEC,
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_BROWSER_TYPE( o ) ),
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_BROWSER_DOMAIN( o ) ),
	 0,
	 bgl_avahi_service_browser_callback,
	 o );

   if( !browser ) {
      error = avahi_client_errno( client );
      bgl_avahi_error( "avahi-service-browser-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
   } else {
      BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) = browser;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_browser_close ...                              */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_browser_close( bgl_avahi_service_browser_t o ) {
   if( BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) ) {
      avahi_service_browser_free( BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_type_browser_callback ...                      */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_service_type_browser_callback( AvahiServiceTypeBrowser *browser,
					 AvahiIfIndex interface,
					 AvahiProtocol protocol,
					 AvahiBrowserEvent event,
					 const char *type,
					 const char *domain,
					 AvahiLookupResultFlags flags,
					 void *udata ) {
   obj_t o = (obj_t)udata;
   callback_t cb = make_callback( BGL_AVAHI_SERVICE_TYPE_BROWSER_PROC( o ), 7, "type-browser" );

   if( !BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) )
      BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) = browser;

   cb->args[ 0 ].convert = &bgl_avahi_identity;
   cb->args[ 0 ].value = o;
   
   cb->args[ 1 ].convert = &bgl_avahi_int;
   cb->args[ 1 ].value = (void *)interface;

   cb->args[ 2 ].convert = (obj_t (*)(void*))&bgl_avahi_protocol_to_symbol;
   cb->args[ 2 ].value = (void *)protocol;

   cb->args[ 3 ].convert = (obj_t (*)(void*))&bgl_avahi_browser_event_to_symbol;
   cb->args[ 3 ].value = (void *)event;

   cb->args[ 4 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 4 ].value = (void *)STRDUP( type );

   cb->args[ 5 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 5 ].value = (void *)STRDUP( domain );

   cb->args[ 6 ].convert = &bgl_avahi_int;
   cb->args[ 6 ].value = (void *)flags;

   bgl_avahi_call_or_register_callback(
      BGL_AVAHI_SERVICE_TYPE_BROWSER_CLIENT( o ), cb );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_type_browser_new ...                           */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_type_browser_new( bgl_avahi_service_type_browser_t o ) {
   int error;
   bgl_avahi_client_t bclient = BGL_AVAHI_SERVICE_TYPE_BROWSER_CLIENT( o );
   AvahiClient *client = BGL_AVAHI_CLIENT_BUILTIN( bclient );
   AvahiServiceTypeBrowser *browser =
      avahi_service_type_browser_new(
	 client,
	 AVAHI_IF_UNSPEC,
	 AVAHI_PROTO_UNSPEC,
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_TYPE_BROWSER_DOMAIN( o ) ),
	 0,
	 bgl_avahi_service_type_browser_callback,
	 o );

   if( !browser ) {
      error = avahi_client_errno( client );
      bgl_avahi_error( "avahi-service-type-browser-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       errno );
   } else {
      BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) = browser;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_type_browser_close ...                         */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_type_browser_close( bgl_avahi_service_type_browser_t o ) {
   if( BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) ) {
      avahi_service_type_browser_free( BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_domain_browser_callback ...                            */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_domain_browser_callback( AvahiDomainBrowser *browser,
				   AvahiIfIndex interface,
				   AvahiProtocol protocol,
				   AvahiBrowserEvent event,
				   const char *domain,
				   AvahiLookupResultFlags flags,
				   void *udata ) {
   obj_t o = (obj_t)udata;
   callback_t cb = make_callback( BGL_AVAHI_DOMAIN_BROWSER_PROC( o ), 5, "domain-browser" );

   if( !BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) )
      BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) = browser;
   
   cb->args[ 0 ].convert = &bgl_avahi_identity;
   cb->args[ 0 ].value = o;
   
   cb->args[ 1 ].convert = &bgl_avahi_int;
   cb->args[ 1 ].value = (void *)interface;

   cb->args[ 2 ].convert = (obj_t (*)(void*))&bgl_avahi_protocol_to_symbol;
   cb->args[ 2 ].value = (void *)protocol;

   cb->args[ 3 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 3 ].value = (void *)STRDUP( domain );

   cb->args[ 4 ].convert = &bgl_avahi_int;
   cb->args[ 4 ].value = (void *)flags;

   bgl_avahi_call_or_register_callback(
      BGL_AVAHI_DOMAIN_BROWSER_CLIENT( o ), cb );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_domain_browser_new ...                                 */
/*---------------------------------------------------------------------*/
void
bgl_avahi_domain_browser_new( bgl_avahi_domain_browser_t o,
			      AvahiDomainBrowserType btype ) {
   int error;
   AvahiClient *client =
      BGL_AVAHI_CLIENT_BUILTIN( BGL_AVAHI_DOMAIN_BROWSER_CLIENT( o ) );
   AvahiDomainBrowser *browser =
      avahi_domain_browser_new(
	 client,
	 AVAHI_IF_UNSPEC,
	 AVAHI_PROTO_UNSPEC,
	 BGL_STRING_TO_STRING( BGL_AVAHI_DOMAIN_BROWSER_DOMAIN( o ) ),
	 btype,
	 0,
	 bgl_avahi_domain_browser_callback,
	 o );

   if( !browser ) {
      error = avahi_client_errno( client );
      bgl_avahi_error( "avahi-domain-browser-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
   } else {
      BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) = browser;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_domain_browser_close ...                               */
/*---------------------------------------------------------------------*/
void
bgl_avahi_domain_browser_close( bgl_avahi_domain_browser_t o ) {
   if( BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) ) {
      avahi_domain_browser_free( BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) );
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_resolver_callback ...                          */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_service_resolver_callback( AvahiServiceResolver *resolver,
				     AvahiIfIndex interface,
				     AvahiProtocol protocol,
				     AvahiResolverEvent event,
				     const char *name,
				     const char *type,
				     const char *domain,
				     const char *hostname,
				     const AvahiAddress *address,
				     uint16_t port,
				     AvahiStringList *txt,
				     AvahiLookupResultFlags flags,
				     void *udata ) {
   obj_t o = (obj_t)udata;
   callback_t cb = make_callback( BGL_AVAHI_SERVICE_RESOLVER_PROC( o ), 12, "service-resolver" );
   char a[ AVAHI_ADDRESS_STR_MAX ];

   if( address ) {
      avahi_address_snprint( a, sizeof( a ), address );
   } else {
      a[ 0 ] = 0;
   }
   if( !BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) )
      BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) = resolver;

   cb->args[ 0 ].convert = &bgl_avahi_identity;
   cb->args[ 0 ].value = o;
   
   cb->args[ 1 ].convert = &bgl_avahi_int;
   cb->args[ 1 ].value = (void *)interface;

   cb->args[ 2 ].convert = (obj_t (*)(void*))&bgl_avahi_protocol_to_symbol;
   cb->args[ 2 ].value = address ? (void *)address->proto : AVAHI_PROTO_UNSPEC;

   cb->args[ 3 ].convert = (obj_t (*)(void*))bgl_avahi_resolver_event_to_symbol;
   cb->args[ 3 ].value = (void *)event;

   cb->args[ 4 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 4 ].value = (void *)STRDUP( name );

   cb->args[ 5 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 5 ].value = (void *)STRDUP( type );

   cb->args[ 6 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 6 ].value = (void *)STRDUP( domain );

   cb->args[ 7 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 7 ].value = (void *)STRDUP( hostname );

   cb->args[ 8 ].convert = (obj_t (*)(void*))&bgl_avahi_string_to_bstring;
   cb->args[ 8 ].value = (void *)STRDUP( a );

   cb->args[ 9 ].convert = &bgl_avahi_int;
   cb->args[ 9 ].value = (void *)((long)port);

   cb->args[ 10 ].convert = (obj_t (*)(void*))&bgl_avahi_string_list_to_list;
   cb->args[ 10 ].value = (void *)avahi_string_list_copy( txt );

   cb->args[ 11 ].convert = &bgl_avahi_int;
   cb->args[ 11 ].value = (void *)flags;

   bgl_avahi_call_or_register_callback(
      BGL_AVAHI_SERVICE_RESOLVER_CLIENT( o ), cb );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_resolver_new ...                               */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_resolver_new( bgl_avahi_service_resolver_t o ) {
   int error;
   AvahiClient *client =
      BGL_AVAHI_CLIENT_BUILTIN( BGL_AVAHI_SERVICE_RESOLVER_CLIENT( o ) );
   AvahiServiceResolver *resolver =
      avahi_service_resolver_new(
	 client,
	 BGL_AVAHI_SERVICE_RESOLVER_INTERFACE( o ),
	 bgl_avahi_symbol_to_protocol( BGL_AVAHI_SERVICE_RESOLVER_PROTOCOL( o ) ),
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_RESOLVER_NAME( o ) ),
	 (const char*)BSTRING_TO_STRING( BGL_AVAHI_SERVICE_RESOLVER_TYPE( o ) ),
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_RESOLVER_DOMAIN( o ) ),
	 bgl_avahi_symbol_to_protocol( BGL_AVAHI_SERVICE_RESOLVER_PROTOCOL( o ) ),
	 0,
	 bgl_avahi_service_resolver_callback,
	 o );

   if( !resolver ) {
      error = avahi_client_errno( client );
      bgl_avahi_error( "avahi-service-resolver-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
   } else {
      BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) = resolver;
   }
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_resolver_close ...                             */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_resolver_close( bgl_avahi_service_resolver_t o ) {
   if( BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) ) {
      avahi_service_resolver_free( BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) );
   }
}
