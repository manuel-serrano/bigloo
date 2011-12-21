/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/avahi/src/Clib/bglavahi.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 20 14:50:56 2011                          */
/*    Last change :  Tue Dec 20 12:26:35 2011 (serrano)                */
/*    Copyright   :  2011 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    avahi Bigloo binding                                             */
/*=====================================================================*/
#include <bigloo.h>

#include <avahi-client/client.h>
#include <avahi-client/lookup.h>
#include <avahi-client/publish.h>

#include <avahi-common/simple-watch.h>
#include <avahi-common/error.h>
#include <avahi-common/timeval.h>

#include "bglavahi.h"

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern int bgl_avahi_error( char *, char *, obj_t, int );

/*---------------------------------------------------------------------*/
/*    Strings                                                          */
/*---------------------------------------------------------------------*/
#define BGL_STRING_TO_STRING( o ) \
   (STRING_LENGTH( o ) == 0 ? 0L : (const char *)(BSTRING_TO_STRING( o )))

/*---------------------------------------------------------------------*/
/*    avahi_simple_poll bigloo object                                  */
/*---------------------------------------------------------------------*/
#define bgl_avahi_simple_poll_t BgL_avahizd2simplezd2pollz00_bglt
#define BGL_AVAHI_SIMPLE_POLL_BUILTIN( o ) \
   (((bgl_avahi_simple_poll_t)o)->BgL_z42builtinz42)

/*---------------------------------------------------------------------*/
/*    avahi_client                                                     */
/*---------------------------------------------------------------------*/
#define bgl_avahi_client_t BgL_avahizd2clientzd2_bglt

#define BGL_AVAHI_CLIENT_BUILTIN( o ) \
   (((bgl_avahi_client_t)o)->BgL_z42builtinz42)
#define BGL_AVAHI_CLIENT_PROC( o ) \
   (((bgl_avahi_client_t)o)->BgL_procz00)
#define BGL_AVAHI_CLIENT_POLL( o ) \
   (((bgl_avahi_client_t)o)->BgL_pollz00)

/*---------------------------------------------------------------------*/
/*    avahi_entry_group                                                */
/*---------------------------------------------------------------------*/
#define bgl_avahi_entry_group_t BgL_avahizd2entryzd2groupz00_bglt

#define BGL_AVAHI_ENTRY_GROUP_BUILTIN( o ) \
   (((bgl_avahi_entry_group_t)o)->BgL_z42builtinz42)
#define BGL_AVAHI_ENTRY_GROUP_CLIENT( o ) \
   (((bgl_avahi_entry_group_t)o)->BgL_clientz00)
#define BGL_AVAHI_ENTRY_GROUP_PROC( o ) \
   (((bgl_avahi_entry_group_t)o)->BgL_procz00)

/*---------------------------------------------------------------------*/
/*    avahi_service_browser                                            */
/*---------------------------------------------------------------------*/
#define bgl_avahi_service_browser_t BgL_avahizd2servicezd2browserz00_bglt

#define BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) \
   (((bgl_avahi_service_browser_t)o)->BgL_z42builtinz42)
#define BGL_AVAHI_SERVICE_BROWSER_CLIENT( o ) \
   (((bgl_avahi_service_browser_t)o)->BgL_clientz00)
#define BGL_AVAHI_SERVICE_BROWSER_PROC( o ) \
   (((bgl_avahi_service_browser_t)o)->BgL_procz00)
#define BGL_AVAHI_SERVICE_BROWSER_TYPE( o ) \
   (((bgl_avahi_service_browser_t)o)->BgL_typez00)
#define BGL_AVAHI_SERVICE_BROWSER_DOMAIN( o ) \
   (((bgl_avahi_service_browser_t)o)->BgL_domainz00)

/*---------------------------------------------------------------------*/
/*    avahi_service_type_browser                                       */
/*---------------------------------------------------------------------*/
#define bgl_avahi_service_type_browser_t BgL_avahizd2servicezd2typezd2browserzd2_bglt

#define BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) \
   (((bgl_avahi_service_type_browser_t)o)->BgL_z42builtinz42)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_CLIENT( o ) \
   (((bgl_avahi_service_type_browser_t)o)->BgL_clientz00)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_PROC( o ) \
   (((bgl_avahi_service_type_browser_t)o)->BgL_procz00)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_TYPE( o ) \
   (((bgl_avahi_service_type_browser_t)o)->BgL_typez00)
#define BGL_AVAHI_SERVICE_TYPE_BROWSER_DOMAIN( o ) \
   (((bgl_avahi_service_type_browser_t)o)->BgL_domainz00)

/*---------------------------------------------------------------------*/
/*    avahi_domain_browser                                             */
/*---------------------------------------------------------------------*/
#define bgl_avahi_domain_browser_t BgL_avahizd2domainzd2browserz00_bglt

#define BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) \
   (((bgl_avahi_domain_browser_t)o)->BgL_z42builtinz42)
#define BGL_AVAHI_DOMAIN_BROWSER_CLIENT( o ) \
   (((bgl_avahi_domain_browser_t)o)->BgL_clientz00)
#define BGL_AVAHI_DOMAIN_BROWSER_PROC( o ) \
   (((bgl_avahi_domain_browser_t)o)->BgL_procz00)
#define BGL_AVAHI_DOMAIN_BROWSER_DOMAIN( o ) \
   (((bgl_avahi_domain_browser_t)o)->BgL_domainz00)

/*---------------------------------------------------------------------*/
/*    avahi_service_resolver                                           */
/*---------------------------------------------------------------------*/
#define bgl_avahi_service_resolver_t BgL_avahizd2servicezd2resolverz00_bglt

#define BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_z42builtinz42)
#define BGL_AVAHI_SERVICE_RESOLVER_CLIENT( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_clientz00)
#define BGL_AVAHI_SERVICE_RESOLVER_PROC( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_procz00)
#define BGL_AVAHI_SERVICE_RESOLVER_TYPE( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_typez00)
#define BGL_AVAHI_SERVICE_RESOLVER_NAME( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_namez00)
#define BGL_AVAHI_SERVICE_RESOLVER_DOMAIN( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_domainz00)
#define BGL_AVAHI_SERVICE_RESOLVER_INTERFACE( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_interfacez00)
#define BGL_AVAHI_SERVICE_RESOLVER_PROTOCOL( o ) \
   (((bgl_avahi_service_resolver_t)o)->BgL_protocolz00)

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_avahi_string_list_to_list ...                                */
/*---------------------------------------------------------------------*/
static obj_t
bgl_avahi_string_list_to_list( AvahiStringList *l ) {
   obj_t hd = MAKE_PAIR( BNIL, BNIL ), tl = hd;
   
   while( l ) {
      obj_t s = string_to_bstring( avahi_string_list_get_text( l ) );
      SET_CDR( tl, MAKE_PAIR( s, BNIL ) );
      tl = CDR( tl );
      l = avahi_string_list_get_next( l );
   }

   return CDR( hd );
}

/*---------------------------------------------------------------------*/
/*    AvahiStringList *                                                */
/*    bgl_avahi_list_to_string_list ...                                */
/*---------------------------------------------------------------------*/
AvahiStringList *
bgl_avahi_list_to_string_list( obj_t p ) {
   AvahiStringList *l = avahi_string_list_new( 0L );

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
/*    timeout_callback ...                                             */
/*---------------------------------------------------------------------*/
static void
timeout_callback( AvahiTimeout *e, void *udata ) {
   obj_t proc = (obj_t)udata;
   PROCEDURE_ENTRY( proc )( proc, BEOA );
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
		      timeout_callback,
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
/*    bgl_avahi_client_callback ...                                    */
/*---------------------------------------------------------------------*/
static void
bgl_avahi_client_callback( AvahiClient *client,
			   AvahiClientState state,
			   void *udata ) {
   obj_t o = (obj_t)udata;
   obj_t proc = BGL_AVAHI_CLIENT_PROC( o );
   
   if( !BGL_AVAHI_CLIENT_BUILTIN( o ) )
      BGL_AVAHI_CLIENT_BUILTIN( o ) = client;
   
   PROCEDURE_ENTRY( proc )(
      proc,
      o,
      bgl_avahi_client_state_to_symbol( state ),
      BEOA );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_client_new ...                                         */
/*---------------------------------------------------------------------*/
void
bgl_avahi_client_new( bgl_avahi_client_t o ) {
   int error;
   AvahiSimplePoll *poll =
      BGL_AVAHI_SIMPLE_POLL_BUILTIN( BGL_AVAHI_CLIENT_POLL( o ) );
   AvahiClient *client =
      avahi_client_new(
	 avahi_simple_poll_get( poll ),
	 0,
	 bgl_avahi_client_callback,
	 o,
	 &error );

   if( !client ) {
      bgl_avahi_error( "avahi-client-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
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
   obj_t proc = BGL_AVAHI_ENTRY_GROUP_PROC( o );
   PROCEDURE_ENTRY( proc )(
      proc,
      o,
      bgl_avahi_entry_group_state_to_symbol( state ),
      BEOA );
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
   obj_t proc = BGL_AVAHI_SERVICE_BROWSER_PROC( o );
   
   if( !BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) )
      BGL_AVAHI_SERVICE_BROWSER_BUILTIN( o ) = browser;
      
   PROCEDURE_ENTRY( proc )(
      proc,
      o,
      BINT( interface ),
      bgl_avahi_protocol_to_symbol( protocol ),
      bgl_avahi_browser_event_to_symbol( event ),
      string_to_bstring( (char *)name ),
      string_to_bstring( (char *)type ),
      string_to_bstring( (char *)domain ),
      BINT( flags ),
      BEOA );
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
   obj_t proc = BGL_AVAHI_SERVICE_TYPE_BROWSER_PROC( o );
   
   if( !BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) )
      BGL_AVAHI_SERVICE_TYPE_BROWSER_BUILTIN( o ) = browser;
      
   PROCEDURE_ENTRY( proc )(
      proc,
      o,
      BINT( interface ),
      bgl_avahi_protocol_to_symbol( protocol ),
      bgl_avahi_browser_event_to_symbol( event ),
      string_to_bstring( (char *)type ),
      string_to_bstring( (char *)domain ),
      BINT( flags ),
      BEOA );
}
   
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_avahi_service_type_browser_new ...                           */
/*---------------------------------------------------------------------*/
void
bgl_avahi_service_type_browser_new( bgl_avahi_service_type_browser_t o ) {
   int error;
   AvahiClient *client =
      BGL_AVAHI_CLIENT_BUILTIN( BGL_AVAHI_SERVICE_TYPE_BROWSER_CLIENT( o ) );
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
      bgl_avahi_error( "avahi-service-type-browser-new",
		       (char *)avahi_strerror( error ),
		       (obj_t)o,
		       error );
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
   obj_t proc = BGL_AVAHI_DOMAIN_BROWSER_PROC( o );

   if( !BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) )
      BGL_AVAHI_DOMAIN_BROWSER_BUILTIN( o ) = browser;
      
   PROCEDURE_ENTRY( proc )(
      proc,
      o,
      BINT( interface ),
      bgl_avahi_protocol_to_symbol( protocol ),
      string_to_bstring( (char *)domain ),
      BINT( flags ),
      BEOA );
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
   obj_t proc = BGL_AVAHI_SERVICE_RESOLVER_PROC( o );
   char a[ AVAHI_ADDRESS_STR_MAX ];

   avahi_address_snprint( a, sizeof( a ), address );
   
   if( !BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) )
      BGL_AVAHI_SERVICE_RESOLVER_BUILTIN( o ) = resolver;
      
   PROCEDURE_ENTRY( proc )(
      proc,
      o,
      BINT( interface ),
      bgl_avahi_protocol_to_symbol( protocol ),
      bgl_avahi_resolver_event_to_symbol( event ),
      string_to_bstring( (char *)name ),
      string_to_bstring( (char *)type ),
      string_to_bstring( (char *)domain ),
      string_to_bstring( (char *)hostname ),
      string_to_bstring( a ),
      BINT( port ),
      bgl_avahi_string_list_to_list( txt ),
      BINT( flags ),
      BEOA );
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
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_RESOLVER_TYPE( o ) ),
	 BGL_STRING_TO_STRING( BGL_AVAHI_SERVICE_RESOLVER_DOMAIN( o ) ),
	 AVAHI_PROTO_UNSPEC,
	 0,
	 bgl_avahi_service_resolver_callback,
	 o );

   if( !resolver ) {
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
