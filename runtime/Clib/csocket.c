/*=====================================================================*/
/*    serrano/prgm/project/bigloo/runtime/Clib/csocket.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 29 18:18:45 1998                          */
/*    Last change :  Fri Dec  3 18:18:39 2010 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Scheme sockets                                                   */
/*    -------------------------------------------------------------    */
/*    This file is based on a contribution of                          */
/*    David Tolpin (dvd@pizza.msk.su)                                  */
/*                                                                     */
/*    Bugs correction (conversion between host and network byte order) */
/*    by Marc Furrer (Marc.Furrer@di.epfl.ch)                          */
/*                                                                     */
/*    Reworked  by Erick Gallesio for 2.2 release.                     */
/*    Some additions and simplifications (I hope).                     */
/*                                                                     */
/*    DNS caching added by Manuel Serrano, 22 oct 2006.                */
/*=====================================================================*/
#if defined( _MSC_VER) || defined( _MINGW_VER )
#  define _BGL_WIN32_VER
#endif

#include <bigloo_config.h>
#include <time.h>
#ifndef _BGL_WIN32_VER
#   include <sys/types.h> 
#   include <sys/socket.h>
#   include <netinet/in.h>
#   include <arpa/inet.h>
#   include <netdb.h>
#   if( BGL_HAVE_SELECT )
#     include <sys/time.h>
#     include <unistd.h>
#   endif
#else
#   if defined( _MINGW_VER )
#      include "windows.h"
#   endif
#   include <winsock2.h>
#   include <mswsock.h>
#   include <io.h>
#endif
#include <fcntl.h>
#include <memory.h>
#include <errno.h>
#include <bigloo.h>

#ifndef _BGL_WIN32_VER
#   define BAD_SOCKET(s) ((s) < 0)
#else
#   define BAD_SOCKET(s) ((s) == INVALID_SOCKET)
#endif

#if !BGL_HAVE_SOCKLEN
typedef int socklen_t;
#endif

#if( BGL_HAVE_SOCKET_TCP_NODELAY \
     || BGL_HAVE_SOCKET_TCP_CORK \
     || BGL_HAVE_SOCKET_TCP_QUICKACK )
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <netinet/tcp.h>
#endif

#if( BGL_HAVE_UNIX_SOCKET )
#include <sys/un.h>
#endif

long opensocket;

/*---------------------------------------------------------------------*/
/*    Imports ...                                                      */
/*---------------------------------------------------------------------*/
extern obj_t bgl_close_input_port( obj_t );
extern obj_t bgl_file_to_buffered_input_port( obj_t, FILE *, obj_t );
extern long bgl_read( obj_t, char *, long );
extern obj_t make_vector();
extern unsigned char get_hash_number( char * );
extern unsigned char bgl_get_hash_number_len( char *, int, int );
extern bool_t bigloo_strcmp( obj_t o1, obj_t o2 );
extern bool_t bgl_dns_enable_cache();
extern long bgl_dns_cache_validity_timeout();

#ifndef _BGL_WIN32_VER
extern int dup( int );
extern int close( int );
#endif

#define DEBUG_CACHE_DNS 1
#undef DEBUG_CACHE_DNS
/* #undef BGL_HAVE_GETADDRINFO                                         */
/* #define BGL_HAVE_GETADDRINFO 0                                      */

/*---------------------------------------------------------------------*/
/*    bglhostent ...                                                   */
/*---------------------------------------------------------------------*/
struct bglhostent {
   header_t header;
   int state;
   struct hostent hp;
   obj_t hostaddr;
   long exptime;
};

#define BGLHOSTENT_STATE_OK      0
#define BGLHOSTENT_STATE_FAILURE 1
#define BGLHOSTENT_STATE_PENDING 2

/*---------------------------------------------------------------------*/
/*    socket mutex                                                     */
/*---------------------------------------------------------------------*/
static obj_t socket_mutex = BUNSPEC;
DEFINE_STRING( socket_mutex_name, _2, "socket-mutex", 12 );
static obj_t socket_condv = BUNSPEC;
DEFINE_STRING( socket_condv_name, _6, "socket-condv", 12 );
static struct bglhostent *socket_condv_value = 0L;
static obj_t gethostby_mutex = BUNSPEC;
DEFINE_STRING( gethostby_mutex_name, _3, "socket-gethostby", 22 );
static obj_t socket_port_mutex = BUNSPEC;
DEFINE_STRING( socket_port_mutex_name, _4, "socket-port-mutex", 17 );
static obj_t protoent_mutex = BUNSPEC;
DEFINE_STRING( protoent_mutex_name, _5, "protoent-mutex", 14 );

/*---------------------------------------------------------------------*/
/*    Global C variables                                               */
/*---------------------------------------------------------------------*/
static obj_t hosttable = BUNSPEC;
static obj_t addrtable = BUNSPEC;
static obj_t so_keepalive;
static obj_t so_oobinline;
static obj_t so_rcvbuf;
static obj_t so_sndbuf;
static obj_t so_reuseaddr;
static obj_t so_timeout;
static obj_t tcp_nodelay;
static obj_t tcp_cork;
static obj_t tcp_quickack;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_socket ...                                              */
/*    -------------------------------------------------------------    */
/*    Initialize the socket machinery (mostly host names caching).     */
/*---------------------------------------------------------------------*/
void
bgl_init_socket() {
   if( !VECTORP( hosttable ) ) {
      socket_mutex = bgl_make_mutex( socket_mutex_name );
      socket_condv = bgl_make_condvar( socket_condv_name );
      gethostby_mutex = bgl_make_mutex( gethostby_mutex_name );
      protoent_mutex = bgl_make_mutex( protoent_mutex_name );
//      socket_port_mutex = bgl_make_mutex( socket_port_mutex_name );
      
      hosttable = make_vector( 256, 0 );
      addrtable = make_vector( 256, 0 );

      so_keepalive = string_to_keyword( "SO_KEEPALIVE" );
      so_oobinline = string_to_keyword( "SO_OOBINLINE" );
      so_rcvbuf = string_to_keyword( "SO_RCVBUF" );
      so_sndbuf = string_to_keyword( "SO_SNDBUF" );
      so_reuseaddr = string_to_keyword( "SO_REUSEADDR" );
      so_timeout = string_to_keyword( "SO_TIMEOUT" );
      tcp_nodelay = string_to_keyword( "TCP_NODELAY" );
      tcp_cork = string_to_keyword( "TCP_CORK" );
      tcp_quickack = string_to_keyword( "TCP_QUICKACK" );
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    socket_error ...                                                 */
/*---------------------------------------------------------------------*/
static void
socket_error( char *who, char *message, obj_t object ) {
   C_SYSTEM_FAILURE( BGL_IO_ERROR, who, message, object );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    socket_timeout_error ...                                         */
/*---------------------------------------------------------------------*/
static void
socket_timeout_error( obj_t hostname, int port ) {
   char buffer[ 512 ];
   
   sprintf( buffer, "%s:%d", BSTRING_TO_STRING( hostname ), port );

   C_SYSTEM_FAILURE( BGL_IO_TIMEOUT_ERROR,
		     "make-client-socket",
		     "Connection time out",
		     string_to_bstring( buffer ) );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    client_socket_error ...                                          */
/*---------------------------------------------------------------------*/
static void
client_socket_error( obj_t hostname, int port, char *msg, int err ) {
   char buffer1[ 512 ];
   char buffer2[ 512 ];

   if( msg ) sprintf( buffer1, "%s, ", msg );
   sprintf( buffer1, "%s (%d)", strerror( err ), err );

   if( port >= 0 ) {
      sprintf( buffer2, "%s:%d", BSTRING_TO_STRING( hostname ), port );
   } else {
      strcpy( buffer2, BSTRING_TO_STRING( hostname ) );
   }

   socket_error( "make-client-socket", buffer1, string_to_bstring( buffer2 ) );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    system_error ...                                                 */
/*---------------------------------------------------------------------*/
static void
system_error( char *who, obj_t val ) {
   char buffer[ 512 ];

   sprintf( buffer, "%s (%d)", strerror( errno ), errno );

   socket_error( who, buffer, val );
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    make_string ...                                                  */
/*---------------------------------------------------------------------*/
static char *
make_string( char *src ) {
   char *res = (char *)GC_MALLOC_ATOMIC( strlen( src ) + 1 );

   strcpy( res, src );
   return res;
}

/*---------------------------------------------------------------------*/
/*    static char **                                                   */
/*    make_string_array ...                                            */
/*---------------------------------------------------------------------*/
static char **
make_string_array( char **src ) {
   int len = 0;
   char **run, **res;

   for( run = src; *run; run++ );
   len = (run - src);

   res = (char **)GC_MALLOC( sizeof( char * ) * len + 1 );
   for( run = src; *run; run++ ) {
      *res++ = make_string( *run );
   }

   *res = 0;

   return res - len;
}

/*---------------------------------------------------------------------*/
/*    static char **                                                   */
/*    make_inet_array ...                                              */
/*---------------------------------------------------------------------*/
static char **
make_inet_array( char **src, int size ) {
   int len = 0;
   char **run, **res;

   for( run = src; *run; run++ );
   len = (run - src);

   res = (char **)GC_MALLOC( sizeof( char * ) * len + 1 );
   for( run = src; *run; run++ ) {
      char *d = (unsigned char *)GC_MALLOC_ATOMIC( size );
      char *s = *run;
      memcpy( d, s, size );
      *res++ = d;
   }

   *res = 0;

   return res - len;
}

/*---------------------------------------------------------------------*/
/*    static struct bglhostent *                                       */
/*    make_bglhostent ...                                              */
/*---------------------------------------------------------------------*/
static struct bglhostent *
make_bglhostent( obj_t hostaddr, struct hostent *hp ) {
   struct bglhostent *bhp = (struct bglhostent *)GC_MALLOC( sizeof( struct bglhostent ) );

   bhp->header = MAKE_HEADER( OPAQUE_TYPE, 0 );
   bhp->hostaddr = hostaddr;

   if( hp ) {
      /* a sucessful hostent */
      bhp->exptime = time( 0L ) + bgl_dns_cache_validity_timeout();
      bhp->state = BGLHOSTENT_STATE_OK;
      
      memcpy( &(bhp->hp), hp, sizeof( struct hostent ) );
      bhp->hp.h_name = make_string( hp->h_name );
      bhp->hp.h_aliases = make_string_array( hp->h_aliases );
      bhp->hp.h_addr_list = make_inet_array( hp->h_addr_list, hp->h_length );
   } else {
      /* a failure hostent */
      bhp->exptime = LONG_MAX;
      bhp->state = BGLHOSTENT_STATE_PENDING;
   }

   return bhp;
}

/*---------------------------------------------------------------------*/
/*    static struct bglhostent *                                       */
/*    make_bglhostent_from_name ...                                    */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_GETADDRINFO )
static struct bglhostent *
make_bglhostent_from_name( obj_t hostaddr, struct sockaddr_in *sin, char *n ) {
   struct bglhostent *bhp = (struct bglhostent *)GC_MALLOC( sizeof( struct bglhostent ) );
   char **l = (char **)GC_MALLOC( sizeof( char * ) + 1 );
   void *d = GC_MALLOC_ATOMIC( sizeof( *sin ) );

   bhp->header = MAKE_HEADER( OPAQUE_TYPE, 0 );
   bhp->hostaddr = hostaddr;
   bhp->exptime = time( 0L ) + bgl_dns_cache_validity_timeout();

   /* h_addrtype */
   bhp->hp.h_addrtype = AF_INET;
   
   /* h_name */
   bhp->hp.h_name = make_string( n );

   /* h_length */
   bhp->hp.h_length = sizeof( *sin );

   /* addr_list */
   bhp->hp.h_addr_list = l;
   memcpy( (unsigned char *)d, sin, bhp->hp.h_length );
   *l++ = d;
   *l = 0;

   /* h_addr */
   bhp->hp.h_addr = *(bhp->hp.h_addr_list);

   return bhp;
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    make_bglhostent ...                                              */
/*---------------------------------------------------------------------*/
static void
bglhostent_fill_from_hostent( obj_t hostaddr, struct bglhostent *bhp, struct hostent *hp ) {
   bhp->header = MAKE_HEADER( OPAQUE_TYPE, 0 );
   bhp->hostaddr = hostaddr;

   if( hp ) {
      /* a sucessful hostent */
      bhp->exptime = time( 0L ) + bgl_dns_cache_validity_timeout();
      bhp->state = BGLHOSTENT_STATE_OK;
      
      memcpy( &(bhp->hp), hp, sizeof( struct hostent ) );
      bhp->hp.h_name = make_string( hp->h_name );
      bhp->hp.h_aliases = make_string_array( hp->h_aliases );
      bhp->hp.h_addr_list = make_inet_array( hp->h_addr_list, hp->h_length );
   } else {
      /* a failure hostent */
      bhp->exptime = LONG_MAX;
      bhp->state = BGLHOSTENT_STATE_PENDING;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglhostent_fill_from_addrinfo ...                                */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_GETADDRINFO )
static void
bglhostent_fill_from_addrinfo( obj_t hostaddr, struct bglhostent *bhp, struct addrinfo *ai ) {
   /* set the correct expiration timeout and state */
   bhp->exptime = time( 0L ) + bgl_dns_cache_validity_timeout();
   bhp->state = BGLHOSTENT_STATE_OK;
   
   /* h_addrtype */
   bhp->hp.h_addrtype = AF_INET;
   
   /* h_name */
   bhp->hp.h_name = make_string( BSTRING_TO_STRING( hostaddr ) );

   /* h_length */
   bhp->hp.h_length = sizeof( struct in_addr );

   /* h_aliases */
   if( ai->ai_canonname ) {
      bhp->hp.h_name = make_string( ai->ai_canonname );
      char **aliases = alloca( sizeof( char * ) * 2 );
      aliases[ 0 ] = make_string( BSTRING_TO_STRING( hostaddr ) );
      aliases[ 1 ] = 0;
      bhp->hp.h_aliases = make_string_array( aliases );
   } else {
      bhp->hp.h_name = make_string( BSTRING_TO_STRING( hostaddr ) );
      bhp->hp.h_aliases = 0;
   }

   /* addr_list */
   {
      int len = 0;
      struct addrinfo *run;
      char **l;

      for( run = ai; run; run = run->ai_next, len++ );

      l = (char **)GC_MALLOC( sizeof( char * ) * len + 1 );
      bhp->hp.h_addr_list = l;

      for( run = ai; run; run = run->ai_next ) {
	 /* CARE, MS 19 jan 2009: I'm not sure of the cast into  */
	 /* sockaddr_in and I'm not sure that the h_length       */
	 /* corresponds to the length of sin_addr.               */
	 void *d = GC_MALLOC_ATOMIC( bhp->hp.h_length );
	 memcpy( (unsigned char *)d,
		 (char *)&(((struct sockaddr_in *)(run->ai_addr))->sin_addr),
		 bhp->hp.h_length );
	 *l++ = d;
      }

      *l = 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    static struct bglhostent *                                       */
/*    make_bglhostent_from_addrinfo ...                                */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_GETADDRINFO )
static struct bglhostent *
make_bglhostent_from_addrinfo( obj_t hostaddr, struct addrinfo *ai ) {
   struct bglhostent *bhp = make_bglhostent( hostaddr, 0 );

   bglhostent_fill_from_addrinfo( hostaddr, bhp, ai );
   
   return bhp;
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglhostentbyname ...                                             */
/*    -------------------------------------------------------------    */
/*    When getaddinfo is available, it is prefered because it is       */
/*    re-entrant while gethostbyname is not.                           */
/*---------------------------------------------------------------------*/
static void
bglhostentbyname( obj_t hostname, struct bglhostent *bhp, int canon ) {
#if( !BGL_HAVE_GETADDRINFO )
   struct hostent *hp;
   struct bglhostent *res;
   
   bgl_mutex_lock( gethostby_mutex );
   hp = gethostbyname( BSTRING_TO_STRING( hostname ) );
   bglhostent_fill_from_hostent( hostname, bhp, hp );
   bgl_mutex_unlock( gethostby_mutex );
#else
   struct addrinfo hints;
   struct addrinfo *res;
   int v;
   
   /* zero out hints. */
   memset( &hints, 0, sizeof( hints ) );
   
/*    hints.ai_family = AF_INET;                                       */
   hints.ai_family = AF_UNSPEC;
   hints.ai_socktype = SOCK_STREAM;
   hints.ai_protocol = 0;
   hints.ai_flags = canon ? AI_CANONNAME | AI_ADDRCONFIG : AI_ADDRCONFIG;

   if( !( v=getaddrinfo( BSTRING_TO_STRING( hostname ), 0L, &hints, &res )) ) {

      bglhostent_fill_from_addrinfo( hostname, bhp, res );
      freeaddrinfo( res );
   } else {
      /* error message could be printed as follows: */
      /* printf( "%s\n", gai_strerror( v) );        */
      bhp->exptime = time( 0L ) + bgl_dns_cache_validity_timeout() / 4;
      bhp->state = BGLHOSTENT_STATE_FAILURE;
   }
#endif
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    invalidate_hostbyname ...                                        */
/*---------------------------------------------------------------------*/
static void
invalidate_hostbyname( obj_t hostname ) {
#if BGL_DNS_CACHE
   if( bgl_dns_enable_cache() ) {
      int key = get_hash_number( BSTRING_TO_STRING( hostname ) );
      struct bglhostent *bhp;
      
      bgl_mutex_lock( socket_mutex );
      
      bhp = (struct bglhostent *)VECTOR_REF( hosttable, key );

      if( bhp && bigloo_strcmp( bhp->hostaddr, hostname ) )
	 VECTOR_SET( hosttable, key, 0 );
      
      bgl_mutex_unlock( socket_mutex );
      
      return;
   }
#endif
}

/*---------------------------------------------------------------------*/
/*    struct hostent *                                                 */
/*    bglhostbyname ...                                                */
/*    -------------------------------------------------------------    */
/*    It function is comparable to gethostbyname except that           */
/*    it accepts a Bigloo string instead of a C string.                */
/*                                                                     */
/*    This function is thread-safe. It can be called simultaneously    */
/*    by several threads. It returns a fresh data-structure so         */
/*    client don't have to deploy a lock machinery for using it.       */
/*---------------------------------------------------------------------*/
static struct hostent *
bglhostbyname( obj_t hostname, int canon ) {
   struct bglhostent *bhp;

#if BGL_DNS_CACHE
   if( bgl_dns_enable_cache() ) {
      int key = get_hash_number( BSTRING_TO_STRING( hostname ) );

      /* acquire the global socket lock */
      bgl_mutex_lock( socket_mutex );

      bhp = (struct bglhostent *)VECTOR_REF( hosttable, key );

retry_cache:      
      /* is it currently in the table? */
      if( bhp
	  && bigloo_strcmp( bhp->hostaddr, hostname )
	  && ((time( 0 ) - bhp->exptime) <= 0)
	  && (!canon || bhp->hp.h_aliases ) ) {
	 bgl_mutex_unlock( socket_mutex );

	 /* we still have to check if the entry in the table corresponds */
	 /* to a sucess, a failure, and pending request.                 */
	 switch( bhp->state ) {
	    case BGLHOSTENT_STATE_FAILURE:
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname hostname=%s FAILURE CACHED...\n", BSTRING_TO_STRING( hostname ) );
#endif	       
	       return 0L;

	    case BGLHOSTENT_STATE_PENDING:
	       /* there is already a pending request for that host, */
	       /* instead of spawing a new request, we wait for the */
	       /* previous running one to complete.                 */
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname hostname=%s PENDING...\n", BSTRING_TO_STRING( hostname ) );
#endif	       
	       while( 1 ) {
		  bgl_condvar_wait( socket_condv, socket_mutex );
		  
		  if( socket_condv_value == bhp ) {
		     /* we got it */
#if( DEBUG_CACHE_DNS )
		     fprintf( stderr, ">>> bglhostbyname hostname=%s RECEIVED\n", BSTRING_TO_STRING( hostname ) );
#endif			      
		     goto retry_cache;
		  }
	       }
	    default:
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname hostname=%s CACHED...\n", BSTRING_TO_STRING( hostname ) );
#endif	       
	       return &(bhp->hp);
	 }
      } else {
	 if( bhp ) {
#if( DEBUG_CACHE_DNS )
	    fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s EXPIRED...\n",
		     __FILE__, __LINE__, BSTRING_TO_STRING( hostname ) );
#endif
	 }
	 
	 /* create the bglhostent entry with the request_pending mark */
	 bhp = make_bglhostent( hostname, 0 );
	 VECTOR_SET( hosttable, key, (obj_t)bhp );
	 bgl_mutex_unlock( socket_mutex );

	 /* make the actual DNS call */
#if( DEBUG_CACHE_DNS )
	 fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s QUERYING DNS...\n",
		  __FILE__, __LINE__, BSTRING_TO_STRING( hostname ) );
#endif	 
	 bglhostentbyname( hostname, bhp, canon );
	 
#if( DEBUG_CACHE_DNS )
	 fprintf( stderr, ">>> state=%d (ok=%d)\n",
		  bhp->state,
		  BGLHOSTENT_STATE_OK );
#endif	 
	 /* store the address in the hashtable and notify */
	 bgl_mutex_lock( socket_mutex );
	 socket_condv_value = bhp;
	 bgl_condvar_broadcast( socket_condv );
	 bgl_mutex_unlock( socket_mutex );
	 
	 /* we still have to check if the entry in the table corresponds */
	 /* to a sucess or a failure. In the latter case, returns 0.     */  
	 return (bhp->state == BGLHOSTENT_STATE_OK) ? &(bhp->hp) : 0L;
      }
   } else 
#endif
   {
      bhp = make_bglhostent( hostname, 0 );
      bglhostentbyname( hostname, bhp, canon );

      return (bhp->state == BGLHOSTENT_STATE_OK) ? &(bhp->hp) : 0L;
   }
}

/*---------------------------------------------------------------------*/
/*    static struct bglhostent *                                       */
/*    make_bglhostentbyaddr ...                                        */
/*---------------------------------------------------------------------*/
static struct bglhostent *
make_bglhostentbyaddr( obj_t hostaddr, struct sockaddr_in *sin ) {
#if( !BGL_HAVE_GETADDRINFO )
   struct hostent *hp;
   struct bglhostent *res;

   bgl_mutex_lock( gethostby_mutex );
   hp = gethostbyaddr( (char *)&(sin->sin_addr),
		       sizeof( sin->sin_addr ),
		       AF_INET );

   res = make_bglhostent( hostaddr, hp );

   bgl_mutex_unlock( gethostby_mutex );
   return res;
#else
   char host[ 80 ];
   int n;
   
   if( !(n = getnameinfo( (struct sockaddr *)sin, sizeof( *sin ),
			  host, sizeof( host ), 0, 0, 0 ) ) ) {
      return make_bglhostent_from_name( hostaddr, sin, host );
   } else {
      return make_bglhostent( hostaddr, 0 );
   }
#endif
}

static struct bglhostent *
make_bglhostentbyaddr_dbg( obj_t hostaddr, struct sockaddr_in *sin ) {
   fprintf( stderr, "%s:%d make_bglgethostbyaddr: %d.%d.%d.%d\n", __FILE__, __LINE__,
	    ((unsigned char *)BSTRING_TO_STRING( hostaddr ))[ 0 ],
	    ((unsigned char *)BSTRING_TO_STRING( hostaddr ))[ 1 ],
	    ((unsigned char *)BSTRING_TO_STRING( hostaddr ))[ 2 ],
	    ((unsigned char *)BSTRING_TO_STRING( hostaddr ))[ 3 ] );
   return make_bglhostentbyaddr( hostaddr, sin );
}

/*---------------------------------------------------------------------*/
/*    struct hostent *                                                 */
/*    bglhostbyaddr ...                                                */
/*    -------------------------------------------------------------    */
/*    See bglhostbynadd.                                               */
/*---------------------------------------------------------------------*/
static struct hostent *
bglhostbyaddr( struct sockaddr_in *sin ) {
   struct bglhostent *bhp;

#if BGL_DNS_CACHE
   if( bgl_dns_enable_cache() ) {
      int key = bgl_get_hash_number_len( (char *)&(sin->sin_addr),
					 0,
					 sizeof( sin->sin_addr ) );

      /* acquire the global socket lock */
      bgl_mutex_lock( socket_mutex );

      bhp = (struct bglhostent *)VECTOR_REF( addrtable, key );

      /* is it currently in the table */
      if( bhp
	  && !strncmp( BSTRING_TO_STRING( bhp->hostaddr ),
		      (char *)&(sin->sin_addr),
		      sizeof( sin->sin_addr ) )
	  && ((time( 0 ) - bhp->exptime) <= 0) ) {
	 bgl_mutex_unlock( socket_mutex );

	 /* we still have to check if the entry in the table corresponds */
	 /* to a sucess or a failure. In the latter case, returns 0.     */
	 return (bhp->state == BGLHOSTENT_STATE_OK) ? &(bhp->hp) : 0L;
      } else {
	 obj_t hostaddr = string_to_bstring_len( (char *)&(sin->sin_addr),
						 sizeof( sin->sin_addr ) );
	 bgl_mutex_unlock( socket_mutex );
	 if( bhp = make_bglhostentbyaddr( hostaddr, sin ) ) {
	    
	    bgl_mutex_lock( socket_mutex );
	    VECTOR_SET( addrtable, key, (obj_t)bhp );
	    bgl_mutex_unlock( socket_mutex );
	    
	    return &(bhp->hp);
	 } else {
	    return 0L;
	 }
      }
   } else 
#endif
   {
      obj_t hostaddr = string_to_bstring_len( (char *)&(sin->sin_addr),
					    sizeof( sin->sin_addr ) );
      bhp = make_bglhostentbyaddr( hostaddr, sin );
      
      if( bhp )
	 return &(bhp->hp);
      else
	 return 0L;
   }
}

/*---------------------------------------------------------------------*/
/*    static struct hostent *                                          */
/*    bgl_gethostent ...                                               */
/*---------------------------------------------------------------------*/
static struct hostent *
bgl_gethostent( obj_t hostname ) {
   struct hostent *hp;

   if( (hp = bglhostbyname( hostname, 1 )) == NULL ) {
      char *msg;
      
      switch( h_errno ) {
         case HOST_NOT_FOUND:
	    msg = "Unknown host";
	    break;
	 case NO_ADDRESS:
	    msg = "No address or no data";
	    break;
	 case NO_RECOVERY:
	    msg = "Internal DNS error";
	    break;
	 case TRY_AGAIN:
	    msg = "temporary error";
	    break;
	 default:
	    msg = "Unknown error";
      }
	    
      C_SYSTEM_FAILURE( BGL_IO_UNKNOWN_HOST_ERROR, "host", msg, hostname );
   }

   return hp;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_host ...                                                     */
/*---------------------------------------------------------------------*/
obj_t
bgl_host( obj_t hostname ) {
   struct hostent *hp = bgl_gethostent( hostname );

   return string_to_bstring( inet_ntoa( *(struct in_addr *)(hp->h_addr) ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_hostinfo ...                                                 */
/*---------------------------------------------------------------------*/
obj_t
bgl_hostinfo( obj_t hostname ) {
   struct hostent *hp = bgl_gethostent( hostname );
   obj_t res = BNIL;
   obj_t addr = BNIL;
   obj_t alias = BNIL;
   obj_t name = BNIL;
   char **runner;
   obj_t s;

   if( hp->h_addr_list ) {
      for( runner = hp->h_addr_list; *runner; runner++ ) {
	 s = string_to_bstring( inet_ntoa( *(struct in_addr *)(*runner) ) );
	 addr = MAKE_PAIR( s, addr );
      }
   }

   if( hp->h_aliases ) {
      for( runner = hp->h_aliases; *runner; runner++ ) {
	 alias = MAKE_PAIR( string_to_bstring( *runner ), alias );
      }
   }

   if( PAIRP( alias ) ) {
      alias = MAKE_PAIR( string_to_symbol( "aliases" ), alias );
      res = MAKE_PAIR( alias, res );
   }
   if( PAIRP( addr ) ) {
      addr = MAKE_PAIR( string_to_symbol( "addresses" ), addr );
      res = MAKE_PAIR( addr, res );
   }

   s = string_to_bstring( hp->h_name );
   name = MAKE_PAIR( s, BNIL );
   name = MAKE_PAIR( string_to_symbol( "name"), name );
		     
   res = MAKE_PAIR( name, res );

   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gethostname ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_gethostname() {
#define MAXHOSTNAME 1024   
   struct hostent *hp;
   static char h[ MAXHOSTNAME + 1 ];
   obj_t res;

   gethostname( h, MAXHOSTNAME );
   hp = bglhostbyname( string_to_bstring( h ), 1 );

   res = string_to_bstring( hp ? hp->h_name : "localhost" );

   return res;
#undef MAXHOSTNAME
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    socket_startup ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
socket_startup() {
#ifdef _BGL_WIN32_VER
   WSADATA data;
   int result = 0;
   WORD version_requested = MAKEWORD( 2, 2 );
   DWORD val = SO_SYNCHRONOUS_NONALERT;

   result = WSAStartup( version_requested, &data );
   if( 0 != result ) {
	socket_error( "socket_init",
		      "Failed to Initialize socket library",
		      BNIL );
   }

   result = setsockopt( INVALID_SOCKET,
			SOL_SOCKET, SO_OPENTYPE,
			(const char *)&val,
			sizeof( val ) );
   if( 0 != result ) {
      socket_error( "make_server_socket",
		    "cannot set socket options",
		    BUNSPEC);
   }
#endif
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    socket_cleanup ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
socket_cleanup() {
#ifdef _BGL_WIN32_VER
   WSACleanup();
#endif
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_sclose_rd ...                                                */
/*---------------------------------------------------------------------*/
static int
bgl_sclose_rd( FILE *stream ) {
#if( !defined( SHUT_RD ) )
#  define SHUT_RD 1
#endif   
   shutdown( fileno( stream ), SHUT_RD );
   return fclose( stream );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_socket_flush ...                                             */
/*---------------------------------------------------------------------*/
static obj_t
bgl_socket_flush( obj_t port ) {
   return BTRUE;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    set_socket_io_ports ...                                          */
/*---------------------------------------------------------------------*/
static void
set_socket_io_ports( int s, obj_t sock, char *who, obj_t inb, obj_t outb ) {
   int t, port;
   obj_t host;
   FILE *fs;

   /* if on windows obtain a C run-time compatible file descriptor */
#ifdef _BGL_WIN32_VER
   s = _open_osfhandle( s, _O_RDWR );
#endif

   /* duplicate handles so that we are able to access one */
   /* socket channel via two scheme ports and thus we are */
   /* able to close each port independently.              */
   t = dup( s );

   if( (t == -1) || (s == -1) ) {
      char *buffer = alloca( 1024 );
      
      sprintf( buffer, "%s: cannot duplicate io port, %s",
	       who, strerror( errno ) );
      socket_error( "set_socket_io_ports", buffer, BUNSPEC );
   }

   if( !(fs = fdopen( t, "r" )) ) { 
      char *buffer = alloca( 1024 );
      
      sprintf( buffer, "%s: cannot create socket io ports, %s (s=%d->%p)",
	       who, strerror( errno ), t, fs );
      socket_error( "set_socket_io_ports", buffer, sock );
   }

   port = SOCKET( sock ).portnum;
   host = SOCKET( sock ).hostip;

   /* Create input port */
   SOCKET( sock ).input = bgl_file_to_buffered_input_port( host, fs, inb );
   SOCKET( sock ).input->port_t.kindof = KINDOF_SOCKET;
   SOCKET( sock ).input->input_port_t.sysread = bgl_read;
   SOCKET( sock ).input->port_t.sysclose = &bgl_sclose_rd;

   /* Create output port */
   SOCKET( sock ).output = bgl_make_output_port( host, (void *)s,
						 KINDOF_SOCKET,
						 outb,
						 (size_t (*)())write,
						 lseek,
						 close );
   SOCKET( sock ).output->output_port_t.sysflush = &bgl_socket_flush;
      
   if( STRING_LENGTH( inb ) <= 1 )
      OUTPUT_PORT( SOCKET( sock ).output ).syswrite = (size_t (*)())&write;

   if( STRING_LENGTH( outb ) <= 1 )
      OUTPUT_PORT( SOCKET( sock ).output ).bufmode = BGL_IONB;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    set_socket_blocking ...                                          */
/*---------------------------------------------------------------------*/
#if( BGL_HAVE_FCNTL )
static void
set_socket_blocking( int fd, int bool ) {
   int val;

   if( (val = fcntl( fd, F_GETFL, 0)) < 0 ) {
      socket_error( "make-client-socket", "cannot get socket control", BUNSPEC);
   }

   if( bool ) {
      val |= O_NONBLOCK;
   } else {
      val &= ~O_NONBLOCK;
   }

   if( fcntl( fd, F_SETFL, val ) < 0) {
      socket_error( "make-client-socket", "cannot set socket control", BUNSPEC);
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_client_socket ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_client_socket( obj_t hostname, int port, int timeo, obj_t inb, obj_t outb ) {
   struct hostent *hp;
   struct sockaddr_in server;
   int s, err;
   obj_t a_socket;
   obj_t hname;

   /* Locate the host IP address */
   if( (hp = bglhostbyname( hostname, 0 )) == NULL ) {
      C_SYSTEM_FAILURE( BGL_IO_UNKNOWN_HOST_ERROR,
			"make-client-socket",
			"unknown or misspelled host name",
			hostname );
   }

   /* Get a socket */
   if( BAD_SOCKET( s = (int)socket( AF_INET, SOCK_STREAM, 0 ) ) ) {
      client_socket_error( hostname, port, "cannot create socket", errno );
   }

   /* Setup a connect address */
   memset( &server, 0, sizeof( server ) );
   memcpy( (char *)&(server.sin_addr), hp->h_addr, hp->h_length );
   server.sin_family = AF_INET;
   server.sin_port = htons( port );

   hname = string_to_bstring( hp->h_name );
   
#if( BGL_HAVE_SELECT && BGL_HAVE_FCNTL )
   if( timeo > 0 ) set_socket_blocking( s, 1 );
#endif

   
   /* Try to connect */
   while( (err = connect( s,
			  (struct sockaddr *)&server,
			  sizeof( struct sockaddr_in )) ) != 0
          && errno == EINTR );
   
   if( err < 0 ) {
#if( BGL_HAVE_SELECT && defined( EINPROGRESS ) ) 
      if( errno == EINPROGRESS ) {
	 fd_set writefds;
	 struct timeval timeout;

	 FD_ZERO( &writefds );
	 FD_SET( s, &writefds );
	 timeout.tv_sec = timeo / 1000000;;
	 timeout.tv_usec = timeo % 1000000;

         /* retry if interrupted */
         while((err = select( s + 1, NULL, &writefds, NULL, &timeout )) < 0
               && errno == EINTR);

	 if( err == 0 ) {
	    close( s );
	    socket_timeout_error( hostname, port );
	 } else {
	    if( err < 0 ) {
	       /* we have experienced a failure so we */
	       /* invalidate the host name entry */
	       invalidate_hostbyname( hostname );
	       
	       close( s );
	       client_socket_error( hostname, port, "Connection failed", errno );
	    } else {
	       int len = sizeof( int );
	       int r = getsockopt( s, SOL_SOCKET, SO_ERROR, (void *)&err, (socklen_t *)&len );

	       if( (r < 0) || (err != 0) ) {
		  /* we have experienced a failure so we */
		  /* invalidate the host name entry */
		  close( s );
		  client_socket_error( hostname, port, 0, err );
	       }
	    }
	 }
	 set_socket_blocking( s, 0 );
      } else {
	 /* we have experienced a failure so we */
	 /* invalidate the host name entry */
	 invalidate_hostbyname( hostname );
      
	 close( s );
	 client_socket_error( hostname, port, "Connection failed", errno );
      }
#else
      /* we have experienced a failure so we */
      /* invalidate the host name entry */
      invalidate_hostbyname( hostname );
      
      close( s );
      client_socket_error( hostname, port, "Connection failed", errno );
#endif
   }

   /* Create a new Scheme socket object */
   a_socket = GC_MALLOC( SOCKET_SIZE );
   a_socket->socket_t.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket_t.portnum = ntohs( server.sin_port );
   a_socket->socket_t.hostname = hname;
   a_socket->socket_t.hostip = string_to_bstring( inet_ntoa( server.sin_addr ) );
   a_socket->socket_t.fd = s;
   a_socket->socket_t.input = BFALSE;
   a_socket->socket_t.output = BFALSE;
   a_socket->socket_t.stype = BGL_SOCKET_CLIENT;
   a_socket->socket_t.userdata = BUNSPEC;

   set_socket_io_ports( s, BREF( a_socket ), "make-client-socket", inb, outb );
   
   return BREF( a_socket );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_unix_socket ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_unix_socket( obj_t path, int timeo, obj_t inb, obj_t outb ) {
#   if( BGL_HAVE_UNIX_SOCKET )
   int s, err;
   obj_t a_socket;
   struct sockaddr_un saddr;

   /* Get a socket */
   if( BAD_SOCKET( s = (int)socket( AF_UNIX, SOCK_STREAM, 0 ) ) ) {
      client_socket_error( path, -1, "cannot create socket", errno );
   }

#if( BGL_HAVE_FCNTL )
   if( timeo > 0 ) set_socket_blocking( s, 1 );
#endif

   saddr.sun_family = AF_UNIX;
   strcpy( saddr.sun_path, BSTRING_TO_STRING( path ) );
   
   /* Try to connect */
   while( (err = connect( s,
			  (struct sockaddr *)&saddr, sizeof( saddr ) ) ) != 0 
          && errno == EINTR );
   
   if( err < 0 ) {
      close( s );
      client_socket_error( path, -1, "Connection failed", errno );
   }

   /* Create a new Scheme socket object */
   a_socket = GC_MALLOC( SOCKET_SIZE );
   a_socket->socket_t.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket_t.hostname = path;
   a_socket->socket_t.portnum = -1;
   a_socket->socket_t.hostip = BUNSPEC;
   a_socket->socket_t.fd = s;
   a_socket->socket_t.input = BFALSE;
   a_socket->socket_t.output = BFALSE;
   a_socket->socket_t.stype = BGL_SOCKET_UNIX;
   a_socket->socket_t.userdata = BUNSPEC;

   set_socket_io_ports( s, BREF( a_socket ), "make-client-socket", inb, outb );
   
   return BREF( a_socket );
#else
   client_socket_error( path, -1, "unix socket domain not supported", errno );
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_server_socket ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_server_socket( obj_t hostname, int port, int backlog ) {
   char msg[] = "make-server-socket";
   struct sockaddr_in sin;
   struct hostent *hp;
   int s, portnum = port, len;
   obj_t a_socket;
   int sock_opt = 1;

   /* Determine port to use */
   if( portnum < 0 )
      socket_error( "make-server-socket", "bad port number", BINT( port ) );

   /* Locate the host IP address */
   if( (hostname != BFALSE) && !(hp = bglhostbyname( hostname, 0 )) ) {
     socket_error( "make-server-socket", "unknown or misspelled host name", 
                   hostname );
   }

   /* Create a socket */
   if( BAD_SOCKET(s = (int)socket( AF_INET, SOCK_STREAM, 0 )) ){
      socket_error( "make-server-socket", "Cannot create socket", BUNSPEC );
   }

   /* Bind the socket to a name */
   if( hostname != BFALSE ) {
      memset( &sin, 0, sizeof( sin ) );
      memcpy( (char*)&sin.sin_addr, hp->h_addr, hp->h_length );
   } else {
     sin.sin_addr.s_addr = INADDR_ANY;
   }

   sin.sin_family = AF_INET;
   sin.sin_port = htons( portnum );

   /* set the reuse flag */
   if( setsockopt( s, SOL_SOCKET, SO_REUSEADDR,
		   &sock_opt, sizeof( sock_opt ) ) < 0 ) {
	 system_error( msg, BINT( port ) );
   }

   if( bind( s, (struct sockaddr *)&sin, sizeof( sin ) ) < 0 ) {
      close( s );
      system_error( msg, BINT( port ) );
   }

   /* Query the socket name, permits to get the true socket number */
   /* if 0 was given                                               */
   len = sizeof( sin );
   if( getsockname( s, (struct sockaddr *) &sin, (socklen_t *) &len ) < 0 ) {
      close( s );
      system_error( msg, BINT( port ) );
   }

   /* Indicate that we are ready to listen */
   if( listen( s, backlog ) < 0 ) {
      close( s );
      system_error( msg, BINT( port ) );
   }

   /* Now we can create the socket object */
   a_socket = GC_MALLOC( SOCKET_SIZE );
   a_socket->socket_t.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket_t.portnum = ntohs( sin.sin_port );
   a_socket->socket_t.hostname = BUNSPEC;
   a_socket->socket_t.hostip = BFALSE;
   a_socket->socket_t.fd = s;
   a_socket->socket_t.input = BFALSE;
   a_socket->socket_t.output = BFALSE;
   a_socket->socket_t.stype = BGL_SOCKET_SERVER;
   a_socket->socket_t.accept = 0L;
   a_socket->socket_t.userdata = BUNSPEC;

   return BREF( a_socket );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    socket_local_addr ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
socket_local_addr( obj_t sock ) {
   struct sockaddr_in sin;
   int len = sizeof( sin );

   if( SOCKET( sock ).stype == BGL_SOCKET_SERVER ) {
      return string_to_bstring( "0.0.0.0" );
   }

   if( getsockname( SOCKET( sock ).fd,
		    (struct sockaddr *)&sin,
		    (socklen_t *) & len) )
      socket_error( "socket-local-address", strerror( errno ), sock );

   return string_to_bstring( (char *)inet_ntoa( sin.sin_addr ) );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    socket_shutdown ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
socket_shutdown( obj_t sock, int close_socket ) {
   int fd = SOCKET( sock ).fd;
   obj_t chook = SOCKET_CHOOK( sock );

   if( fd > 0 ) {
      /* MS: 19 Aug 2008, we don't have to close fd */
      /* since it will be closed automatically with */
      /* SOCKET( sock ).input                       */
      SOCKET( sock ).fd = -1;
      
      if( close_socket ) {
#if( !defined( SHUT_RDWR ) )
#  define SHUT_RDWR 2
#endif   
	 if( shutdown( fd, SHUT_RDWR ) ) {
	    char *buffer = alloca( 1024 );
	    
	    sprintf( buffer, "cannot shutdown socket, %s", strerror( errno ) );
	    socket_error( "socket-shutdown", buffer, sock );
	 }
      }

      if( PROCEDUREP( chook ) ) {
	 if( PROCEDURE_ARITY(chook) == 1 ) {
	    PROCEDURE_ENTRY( chook )( chook, sock, BEOA );
	 } else {
	    C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			      "socket-shutdown",
			      "illegal close hook arity",
			      chook );
	 }
      }

      /* Warning: input and output can have already be garbaged :if the   */
      /* socket is no more used, the input and output are not marked as   */
      /* used and can (eventually) be released before the call to         */
      /* shutdown (through free_socket) be done. One way could be to just */
      /* set SOCKET(sock).{in|out}put to #t and wait that next GC frees   */
      /* the ports if not already down. However, this will really         */
      /* disconnect the peer when the GC occurs rather than when the call */
      /* to shutdown is done. This is not important if this function is   */
      /* called by the GC, but could be annoying when it is called by the */
      /* user                                                             */
      if( INPUT_PORTP( SOCKET(sock).input ) ) {
	 bgl_close_input_port( SOCKET( sock ).input );
	 /* MS: 26 apr 2008, don't loose the port */
	 /* SOCKET( sock ).input = BFALSE; */
      }
   
      if( OUTPUT_PORTP( SOCKET( sock ).output ) ) {
	 bgl_close_output_port( SOCKET( sock ).output );
	 /* MS: 26 apr 2008, don't loose the port */
	 /* SOCKET(sock).output = BFALSE; */
      }

      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_socket_accept ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_socket_accept( obj_t serv, bool_t errp, obj_t inb, obj_t outb ) {
   char *ip;
   struct sockaddr_in sin;
   int len = sizeof( sin );
   int new_s;
   obj_t a_socket;
   int new_fd;
   obj_t hname;

   while( BAD_SOCKET( new_s = (int)accept( SOCKET( serv ).fd,
					   (struct sockaddr *)&sin,
					   (socklen_t *)&len)) ) {
      if( errno == EINTR )
	 continue;

      if( errp )
	 system_error( "socket-accept", serv );
      else
	 return BFALSE;
   }

   /* allocate the socket before any other allocation for bmem */
   a_socket = GC_MALLOC( SOCKET_SIZE );

   /* allocate and fill the new socket client for this connection */
   a_socket->socket_t.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket_t.portnum = ntohs( sin.sin_port );
   a_socket->socket_t.hostname = BUNSPEC;
   a_socket->socket_t.hostip = string_to_bstring( inet_ntoa( sin.sin_addr ) );
   a_socket->socket_t.fd = new_s;
   a_socket->socket_t.stype = BGL_SOCKET_CLIENT;
   a_socket->socket_t.userdata = BUNSPEC;

   set_socket_io_ports( new_s, BREF( a_socket ), "socket-accept", inb, outb );

   if( SOCKET( serv ).accept ) {
      /* this is used, for instance, by the ssl library */
      return SOCKET( serv ).accept( serv, BREF( a_socket ) );
   } else {
      return BREF( a_socket );
   }
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_socket_accept_many ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_socket_accept_many( obj_t serv, bool_t errp, obj_t inbs, obj_t outbs, obj_t vec ) {
   int fd = SOCKET( serv ).fd;
   int flags;
   fd_set set;
   int n;
   long i;
   long l1 = VECTOR_LENGTH( inbs ), l2 = VECTOR_LENGTH( outbs );
   long l3 = VECTOR_LENGTH( vec );
   long l = l1 < l2 ? l1 : l2;


   if( l1 != l2 ) {
      C_SYSTEM_FAILURE( BGL_ERROR,
			"socket-accept-many",
			"in buffers and out buffers lengths mismatch",
			MAKE_PAIR( inbs, outbs ) );
   }
   
#if( BGL_HAVE_SELECT && BGL_HAVE_FCNTL )
   /* set the socket in non-blocking mode */
   /* this seems to be expensive so this should be done once for all */
   flags = fcntl( fd, F_GETFL );
   if( flags == -1 ) {
      if( errp )
	 system_error( "socket-accept-many", serv );
      else
	 return 0;
   }

   flags |= O_NONBLOCK;
   flags = fcntl( fd, F_SETFL, flags );

   if( flags == -1 ) {
      if( errp )
	 system_error( "socket-accept-many", serv );
      else
	 return 0;
   }

   /* select on the sock fd */
   FD_ZERO( &set );
   FD_SET( fd, &set );

   while( (n = select( fd + 1, &set, NULL, NULL, NULL )) <= 0 ) {

      if( errno == EINTR )
	 continue;

      if( errp )
	 C_SYSTEM_FAILURE( BGL_IO_READ_ERROR,
			   "socket-accept-many",
			   strerror( errno ),
			   serv );
      else
	 return 0;
   }

   /* fill the result vector */
   for( i = 0; i < l; i++ ) {
      obj_t sock = bgl_socket_accept( serv,
				      0,
				      VECTOR_REF( inbs, i ),
				      VECTOR_REF( outbs, i ) );

      if( sock == BFALSE ) break;

      VECTOR_SET( vec, i, sock );
   }

   /* this seems to be expensive so this should be done once for all */
   /* WARNING: NOT TESTED... */
   flags &= ~O_NONBLOCK;
   flags = fcntl( fd, F_SETFL, flags );

   if( flags == -1 )
      system_error( "socket-accept-many", serv );
   
   return i;
#else
   {
      obj_t sock = bgl_socket_accept( serv,
				      0,
				      VECTOR_REF( inbs, i ),
				      VECTOR_REF( outbs, i ) );
      VECTOR_SET( vec, 0, sock );

      return 1;
   }
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_socket_hostname ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_socket_hostname( obj_t sock ) {
   if( SOCKET( sock ).hostname == BUNSPEC ) {
      struct hostent *host = 0;
#if( BGL_HAVE_INET_ATON || BGL_HAVE_INET_PTON )
      struct sockaddr_in sin;
#else
      struct sockaddr_in *sin;
#endif      
      
#if( BGL_HAVE_GETADDRINFO )
      socklen_t len = sizeof( sin );

      /* cannot fail because we have created the socket */
      getsockname( SOCKET( sock ).fd,
		   (struct sockaddr *)&sin,
		   (socklen_t *)&len );
#endif
      
#if( BGL_HAVE_INET_ATON )
      /* For IPv4 prefer inet_aton when available because it */
      /* supports more IP format than inet_pton.             */
      if( inet_aton( BSTRING_TO_STRING( SOCKET( sock ).hostip ),
		     &(sin.sin_addr) ) )
	 host = bglhostbyaddr( &sin );
#else
#  if( BGL_HAVE_INET_PTON )	 
      if( inet_pton( AF_INET,
		     (const char *)BSTRING_TO_STRING( SOCKET( sock ).hostip ),
		     &sin.sin_addr ) )
	 host = bglhostbyaddr( &sin );
#  else
      sin = inet_addr( (const char *)BSTRING_TO_STRING( SOCKET( sock ).hostip ) );
      host = bglhostbyaddr( sin );
#  endif
#endif      
      
      if( host ) {
	 return SOCKET( sock ).hostname = string_to_bstring( host->h_name );
      } else {
	 return SOCKET( sock ).hostname = SOCKET( sock ).hostip;
      }
   } else {
      return SOCKET( sock ).hostname;
   }
}
      
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    socket_close ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
socket_close( obj_t sock ) {
   int fd = SOCKET( sock ).fd;
   obj_t chook = SOCKET_CHOOK( sock );

   if( fd > 0 ) {
      /* MS: 19 Aug 2008, we don't have to close fd */
      /* since it will be closed automatically with */
      /* SOCKET( sock ).input                       */
      SOCKET( sock ).fd = -1;

      if( PROCEDUREP( chook ) ) {
	 if( PROCEDURE_ARITY( chook ) == 1 ) {
	    PROCEDURE_ENTRY( chook )( chook, sock, BEOA );
	 } else {
	    C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			      "socket-close",
			      "Illegal close hook arity",
			      chook );
	 }
      }

      if( INPUT_PORTP( SOCKET( sock ).input ) ) {
	 bgl_close_input_port( SOCKET( sock ).input );
	 /* MS: 26 apr 2008, don't loose the port */
	 /* SOCKET( sock ).input = BFALSE; */
      }
   
      if( OUTPUT_PORTP( SOCKET( sock ).output ) ) {
	 bgl_close_output_port( SOCKET( sock ).output );
	 /* MS: 26 apr 2008, don't loose the port */
	 /* SOCKET( sock ).output = BFALSE; */
      }
   }

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    protoalist_to_list ...                                           */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_GETPROTOENT
static obj_t
protoalias_to_list( char **src ) {
   obj_t res = BNIL;
   
   while( *src ) {
      res = MAKE_PAIR( string_to_bstring( *src++ ), res );
   }

   return res;
}
#endif

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    protoent_to_obj ...                                              */
/*---------------------------------------------------------------------*/
#if BGL_HAVE_GETPROTOENT
static obj_t
protoent_to_obj( struct protoent *pe ) {
   obj_t e;
      
   e = MAKE_PAIR( protoalias_to_list( pe->p_aliases ), BNIL );
   e = MAKE_PAIR( BINT( pe->p_proto ), e );
   e = MAKE_PAIR( string_to_bstring( pe->p_name ), e );

   return e;
}
#endif

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_getprotoents ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_getprotoents() {
   obj_t res = BNIL;
#if BGL_HAVE_GETPROTOENT
   struct protoent *pe;
   
   bgl_mutex_lock( protoent_mutex );
   setprotoent( 1 );
   
   while( pe = getprotoent() )
      res = MAKE_PAIR( protoent_to_obj( pe ), res );
   
   endprotoent();
   bgl_mutex_unlock( protoent_mutex );
#endif
   
   return res;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_getprotobyname ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_getprotobyname( char *name ) {
#if BGL_HAVE_GETPROTOENT
   struct protoent *pe = getprotobyname( name );

   return pe ? protoent_to_obj( pe ) : BFALSE;
#else
   return BFALSE;
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_getprotobynumber ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_getprotobynumber( int number ) {
#if BGL_HAVE_GETPROTOENT
   struct protoent *pe = getprotobynumber( number );

   return pe ? protoent_to_obj( pe ) : BFALSE;
#else
   return BFALSE;
#endif   
}

/*---------------------------------------------------------------------*/
/*    GETSOCKOPT ...                                                   */
/*---------------------------------------------------------------------*/
#define GETSOCKOPT( s, level, optname, type, conv )			\
   {									\
      type _v;								\
      socklen_t _l = sizeof( type );					\
      									\
      if( getsockopt( SOCKET( s ).fd, level, optname, &_v, &_l ) ) {	\
	 return BUNSPEC;						\
      } else {								\
	 return conv( _v );						\
      }									\
   }

/*---------------------------------------------------------------------*/
/*    SETSOCKOPT ...                                                   */
/*---------------------------------------------------------------------*/
#define SETSOCKOPT( s, level, optname, type, val )			\
   {									\
      type _v = val;							\
      socklen_t _l = sizeof( type );					\
      									\
      if( setsockopt( SOCKET( s ).fd, level, optname, &_v, _l ) ) {	\
	 return BFALSE;							\
      } else {								\
	 return s;							\
      }									\
   }

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_getsockopt ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_getsockopt( obj_t socket, obj_t option ) {
   if( option == tcp_nodelay )
#if BGL_HAVE_SOCKET_TCP_NODELAY
      GETSOCKOPT( socket, IPPROTO_TCP, TCP_NODELAY, int, BBOOL );
#else
      return BUNSPEC;
#endif      

   if( option == tcp_cork )
#if BGL_HAVE_SOCKET_TCP_CORK
      GETSOCKOPT( socket, IPPROTO_TCP, TCP_CORK, int, BBOOL );
#else
      return BUNSPEC;
#endif      

   if( option == tcp_quickack )
#if BGL_HAVE_SOCKET_TCP_QUICKACK
      GETSOCKOPT( socket, IPPROTO_TCP, TCP_QUICKACK, int, BBOOL );
#else
      return BUNSPEC;
#endif      

   if( option == so_keepalive )
#if( defined( SO_KEEPALIVE ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_KEEPALIVE, int, BBOOL );
#else
      return BUNSPEC;
#endif

   if( option == so_oobinline )
#if( defined( SO_OOBINLINE ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_OOBINLINE, int, BBOOL );
#else
      return BUNSPEC;
#endif      

   if( option == so_rcvbuf )
#if( defined( SO_RCVBUF ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_RCVBUF, int, BINT );
#else
      return BINT( 0 );
#endif      

   if( option == so_sndbuf )
#if( defined( SO_SNDBUF ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_SNDBUF, int, BINT );
#else
      return BINT( 0 );
#endif      

   if( option == so_reuseaddr )
#if( defined( SO_REUSEADDR ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_REUSEADDR, int, BBOOL );
#else
      return BUNSPEC;
#endif      

   if( option == so_timeout )
#if( defined( SO_TIMEOUT ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_TIMEOUT, int, BINT );
#else
      return BINT( 0 );
#endif      
   
   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_setsockopt ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_setsockopt( obj_t socket, obj_t option, obj_t val ) {
   if( option == tcp_nodelay )
#if BGL_HAVE_SOCKET_TCP_NODELAY
      SETSOCKOPT( socket, IPPROTO_TCP, TCP_NODELAY, int, CBOOL( val ) );
#else
      return BFALSE;
#endif      

   if( option == tcp_cork )
#if BGL_HAVE_SOCKET_TCP_CORK
      SETSOCKOPT( socket, IPPROTO_TCP, TCP_CORK, int, CBOOL( val ) );
#else
      return BFALSE;
#endif      

   if( option == tcp_quickack )
#if BGL_HAVE_SOCKET_TCP_QUICKACK
      SETSOCKOPT( socket, IPPROTO_TCP, TCP_QUICKACK, int, CBOOL( val ) );
#else
      return BFALSE;
#endif      

   if( option == so_keepalive )
#if( defined( SO_KEEPALIVE ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_KEEPALIVE, int, CBOOL( val ) );
#else
      return BFALSE;
#endif      

   if( option == so_oobinline )
#if( defined( SO_OOBINLINE ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_OOBINLINE, int, CBOOL( val ) );
#else
      return BFALSE;
#endif      

   if( option == so_rcvbuf )
#if( defined( SO_RCVBUF ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_RCVBUF, int, CINT( val ) );
#else
      return BFALSE;
#endif      

   if( option == so_sndbuf )
#if( defined( SO_SNDBUF ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_SNDBUF, int, CINT( val ) );
#else
      return BFALSE;
#endif      

   if( option == so_reuseaddr )
#if( defined( SO_REUSEADDR ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_REUSEADDR, int, CBOOL( val ) );
#else
      return BFALSE;
#endif      

   if( option == so_timeout )
#if( defined( SO_TIMEOUT ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_TIMEOUT, int, CINT( val ) );
#else
      return BFALSE;
#endif      
   
   return BFALSE;
}
