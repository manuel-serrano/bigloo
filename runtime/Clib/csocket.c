/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/runtime/Clib/csocket.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Jun 29 18:18:45 1998                          */
/*    Last change :  Mon Apr 20 15:59:37 2020 (serrano)                */
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

#include <stddef.h>
#include <bigloo_config.h>
#include <time.h>
#ifndef _BGL_WIN32_VER
#   include <sys/types.h> 
#   include <sys/socket.h>
#   include <netinet/in.h>
#   include <arpa/inet.h>
#   include <netdb.h>
#   ifdef BGL_ANDROID
#     include <linux/in.h>
#     if( !defined( INET_ADDRSTRLEN ) )
         /* INET_ADDRSTRLEN seems to be missing up to r8b */
#        define INET_ADDRSTRLEN 16
#     endif
#   endif
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
#   include <ws2tcpip.h>
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
#   include <sys/socket.h>
#   include <netinet/in.h>
#   include <netinet/tcp.h>
#endif

#if( BGL_HAVE_UNIX_SOCKET )
#   include <sys/un.h>
#endif

#if( BGL_HAVE_GETIFADDRS )
#   include <arpa/inet.h>
#   include <ifaddrs.h>
#endif

#if( BGL_HAVE_GETHWADDRS )
#  include <sys/ioctl.h>
#  include <net/if.h>
#endif

#if( !defined( IFF_LOOPBACK ) )
#  define IFF_LOOPBACK 0
#endif

#if( !defined( SHUT_RD ) )
#  define SHUT_RD 0
#endif
#if( !defined( SHUT_WR ) )
#  define SHUT_WR 1
#endif
#if( !defined( SHUT_RDWR ) )
#  define SHUT_RDWR 2
#endif   

/*---------------------------------------------------------------------*/
/*    DEBUG_SEGV                                                       */
/*---------------------------------------------------------------------*/
#if defined( DEBUG_SEGV )
static FILE *debug_segv_file = 0;

debug_init() {
   struct tm *t;
   char *s;
   long sec = time( 0 );
   char path[ 1024 ];
   extern char *executable_name;
   extern char *rindex(const char *s, int c);
   
   char *i = rindex( executable_name, '/' );

   if( !i ) i = executable_name; else i++;

   sprintf( path, "/tmp/LOG.socket-segv.%s", i );
   
   t = gmtime( (time_t *)&sec );
   s = asctime( t );
   debug_segv_file = fopen( path, "w" );
   
   fprintf( debug_segv_file, "DEBUG_INIT: %s\n", asctime( t ) );
}   

debug_socket_segv( char *fun, unsigned char *ptr, int len ) {
   fprintf( debug_segv_file, "%p,%d %d.%d.%d.%d\n", ptr, len,
	    ptr[ 0 ], ptr[ 1 ], 
	    ptr[ 2 ], ptr[ 3 ] );
   fflush( debug_segv_file );
}
#endif

/*---------------------------------------------------------------------*/
/*    imports ...                                                      */
/*---------------------------------------------------------------------*/
extern obj_t bgl_make_input_port( obj_t, FILE *, obj_t, obj_t );
extern obj_t bgl_close_input_port( obj_t );
extern obj_t bgl_close_output_port( obj_t );
extern long bgl_read( obj_t, char *, long );
extern unsigned char get_hash_number( char * );
extern unsigned char bgl_get_hash_number_len( char *, int, int );
extern bool_t bigloo_strcmp( obj_t o1, obj_t o2 );
extern bool_t bgl_dns_enable_cache();
extern long bgl_dns_cache_validity_timeout();
extern ssize_t bgl_syswrite( obj_t, const void *, size_t );
extern obj_t make_string_sans_fill( long );

#ifndef _BGL_WIN32_VER
#include <unistd.h>
#endif

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
#if defined( DEBUG_SEGV )
static obj_t socket_port_mutex = BUNSPEC;
DEFINE_STRING( socket_port_mutex_name, _4, "socket-port-mutex", 17 );
#endif
static obj_t protoent_mutex = BUNSPEC;
DEFINE_STRING( protoent_mutex_name, _5, "protoent-mutex", 14 );

/*---------------------------------------------------------------------*/
/*    global C variables                                               */
/*---------------------------------------------------------------------*/
static obj_t hosttable = BUNSPEC;
static obj_t addrtable = BUNSPEC;
static obj_t so_keepalive;
static obj_t so_oobinline;
static obj_t so_rcvbuf;
static obj_t so_sndbuf;
static obj_t so_reuseaddr;
static obj_t so_timeout;
static obj_t so_rcvtimeo;
static obj_t so_sndtimeo;
static obj_t tcp_nodelay;
static obj_t tcp_cork;
static obj_t tcp_quickack;
static obj_t ip_multicast_ttl;
static obj_t ip_add_membership;
static obj_t ip_drop_membership;

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_init_socket ...                                              */
/*    -------------------------------------------------------------    */
/*    Initialize the socket machinery (mostly host names caching).     */
/*---------------------------------------------------------------------*/
void
bgl_init_socket() {
   if( !VECTORP( hosttable ) ) {
      hosttable = make_vector( 256, 0 );
      addrtable = make_vector( 256, 0 );

      socket_mutex = bgl_make_mutex( socket_mutex_name );
      socket_condv = bgl_make_condvar( socket_condv_name );
      gethostby_mutex = bgl_make_mutex( gethostby_mutex_name );
      protoent_mutex = bgl_make_spinlock( protoent_mutex_name );
#if defined( DEBUG_SEGV )
      socket_port_mutex = bgl_make_mutex( socket_port_mutex_name );
#endif
      
      so_keepalive = string_to_keyword( "SO_KEEPALIVE" );
      so_oobinline = string_to_keyword( "SO_OOBINLINE" );
      so_rcvbuf = string_to_keyword( "SO_RCVBUF" );
      so_sndbuf = string_to_keyword( "SO_SNDBUF" );
      so_reuseaddr = string_to_keyword( "SO_REUSEADDR" );
      so_timeout = string_to_keyword( "SO_TIMEOUT" );
      so_rcvtimeo = string_to_keyword( "SO_RCVTIMEO" );
      so_sndtimeo = string_to_keyword( "SO_SNDTIMEO" );
      tcp_nodelay = string_to_keyword( "TCP_NODELAY" );
      tcp_cork = string_to_keyword( "TCP_CORK" );
      tcp_quickack = string_to_keyword( "TCP_QUICKACK" );
      ip_multicast_ttl = string_to_keyword( "IP_MULTICAST_TTL" );
      ip_add_membership = string_to_keyword( "IP_ADD_MEMBERSHIP" );
      ip_drop_membership = string_to_keyword( "IP_DROP_MEMBERSHIP" );
      
#if( defined( DEBUG_SEGV ) )
      debug_init();
#endif   
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    socket_error ...                                                 */
/*---------------------------------------------------------------------*/
static void
socket_error( const char *who, const char *message, obj_t object ) {
   C_SYSTEM_FAILURE( BGL_IO_ERROR, (char *)who, (char *)message, object );
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
/*    char *                                                           */
/*    socket_hostname ...                                              */
/*---------------------------------------------------------------------*/
static char *
socket_hostname( obj_t hostname ) {
   char *name = BSTRING_TO_STRING( hostname );

   if( *name == 0 ) {
      /* a local socket */
      return name + 1;
   } else {
      return name;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    client_socket_error ...                                          */
/*---------------------------------------------------------------------*/
static void
client_socket_error( char *proc, obj_t hostname, int port, char *msg, int err ) {
   char buffer1[ 512 ];
   char buffer2[ 512 ];

   BGL_MUTEX_LOCK( socket_mutex );
   if( msg ){
      sprintf( buffer1, "%s (%d), %s", strerror( err ), err, msg ); 
   } else {
      sprintf( buffer1, "%s (%d)", strerror( err ), err );
   }
   BGL_MUTEX_UNLOCK( socket_mutex );
   
   if( port >= 0 ) {
      sprintf( buffer2, "%s:%d", socket_hostname( hostname ), port );
   } else {
      strcpy( buffer2, socket_hostname( hostname ) );
   }

   socket_error( proc, buffer1, string_to_bstring( buffer2 ) );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    tcp_client_socket_error ...                                      */
/*---------------------------------------------------------------------*/
static void
tcp_client_socket_error( obj_t hostname, int port, char *msg, int err ) {
   return client_socket_error( "make-client-socket", hostname, port, msg, err );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    datagram_client_socket_error ...                                 */
/*---------------------------------------------------------------------*/
static void
datagram_client_socket_error( obj_t hostname, int port, char *msg, int err ) {
   return client_socket_error( "make-datagram-client-socket", hostname, port, msg, err );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    system_error ...                                                 */
/*---------------------------------------------------------------------*/
static void
system_error( char *who, obj_t val ) {
   char buffer[ 512 ];

   BGL_MUTEX_LOCK( socket_mutex );
   sprintf( buffer, "%s (%d)", strerror( errno ), errno );
   BGL_MUTEX_UNLOCK( socket_mutex );

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
      char *d = (char *)GC_MALLOC_ATOMIC( size );
      char *s = *run;

#if defined( DEBUG_SEGV )
      // BGL_MUTEX_LOCK( socket_port_mutex );
      fprintf( debug_segv_file, "make_init_array, memcpy atomic=%p size=%d\n", d );
      debug_socket_segv( "make_inet_array", s, size );
      // BGL_MUTEX_UNLOCK( socket_port_mutex );
#endif
      
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
   struct bglhostent *bhp =
      (struct bglhostent *)GC_MALLOC( sizeof( struct bglhostent ) );

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
   struct bglhostent *bhp =
      (struct bglhostent *)GC_MALLOC( sizeof( struct bglhostent ) );
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
/*    bglhostent_fill_from_hostent ...                                 */
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

      for( run = ai; run; run = run->ai_next ) {
	 /* currently, only support IPv4 address lookup */
	 if( run->ai_family == AF_INET ) len++;
      }

      l = (char **)GC_MALLOC( sizeof( char * ) * len + 1 );
      bhp->hp.h_addr_list = l;

      for( run = ai; run; run = run->ai_next ) {
	 if( run->ai_family == AF_INET ) {
	    /* only handle IPv4 addresses (AF_INET). To handle IPv6 */
	    /* as well, should cast into a struct sockaddr_in6 *    */
	    void *d = GC_MALLOC_ATOMIC( bhp->hp.h_length );
	    memcpy( (unsigned char *)d,
		    (char *)&(((struct sockaddr_in *)(run->ai_addr))->sin_addr),
		    bhp->hp.h_length );
	    *l++ = d;
	 }
      }

      *l = 0;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bglhostentbyname ...                                             */
/*    -------------------------------------------------------------    */
/*    When getaddrinfo is available, it is prefered because it is      */
/*    re-entrant while gethostbyname is not.                           */
/*---------------------------------------------------------------------*/
static void
bglhostentbyname( obj_t hostname, struct bglhostent *bhp, int canon ) {
#if( !BGL_HAVE_GETADDRINFO )
   struct hostent *hp;
   struct bglhostent *res;
   
   // MS: 12may19
   // BGL_MUTEX_LOCK( gethostby_mutex );
   hp = gethostbyname( BSTRING_TO_STRING( hostname ) );
   bglhostent_fill_from_hostent( hostname, bhp, hp );
   // MS: 12may19
   // BGL_MUTEX_UNLOCK( gethostby_mutex );
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

   if( !(v=getaddrinfo( BSTRING_TO_STRING( hostname ), 0L, &hints, &res )) ) {
      bglhostent_fill_from_addrinfo( hostname, bhp, res );

      freeaddrinfo( res );
      
      if( bhp->hp.h_addr_list[ 0 ] == 0 ) {
#if( DEBUG_CACHE_DNS )
	 fprintf( stderr, "!!!!!!! %s:%d NO VALID IPv4 address for %s\n",
		  BSTRING_TO_STRING( hostname ) );
#endif
	 bhp->exptime = time( 0L ) + bgl_dns_cache_validity_timeout() / 4;
	 bhp->state = BGLHOSTENT_STATE_FAILURE;
      }
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
      
      BGL_MUTEX_LOCK( socket_mutex );
      
      bhp = (struct bglhostent *)VECTOR_REF( hosttable, key );

      if( bhp && bigloo_strcmp( bhp->hostaddr, hostname ) )
	 VECTOR_SET( hosttable, key, 0 );
      
      BGL_MUTEX_UNLOCK( socket_mutex );
      
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
/*    client don't have to deploy a locking machinery to use it.       */
/*---------------------------------------------------------------------*/
static struct hostent *
bglhostbyname( obj_t hostname, int canon ) {
   struct bglhostent *bhp;

#if BGL_DNS_CACHE
   if( bgl_dns_enable_cache() ) {
      int key = get_hash_number( BSTRING_TO_STRING( hostname ) );

      /* acquire the global socket lock */
      BGL_MUTEX_LOCK( socket_mutex );

retry_cache:
      
      bhp = (struct bglhostent *)VECTOR_REF( hosttable, key );

      /* is it currently in the table? */
      if( bhp
	  && bigloo_strcmp( bhp->hostaddr, hostname )
	  && ((time( 0 ) - bhp->exptime) <= 0)
	  && (!canon || bhp->hp.h_aliases ) ) {

	 /* we still have to check if the entry in the table corresponds */
	 /* to a sucess, a failure, and pending request.                 */
	 switch( bhp->state ) {
	    case BGLHOSTENT_STATE_FAILURE:
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d FAILURE CACHED...\n",
			__FILE__, __LINE__,
			BSTRING_TO_STRING( hostname ), key );
#endif	       
	       BGL_MUTEX_UNLOCK( socket_mutex );
	       return 0L;

	    case BGLHOSTENT_STATE_PENDING:
	       /* there is already a pending request for that host, */
	       /* instead of spawing a new request, we wait for the */
	       /* previous running one to complete.                 */
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d PENDING...\n",
			__FILE__, __LINE__,
			BSTRING_TO_STRING( hostname ), key );
#endif	       
	       BGL_CONDVAR_WAIT( socket_condv, socket_mutex );
		  
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d RECEIVED\n",
			__FILE__, __LINE__,
			BSTRING_TO_STRING( hostname ), key );
#endif	       
	       goto retry_cache;

	    default:
#if( DEBUG_CACHE_DNS )
	       fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d CACHED...\n",
			__FILE__, __LINE__,
			BSTRING_TO_STRING( hostname ), key );
#endif	       
	       BGL_MUTEX_UNLOCK( socket_mutex );
	       return &(bhp->hp);
	 }
      } else {
	 if( bhp ) {
#if( DEBUG_CACHE_DNS )
	    fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d EXPIRED...\n",
		     __FILE__, __LINE__,
		     BSTRING_TO_STRING( hostname ), key );
#endif
	 }
	 
	 /* create the bglhostent entry with the request_pending mark */
	 bhp = make_bglhostent( hostname, 0 );
	 VECTOR_SET( hosttable, key, (obj_t)bhp );
	 // MS: 12may19
	 // BGL_MUTEX_UNLOCK( socket_mutex );

#if( DEBUG_CACHE_DNS )
	 fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d QUERYING DNS...\n",
		  __FILE__, __LINE__, BSTRING_TO_STRING( hostname ), key );
#endif
	 
	 /* make the actual DNS call */
	 bglhostentbyname( hostname, bhp, canon );
	 
#if( DEBUG_CACHE_DNS )
	 fprintf( stderr, ">>> bglhostbyname (%s:%d) hostname=%s key=%d -> state=%d (ok=%d)\n",
		  __FILE__, __LINE__, BSTRING_TO_STRING( hostname ), key,
		  bhp->state,
		  BGLHOSTENT_STATE_OK );
#endif	 
	 /* store the address in the hashtable and notify */
	 // MS: 12may19
	 // BGL_MUTEX_LOCK( socket_mutex );
	 socket_condv_value = bhp;
	 BGL_CONDVAR_BROADCAST( socket_condv );
	 BGL_MUTEX_UNLOCK( socket_mutex );
	 
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

   // MS: 12may19
   // BGL_MUTEX_LOCK( gethostby_mutex );
   hp = gethostbyaddr( (char *)&(sin->sin_addr),
		       sizeof( sin->sin_addr ),
		       AF_INET );

   res = make_bglhostent( hostaddr, hp );

   // MS: 12may19
   // BGL_MUTEX_UNLOCK( gethostby_mutex );
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
/*    See bglhostbyaddr.                                               */
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
      BGL_MUTEX_LOCK( socket_mutex );

      bhp = (struct bglhostent *)VECTOR_REF( addrtable, key );

      /* is it currently in the table */
      if( bhp
	  && !strncmp( BSTRING_TO_STRING( bhp->hostaddr ),
		      (char *)&(sin->sin_addr),
		      sizeof( sin->sin_addr ) )
	  && ((time( 0 ) - bhp->exptime) <= 0) ) {
	 BGL_MUTEX_UNLOCK( socket_mutex );

	 /* we still have to check if the entry in the table corresponds */
	 /* to a sucess or a failure. In the latter case, returns 0.     */
	 return (bhp->state == BGLHOSTENT_STATE_OK) ? &(bhp->hp) : 0L;
      } else {
	 obj_t hostaddr = string_to_bstring_len( (char *)&(sin->sin_addr),
						 sizeof( sin->sin_addr ) );
	 // MS: 12may19
	 // BGL_MUTEX_UNLOCK( socket_mutex );
	 if( bhp = make_bglhostentbyaddr( hostaddr, sin ) ) {
	    
	    // MS: 12may19
	    // BGL_MUTEX_LOCK( socket_mutex );
	    VECTOR_SET( addrtable, key, (obj_t)bhp );
	    BGL_MUTEX_UNLOCK( socket_mutex );
	    
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
/*    bgl_inet_ntop ...                                                */
/*---------------------------------------------------------------------*/
static obj_t
bgl_inet_ntop( int af, struct in_addr *addr ) {
   obj_t obj = make_string_sans_fill( INET_ADDRSTRLEN );
   char *buf = (char *)&(STRING_REF( obj, 0 ));
   const char *s = inet_ntop( af, addr, buf, INET_ADDRSTRLEN );
   
   return bgl_string_shrink( obj, strlen( s ) );
}
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_host ...                                                     */
/*---------------------------------------------------------------------*/
obj_t
bgl_host( obj_t hostname ) {
   struct hostent *hp = bgl_gethostent( hostname );

   return bgl_inet_ntop( AF_INET, (struct in_addr *)(hp->h_addr) );
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
	 s = bgl_inet_ntop( AF_INET, (struct in_addr *)(*runner) );
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

   res = string_to_bstring( hp ? hp->h_name : h );

   return res;
#undef MAXHOSTNAME
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    gethwaddr ...                                                    */
/*---------------------------------------------------------------------*/
static obj_t
gethwaddr( char *intf ) {
#if( BGL_HAVE_GETHWADDRS )
    struct ifreq buffer;
    int s;
    if( (s = socket( PF_INET, SOCK_DGRAM, 0 )) == -1 ) {
       return BFALSE;
    } else {
       char buf[ 6 * 3 + 1 ];
       memset( &buffer, 0x00, sizeof( buffer ) );
       
       strcpy( buffer.ifr_name, intf );

       ioctl( s, SIOCGIFHWADDR, &buffer );
       
       close( s );

       sprintf( buf,"%.2x:%.2x:%.2x:%.2x:%.2x:%.2x",
		(unsigned char)buffer.ifr_hwaddr.sa_data[ 0 ],
		(unsigned char)buffer.ifr_hwaddr.sa_data[ 1 ],
		(unsigned char)buffer.ifr_hwaddr.sa_data[ 2 ],
		(unsigned char)buffer.ifr_hwaddr.sa_data[ 3 ],
		(unsigned char)buffer.ifr_hwaddr.sa_data[ 4 ],
		(unsigned char)buffer.ifr_hwaddr.sa_data[ 5 ] );

       return string_to_bstring( buf );
    }
#else
   return BFALSE;
#endif
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_gethostinterfaces ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_gethostinterfaces() {
#if( BGL_HAVE_GETIFADDRS )
   struct ifaddrs *ifAddrStruct = 0L;
   struct ifaddrs *ifa = 0L;
   void *tmpAddrPtr = 0L;
   obj_t res = BNIL;

   getifaddrs( &ifAddrStruct );

   for( ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next ) {
      obj_t tmp;

      if( !ifa->ifa_addr ) {
	 ;
      } else if( ifa->ifa_addr->sa_family == AF_INET ) {
	 /* a valid IPv4 addr */
	 char addressBuffer[ INET_ADDRSTRLEN ];
	 
	 /* netmask */
	 tmpAddrPtr = &((struct sockaddr_in *)ifa->ifa_netmask)->sin_addr;
	 inet_ntop( AF_INET, tmpAddrPtr, addressBuffer, INET_ADDRSTRLEN );
	 tmp = MAKE_PAIR( string_to_bstring( addressBuffer ), BNIL );

	 /* address */
	 tmpAddrPtr = &((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
	 inet_ntop( AF_INET, tmpAddrPtr, addressBuffer, INET_ADDRSTRLEN );

	 tmp = MAKE_PAIR( BBOOL( ifa->ifa_flags & IFF_LOOPBACK ), tmp );
	 tmp = MAKE_PAIR( gethwaddr( ifa->ifa_name ), tmp );
	 tmp = MAKE_PAIR( string_to_bstring( "IPv4" ), tmp );
	 tmp = MAKE_PAIR( string_to_bstring( addressBuffer ), tmp );
	 tmp = MAKE_PAIR( string_to_bstring( ifa->ifa_name ), tmp );
	 
	 res = MAKE_PAIR( tmp, res );
      } else if( ifa->ifa_addr->sa_family == AF_INET6 ) {
	 /* a valid IPv6 addr */
	 char addressBuffer[ INET6_ADDRSTRLEN ];
	 
	 /* netmask */
	 tmpAddrPtr = &((struct sockaddr_in *)ifa->ifa_netmask)->sin_addr;
	 inet_ntop( AF_INET6, tmpAddrPtr, addressBuffer, INET6_ADDRSTRLEN );
	 tmp = MAKE_PAIR( string_to_bstring( addressBuffer ), BNIL );

	 /* address */
	 tmpAddrPtr = &((struct sockaddr_in6 *)ifa->ifa_addr)->sin6_addr;
	 
	 inet_ntop( AF_INET6, tmpAddrPtr, addressBuffer, INET6_ADDRSTRLEN );
	 tmp = MAKE_PAIR( BBOOL( ifa->ifa_flags & IFF_LOOPBACK ), tmp );
	 tmp = MAKE_PAIR( gethwaddr( ifa->ifa_name ), tmp );
	 tmp = MAKE_PAIR( string_to_bstring( "IPv6" ), tmp );
	 tmp = MAKE_PAIR( string_to_bstring( addressBuffer ), tmp );
	 tmp = MAKE_PAIR( string_to_bstring( ifa->ifa_name ), tmp );

	 res = MAKE_PAIR( tmp, res );
      }
   }
   
   if( ifAddrStruct != 0L ) freeifaddrs( ifAddrStruct );
    
   return res;
#else
#  if( BGL_HAVE_GETHWADDRS )
   int fd;
   struct ifconf conf;
   char data[ 4096 ];
   struct ifreq *ifr;
   obj_t res = BNIL;
   void *tmpAddrPtr = 0L;

   if( (fd = socket( AF_INET, SOCK_DGRAM, 0 )) >= 0 ) {
      conf.ifc_len = sizeof( data );
      conf.ifc_buf = (caddr_t)data;

      if( ioctl( fd, SIOCGIFCONF, &conf ) < 0 ) {
	 goto end;
      } else {
	 ifr = (struct ifreq*)data;
	 while( (char*)ifr < data+conf.ifc_len ) {
	    obj_t tmp;
	    if( ifr->ifr_addr.sa_family == AF_INET ) {
	       char addressBuffer[ INET_ADDRSTRLEN ];
	       
	       /* a valid IPv4 addr */
	       tmpAddrPtr = &((struct sockaddr_in *)&ifr->ifr_netmask)->sin_addr;
	       inet_ntop( AF_INET, tmpAddrPtr, addressBuffer, INET_ADDRSTRLEN );
	       tmp = MAKE_PAIR( string_to_bstring( addressBuffer ), tmp );
	       
	       tmpAddrPtr = &((struct sockaddr_in *)&ifr->ifr_addr)->sin_addr;
	       inet_ntop( AF_INET, tmpAddrPtr, addressBuffer, INET_ADDRSTRLEN );
	       
	       tmp = MAKE_PAIR( BBOOL( ifr->ifr_flags & IFF_LOOPBACK ), BNIL );
	       tmp = MAKE_PAIR( gethwaddr( ifr->ifr_name ), tmp );
	       tmp = MAKE_PAIR( string_to_bstring( "IPv4" ), tmp );
	       tmp = MAKE_PAIR( string_to_bstring( addressBuffer ), tmp );
	       tmp = MAKE_PAIR( string_to_bstring( ifr->ifr_name ), tmp );

	       res = MAKE_PAIR( tmp, res );
	    }
	    
	    ifr++;
	 }
      }

   end:    
      close( fd );
   }
   
   return res;
#  else
   return BNIL;
#  endif
#endif   
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
   shutdown( fileno( stream ), SHUT_RD );
   // ignore shutdown errors to avoid fd leaks
   return fclose( stream ); 
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_sclose_wd ...                                                */
/*---------------------------------------------------------------------*/
static int
bgl_sclose_wd( int stream ) {
   // ignore shutdown errors to avoid fd leaks
   shutdown( stream, SHUT_WR );
   return close( stream );
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_input_socket_seek ...                                        */
/*---------------------------------------------------------------------*/
static void
bgl_input_socket_seek( obj_t port, long offset ) {
   long pos = INPUT_PORT_FILEPOS( port );

   if( offset > pos ) {
      obj_t buf = INPUT_PORT( port ).buf;
      long buflen = STRING_LENGTH( buf );
      
      /* ignore the chars up to the desired position */
      while( offset > 0 ) {
	 long sz = offset - pos;
	 long rs = sz > buflen ? buflen : sz;

	 INPUT_PORT( port ).sysread( port, (char *)&STRING_REF( buf, 0 ), rs );
	 offset -= rs;
      }
	 
      INPUT_PORT( port ).filepos = pos;
      INPUT_PORT( port ).eof = 0;
      INPUT_PORT( port ).matchstart = 0;
      INPUT_PORT( port ).matchstop = 0;
      INPUT_PORT( port ).forward = 0;
      INPUT_PORT( port ).bufpos = 0;
      INPUT_PORT( port ).lastchar = '\n';
      RGC_BUFFER_SET( port, 0, '\0' );

      return;
   }

   if( offset < pos ) {
      /* cannot seek backward */
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"set-input-port-position!",
			"cannot rewind socket input port",
			port );
   }
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
set_socket_io_ports( int s, obj_t sock, const char *who, obj_t inb, obj_t outb ) {
   int t, port;
   obj_t name;
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
      
      BGL_MUTEX_LOCK( socket_mutex );
      sprintf( buffer, "%s: cannot duplicate io port, %s",
	       who, strerror( errno ) );
      BGL_MUTEX_UNLOCK( socket_mutex );
      socket_error( "set_socket_io_ports", buffer, BUNSPEC );
   }

   if( !(fs = fdopen( s, "r" )) ) { 
      char *buffer = alloca( 1024 );
      
      BGL_MUTEX_LOCK( socket_mutex );
      sprintf( buffer, "%s: cannot create socket io ports, %s (%d)",
	       who, strerror( errno ), s );
      BGL_MUTEX_UNLOCK( socket_mutex );
      close( t );
      socket_error( "set_socket_io_ports", buffer, sock );
   }
   
   name = SOCKET( sock ).hostname;

   /* Create input port */
   SOCKET( sock ).input = bgl_make_input_port( name, fs, KINDOF_SOCKET, inb );
   INPUT_PORT( SOCKET( sock ).input ).sysread = &bgl_read;
   INPUT_PORT( SOCKET( sock ).input ).sysseek = &bgl_input_socket_seek;
   PORT( SOCKET( sock ).input ).sysclose = &bgl_sclose_rd;

   /* Create output port */
   SOCKET( sock ).output = bgl_make_output_port( sock,
						 (bgl_stream_t)t,
						 BGL_STREAM_TYPE_FD,
						 KINDOF_SOCKET,
						 outb,
						 bgl_syswrite,
						 (long (*)())&lseek,
						 &bgl_sclose_wd );
   OUTPUT_PORT( SOCKET( sock ).output ).sysflush = &bgl_socket_flush;
      
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
#if( DEBUG_CACHE_DNS )
      fprintf( stderr, "!!!! %s:%d bglhostbyname returns null\n", __FILE__, __LINE__ );
#endif      
      C_SYSTEM_FAILURE( BGL_IO_UNKNOWN_HOST_ERROR,
			"make-client-socket",
			"unknown or misspelled host name",
			hostname );
   }

   /* Get a socket */
   if( BAD_SOCKET( s = (int)socket( AF_INET, SOCK_STREAM, 0 ) ) ) {
      tcp_client_socket_error( hostname, port, "cannot create socket", errno );
   }

   /* Setup a connect address */
   memset( &server, 0, sizeof( server ) );
   
#if defined( DEBUG_SEGV )
   // BGL_MUTEX_LOCK( socket_port_mutex );
   fprintf( debug_segv_file, "bgl_make_client_socket, hp=%p", hp );
   fflush( debug_segv_file );
   fprintf( debug_segv_file, " name=%s src=%p len=%d\n", hp->h_name, hp->h_addr, hp->h_length );
   fflush( debug_segv_file );
   debug_socket_segv( "bgl_make_client", hp->h_addr, hp->h_length );
   // BGL_MUTEX_UNLOCK( socket_port_mutex );
#endif
   
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
	       tcp_client_socket_error( hostname, port, "select failed", errno );
	    } else {
	       int len = sizeof( int );
	       int r = getsockopt( s, SOL_SOCKET, SO_ERROR, (void *)&err, (socklen_t *)&len );

	       if( r < 0 ) {
		  /* we have experienced a failure so we */
		  /* invalidate the host name entry */
                  invalidate_hostbyname( hostname );

		  close( s );
		  tcp_client_socket_error( hostname, port, "getsockopt failed", errno );
	       } else if( err != 0 ) {
		  /* we have experienced a failure so we */
		  /* invalidate the host name entry */
                  invalidate_hostbyname( hostname );

		  close( s );
		  tcp_client_socket_error( hostname, port, "connect failed", err );
	       }
	    }
	 }
	 set_socket_blocking( s, 0 );
      } else {
	 /* we have experienced a failure so we */
	 /* invalidate the host name entry */
	 invalidate_hostbyname( hostname );
      
	 close( s );
	 tcp_client_socket_error( hostname, port, "connect failed", errno );
      }
#else
      /* we have experienced a failure so we */
      /* invalidate the host name entry */
      invalidate_hostbyname( hostname );
      
      close( s );
      tcp_client_socket_error( hostname, port, "connect failed", errno );
#endif
   }

   /* Create a new Scheme socket object */
   a_socket = GC_MALLOC( SOCKET_SIZE );
   a_socket->socket.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket.portnum = ntohs( server.sin_port );
   a_socket->socket.hostname = hname;
   a_socket->socket.hostip = BUNSPEC;
   a_socket->socket.family = AF_INET;
   a_socket->socket.address.in_addr = server.sin_addr;
   a_socket->socket.fd = s;
   a_socket->socket.input = BFALSE;
   a_socket->socket.output = BFALSE;
   a_socket->socket.stype = BGL_SOCKET_CLIENT;
   a_socket->socket.userdata = BUNSPEC;

   set_socket_io_ports( s, BREF( a_socket ), "make-client-socket", inb, outb );
   
   return BREF( a_socket );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_unix_socket ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_unix_socket( obj_t path, int timeo, obj_t inb, obj_t outb ) {
#if( BGL_HAVE_UNIX_SOCKET )
   int s, err;
   obj_t a_socket;
   struct sockaddr_un saddr;
   int namelen = STRING_LENGTH( path );
   char *name = BSTRING_TO_STRING( path );
   socklen_t slen =
      (name[ 0 ] == 0
       ? offsetof( struct sockaddr_un, sun_path ) + namelen 
       : sizeof( saddr ));

   /* Get a socket */
   if( BAD_SOCKET( s = (int)socket( AF_UNIX, SOCK_STREAM, 0 ) ) ) {
      tcp_client_socket_error( path, -1, "cannot create socket", errno );
   }

#if( BGL_HAVE_FCNTL )
   if( timeo > 0 ) set_socket_blocking( s, 1 );
#endif

   /* clear every bytes, they all count for abstract name space */
   memset( &saddr, 0, sizeof( saddr ) );

   saddr.sun_family = AF_UNIX;
   memcpy( saddr.sun_path, name, namelen );
   
   /* Try to connect */
   while( (err = connect( s, (struct sockaddr *)&saddr, slen ) ) != 0 
          && errno == EINTR );
   
   if( err < 0 ) {
      close( s );
      tcp_client_socket_error( path, -1, "Connection failed", errno );
   }

   /* Create a new Scheme socket object */
   a_socket = GC_MALLOC( SOCKET_SIZE );
   a_socket->socket.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket.hostname = path;
   a_socket->socket.portnum = -1;
   a_socket->socket.hostip = BFALSE;
   a_socket->socket.family = AF_UNIX;
   a_socket->socket.fd = s;
   a_socket->socket.input = BFALSE;
   a_socket->socket.output = BFALSE;
   a_socket->socket.stype = BGL_SOCKET_UNIX;
   a_socket->socket.userdata = BUNSPEC;

   set_socket_io_ports( s, BREF( a_socket ), "make-client-socket", inb, outb );
   
   return BREF( a_socket );
#else
   tcp_client_socket_error( path, -1, "unix socket domain not supported", errno );
#endif   
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_server_socket ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_server_socket( obj_t hostname, int portnum, int backlog, bool_t ipv6 ) {
   char msg[] = "make-server-socket";
   struct sockaddr_in sin;
   struct hostent *hp;
   int s, len;
   obj_t a_socket;
   int sock_opt = 1;

   /* Determine port to use */
   if( portnum < 0 )
      socket_error( msg, "bad port number", BINT( portnum ) );

   /* Locate the host IP address */
   if( (hostname != BFALSE) && !(hp = bglhostbyname( hostname, 0 )) ) {
     socket_error( msg, "unknown or misspelled host name", hostname );
   }

   /* Create a socket */
   if( BAD_SOCKET(s = (int)socket( AF_INET, SOCK_STREAM, 0 )) ){
      socket_error( msg, "Cannot create socket", BUNSPEC );
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
	 system_error( msg, BINT( portnum ) );
   }

   if( bind( s, (struct sockaddr *)&sin, sizeof( sin ) ) < 0 ) {
      close( s );
      system_error( msg, BINT( portnum ) );
   }

   /* Query the socket name, permits to get the true socket number */
   /* if 0 was given                                               */
   len = sizeof( sin );
   if( getsockname( s, (struct sockaddr *) &sin, (socklen_t *) &len ) < 0 ) {
      close( s );
      system_error( msg, BINT( portnum ) );
   }

   /* Indicate that we are ready to listen */
   if( listen( s, backlog ) < 0 ) {
      close( s );
      system_error( msg, BINT( portnum ) );
   }

   /* Now we can create the socket object */
   a_socket = GC_MALLOC( SOCKET_SIZE );
   a_socket->socket.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket.portnum = ntohs( sin.sin_port );
   a_socket->socket.hostname = BUNSPEC;
   a_socket->socket.hostip = BFALSE;
   a_socket->socket.family = AF_INET;
   a_socket->socket.fd = s;
   a_socket->socket.input = BFALSE;
   a_socket->socket.output = BFALSE;
   a_socket->socket.stype = BGL_SOCKET_SERVER;
   a_socket->socket.accept = 0L;
   a_socket->socket.userdata = BUNSPEC;

   return BREF( a_socket );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_socket_host_addr ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_socket_host_addr( obj_t sock ) {
   if( SOCKET( sock ).hostip != BUNSPEC ) {
      return SOCKET( sock ).hostip;
   } else {
      switch( SOCKET( sock ).family ) {
	 case AF_INET: 
	    SOCKET( sock ).hostip =
	       bgl_inet_ntop( AF_INET, (struct in_addr *)&(SOCKET( sock ).address) );
	    break;

	 case AF_INET6: 
	    SOCKET( sock ).hostip =
	       bgl_inet_ntop( AF_INET6, (struct in_addr *)&(SOCKET( sock ).address) );
	    break;
      }
	       
      return SOCKET( sock ).hostip;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_socket_local_addr ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_socket_local_addr( obj_t sock ) {
   struct sockaddr_in sin;
   int len = sizeof( sin );

   if( SOCKET( sock ).stype == BGL_SOCKET_SERVER ) {
      return string_to_bstring( "0.0.0.0" );
   }

   if( getsockname( SOCKET( sock ).fd,
		    (struct sockaddr *)&sin,
		    (socklen_t *)&len) ) {
      char *buffer = alloca( 1024 );
      
      BGL_MUTEX_LOCK( socket_mutex );
      strcpy( buffer, strerror( errno ) );
      BGL_MUTEX_UNLOCK( socket_mutex );
      
      socket_error( "socket-local-address", buffer, sock );
   }

   return bgl_inet_ntop( SOCKET( sock ).family, &(sin.sin_addr) );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    sock_localp ...                                                  */
/*    -------------------------------------------------------------    */
/*    Returns true iff the socket client address is the equal          */
/*    to the host address. Returns false otherwise.                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_socket_localp( obj_t sock ) {
   if( SOCKET( sock ).stype == BGL_SOCKET_SERVER ) {
      return 0;
   } else {
      struct sockaddr_storage address;
      int len = sizeof( address );
      
      if( getsockname( SOCKET( sock ).fd,
		       (struct sockaddr *)&address,
		       (socklen_t *)&len) ) {
	 char *buffer = alloca( 1024 );
      
	 BGL_MUTEX_LOCK( socket_mutex );
	 strcpy( buffer, strerror( errno ) );
	 BGL_MUTEX_UNLOCK( socket_mutex );
      
	 socket_error( "socket-localp", buffer, sock );
      } else if( SOCKET( sock ).family == AF_INET ) {
	 /* ipv4 addr */
         struct sockaddr_in* sin = (struct sockaddr_in *)&address;
	 return sin->sin_addr.s_addr == SOCKET( sock ).address.in_addr.s_addr;
      } else {
	 /* ipv6 addr */
	 /* MS: 17nov2014 don't know how to implement this */
         struct sockaddr_in6* sin = (struct sockaddr_in6 *)&address;
	 fprintf( stderr, "(%s:%d) IPV6 UNTESTED\n", __FILE__, __LINE__ );
	 return memcmp( (char *)&(sin->sin6_addr),
			(char *)&(SOCKET( sock ).address.in6_addr),
			sizeof( (SOCKET( sock ).address.in6_addr) ) );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_socket_host_addr_cmp ...                                     */
/*    -------------------------------------------------------------    */
/*    Compare the socket address to the argument.                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_socket_host_addr_cmp( obj_t sock, obj_t addr ) {
   char *a = BSTRING_TO_STRING( addr );

   if( strchr( a, ':' ) ) {
      /* ipv6 family */
      unsigned char buf[ sizeof( struct in6_addr ) ];
      int r = inet_pton( AF_INET6, a, buf );

      if( r <= 0 ) goto error;
	 
      fprintf( stderr, "(%s:%d) IPV6 UNTESTED\n", __FILE__, __LINE__ );
      return memcmp( buf,
		     (char *)&(SOCKET( sock ).address.in6_addr.s6_addr),
		     sizeof( (SOCKET( sock ).address.in6_addr.s6_addr) ) );
   } else {
      /* ipv4 family */
      struct in_addr buf;
      int r = inet_pton( AF_INET, a, &buf );

      if( r <= 0 ) goto error;
	 
      return buf.s_addr == SOCKET( sock ).address.in_addr.s_addr;
   }

error: {
      char *buffer = alloca( 1024 );
      
      BGL_MUTEX_LOCK( socket_mutex );
      strcpy( buffer, strerror( errno ) );
      BGL_MUTEX_UNLOCK( socket_mutex );
      
      socket_error( "socket-localp", buffer, sock );
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    sock_lanp ...                                                    */
/*    -------------------------------------------------------------    */
/*    Returns true iff the socket client address is on the same lan.   */
/*    Returns false otherwise.                                         */
/*---------------------------------------------------------------------*/
/* BGL_RUNTIME_DEF bool_t                                              */
/* sock_lanp( obj_t sock ) {                                           */
/*    if( SOCKET( sock ).stype == BGL_SOCKET_SERVER ) {                */
/*       return 0;                                                     */
/*    } else {                                                         */
/*       struct sockaddr_in sin;                                       */
/*       int len = sizeof( sin );                                      */
/*                                                                     */
/*       if( getsockname( SOCKET( sock ).fd,                           */
/* 		       (struct sockaddr *)&sin,                        */
/* 		       (socklen_t *)&len) ) {                          */
/* 	 char *buffer = alloca( 1024 );                                */
/*                                                                     */
/* 	 BGL_MUTEX_LOCK( socket_mutex );                               */
/* 	 strcpy( buffer, strerror( errno ) );                          */
/* 	 BGL_MUTEX_UNLOCK( socket_mutex );                             */
/*                                                                     */
/* 	 socket_error( "socket-localp", buffer, sock );                */
/*       } else if( SOCKET( sock ).family = AF_INET ) {                */
/* 	 {* ipv4 addr *}                                               */
/* 	 long m = BGL_BINT32_TO_INT32( mask );                         */
/* 	 return (sin.sin_addr.s_addr & m) == (SOCKET( sock ).address.in_addr.s_addr & m); */
/*       } else {                                                      */
/* 	 {* ipv6 addr *}                                               */
/* 	 {* MS: 17nov2014 don't know how to implement this *}          */
/* 	 fprintf( stderr, "(%s:%d) NOT IMPLEMENTED\n", __FILE__, __LINE__ ); */
/* 	 return 0;                                                     */
/*       }                                                             */
/*    }                                                                */
/* }                                                                   */

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
   a_socket->socket.header = MAKE_HEADER( SOCKET_TYPE, 0 );
   a_socket->socket.portnum = ntohs( sin.sin_port );
   a_socket->socket.hostname = BUNSPEC;
   a_socket->socket.hostip = BUNSPEC;
   a_socket->socket.family = AF_INET;
   a_socket->socket.address.in_addr = sin.sin_addr;
   a_socket->socket.fd = new_s;
   a_socket->socket.stype = BGL_SOCKET_CLIENT;
   a_socket->socket.userdata = BUNSPEC;

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

      if( errp ) {
	 char *buffer = alloca( 1024 );

	 BGL_MUTEX_LOCK( socket_mutex );
	 strcpy( buffer, strerror( errno ) );
	 BGL_MUTEX_UNLOCK( socket_mutex );

	 C_SYSTEM_FAILURE( BGL_IO_READ_ERROR, "socket-accept-many",
			   buffer, serv );
      } else {
	 return 0;
      }
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
/*    static obj_t                                                     */
/*    get_socket_hostname ...                                          */
/*---------------------------------------------------------------------*/
static obj_t
get_socket_hostname( int fd, obj_t hostip ) {
   struct hostent *host = 0;

#if( BGL_HAVE_INET_ATON || BGL_HAVE_INET_PTON )
   struct sockaddr_in sin;
#else
   struct sockaddr_in *sin;
#endif      
      
#if( BGL_HAVE_GETADDRINFO )
   socklen_t len = sizeof( sin );

   /* cannot fail because we have created the socket */
   if( fd >= 0 ) {
      getsockname( fd, (struct sockaddr *)&sin, (socklen_t *)&len );
   } else {
      sin.sin_family = AF_INET;
   }
#endif
      
#if( BGL_HAVE_INET_ATON )
   /* For IPv4 prefer inet_aton when available because it */
   /* supports more IP format than inet_pton.             */
   if( inet_aton( BSTRING_TO_STRING( hostip ), &(sin.sin_addr) ) )
      host = bglhostbyaddr( &sin );
#else
#  if( BGL_HAVE_INET_PTON )	 
   if( inet_pton( AF_INET, BSTRING_TO_STRING( hostip ), &sin.sin_addr ) )
      host = bglhostbyaddr( &sin );
#  else
   sin = inet_addr( hostip );
   host = bglhostbyaddr( sin );
#  endif
#endif      
      
   if( host ) {
      return string_to_bstring( host->h_name );
   } else {
      return hostip;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_gethostname_by_address ...                                   */
/*---------------------------------------------------------------------*/
obj_t
bgl_gethostname_by_address( obj_t hostip ) {
   return get_socket_hostname( -1, hostip );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_socket_hostname ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_socket_hostname( obj_t sock ) {
   if( SOCKET( sock ).hostname == BUNSPEC ) {
      if( !STRINGP( SOCKET( sock ).hostip ) ) {
	 return BFALSE;
      } else {
	 return SOCKET( sock ).hostname =
	    get_socket_hostname( SOCKET( sock ).fd, SOCKET( sock ).hostip );
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

   if( fd > 0 ) {
      obj_t chook = SOCKET_CHOOK( sock );
      
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
      }
   
      if( OUTPUT_PORTP( SOCKET( sock ).output ) ) {
	 bgl_close_output_port( SOCKET( sock ).output );
      }
   }

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    socket_shutdown ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
socket_shutdown( obj_t sock, int how ) {
   int fd = SOCKET( sock ).fd;

   if( fd > 0 ) {
      int h;
      switch( how ) {
	 case 2: h = SHUT_RDWR; break;
	 case 1: h = SHUT_WR; break;
	 default: h = SHUT_RD; break;
      }
      
      return shutdown( fd, h );
   }
   
   return 0;
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
   
   BGL_MUTEX_LOCK( protoent_mutex );
   setprotoent( 1 );
   
   while( pe = getprotoent() )
      res = MAKE_PAIR( protoent_to_obj( pe ), res );
   
   endprotoent();
   BGL_MUTEX_UNLOCK( protoent_mutex );
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
   /* This require socket_t and datagram_socket_t to be compatible */
   assert( SOCKET( socket ).fd == DATAGRAM_SOCKET( socket ).fd );
   
   if( option == tcp_nodelay ) {
#if BGL_HAVE_SOCKET_TCP_NODELAY
      GETSOCKOPT( socket, IPPROTO_TCP, TCP_NODELAY, int, BBOOL );
#else
      return BUNSPEC;
#endif      
   }
   
   if( option == tcp_cork ) {
#if BGL_HAVE_SOCKET_TCP_CORK
      GETSOCKOPT( socket, IPPROTO_TCP, TCP_CORK, int, BBOOL );
#else
      return BUNSPEC;
#endif
   }

   if( option == tcp_quickack ) {
#if BGL_HAVE_SOCKET_TCP_QUICKACK
      GETSOCKOPT( socket, IPPROTO_TCP, TCP_QUICKACK, int, BBOOL );
#else
      return BUNSPEC;
#endif
   }

   if( option == so_keepalive ) {
#if( defined( SO_KEEPALIVE ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_KEEPALIVE, int, BBOOL );
#else
      return BUNSPEC;
#endif
   }

   if( option == so_oobinline ) {
#if( defined( SO_OOBINLINE ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_OOBINLINE, int, BBOOL );
#else
      return BUNSPEC;
#endif
   }

   if( option == so_rcvbuf ) {
#if( defined( SO_RCVBUF ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_RCVBUF, int, BINT );
#else
      return BINT( 0 );
#endif      
   }
   
   if( option == so_sndbuf ) {
#if( defined( SO_SNDBUF ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_SNDBUF, int, BINT );
#else
      return BINT( 0 );
#endif
   }

   if( option == so_reuseaddr ) {
#if( defined( SO_REUSEADDR ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_REUSEADDR, int, BBOOL );
#else
      return BUNSPEC;
#endif
   }

   if( option == so_timeout ) {
#if( defined( SO_TIMEOUT ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_TIMEOUT, int, BINT );
#else
      return BINT( 0 );
#endif
   }

#define BTIMEVAL( x ) ELONG_TO_BELONG( _v.tv_sec * 1000000 + _v.tv_usec );
   
   if( option == so_rcvtimeo ) {
#if( defined( SO_RCVTIMEO ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_RCVTIMEO, struct timeval, BTIMEVAL );
#else
      return BINT( 0 );
#endif
   }
   
   if( option == so_sndtimeo ) {
#if( defined( SO_SNDTIMEO ) )
      GETSOCKOPT( socket, SOL_SOCKET, SO_SNDTIMEO, struct timeval, BTIMEVAL );
#else
      return BINT( 0 );
#endif
   }
   
   if( option == ip_multicast_ttl ) {
#if( defined( IP_MULTICAST_TTL ) )
      GETSOCKOPT( socket, SOL_SOCKET, IP_MULTICAST_TTL, int, BINT );
#else
      return BINT( 0 );
#endif
   }
   
    return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    set_timeval ...                                                  */
/*---------------------------------------------------------------------*/
static void
set_timeval( struct timeval *timeout, obj_t val ) {
   if( INTEGERP( val ) ) {
      long timeo = CINT( val );

      timeout->tv_sec = timeo / 1000000;
      timeout->tv_usec = timeo % 1000000;
   } else if( ELONGP( val ) ) {
      long timeo = BELONG_TO_LONG( val );

      timeout->tv_sec = timeo / 1000000;
      timeout->tv_usec = timeo % 1000000;
   } else if( LLONGP( val ) ) {
      BGL_LONGLONG_T timeo = BLLONG_TO_LLONG( val );

      timeout->tv_sec = (long)(timeo / 1000000);
      timeout->tv_usec = (long)(timeo % 1000000);
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_setsockopt ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_setsockopt( obj_t socket, obj_t option, obj_t val ) {
   /* This require socket_t and datagram_socket_t to be compatible */
   assert( SOCKET( socket ).fd == DATAGRAM_SOCKET( socket ).fd );
   
   if( option == tcp_nodelay ) {
#if BGL_HAVE_SOCKET_TCP_NODELAY
      SETSOCKOPT( socket, IPPROTO_TCP, TCP_NODELAY, int, CBOOL( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == tcp_cork ) {
#if BGL_HAVE_SOCKET_TCP_CORK
      SETSOCKOPT( socket, IPPROTO_TCP, TCP_CORK, int, CBOOL( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == tcp_quickack ) {
#if BGL_HAVE_SOCKET_TCP_QUICKACK
      SETSOCKOPT( socket, IPPROTO_TCP, TCP_QUICKACK, int, CBOOL( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_keepalive ) {
#if( defined( SO_KEEPALIVE ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_KEEPALIVE, int, CBOOL( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_oobinline ) {
#if( defined( SO_OOBINLINE ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_OOBINLINE, int, CBOOL( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_rcvbuf ) {
#if( defined( SO_RCVBUF ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_RCVBUF, int, CINT( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_sndbuf ) {
#if( defined( SO_SNDBUF ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_SNDBUF, int, CINT( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_reuseaddr ) {
#if( defined( SO_REUSEADDR ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_REUSEADDR, int, CBOOL( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_timeout ) {
#if( defined( SO_TIMEOUT ) )
      SETSOCKOPT( socket, SOL_SOCKET, SO_TIMEOUT, int, CINT( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == so_rcvtimeo ) {
#if( defined( SO_RCVTIMEO ) )
      struct timeval timeout;
      set_timeval( &timeout, val );
	 
      SETSOCKOPT( socket, SOL_SOCKET, SO_RCVTIMEO, struct timeval, timeout );
#else
      return BFALSE;
#endif
   }
   
   if( option == so_sndtimeo ) {
#if( defined( SO_SNDTIMEO ) )
      struct timeval timeout;
      set_timeval( &timeout, val );

      SETSOCKOPT( socket, SOL_SOCKET, SO_SNDTIMEO, struct timeval, timeout );
#else
      return BFALSE;
#endif
   }
   
   if( option == ip_multicast_ttl ) {
#if IP_MULTICAST_TTL
      SETSOCKOPT( socket, IPPROTO_TCP, IP_MULTICAST_TTL, int, CINT( val ) );
#else
      return BFALSE;
#endif
   }

   if( option == ip_add_membership ) {
#if IP_ADD_MEMBERSHIP
      struct ip_mreq mreq;

      mreq.imr_multiaddr.s_addr = inet_addr( BSTRING_TO_STRING( val ) );
      mreq.imr_interface.s_addr = htonl( INADDR_ANY );
      
      if( setsockopt( SOCKET( socket ).fd, IPPROTO_IP,
	 IP_ADD_MEMBERSHIP, &mreq, sizeof( struct ip_mreq ) ) )
	 return BFALSE;
      else
	 return socket;
#else
   } else {
      return BFALSE;
#endif
   }

   if( option == ip_drop_membership ) {
#if IP_DROP_MEMBERSHIP
      struct ip_mreq mreq;

      mreq.imr_multiaddr.s_addr = inet_addr( BSTRING_TO_STRING( val ) );
      mreq.imr_interface.s_addr = htonl( INADDR_ANY );
      if( setsockopt( SOCKET( socket ).fd, IPPROTO_IP,
	 IP_DROP_MEMBERSHIP, &mreq, sizeof( struct ip_mreq ) ) )
	 return BFALSE;
      else
	 return socket;
#else
   } else {
      return BFALSE;
#endif
   }

   return BFALSE;
}

/*---------------------------------------------------------------------*/
/*    static ssize_t                                                   */
/*    datagram_socket_write ...                                        */
/*---------------------------------------------------------------------*/
static ssize_t
datagram_socket_write( obj_t port, void *buf, size_t len ) {
   obj_t s = (obj_t)PORT_CHANNEL( port );
   obj_t sock = (obj_t)s;
   int fd = BGL_DATAGRAM_SOCKET( sock ).fd;
   struct sockaddr *server = BGL_DATAGRAM_SOCKET( sock ).server;
   int n;

   if( BGL_DATAGRAM_SOCKET( sock ).stype == BGL_SOCKET_SERVER ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-write",
			"server socket",
			sock );
   }
   
   if( fd < 0 ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-write",
			"socket closed",
			sock );
   }

   if( (n = sendto( fd, buf, len, 0,
		    (struct sockaddr *)&BGL_DATAGRAM_SOCKET( sock ).server,
		    sizeof( struct sockaddr_in ) )) == -1 ) {
      char buffer[ 512 ];
      
      BGL_MUTEX_LOCK( socket_mutex );
      sprintf( buffer, "%s (%d)", strerror( errno ), errno );
      BGL_MUTEX_UNLOCK( socket_mutex );

      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-write",
			buffer,
			sock );
   } else {
      return (long)n;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_datagram_client_socket ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_datagram_client_socket( obj_t hostname, int port, bool_t broadcast ) {
   struct hostent *hp;
   int s, err;
   obj_t a_socket;
   obj_t hname;
   struct sockaddr_in *server;

   /* determine port to use */
   if( port < 0 )
      socket_error( "make-datagram-client-socket", "bad port number", BINT( port ) );

   /* locate the host IP address */
   if( (hp = bglhostbyname( hostname, 0 )) == NULL ) {
      C_SYSTEM_FAILURE( BGL_IO_UNKNOWN_HOST_ERROR,
			"make-datagram-client-socket",
			"unknown or misspelled host name",
			hostname );
   }

   /* get a socket */
   if( BAD_SOCKET( s = (int)socket( AF_INET, SOCK_DGRAM, 0 ) ) ) {
      datagram_client_socket_error( hostname, port, "cannot create socket", errno );
   }

   // configure the socket
   if( broadcast ) {
      int bcast = 1;
      if( setsockopt( s, SOL_SOCKET, SO_BROADCAST, &bcast, sizeof( bcast ) ) == -1) {
	 datagram_client_socket_error( hostname, port,
				       "cannot configure socket for broadcast",
				       errno );
      }
   }
   
   a_socket = GC_MALLOC( BGL_DATAGRAM_SOCKET_SIZE + sizeof( struct sockaddr_in ) );
   server = (struct sockaddr_in *)&(a_socket->datagram_socket.server);
   
   /* setup a connect address */
   memset( server, 0, sizeof( struct sockaddr_in ) );
   memcpy( (char *)&(server->sin_addr), hp->h_addr, hp->h_length );
   server->sin_family = AF_INET;
   server->sin_port = htons( port );
   server->sin_addr = *((struct in_addr *)hp->h_addr);

   hname = string_to_bstring( hp->h_name );
   memset( server->sin_zero, 0, sizeof( server->sin_zero ) );

   a_socket->datagram_socket.header = MAKE_HEADER( DATAGRAM_SOCKET_TYPE, 0 );
   a_socket->datagram_socket.portnum = ntohs( server->sin_port );
   a_socket->datagram_socket.hostname = hname;
/*    a_socket->datagram_socket.hostip = bgl_inet_ntop( AF_INET, &(server->sin_addr) ); */
   a_socket->datagram_socket.hostip = BUNSPEC;
   a_socket->datagram_socket.family = AF_INET;
   a_socket->datagram_socket.address.in_addr = server->sin_addr;
   
   a_socket->datagram_socket.stype = BGL_SOCKET_CLIENT;
   a_socket->datagram_socket.fd = s;
   
   /* socket port */
   a_socket->datagram_socket.port =
      bgl_make_output_port( a_socket->datagram_socket.hostip,
			    (bgl_stream_t)(void *)BREF( a_socket ),
			    BGL_STREAM_TYPE_CHANNEL,
			    KINDOF_SOCKET,
			    make_string_sans_fill( 0 ),
			    &datagram_socket_write,
			    0L,
			    &bgl_sclose_wd );
   OUTPUT_PORT( a_socket->datagram_socket.port ).sysflush = &bgl_socket_flush;
   OUTPUT_PORT( a_socket->datagram_socket.port ).bufmode = BGL_IONB;
   
   return BREF( a_socket );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_datagram_server_socket ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_datagram_server_socket( int portnum ) {
   char msg[] = "make-datagram-server-socket";
#if( !BGL_HAVE_GETADDRINFO )
      socket_error( msg, "not supported (requires getaddrinfo)", BFALSE );
#else
   int s;
   struct addrinfo hints, *servinfo, *p;
   int rv;
   char service[ 10 ];
   obj_t sock, buf, inb, iport;
   FILE *fs;

   /* Determine port to use */
   if( portnum < 0 )
      socket_error( msg, "bad port number", BINT( portnum ) );

   memset( &hints, 0, sizeof( hints ) );
#ifdef BGL_ANDROID
   /* set to AF_INET to force IPv4 */
   hints.ai_family = AF_INET; 
#else   
   hints.ai_family = AF_UNSPEC;
#endif
   hints.ai_socktype = SOCK_DGRAM;
#if( !defined( AI_NUMERICSERV ) )
   hints.ai_flags = AI_PASSIVE;
#else
   /* use my IP and numeric port */
   hints.ai_flags = AI_PASSIVE | AI_NUMERICSERV; 
#endif
   sprintf( service, "%d", portnum );

   if( (rv = getaddrinfo( NULL, service, &hints, &servinfo )) != 0 ) {
      socket_error( msg, (char *)gai_strerror( rv ), BINT( portnum ) );
   }

   /* loop through all the results and bind to the first we can */
   for( p = servinfo; p != NULL; p = p->ai_next ) {
      int sock_opt = 1;
      
      if( (s = socket( p->ai_family, p->ai_socktype, p->ai_protocol )) == -1 ) {
	 socket_error( msg, "cannot create socket", BINT( portnum ) );
      }

      /* set the reuse flag */
      if( setsockopt( s, SOL_SOCKET, SO_REUSEADDR,
		      &sock_opt, sizeof( sock_opt ) ) < 0 ) {
	 system_error( msg, BINT( portnum ) );
      }

      if( bind( s, p->ai_addr, p->ai_addrlen ) == -1 ) {
	 close( s );
	 socket_error( msg, "cannot bind socket", BINT( portnum ) );
      }

      break;
   }

   freeaddrinfo( servinfo );

   /* Now we can create the socket object */
   sock = GC_MALLOC( SOCKET_SIZE );
   sock->datagram_socket.header = MAKE_HEADER( DATAGRAM_SOCKET_TYPE, 0 );
   sock->datagram_socket.portnum = portnum;
   sock->datagram_socket.hostname = BUNSPEC;
   sock->datagram_socket.hostip = BFALSE;
   sock->datagram_socket.family = AF_INET;
   sock->datagram_socket.fd = s;
   sock->datagram_socket.stype = BGL_SOCKET_SERVER;

   if ( !(fs = fdopen( s, "r" )) ) {
      char buffer[1024];

      BGL_MUTEX_LOCK( socket_mutex );
      sprintf( buffer, "%s: cannot create datagram server socket io port, %s (s=%d->%p)",
	       msg, strerror( errno ), s, fs );
      BGL_MUTEX_UNLOCK( socket_mutex );
      socket_error( "bgl_make_datagram_server_socket", buffer, sock );
   }

   /* Make an unbuffered input port, so that `datagram-socket-receive',   */
   /* which bypasses port buffering, can still be used without troubles.  */
   setbuf( fs, NULL );
   sock->datagram_socket.port =
      bgl_make_input_port( string_to_bstring( "datagram-server" ),
			   fs, KINDOF_DATAGRAM,
			   make_string_sans_fill( 0 ) );
   INPUT_PORT( sock->datagram_socket.port ).sysread = bgl_read;
   INPUT_PORT( sock->datagram_socket.port ).sysseek = bgl_input_socket_seek;
   PORT( sock->datagram_socket.port ).sysclose = bgl_sclose_rd;

   return BREF( sock );
#endif
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_datagram_server_unbound_socket ...                      */
/*---------------------------------------------------------------------*/
obj_t
bgl_make_datagram_unbound_socket( obj_t family ) {
   static const char msg[] = "make-datagram-unbound-socket";
   int fam, s;
   obj_t sock, buf, inb, iport;
   FILE *fs;

   if( family == string_to_symbol( "inet" ) ) {
      fam = AF_INET;
   } else if( family == string_to_symbol( "inet6" ) ) {
      fam = AF_INET6;
   } else if( family == string_to_symbol( "unix" ) ) {
      fam = AF_UNIX;
   } else if( family == string_to_symbol( "local" ) ) {
      fam = AF_UNIX;
   } else {
      socket_error( msg, "unsupported socket family", family );
   }

   if( (s = socket( fam, SOCK_DGRAM, 0 )) == -1 ) {
      socket_error( msg, "cannot create socket", family );
   }

   sock = GC_MALLOC( SOCKET_SIZE );
   sock->datagram_socket.header = MAKE_HEADER( DATAGRAM_SOCKET_TYPE, 0 );
   sock->datagram_socket.portnum = 0;
   sock->datagram_socket.hostname = BUNSPEC;
   sock->datagram_socket.hostip = BFALSE;
   sock->datagram_socket.family = AF_INET;
   sock->datagram_socket.fd = s;
   sock->datagram_socket.stype = BGL_SOCKET_SERVER;

   if ( !(fs = fdopen( s, "r" )) ) {
      char buffer[1024];

      BGL_MUTEX_LOCK( socket_mutex );
      sprintf( buffer, "%s: cannot create datagram server socket io port, %s (s=%d->%p)",
	       msg, strerror( errno ), s, fs );
      BGL_MUTEX_UNLOCK( socket_mutex );
      socket_error( "bgl_make_datagram_server_socket", buffer, sock );
   }

   /* Make an unbuffered input port, so that `datagram-socket-receive',   */
   /* which bypasses port buffering, can still be used without troubles.  */
   setbuf( fs, NULL );
   sock->datagram_socket.port =
      bgl_make_input_port( string_to_bstring( "datagram-server" ),
			   fs, KINDOF_DATAGRAM,
			   make_string_sans_fill( 0 ) );
   INPUT_PORT( sock->datagram_socket.port ).sysread = bgl_read;
   INPUT_PORT( sock->datagram_socket.port ).sysseek = bgl_input_socket_seek;
   PORT( sock->datagram_socket.port ).sysclose = bgl_sclose_rd;

   return BREF( sock );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_datagram_socket_hostname ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_datagram_socket_hostname( obj_t sock ) {
   if( BGL_DATAGRAM_SOCKET( sock ).hostname == BUNSPEC &&
       BGL_DATAGRAM_SOCKET( sock ).hostip != BFALSE ) {
      return BGL_DATAGRAM_SOCKET( sock ).hostname =
	 get_socket_hostname( BGL_DATAGRAM_SOCKET( sock ).fd,
			      BGL_DATAGRAM_SOCKET( sock ).hostip );
   } else {
      return BGL_DATAGRAM_SOCKET( sock ).hostname;
   }
}
      
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_datagram_socket_close ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_datagram_socket_close( obj_t sock ) {
   int fd = BGL_DATAGRAM_SOCKET( sock ).fd;

   if( fd > 0 ) {
      obj_t chook = BGL_DATAGRAM_SOCKET( sock ).chook;
      shutdown( BGL_DATAGRAM_SOCKET( sock ).fd, SHUT_RDWR );
      close( BGL_DATAGRAM_SOCKET( sock ).fd );
      
      BGL_DATAGRAM_SOCKET( sock ).fd = -1;

      if( PROCEDUREP( chook ) ) {
	 if( PROCEDURE_ARITY( chook ) == 1 ) {
	    PROCEDURE_ENTRY( chook )( chook, sock, BEOA );
	 } else {
	    C_SYSTEM_FAILURE( BGL_ERROR,
			      "datagram-socket-close",
			      "Illegal close hook arity",
			      chook );
	 }
      }

      if( OUTPUT_PORTP( BGL_DATAGRAM_SOCKET( sock ).port ) ) {
	 bgl_close_output_port( BGL_DATAGRAM_SOCKET( sock ).port );
      }
   }

   return BUNSPEC;
}

/*---------------------------------------------------------------------*/
/*    static const char *                                              */
/*    get_hostip ...                                                   */
/*---------------------------------------------------------------------*/
static const char *
get_hostip( struct sockaddr *sa, char *s, int sz ) {
#ifndef _BGL_WIN32_VER    
   return inet_ntop( sa->sa_family,
		     &(((struct sockaddr_in *)sa)->sin_addr),
		     s, sz );
#else
   DWORD dwLength = INET6_ADDRSTRLEN;
   WSAAddressToString( sa, sizeof(struct sockaddr_storage), NULL, s, &dwLength );
   return s;
#endif
}
   
/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_datagram_socket_receive ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_datagram_socket_receive( obj_t sock, long sz ) {
   struct sockaddr_storage their_addr;
   char buf[ sz ];
   socklen_t addr_len;
   int n;
   int fd = BGL_DATAGRAM_SOCKET( sock ).fd;

   if( BGL_DATAGRAM_SOCKET( sock ).stype == BGL_SOCKET_CLIENT ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-receive",
			"client socket",
			sock );
   }
   
   if( fd < 0 ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-receive",
			"socket closed",
			sock );
   }

   addr_len = sizeof( their_addr );
   if( (n = recvfrom( fd, buf, sz - 1 , 0,
		      (struct sockaddr *)&their_addr, &addr_len )) == -1 ) {
      socket_error( "datagram-socket-receive", "cannot receive datagram", sock );
   } else {
      obj_t env = BGL_CURRENT_DYNAMIC_ENV();
      struct sockaddr *sa = (struct sockaddr *)&their_addr;
      char addrbuf[ INET6_ADDRSTRLEN ];
      const char *c = get_hostip( sa, addrbuf, sizeof( addrbuf ) );
      
      BGL_ENV_MVALUES_NUMBER_SET( env, 2 );
      BGL_ENV_MVALUES_VAL_SET( env, 1, string_to_bstring( (char *)c ) );

      return string_to_bstring_len( buf, n );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_datagram_socket_send ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_datagram_socket_send( obj_t sock, obj_t str, obj_t host, int port ) {
   struct sockaddr_storage their_addr;
   socklen_t slen;
   ssize_t sent;
   int fd = BGL_DATAGRAM_SOCKET( sock ).fd;

   if( BGL_DATAGRAM_SOCKET( sock ).stype == BGL_SOCKET_CLIENT ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-send",
			"client socket",
			sock );
   }

   if( fd < 0 ) {
      C_SYSTEM_FAILURE( BGL_IO_PORT_ERROR,
			"datagram-socket-send",
			"socket closed",
			sock );
   }

   /* FIXME: No support for AF_UNIX, etc.  */
   if( !inet_pton( AF_INET, BSTRING_TO_STRING( host ),
		   &((struct sockaddr_in *)&their_addr)->sin_addr ) ) {
     if( !inet_pton( AF_INET6, BSTRING_TO_STRING( host ),
		     &((struct sockaddr_in6 *)&their_addr)->sin6_addr ) ) {
       socket_error( "datagram-socket-send",
		     "cannot convert destination address", sock );
     } else {
	((struct sockaddr_in6 *)&their_addr)->sin6_port = htons( port );
	((struct sockaddr *)&their_addr)->sa_family = AF_INET6;
	slen = sizeof( struct sockaddr_in6 );
     }
   } else {
      ((struct sockaddr_in *)&their_addr)->sin_port = htons( port );
      ((struct sockaddr *)&their_addr)->sa_family = AF_INET;
      slen = sizeof( struct sockaddr_in );
   }

   sent = sendto( fd, BSTRING_TO_STRING( str ), STRING_LENGTH( str ), 0,
		  (struct sockaddr *) &their_addr, slen );
   if( sent < 0 ) {
      socket_error( "datagram-socket-send", "cannot send datagram", sock );
   }

   return BINT( sent );
}
