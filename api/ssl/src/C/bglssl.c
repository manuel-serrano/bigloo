/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/api/ssl/src/C/bglssl.c        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano & Stephane Epardaud                */
/*    Creation    :  Wed Mar 23 16:54:42 2005                          */
/*    Last change :  Thu Sep 12 14:07:21 2019 (serrano)                */
/*    Copyright   :  2005-20 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    SSL socket client-side support                                   */
/*=====================================================================*/
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/bio.h>
#include <openssl/crypto.h>
#include <openssl/pkcs12.h>
#include <openssl/x509v3.h>
#include <fcntl.h>

#if defined( _MSC_VER) || defined( _MINGW_VER )
#  define _BGL_WIN32_VER
#endif

#include <bigloo_config.h>
#include <sys/types.h>
#ifndef _BGL_WIN32_VER
#   include <sys/socket.h>
#   include <netinet/in.h>
#   include <arpa/inet.h>
#   include <netdb.h>
#   include <time.h>
#   if( BGL_HAVE_SELECT )
#     include <sys/time.h>
#     include <sys/types.h>
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
#include "bglssl.h"
#include "ssl.h"

#if( defined( BGLSSL_HAVE_RAND_POLL ) || defined( BGLSSL_HAVE_RAND_STATUS ) )
#  include <openssl/rand.h>
#endif

#if( !defined( OPENSSL_NPN_UNSUPPORTED ) )
#  define OPENSSL_NPN_UNSUPPORTED 0
#endif
#if( !defined( OPENSSL_NPN_NEGOTIATED ) )
#  define OPENSSL_NPN_NEGOTIATED 1
#endif
#if( !defined( OPENSSL_NPN_NO_OVERLAP ) )
#  define OPENSSL_NPN_NO_OVERLAP  2
#endif

#define socklen_t void

/* DON'T REMOVE, used by ssl_debug.h (see below)! */
#define BGL_SSL

#ifndef _BGL_WIN32_VER
#   define BAD_SOCKET(s) ((s) < 0)
#else
#   define BAD_SOCKET(s) ((s) == INVALID_SOCKET)
#endif

#define SOCKET_IO_BUFSIZE 1024

static const int X509_NAME_FLAGS = ASN1_STRFLGS_ESC_CTRL
   | ASN1_STRFLGS_ESC_MSB
   | XN_FLAG_SEP_MULTILINE
   | XN_FLAG_FN_SN;

const char *root_certs[] = {
#include "root_certs.h"
  NULL
};

extern obj_t void_star_to_obj( void * );
extern obj_t make_string( long, char );

#define kMaxSessionSize (10 * 1014)

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    RAND_status ...                                                  */
/*---------------------------------------------------------------------*/
#if( !defined( BGLSSL_HAVE_RAND_STATUS ) )
int RAND_status() {
   return 1;
}
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    RAND_poll ...                                                    */
/*---------------------------------------------------------------------*/
#if( !defined( BGLSSL_HAVE_RAND_POLL ) )
int RAND_poll() {
   return 1;
}
#endif

/*---------------------------------------------------------------------*/
/*    type aliasing                                                    */
/*---------------------------------------------------------------------*/
typedef BgL_sslzd2connectionzd2_bglt ssl_connection;
typedef BgL_securezd2contextzd2_bglt secure_context;
typedef BgL_sslzd2hashzd2_bglt ssl_hash;
typedef BgL_sslzd2hmaczd2_bglt ssl_hmac;
typedef BgL_sslzd2signzd2_bglt ssl_sign;
typedef BgL_sslzd2verifyzd2_bglt ssl_verify;
typedef BgL_sslzd2cipherzd2_bglt ssl_cipher;

#define CCON( o ) ((ssl_connection)COBJECT( o))
#define CSC( o ) ((secure_context)COBJECT( o))
#define CHASH( o ) ((ssl_hash)COBJECT( o))
#define CHMAC( o ) ((ssl_hmac)COBJECT( o))
#define CSIGN( o ) ((ssl_sign)COBJECT( o))
#define CVERIFY( o ) ((ssl_verify)COBJECT( o))
#define CCIPHER( o ) ((ssl_cipher)COBJECT( o))

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
extern obj_t bigloo_mutex;

/*---------------------------------------------------------------------*/
/*    SSL mutex                                                        */
/*---------------------------------------------------------------------*/
static obj_t ssl_mutex = BUNSPEC;
DEFINE_STRING( ssl_mutex_name, _1, "ssl-mutex", sizeof( "ssl-mutex" ) + 1 );

/*---------------------------------------------------------------------*/
/*    SSL socket close hook                                            */
/*---------------------------------------------------------------------*/
extern obj_t socket_close(obj_t);
static obj_t socket_close_hook( obj_t, obj_t );
static obj_t input_close_hook( obj_t, obj_t );
static obj_t output_close_hook( obj_t, obj_t );

DEFINE_STATIC_BGL_PROCEDURE( ssl_socket_close_hook, _2, socket_close_hook, 0L, BUNSPEC, 1 );
DEFINE_STATIC_BGL_PROCEDURE( ssl_input_close_hook, _3, input_close_hook, 0L, BUNSPEC, 1 );
DEFINE_STATIC_BGL_PROCEDURE( ssl_output_close_hook, _4, output_close_hook, 0L, BUNSPEC, 1 );

/*---------------------------------------------------------------------*/
/*    The global SSL context                                           */
/*---------------------------------------------------------------------*/
static SSL_CTX *ctxc[ BGLSSL_TLSV1_3 + 1 ], *ctxs[ BGLSSL_TLSV1_3 + 1 ];

extern obj_t bgl_make_certificate( X509 *cert );
extern obj_t bgl_make_private_key( EVP_PKEY* pkey );
extern X509 *bgl_certificate_native( obj_t cert );
extern EVP_PKEY *bgl_private_key_native( obj_t pkey );

/*---------------------------------------------------------------------*/
/*    bgl_ssl_library_init                                             */
/*---------------------------------------------------------------------*/
void static bgl_ssl_library_init() {
   SSL_library_init();

   OpenSSL_add_all_algorithms();
   OpenSSL_add_all_digests();

   // Turn off compression. Saves memory and protects against CRIME attacks.
   // No-op with OPENSSL_NO_COMP builds of OpenSSL.
   sk_SSL_COMP_zero(SSL_COMP_get_compression_methods());

   SSL_load_error_strings();
   ERR_load_crypto_strings();
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_ssl_init ...                                                 */
/*---------------------------------------------------------------------*/
static void
bgl_ssl_init() {
   static int initialized = 0;

   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !initialized ) {
      initialized = 1;

      /* the SSL dedicated lock */
      ssl_mutex = bgl_make_mutex( ssl_mutex_name );
      
      /* Initialize SSL context */
      bgl_ssl_library_init();
      
#if( BGLSSL_HAVE_SSLV2 )
      ctxc[ BGLSSL_SSLV2 ] = SSL_CTX_new( SSLv2_client_method() );
#endif
#if( BGLSSL_HAVE_SSLV3 )
      ctxc[ BGLSSL_SSLV3 ] = SSL_CTX_new( SSLv3_client_method() );
#endif      
#if( BGLSSL_HAVE_SSLV23 )
      ctxc[ BGLSSL_SSLV23 ] = SSL_CTX_new( SSLv23_client_method() );
#endif      
#if( BGLSSL_HAVE_TLSV1 )
      ctxc[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLSv1_client_method() );
#else      
      ctxc[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLS_client_method() );
#endif      
#if( BGLSSL_HAVE_TLSV1_1 )
      ctxc[ BGLSSL_TLSV1_1 ] = SSL_CTX_new( TLSv1_1_client_method() );
#else      
      ctxc[ BGLSSL_TLSV1_1 ] = ctxc[ BGLSSL_TLSV1 ];
#endif
#if( BGLSSL_HAVE_TLSV1_2 )
      ctxc[ BGLSSL_TLSV1_2 ] = SSL_CTX_new( TLSv1_2_client_method() );
#else      
      ctxc[ BGLSSL_TLSV1_2 ] = ctxc[ BGLSSL_TLSV1 ];
#endif
#if( BGLSSL_HAVE_TLSV1_3 )
      ctxc[ BGLSSL_TLSV1_3 ] = SSL_CTX_new( TLSv1_3_client_method() );
#else      
      ctxc[ BGLSSL_TLSV1_3 ] = ctxc[ BGLSSL_TLSV1 ];
#endif
#if( BGLSSL_HAVE_DTLS )
      ctxc[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLSv1_client_method() );
#else      
      ctxc[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLS_client_method() );
#endif
      
#if( BGLSSL_HAVE_SSLV2 )
      ctxs[ BGLSSL_SSLV2 ] = SSL_CTX_new( SSLv2_server_method() );
#endif      
#if( BGLSSL_HAVE_SSLV3 )
      ctxs[ BGLSSL_SSLV3 ] = SSL_CTX_new( SSLv3_server_method() );
#endif
#if( BGLSSL_HAVE_SSLV23 )
      ctxs[ BGLSSL_SSLV23 ] = SSL_CTX_new( SSLv23_server_method() );
#endif      
#if( BGLSSL_HAVE_TLSV1 )
      ctxs[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLSv1_server_method() );
#else
      ctxs[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLS_server_method() );
#endif      
#if( BGLSSL_HAVE_TLSV1_1 )
      ctxs[ BGLSSL_TLSV1_1 ] = SSL_CTX_new( TLSv1_1_server_method() );
#else      
      ctxs[ BGLSSL_TLSV1_1 ] = ctxs[ BGLSSL_TLSV1 ];
#endif
#if( BGLSSL_HAVE_TLSV1_2 )
      ctxs[ BGLSSL_TLSV1_2 ] = SSL_CTX_new( TLSv1_2_server_method() );
#else      
      ctxs[ BGLSSL_TLSV1_2 ] = ctxs[ BGLSSL_TLSV1 ];
#endif
#if( BGLSSL_HAVE_TLSV1_3 )
      ctxs[ BGLSSL_TLSV1_3 ] = SSL_CTX_new( TLSv1_3_server_method() );
#else      
      ctxs[ BGLSSL_TLSV1_3 ] = ctxs[ BGLSSL_TLSV1 ];
#endif
#if( BGLSSL_HAVE_DTLS )
      ctxs[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLSv1_server_method() );
#else      
      ctxs[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLS_server_method() );
#endif
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    bgl_ssl_version ...                                              */
/*---------------------------------------------------------------------*/
char *
bgl_ssl_version() {
   return (char *)SSLeay_version( SSLEAY_VERSION );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_cert ...                                                    */
/*---------------------------------------------------------------------*/
static void
free_cert( void* obj, void* cert ) {
   X509_free( bgl_certificate_native( cert ) );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    free_pkey ...                                                    */
/*---------------------------------------------------------------------*/
static void
free_pkey( void* obj, void* pkey ) {
   EVP_PKEY_free( bgl_private_key_native( pkey ) );
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    sslread ...                                                      */
/*---------------------------------------------------------------------*/
static long
sslread( obj_t port, char *ptr, long len ) {
   long r;
   obj_t drag = PORT( port ).userdata;
   SSL *ssl;

   BGL_MUTEX_LOCK( ssl_mutex );
   if( drag != BUNSPEC ) {
      SET_CAR( CDR( PORT( port ).userdata ), BINT( 1 ) );
      ssl = (SSL*)CAR( drag );
   }
   BGL_MUTEX_UNLOCK( ssl_mutex );
   
loop:
   if( (r = SSL_read( ssl, ptr, len )) <= 0 ) {
      if( r == 0 ) {
	 INPUT_PORT( port ).eof = 1;
      } else {
	 if( (SSL_get_error( ssl, r ) == SSL_ERROR_SSL) && (errno == EINTR) ) {
	    goto loop;
	 }
      }
   }

   BGL_MUTEX_LOCK( ssl_mutex );
   if( CAR( PORT( port ).userdata ) == BUNSPEC ) {
      SSL_free( ssl );
   } else {
      SET_CAR( CDR( PORT( port ).userdata ), BINT( 0 ) );
   }
   BGL_MUTEX_UNLOCK( ssl_mutex );

   return r;
}

/*---------------------------------------------------------------------*/
/*    ssize_t                                                          */
/*    sslwrite ...                                                     */
/*---------------------------------------------------------------------*/
ssize_t
sslwrite( obj_t port, char *ptr, long len ) {
   return SSL_write( (SSL *)PORT_CHANNEL( port ), ptr, len );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    socket_close_hook ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
socket_close_hook( obj_t env, obj_t s ) {
   obj_t drag = SOCKET( s ).userdata;
   SSL *ssl = (SSL *)CAR( drag );
 
   BGL_MUTEX_LOCK( ssl_mutex );
   
   SSL_shutdown( ssl );

   if( CAR( CDR( drag ) ) == BINT( 0 ) ) {
      /* in read, free must be delayed */
      SSL_free( ssl );
   }
   
   SOCKET( s ).userdata = BUNSPEC;

   BGL_MUTEX_UNLOCK( ssl_mutex );
   
   return s;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    input_close_hook ...                                             */
/*---------------------------------------------------------------------*/
static obj_t
input_close_hook( obj_t env, obj_t ip ) {
   fclose( PORT_FILE( ip ) );
   
   return ip;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    output_close_hook ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
output_close_hook( obj_t env, obj_t op ) {
   close( (int)(long)(PORT( op ).userdata) );
   
   return op;
}

/*---------------------------------------------------------------------*/
/*    char *                                                           */
/*    ssl_error_message ...                                            */
/*---------------------------------------------------------------------*/
static char *
ssl_error_message( char *buf ) {
   int errid = ERR_get_error();
   if( errid == SSL_ERROR_SYSCALL ) {
      return "unexpected EOF";
   } else {
      if( errid && (ERR_GET_LIB( errid ) == ERR_LIB_SYS) ) {
	 return "Cannot create SSL";
      } else {
	 memset( buf, 0, 121 );
	 ERR_error_string( errid, buf );
	 return buf; 
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    socket_enable_ssl ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
socket_enable_ssl( obj_t s, char accept, SSL_CTX *ctx, obj_t cert,
                   obj_t pkey, obj_t ca_list, obj_t accepted_certs ) {
   obj_t ip, op;
   SSL *ssl;
   BIO *sbio;
   int status;
   char verify = 0;
   char errbuf[ 121 ];
   obj_t drag = BNIL;

   bgl_ssl_init();

   BGL_MUTEX_LOCK( ssl_mutex );

   sbio = BIO_new_socket( SOCKET( s ).fd, BIO_NOCLOSE );
   if( !sbio ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"make-client-ssl-socket, cannot create BIO stream",
			ssl_error_message( errbuf ),
			s );
   }

   /* if we want a certain ca_list we have to duplicate the context first */
   if( ca_list != BNIL ) {
      STACK_OF( X509_NAME ) *cert_names;
      X509_STORE *store;

      /* we want to verify */
      verify = 1;
     
      /* keep the ca_list away from the GC */
      drag = MAKE_PAIR( ca_list, drag );

      ctx = SSL_CTX_new( BGL_SSL_CTX_get_ssl_method( ctx ) );
      if( ctx == NULL )
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "make-client-ssl-socket, cannot create SSL context",
			   ssl_error_message( errbuf ),
			   s );

      store = SSL_CTX_get_cert_store( ctx );
      if( store == NULL )
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "make-client-ssl-socket, cert store is NULL",
			   ssl_error_message( errbuf ),
			   ca_list );
     
      while( ca_list != BNIL ) {
	 X509 *c = bgl_certificate_native( CAR( ca_list ) );
	 /* add the CA certificate as trusted */
	 X509_STORE_add_cert( store, c );
	 /* and send its name to the client */
	 if( SSL_CTX_add_client_CA( ctx, c ) != 1 )
	    C_SYSTEM_FAILURE( BGL_IO_ERROR,
			      "make-client-ssl-socket, cannot use ca-list",
			      ssl_error_message( errbuf ),
			      ca_list );
	 ca_list = CDR( ca_list );
      }
   }

   if( !(ssl = SSL_new( ctx )) ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"make-client-ssl-socket, cannot create SSL",
			ssl_error_message( errbuf ),
			s );
   }

   SSL_set_bio( ssl, sbio, sbio );
   SSL_set_mode( ssl, SSL_MODE_AUTO_RETRY );
   SSL_set_options( ssl, SSL_OP_ALL );

   if( cert != BFALSE ) {
      /* keep the cert/pkey away from the GC */
      drag = MAKE_PAIR( cert, drag );
      drag = MAKE_PAIR( pkey, drag );

      if( SSL_use_certificate( ssl, bgl_certificate_native( cert ) ) != 1 )
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "make-client-ssl-socket, cannot use certificate",
			   ssl_error_message( errbuf ),
			   cert );
     
      if( SSL_use_PrivateKey( ssl, bgl_private_key_native( pkey ) ) != 1 )
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "make-client-ssl-socket, cannot use private key",
			   ssl_error_message( errbuf ),
			   pkey );
     
      if( SSL_check_private_key( ssl ) != 1 )
	 C_SYSTEM_FAILURE(
	    BGL_IO_ERROR,
	    "make-client-ssl-socket, private key doesn't match certificate",
	    ssl_error_message( errbuf ),
	    pkey );
   }
   
   /* we want to verify the peer certificate if we have a list of CA */
   if( verify ) {
      SSL_set_verify( ssl,
		      SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT,
		      NULL );
   } else {
      SSL_set_verify( ssl, ( SSL_VERIFY_NONE ), NULL );
      SSL_set_verify_depth( ssl, 0 );
   }
   /* FIXME: is it possible we want a list of accepted certificates */
   /* we don't want to verify ? if yes how do we ask for them ?     */ 
   
   BGL_MUTEX_UNLOCK( ssl_mutex );

   if( (status = (accept ? SSL_accept( ssl ) : SSL_connect( ssl ))) <= 0 ) {
      int err = SSL_get_error( ssl, status );
      if( err == SSL_ERROR_SSL ) err = ERR_get_error();

      BIO_free( sbio );
      socket_close( s );
      
      if( err == SSL_ERROR_SYSCALL ) {
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "make-client-ssl-socket",
			   "cannot accept: unexpected EOF",
			   s );
      } else {
	 if( err && (ERR_GET_LIB( err ) == ERR_LIB_SYS ) ) {
	    C_SYSTEM_FAILURE( BGL_IO_ERROR,
			      "make-client-ssl-socket",
			      "cannot accept",
			      s );
	 } else {
	    memset( errbuf, 0, 121 );
	    ERR_error_string( err, errbuf );
	    C_SYSTEM_FAILURE( BGL_IO_ERROR,
			      "make-client-ssl-socket",
			      errbuf,
			      s );
	 }
      }

      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"make-client-ssl-socket",
			"cannot accept: bad status",
			s );
   }

   /* now check the peer certificate if need be */
   if( accepted_certs != BFALSE ) {
      /* cannot be null if we verified, but it's a bit fuzzy so we check */
      X509 *cert = SSL_get_peer_certificate( ssl );
      char success = 0;

      /* keep the certs away from the GC */
      drag = MAKE_PAIR( accepted_certs, drag );

      if( cert == NULL )
	 C_SYSTEM_FAILURE(
	    BGL_IO_ERROR,
	    "make-client-ssl-socket, failed to get a client cert",
	    ssl_error_message( errbuf ),
	    s );
      /* we assume that accepted_certs is a list of X509* certificates */
      while( accepted_certs != BNIL ) {
	 X509 *tcert = bgl_certificate_native( CAR( accepted_certs ) );
	 if( X509_cmp( tcert, cert ) == 0 ) {
	    success = 1;
	    break;
	 }
	 accepted_certs = CDR( accepted_certs );
      }
      
      if( success == 0 ) {
	 char buf[ 100 ];
	 char *subject;
	 /* subject */
	 if( X509_NAME_get_text_by_NID( X509_get_subject_name( cert ),
					NID_commonName, buf, 100 ) >= 0 ) {
	    subject = buf;
	 } else {
	    subject = "<no info>";
	 }
	 C_SYSTEM_FAILURE(
	    BGL_IO_ERROR,
	    "make-client-ssl-socket, presented certificate is not acceptable",
	    subject,
	    s );
      }
      X509_free( cert );
   }
   
   ip = SOCKET_INPUT( s );
   op = SOCKET_OUTPUT( s );
   
   /* drag whatever is necessary for the GC */
   drag = MAKE_PAIR( BINT( 0 ), drag );
   drag = MAKE_PAIR( (obj_t)ssl, drag );

   PORT( ip ).userdata = drag;
   PORT( ip ).chook = ssl_input_close_hook;
   PORT( ip ).sysclose = 0L;
   INPUT_PORT( ip ).sysread = &sslread;
   
   PORT( op ).userdata = (void *)(long)PORT_FD( op );
   PORT( op ).stream.channel = (obj_t)ssl;
   OUTPUT_PORT( op ).stream_type = BGL_STREAM_TYPE_CHANNEL;
   PORT( op ).sysclose = 0L;
   PORT( op ).chook = ssl_output_close_hook;
   OUTPUT_PORT( op ).syswrite = &sslwrite;
   OUTPUT_PORT( op ).sysflush = 0L;

   SOCKET( s ).userdata = drag;
   SOCKET_CHOOK( s ) = ssl_socket_close_hook;
   
   return s;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_client_socket_use_ssl ...                                    */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_client_socket_use_ssl( obj_t socket, 
			   int protocol, obj_t cert, obj_t pkey,
			   obj_t ca_list, obj_t accepted_certs ) {
   bgl_ssl_init();

   return socket_enable_ssl( socket, 0, ctxc[ protocol ], cert, pkey,
                             ca_list, accepted_certs );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_make_ssl_client_socket ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_ssl_client_socket( obj_t hostname, int port, int ms, 
                            int protocol, obj_t cert, obj_t pkey,
                            obj_t ca_list, obj_t accepted_certs,
			    obj_t inbuf, obj_t outbuf ) {
   obj_t sock = bgl_make_client_socket( hostname, port, ms, inbuf, outbuf );
   
   return bgl_client_socket_use_ssl( sock,
				     protocol, cert, pkey,
				     ca_list, accepted_certs );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    socket_server_enable_ssl ...                                     */
/*---------------------------------------------------------------------*/
static obj_t
socket_server_enable_ssl( obj_t serv, obj_t s ) {
   obj_t data = SOCKET( serv ).userdata;
   obj_t pkey, cert, ca_list, proto, accepted_certs;

   /* data is (proto certfile pkeyfile cafile acceptedcerts) */
   bgl_ssl_init();
   proto = CAR( data );
   data = CDR( data );

   cert = CAR( data );
   data = CDR( data );

   pkey = CAR( data );
   data = CDR( data );

   ca_list = CAR( data );
   data = CDR( data );

   accepted_certs = CAR( data );

   return socket_enable_ssl( s, 1, ctxs[ CINT( proto ) ], 
                             cert, pkey, 
                             ca_list, accepted_certs );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    make_ssl_server_socket ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_make_ssl_server_socket( obj_t hostname, int port, int protocol, 
                            obj_t cert, obj_t pkey, obj_t ca_list,
                            obj_t accepted_certs, int backlog, bool_t ipv6 ) {
   obj_t serv = bgl_make_server_socket( hostname, port, backlog, ipv6 );
   obj_t data = BNIL;
   /* data is (proto cert pkey ca_list acceptedcerts) */
   data = MAKE_PAIR( accepted_certs, data );
   data = MAKE_PAIR( ca_list, data );
   data = MAKE_PAIR( pkey, data );
   data = MAKE_PAIR( cert, data );
   data = MAKE_PAIR( BINT( protocol ), data );

   SOCKET( serv ).accept = socket_server_enable_ssl;
   SOCKET( serv ).userdata = data;
   return serv;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_ssl_socketp ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_socketp( obj_t o ) {
   return (SOCKETP( o )
	   && ((SOCKET_CHOOK( o ) == ssl_socket_close_hook)
	       || (SOCKET( o ).accept == socket_server_enable_ssl)));
}

#include "ssl_debug.h"

/*---------------------------------------------------------------------*/
/*    static BIO *                                                     */
/*    bgl_load_bio ...                                                 */
/*---------------------------------------------------------------------*/
static BIO *
bgl_load_bio( obj_t cert, long offset, long len ) {
   BIO *bio = BIO_new( BIO_s_mem() );

#if( SSL_DEBUG )
   int l = len > 80 ? 80 : len;
   char s[ l + 1 ];
   memcpy( s, &(STRING_REF( cert, offset )), l );
   s[ l ] = 0;
   
   fprintf( stderr, "%s,%d:LoadBIO [%d:%s]\n", __FILE__, __LINE__, len, s );
#endif
   if( !bio ) return 0L;

   if( BIO_write( bio, &STRING_REF( cert, offset ), len ) <= 0 ) {
      BIO_free( bio );
      return 0L;
   }
   return bio;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_load_private_key ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_load_private_key( obj_t file ) {
   FILE* fp;
   EVP_PKEY *pkey;
   obj_t ret;
   char errbuf[ 121 ];

   if( !(fp = fopen( BSTRING_TO_STRING( file ), "r" )) ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"read-private-key, failed to open private key file",
			strerror( errno ),
			file );
   }

   pkey = PEM_read_PrivateKey( fp, NULL, NULL, NULL );
   fclose( fp );

   if( pkey == NULL ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"read-private-key, failed to load private key",
			ssl_error_message( errbuf ),
			file );
   }

   ret = bgl_make_private_key( pkey );
   GC_register_finalizer( ret, free_pkey, ret, NULL, NULL );
   
   return ret;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_load_certificate ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_load_certificate( obj_t file ) {
   FILE* fp;
   X509 *cert;
   obj_t ret;
   char errbuf[ 121 ];

   if( !(fp = fopen( BSTRING_TO_STRING( file ), "r" )) ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"read-certificate, failed to open certificate file",
			strerror( errno ),
			file );
   }

   cert = PEM_read_X509( fp, NULL, NULL, NULL );
   fclose( fp );

   if( cert == NULL ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"read-certificate, failed to load certificate",
			ssl_error_message(errbuf),
			file );
   }
   
   ret = bgl_make_certificate( cert );
   GC_register_finalizer( ret, free_cert, ret, NULL, NULL );
   
   return ret;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_load_pem ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_load_pem( obj_t file ) {
   STACK_OF(X509_INFO) *inf;
   X509_INFO *itmp;
   BIO *in;
   int i;
   obj_t ret = BNIL;
   char errbuf[ 121 ];

   if( !(in = BIO_new_file( BSTRING_TO_STRING( file ), "r" )) ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"read-pem, failed to open file",
			ssl_error_message( errbuf ),
			file );
   }

   inf = PEM_X509_INFO_read_bio( in, NULL, NULL, NULL );
   BIO_free( in );
   if( !inf ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"read-pem, failed to load file",
			ssl_error_message( errbuf ),
			file );
   }
   
   for( i = 0; i < sk_X509_INFO_num( inf ); i++ ) {
      itmp = sk_X509_INFO_value( inf, i );
      if( itmp->x509 ) {
	 obj_t cert = bgl_make_certificate( itmp->x509 );
	 ret = MAKE_PAIR( cert, ret );
	 GC_register_finalizer( cert, free_cert, cert, NULL, NULL );
      }
      /* FIXME: when we support CRL we'll do that
	 if(itmp->crl) {
	 X509_STORE_add_crl(ctx->store_ctx, itmp->crl);
	 }*/
   }
   /*  sk_X509_INFO_pop_free(inf, X509_INFO_free); */
   
   return ret;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_certificate_subject ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_certificate_subject( obj_t bcert ) {
   X509* cert = bgl_certificate_native( bcert );
   char buf[ 255 ];
   
   if( X509_NAME_get_text_by_NID( X509_get_subject_name( cert ),
				  NID_commonName, buf, 255 ) < 0 ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"certificate-subject, could not read subject",
			"",
			bcert );
   }
   
   return string_to_bstring( buf );
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_certificate_issuer ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_certificate_issuer( obj_t bcert ) {
   X509* cert = bgl_certificate_native( bcert );
   char buf[ 255 ];
   
   if( X509_NAME_get_text_by_NID( X509_get_issuer_name( cert ),
				  NID_commonName, buf, 255) < 0) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"certificate-issuer, could not read issuer",
			"",
			bcert );
   }
   
   return string_to_bstring( buf );
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    SSL_CTX_use_certificate_chain ...                                */
/*---------------------------------------------------------------------*/
static int
SSL_CTX_use_certificate_chain( SSL_CTX *ctx, BIO *in ) {
   // Read a file that contains our certificate in "PEM" format,
   // possibly followed by a sequence of CA certificates that should be
   // sent to the peer in the Certificate message.
   //
   // Taken from OpenSSL - editted for style.
   int ret = 0;
   X509 *x = NULL;

   x = PEM_read_bio_X509_AUX( in, NULL, NULL, NULL );

   if( x == NULL ) {
      SSLerr( SSL_F_SSL_CTX_USE_CERTIFICATE_CHAIN_FILE, ERR_R_PEM_LIB );
      goto end;
   }

   ret = SSL_CTX_use_certificate( ctx, x );

   if( ERR_peek_error() != 0 ) {
      // Key/certificate mismatch doesn't imply ret==0 ...
      ret = 0;
   }

   if( ret ) {
      // If we could set up our certificate, now proceed to
      // the CA certificates.
      X509 *ca;
      int r;
      unsigned long err;

      // MS 28 Novembre 2016: WARNING !!!
      // before openssl 1.1, used to be
      // if( ctx->extra_certs != NULL ) {
      //   sk_X509_pop_free( ctx->extra_certs, X509_free );
      //   ctx->extra_certs = NULL;
      // }
      // see bglss.h for the definition of BGL_SSL_CTX_clear_extra_chain_certs
      BGL_SSL_CTX_clear_extra_chain_certs( ctx );

      while( (ca = PEM_read_bio_X509( in, NULL, NULL, NULL )) ) {
	 r = SSL_CTX_add_extra_chain_cert( ctx, ca );

	 if( !r ) {
	    X509_free( ca );
	    ret = 0;
	    goto end;
	 }
	 // Note that we must not free r if it was successfully
	 // added to the chain (while we must free the main
	 // certificate, since its reference count is increased
	 // by SSL_CTX_use_certificate).
      }

      // When the while loop ends, it's usually just EOF.
      err = ERR_peek_last_error();
      if( ERR_GET_LIB( err ) == ERR_LIB_PEM &&
	  ERR_GET_REASON( err ) == PEM_R_NO_START_LINE ) {
	 ERR_clear_error();
      } else  {
	 // some real error
	 ret = 0;
      }
   }

end:
   if( x != NULL ) X509_free( x );
   return ret;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_ssl_ctx_add_root_certs ...                                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_ctx_add_root_certs( secure_context sc ) {
   static X509_STORE *root_cert_store = 0L;

   if ( !root_cert_store ) {
      int i;
      BGL_MUTEX_LOCK( bigloo_mutex );
      
      root_cert_store = X509_STORE_new();
      
      for( i = 0; root_certs[ i ]; i++ ) {
	 BIO *bp = BIO_new( BIO_s_mem() );
	 X509 *x509;

	 if( !BIO_write( bp, root_certs[ i ], strlen( root_certs[ i ] ) ) ) {
	    BIO_free( bp );
	    BGL_MUTEX_UNLOCK( bigloo_mutex );
	    return 0;
	 }

	  x509 = PEM_read_bio_X509( bp, NULL, NULL, NULL );

	  if( x509 == NULL ) {
	     BIO_free( bp );
	     BGL_MUTEX_UNLOCK( bigloo_mutex );
	     return 0;
	  }

	  X509_STORE_add_cert( root_cert_store, x509 );

	  BIO_free( bp );
	  X509_free( x509 );
      }
      BGL_MUTEX_UNLOCK( bigloo_mutex );
   }

   CSC( sc )->BgL_z42cazd2storez90 = root_cert_store;
   SSL_CTX_set_cert_store( CSC( sc )->BgL_z42nativez42, root_cert_store );
   
   return 1;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_ssl_ctx_add_ca_cert ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_ctx_add_ca_cert( secure_context sc, obj_t cert, long offset, long len ) {
   char newCAStore = 0;
   BIO *bio;
   X509 *x509;
   
   if( CSC( sc )->BgL_z42cazd2storez90 == 0L ) {
      CSC( sc )->BgL_z42cazd2storez90 = X509_STORE_new();
      newCAStore = 1;
   }
   
   bio = bgl_load_bio( cert, offset, len );

   if( !bio ) {
      return 0;
   }
   
   x509 = PEM_read_bio_X509( bio, NULL, NULL, NULL );
   BIO_free( bio );
   
   if( !x509) {
      return 0;
   }

   X509_STORE_add_cert( CSC( sc )->BgL_z42cazd2storez90, x509 );
   SSL_CTX_add_client_CA( CSC( sc )->BgL_z42nativez42, x509 );

   X509_free( x509 );

   if( newCAStore ) {
      SSL_CTX_set_cert_store( CSC( sc )->BgL_z42nativez42,
			      CSC( sc )->BgL_z42cazd2storez90 );
   }

   return 1;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_ssl_ctx_add_crl ...                                          */
/*---------------------------------------------------------------------*/
bool_t
bgl_ssl_ctx_add_crl( secure_context sc, obj_t cert, long offset, long len ) {
   X509_CRL *x509;
   BIO *bio = bgl_load_bio( cert, offset, len );

   if( !bio ) return 0;
   
   x509 = PEM_read_bio_X509_CRL( bio, NULL, NULL, NULL );
   
   if( x509 == NULL ) {
      BIO_free( bio );
      return 0;
  }

   X509_STORE_add_crl( CSC( sc )->BgL_z42cazd2storez90, x509 );

   X509_STORE_set_flags( CSC( sc )->BgL_z42cazd2storez90,
			 X509_V_FLAG_CRL_CHECK | X509_V_FLAG_CRL_CHECK_ALL );

  BIO_free( bio );
  X509_CRL_free( x509 );

  return 1;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_tlsext_servername_callback ...                               */
/*---------------------------------------------------------------------*/
static int
bgl_tlsext_servername_callback( SSL *ssl, int *ad, void* arg ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));
   const char *servername = SSL_get_servername( ssl, TLSEXT_NAMETYPE_host_name );

   if( servername ) {
      CCON( c )->BgL_serverzd2namezd2 = string_to_bstring( (char *)servername );
   }

   return SSL_TLSEXT_ERR_OK;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_advertise_next_proto_callback ...                            */
/*---------------------------------------------------------------------*/
static int
bgl_advertise_next_proto_callback( SSL *ssl,
				   const unsigned char** data,
				   unsigned int *len,
				   void *arg ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));

   if( !STRINGP( CCON( c )->BgL_npnzd2protoszd2 ) ) {
      // No initialization - no NPN protocols
      *data = "";
      *len = 0;
   } else {
      *data = BSTRING_TO_STRING( CCON( c )->BgL_npnzd2protoszd2 );
      *len = STRING_LENGTH( CCON( c )->BgL_npnzd2protoszd2 );
   }

   return SSL_TLSEXT_ERR_OK;
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_select_next_proto_callback ...                               */
/*---------------------------------------------------------------------*/
static int
bgl_select_next_proto_callback( SSL *ssl,
				unsigned char **out, unsigned char *outlen,
				const unsigned char *in,
				unsigned int inlen, void *arg ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));

   if( !STRINGP( CCON( c )->BgL_npnzd2protoszd2 ) ) {
      *out = "http/1.1";
      *outlen = 8;
      CCON( c )->BgL_selectedzd2npnzd2protosz00 = BFALSE;
      return SSL_TLSEXT_ERR_OK;
   } else {
      int status = SSL_select_next_proto
	 ( out, outlen, in, inlen,
	   BSTRING_TO_STRING( CCON( c )->BgL_npnzd2protoszd2 ),
	   STRING_LENGTH( CCON( c )->BgL_npnzd2protoszd2 ) );

      switch( status ) {
	 case OPENSSL_NPN_UNSUPPORTED:
	    CCON( c )->BgL_selectedzd2npnzd2protosz00 = BUNSPEC;
	    break;
	 case OPENSSL_NPN_NEGOTIATED:
	    CCON( c )->BgL_selectedzd2npnzd2protosz00 =
	       string_to_bstring_len( *out, *outlen );
	    break;
	 case OPENSSL_NPN_NO_OVERLAP:
	    CCON( c )->BgL_selectedzd2npnzd2protosz00 = BFALSE;
	    break;
	 default:
	    break;
      }

      return SSL_TLSEXT_ERR_OK;
   }
}
   
/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_init_npm ...                                                 */
/*---------------------------------------------------------------------*/
static void
bgl_init_npm( secure_context sc, int is_server ) {
#if( SSL_DEBUG )
   fprintf( stderr, "%s,%d:init_npm %d\n", __FILE__, __LINE__, is_server );
#endif   
   if( is_server ) {
      // Server should advertise NPN protocols
      SSL_CTX_set_next_protos_advertised_cb(
	 CSC( sc )->BgL_z42nativez42, bgl_advertise_next_proto_callback, NULL );
   } else {
      // Client should select protocol from advertised
      // If server supports NPN
      SSL_CTX_set_next_proto_select_cb(
	 CSC( sc )->BgL_z42nativez42, bgl_select_next_proto_callback, NULL );
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_info_callback ...                                            */
/*---------------------------------------------------------------------*/
static void
bgl_info_callback( const SSL *ssl, int where, int ret ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));
   obj_t cb = CCON( c )->BgL_infozd2callbackzd2;

   if( PROCEDUREP( cb ) ) {
      if( where & SSL_CB_HANDSHAKE_START ) {
	 PROCEDURE_ENTRY( cb )( cb, BINT( 0 ), BEOA );
      }
      if( where & SSL_CB_HANDSHAKE_DONE ) {
	 PROCEDURE_ENTRY( cb )( cb, BINT( 1 ), BEOA );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_select_sni_context_callback ...                              */
/*---------------------------------------------------------------------*/
static int
bgl_select_sni_context_callback( SSL *ssl, int *ad, void* arg ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));
#if( SSL_DEBUG )
   fprintf( stderr, "%s,%d:SelectSNIContextCallback\n", __FILE__, __LINE__ );
#endif   
   const char* servername = SSL_get_servername( ssl, TLSEXT_NAMETYPE_host_name );

   if( servername ) {
      obj_t proc = CCON( c )->BgL_snizd2contextzd2callbackz00;
      obj_t bsrv = string_to_bstring( (char *)servername );

      CCON( c )->BgL_serverzd2namezd2 = bsrv;
      
      // Call the SNI callback and use its return value as context
      if( PROCEDUREP( proc ) ) {
	 if( !PROCEDURE_CORRECT_ARITYP( proc, 2 ) ) {
	    C_SYSTEM_FAILURE( BGL_TYPE_ERROR, "ssl-connection",
			      "wrong callback arity", proc );
	 } else {
	    obj_t ret = PROCEDURE_ENTRY( proc )( proc, c, bsrv, BEOA );

	    if( ret != BFALSE ) {
	       secure_context sc = (secure_context)ret;

	       bgl_init_npm( sc, 1 );
	       SSL_set_SSL_CTX( ssl, CSC( sc )->BgL_z42nativez42 );
	    } else {
	       return SSL_TLSEXT_ERR_NOACK;
	    }
	 }
      }
   }

   return SSL_TLSEXT_ERR_OK;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_verify_callback ...                                          */
/*---------------------------------------------------------------------*/
static int
bgl_verify_callback( int preverify_ok, X509_STORE_CTX *ctx ) {
   // Quoting SSL_set_verify(3ssl):
   //
   //   The VerifyCallback function is used to control the behaviour when
   //   the SSL_VERIFY_PEER flag is set. It must be supplied by the
   //   application and receives two arguments: preverify_ok indicates,
   //   whether the verification of the certificate in question was passed
   //   (preverify_ok=1) or not (preverify_ok=0). x509_ctx is a pointer to
   //   the complete context used for the certificate chain verification.
   //
   //   The certificate chain is checked starting with the deepest nesting
   //   level (the root CA certificate) and worked upward to the peer's
   //   certificate.  At each level signatures and issuer attributes are
   //   checked.  Whenever a verification error is found, the error number is
   //   stored in x509_ctx and VerifyCallback is called with preverify_ok=0.
   //   By applying X509_CTX_store_* functions VerifyCallback can locate the
   //   certificate in question and perform additional steps (see EXAMPLES).
   //   If no error is found for a certificate, VerifyCallback is called
   //   with preverify_ok=1 before advancing to the next level.
   //
   //   The return value of VerifyCallback controls the strategy of the
   //   further verification process. If VerifyCallback returns 0, the
   //   verification process is immediately stopped with "verification
   //   failed" state. If SSL_VERIFY_PEER is set, a verification failure
   //   alert is sent to the peer and the TLS/SSL handshake is terminated. If
   //   VerifyCallback returns 1, the verification process is continued. If
   //   VerifyCallback always returns 1, the TLS/SSL handshake will not be
   //   terminated with respect to verification failures and the connection
   //   will be established. The calling process can however retrieve the
   //   error code of the last verification error using
   //   SSL_get_verify_result(3) or by maintaining its own error storage
   //   managed by VerifyCallback.
   //
   //   If no VerifyCallback is specified, the default callback will be
   //   used.  Its return value is identical to preverify_ok, so that any
   //   verification failure will lead to a termination of the TLS/SSL
   //   handshake with an alert message, if SSL_VERIFY_PEER is set.
   //
   // Since we cannot perform I/O quickly enough in this callback, we ignore
   // all preverify_ok errors and let the handshake continue. It is
   // imparative that the user use Connection::VerifyError after the
   // 'secure' callback has been made.
   return 1;
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_connection_init ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_init( ssl_connection ssl) {
   secure_context bctx = CCON( ssl )->BgL_ctxz00;
   int verify_mode;
   SSL *_ssl = SSL_new( CSC( bctx )->BgL_z42nativez42 );
   long mode;

   CCON( ssl )->BgL_z42nativez42 = _ssl;
   CCON( ssl )->BgL_z42biozd2readz90 = BIO_new( BIO_s_mem() );
   CCON( ssl )->BgL_z42biozd2writez90 = BIO_new( BIO_s_mem() );

   SSL_set_app_data( _ssl, ssl );

   if( CCON( ssl )->BgL_isserverz00 ) {
      SSL_set_info_callback( _ssl, bgl_info_callback );
   }

   bgl_init_npm( bctx, CCON( ssl )->BgL_isserverz00 );
   
   if( CCON( ssl )->BgL_isserverz00 ) {
      SSL_CTX_set_tlsext_servername_callback( CSC( bctx )->BgL_z42nativez42,
					      bgl_select_sni_context_callback );
   } else {
      if( STRINGP( CCON( ssl )->BgL_serverzd2namezd2 ) ) {
#if( SSL_DEBUG )
	 fprintf( stderr, "%s,%d SSL_set_tlsext_host_name %s\n",
		  __FILE__, __LINE__, BSTRING_TO_STRING( CCON( ssl )->BgL_serverzd2namezd2 ) );
#endif	 
	 SSL_set_tlsext_host_name( _ssl, BSTRING_TO_STRING( CCON( ssl )->BgL_serverzd2namezd2 ) );
      }
   }

   SSL_set_bio( _ssl,
		CCON( ssl )->BgL_z42biozd2readz90,
		CCON( ssl )->BgL_z42biozd2writez90 );

#if( defined( SSL_MODE_RELEASE_BUFFERS ) )
   mode = SSL_get_mode( _ssl );
   SSL_set_mode( _ssl, mode | SSL_MODE_RELEASE_BUFFERS );
#endif
   
   if( CCON( ssl )->BgL_isserverz00 ) {
      if( !(CCON( ssl )->BgL_requestzd2certzd2) ) {
	 verify_mode = SSL_VERIFY_NONE;
      } else {
	 verify_mode = SSL_VERIFY_PEER;
	 if( CCON( ssl )->BgL_rejectzd2unauthoriza7edz75 ) {
	    verify_mode |= SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
	 }
      }
   } else {
      verify_mode = SSL_VERIFY_NONE;
   }

   SSL_set_verify( _ssl, verify_mode, bgl_verify_callback );
   
   if( CCON( ssl )->BgL_isserverz00 ) {
      SSL_set_accept_state( _ssl );
   } else {
      SSL_set_connect_state( _ssl );
   }

   return (obj_t)ssl;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    handle_bio_error ...                                             */
/*---------------------------------------------------------------------*/
static void
handle_bio_error( ssl_connection ssl, BIO *bio, int n, char *fun ) {
   int retry = BIO_should_retry( bio );
   (void) retry; // unused if !defined(SSL_PRINT_DEBUG)

   if( BIO_should_write( bio ) ) {
      return;
   } else if( BIO_should_read( bio ) ) {
      return;
   } else {
      static char ssl_error_buf[ 512 ];
      ERR_error_string_n( n, ssl_error_buf, sizeof( ssl_error_buf ) );

      CCON( ssl )->BgL_errz00 = string_to_bstring( ssl_error_buf );

      return;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    handle_ssl_error ...                                             */
/*---------------------------------------------------------------------*/
static void
handle_ssl_error( ssl_connection ssl, int n, char *fun, int ignsys ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   int err = SSL_get_error( _ssl, n );
   int res = 0;

   if( err == SSL_ERROR_NONE ) {
      goto ret;
   } else if( err == SSL_ERROR_WANT_WRITE ) {
      goto ret;
   } else if( err == SSL_ERROR_WANT_READ ) {
      goto ret;
   } else if( err == SSL_ERROR_ZERO_RETURN ) {
      CCON( ssl )->BgL_errz00 = string_to_bstring( "ZERO_RETURN" );
      res = n;
      goto ret;
   } else if( (err == SSL_ERROR_SYSCALL) && ignsys ) {
      goto ret;
   } else {
      BUF_MEM* mem;
      BIO *bio;
      
      if( (bio = BIO_new( BIO_s_mem() )) ) {
	 ERR_print_errors( bio );
	 BIO_get_mem_ptr( bio, &mem );
	 CCON( ssl )->BgL_errz00 =
	    string_to_bstring_len( mem->data, mem->length );
	 BIO_free( bio );
      }

      res = n;
      goto ret;
   }

ret:
   ERR_clear_error();
   return;
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    set_shutdown_flags ...                                           */
/*---------------------------------------------------------------------*/
static void
set_shutdown_flags( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   int flags = SSL_get_shutdown( _ssl );

   if( flags & SSL_SENT_SHUTDOWN ) {
      CCON( ssl )->BgL_sentzd2shutdownzd2 = 1;
   }

   if( flags & SSL_RECEIVED_SHUTDOWN ) {
      CCON( ssl )->BgL_receivedzd2shutdownzd2 = 1;
   }
}

#if( SSL_DEBUG )
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    set_shutdown_flags ...                                           */
/*---------------------------------------------------------------------*/
static void
set_shutdown_flags2( char *name, ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   int flags = SSL_get_shutdown2( name, _ssl );

   if( flags & SSL_SENT_SHUTDOWN ) {
      CCON( ssl )->BgL_sentzd2shutdownzd2 = 1;
   }

   if( flags & SSL_RECEIVED_SHUTDOWN ) {
      CCON( ssl )->BgL_receivedzd2shutdownzd2 = 1;
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_ssl_connection_start ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_start( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   int n;

   if( !SSL_is_init_finished( _ssl ) ) {
      if( CCON( ssl )->BgL_isserverz00 ) {
	 if( (n = SSL_accept( _ssl )) <= 0 ) {
	    handle_ssl_error( ssl, n, "ssl-connection-start", 0 );
	 }
      } else {
#if( SSL_DEBUG )	 
	 if( (n = SSL_connect2( "start", _ssl )) <= 0 )
#else	    
	 if( (n = SSL_connect( _ssl )) <= 0 )
#endif
	 {
	    handle_ssl_error( ssl, n, "ssl-connection-start", 0 );
	 }
      }

      return n;
   } else {
      return 0;
   }
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_ssl_connection_close ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_close( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;

   SSL_free( _ssl );
   return 0;
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_ssl_connection_shutdown ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_shutdown( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   int rv;
   
   if( !_ssl ) return BFALSE;

   if( (rv = SSL_shutdown( _ssl )) < 0 ) {
      handle_ssl_error( ssl, rv, "ssl-connection-shutdown", 1 );
   }

#if( SSL_DEBUG )    
   set_shutdown_flags2( "shutdown", ssl );
#else
   set_shutdown_flags( ssl );
#endif

   return BINT( rv );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_ssl_connection_read ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_ssl_connection_read( ssl_connection ssl, char *buf, long off, long len ) {
   long int n = BIO_read( CCON( ssl )->BgL_z42biozd2writez90, buf + off, len );

   if( n < 0 ) {
      handle_bio_error( ssl, CCON( ssl )->BgL_z42biozd2writez90, n, "connection_read" );
   }
#if( SSL_DEBUG )	 
   else {
      int bytes_read = n;
      if( bytes_read >= 0 ) {
#if( SSL_DEBUG >=2 )	 
	 int i;
	 int bn = bytes_read > 80 ? 80 : bytes_read;
	 for( i = 0; i < bn; i++ ) {
	    fprintf( stderr, "%02x ", ((unsigned char *)(buf))[ off + i ] );
	 }

	 fprintf( stderr, "\n" );
#endif	 
#if( SSL_DEBUG >=3 )	 
	 for( i = 0; i < bn; i++ ) {
	    unsigned char c = ((unsigned char *)(buf))[ off + i ];
	    if( c <= 127 && c >= 20 ) {
	       fprintf( stderr, "%c", c );
	    } else {
	       fprintf( stderr, "." );
	    }
	 }

	 fprintf( stderr, "\n" );
#endif	 
      }
   }
   
#endif   
#if( SSL_DEBUG )    
   set_shutdown_flags2( "connection_read", ssl );
#else
   set_shutdown_flags( ssl );
#endif
   
   return n;
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_ssl_connection_write ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_ssl_connection_write( ssl_connection ssl, char *buf, long off, long len ) {
   long int n = BIO_write( CCON( ssl )->BgL_z42biozd2readz90, buf + off, len );

   if( n < 0 ) {
      handle_bio_error( ssl, CCON( ssl )->BgL_z42biozd2readz90, n, "connection_write" );
   }

#if( SSL_DEBUG )    
   set_shutdown_flags2( "connection_write", ssl );
#else
   set_shutdown_flags( ssl );
#endif
   
   return n;
}

/*---------------------------------------------------------------------*/
/*    static long                                                      */
/*    bgl_ssl_connection_clear ...                                     */
/*---------------------------------------------------------------------*/
static long
bgl_ssl_connection_clear( ssl_connection ssl, char *buf, long off, long len,
			  int (*SSL_fun)( SSL *, void *, int ),
			  char *name ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   long n;

   if( !SSL_is_init_finished( _ssl ) ) {
      if( CCON( ssl )->BgL_isserverz00 ) {
	 long m;
	 if( (m = SSL_accept( _ssl )) <= 0 ) {
	    handle_ssl_error( ssl, m, "ssl-connection-clear (accept)", 0 );
	    return m;
	 }
      } else {
	 int m;
#if( SSL_DEBUG ) 
	 if( (m = SSL_connect2( name, _ssl )) <= 0 )
#else	    
	 if( (m = SSL_connect( _ssl )) <= 0 )
#endif
	 {
	    handle_ssl_error( ssl, m, "ssl-connection-clear (connect)", 0 );
	    return m;
	 }
      }
   }

#if( defined( SSL_DEBUG) )   
   if( SSL_fun == &SSL_write ) {
      n = SSL_write( _ssl, buf + off, len );
   } else {
      fprintf( stderr, "%s,%d:~~~ Clear reading len bytes=%d\n",
	       __FILE__, __LINE__, len );
      n = SSL_read( _ssl, buf + off, len );
   }
#else   
   n = SSL_fun( _ssl, buf + off, len );
#endif

   if( n < 0 ) {
      handle_ssl_error( ssl, n, "ssl-connection-clear (read/write)", 0 );
   }
   
#if( defined( SSL_DEBUG) )   
   set_shutdown_flags2( name, ssl );
#else
   set_shutdown_flags( ssl );
#endif
   
   return n;
}
   
/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_ssl_connection_clear_in ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_ssl_connection_clear_in( ssl_connection ssl, char *buf, long off, long len ) {
#if( defined( SSL_DEBUG) )   
   {
      int l = len > 80 ? 80 : len;
      char s[ l + 1 ];
      strncpy( s, buf + off, l );
      s[ l ] = 0;
      
      fprintf( stderr, "%s:%d clearin [%s]\n", __FILE__, __LINE__, s );
   }
#endif
   
   return bgl_ssl_connection_clear( ssl, buf, off, len,
				    (int (*)( SSL *, void *, int))&SSL_write,
				    "connection-clear-in" );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_ssl_connection_clear_out ...                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_ssl_connection_clear_out( ssl_connection ssl, char *buf, long off, long len ) {
   return bgl_ssl_connection_clear( ssl, buf, off, len,
				    &SSL_read,
				    "connection-clear-out" );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_ssl_connection_init_finishedp ...                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_init_finishedp( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;

   return (_ssl != NULL && SSL_is_init_finished( _ssl ));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_ssl_connection_enc_pending ...                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_enc_pending( ssl_connection ssl ) {
   return BIO_pending( CCON( ssl )->BgL_z42biozd2writez90 );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_ssl_connection_clear_pending ...                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_clear_pending( ssl_connection ssl ) {
   return BIO_pending( CCON( ssl )->BgL_z42biozd2readz90 );
}

/*---------------------------------------------------------------------*/
/*    static SSL_SESSION *                                             */
/*    bgl_get_session_callback ...                                     */
/*---------------------------------------------------------------------*/
static SSL_SESSION *
bgl_get_session_callback( SSL *ssl, unsigned char *key, int len, int *copy ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));
   SSL_SESSION *sess = CCON( c )->BgL_z42nextzd2sessionz90;
   
   *copy = 0;

   CCON( c )->BgL_z42nextzd2sessionz90 = 0L;
   
   return sess;
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_new_session_callback ...                                     */
/*---------------------------------------------------------------------*/
static int
bgl_new_session_callback( SSL *ssl, SSL_SESSION *sess ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));
   int size = i2d_SSL_SESSION( sess, NULL );
   
   if( size > kMaxSessionSize ) {
      return 0;
   } else {
      obj_t serialized = make_string( size, 0 );
      unsigned char *pserialized = BSTRING_TO_STRING( serialized );
      obj_t cb = CCON( c )->BgL_newsessionzd2callbackzd2;
   
      i2d_SSL_SESSION( sess, &pserialized );

      if( !PROCEDURE_CORRECT_ARITYP( cb, 2 ) ) {
	 C_SYSTEM_FAILURE( BGL_TYPE_ERROR, "ssl-session",
			   "wrong callback arity", cb );
      } else {
	 unsigned int sidlen;
	 const char *sid = BGL_SSL_SESSION_get_id( sess, sidlen );

	 PROCEDURE_ENTRY( cb )
	 ( cb,
	   string_to_bstring_len( (char *)sid, sidlen ),
	   serialized,
	   BEOA );
	 return 0;
      }
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_connection_set_session ...                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_connection_set_session( ssl_connection ssl, obj_t buf ) {
   int wlen = STRING_LENGTH( buf );
   char *sbuf = BSTRING_TO_STRING( buf );
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   SSL_SESSION *sess;
   
   sess = d2i_SSL_SESSION( NULL, (const unsigned char **)&sbuf, wlen );

   if( !sess ) {
      return 0;
   } else {
      int r = SSL_set_session( _ssl, sess );
      SSL_SESSION_free( sess );

      if( !r ) {
	 char errbuf[ 121 ];
	 C_SYSTEM_FAILURE( BGL_IO_ERROR,
			   "SSL_set_session error",
			   ssl_error_message( errbuf ),
			   (obj_t)ssl );
      }

      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_connection_get_session ...                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_get_session( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   SSL_SESSION *sess;
   
   sess = SSL_get_session( _ssl );

   if( !sess ) {
      return BUNSPEC;
   } else {
      int slen = i2d_SSL_SESSION( sess, NULL );
      if( slen > 0 ) {
	 unsigned char *sbuf = alloca( slen + 1 );
	 unsigned char *p = sbuf;
	 
	 i2d_SSL_SESSION( sess, &p );

	 return string_to_bstring_len( sbuf, slen );
      }

      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_connection_get_current_cipher ...                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_get_current_cipher( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;

   if( !_ssl ) {
      return BUNSPEC;
   } else {
      SSL_CIPHER *c = (SSL_CIPHER *)SSL_get_current_cipher( _ssl );
      char *cipher_name = (char *)SSL_CIPHER_get_name( c );
      char *cipher_version = (char *)SSL_CIPHER_get_version( c );
      
      return MAKE_PAIR( string_to_bstring( cipher_name ),
			string_to_bstring( cipher_version ) );
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_connection_load_session ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_connection_load_session( ssl_connection ssl, obj_t buf ) {
   int wlen = STRING_LENGTH( buf );
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   SSL_SESSION *sess;
   char *sbuf = BSTRING_TO_STRING( buf );
   
   sess = d2i_SSL_SESSION( NULL, (const unsigned char **)&sbuf, wlen );

   if( CCON( ssl )->BgL_z42nextzd2sessionz90 != 0L ) {
      SSL_SESSION_free( CCON( ssl )->BgL_z42nextzd2sessionz90 );
   }

   CCON( ssl )->BgL_z42nextzd2sessionz90 = sess;

   return 1;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_connection_verify_error ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_verify_error( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   long x509_verify_error;
   
   if( !_ssl ) {
      return BUNSPEC;
   } else {
      X509* peer_cert = SSL_get_peer_certificate( _ssl );
      if( peer_cert == NULL ) {
	 // We requested a certificate and they did not send us one.
	 // Definitely an error.
	 // XXX is this the right error message?
	 return string_to_bstring( "UNABLE_TO_GET_ISSUER_CERT" );
      }
      X509_free( peer_cert );
   }
   
   x509_verify_error = SSL_get_verify_result( _ssl );

   switch ( x509_verify_error ) {
      case X509_V_OK:
	 return BUNSPEC;

      case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
	 return string_to_bstring( "UNABLE_TO_GET_ISSUER_CERT" );
	 break;

      case X509_V_ERR_UNABLE_TO_GET_CRL:
	 return string_to_bstring( "UNABLE_TO_GET_CRL" );
	 break;

      case X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
	 return string_to_bstring( "UNABLE_TO_DECRYPT_CERT_SIGNATURE" );
	 break;

      case X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE:
	 return string_to_bstring( "UNABLE_TO_DECRYPT_CRL_SIGNATURE" );
	 break;

      case X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
	 return string_to_bstring( "UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY" );
	 break;

      case X509_V_ERR_CERT_SIGNATURE_FAILURE:
	 return string_to_bstring( "CERT_SIGNATURE_FAILURE" );
	 break;

      case X509_V_ERR_CRL_SIGNATURE_FAILURE:
	 return string_to_bstring( "CRL_SIGNATURE_FAILURE" );
	 break;

      case X509_V_ERR_CERT_NOT_YET_VALID:
	 return string_to_bstring( "CERT_NOT_YET_VALID" );
	 break;

      case X509_V_ERR_CERT_HAS_EXPIRED:
	 return string_to_bstring( "CERT_HAS_EXPIRED" );
	 break;

      case X509_V_ERR_CRL_NOT_YET_VALID:
	 return string_to_bstring( "CRL_NOT_YET_VALID" );
	 break;

      case X509_V_ERR_CRL_HAS_EXPIRED:
	 return string_to_bstring( "CRL_HAS_EXPIRED" );
	 break;

      case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
	 return string_to_bstring( "ERROR_IN_CERT_NOT_BEFORE_FIELD" );
	 break;

      case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
	 return string_to_bstring( "ERROR_IN_CERT_NOT_AFTER_FIELD" );
	 break;

      case X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD:
	 return string_to_bstring( "ERROR_IN_CRL_LAST_UPDATE_FIELD" );
	 break;

      case X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD:
	 return string_to_bstring( "ERROR_IN_CRL_NEXT_UPDATE_FIELD" );
	 break;

      case X509_V_ERR_OUT_OF_MEM:
	 return string_to_bstring( "OUT_OF_MEM" );
	 break;

      case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
	 return string_to_bstring( "DEPTH_ZERO_SELF_SIGNED_CERT" );
	 break;

      case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
	 return string_to_bstring( "SELF_SIGNED_CERT_IN_CHAIN" );
	 break;

      case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
	 return string_to_bstring( "UNABLE_TO_GET_ISSUER_CERT_LOCALLY" );
	 break;

      case X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
	 return string_to_bstring( "UNABLE_TO_VERIFY_LEAF_SIGNATURE" );
	 break;

      case X509_V_ERR_CERT_CHAIN_TOO_LONG:
	 return string_to_bstring( "CERT_CHAIN_TOO_LONG" );
	 break;

      case X509_V_ERR_CERT_REVOKED:
	 return string_to_bstring( "CERT_REVOKED" );
	 break;

      case X509_V_ERR_INVALID_CA:
	 return string_to_bstring( "INVALID_CA" );
	 break;

      case X509_V_ERR_PATH_LENGTH_EXCEEDED:
	 return string_to_bstring( "PATH_LENGTH_EXCEEDED" );
	 break;

      case X509_V_ERR_INVALID_PURPOSE:
	 return string_to_bstring( "INVALID_PURPOSE" );
	 break;

      case X509_V_ERR_CERT_UNTRUSTED:
	 return string_to_bstring( "CERT_UNTRUSTED" );
	 break;

      case X509_V_ERR_CERT_REJECTED:
	 return string_to_bstring( "CERT_REJECTED" );
	 break;

      default:
	 return string_to_bstring(
	    (char *)X509_verify_cert_error_string( x509_verify_error ) );
	 break;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_ctx_set_key ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_ctx_set_key( secure_context sc, obj_t cert, long offset, long len, obj_t passphrase ) {
   
#if( SSL_DEBUG )
   fprintf( stderr, "%s,%d:SetKey\n", __FILE__, __LINE__ );
#endif
   {
      BIO *bio = bgl_load_bio( cert, offset, len );

      if( !bio ) return 0;

      EVP_PKEY *key =  PEM_read_bio_PrivateKey(
	 bio, NULL, NULL,
	 STRINGP( passphrase ) ? BSTRING_TO_STRING( passphrase ) : NULL );

      if( !key ) {
	 char ebuf[ 121 ];
      
	 BIO_free( bio );
   
	 C_SYSTEM_FAILURE( BGL_IO_ERROR, "set-key",
			   ssl_error_message( ebuf ),
			   (obj_t)sc );
      }

      SSL_CTX_use_PrivateKey( CSC( sc )->BgL_z42nativez42, key );

      EVP_PKEY_free( key );

      BIO_free( bio );
   
      return 1;
   }
}
	 
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_ctx_set_cert ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_ctx_set_cert( secure_context sc, obj_t cert, long offset, long len) {
#if( SSL_DEBUG )
   fprintf( stderr, "%s,%d:setCert\n", __FILE__, __LINE__ );
#endif

   {
      int rv;
      int r;
      BIO *bio = bgl_load_bio( cert, offset, len );
	 
      if( !bio ) return 0;

      rv = SSL_CTX_use_certificate_chain( CSC( sc )->BgL_z42nativez42, bio );

      BIO_free( bio );

      if( !rv ) {
	 char ebuf[ 121 ];
	 C_SYSTEM_FAILURE( BGL_IO_ERROR, "set-key",
			   ssl_error_message( ebuf ),
			   (obj_t)sc );
      }

      return 1;
   }
}
	 
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_ctx_set_session_id_context ...                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_ctx_set_session_id_context( secure_context sc, obj_t sic, long offset, long len ) {
   int r = SSL_CTX_set_session_id_context
      ( CSC( sc )->BgL_z42nativez42, &(STRING_REF( sic, offset)), len );

   if( r != 1 ) {
      BIO *bio;
      BUF_MEM *mem;
      char *msg;
	 
      if( (bio = BIO_new( BIO_s_mem() )) ) {
	 ERR_print_errors( bio );
	 BIO_get_mem_ptr( bio, &mem );
	    
	 msg = alloca( mem->length + 1 );
	 msg[ mem->length ] = 0;
	 memcpy( msg, mem->data, mem->length );

	 BIO_free( bio );
      } else {
	 msg = "error";
      }
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "set_session_id_context",
			msg, (obj_t)sc );
   }

   return 1;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_load_pkcs12 ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_load_pkcs12( secure_context sc, obj_t pfx, obj_t pass ) {
#if( SSL_DEBUG )
   fprintf( stderr, "%s,%d:LoadPKCS12\n", __FILE__, __LINE__ );
#endif

   {
      BIO *in = bgl_load_bio( pfx, 0, STRING_LENGTH( pfx ) );
      X509 *cert = NULL;
      PKCS12 *p12 = NULL;
      EVP_PKEY *pkey = NULL;
      STACK_OF(X509) *extraCerts = NULL;
      X509 *x509;
      char ret = 0;
      char *strpass = STRINGP( pass ) ? BSTRING_TO_STRING( pass ) : 0L;
      
      if( d2i_PKCS12_bio( in, &p12 ) &&
	  PKCS12_parse( p12, strpass, &pkey, &cert, &extraCerts ) &&
	  SSL_CTX_use_certificate( CSC( sc )->BgL_z42nativez42, cert ) &&
	  SSL_CTX_use_PrivateKey( CSC( sc )->BgL_z42nativez42, pkey ) ) {
	 // set extra certs
	 while( x509 = sk_X509_pop( extraCerts ) ) {
	    if( !(CSC( sc )->BgL_z42cazd2storez90 ) ) {
	       CSC( sc )->BgL_z42cazd2storez90 = X509_STORE_new();
	       SSL_CTX_set_cert_store( CSC( sc )->BgL_z42nativez42,
				       CSC( sc )->BgL_z42cazd2storez90 );
	    }
	    
	    X509_STORE_add_cert( CSC( sc )->BgL_z42cazd2storez90, x509 );
	    SSL_CTX_add_client_CA( CSC( sc )->BgL_z42nativez42, x509 );
	    X509_free( x509 );
	 }

	 EVP_PKEY_free( pkey );
	 X509_free( cert );
	 sk_X509_free( extraCerts );

	 ret = 0;
      }

      PKCS12_free( p12 );
      BIO_free( in );

      if( !ret ) {
	 unsigned long err = ERR_get_error();
	 char *str = (char *)ERR_reason_error_string( err );

	 C_SYSTEM_FAILURE( BGL_IO_ERROR, "load-pkcs12", str, (obj_t)sc );
      }
	 
      return 1;
   }
}
  
/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    cons ...                                                         */
/*---------------------------------------------------------------------*/
static obj_t
cons( char *sym, BUF_MEM *mem ) {
   obj_t key = string_to_symbol( sym );
   obj_t val = string_to_bstring_len( mem->data, mem->length );

   return MAKE_PAIR( key, val );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_connection_get_peer_certificate ...                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_get_peer_certificate( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;
   X509 *peer_cert = SSL_get_peer_certificate( _ssl );
   
   if( peer_cert ) {
      int index;
      unsigned int md_size, i;
      unsigned char md[ EVP_MAX_MD_SIZE ];
      
      BIO *bio = BIO_new( BIO_s_mem() );
      BUF_MEM *mem;
      obj_t info = BNIL;
      
      if( X509_NAME_print_ex(
	     bio, X509_get_subject_name(peer_cert), 0, X509_NAME_FLAGS ) > 0 ) {
	 BIO_get_mem_ptr( bio, &mem );
	 info = MAKE_PAIR( cons( "subject", mem ), info );
      }
      BIO_reset( bio );

      if( X509_NAME_print_ex(
	     bio, X509_get_issuer_name(peer_cert), 0, X509_NAME_FLAGS ) > 0 ) {
	 BIO_get_mem_ptr( bio, &mem );
	 info = MAKE_PAIR( cons( "issuer", mem ), info );
      }
      BIO_reset( bio );

      index = X509_get_ext_by_NID( peer_cert, NID_subject_alt_name, -1 );
      if( index >= 0 ) {
	 X509_EXTENSION *ext = X509_get_ext( peer_cert, index );
	 int rv = X509V3_EXT_print( bio, ext, 0, 0 );

	 BIO_get_mem_ptr( bio, &mem );
	 info = MAKE_PAIR( cons( "subjectaltname", mem ), info );

	 BIO_reset( bio );
      }

      EVP_PKEY *pkey = NULL;
      RSA *rsa = NULL;
      if( NULL != (pkey = X509_get_pubkey( peer_cert ))
	  && NULL != (rsa = EVP_PKEY_get1_RSA( pkey )) ) {
	 const BIGNUM *z;
	 BN_print( bio, BGL_RSA_N( rsa, z ) );
	 BIO_get_mem_ptr( bio, &mem );
	 info = MAKE_PAIR( cons( "modulus", mem ), info );
	 BIO_reset( bio );

	 BN_print( bio, BGL_RSA_E( rsa, z ) );
	 BIO_get_mem_ptr( bio, &mem );
	 info = MAKE_PAIR( cons( "exponent", mem ), info );
	 BIO_reset( bio );
      }

      if( pkey != NULL ) {
	 EVP_PKEY_free( pkey );
	 pkey = NULL;
      }
      if( rsa != NULL ) {
	 RSA_free( rsa );
	 rsa = NULL;
      }

      ASN1_TIME_print( bio, X509_get_notBefore( peer_cert )) ;
      BIO_get_mem_ptr( bio, &mem );

      info = MAKE_PAIR( cons( "valid-from", mem ), info );
      BIO_reset( bio );

      ASN1_TIME_print( bio, X509_get_notAfter( peer_cert ) );
      BIO_get_mem_ptr( bio, &mem );
      info = MAKE_PAIR( cons( "valid-to", mem ), info );
      BIO_free( bio );

      if( X509_digest( peer_cert, EVP_sha1(), md, &md_size ) ) {
	 const char hex[] = "0123456789ABCDEF";
	 char fingerprint[ EVP_MAX_MD_SIZE * 3 ];

	 for( i = 0; i<md_size; i++ ) {
	    fingerprint[ 3*i ] = hex[ (md[ i ] & 0xf0) >> 4 ];
	    fingerprint[ (3*i)+1 ] = hex[ (md[i] & 0x0f) ];
	    fingerprint[ (3*i)+2 ] = ':';
	 }

	 if( md_size > 0 ) {
	    fingerprint[ (3*(md_size-1))+2 ] = '\0';
	 } else {
	    fingerprint[ 0 ] = '\0';
	 }

	 info = MAKE_PAIR(
	    MAKE_PAIR(
	       string_to_symbol( "fingerprint" ),
	       string_to_bstring( fingerprint ) ),
	    info );
      }

      STACK_OF(ASN1_OBJECT) *eku =
	 (ASN1_OBJECT *)X509_get_ext_d2i( peer_cert, NID_ext_key_usage, NULL, NULL );
      if( eku != NULL ) {
	 char buf[ 256 ];
	 int len = sk_ASN1_OBJECT_num( eku );
	 obj_t ext_key_usage = create_vector( len );

	 for( i = 0; i < len; i++ ) {
	    memset( buf, 0, sizeof( buf ) );
	    OBJ_obj2txt( buf, sizeof(buf) - 1, sk_ASN1_OBJECT_value( eku, i ), 1 );
			
	    VECTOR_SET( ext_key_usage, i, string_to_bstring( buf ) );
	 }

	 sk_ASN1_OBJECT_pop_free( eku, ASN1_OBJECT_free );
	 info = MAKE_PAIR(
	    MAKE_PAIR(
	       string_to_symbol( "ext-key-usage" ),
	       ext_key_usage ),
	    info );
      }

      X509_free( peer_cert );
      return info;
   } else {
      return BUNSPEC;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_connection_get_negotiated_protocol ...                   */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_get_negotiated_protocol( ssl_connection ssl ) {
   SSL *_ssl = CCON( ssl )->BgL_z42nativez42;

   if( CCON( ssl )->BgL_isserverz00 ) {
      const unsigned char *npn_proto;
      unsigned int npn_proto_len;

      SSL_get0_next_proto_negotiated( _ssl, (&npn_proto), (&npn_proto_len) );

      {
	 char *s = alloca( npn_proto_len + 1 );
	 memcpy( s, npn_proto, npn_proto_len );
	 s[ npn_proto_len ] = 0;
      }
      if( !npn_proto ) {
	 return BFALSE;
      }

      return string_to_bstring_len( (char *)npn_proto, npn_proto_len );
   } else {
      return CCON( ssl )->BgL_selectedzd2npnzd2protosz00;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_ssl_ctx_init ...                                             */
/*    -------------------------------------------------------------    */
/*    Leave this function at the end of the file as it breaks emacs    */
/*    auto indentation.                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_ctx_init( secure_context sc ) {
   char *sslmethod = BSTRING_TO_STRING( CSC( sc )->BgL_methodz00 );
   const SSL_METHOD *method;
   
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );

   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif

   /* Nodejs compatibility */
   method = SSLv23_method();

   if( !strcmp( sslmethod, "default" ) ) {
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( method );
   } else if( !strcmp( sslmethod, "SSLv2_method" ) ) {
#if( BGLSSL_HAVE_SSLV2 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv2_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv2_server_method" ) ) {
#if( BGLSSL_HAVE_SSLV2 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv2_server_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv2_client_method" ) ) {
#if( BGLSSL_HAVE_SSLV2 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv2_client_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv3_method" ) ) {
#if( BGLSSL_HAVE_SSLV3 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv3_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv3_server_method" ) ) {
#if( BGLSSL_HAVE_SSLV3 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv3_server_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv3_client_method" ) ) {
#if( BGLSSL_HAVE_SSLV3 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv3_client_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv23_method" ) ) {
#if( BGLSSL_HAVE_SSLV23 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv23_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv23_server_method" ) ) {
#if( BGLSSL_HAVE_SSLV23 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv23_server_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv23_client_method" ) ) {
#if( BGLSSL_HAVE_SSLV23 )
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( SSLv23_client_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "TLSv1_method" ) ) {
#if( BGLSSL_HAVE_TLSV1 )      
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( TLSv1_method() );
#else      
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( TLS_method() );
#endif      
   } else if( !strcmp( sslmethod, "TLSv1_server_method" ) ) {
#if( BGLSSL_HAVE_TLSV1 )      
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( TLSv1_server_method() );
#else      
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( TLS_server_method() );
#endif      
   } else if( !strcmp( sslmethod, "TLSv1_client_method" ) ) {
#if( BGLSSL_HAVE_TLSV1 )      
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( TLSv1_client_method() );
#else      
      CSC( sc )->BgL_z42nativez42 = SSL_CTX_new( TLS_client_method() );
#endif      
   } else {
      goto unsupported;
   }

   // SSL session cache configuration
   if( !(CSC( sc )->BgL_z42nativez42) ) {
      char errbuf[ 121 ];
      
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "secure-context-init",
			ssl_error_message( errbuf ),
			(obj_t)sc );
      return (obj_t)sc;
   }
   
   SSL_CTX_set_session_cache_mode( CSC( sc )->BgL_z42nativez42,
				   SSL_SESS_CACHE_SERVER
				   | SSL_SESS_CACHE_NO_INTERNAL
				   | SSL_SESS_CACHE_NO_AUTO_CLEAR );
   
   SSL_CTX_sess_set_get_cb( CSC( sc )->BgL_z42nativez42, bgl_get_session_callback );
   SSL_CTX_sess_set_new_cb( CSC( sc )->BgL_z42nativez42, bgl_new_session_callback );

   return (obj_t)sc;
  
unsupported:
   C_SYSTEM_FAILURE( BGL_ERROR, "secure-context",
	 "method not supported", 
	 CSC( sc )->BgL_methodz00 );
   return (obj_t)sc;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_ctx_close ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_ctx_close( secure_context sc ) {
   SSL_CTX_free( CSC( sc )->BgL_z42nativez42 );
   return BNIL;
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_dh_check ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_dh_check( DH *dh ) {
   int codes;
   
   if( !DH_check( dh, &codes ) ) {
      return BTRUE;
   }
   if( codes & DH_CHECK_P_NOT_SAFE_PRIME ) {
      return string_to_symbol( "DH-CHECK-P-NOT-PRIME" );
   }
   if( codes & DH_CHECK_P_NOT_PRIME ) {
      return string_to_symbol( "DH-CHECK-P-NOT-PRIME" );
   }
   
   if( codes & DH_UNABLE_TO_CHECK_GENERATOR ) {
      return string_to_symbol( "DH-UNABLE-TO-CHECK-GENERATOR" );
   }
      
   if( codes & DH_NOT_SUITABLE_GENERATOR ) {
      return string_to_symbol( "DH_NOT_SUITABLE_GENERATOR" );
   }
   return BFALSE;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_dh_check_pub_key ...                                         */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_dh_check_pub_key( DH *dh, BIGNUM *key ) {
   int checked, checkr;

   checked = DH_check_pub_key( dh, key, &checkr );
   
   if( !checked ) {
      return BTRUE;
   }
   if( checkr ) {
      if( checkr & DH_CHECK_PUBKEY_TOO_SMALL ) {
	 return string_to_symbol( "DH-CHECK-PUBKEY-TOO-SMALL" );
      }
      if( checkr & DH_CHECK_PUBKEY_TOO_LARGE ) {
	 return string_to_symbol( "DH-CHECK-PUBKEY-TOO-LARGE" );
      }
      
      return string_to_symbol( "INVALID-KEY" );
   }
   return BFALSE;
}

/*---------------------------------------------------------------------*/
/*    BIGNUM *                                                         */
/*    bgl_bn_bin2bn ...                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BIGNUM *
bgl_bn_bin2bn( char *s, int len ) {
   return BN_bin2bn( s, len, 0 );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF void                                             */
/*    bgl_dh_pub_priv_key_set ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void
bgl_dh_pub_priv_key_set( DH *dh, BIGNUM *pub, BIGNUM *priv ) {
   if( pub != 0 && priv !=0 ) {
      BGL_DH_SET_PUB_PRIV( dh, pub, priv );
   }
}

/*---------------------------------------------------------------------*/
/*    BIGNUM *                                                         */
/*    bgl_dh_private_key ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BIGNUM *
bgl_dh_private_key( DH *dh ) {
   const BIGNUM *priv_key;
   return (BIGNUM *)BGL_DH_GET_PRIV( dh, priv_key );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_dh_private_key_set ...                                       */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void 
bgl_dh_private_key_set( DH *dh, BIGNUM *v ) {
   BGL_DH_SET_PRIV( dh, v );
}

/*---------------------------------------------------------------------*/
/*    BIGNUM *                                                         */
/*    bgl_dh_public_key ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BIGNUM *
bgl_dh_public_key( DH *dh ) {
   const BIGNUM *pub_key;
   return (BIGNUM *)BGL_DH_GET_PUB( dh, pub_key );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_dh_public_key_set ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void 
bgl_dh_public_key_set( DH *dh, BIGNUM *v ) {
   BGL_DH_SET_PUB( dh, v );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_dh_pqg_set ...                                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void 
bgl_dh_pqg_set( DH *dh, BIGNUM *p, BIGNUM *q, BIGNUM *g ) {
   if( p != 0 && g != 0 ) {
      BGL_DH_SET_PQG( dh, p, q, g );
   }
}

/*---------------------------------------------------------------------*/
/*    BIGNUM *                                                         */
/*    bgl_dh_p ...                                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BIGNUM *
bgl_dh_p( DH *dh ) {
   const BIGNUM *key;
   return (BIGNUM *)BGL_DH_GET_P( dh, key );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_dh_p_set ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void 
bgl_dh_p_set( DH *dh, BIGNUM *v ) {
   BGL_DH_SET_P( dh, v );
}

/*---------------------------------------------------------------------*/
/*    BIGNUM *                                                         */
/*    bgl_dh_q ...                                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BIGNUM *
bgl_dh_q( DH *dh ) {
   const BIGNUM *key;
   return (BIGNUM *)BGL_DH_GET_Q( dh, key );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_dh_q_set ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void 
bgl_dh_q_set( DH *dh, BIGNUM *v ) {
   BGL_DH_SET_Q( dh, v );
}

/*---------------------------------------------------------------------*/
/*    BIGNUM *                                                         */
/*    bgl_dh_g ...                                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF BIGNUM *
bgl_dh_g( DH *dh ) {
   const BIGNUM *key;
   return (BIGNUM *)BGL_DH_GET_G( dh, key );
}

/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    bgl_dh_g_set ...                                                 */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF void 
bgl_dh_g_set( DH *dh, BIGNUM *v ) {
   BGL_DH_SET_G( dh, v );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_get_ciphers ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_get_ciphers() {
   SSL_CTX* ctx;
   
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif

#if( BGLSSL_HAVE_TLSV1 )  
   ctx = SSL_CTX_new( TLSv1_server_method() );
#else   
   ctx = SSL_CTX_new( TLS_server_method() );
#endif   
   if( ctx == NULL ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"ssl-get-ciphers",
			"SSL_CTX_new() failed",
			BFALSE );
   }

   SSL *ssl = SSL_new( ctx );
   
   if( ssl == NULL ) {
      SSL_CTX_free( ctx );
      C_SYSTEM_FAILURE( BGL_IO_ERROR,
			"ssl-get-ciphers",
			"SSL_new() failed",
			BFALSE );
   } else {
      STACK_OF(SSL_CIPHER) *ciphers = SSL_get_ciphers( ssl );
      obj_t res;
      int i;

      res = create_vector( sk_SSL_CIPHER_num( ciphers ) );
      
      for( i = 0; i < sk_SSL_CIPHER_num( ciphers ); ++i ) {
	 const SSL_CIPHER *c = sk_SSL_CIPHER_value( ciphers, i );
	 VECTOR_SET( res, i, string_to_bstring( (char *)SSL_CIPHER_get_name( c ) ) );
      }

      SSL_free( ssl );
      SSL_CTX_free( ctx );

      return res;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    list_push ...                                                    */
/*---------------------------------------------------------------------*/
static void
list_push( const void *md, const char *from, const char *to, void *arg ) {
   CELL_SET( arg, MAKE_PAIR( string_to_bstring( (char *)from ), CELL_REF( arg ) ) );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_evp_get_ciphers ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_evp_get_ciphers() {
   obj_t acc;
   
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif
   acc = MAKE_CELL( BNIL );
   
   EVP_CIPHER_do_all_sorted(
      (void (*)(const EVP_CIPHER *, const char *, const char *, void *))
      list_push, (void *)acc );
   
   return bgl_reverse( CELL_REF( acc ) );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_evp_get_hashes ...                                           */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_evp_get_hashes() {
   obj_t acc = MAKE_CELL( BNIL );
   
   EVP_MD_do_all_sorted(
      (void (*)(const EVP_MD *, const char *, const char *, void *))
      list_push, (void *)acc );
   
   return bgl_reverse( CELL_REF( acc ) );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_hash_init ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_hash_init( ssl_hash hash ) {
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif
   
   CHASH( hash )->BgL_z42mdz42 =
      (void *)EVP_get_digestbyname( (const char *)BSTRING_TO_STRING( CHASH( hash )->BgL_typez00 ) );
   if( !(CHASH( hash )->BgL_z42mdz42) ) return 0;

   CHASH( hash )->BgL_z42mdzd2ctxz90 = BGL_EVP_MD_CTX_new();
   
   EVP_MD_CTX_init( CHASH( hash )->BgL_z42mdzd2ctxz90 );
   
   EVP_DigestInit_ex( CHASH( hash )->BgL_z42mdzd2ctxz90,
		      CHASH( hash )->BgL_z42mdz42, NULL );
   return 1;
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_hash_update ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_hash_update( ssl_hash hash, obj_t data, long offset, long len ) {
   if( CHASH( hash )->BgL_z42mdzd2ctxz90 == 0L ) {
      return 0;
   } else {
      EVP_DigestUpdate( CHASH( hash )->BgL_z42mdzd2ctxz90,
			&(STRING_REF( data, offset )),
			len );
      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_hash_digest ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_hash_digest( ssl_hash hash ) {
   if( CHASH( hash )->BgL_z42mdzd2ctxz90 == 0L ) {
      return 0;
   } else {
      unsigned char md_value[ EVP_MAX_MD_SIZE ];
      unsigned int md_len;

      EVP_DigestFinal_ex( CHASH( hash )->BgL_z42mdzd2ctxz90, md_value, &md_len );
      BGL_EVP_MD_CTX_reset( CHASH( hash )->BgL_z42mdzd2ctxz90 );
      BGL_EVP_MD_CTX_free( CHASH( hash )->BgL_z42mdzd2ctxz90 );
      CHASH( hash )->BgL_z42mdzd2ctxz90 = 0L;

      return string_to_bstring_len( md_value, md_len );
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                            */
/*    bgl_ssl_hmac_init ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_hmac_init( ssl_hmac hmac, obj_t type, obj_t key ) {
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif

   CHMAC( hmac )->BgL_z42mdz42 =
      (void *)EVP_get_digestbyname( (const char *)BSTRING_TO_STRING( type ) );
   if( !(CHMAC( hmac )->BgL_z42mdz42) ) return 0;

   CHMAC( hmac )->BgL_z42mdzd2ctxz90 = BGL_HMAC_CTX_new();
   BGL_HMAC_CTX_init( CHMAC( hmac )->BgL_z42mdzd2ctxz90 );

   if( !STRINGP( key ) ) {
      BGL_HMAC_Init( CHMAC( hmac )->BgL_z42mdzd2ctxz90,
		     "",
		     0,
		     CHMAC( hmac )->BgL_z42mdz42 );
   } else {
      BGL_HMAC_Init( CHMAC( hmac )->BgL_z42mdzd2ctxz90,
		     BSTRING_TO_STRING( key ),
		     STRING_LENGTH( key ),
		     CHMAC( hmac )->BgL_z42mdz42 );
   }
   return 1;
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_hmac_update ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_hmac_update( ssl_hmac hmac, obj_t data, long offset, long len ) {
   if( CHMAC( hmac )->BgL_z42mdzd2ctxz90 == 0L ) {
      return 0;
   } else {
      HMAC_Update( CHMAC( hmac )->BgL_z42mdzd2ctxz90,
		   &(STRING_REF( data, offset )),
		   len );
      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_hmac_digest ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_hmac_digest( ssl_hmac hmac ) {
   if( CHMAC( hmac )->BgL_z42mdzd2ctxz90 == 0L ) {
      return BFALSE;
   } else {
      unsigned char md_value[ EVP_MAX_MD_SIZE ];
      unsigned int md_len;

      HMAC_Final( CHMAC( hmac )->BgL_z42mdzd2ctxz90, md_value, &md_len );
      BGL_HMAC_CTX_reset( CHMAC( hmac )->BgL_z42mdzd2ctxz90 );
      BGL_HMAC_CTX_free( CHMAC( hmac )->BgL_z42mdzd2ctxz90 );
      CHMAC( hmac )->BgL_z42mdzd2ctxz90 = 0L;

      return string_to_bstring_len( md_value, md_len );
   }
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_sign_init ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_sign_init( ssl_sign sign, obj_t type ) {
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif

   CSIGN( sign )->BgL_z42mdz42 =
      (void *)EVP_get_digestbyname( (const char *)BSTRING_TO_STRING( type ) );
   if( !(CSIGN( sign )->BgL_z42mdz42) ) return 0;

   CSIGN( sign )->BgL_z42mdzd2ctxz90 = BGL_EVP_MD_CTX_new();
   EVP_MD_CTX_init( CSIGN( sign )->BgL_z42mdzd2ctxz90 );
   
   EVP_SignInit_ex( CSIGN( sign )->BgL_z42mdzd2ctxz90,
		    CSIGN( sign )->BgL_z42mdz42, NULL );
   return 1;
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_sign_update ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_sign_update( ssl_sign sign, obj_t data, long offset, long len ) {
   if( CSIGN( sign )->BgL_z42mdzd2ctxz90 == 0L ) {
      return 0;
   } else {
      EVP_SignUpdate( CSIGN( sign )->BgL_z42mdzd2ctxz90,
		      &(STRING_REF( data, offset )),
		      len );
      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_sign_sign ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_sign_sign( ssl_sign sign, obj_t key_pem, long offset, long kplen ) {
   if( CSIGN( sign )->BgL_z42mdzd2ctxz90 == 0L ) {
      return BFALSE;
   } else {
#define MAX_KEY_SIZE 8192      
      unsigned char md_value[ MAX_KEY_SIZE ];
      unsigned int md_len;
      BIO *bp = BIO_new( BIO_s_mem() );
      EVP_PKEY *pkey;
      
      if( !BIO_write( bp, &(STRING_REF( key_pem, offset )), kplen ) ) {
	 return BFALSE;
      }
      
      pkey = PEM_read_bio_PrivateKey( bp, NULL, NULL, NULL );
      if( !pkey ) {
	 ERR_print_errors_fp( stderr );
	 return BFALSE;
      }

      if( !EVP_SignFinal( CSIGN( sign )->BgL_z42mdzd2ctxz90, md_value, &md_len, pkey ) ) {
	 ERR_print_errors_fp( stderr );
	 return BFALSE;
      }

      BGL_EVP_MD_CTX_reset( CSIGN( sign )->BgL_z42mdzd2ctxz90 );
      BGL_EVP_MD_CTX_free( CSIGN( sign )->BgL_z42mdzd2ctxz90 );
      CSIGN( sign )->BgL_z42mdzd2ctxz90 = 0L;

      EVP_PKEY_free( pkey );
      BIO_free( bp );

      return string_to_bstring_len( md_value, md_len );
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_verify_init ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_verify_init( ssl_verify verify, obj_t type ) {
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif
   
   CVERIFY( verify )->BgL_z42mdz42 =
      (void *)EVP_get_digestbyname( (const char *)BSTRING_TO_STRING( type ) );
   if( !(CVERIFY( verify )->BgL_z42mdz42) ) return 0;

   CVERIFY( verify )->BgL_z42mdzd2ctxz90 = BGL_EVP_MD_CTX_new();
   EVP_MD_CTX_init( CVERIFY( verify )->BgL_z42mdzd2ctxz90 );
   
   EVP_VerifyInit_ex( CVERIFY( verify )->BgL_z42mdzd2ctxz90,
		      CVERIFY( verify )->BgL_z42mdz42, NULL );
   return 1;
}
   
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_verify_update ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_verify_update( ssl_verify verify, obj_t data, long offset, long len ) {
   if( CVERIFY( verify )->BgL_z42mdzd2ctxz90 == 0L ) {
      return 0;
   } else {
      EVP_VerifyUpdate( CVERIFY( verify )->BgL_z42mdzd2ctxz90,
			&(STRING_REF( data, offset )),
			len );
      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_verify_final  ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_verify_final( ssl_verify verify,
		      obj_t kpem, long koffset, long klen,
		      obj_t spem, long soffset, long slen ) {
   
   static const char PUBLIC_KEY_PFX[] =  "-----BEGIN PUBLIC KEY-----";
   static const int PUBLIC_KEY_PFX_LEN = sizeof(PUBLIC_KEY_PFX) - 1;
   static const char PUBRSA_KEY_PFX[] =  "-----BEGIN RSA PUBLIC KEY-----";
   static const int PUBRSA_KEY_PFX_LEN = sizeof(PUBRSA_KEY_PFX) - 1;
   static const int X509_NAME_FLAGS = ASN1_STRFLGS_ESC_CTRL
      | ASN1_STRFLGS_ESC_MSB
      | XN_FLAG_SEP_MULTILINE
      | XN_FLAG_FN_SN;
   
   if( CVERIFY( verify )->BgL_z42mdzd2ctxz90 == 0L ) {
      ERR_clear_error();
      return 0;
   } else {
      EVP_PKEY* pkey = NULL;
      BIO *bp = BIO_new( BIO_s_mem() );
      X509 *x509 = NULL;
      int r = 0;
      char *key_pem = &(STRING_REF( kpem, koffset ));
      char *sig = &(STRING_REF( spem, soffset ));

      if( !bp ) {
	 ERR_print_errors_fp( stderr );
	 ERR_clear_error();
	 return 0;
      }
      
      if( !BIO_write( bp, key_pem, klen ) ) {
	 ERR_print_errors_fp( stderr );
	 ERR_clear_error();
	 return 0;
      }

      // Check if this is a PKCS#8 or RSA public key before trying as X.509.
      // Split this out into a separate function once we have more than one
      // consumer of public keys.
      if( strncmp( key_pem, PUBLIC_KEY_PFX, PUBLIC_KEY_PFX_LEN ) == 0 ) {
	 pkey = PEM_read_bio_PUBKEY( bp, NULL, NULL, NULL );
	 if( pkey == NULL ) {
	    ERR_print_errors_fp( stderr );
	    ERR_clear_error();
	    return 0;
	 }
      } else if( strncmp( key_pem, PUBRSA_KEY_PFX, PUBRSA_KEY_PFX_LEN ) == 0 ) {
	 RSA* rsa = PEM_read_bio_RSAPublicKey( bp, NULL, NULL, NULL );
	 if( rsa ) {
	    pkey = EVP_PKEY_new();
	    if (pkey) EVP_PKEY_set1_RSA( pkey, rsa );
	    RSA_free( rsa );
	 }
	 if( !pkey ) {
	    ERR_print_errors_fp( stderr );
	    ERR_clear_error();
	    return 0;
	 }
      } else {
	 // X.509 fallback
	 x509 = PEM_read_bio_X509( bp, NULL, NULL, NULL );
	 if( !x509 ) {
	    ERR_print_errors_fp( stderr );
	    ERR_clear_error();
	    return 0;
	 }

	 pkey = X509_get_pubkey( x509 );
	 if( pkey == NULL ) {
	    ERR_print_errors_fp( stderr );
	    ERR_clear_error();
	    return 0;
	 }
      }

      r = EVP_VerifyFinal( CVERIFY( verify )->BgL_z42mdzd2ctxz90, sig, slen, pkey );

      if( !r ) {
	 ERR_clear_error();
      }

      if( pkey ) EVP_PKEY_free( pkey );
      if( x509 ) X509_free( x509 );
      if( bp ) BIO_free( bp );
      BGL_EVP_MD_CTX_reset( CVERIFY( verify )->BgL_z42mdzd2ctxz90 );
      BGL_EVP_MD_CTX_free( CVERIFY( verify )->BgL_z42mdzd2ctxz90 );
      CVERIFY( verify )->BgL_z42mdzd2ctxz90 = 0;

      return r && (r != -1);
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_cipher_init ...                                          */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_cipher_init( ssl_cipher cipher, obj_t type,
		     obj_t keybuf, long koffset, long klen, bool_t enc ) {
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );
   
   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif
   
   CCIPHER( cipher )->BgL_z42cipherz42 =
      (void *)EVP_get_cipherbyname( (const char *)BSTRING_TO_STRING( type ) );
   
   if( !(CCIPHER( cipher )->BgL_z42cipherz42) ) {
      return 0;
   } else {
      unsigned char key[ EVP_MAX_KEY_LENGTH ], iv[ EVP_MAX_IV_LENGTH ];
      int key_len = EVP_BytesToKey( CCIPHER( cipher )->BgL_z42cipherz42,
				    EVP_md5(), NULL,
				    &(STRING_REF( keybuf, koffset )), klen,
				    1, key, iv );
      EVP_CIPHER_CTX *ctx = BGL_EVP_CIPHER_CTX_new();
      CCIPHER( cipher )->BgL_z42cipherzd2ctxz90 = ctx;

      EVP_CIPHER_CTX_init( ctx );
      EVP_CipherInit_ex( ctx, CCIPHER( cipher )->BgL_z42cipherz42,
			 NULL, NULL, NULL, 0 );
      
      if( !EVP_CIPHER_CTX_set_key_length( ctx, key_len )) {
	 fprintf( stderr, "node-crypto : Invalid key length %ld\n", klen );
	 BGL_EVP_CIPHER_CTX_reset( ctx );
	 BGL_EVP_CIPHER_CTX_free( ctx );
	 return 0;
      }
      EVP_CipherInit_ex( ctx, NULL, NULL,
			 (unsigned char*)key,
			 (unsigned char*)iv, enc );
    
      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_cipher_initiv ...                                        */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_cipher_initiv( ssl_cipher cipher,
		       obj_t type, obj_t key, long koffset, long klen,
		       obj_t iv, long ivoffset, long ivlen, bool_t enc ) {
#if( SSL_DEBUG )
   BGL_MUTEX_LOCK( bigloo_mutex );

   if( !init ) {
      init = 1;
      bgl_ssl_library_init();
      SSL_DEBUG_INIT();
   }
   
   BGL_MUTEX_UNLOCK( bigloo_mutex );
#else
   bgl_ssl_init();
#endif
    
   CCIPHER( cipher )->BgL_z42cipherz42 =
      (void *)EVP_get_cipherbyname( (const char *)BSTRING_TO_STRING( type ) );

   if( !CCIPHER( cipher )->BgL_z42cipherz42 ) {
      fprintf( stderr, "node-crypto : Unknown cipher %s\n",
	       (const char *)BSTRING_TO_STRING( type ));
      return 0;
   }
   
   /* OpenSSL versions up to 0.9.8l failed to return the correct */
   /* iv_length (0) for ECB ciphers */
   if( EVP_CIPHER_iv_length( CCIPHER( cipher )->BgL_z42cipherz42 ) != ivlen &&
       !(EVP_CIPHER_mode( CCIPHER( cipher )->BgL_z42cipherz42) == EVP_CIPH_ECB_MODE &&
	 ivlen == 0) ) {
      fprintf( stderr, "node-crypto : Invalid IV length %ld\n", ivlen );
      return 0;
   } else {
      EVP_CIPHER_CTX *ctx = BGL_EVP_CIPHER_CTX_new();
      CCIPHER( cipher )->BgL_z42cipherzd2ctxz90 = ctx;

      EVP_CIPHER_CTX_init( ctx );
      EVP_CipherInit_ex( ctx, CCIPHER( cipher )->BgL_z42cipherz42, NULL, NULL, NULL, enc );
      
      if( !EVP_CIPHER_CTX_set_key_length( ctx, klen ) ) {
	 fprintf( stderr, "node-crypto : Invalid key length %ld\n", klen );
	 BGL_EVP_CIPHER_CTX_reset( ctx );
	 BGL_EVP_CIPHER_CTX_free( ctx );
	 return 0;
      }
      
      EVP_CipherInit_ex( ctx, NULL, NULL,
			 &(STRING_REF( key, koffset )),
			 &(STRING_REF( iv, ivoffset )),
			 enc );

      return 1;
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_cipher_update ...                                            */
/*---------------------------------------------------------------------*/
obj_t
bgl_cipher_update( ssl_cipher cipher, obj_t data, long offset, long len ) {
   if( CCIPHER( cipher )->BgL_z42cipherzd2ctxz90 == 0L ) {
      return BFALSE;
   } else {
      EVP_CIPHER_CTX *ctx = (EVP_CIPHER_CTX *)CCIPHER( cipher )->BgL_z42cipherzd2ctxz90;
      int olen = len + EVP_CIPHER_CTX_block_size( ctx );
      obj_t str = make_string( olen, ' ' );
      
      EVP_CipherUpdate( ctx, &(STRING_REF( str, 0 )), &olen,
			&(STRING_REF( data, offset )), len );

      return bgl_string_shrink( str, olen );
   }
}
		   
/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_cipher_set_auto_padding ...                                  */
/*---------------------------------------------------------------------*/
bool_t
bgl_cipher_set_auto_padding( ssl_cipher cipher, bool_t auto_padding ) {
   if( CCIPHER( cipher )->BgL_z42cipherzd2ctxz90 == 0L ) {
      return 0;
   } else {
      EVP_CIPHER_CTX *ctx = (EVP_CIPHER_CTX *)CCIPHER( cipher )->BgL_z42cipherzd2ctxz90;
      return EVP_CIPHER_CTX_set_padding( ctx, auto_padding );
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_cipher_final ...                                             */
/*---------------------------------------------------------------------*/
obj_t
bgl_cipher_final( ssl_cipher cipher ) {
   char errbuf[ 121 ];

   if( CCIPHER( cipher )->BgL_z42cipherzd2ctxz90 == 0L ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "cipher-final",
			"uninitialized cipher",
			(obj_t)cipher );
   } else {
      EVP_CIPHER_CTX *ctx = (EVP_CIPHER_CTX *)CCIPHER( cipher )->BgL_z42cipherzd2ctxz90;
      int size = EVP_CIPHER_CTX_block_size( ctx );
      obj_t obj = make_string( size, ' ' );
      int r;

      r = EVP_CipherFinal_ex( ctx, BSTRING_TO_STRING( obj ), &size );

      BGL_EVP_CIPHER_CTX_reset( ctx );
      BGL_EVP_CIPHER_CTX_free( ctx );
      CCIPHER( cipher )->BgL_z42cipherzd2ctxz90 = 0L;

      if( r ) {
	 return bgl_string_shrink( obj, size );
      } else {
	 C_SYSTEM_FAILURE( BGL_IO_ERROR, "cipher-final",
			   ssl_error_message( errbuf ),
			   (obj_t)cipher );
      }
   }
}

/*---------------------------------------------------------------------*/
/*    obj_t                                                            */
/*    bgl_pkcs5_pbkdf2_hmac_sha1 ...                                   */
/*---------------------------------------------------------------------*/
obj_t
bgl_pkcs5_pbkdf2_hmac_sha1( obj_t pass, obj_t salt, int iter, int keylen ) {
   obj_t obj = make_string( keylen, ' ' );
   char errbuf[ 121 ];

   int r = PKCS5_PBKDF2_HMAC_SHA1(
      &(STRING_REF( pass, 0 )), STRING_LENGTH( pass ),
      &(STRING_REF( salt, 0 )), STRING_LENGTH( salt ),
      iter, keylen, &(STRING_REF( obj, 0 )) );

   if( !r ) {
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "pkcs2-pbkdf2-hmac-sha1",
			ssl_error_message( errbuf ),
			(obj_t)pass );
   } else {
      return obj;
   }
}
      
/*---------------------------------------------------------------------*/
/*    bgl_ssl_error_string                                             */
/*---------------------------------------------------------------------*/
obj_t bgl_ssl_error_string() {
   int err = ERR_get_error();
   obj_t errmsg = make_string( 128, 0 );
   
   ERR_error_string_n(err, BSTRING_TO_STRING( errmsg ), 128 );

   return errmsg;
}
