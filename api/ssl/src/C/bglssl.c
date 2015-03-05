/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/ssl/src/C/bglssl.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano & Stephane Epardaud                */
/*    Creation    :  Wed Mar 23 16:54:42 2005                          */
/*    Last change :  Thu Mar  5 10:43:39 2015 (serrano)                */
/*    Copyright   :  2005-15 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    SSL socket client-side support                                   */
/*=====================================================================*/
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/bio.h>
#include <openssl/crypto.h>
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

#define socklen_t void

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

/*---------------------------------------------------------------------*/
/*    type aliasing                                                    */
/*---------------------------------------------------------------------*/
typedef BgL_sslzd2connectionzd2_bglt ssl_connection;
typedef BgL_securezd2contextzd2_bglt secure_context;

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
static SSL_CTX *ctxc[ BGLSSL_DTLSV1 + 1 ], *ctxs[ BGLSSL_DTLSV1 + 1 ];

extern obj_t bgl_make_certificate( X509 *cert );
extern obj_t bgl_make_private_key( EVP_PKEY* pkey );
extern X509 *bgl_certificate_native( obj_t cert );
extern EVP_PKEY *bgl_private_key_native( obj_t pkey );

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
      SSL_library_init();
      SSL_load_error_strings();
#if( BGLSSL_HAVE_SSLV2 )
      ctxc[ BGLSSL_SSLV2 ] = SSL_CTX_new( SSLv2_client_method() );
#endif
      ctxc[ BGLSSL_SSLV3 ] = SSL_CTX_new( SSLv3_client_method() );
#if( BGLSSL_HAVE_SSLV23 )
      ctxc[ BGLSSL_SSLV23 ] = SSL_CTX_new( SSLv23_client_method() );
#endif      
      ctxc[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLSv1_client_method() );
#if( BGLSSL_HAVE_DTLS )
      ctxc[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLSv1_client_method() );
#else      
      ctxc[ BGLSSL_DTLSV1 ] = 0;
#endif
      
#if( BGLSSL_HAVE_SSLV2 )
      ctxs[ BGLSSL_SSLV2 ] = SSL_CTX_new( SSLv2_server_method() );
#endif      
      ctxs[ BGLSSL_SSLV3 ] = SSL_CTX_new( SSLv3_server_method() );
#if( BGLSSL_HAVE_SSLV23 )
      ctxs[ BGLSSL_SSLV23 ] = SSL_CTX_new( SSLv23_server_method() );
#endif      
      ctxs[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLSv1_server_method() );
#if( BGLSSL_HAVE_DTLS )
      ctxs[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLSv1_server_method() );
#else      
      ctxs[ BGLSSL_DTLSV1 ] = 0;
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
   SSL *ssl = (SSL*)CAR( PORT( port ).userdata );

loop:   
   if( (r = SSL_read( ssl, ptr, len )) <= 0 ) {
      if( r == 0 ) {
	 INPUT_PORT( port ).eof = 1;
      } else {
	 if( (SSL_get_error( ssl, r ) == SSL_ERROR_SSL) && (errno == EINTR) )
	    goto loop;
      }
   }
   
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
   SSL *ssl = (SSL *)CAR( SOCKET( s ).userdata );

   BGL_MUTEX_LOCK( ssl_mutex );
   
   SSL_shutdown( ssl );
   SSL_free( ssl );
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
   close( (int)(PORT( op ).userdata) );
   
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

      ctx = SSL_CTX_new( ctx->method );
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
   drag = MAKE_PAIR( (obj_t)ssl, drag );

   PORT( ip ).userdata = drag;
   PORT( ip ).chook = ssl_input_close_hook;
   PORT( ip ).sysclose = 0L;
   INPUT_PORT( ip ).sysread = &sslread;
   
   PORT( op ).userdata = (void *)PORT_FD( op );
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
                            obj_t accepted_certs, int backlog ) {
   obj_t serv = bgl_make_server_socket( hostname, port, backlog );
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

//#include "ssl_debug.h"

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
      SSLerr(SSL_F_SSL_CTX_USE_CERTIFICATE_CHAIN_FILE, ERR_R_PEM_LIB );
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

      if( ctx->extra_certs != NULL ) {
	 sk_X509_pop_free( ctx->extra_certs, X509_free );
	 ctx->extra_certs = NULL;
      }

      while( (ca = PEM_read_bio_X509(in, NULL, NULL, NULL)) ) {
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
bgl_ssl_ctx_add_root_certs( BgL_securezd2contextzd2_bglt sc ) {
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

   SSL_CTX_set_cert_store( sc->BgL_z42nativez42, root_cert_store );
   return 1;
}

/*---------------------------------------------------------------------*/
/*    bool_t                                                           */
/*    bgl_ssl_ctx_add_ca_cert ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_ctx_add_ca_cert( secure_context sc, obj_t cert, long offset, long len ) {
   char newCAStore = 0;
   BIO *bio = BIO_new( BIO_s_mem() );
   X509 *x509;
   
   if( !bio ) {
      return 0;
   }

   BIO_write( bio, &STRING_REF( cert, offset ), len );
   
   if( sc->BgL_z42cazd2storez90 == 0L ) {
      sc->BgL_z42cazd2storez90 = X509_STORE_new();
      newCAStore = 1;
  }

  x509 = PEM_read_bio_X509( bio, NULL, NULL, NULL );
  BIO_free( bio );
  
  if( !x509) {
     return 0;
  }

  X509_STORE_add_cert( sc->BgL_z42cazd2storez90, x509 );
  SSL_CTX_add_client_CA( sc->BgL_z42nativez42, x509 );

  X509_free( x509 );

  if( newCAStore ) {
    SSL_CTX_set_cert_store( sc->BgL_z42nativez42, sc->BgL_z42cazd2storez90 );
  }

  return 1;
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    bgl_info_callback ...                                            */
/*---------------------------------------------------------------------*/
static void
bgl_info_callback( const SSL *ssl, int where, int ret ) {
   ssl_connection c = (ssl_connection)(SSL_get_app_data( ssl ));
   obj_t cb = c->BgL_infozd2callbackzd2;

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
bgl_ssl_connection_init( ssl_connection ssl, char *servname ) {
   secure_context bctx = ssl->BgL_ctxz00;
   int verify_mode;
   SSL *_ssl = SSL_new( bctx->BgL_z42nativez42 );
   long mode;

   ssl->BgL_z42nativez42 = _ssl;
   ssl->BgL_z42biozd2readz90 = BIO_new( BIO_s_mem() );
   ssl->BgL_z42biozd2writez90 = BIO_new( BIO_s_mem() );

   SSL_set_app_data( _ssl, ssl );

   if( ssl->BgL_isserverz00 ) {
      SSL_set_info_callback( _ssl, bgl_info_callback );
   }

   if( ssl->BgL_isserverz00 ) {
   } else {
      if( STRINGP( ssl->BgL_serverzd2namezd2 ) ) {
	 SSL_set_tlsext_host_name( _ssl, BSTRING_TO_STRING( ssl->BgL_serverzd2namezd2 ) );
      }
   }

   SSL_set_bio( _ssl, ssl->BgL_z42biozd2readz90, ssl->BgL_z42biozd2writez90 );

#if( defined( SSL_MODE_RELEASE_BUFFERS ) )
   mode = SSL_get_mode( _ssl );
   SSL_set_mode( _ssl, mode | SSL_MODE_RELEASE_BUFFERS );
#endif
   
   if( ssl->BgL_isserverz00 ) {
      if( ssl->BgL_requestzd2certzd2 ) {
	 verify_mode = SSL_VERIFY_NONE;
      } else {
	 verify_mode = SSL_VERIFY_PEER;
	 if( ssl->BgL_rejectzd2unauthoriza7edz75 ) {
	    verify_mode |= SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
	 }
      }
   } else {
      verify_mode = SSL_VERIFY_NONE;
   }

   SSL_set_verify( _ssl, verify_mode, bgl_verify_callback );
   
   if( ssl->BgL_isserverz00 ) {
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

      fprintf( stderr, "TODO ERROR (%s) %s:%d\n", ssl_error_buf,
	       __FILE__, __LINE__ );

      return;
   }
}

/*---------------------------------------------------------------------*/
/*    static void                                                      */
/*    handle_ssl_error ...                                             */
/*---------------------------------------------------------------------*/
static void
handle_ssl_error( ssl_connection ssl, int n, char *fun ) {
   SSL *_ssl = ssl->BgL_z42nativez42;
   int err = SSL_get_error( _ssl, n );
   int res = 0;

   if( err == SSL_ERROR_NONE ) {
      goto ret;
   } else if( err == SSL_ERROR_WANT_WRITE ) {
      goto ret;
   } else if( err == SSL_ERROR_WANT_READ ) {
      goto ret;
   } else if( err == SSL_ERROR_ZERO_RETURN ) {
      res = n;
      goto ret;
   } else {
      BUF_MEM* mem;
      BIO *bio;
      
      if( (bio = BIO_new( BIO_s_mem() )) ) {
	 ERR_print_errors( bio );
	 BIO_get_mem_ptr( bio, &mem );
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
   SSL *_ssl = ssl->BgL_z42nativez42;
   int flags = SSL_get_shutdown( _ssl );

   if( flags & SSL_SENT_SHUTDOWN ) {
      fprintf( stderr, "TODO %s:%d\n", __FILE__, __LINE__ );
   }

   if( flags & SSL_RECEIVED_SHUTDOWN ) {
      fprintf( stderr, "TODO %s:%d\n", __FILE__, __LINE__ );
   }
}

#if( SSL_DEBUG )
/*---------------------------------------------------------------------*/
/*    void                                                             */
/*    set_shutdown_flags ...                                           */
/*---------------------------------------------------------------------*/
static void
set_shutdown_flags2( char *name, ssl_connection ssl ) {
   SSL *_ssl = ssl->BgL_z42nativez42;
   int flags = SSL_get_shutdown2( name, _ssl );

   if( flags & SSL_SENT_SHUTDOWN ) {
      fprintf( stderr, "TODO %s:%d\n", __FILE__, __LINE__ );
   }

   if( flags & SSL_RECEIVED_SHUTDOWN ) {
      fprintf( stderr, "TODO %s:%d\n", __FILE__, __LINE__ );
   }
}
#endif

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_ssl_connection_start ...                                     */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_start( ssl_connection ssl ) {
   SSL *_ssl = ssl->BgL_z42nativez42;
   int n;

   if( !SSL_is_init_finished( _ssl ) ) {
      if( ssl->BgL_isserverz00 ) {
	 if( (n = SSL_accept( _ssl )) <= 0 ) {
	    handle_ssl_error( ssl, n, "ssl-connection-start" );
	 }
      } else {
#if( SSL_DEBUG )	 
	 if( (n = SSL_connect2( "start", _ssl )) <= 0 )
#else	    
	 if( (n = SSL_connect( _ssl )) <= 0 )
#endif
	 {
	    handle_ssl_error( ssl, n, "ssl-connection-start" );
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
   SSL *_ssl = ssl->BgL_z42nativez42;

   SSL_free( _ssl );
}

/*---------------------------------------------------------------------*/
/*    int                                                              */
/*    bgl_ssl_connection_shutdown ...                                  */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_shutdown( ssl_connection ssl ) {
   SSL *_ssl = ssl->BgL_z42nativez42;
   int flags = SSL_get_shutdown( _ssl );
   obj_t res = BNIL;

   if( flags & SSL_SENT_SHUTDOWN ) {
      res = MAKE_PAIR( res, string_to_symbol( "sent" ) );
   }

   if( flags & SSL_RECEIVED_SHUTDOWN ) {
      res = MAKE_PAIR( res, string_to_symbol( "received" ) );
   }
   
   SSL_free( _ssl );
}

/*---------------------------------------------------------------------*/
/*    long                                                             */
/*    bgl_ssl_connection_read ...                                      */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF long
bgl_ssl_connection_read( ssl_connection ssl, char *buf, long off, long len ) {
   long int n = BIO_read( ssl->BgL_z42biozd2writez90, buf + off, len );

#if( SSL_DEBUG )	 
   fprintf( stderr, "%s (bgl_ssl_connection_read) off=%d len=%d bytes_read=%d\n", __FILE__, off, len, n );
#endif   
   
   if( n < 0 ) {
      handle_bio_error( ssl, ssl->BgL_z42biozd2writez90, n, "connection_read" );
   }
#if( SSL_DEBUG )	 
   else {
      int bytes_read = n;
      if( bytes_read >= 0 ) {
#if( SSL_DEBUG >=2 )	 
	 int i;
	 for( i = 0; i < bytes_read; i++ ) {
	    fprintf( stderr, "%02x ", ((unsigned char *)(buf))[ off + i ] );
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
   long int n = BIO_write( ssl->BgL_z42biozd2readz90, buf + off, len );

#if( SSL_DEBUG )	 
   fprintf( stderr, "%s (bgl_ssl_connection_write) off=%d len=%d\n", __FILE__, off, len );
   {
      int i;

#if( SSL_DEBUG >=2 )	 
      for( i = 0; i < (len < 160 ? len : 160); i++ ) {
	 fprintf( stderr, "%02x ", ((unsigned char *)(buf))[ off + i ] );
      }

      fprintf( stderr, "\n" );
#endif      
   }
#endif
   
   if( n < 0 ) {
      handle_bio_error( ssl, ssl->BgL_z42biozd2readz90, n, "connection_write" );
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
   SSL *_ssl = ssl->BgL_z42nativez42;
   long n;

   if( !SSL_is_init_finished( _ssl ) ) {
      if( ssl->BgL_isserverz00 ) {
	 long m;
	 if( (m = SSL_accept( _ssl )) <= 0 ) {
	    handle_ssl_error( ssl, m, "ssl-connection-clear (accept)" );
	 }
	 return m;
      } else {
	 int m;
#if( SSL_DEBUG ) 
	 if( (m = SSL_connect2( name, _ssl )) <= 0 )
#else	    
	 if( (m = SSL_connect( _ssl )) <= 0 )
#endif
	 {
	    handle_ssl_error( ssl, m, "ssl-connection-clear (connect)" );
	 }
	 return m;
      }
   }

#if( defined( SSL_DEBUG) )   
   if( SSL_fun == &SSL_write ) {
      n = SSL_write( _ssl, buf + off, len );
   } else {
      n = SSL_read( _ssl, buf + off, len );
   }
#else   
   n = SSL_fun( _ssl, buf + off, len );
#endif

   if( n < 0 ) {
      handle_ssl_error( ssl, n, "ssl-connection-clear (read/write)" );
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
      char s[ len + 1 ];
      strncpy( s, buf + off, len );
      s[ len ] = 0;
      
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
   SSL *_ssl = ssl->BgL_z42nativez42;

   return (_ssl != NULL && SSL_is_init_finished( _ssl ));
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_ssl_connection_enc_pending ...                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_enc_pending( ssl_connection ssl ) {
   return BIO_pending( ssl->BgL_z42biozd2writez90 );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF int                                              */
/*    bgl_ssl_connection_clear_pending ...                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF int
bgl_ssl_connection_clear_pending( ssl_connection ssl ) {
   return BIO_pending( ssl->BgL_z42biozd2readz90 );
}

/*---------------------------------------------------------------------*/
/*    static SSL_SESSION *                                             */
/*    bgl_get_session_callback ...                                     */
/*---------------------------------------------------------------------*/
static SSL_SESSION *
bgl_get_session_callback( SSL *s, unsigned char *key, int len, int *copy ) {
   fprintf( stderr, "TODO: %s:%d\n", __FILE__, __LINE__ );
}

/*---------------------------------------------------------------------*/
/*    static int                                                       */
/*    bgl_new_session_callback ...                                     */
/*---------------------------------------------------------------------*/
static int
bgl_new_session_callback( SSL *s, SSL_SESSION *sess ) {
   fprintf( stderr, "TODO: %s:%d\n", __FILE__, __LINE__ );
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF bool_t                                           */
/*    bgl_ssl_connection_set_session ...                               */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF bool_t
bgl_ssl_connection_set_session( ssl_connection ssl, obj_t buf ) {
   int wlen = STRING_LENGTH( buf );
   char *sbuf = alloca( wlen );
   SSL *_ssl = ssl->BgL_z42nativez42;
   SSL_SESSION *sess;
   
   memcpy( sbuf, BSTRING_TO_STRING( buf ), wlen );
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
/*    bgl_ssl_connection_verify_error ...                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_connection_verify_error( ssl_connection ssl ) {
   SSL *_ssl = ssl->BgL_z42nativez42;
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
/*    bgl_ssl_set_key ...                                              */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_set_key( secure_context sc, obj_t cert, long offset, long len, obj_t passphrase ) {
   BIO *bio = BIO_new( BIO_s_mem() );

   BIO_write( bio, &STRING_REF( cert, offset ), len );
   EVP_PKEY *key =  PEM_read_bio_PrivateKey(
      bio, NULL, NULL,
      STRINGP( passphrase ) ? BSTRING_TO_STRING( passphrase ) : NULL );

   BIO_free( bio );
   
   if( !key ) {
      char ebuf[ 121 ];
      
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "set-key",
			ssl_error_message( ebuf ),
			(obj_t)sc );
   }

   SSL_CTX_use_PrivateKey( sc->BgL_z42nativez42, key );

   EVP_PKEY_free( key );

   return BTRUE;
}
	 
/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_set_cert ...                                             */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_set_cert( secure_context sc, obj_t cert, long offset, long len, obj_t passphrase ) {
   BIO *bio = BIO_new( BIO_s_mem() );
   int rv;
   
   BIO_write( bio, &STRING_REF( cert, offset ), len );

   rv = SSL_CTX_use_certificate_chain( sc->BgL_z42nativez42, bio );
   BIO_free( bio );

   if( !rv ) {
      char ebuf[ 121 ];
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "set-key",
			ssl_error_message( ebuf ),
			(obj_t)sc );
   }

   return BTRUE;
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
   SSL *_ssl = ssl->BgL_z42nativez42;
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
	 BN_print( bio, rsa->n );
	 BIO_get_mem_ptr( bio, &mem );
	 info = MAKE_PAIR( cons( "modulus", mem ), info );
	 BIO_reset( bio );

	 BN_print( bio, rsa->e );
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

      ASN1_OBJECT *eku =
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
/*    obj_t                                                            */
/*    bgl_ssl_ctx_init ...                                             */
/*    -------------------------------------------------------------    */
/*    Leave this function at the end of the file as it breaks emacs    */
/*    auto indentation.                                                */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_ctx_init( secure_context sc ) {
   char *sslmethod = BSTRING_TO_STRING( sc->BgL_methodz00 );
   SSL_METHOD *method;
   
#if( SSL_DEBUG )   
      SSL_library_init();
      SSL_DEBUG_INIT();
      SSL_load_error_strings();
#else
   bgl_ssl_init();
#endif
   
   if( !strcmp( sslmethod, "SSLv2_method" ) ) {
#if( BGLSSL_HAVE_SSLV2 )
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv2_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv2_server_method" ) ) {
#if( BGLSSL_HAVE_SSLV2 )
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv2_server_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv2_client_method" ) ) {
#if( BGLSSL_HAVE_SSLV2 )
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv2_client_method() );
#else
      goto unsupported;
#endif
   } else if( !strcmp( sslmethod, "SSLv3_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv3_method() );
   } else if( !strcmp( sslmethod, "SSLv3_server_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv3_server_method() );
   } else if( !strcmp( sslmethod, "SSLv3_client_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv3_client_method() );
   } else if( !strcmp( sslmethod, "SSLv23_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv23_method() );
   } else if( !strcmp( sslmethod, "SSLv23_server_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv23_server_method() );
   } else if( !strcmp( sslmethod, "SSLv23_client_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( SSLv23_client_method() );
   } else if( !strcmp( sslmethod, "TLSv1_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( TLSv1_method() );
   } else if( !strcmp( sslmethod, "TLSv1_server_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( TLSv1_server_method() );
   } else if( !strcmp( sslmethod, "TLSv1_client_method" ) ) {
      sc->BgL_z42nativez42 = SSL_CTX_new( TLSv1_client_method() );
   } else {
      goto unsupported;
   }

   // SSL session cache configuration
   if( !(sc->BgL_z42nativez42) ) {
      char errbuf[ 121 ];
      
      C_SYSTEM_FAILURE( BGL_IO_ERROR, "secure-context-init",
			ssl_error_message( errbuf ),
			(obj_t)sc );
      return (obj_t)sc;
   }
   
   SSL_CTX_set_session_cache_mode( sc->BgL_z42nativez42,
				   SSL_SESS_CACHE_SERVER
				   | SSL_SESS_CACHE_NO_INTERNAL
				   | SSL_SESS_CACHE_NO_AUTO_CLEAR );
   
   SSL_CTX_sess_set_get_cb( sc->BgL_z42nativez42, bgl_get_session_callback );
   SSL_CTX_sess_set_new_cb( sc->BgL_z42nativez42, bgl_new_session_callback );

   return (obj_t)sc;
  
unsupported:
   C_SYSTEM_FAILURE( BGL_ERROR, "secure-context",
	 "method not supported", 
	 sc->BgL_methodz00 );
   return (obj_t)sc;
}

/*---------------------------------------------------------------------*/
/*    BGL_RUNTIME_DEF obj_t                                            */
/*    bgl_ssl_ctx_close ...                                            */
/*---------------------------------------------------------------------*/
BGL_RUNTIME_DEF obj_t
bgl_ssl_ctx_close( secure_context sc ) {
   SSL_CTX_free( sc->BgL_z42nativez42 );
}
   
