/*=====================================================================*/
/*    serrano/prgm/project/bigloo/api/ssl/src/C/bglssl.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano & Stephane Epardaud                */
/*    Creation    :  Wed Mar 23 16:54:42 2005                          */
/*    Last change :  Fri Oct 10 07:07:06 2008 (serrano)                */
/*    Copyright   :  2005-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    SSL socket client-side support                                   */
/*=====================================================================*/
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/bio.h>
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

#define socklen_t void

#ifndef _BGL_WIN32_VER
#   define BAD_SOCKET(s) ((s) < 0)
#else
#   define BAD_SOCKET(s) ((s) == INVALID_SOCKET)
#endif

#define SOCKET_IO_BUFSIZE 1024

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
   static initialized = 0;

   bgl_mutex_lock( bigloo_mutex );
   
   if( !initialized ) {
      initialized = 1;

      /* the SSL dedicated lock */
      ssl_mutex = bgl_make_mutex( ssl_mutex_name );
      
      /* Initialize SSL context */
      SSL_library_init();
      SSL_load_error_strings();
      ctxc[ BGLSSL_SSLV2 ] = SSL_CTX_new( SSLv2_client_method() );
      ctxc[ BGLSSL_SSLV3 ] = SSL_CTX_new( SSLv3_client_method() );
      ctxc[ BGLSSL_SSLV23 ] = SSL_CTX_new( SSLv23_client_method() );
      ctxc[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLSv1_client_method() );
#if( BGLSSL_HAVE_DTLS )
      ctxc[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLSv1_client_method() );
#else      
      ctxc[ BGLSSL_DTLSV1 ] = 0;
#endif
      
      ctxs[ BGLSSL_SSLV2 ] = SSL_CTX_new( SSLv2_server_method() );
      ctxs[ BGLSSL_SSLV3 ] = SSL_CTX_new( SSLv3_server_method() );
      ctxs[ BGLSSL_SSLV23 ] = SSL_CTX_new( SSLv23_server_method() );
      ctxs[ BGLSSL_TLSV1 ] = SSL_CTX_new( TLSv1_server_method() );
#if( BGLSSL_HAVE_DTLS )
      ctxs[ BGLSSL_DTLSV1 ] = SSL_CTX_new( DTLSv1_server_method() );
#else      
      ctxs[ BGLSSL_DTLSV1 ] = 0;
#endif
   }
   
   bgl_mutex_unlock( bigloo_mutex );
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
   int r;
   SSL *ssl = (SSL*)CAR(PORT( port ).userdata);

loop:   
   if( (r = SSL_read( ssl, ptr, len )) <= 0 ) {
      if( (SSL_get_error( ssl, r ) == SSL_ERROR_SSL) && (errno == EINTR) )
	 goto loop;
   }
   
   return r;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    socket_close_hook ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
socket_close_hook( obj_t env, obj_t s ) {
   SSL *ssl = (SSL *)CAR( SOCKET( s ).userdata );

   bgl_mutex_lock( ssl_mutex );
   
   SSL_shutdown( ssl );
   SSL_free( ssl );
   SOCKET( s ).userdata = BUNSPEC;
   
   bgl_mutex_unlock( ssl_mutex );
   
   return s;
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    input_close_hook ...                                             */
/*---------------------------------------------------------------------*/
static obj_t
input_close_hook( obj_t env, obj_t ip ) {
   fclose( (FILE *)PORT_STREAM( ip ) );
}

/*---------------------------------------------------------------------*/
/*    static obj_t                                                     */
/*    output_close_hook ...                                            */
/*---------------------------------------------------------------------*/
static obj_t
output_close_hook( obj_t env, obj_t op ) {
   close( (long)(PORT( op ).userdata) );
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
      if( errid && (ERR_GET_LIB(errid) == ERR_LIB_SYS) ) {
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

   bgl_mutex_lock( ssl_mutex );
   
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
   
   bgl_mutex_unlock( ssl_mutex );

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

   PORT( op ).userdata = PORT_STREAM( op );
   PORT( op ).stream = (obj_t)ssl;
   PORT( op ).sysclose = 0L;
   PORT( op ).chook = ssl_output_close_hook;
   OUTPUT_PORT( op ).syswrite = (size_t(*)())&SSL_write;
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

