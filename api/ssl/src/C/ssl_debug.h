/*=====================================================================*/
/*    serrano/prgm/project/bigloo/bigloo/api/ssl/src/C/ssl_debug.h     */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jun 23 08:29:53 2015                          */
/*    Last change :  Tue Dec  4 13:43:42 2018 (serrano)                */
/*    Copyright   :  2015-18 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Temporary include for debugging ssl                              */
/*=====================================================================*/

#define SSL_DEBUG 3
#undef SSL_DEBUG

static int init = 0;

#if( SSL_DEBUG )
int call1name;
int callr1;
void *callr1void;

#if( defined( BGL_SSL ) )
int count;
#elif( defined( BIGLOO_EXIT ) ) 
extern int count; 
#else
static int count;
#endif

#define call0( fun ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun())
#define call1( fun, a0 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0))
#define call1name( fun, from, a0 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): (%s) " #fun " -> %d\n", count++, from, (call1name = (&fun)(a0)) ), call1name)
#define call1r( fun, a0 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun " -> %d\n", count++, (callr1=fun(a0)) ), callr1)
#define call2( fun, a0, a1 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1))
#define call2r( fun, a0, a1 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun " -> %d\n", count++, fun(a0,a1) ), fun(a0,a1))
#define call3( fun, a0, a1, a2 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1,a2))
#define call3b( fun, a0, a1, a2 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "a2=%d\n", count++, a2 ), fun(a0,a1,a2))
#define call4( fun, a0, a1, a2, a3 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1,a2,a3))
#define call5( fun, a0, a1, a2, a3, a4 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1,a2,a3,a4))
#define call6( fun, a0, a1, a2, a3, a4, a5 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1,a2,a3,a4,a5))
#define call7( fun, a0, a1, a2, a3, a4, a5, a6 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1,a2,a3,a4,a5,a6))
#define call8( fun, a0, a1, a2, a3, a4, a5, a6, a7 ) \
   (fprintf( stderr, "SSL_DEBUG(%d): " #fun "\n", count++ ), fun(a0,a1,a2,a3,a4,a5,a6,a7))

//#define SSLv23_method()	call0(SSLv23_method)
#define SSLv2_method() call0(SSLv2_method)
#define SSLv1_server_method() call0(SSLv1_server_method)
#define SSLv1_client_method() call0(SSLv1_client_method)
#define SSLv2_server_method() call0(SSLv2_server_method)
#define SSLv2_client_method() call0(SSLv2_client_method)
#define SSLv3_server_method() call0(SSLv3_server_method)
#define SSLv3_client_method() call0(SSLv3_client_method)
#define SSLv23_server_method() call0(SSLv23_server_method)
#define SSLv23_client_method() call0(SSLv23_client_method)

static SSL_METHOD *__method;
#define SSL_CTX_new(a0) (__method = (SSL_METHOD *)a0, call1(SSL_CTX_new, __method))

#define SSL_CTX_sess_set_get_cb(a0, a1) call2(SSL_CTX_sess_set_get_cb, a0, a1)
#define SSL_CTX_sess_set_new_cb(a0, a1) call2(SSL_CTX_sess_set_new_cb, a0, a1)


#define i2d_SSL_SESSION(a0, a1) call2(i2d_SSL_SESSION, a0, a1)
#define SSL_CTX_use_PrivateKey(a0, a1) call2(SSL_CTX_use_PrivateKey, a0, a1)

#define SSL_CTX_use_certificate(a0, a1) call2(SSL_CTX_use_certificate, a0, a1)

#define SSL_CTX_set_cipher_list(a0,a1) call2(SSL_CTX_set_cipher_list, a0, a1)
#define SSL_CTX_set_session_id_context(a0,a1,a2) call3(SSL_CTX_set_session_id_context, a0, a1, a2)

#define SSL_CTX_set_cert_store(a0, a1) call2(SSL_CTX_set_cert_store,a0, a1)
#define X509_STORE_add_cert(a0, x509) call2(X509_STORE_add_cert,a0, x509)
#define SSL_CTX_add_client_CA(a0, x509) call2(SSL_CTX_add_client_CA,a0, x509)
#define X509_free(x509) call1(X509_free,x509)

#define EVP_md5() call0(EVP_md5)
#define EVP_sha1() call0(EVP_sha1)
#define EVP_PKEY_free(pkey) call1(EVP_PKEY_free,pkey)
#define EVP_PKEY_get1_RSA(pkey) call1(EVP_PKEY_get1_RSA,pkey)
#define EVP_PKEY_set1_RSA(a0,a1) call2(EVP_PKEY_set1_RSA,a0,a1)
#define EVP_get_cipherbyname(name) call1(EVP_get_cipherbyname,name)
#define EVP_BytesToKey(a0,a1,a2,a3,a4,a5,a6,a7) call8(EVP_BytesToKey,a0,a1,a2,a3,a4,a5,a6,a7)
#define EVP_CIPHER_CTX_init(a0) call1(EVP_CIPHER_CTX_init,a0)
#define EVP_CipherInit_ex(a0,a1,a2,a3,a4,a5) call6(EVP_CipherInit_ex,a0,a1,a2,a3,a4,a5)
#define EVP_CIPHER_CTX_set_key_length(a0,a1) call2(EVP_CIPHER_CTX_set_key_length,a0,a1)
#define EVP_CIPHER_CTX_cleanup(a0) call1(EVP_CIPHER_CTX_cleanup,a0)
#define EVP_MD_CTX_cleanup(a0) call1(EVP_MD_CTX_cleanup,a0)
#define EVP_CIPHER_iv_length(a0) call1(EVP_CIPHER_iv_length,a0)
#define EVP_CipherUpdate(a0,a1,a2,a3,a4) call5(EVP_CipherUpdate,a0,a1,a2,a3,a4)
#define EVP_CIPHER_CTX_set_padding(a0,a1) call2(EVP_CIPHER_CTX_set_padding,a0,a1)
#define EVP_CIPHER_CTX_block_size(a0) call1(EVP_CIPHER_CTX_block_size,a0)
#define EVP_CipherFinal_ex(a0,a1,a2) call3(EVP_CipherFinal_ex,a0,a1,a2)
#define EVP_MD_CTX_init(a0) call1(EVP_MD_CTX_init,a0)
#define EVP_DigestInit_ex(a0,a1,a2) call3(EVP_DigestInit_ex,a0,a1,a2)
#define EVP_DigestFinal_ex(a0,a1,a2) call3(EVP_DigestFinal,a0,a1,a2)
#define EVP_DigestUpdate(a0,a1,a2) call3(EVP_DigestUpdate,a0,a1,a2)
#define EVP_SignFinal(a0,a1,a2,a3) call4(EVP_SignFinal,a0,a1,a2,a3)
#define EVP_get_digestbyname(a0) call1(EVP_get_digestbyname,a0)
#define EVP_VerifyFinal(a0,a1,a2,a3) call4(EVP_VerifyFinal,a0,a1,a2,a3)
#define EVP_PKEY_new() call0(EVP_PKEY_new)
#define EVP_CIPHER_do_all_sorted(a0,a1) call2(EVP_CIPHER_do_all_sorted,a0,a1)
#define EVP_MD_do_all_sorted(a0,a1) call2(EVP_MD_do_all_sorted,a0,a1)

#define PKCS12_free(p12) call1(PKCS12_free,p12)
#define BIO_free(in) call1(BIO_free,in)
#define SSL_get_error(ssl_, rv) call2r(SSL_get_error,ssl_, rv)
#define SSL_get_shutdown(ssl_) call1r(SSL_get_shutdown,ssl_)
#define SSL_get_shutdown2(name,ssl_) call1name(SSL_get_shutdown,name,ssl_)
#define SSL_select_next_proto(a0,a1,a2,a3,a4,a5) call6(SSL_select_next_proto,a0,a1,a2,a3,a4,a5)
#define SSL_get_servername(s, TLSEXT_NAMETYPE_host_name) call2(SSL_get_servername,s, TLSEXT_NAMETYPE_host_name)
#define SSL_set_SSL_CTX(s, a1) call2(SSL_set_SSL_CTX,s, a1)
#define SSL_new(a0) call1(SSL_new,a0)
#define SSL_set_info_callback(a0, SSLInfoCallback) call2(SSL_set_info_callback,a0, SSLInfoCallback)
#define SSL_set_bio(a0, a1,a2) call3(SSL_set_bio,a0, a1,a2)
#define SSL_set_verify(a0, verify_mode, VerifyCallback) call3(SSL_set_verify,a0, verify_mode, VerifyCallback)
#define SSL_set_accept_state(a0) call1(SSL_set_accept_state,a0)
#define SSL_set_connect_state(a0) call1(SSL_set_connect_state,a0)
#define SSL_accept(a0) call1(SSL_accept,a0)
#define SSL_connect(a0) call1r(SSL_connect,a0)
#define SSL_connect2(name,a0) call1name(SSL_connect,name,a0)
#define SSL_get_peer_certificate(a0) call1(SSL_get_peer_certificate,a0)
#define d2i_SSL_SESSION(NULL, p, wlen) call3(d2i_SSL_SESSION,NULL, p, wlen)
#define SSL_SESSION_free(sess) call1(SSL_SESSION_free,sess)
#define SSL_shutdown(a0) call1(SSL_shutdown,a0)
#define SSL_get_verify_result(a0) call1(SSL_get_verify_result,a0)
#define SSL_get_current_cipher(a0) call1(SSL_get_current_cipher,a0)
#define SSL_CIPHER_get_name(c) call1(SSL_CIPHER_get_name,c)
#define SSL_CIPHER_get_version(c) call1(SSL_CIPHER_get_version,c)
#define SSL_free(a0) call1(SSL_free,a0)
#define SSL_CTX_set_next_protos_advertised_cb(a0,a1,a2) call3(SSL_CTX_set_next_protos_advertised_cb,a0,a1,a2)
#define SSL_CTX_set_next_proto_select_cb(a0,a1,a2) call3(SSL_CTX_set_next_proto_select_cb,a0,a1,a2)
#define SSL_get0_next_proto_negotiated(a0,a1,a2) call3(SSL_get0_next_proto_negotiated,a0,a1,a2)
#define SSL_get_ciphers(ssl) call1(SSL_get_ciphers,ssl)
#define SSL_CTX_free(ctx) call1(SSL_CTX_free,ctx)
#define SSL_load_error_strings() call0(SSL_load_error_strings)
#define SSL_library_init() call0(SSL_library_init)
#define PEM_read_bio_X509_AUX(a0,a1,a2,a3) call4(PEM_read_bio_X509_AUX,a0,a1,a2,a3)
#define PEM_read_bio_X509(a0,a1,a2,a3) call4(PEM_read_bio_X509,a0,a1,a2,a3)
#define PEM_read_bio_PrivateKey(a0,a1,a2,a3) call4(PEM_read_bio_PrivateKey,a0,a1,a2,a3)
#define BIO_new_file(a0,a1) call2(BIO_new_file,a0,a1)

#define SSL_DEBUG_INIT() (fprintf( stderr, "~~~~~~~~~~~~ SSL_DEBUG_INIT ~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n" ))
#define SSL_debug(name) (fprintf( stderr, "SSL_DEBUG: %s\n", name ))


static int wsum = 0;
static int swsum = 0;
static int rsum = 0;
static int srsum = 0;
static int biocount = 0;
static int rres = 0;
static int wres = 0;
static int sxor( char *s, int len ) {
   int r = (int)s[ 0 ];
   int i;
   for( i = 0; i < len; i++ ) r ^= (int)s[ i ];
   return r;
}
#define BIO_write( bio, s, len ) \
   (wres = (&BIO_write)( bio, s, len ), wsum += wres, fprintf( stderr, "BIO_Write(%d) res=%d len=%d sum=%d c0=%02x xor=%d\n", biocount++, wres, len, wsum, *(s), sxor( (char *)(s), len ) ), wres)

#define BIO_read( bio, s, len ) \
   (rres = (&BIO_read)( bio, s, len ), rsum += rres, fprintf( stderr, "BIO_Read(%d) res=%d len=%d sum=%d\n", biocount++, rres, len, rsum ), rres )

#define SSL_write( bio, s, len ) \
   (swsum+= len, fprintf( stderr, "SSL_write(%d) len=%d sum=%d c0=%02x xor=%d\n", biocount++, len, swsum, *(s), sxor( (char *)(s), len ) ), (&SSL_write)( bio, s, len ))

#define SSL_read( bio, s, len ) \
   (srsum+= len, fprintf( stderr, "SSL_Read(%d) len=%d sum=%d\n", biocount++, len, srsum ), (&SSL_read)( bio, s, len ))

#define BIO_s_mem() call0(BIO_s_mem)
#define BIO_new(a0) call1(BIO_new,a0)

#endif
