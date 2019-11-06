;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/ssl/src/Llib/ssl.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano & Stephane Epardaud                */
;*    Creation    :  Thu Mar 24 10:24:38 2005                          */
;*    Last change :  Thu Sep 12 14:04:14 2019 (serrano)                */
;*    Copyright   :  2005-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SSL Bigloo library                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ssl_ssl
   
   (option (set! *dlopen-init-gc* #t))
   
   (extern (include "bglssl.h")
	   (include "ssl_debug.h")
	   
           (export %make-certificate "bgl_make_certificate")
           (export %make-private-key "bgl_make_private_key")
           (export %certificate-$native "bgl_certificate_native")
           (export %private-key-$native "bgl_private_key_native")
	   
           (type $private-key void* "void *")
           (type $certificate void* "void *")
	   (type $ssl-ctx void* "void *")
	   (type $ssl void* "void *")
	   (type $bio void* "void *")
	   (type $X509-store void* "void *")
	   (type $dh void* "void *")
	   (type $bignum void* "void *")
	   (type $ssl-session void* "void *")
	   (type $ssl-evp-md void* "void *")
	   (type $ssl-evp-md-ctx void* "void *")
	   (type $ssl-hmac-ctx void* "void *")
	   (type $ssl-cipher void* "void *")
	   (type $ssl-cipher-ctx void* "void *")

	   (macro $ssl-ctx-nil::$ssl-ctx "0L")
	   (macro $ssl-nil::$ssl "0L")
	   (macro $bio-nil::$bio "0L")
	   (macro $X509-store-nil::$X509-store "0L")
	   (macro $dh-nil::$dh "0L")
	   (macro $ssl-session-nil::$ssl-session "0L")
	   (macro $ssl-evp-md-nil::$ssl-evp-md "0L")
	   (macro $ssl-evp-md-ctx-nil::$ssl-evp-md-ctx "0L")
	   (macro $ssl-cipher-nil::$ssl-cipher "0L")
	   (macro $ssl-cipher-ctx-nil::$ssl-cipher-ctx "0L")

	   (macro $obj->bignum::$bignum (::obj) "(BIGNUM *)FOREIGN_TO_COBJ")
	   (macro $bignum->obj::obj ($bignum) "void_star_to_obj")
	   (macro $bignum-nil::$bignum "((BIGNUM *)0L)")
	   
	   ($ssl-version::string () "bgl_ssl_version")
           ($certificate-subject::bstring (::obj)
	      "bgl_ssl_certificate_subject")
           ($certificate-issuer::bstring (::obj)
	      "bgl_ssl_certificate_issuer")
           ($certificate-load::obj (::bstring)
	      "bgl_ssl_load_certificate")
           ($certificate-load-pem::pair-nil (::bstring)
	      "bgl_ssl_load_pem")
           ($private-key-load::obj (::bstring)
	      "bgl_ssl_load_private_key")

	   (macro $ssl-clear-error::void () "ERR_clear_error")
	   ($ssl-error-string::bstring () "bgl_ssl_error_string")
	   
	   (macro $ssl-rand-status::bool () "RAND_status")
	   (macro $ssl-rand-poll::bool () "RAND_poll")
	   (macro $ssl-rand-bytes::bool (::string ::int) "RAND_bytes")
	   (macro $ssl-rand-pseudo-bytes::bool (::string ::int) "RAND_pseudo_bytes")

	   ($ssl-socket?::bool (::obj) "bgl_ssl_socketp")
	   ($ssl-client-make-socket::obj (::bstring ::int ::int ::int
					    ::obj ::obj ::pair-nil
					    ::obj ::bstring ::bstring)
	      "bgl_make_ssl_client_socket")
	   ($ssl-client-socket-use-ssl!::socket (::socket ::int ::obj ::obj
						   ::pair-nil ::obj)
	      "bgl_client_socket_use_ssl")
	   ($ssl-server-make-socket::obj (::obj ::int ::int ::obj ::obj
					    ::pair-nil ::obj ::int ::bool)
	      "bgl_make_ssl_server_socket")

	   ($bgl-secure-context-init!::obj (::secure-context)
	      "bgl_ssl_ctx_init")
	   ($bgl-secure-context-close!::obj (::secure-context)
	      "bgl_ssl_ctx_close")

	   ($bgl-secure-context-add-crl!::bool (::secure-context ::bstring ::long ::long)
	      "bgl_ssl_ctx_add_crl")
	   ($bgl-secure-context-add-root-certs!::bool (::secure-context)
	      "bgl_ssl_ctx_add_root_certs")

	   ($bgl-secure-context-add-ca-cert!::bool (::secure-context ::bstring ::long ::long)
	      "bgl_ssl_ctx_add_ca_cert")
	   ($bgl-secure-context-set-key!::bool (::secure-context ::bstring ::long ::long ::obj)
	      "bgl_ssl_ctx_set_key")
	   ($bgl-secure-context-set-cert!::bool (::secure-context ::bstring ::long ::long)
	      "bgl_ssl_ctx_set_cert")
	   ($bgl-secure-context-set-session-id-context!::bool (::secure-context ::bstring ::long ::long)
	      "bgl_ssl_ctx_set_session_id_context")
	   ($bgl-secure-context-load-pkcs12::bool (::secure-context ::bstring ::bstring)
	      "bgl_load_pkcs12")
	   (macro $ssl-ctx-set-cipher-list::int (::$ssl-ctx ::string)
	      "SSL_CTX_set_cipher_list")
	   (macro $ssl-ctx-set-options::void (::$ssl-ctx ::int)
	      "SSL_CTX_set_options")
	   ($bgl-ssl-connection-init!::obj (::ssl-connection)
	      "bgl_ssl_connection_init")
	   
	   ($bgl-ssl-connection-start::int (::ssl-connection)
	      "bgl_ssl_connection_start")
	   ($bgl-ssl-connection-close::int (::ssl-connection)
	      "bgl_ssl_connection_close")
	   ($bgl-ssl-connection-shutdown::obj (::ssl-connection)
	      "bgl_ssl_connection_shutdown")
	   
	   ($bgl-ssl-connection-read::long (::ssl-connection ::string ::long ::long)
	      "bgl_ssl_connection_read")
	   ($bgl-ssl-connection-write::long (::ssl-connection ::string ::long ::long)
	      "bgl_ssl_connection_write")
	   ($bgl-ssl-connection-clear-in::long (::ssl-connection ::string ::long ::long)
	      "bgl_ssl_connection_clear_in")
	   ($bgl-ssl-connection-clear-out::long (::ssl-connection ::string ::long ::long)
	      "bgl_ssl_connection_clear_out")
	   ($bgl-ssl-connection-init-finished?::bool (::ssl-connection)
	      "bgl_ssl_connection_init_finishedp")
	   ($bgl-ssl-connection-enc-pending::int (::ssl-connection)
	      "bgl_ssl_connection_enc_pending")
	   ($bgl-ssl-connection-clear-pending::int (::ssl-connection)
	      "bgl_ssl_connection_clear_pending")
	   ($bgl-ssl-connection-set-session::int (::ssl-connection ::bstring)
	      "bgl_ssl_connection_set_session")
	   ($bgl-ssl-connection-get-session::obj (::ssl-connection)
	      "bgl_ssl_connection_get_session")
	   ($bgl-ssl-connection-get-current-cipher::obj (::ssl-connection)
	      "bgl_ssl_connection_get_current_cipher")
	   ($bgl-ssl-connection-load-session::int (::ssl-connection ::bstring)
	      "bgl_ssl_connection_load_session")
	   ($bgl-ssl-connection-verify-error::obj (::ssl-connection)
	      "bgl_ssl_connection_verify_error")
	   ($bgl-ssl-connection-get-peer-certificate::obj (::ssl-connection)
	      "bgl_ssl_connection_get_peer_certificate")
	   (macro $ssl-session-reused::bool (::$ssl)
	      "SSL_session_reused")
	   ($bgl-ssl-connection-get-negotiated-protocol::obj (::ssl-connection)
	      "bgl_ssl_connection_get_negotiated_protocol")
	   
	   (macro $ssl-client-sslv2::int "BGLSSL_SSLV2")
	   (macro $ssl-client-sslv3::int "BGLSSL_SSLV3")
	   (macro $ssl-client-sslv23::int "BGLSSL_SSLV23")
	   (macro $ssl-client-tlsv1::int "BGLSSL_TLSV1")
	   (macro $ssl-client-tlsv1_1::int "BGLSSL_TLSV1_1")
	   (macro $ssl-client-tlsv1_2::int "BGLSSL_TLSV1_2")
	   (macro $ssl-client-tlsv1_3::int "BGLSSL_TLSV1_3")
	   (macro $ssl-client-dtlsv1::int "BGLSSL_DTLSV1")

	   (macro $DH-GENERATOR-2::int "DH_GENERATOR_2")

	   (macro $dh-new::$dh () "DH_new")
	   (macro $dh-size::int (::$dh) "DH_size")
	   (macro $dh-generate-parameters-ex::int (::$dh ::int ::int ::long)
		  "DH_generate_parameters_ex")
	   (macro $dh-generate-key::int (::$dh)
		  "DH_generate_key")
	   (macro $dh-compute-key::int (::string ::$bignum ::$dh)
		  "DH_compute_key")
	   ($bgl-dh-check::obj (::$dh) "bgl_dh_check")
	   ($bgl-dh-check-pub-key::obj (::$dh ::$bignum) "bgl_dh_check_pub_key")

	   ($bgl-bn-bin2bn::$bignum (::string ::int) "bgl_bn_bin2bn")
	   (macro $bn-bn2bin::int (::$bignum ::string) "BN_bn2bin")
	   (macro $bn-num-bytes::int (::$bignum) "BN_num_bytes")
	   (macro $bn-free::void (::$bignum) "BN_free")
	   (macro $bn-new::$bignum () "BN_new")
	   (macro $bn-set-word::bool (::$bignum ::int) "BN_set_word")

	   ($bgl-ssl-get-ciphers::vector () "bgl_ssl_get_ciphers")
	   ($bgl-evp-get-ciphers::pair-nil () "bgl_evp_get_ciphers")
	   ($bgl-evp-get-hashes::pair-nil () "bgl_evp_get_hashes")

	   ($bgl-dh-pub-priv-key-set!::void (::$dh $bignum $bignum)
	      "bgl_dh_pub_priv_key_set")
	   ($bgl-dh-private-key::$bignum (::$dh)
	      "bgl_dh_private_key")
	   ($bgl-dh-private-key-set!::void (::$dh $bignum)
	      "bgl_dh_private_key_set")
	   ($bgl-dh-public-key::$bignum (::$dh)
	      "bgl_dh_public_key")
	   ($bgl-dh-public-key-set!::void (::$dh $bignum)
	      "bgl_dh_public_key_set")
	   ($bgl-dh-p::$bignum (::$dh)
	      "bgl_dh_p")
	   ($bgl-dh-pqg-set!::void (::$dh $bignum $bignum $bignum)
	      "bgl_dh_pqg_set")
	   ($bgl-dh-q::$bignum (::$dh)
	      "bgl_dh_p")
;* 	   ($bgl-dh-q-set!::void (::$dh $bignum)                       */
;* 	      "bgl_dh_p_set")                                          */
	   ($bgl-dh-g::$bignum (::$dh)
	      "bgl_dh_g")
;* 	   ($bgl-dh-g-set!::void (::$dh $bignum)                       */
;* 	      "bgl_dh_g_set")                                          */

	   ($bgl-ssl-hash-init::bool (::ssl-hash)
	      "bgl_ssl_hash_init")
	   ($bgl-ssl-hash-update!::bool (::ssl-hash ::bstring ::long ::long)
	      "bgl_ssl_hash_update")
	   ($bgl-ssl-hash-digest::obj (::ssl-hash)
	      "bgl_ssl_hash_digest")

	   ($bgl-ssl-hmac-init::bool (::ssl-hmac ::bstring ::bstring)
	      "bgl_ssl_hmac_init")
	   ($bgl-ssl-hmac-update!::bool (::ssl-hmac ::bstring ::long ::long)
	      "bgl_ssl_hmac_update")
	   ($bgl-ssl-hmac-digest::obj (::ssl-hmac)
	      "bgl_ssl_hmac_digest")

	   ($bgl-ssl-sign-init::bool (::ssl-sign ::bstring)
	      "bgl_ssl_sign_init")
	   ($bgl-ssl-sign-update!::bool (::ssl-sign ::bstring ::long ::long)
	      "bgl_ssl_sign_update")
	   ($bgl-ssl-sign-sign::obj (::ssl-sign ::bstring ::long ::long)
	      "bgl_ssl_sign_sign")

	   ($bgl-ssl-verify-init::bool (::ssl-verify ::bstring)
	      "bgl_ssl_verify_init")
	   ($bgl-ssl-verify-update!::bool (::ssl-verify ::bstring ::long ::long)
	      "bgl_ssl_verify_update")
	   ($bgl-ssl-verify-final::bool (::ssl-verify ::bstring ::long ::long
					   ::bstring ::long ::long)
	      "bgl_ssl_verify_final")

	   ($bgl-ssl-cipher-init::bool (::ssl-cipher ::bstring ::bstring ::long ::long ::bool)
	      "bgl_ssl_cipher_init")
	   ($bgl-ssl-cipher-initiv::bool (::ssl-cipher ::bstring ::bstring ::long ::long ::bstring ::long ::long ::bool)
	      "bgl_ssl_cipher_initiv")
	   ($bgl-ssl-cipher-update!::obj (::ssl-cipher ::bstring ::long ::long)
	      "bgl_cipher_update")
	   ($bgl-ssl-cipher-set-auto-padding::bool (::ssl-cipher ::bool)
	      "bgl_cipher_set_auto_padding")
	   ($bgl-ssl-cipher-final::bstring (::ssl-cipher)
	      "bgl_cipher_final")

	   ($bgl-pkcs5-pbkdf2-hmac-sha1::bstring (::bstring ::bstring ::int ::int)
	      "bgl_pkcs5_pbkdf2_hmac_sha1")
	   (macro $ssl-op-cipher-server-preference::int
	      "SSL_OP_CIPHER_SERVER_PREFERENCE"))
   
   (java (export %make-certificate "make_certificate")
      (export %make-private-key "make_private_key")
      (export %certificate-$native "certificate_native")
      (export %private-key-$native "private_key_native")
      
      (class $private-key
	 (method static load::obj (::bstring) "load")
	 "bigloo.ssl.private_key")
      
      (class $certificate
	 (method static load::obj (::bstring) "load")
	 (method static load-pem::obj (::bstring) "load_pem")
	 (method static subject::bstring (::obj) "subject")
	 (method static issuer::bstring (::obj) "issuer")
	 "bigloo.ssl.certificate")

      (class $ssl-ctx
	 "bigloo.ssl.ctx")
      
      (class $ssl-client
	 (constructor make-socket (::bstring ::int ::int ::int ::obj ::obj
				     ::pair-nil ::obj ::bstring ::bstring))
	 (method static socket-use-ssl!::socket (::socket ::int ::obj ::obj
						   ::pair-nil ::obj)
	    "bgl_client_socket_use_ssl")
	 (method static socket?::bool (::obj)
	    "bgl_ssl_client_socketp")
	 (field static sslv2::int "BGLSSL_SSLV2")
	 (field static sslv3::int "BGLSSL_SSLV3")
	 (field static sslv23::int "BGLSSL_SSLV23")
	 (field static tlsv1::int "BGLSSL_TLSV1")
	 (field static dtlsv1::int "BGLSSL_DTLSV1")
	 (field static tlsv1_1::int "BGLSSL_TLSV1_1")
	 (field static tlsv1_2::int "BGLSSL_TLSV1_2")
	 (field static tlsv1_3::int "BGLSSL_TLSV1_3")
	 "bigloo.ssl.ssl_client_socket")
      
      (class $ssl-server
	 (constructor make-socket (::obj ::int ::int ::obj ::obj
				     ::pair-nil ::obj ::int ::bool))
	 (method static socket?::bool (::obj)
	    "bgl_ssl_server_socketp")
	 "bigloo.ssl.ssl_server_socket"))
   
   (export (class certificate
	      ($native::$certificate read-only))
	   
	   (class private-key
	      ($native::$private-key read-only))

	   (ssl-version::string)

	   (ssl-clear-error)
	   
	   (read-private-key::private-key ::bstring)
	   (read-certificate::certificate ::bstring)
	   (read-pem-file::pair-nil ::bstring)

	   (ssl-rand-status::bool)
	   (ssl-rand-poll::bool)
	   (ssl-rand-bytes ::int)
	   (ssl-rand-pseudo-bytes ::int)
	   
	   (inline certificate-subject::bstring ::certificate)
	   (inline certificate-issuer::bstring ::certificate)
	   
	   (inline ssl-socket?::bool ::obj)
	   
	   (make-ssl-client-socket ::bstring ::int
	      #!key
	      (inbuf #t) (outbuf #t)
	      (timeout 0) (protocol 'sslv23)
	      (cert #f) (pkey #f)
	      (CAs '()) (accepted-certs #f))
	   
	   (client-socket-use-ssl! ::socket
	      #!key
	      (protocol 'sslv23)
	      (cert #f) (pkey #f)
	      (CAs '()) (accepted-certs #f))
	   
	   (make-ssl-server-socket #!optional (port 0)
	      #!key
	      (name #f) (protocol 'sslv23)
	      (cert #f) (pkey #f)
	      (CAs '()) (accepted-certs #f)
	      (backlog 5) (ipv6 #f))
	   
	   (%make-certificate::obj ::$certificate)
	   (%make-private-key::obj ::$private-key)
	   (%certificate-$native::$certificate ::obj)
	   (%private-key-$native::$private-key ::obj)

	   (generic secure-context-init ::secure-context)
	   (generic secure-context-close ::secure-context)
	   (generic secure-context-add-root-certs!::bool ::secure-context)
	   (generic secure-context-add-ca-cert!::bool ::secure-context ::bstring ::long ::long)
	   (generic secure-context-add-crl!::bool ::secure-context ::bstring ::long ::long)
	   (generic secure-context-set-key!::bool ::secure-context ::bstring ::long ::long #!optional passphrase)
	   (generic secure-context-set-cert!::bool ::secure-context ::bstring ::long ::long)
	   (generic secure-context-set-session-id-context!::bool ::secure-context ::bstring ::long ::long)
	   (generic secure-context-load-pkcs12::bool ::secure-context ::bstring ::obj)
	   (generic secure-context-set-ciphers!::bool ::secure-context ::bstring)
	   (generic secure-context-set-options!::bool ::secure-context ::int)

	   (generic ssl-connection-init ::ssl-connection)
	   (generic ssl-connection-start::int ::ssl-connection)
	   (generic ssl-connection-close::int ::ssl-connection)
	   (generic ssl-connection-shutdown::obj ::ssl-connection)
	   (generic ssl-connection-read ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-write ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-clear-in ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-clear-out ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-init-finished? ::ssl-connection)
	   (generic ssl-connection-enc-pending ::ssl-connection)
	   (generic ssl-connection-clear-pending ::ssl-connection)
	   (generic ssl-connection-set-session ::ssl-connection ::bstring)
	   (generic ssl-connection-get-session ::ssl-connection)
	   (generic ssl-connection-get-current-cipher ::ssl-connection)
	   (generic ssl-connection-load-session ::ssl-connection ::bstring)
	   (generic ssl-connection-verify-error ::ssl-connection)
	   (generic ssl-connection-get-peer-certificate ::ssl-connection)
	   (generic ssl-connection-get-negotiated-protocol ::ssl-connection)
	   (generic ssl-connection-reused?::bool ::ssl-connection)

	   (generic dh-size::int ::dh)
	   (generic dh-generate-parameters-ex ::dh ::int ::symbol)
	   (generic dh-generate-key::bool ::dh)
	   (generic dh-check ::dh)
	   (generic dh-check-pub-key ::dh ::foreign)
	   (generic dh-compute-key ::dh ::foreign)

	   (bn-bin2bn::obj ::bstring)
	   (bn-bn2bin::bstring ::foreign)
	   (bn-num-bytes::int ::foreign)
	   (bn-free::obj ::foreign)
	   (bn-new::obj)
	   (bn-set-word::bool ::foreign ::int)
	   
	   (ssl-get-ciphers::vector)
	   (evp-get-ciphers::pair-nil)
	   (evp-get-hashes::pair-nil)

	   (ssl-op-cipher-server-preference::int)
	   (pkcs5-pbkdf2-hmac-sha1::bstring ::bstring ::bstring ::int ::int)
	   )
	   
   (cond-expand
      (bigloo-c
       (export
	  (class secure-context
	     (secure-context-init)
	     ($native::$ssl-ctx (default $ssl-ctx-nil))
	     ($ca-store::$X509-store (default $X509-store-nil))
	     (method::bstring read-only (default "default")))
	  
	  (class ssl-connection
	     (ssl-connection-init)
	     ($native::$ssl (default $ssl-nil))
	     ($bio-read::$bio (default $bio-nil))
	     ($bio-write::$bio (default $bio-nil))
	     ($next-session::$ssl-session (default $ssl-session-nil))
	     (ctx::secure-context read-only)
	     (isserver::bool read-only)
	     (request-cert::bool read-only (default #f))
	     (server-name::obj read-only (default #f))
	     (reject-unauthorized::bool read-only)
	     (info-callback (default #f))
	     (sni-context-callback (default #f))
	     (newsession-callback::procedure read-only (default list))
	     (selected-npn-protos::obj (default #unspecified))
	     (npn-protos::obj (default #unspecified))
	     (err::obj (default #f))
	     (received-shutdown::bool (default #f))
	     (sent-shutdown::bool (default #f)))
	  
	  (class dh
	     (dh-init)
	     ($native::$dh (default $dh-nil))
	     (%p::$bignum (default $bignum-nil))
	     (%q::$bignum (default $bignum-nil))
	     (%g::$bignum (default $bignum-nil))
	     (%pub::$bignum (default $bignum-nil))
	     (%priv::$bignum (default $bignum-nil))
	     (p
		(get (lambda (o::dh)
			(with-access::dh o (%p $native)
			   ($bignum->obj ($bgl-dh-p $native)))))
		(set (lambda (o::dh v::foreign)
			(with-access::dh o ($native %p %q %g)
			   (set! %p ($obj->bignum v))
			   ($bgl-dh-pqg-set! $native %p %q %g)
			   v))))
	     (q
		(get (lambda (o::dh)
			(with-access::dh o ($native)
			   ($bignum->obj ($bgl-dh-q $native)))))
		(set (lambda (o::dh v::foreign)
			(with-access::dh o ($native %p %q %g)
			   (set! %q ($obj->bignum v))
			   ($bgl-dh-pqg-set! $native %p %q %g)
			   v))))
	     (g
		(get (lambda (o::dh)
			(with-access::dh o ($native)
			   ($bignum->obj ($bgl-dh-g $native)))))
		(set (lambda (o::dh v::foreign)
			(with-access::dh o ($native)
			   (with-access::dh o ($native %p %q %g)
			      (set! %g ($obj->bignum v))
			      ($bgl-dh-pqg-set! $native %p %q %g)
			      v)))))
	     (private-key
		(get (lambda (o::dh)
			(with-access::dh o (%priv $native)
			   ($bignum->obj ($bgl-dh-private-key $native)))))
		(set (lambda (o::dh v::foreign)
			(with-access::dh o ($native %pub %priv)
			   (set! %priv ($obj->bignum v))
			   ($bgl-dh-pub-priv-key-set! $native %pub %priv)
			   v))))
	     (public-key
		(get (lambda (o::dh)
			(with-access::dh o (%pub $native)
			   ($bignum->obj ($bgl-dh-public-key $native)))))
		(set (lambda (o::dh v::foreign)
			(with-access::dh o ($native %pub %priv)
			   (set! %pub ($obj->bignum v))
			   ($bgl-dh-pub-priv-key-set! $native %pub %priv)
			   v)))))
	  
	  (class ssl-hash
	     (ssl-hash-init)
	     ($md::$ssl-evp-md (default $ssl-evp-md-nil))
	     ($md-ctx::$ssl-evp-md-ctx (default $ssl-evp-md-ctx-nil))
	     (type::bstring read-only))
	  
	  (generic ssl-hash-init ::ssl-hash)
	  (generic ssl-hash-update! ::ssl-hash ::bstring ::long ::long)
	  (generic ssl-hash-digest ::ssl-hash)
	  
	  (class ssl-hmac
	     ($md::$ssl-evp-md (default $ssl-evp-md-nil))
	     ($md-ctx::$ssl-hmac-ctx (default $ssl-evp-md-ctx-nil)))
	  
	  (generic ssl-hmac-init ::ssl-hmac ::bstring ::bstring)
	  (generic ssl-hmac-update! ::ssl-hmac ::bstring ::long ::long)
	  (generic ssl-hmac-digest ::ssl-hmac)

	  (class ssl-sign
	     ($md::$ssl-evp-md (default $ssl-evp-md-nil))
	     ($md-ctx::$ssl-hmac-ctx (default $ssl-evp-md-ctx-nil)))

	  (generic ssl-sign-init ::ssl-sign ::bstring)
	  (generic ssl-sign-update! ::ssl-sign ::bstring ::long ::long)
	  (generic ssl-sign-sign ::ssl-sign ::bstring ::long ::long)
	  
	  (class ssl-verify
	     ($md::$ssl-evp-md (default $ssl-evp-md-nil))
	     ($md-ctx::$ssl-hmac-ctx (default $ssl-evp-md-ctx-nil)))

	  (generic ssl-verify-init ::ssl-verify ::bstring)
	  (generic ssl-verify-update! ::ssl-verify ::bstring ::long ::long)
	  (generic ssl-verify-final::bool ::ssl-verify ::bstring ::long ::long ::bstring ::long ::long)

	  (class ssl-cipher
	     ($cipher::$ssl-cipher (default $ssl-cipher-nil))
	     ($cipher-ctx::$ssl-cipher-ctx (default $ssl-cipher-ctx-nil)))

	  (generic ssl-cipher-init ::ssl-cipher ::bstring
	     ::bstring ::long ::long
	     ::bool)
	  (generic ssl-cipher-initiv ::ssl-cipher ::bstring
	     ::bstring ::long ::long
	     ::bstring ::long ::long
	     ::bool)
	  (generic ssl-cipher-update!::bstring ::ssl-cipher ::bstring ::long ::long)
	  (generic ssl-cipher-set-auto-padding::bool ::ssl-cipher ::bool)
	  (generic ssl-cipher-final::bstring ::ssl-cipher)

;* 	  (class ssl-decipher                                          */
;* 	     ($md::$ssl-evp-md (default $ssl-evp-md-nil))              */
;* 	     ($md-ctx::$ssl-hmac-ctx (default $ssl-evp-md-ctx-nil)))   */
;*                                                                     */
;* 	  (generic ssl-decipher-init ::ssl-decipher ::bstring)         */
;* 	  (generic ssl-decipher-initiv ::ssl-decipher ::bstring)       */
;* 	  (generic ssl-decipher-update! ::ssl-decipher ::bstring ::long ::long) */
;* 	  (generic ssl-decipher-set-auto-padding::bool ::ssl-decipher) */
;* 	  (generic ssl-decipher-final::bool ::ssl-decipher)            */
;* 	  (generic ssl-decipher-finaltol::bool ::ssl-decipher)         */

	  ))
      (else
       (export
	  (class secure-context
	     (method::bstring read-only (default "SSLv23_method")))

	  (class ssl-connection
	     (ctx::secure-context read-only)
	     (issserver::bool read-only)
	     (sni-context-callback (default #f))
	     (info-callback (default #f))
	     (newsession-callback::procedure read-only))

	  (class dh)
	  (class ssl-hash)
	  (class ssl-hmac)
	  (class ssl-sign)
	  (class ssl-verify)
	  (class ssl-cipher)
	  ))))

;*---------------------------------------------------------------------*/
;*    ssl-version ...                                                  */
;*---------------------------------------------------------------------*/
(define (ssl-version)
   (cond-expand
      (bigloo-c ($ssl-version))
      (else "-1")))

;*---------------------------------------------------------------------*/
;*    ssl-clear-error ...                                              */
;*---------------------------------------------------------------------*/
(define (ssl-clear-error)
   (cond-expand
      (bigloo-c (begin ($ssl-clear-error) #t))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    sanity-args-checks ...                                           */
;*---------------------------------------------------------------------*/
(define (sanity-args-checks func cert pkey CAs accepted-certs)
   (unless (or (not cert) (isa? cert certificate))
      (error func "Invalid certificate" cert))
   (unless (or (not pkey) (isa? pkey private-key))
      (error func "Invalid private key" pkey))
   (unless (and (list? CAs) (every (lambda (c) (isa? c certificate)) CAs))
      (error func "Invalid CA list" CAs))
   (unless (or (not accepted-certs)
	       (and (list? accepted-certs)
		    (every (lambda (c) (isa? c certificate)) accepted-certs)))
      (error func "Invalid accepted-certs" accepted-certs))
   (if (or (and (isa? cert certificate) (not (isa? pkey private-key)))
	   (and (isa? pkey private-key) (not (isa? cert certificate))))
       (error func
	      "pkey and cert must be both #f or both set"
	      (list pkey cert))))

;*---------------------------------------------------------------------*/
;*    make-ssl-client-socket ...                                       */
;*---------------------------------------------------------------------*/
(define (make-ssl-client-socket hostname port
	   #!key
	   (inbuf #t) (outbuf #t)
	   (timeout 0)
	   (protocol 'sslv23)
	   (cert #f) (pkey #f)
	   (CAs '()) (accepted-certs #f))
   (sanity-args-checks 'make-ssl-client-socket cert pkey CAs accepted-certs)
   (%socket-init!)
   ($ssl-client-make-socket hostname port timeout
      (ssl-protocols->integer protocol)
      cert pkey
      CAs accepted-certs
      (get-port-buffer 'make-ssl-client-socket
	 inbuf
	 c-default-io-bufsiz)
      (get-port-buffer 'make-ssl-client-socket
	 outbuf
	 c-default-io-bufsiz)))

;*---------------------------------------------------------------------*/
;*    client-socket-use-ssl! ...                                       */
;*---------------------------------------------------------------------*/
(define (client-socket-use-ssl! s #!key
	   (protocol 'sslv23)
	   (cert #f) (pkey #f)
	   (CAs '()) (accepted-certs #f))
   (sanity-args-checks 'client-socket-use-ssl! cert pkey CAs accepted-certs)
   (%socket-init!)
   ($ssl-client-socket-use-ssl! s
      (ssl-protocols->integer protocol)
      cert pkey
      CAs accepted-certs))
   
;*---------------------------------------------------------------------*/
;*    ssl-socket? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (ssl-socket? obj)
   (cond-expand
      (bigloo-c ($ssl-socket? obj))
      (else (or ($ssl-client-socket? obj) ($ssl-server-socket? obj)))))

;*---------------------------------------------------------------------*/
;*    make-ssl-server-socket ...                                       */
;*---------------------------------------------------------------------*/
(define (make-ssl-server-socket #!optional (port 0) #!key (name #f)
	   (protocol 'sslv23)
	   (cert #f) (pkey #f)
	   (CAs '()) (accepted-certs #f)
	   (backlog 5)
	   (ipv6 #f))
   (sanity-args-checks 'make-ssl-server-socket cert pkey CAs accepted-certs)
   (%socket-init!)
   ($ssl-server-make-socket name port (ssl-protocols->integer protocol)
      cert pkey
      CAs accepted-certs backlog ipv6))

;*---------------------------------------------------------------------*/
;*    ssl-protocols->integer ...                                       */
;*---------------------------------------------------------------------*/
(define (ssl-protocols->integer protocol)
   (case (string->symbol (string-downcase (symbol->string! protocol)))
      ((sslv2) $ssl-client-sslv2)
      ((sslv3) $ssl-client-sslv3)
      ((ssl sslv23) $ssl-client-sslv23)
      ((tls tlsv1) $ssl-client-tlsv1)
      ((tlsv1_1) $ssl-client-tlsv1_1)
      ((tlsv1_2) $ssl-client-tlsv1_2)
      ((tlsv1_3) $ssl-client-tlsv1_3)
      ((dtls dtlsv1) $ssl-client-dtlsv1)
      (else (error "ssl" "Unknown protocols" protocol))))

;*---------------------------------------------------------------------*/
;*    read-private-key ...                                             */
;*---------------------------------------------------------------------*/
(define (read-private-key::private-key file::bstring)
   ($private-key-load file))

;*---------------------------------------------------------------------*/
;*    read-certificate ...                                             */
;*---------------------------------------------------------------------*/
(define (read-certificate::certificate file::bstring)
   ($certificate-load file))

;*---------------------------------------------------------------------*/
;*    read-pem-file ...                                                */
;*---------------------------------------------------------------------*/
(define (read-pem-file::pair-nil file::bstring)
   ($certificate-load-pem file))

;*---------------------------------------------------------------------*/
;*    ssl-rand-status ...                                              */
;*---------------------------------------------------------------------*/
(define (ssl-rand-status)
   (cond-expand
      (bigloo-c ($ssl-rand-status))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    ssl-rand-poll ...                                                */
;*---------------------------------------------------------------------*/
(define (ssl-rand-poll)
   (cond-expand
      (bigloo-c ($ssl-rand-poll))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    ssl-rand-bytes ...                                               */
;*---------------------------------------------------------------------*/
(define (ssl-rand-bytes sz::int)
   (let ((str (make-string sz)))
      (cond-expand
	 (bigloo-c
	  ($ssl-rand-bytes str sz)
	  str)
	 (else
	  (let loop ((i 0))
	     (if (=fx i sz)
		 str
		 (begin
		    (string-set! str i (integer->char (random 255)))
		    (loop (+fx i 1)))))))))
   
;*---------------------------------------------------------------------*/
;*    ssl-rand-pseudo-bytes ...                                        */
;*---------------------------------------------------------------------*/
(define (ssl-rand-pseudo-bytes sz::int)
   (let ((str (make-string sz)))
      (cond-expand
	 (bigloo-c
	  ($ssl-rand-pseudo-bytes str sz)
	  str)
	 (else
	  (let loop ((i 0))
	     (if (=fx i sz)
		 str
		 (begin
		    (string-set! str i (integer->char (random 255)))
		    (loop (+fx i 1)))))))))
   
;*---------------------------------------------------------------------*/
;*    certificate-subject ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (certificate-subject::bstring cert::certificate)
   ($certificate-subject cert))

;*---------------------------------------------------------------------*/
;*    certificate-issuer ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (certificate-issuer::bstring cert::certificate)
   ($certificate-issuer cert))

;*---------------------------------------------------------------------*/
;*    %make-certificate ...                                            */
;*---------------------------------------------------------------------*/
(define (%make-certificate cert)
   (instantiate::certificate
      ($native cert)))

;*---------------------------------------------------------------------*/
;*    %make-private-key ...                                            */
;*---------------------------------------------------------------------*/
(define (%make-private-key pkey)
   (instantiate::private-key
      ($native pkey)))

;*---------------------------------------------------------------------*/
;*    %certificate-$native ...                                         */
;*---------------------------------------------------------------------*/
(define (%certificate-$native cert)
   (with-access::certificate cert ($native)
      $native))

;*---------------------------------------------------------------------*/
;*    %private-key-$native ...                                         */
;*---------------------------------------------------------------------*/
(define (%private-key-$native pkey)
   (with-access::private-key pkey ($native)
      $native))

;*---------------------------------------------------------------------*/
;*    secure-context-init ::secure-context ...                         */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-init sc::secure-context)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-init! sc))
      (else
       sc)))

;*---------------------------------------------------------------------*/
;*    secure-context-close ::secure-context ...                        */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-close sc::secure-context)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-close! sc))
      (else
       sc)))

;*---------------------------------------------------------------------*/
;*    secure-context-add-root-certs! ::secure-context ...              */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-add-root-certs! sc::secure-context)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-add-root-certs! sc))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-add-ca-cert! ::secure-context ...                 */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-add-ca-cert! sc::secure-context cert offset len)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-add-ca-cert! sc cert offset len))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-add-crl! ::secure-context ...                     */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-add-crl! sc::secure-context cert::bstring offset::long len::long)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-add-crl! sc cert offset len))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-set-key! ::secure-context ...                     */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-set-key! sc::secure-context cert offset len #!optional passphrase)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-set-key! sc cert offset len passphrase))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-set-cert! ::secure-context ...                    */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-set-cert! sc::secure-context cert offset len)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-set-cert! sc cert offset len))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-set-session-id! ::secure-context ...              */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-set-session-id-context! sc::secure-context sic offset len)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-set-session-id-context! sc sic offset len))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-set-session-id! ::secure-context ...              */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-load-pkcs12 sc::secure-context pfx pass)
   (cond-expand
      (bigloo-c
       ($bgl-secure-context-load-pkcs12 sc pfx pass))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-set-ciphers! ::secure-context ...                 */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-set-ciphers! sc::secure-context ciphers)
   (cond-expand
      (bigloo-c
       (with-access::secure-context sc ($native)
	  ($ssl-ctx-set-cipher-list $native ciphers))
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    secure-context-set-options! ::secure-context ...                 */
;*---------------------------------------------------------------------*/
(define-generic (secure-context-set-options! sc::secure-context options)
   (cond-expand
      (bigloo-c
       (with-access::secure-context sc ($native)
	  ($ssl-ctx-set-options $native options))
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-init ::ssl-connection ...                         */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-init ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-init! ssl))
      (else
       ssl)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-start ::ssl-connection ...                        */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-start ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-start ssl))
      (else
       ssl)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-close ::ssl-connection ...                        */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-close ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-close ssl))
      (else
       ssl)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-shutdown ::ssl-connection ...                     */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-shutdown ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-shutdown ssl))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    ssl-connection-read ::ssl-connection ...                         */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-read ssl::ssl-connection buffer offset len)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-read ssl buffer offset len))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-write ::ssl-connection ...                        */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-write ssl::ssl-connection buffer offset len)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-write ssl buffer offset len))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-clear-in ::ssl-connection ...                     */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-clear-in ssl::ssl-connection buffer offset len)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-clear-in ssl buffer offset len))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-clear-out ::ssl-connection ...                    */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-clear-out ssl::ssl-connection buffer offset len)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-clear-out ssl buffer offset len))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-init-finished? ::ssl-connection ...               */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-init-finished? ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-init-finished? ssl))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-enc-pending ::ssl-connection ...                  */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-enc-pending ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-enc-pending ssl))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-clear-pending ::ssl-connection ...                */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-clear-pending ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-clear-pending ssl))
      (else
       0)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-set-session ::ssl-connection ...                  */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-set-session ssl::ssl-connection buf::bstring)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-set-session ssl buf))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-get-session ::ssl-connection ...                  */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-get-session ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-get-session ssl))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-get-current-cipher ::ssl-connection ...           */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-get-current-cipher ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-get-current-cipher ssl))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-load-session ::ssl-connection ...                 */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-load-session ssl::ssl-connection buf::bstring)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-load-session ssl buf))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-verify-error ::ssl-connection ...                 */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-verify-error ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-verify-error ssl))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-get-peer-certificate ::ssl-connection ...         */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-get-peer-certificate ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-get-peer-certificate ssl))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-reused? ::ssl-connection ...                      */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-reused? ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       (with-access::ssl-connection ssl ($native)
	  ($ssl-session-reused $native)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-connection-get-negotiated-protocol ::ssl-connection ...      */
;*---------------------------------------------------------------------*/
(define-generic (ssl-connection-get-negotiated-protocol ssl::ssl-connection)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-connection-get-negotiated-protocol ssl))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dh-init ::dh ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (dh-init dh::dh)
   (cond-expand
      (bigloo-c
       (with-access::dh dh ($native)
	  (set! $native ($dh-new))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dh-size ::dh ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (dh-size::int dh::dh)
   (cond-expand
      (bigloo-c (with-access::dh dh ($native) ($dh-size $native)))
      (else 0)))
   
;*---------------------------------------------------------------------*/
;*    dh-generate-parameters-ex ::dh ...                               */
;*---------------------------------------------------------------------*/
(define-generic (dh-generate-parameters-ex dh::dh len::int generator::symbol)
   (cond-expand
      (bigloo-c
       (with-access::dh dh ($native)
	  ($dh-generate-parameters-ex $native len
	     (case generator
		((DH-GENERATOR-2) $DH-GENERATOR-2)
		(else (error "dh-generate-parameters-ex" "Illegal generator"
			 generator)))
	     0)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dh-check ::dh ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (dh-check dh::dh)
   (cond-expand
      (bigloo-c
       (with-access::dh dh ($native)
	  ($bgl-dh-check $native)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dh-check-pub-key ::dh ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (dh-check-pub-key dh::dh key::foreign)
   (cond-expand
      (bigloo-c
       (with-access::dh dh ($native)
	  ($bgl-dh-check-pub-key $native ($obj->bignum key))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dh-compute-key ::dh ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (dh-compute-key dh::dh key::foreign)
   (cond-expand
      (bigloo-c
       (with-access::dh dh ($native)
	  (let* ((size (dh-size dh))
		 (str (make-string size)))
	     (let ((sz ($dh-compute-key str ($obj->bignum key) $native)))
		(cond
		   ((=fx sz -1)
		    #f)
		   ((=fx sz (string-length str))
		    str)
		   (else
		    (blit-string! str 0 str (-fx size sz) sz)
		    (let loop ((i (-fx size sz)))
		       (when (>fx i 0)
			  (let ((ni (-fx i 1)))
			     (string-set! str ni #a000)
			     (loop ni))))
		    str))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dh-generate-key ::dh ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (dh-generate-key dh::dh)
   (cond-expand
      (bigloo-c
       (with-access::dh dh ($native)
	  (not (=fx ($dh-generate-key $native) 0))))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    ssl-hash-init ::ssl-hash ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (ssl-hash-init ssl-hash::ssl-hash)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-hash-init ssl-hash)
	  (error "ssl-hash" "Digest method not supported" ssl-hash))
       ssl-hash)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-hash-update! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (ssl-hash-update! ssl-hash::ssl-hash data::bstring offset len)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-hash-update! ssl-hash data offset len)
	  (error "ssl-hash-update!" "cannot update" ssl-hash))
       ssl-hash)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-hash-digest ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (ssl-hash-digest ssl-hash::ssl-hash)
   (cond-expand
      (bigloo-c
       (let ((r ($bgl-ssl-hash-digest ssl-hash)))
	  (or r (error "ssl-hash-digest" "cannot digest" ssl-hash))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-hmac-init ::ssl-hmac ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (ssl-hmac-init ssl-hmac::ssl-hmac hmac key)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-hmac-init ssl-hmac hmac key)
	  (error "ssl-hmac" "Digest method not supported" ssl-hmac)
	  ssl-hmac))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-hmac-update! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (ssl-hmac-update! ssl-hmac::ssl-hmac data::bstring offset len)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-hmac-update! ssl-hmac data offset len)
	  (error "ssl-hmac-update!" "cannot update" ssl-hmac))
       ssl-hmac)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-hmac-digest ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (ssl-hmac-digest ssl-hmac::ssl-hmac)
   (cond-expand
      (bigloo-c
       (let ((r ($bgl-ssl-hmac-digest ssl-hmac)))
	  (or r (error "ssl-hmac-digest" "cannot digest" ssl-hmac))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-sign-init ::ssl-sign ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (ssl-sign-init ssl-sign::ssl-sign sign)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-sign-init ssl-sign sign)
	  (error "ssl-sign" "Sign method not supported" ssl-sign))
       ssl-sign)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-sign-update! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (ssl-sign-update! ssl-sign::ssl-sign data::bstring offset len)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-sign-update! ssl-sign data offset len)
	  (error "ssl-sign-update!" "cannot update" ssl-sign))
       ssl-sign)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-sign-sign ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (ssl-sign-sign ssl-sign::ssl-sign key::bstring offset len)
   (cond-expand
      (bigloo-c
       (let ((r ($bgl-ssl-sign-sign ssl-sign key offset len)))
	  (or r (error "ssl-sign-sign" "cannot sign" ssl-sign))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-verify-init ::ssl-verify ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (ssl-verify-init ssl-verify::ssl-verify verify)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-verify-init ssl-verify verify)
	  (error "ssl-verify" "Verify method not supported" ssl-verify))
       ssl-verify)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-verify-update! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (ssl-verify-update! ssl-verify::ssl-verify data::bstring offset len)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-verify-update! ssl-verify data offset len)
	  (error "ssl-verify-update!" "cannot update" ssl-verify))
       ssl-verify)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-verify-final ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (ssl-verify-final ssl-verify::ssl-verify
		   key::bstring koffset klen
		   sig::bstring soffset slen)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-verify-final ssl-verify
	  key koffset klen
	  sig soffset slen))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-cipher-init ::ssl-cipher ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (ssl-cipher-init ssl-cipher::ssl-cipher cipher
		   key offset len
		   enc)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-cipher-init ssl-cipher cipher key offset len enc)
	  (error "ssl-cipher-init" ($ssl-error-string) cipher))
       ssl-cipher)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-cipher-initiv ::ssl-cipher ...                               */
;*---------------------------------------------------------------------*/
(define-generic (ssl-cipher-initiv ssl-cipher::ssl-cipher cipher
		   key offset len
		   iv ivoffset ivlen
		   enc)
   (cond-expand
      (bigloo-c
       (unless ($bgl-ssl-cipher-initiv ssl-cipher cipher
		  key offset len
		  iv ivoffset ivlen
		  enc)
	  (error "ssl-cipher-initiv" ($ssl-error-string) ssl-cipher)))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-cipher-update! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (ssl-cipher-update! ssl-cipher::ssl-cipher data::bstring offset len)
   (cond-expand
      (bigloo-c
       (or ($bgl-ssl-cipher-update! ssl-cipher data offset len)
	   (error "ssl-cipher-update!" "cannot update" ssl-cipher)))
      (else
       (error "ssl-cipher-update!" "not support" ssl-cipher))))

;*---------------------------------------------------------------------*/
;*    ssl-cipher-set-auto-padding ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (ssl-cipher-set-auto-padding ssl-cipher::ssl-cipher auto-padding)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-cipher-set-auto-padding ssl-cipher auto-padding))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssl-cipher-final ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (ssl-cipher-final ssl-cipher::ssl-cipher)
   (cond-expand
      (bigloo-c
       ($bgl-ssl-cipher-final ssl-cipher))
      (else
       (error "ssl-cipher-final" "not supported" ssl-cipher))))

;*---------------------------------------------------------------------*/
;*    bn-bin2bn ...                                                    */
;*---------------------------------------------------------------------*/
(define (bn-bin2bn::obj string)
   (cond-expand
      (bigloo-c ($bignum->obj ($bgl-bn-bin2bn string (string-length string))))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bn-bn2bin ...                                                    */
;*---------------------------------------------------------------------*/
(define (bn-bn2bin bn::foreign)
   (cond-expand
      (bigloo-c
       (let* ((sz (bn-num-bytes bn))
	      (st (make-string sz)))
	  ($bn-bn2bin ($obj->bignum bn) st)
	  st))
      (else "")))
   
;*---------------------------------------------------------------------*/
;*    bn-num-bytes ...                                                 */
;*---------------------------------------------------------------------*/
(define (bn-num-bytes bn)
   (cond-expand
      (bigloo-c ($bn-num-bytes ($obj->bignum bn)))
      (else 0)))
   
;*---------------------------------------------------------------------*/
;*    bn-free ...                                                      */
;*---------------------------------------------------------------------*/
(define (bn-free bn)
   (cond-expand
      (bigloo-c ($bn-free ($obj->bignum bn))))
   #unspecified)
   
;*---------------------------------------------------------------------*/
;*    bn-new ...                                                       */
;*---------------------------------------------------------------------*/
(define (bn-new)
   (cond-expand
      (bigloo-c ($bignum->obj ($bn-new)))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bn-set-word ...                                                  */
;*---------------------------------------------------------------------*/
(define (bn-set-word bn w)
   (cond-expand
      (bigloo-c ($bn-set-word ($obj->bignum bn) w))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    ssl-get-ciphers ...                                              */
;*---------------------------------------------------------------------*/
(define (ssl-get-ciphers)
   (cond-expand
      (bigloo-c ($bgl-ssl-get-ciphers))
      (else '#())))

;*---------------------------------------------------------------------*/
;*    evp-get-ciphers ...                                              */
;*---------------------------------------------------------------------*/
(define (evp-get-ciphers)
   (cond-expand
      (bigloo-c ($bgl-evp-get-ciphers))
      (else '())))

;*---------------------------------------------------------------------*/
;*    evp-get-hashes ...                                               */
;*---------------------------------------------------------------------*/
(define (evp-get-hashes)
   (cond-expand
      (bigloo-c ($bgl-evp-get-hashes))
      (else '())))

;*---------------------------------------------------------------------*/
;*    pkcs5-pbkdf2-hmac-sha1 ...                                       */
;*---------------------------------------------------------------------*/
(define (pkcs5-pbkdf2-hmac-sha1::bstring pass salt iter keylen)
   (cond-expand
      (bigloo-c
       ($bgl-pkcs5-pbkdf2-hmac-sha1 pass salt iter keylen))
      (else
       (error "pkcs5-pbkdf2-hmac-sha1" "not supporter" pass))))
		 
;*---------------------------------------------------------------------*/
;*    constants ...                                                    */
;*---------------------------------------------------------------------*/
(define (ssl-op-cipher-server-preference)
   (cond-expand (bigloo-c $ssl-op-cipher-server-preference) (else 0)))


