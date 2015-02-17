;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/ssl/src/Llib/ssl.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano & Stephane Epardaud                */
;*    Creation    :  Thu Mar 24 10:24:38 2005                          */
;*    Last change :  Sat Feb 14 10:04:47 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SSL Bigloo library                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ssl_ssl
   
   (option (set! *dlopen-init-gc* #t))
   
   (extern (include "bglssl.h")
	   
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

	   (macro $ssl-ctx-nil::$ssl-ctx "0L")
	   (macro $ssl-nil::$ssl "0L")
	   (macro $bio-nil::$bio "0L")
	   (macro $X509-store-nil::$X509-store "0L")

	   ($ssl-version::string () "bgl_ssl_version")
           ($certificate-subject::bstring (::certificate)
	      "bgl_ssl_certificate_subject")
           ($certificate-issuer::bstring (::certificate)
	      "bgl_ssl_certificate_issuer")
           ($certificate-load::certificate (::bstring)
	      "bgl_ssl_load_certificate")
           ($certificate-load-pem::pair-nil (::bstring)
	      "bgl_ssl_load_pem")
           ($private-key-load::private-key (::bstring)
	      "bgl_ssl_load_private_key")

	   (macro $ssl-rand-status::bool () "RAND_status")
	   (macro $ssl-rand-poll::bool () "RAND_poll")
	   (macro $ssl-rand-bytes::bool (::string ::int) "RAND_bytes")
	   (macro $ssl-rand-pseudo-bytes::bool (::string ::int) "RAND_pseudo_bytes")
	   
	   ($ssl-client-make-socket::obj (::bstring ::int ::int ::int
					    ::obj ::obj ::pair-nil
					    ::obj ::bstring ::bstring)
	      "bgl_make_ssl_client_socket")
	   ($ssl-client-socket-use-ssl!::socket (::socket ::int ::obj ::obj
						   ::pair-nil ::obj)
	      "bgl_client_socket_use_ssl")
	   ($ssl-server-make-socket::obj (::obj ::int ::int ::obj ::obj
					    ::pair-nil ::obj ::int)
	      "bgl_make_ssl_server_socket")

	   ($bgl-secure-context-init!::obj (::secure-context)
	      "bgl_ssl_ctx_init")
	   
	   ($bgl-secure-context-add-root-certs!::bool (::secure-context)
	      "bgl_ssl_ctx_add_root_certs")

	   ($bgl-secure-context-add-ca-cert!::bool (::secure-context ::bstring ::long ::long)
	      "bgl_ssl_ctx_add_ca_cert")
	   ($bgl-secure-context-set-key!::bool (::secure-context ::bstring ::long ::long ::obj) "bgl_ssl_set_key")
	   ($bgl-secure-context-set-cert!::bool (::secure-context ::bstring ::long ::long) "bgl_ssl_set_cert")
	   (macro $ssl-ctx-set-cipher-list::void (::$ssl-ctx ::string)
	      "SSL_CTX_set_cipher_list")
	   (macro $ssl-ctx-set-options::void (::$ssl-ctx ::int)
	      "SSL_CTX_set_options")
	   ($bgl-ssl-connection-init!::obj (::ssl-connection)
	      "bgl_ssl_connection_init")
	   
	   ($bgl-ssl-connection-start::int (::ssl-connection)
	      "bgl_ssl_connection_start")
	   
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
	   ($bgl-ssl-connection-verify-error::obj (::ssl-connection)
	      "bgl_ssl_connection_verify_error")
	   ($bgl-ssl-connection-get-peer-certificate::obj (::ssl-connection)
	      "bgl_ssl_connection_get_peer_certificate")
	   
	   (macro $ssl-client-sslv2::int "BGLSSL_SSLV2")
	   (macro $ssl-client-sslv3::int "BGLSSL_SSLV3")
	   (macro $ssl-client-sslv23::int "BGLSSL_SSLV23")
	   (macro $ssl-client-tlsv1::int "BGLSSL_TLSV1")
	   (macro $ssl-client-dtlsv1::int "BGLSSL_DTLSV1"))
   
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
	 (field static sslv2::int "BGLSSL_SSLV2")
	 (field static sslv3::int "BGLSSL_SSLV3")
	 (field static sslv23::int "BGLSSL_SSLV23")
	 (field static tlsv1::int "BGLSSL_TLSV1")
	 (field static dtlsv1::int "BGLSSL_DTLSV1")
	 "bigloo.ssl.ssl_client_socket")
      
      (class $ssl-server
	 (constructor make-socket (::obj ::int ::int ::obj ::obj
				     ::pair-nil ::obj ::int))
	 "bigloo.ssl.ssl_server_socket"))
   
   (export (class certificate
	      ($native::$certificate read-only))
	   
	   (class private-key
	      ($native::$private-key read-only))

	   (ssl-version::string)
	   
	   (read-private-key::private-key ::bstring)
	   (read-certificate::certificate ::bstring)
	   (read-pem-file::pair-nil ::bstring)

	   (inline ssl-rand-status::bool)
	   (inline ssl-rand-poll::bool)
	   (ssl-rand-bytes ::int)
	   (ssl-rand-pseudo-bytes ::int)
	   
	   (inline certificate-subject::bstring ::certificate)
	   (inline certificate-issuer::bstring ::certificate)
	   
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
	      (backlog 5))
	   
	   (%make-certificate::obj ::$certificate)
	   (%make-private-key::obj ::$private-key)
	   (%certificate-$native::$certificate ::obj)
	   (%private-key-$native::$private-key ::obj)

	   (generic secure-context-init ::secure-context)
	   (generic secure-context-add-root-certs!::bool ::secure-context)
	   (generic secure-context-add-ca-cert!::bool ::secure-context ::bstring ::long ::long)
	   (generic secure-context-set-key!::bool ::secure-context ::bstring ::long ::long #!optional passphrase)
	   (generic secure-context-set-cert!::bool ::secure-context ::bstring ::long ::long)
	   (generic secure-context-set-ciphers!::bool ::secure-context ::bstring)
	   (generic secure-context-set-options!::bool ::secure-context ::int)

	   (generic ssl-connection-init ::ssl-connection)
	   (generic ssl-connection-start::int ::ssl-connection)
	   (generic ssl-connection-read ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-write ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-clear-in ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-clear-out ::ssl-connection ::bstring ::long ::long)
	   (generic ssl-connection-init-finished? ::ssl-connection)
	   (generic ssl-connection-enc-pending ::ssl-connection)
	   (generic ssl-connection-clear-pending ::ssl-connection)
	   (generic ssl-connection-set-session ssl::ssl-connection ::bstring)
	   (generic ssl-connection-verify-error ::ssl-connection)
	   (generic ssl-connection-get-peer-certificate ::ssl-connection))
	   
   
   (cond-expand
      (bigloo-c
       (export
	  (class secure-context
	     (secure-context-init)
	     ($native::$ssl-ctx (default $ssl-ctx-nil))
	     ($ca-store::$X509-store (default $X509-store-nil))
	     (method::bstring read-only (default "SSLv23_method")))

	  (class ssl-connection
	      (ssl-connection-init)
	      ($native::$ssl (default $ssl-nil))
	      ($bio-read::$bio (default $bio-nil))
	      ($bio-write::$bio (default $bio-nil))
	      (ctx::secure-context read-only)
	      (isserver::bool read-only)
	      (request-cert::bool read-only (default #f))
	      (server-name::obj read-only (default #f))
	      (reject-unauthorized::bool read-only)
	      (info-callback (default #f)))))
      (else
       (export
	  (class secure-context
	     (method::bstring read-only (default "SSLv23_method")))

	  (class ssl-connection
	     (ctx::secure-context read-only)
	     (issserver::bool read-only))))))

;*---------------------------------------------------------------------*/
;*    ssl-version ...                                                  */
;*---------------------------------------------------------------------*/
(define (ssl-version)
   (cond-expand
      (bigloo-c ($ssl-version))
      (else "-1")))

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
;*    make-ssl-server-socket ...                                       */
;*---------------------------------------------------------------------*/
(define (make-ssl-server-socket #!optional (port 0) #!key (name #f)
                                (protocol 'sslv23)
                                (cert #f) (pkey #f)
                                (CAs '()) (accepted-certs #f)
				(backlog 5))
   (sanity-args-checks 'make-ssl-server-socket cert pkey CAs accepted-certs)
   (%socket-init!)
   ($ssl-server-make-socket name port (ssl-protocols->integer protocol)
			    cert pkey
			    CAs accepted-certs backlog))

;*---------------------------------------------------------------------*/
;*    ssl-protocols->integer ...                                       */
;*---------------------------------------------------------------------*/
(define (ssl-protocols->integer protocol)
   (case (string->symbol (string-downcase (symbol->string! protocol)))
      ((sslv2) $ssl-client-sslv2)
      ((sslv3) $ssl-client-sslv3)
      ((ssl sslv23) $ssl-client-sslv23)
      ((tls tlsv1) $ssl-client-tlsv1)
      ((dtls dtlsv1) $ssl-client-dtlsv1)
      (else (error 'ssl "Unknown protocols" protocol))))

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
(define-inline (ssl-rand-status)
   (cond-expand
      (bigloo-c ($ssl-rand-status))
      (else #t)))

;*---------------------------------------------------------------------*/
;*    ssl-rand-poll ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ssl-rand-poll)
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
	  ($ssl-ctx-set-cipher-list $native options))
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
