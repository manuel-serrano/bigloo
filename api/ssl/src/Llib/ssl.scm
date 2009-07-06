;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/ssl/src/Llib/ssl.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano & Stephane Epardaud                */
;*    Creation    :  Thu Mar 24 10:24:38 2005                          */
;*    Last change :  Sun Sep  7 16:10:24 2008 (serrano)                */
;*    Copyright   :  2005-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SSL Bigloo library                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ssl_ssl
   
   (extern (include "bglssl.h")
	   
           (export %make-certificate "bgl_make_certificate")
           (export %make-private-key "bgl_make_private_key")
           (export %certificate-$native "bgl_certificate_native")
           (export %private-key-$native "bgl_private_key_native")
	   
           (type $private-key (opaque) "void *")
           (type $certificate (opaque) "void *")
           
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
	   
	   ($ssl-client-make-socket::obj (::bstring ::int ::int ::int
                                                    ::obj ::obj
                                                    ::pair-nil
                                                    ::obj
						    ::bstring
						    ::bstring)
					 "bgl_make_ssl_client_socket")
	   ($ssl-client-socket-use-ssl!::socket (::socket
						 ::int
						 ::obj ::obj
						 ::pair-nil
						 ::obj)
						"bgl_client_socket_use_ssl")
	   ($ssl-server-make-socket::obj (::obj ::int ::int
                                                ::obj
                                                ::obj
                                                ::pair-nil
                                                ::obj
						::int)
					 "bgl_make_ssl_server_socket")
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
	 
	 (class $ssl-client
	    (constructor make-socket (::bstring ::int ::int ::int
						::obj ::obj
						::pair-nil ::obj
						::bstring ::bstring))
	    (method static socket-use-ssl!::socket (::socket
						    ::int 
						    ::obj ::obj
						    ::pair-nil ::obj)
		    "bgl_client_socket_use_ssl")
	    (field static sslv2::int "BGLSSL_SSLV2")
	    (field static sslv3::int "BGLSSL_SSLV3")
	    (field static sslv23::int "BGLSSL_SSLV23")
	    (field static tlsv1::int "BGLSSL_TLSV1")
	    (field static dtlsv1::int "BGLSSL_DTLSV1")
	    "bigloo.ssl.ssl_client_socket")
	 
	 (class $ssl-server
	    (constructor make-socket (::obj ::int ::int
					    ::obj ::obj ::pair-nil ::obj ::int))
	    "bigloo.ssl.ssl_server_socket"))
   
   (export (class certificate
	      ($native::$certificate read-only))
	   
	   (class private-key
	      ($native::$private-key read-only))
	   
	   (read-private-key::private-key ::bstring)
	   (read-certificate::certificate ::bstring)
	   (read-pem-file::pair-nil ::bstring)
	   
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
	   (%private-key-$native::$private-key ::obj)))

;*---------------------------------------------------------------------*/
;*    sanity-args-checks ...                                           */
;*---------------------------------------------------------------------*/
(define (sanity-args-checks func cert pkey CAs accepted-certs)
   (unless (or (not cert) (certificate? cert))
      (error func "Invalid certificate" cert))
   (unless (or (not pkey) (private-key? pkey))
      (error func "Invalid private key" pkey))
   (unless (and (list? CAs) (every? certificate? CAs))
      (error func "Invalid CA list" CAs))
   (unless (or (not accepted-certs)
	       (and (list? accepted-certs)
		    (every? certificate? accepted-certs)))
      (error func "Invalid accepted-certs" accepted-certs))
   (if (or (and (certificate? cert) (not (private-key? pkey)))
	   (and (private-key? pkey) (not (certificate? cert))))
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
   (make-certificate cert))

;*---------------------------------------------------------------------*/
;*    %make-private-key ...                                            */
;*---------------------------------------------------------------------*/
(define (%make-private-key pkey)
   (make-private-key pkey))

;*---------------------------------------------------------------------*/
;*    %certificate-$native ...                                         */
;*---------------------------------------------------------------------*/
(define (%certificate-$native cert)
   (certificate-$native cert))

;*---------------------------------------------------------------------*/
;*    %private-key-$native ...                                         */
;*---------------------------------------------------------------------*/
(define (%private-key-$native pkey)
   (private-key-$native pkey))

