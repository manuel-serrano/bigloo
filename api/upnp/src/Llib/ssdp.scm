;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/upnp/src/Llib/ssdp.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  8 08:20:16 2013                          */
;*    Last change :  Fri Apr 12 16:08:39 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    UPnP Simple Service Discovery protocol                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __upnp_ssdp
   
   (library web)
   
   (export (class ssdp-header
	      ;; date
	      (expiration-date::elong read-only)
	      ;; uri
	      (location::bstring read-only)
	      (unique-service-name::bstring read-only)
	      ;; vendor field alist
	      (extra-fields::pair-nil read-only (default '())))
	   
	   (class ssdp-search-response::ssdp-header
	      ;; OS & vendor
	      (server::bstring read-only)
	      ;; uri
	      (search-target::bstring read-only))
	   
	   (class ssdp-advertisement::ssdp-header
	      ;; byebye, etc.
	      (notification-type::symbol read-only)
	      (notification-sub-type::symbol read-only)
	      (server::bstring read-only))

	   (class ssdp-root
	      (major (default 1))
	      (minor (default 0))
	      (services (default '()))
	      (icons (default '()))
	      (device (default #f)))

	   (ssdp-discover #!key
	      (ipv4-multicast-address "239.255.255.250")
	      (multicast-port::int 1900)
	      (buffer-size::int 2048)
	      (target "ssdp:all")
	      (timeout 0)
	      (ondiscover (lambda (x) x))
	      (socket (make-datagram-unbound-socket 'inet)))

	   (ssdp-parse-location ::bstring)))

;*---------------------------------------------------------------------*/
;*    ssdp-search-message ...                                          */
;*---------------------------------------------------------------------*/
(define (ssdp-search-message #!key
	   (ipv4-multicast-address "239.255.255.250")
	   (multicast-port::int 1900)
	   (target "ssdp:all"))
   ;; Return the SSDP search message for TARGET.
   (string-append
      "M-SEARCH * HTTP/1.1\r\n"
      "HOST: " ipv4-multicast-address
      ":" (number->string multicast-port) "\r\n"
      "MAN: \"ssdp:discover\"\r\n"
      "MX: 10\r\n"
      "ST: " target "\r\n"
      "\r\n"))

;*---------------------------------------------------------------------*/
;*    ssdp-parse-header ...                                            */
;*---------------------------------------------------------------------*/
(define (ssdp-parse-header buffer ondiscover)
   
   (define (date+ date seconds)
      (+ seconds (date->seconds date)))
   
   (define (read-cache-control cc)
      (string-case cc
      	 ((: "max-age" (* blank) "=" (* blank) (submatch (+ digit)))
      	  (string->number (the-submatch 1)))
      	 (else
      	  #f)))
   
   (define (header-field field header def)
      (let ((c (assq field header)))
	 (if (pair? c)
	     (cdr c)
	     def)))
   
   (define (ssdp-parse-response buffer)
      ;; response (Section 1.3.3)
      (call-with-input-string buffer
	 (lambda (port)
	    (http-parse-response port #f
	       (lambda (input status header length encoding)
		  (let* ((now  (current-date))
			 (cc (header-field :cache-control header ""))
			 (ttl (read-cache-control cc))
			 (then (date+ now ttl)))
		     (instantiate::ssdp-search-response
			(expiration-date then)
			(location (header-field :location header ""))
			(server (header-field :server header ""))
			(search-target (header-field :st header ""))
			(unique-service-name (header-field :usn header "")))))))))
   
   (define (ssdp-parse-advertisement buffer)
      ;; response (Section 1.2.2)
      (call-with-input-string buffer
	 (lambda (port)
	    (http-parse-response port #f
	       (lambda (input status header length encoding)
		  (let* ((now  (current-date))
			 (cc (header-field :cache-control header ""))
			 (ttl (read-cache-control cc))
			 (then (date+ now ttl)))
		     (instantiate::ssdp-advertisement
			(expiration-date then)
			(location (header-field :location header""))
			(notification-type (header-field :nt header 'unknown))
			(unique-service-name (header-field :usn header ""))
			(notification-sub-type (header-field :nts header 'unknown))
			(server (header-field :server header "")))))))))
   
   ;; Match the start line (Section 1.1.1).
   (cond
      ((string-prefix? "HTTP/1.1 200 OK\r\n" buffer)
       (ondiscover (ssdp-parse-response buffer)))
      ((string-prefix? "NOTIFY * HTTP/1.1\r\n" buffer)
       (ondiscover (ssdp-parse-advertisement buffer)))
      ((string-prefix? "M-SEARCH * HTTP/1.1\r\n" buffer)
       #f)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    ssdp-read-header ...                                             */
;*---------------------------------------------------------------------*/
(define (ssdp-read-header socket buffer ondiscover)
   ;; fill the buffer with READ-CHARS! instead of RECEIVE for the timeout
   (read-chars! buffer (string-length buffer) (datagram-socket-input socket))
   ;; parse the obtained characters
   (ssdp-parse-header buffer ondiscover))
		
;*---------------------------------------------------------------------*/
;*    ssdp-discover ...                                                */
;*---------------------------------------------------------------------*/
(define (ssdp-discover #!key
	   (ipv4-multicast-address "239.255.255.250")
	   (multicast-port::int 1900)
	   (buffer-size::int 2048)
	   (target "ssdp:all")
	   (timeout 0)
	   (ondiscover (lambda (x) x))
	   (socket (make-datagram-unbound-socket 'inet)))
   ;; set the socket timeout
   (when (> timeout 0)
      (input-port-timeout-set! (datagram-socket-input socket) timeout))
   ;; send the search message
   (datagram-socket-send socket
      (ssdp-search-message :ipv4-multicast-address ipv4-multicast-address
	 :multicast-port multicast-port
	 :target target)
      ipv4-multicast-address
      multicast-port)
   ;; wait for the response and parse it
   (let ((buffer (make-string buffer-size))
	 (acc '()))
      (with-handler
	 (lambda (e)
	    (if (isa? e &io-timeout-error)
		acc
		(raise e)))
	 (let loop ()
	    (let ((header (ssdp-read-header socket buffer ondiscover)))
	       (when header
		  (set! acc (cons header acc)))
	       (loop))))))
   
;*---------------------------------------------------------------------*/
;*    ssdp-parse-location ...                                          */
;*---------------------------------------------------------------------*/
(define (ssdp-parse-location url::bstring)
   (call-with-input-file url
      (lambda (ip)
	 (bind-exit (return)
	    (let ((root (instantiate::ssdp-root))
		  (icon '())
		  (iconlist '())
		  (dev '()))
	       (xml-parse ip
		  :procedure (lambda (tag attrs body)
				(case tag
				   ((root)
				    (return root))
				   ((major)
				    (with-access::ssdp-root root (major)
				       (set! major (car body))))
				   ((minor)
				    (with-access::ssdp-root root (minor)
				       (set! minor (car body))))
				   ((device)
				    (with-access::ssdp-root root (device)
				       (set! device dev)))
				   ((iconList)
				    (with-access::ssdp-root root (icons)
				       (set! icons iconlist)
				       (set! iconlist '())))
				   ((deviceType manufacturer friendlyName manufacturerURL modelDescription modelName modelNumber modelURL serialNumber UDN)
				    (set! dev
				       (cons (cons tag (car body)) dev)))
				   ((serviceList)
				    (with-access::ssdp-root root (services)
				       (set! services body)))
				   ((mimetype)
				    (set! icon
				       (cons (cons 'mime-type (car body))
					  icon)))
				   ((width)
				    (set! icon
				       (cons (cons 'width (car body))
					  icon)))
				   ((height)
				    (set! icon
				       (cons (cons 'height (car body))
					  icon)))
				   ((depth)
				    (set! icon
				       (cons (cons 'depth (car body))
					  icon)))
				   ((url)
				    (set! icon
				       (cons (cons 'url (car body))
					  icon)))
				   ((icon)
				    (set! iconlist (cons icon iconlist))
				    (set! icon '()))
				   (else
				    (list tag body attrs))))))))))

