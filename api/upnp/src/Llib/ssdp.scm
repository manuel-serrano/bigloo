;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/upnp/src/Llib/ssdp.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  8 08:20:16 2013                          */
;*    Last change :  Mon Apr 15 08:45:14 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    UPnP Simple Service Discovery protocol                           */
;*=====================================================================*/

;; http://www.upnp.org/specs/arch/UPnP-arch-DeviceArchitecture-v1.1.pdf
;; https://tools.ietf.org/html/draft-cai-ssdp-v1-03

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __upnp_ssdp
   
   (library web)
   
   (export (abstract-class ssdp-message
	      (header::pair-nil read-only))

	   (class ssdp-m-search::ssdp-message
	      (host::bstring read-only)
	      (mx::int read-only)
	      (st::bstring read-only))
	   
	   (class ssdp-notify::ssdp-message
	      ;; expiration date
	      (edate::elong read-only)
	      ;; notification type
	      (nt::bstring read-only)
	      ;; ssdp:alive, ssdp:byebye, ssdp:update
	      (nts::bstring read-only)
	      ;; root device location
	      (location::bstring read-only)
	      ;; host
	      (host::bstring read-only)
	      ;; vendor
	      (server::bstring read-only)
	      ;; unique service identifier
	      (usn::bstring read-only))

	   (class ssdp-response::ssdp-message
	      ;; expiration date
	      (edate::elong read-only)
	      ;; root device location
	      (location::bstring read-only)
	      ;; host
	      (host::bstring read-only)
	      ;; OS & vendor
	      (server::bstring read-only)
	      ;; search target
	      (st::bstring read-only)
	      ;; unique service identifier
	      (usn::bstring read-only))
	   
	   (class ssdp-root
	      (major (default 1))
	      (minor (default 0))
	      (services (default '()))
	      (icons (default '()))
	      (device (default #f)))

	   (ssdp-discover-loop #!key
	      (buffer-size::int 2048)
	      (timeout 0)
	      (onmessage::procedure (lambda (x) x))
	      socket)

	   (ssdp-discover-m-search #!key
	      (ipv4-multicast-address "239.255.255.250")
	      (multicast-port::int 1900)
	      (target "ssdp:all")
	      socket)

	   (ssdp-parse-location ::bstring)))

;*---------------------------------------------------------------------*/
;*    ssdp-parse-header ...                                            */
;*---------------------------------------------------------------------*/
(define (ssdp-parse-header buffer onmessage)
   
   (define (date+ date seconds)
      (+ seconds (date->seconds date)))
   
   (define (read-cache-control cc)
      (string-case cc
      	 ((: "max-age" (* blank) "=" (* blank) (submatch (+ digit)))
      	  (string->number (the-submatch 1)))
      	 (else
      	  #f)))
   
   (define (header-field field header)
      (let ((c (assq field header)))
	 (if (pair? c)
	     (cdr c)
	     (error "ssdp-parse-head" "Cannot find mandatory field" field))))
   
   (define (ssdp-parse-response buffer)
      ;; response (Section 1.3.3)
      (call-with-input-string buffer
	 (lambda (port)
	    (http-parse-response port #f
	       (lambda (input status header length encoding)
		  (let* ((now  (current-date))
			 (cc (header-field :cache-control header))
			 (ttl (read-cache-control cc))
			 (then (date+ now ttl)))
		     (instantiate::ssdp-response
			(header header)
			(edate then)
			(host (header-field :host header))
			(location (header-field :location header))
			(server (header-field :server header))
			(st (header-field :st header))
			(usn (header-field :usn header)))))))))
   
   (define (ssdp-parse-notify buffer)
      ;; response (Section 1.2.2)
      (call-with-input-string buffer
	 (lambda (port)
	    ;; skip the request line
	    (read-line port)
	    (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	       (http-parse-header port #f)
	       (let* ((now  (current-date))
		      (cc (header-field :cache-control header))
		      (ttl (read-cache-control cc))
		      (then (date+ now ttl)))
		  (instantiate::ssdp-notify
		     (header header)
		     (edate then)
		     (location (header-field :location header))
		     (host (header-field :host header))
		     (nt (header-field :nt header))
		     (usn (header-field :usn header))
		     (nts (header-field :nts header))
		     (server (header-field :server header))))))))

   (define (ssdp-parse-m-search buffer)
      (call-with-input-string buffer
	 (lambda (port)
	    ;; skip the request line
	    (read-line port)
	    (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	       (http-parse-header port #f)
	       (instantiate::ssdp-m-search
		  (header header)
		  (host (header-field :host header))
		  (mx (header-field :mx header))
		  (st (header-field :st header)))))))
   
   ;; Match the start line (Section 1.1.1).
   (cond
      ((string-prefix? "HTTP/1.1 200 OK\r\n" buffer)
       (onmessage (ssdp-parse-response buffer)))
      ((string-prefix? "NOTIFY * HTTP/1.1\r\n" buffer)
       (onmessage (ssdp-parse-notify buffer)))
      ((string-prefix? "M-SEARCH * HTTP/1.1\r\n" buffer)
       (onmessage (ssdp-parse-m-search buffer)))
      (else
       (error "ssdp-parse-header"
	  "Illegal start line"
	  (call-with-input-string buffer read-line)))))

;*---------------------------------------------------------------------*/
;*    ssdp-read-header ...                                             */
;*---------------------------------------------------------------------*/
(define (ssdp-read-header socket buffer onmessage)
   ;; fill the buffer with READ-CHARS! instead of RECEIVE for the timeout
   (read-chars! buffer (string-length buffer) (datagram-socket-input socket))
   ;; parse the obtained characters
   (ssdp-parse-header buffer onmessage))
		
;*---------------------------------------------------------------------*/
;*    ssdp-discover-loop ...                                           */
;*---------------------------------------------------------------------*/
(define (ssdp-discover-loop #!key
	   (buffer-size::int 2048)
	   (timeout 0)
	   (onmessage::procedure (lambda (x) x))
	   socket)
   (unless (datagram-socket? socket)
      (bigloo-type-error "ssdp-discover-loop" "datagram-socket" socket))
   (datagram-socket-option-set! socket :IP_ADD_MEMBERSHIP "239.255.255.250")
   ;; set the socket timeout
   (when (> timeout 0)
      (input-port-timeout-set! (datagram-socket-input socket) timeout))
   ;; wait for the response and parse it
   (let ((buffer (make-string buffer-size))
	 (acc '()))
      (with-handler
	 (lambda (e)
	    (tprint "E: " e)
	    (if (isa? e &io-timeout-error)
		acc
		(raise e)))
	 (let loop ()
	    (let ((header (ssdp-read-header socket buffer onmessage)))
	       (when header
		  (set! acc (cons header acc)))
	       (loop))))))

;*---------------------------------------------------------------------*/
;*    ssdp-discover-m-search ...                                       */
;*---------------------------------------------------------------------*/
(define (ssdp-discover-m-search #!key
	   (ipv4-multicast-address "239.255.255.250")
	   (multicast-port::int 1900)
	   (target "ssdp:all")
	   socket)
   
   (define (ssdp-search-message)
      (string-append
	 "M-SEARCH * HTTP/1.1\r\n"
	 "HOST: " ipv4-multicast-address
	 ":" (number->string multicast-port) "\r\n"
	 "MAN: \"ssdp:discover\"\r\n"
	 "MX: 10\r\n"
	 "ST: " target "\r\n"
	 "\r\n"))

   ;; type check
   (unless (datagram-socket? socket)
      (bigloo-type-error "ssdp-discover-loop" "datagram-socket" socket))
   ;; send the search message
   (datagram-socket-send socket
      (ssdp-search-message)
      ipv4-multicast-address
      multicast-port))

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

