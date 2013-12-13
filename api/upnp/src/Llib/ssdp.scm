;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/upnp/src/Llib/ssdp.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr  8 08:20:16 2013                          */
;*    Last change :  Fri Dec 13 12:50:55 2013 (serrano)                */
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
   
   (option (set! *dlopen-init-gc* #t))
   
   (library web)
   
   (export (abstract-class ssdp-message
	      (header::pair-nil read-only))

	   (class ssdp-m-search::ssdp-message
	      (host::bstring read-only)
	      (mx::int read-only)
	      (st::bstring read-only))

	   (abstract-class ssdp-discovery::ssdp-message
	      ;; expiration date
	      (edate::elong read-only)
	      ;; unique service identifier
	      (usn::bstring read-only)
	      ;; root device location
	      (location::bstring read-only)
	      ;; vendor
	      (server::bstring read-only))
	      
	   (class ssdp-notify::ssdp-discovery
	      ;; notification type
	      (nt::bstring read-only)
	      ;; ssdp:alive, ssdp:byebye, ssdp:update
	      (nts::bstring read-only)
	      ;; host
	      (host::bstring read-only))

	   (class ssdp-response::ssdp-discovery
	      ;; search target
	      (st::bstring read-only))
	   
	   (class ssdp-root
	      (major (default 1))
	      (minor (default 0))
	      (services (default '()))
	      (icons (default '()))
	      (device (default #f)))

	   (ssdp-discover-loop #!key
	      (buffer-size::int 2048)
	      (timeout 0)
	      ondiscovery
	      onsearch
	      socket)

	   (ssdp-discover-quit ::obj)

	   (ssdp-discover-m-search #!key
	      (ipv4-multicast-address "239.255.255.250")
	      (multicast-port::int 1900)
	      (target "ssdp:all")
	      socket)

	   (ssdp-parse-location ::bstring)))

;*---------------------------------------------------------------------*/
;*    ssdp-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (ssdp-debug)
   #f)

;*---------------------------------------------------------------------*/
;*    ssdp-parse-header ...                                            */
;*---------------------------------------------------------------------*/
(define (ssdp-parse-header buffer::bstring port::input-port ondiscovery onsearch)
   
   (define (read-cache-control cc)
      (string-case cc
      	 ((: "max-age" (* blank) "=" (* blank) (submatch (+ digit)))
      	  (string->elong (the-submatch 1)))
      	 (else
      	  #e0)))
   
   (define (header-field field header)
      (let ((c (assq field header)))
	 (if (pair? c)
	     (cdr c)
	     (error "ssdp-parse-head" (format "Cannot find field \"~a\"" field) header))))

   (define (header-field/opt field header def)
      (let ((c (assq field header)))
	 (if (pair? c)
	     (cdr c)
	     def)))

   (define (header-cache-control header)
      (let ((c (assq :cache-control header)))
	 (if (pair? c)
	     (let* ((cc (header-field :cache-control header))
		    (ttl (read-cache-control cc)))
		(+elong (current-seconds) ttl))
	     #e0)))
      
   (define (ssdp-parse-response port)
      ;; response (Section 1.3.3)
      (http-parse-response port #f
	 (lambda (input status header length encoding)
	    (instantiate::ssdp-response
	       (header header)
	       (edate (header-cache-control header))
	       (location (header-field :location header))
	       (server (header-field :server header))
	       (st (header-field :st header))
	       (usn (header-field :usn header))))))
   
   (define (ssdp-parse-notify port)
      ;; response (Section 1.2.2)
      (read-line port)
      (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	 (http-parse-header port #f)
	 (instantiate::ssdp-notify
	    (header header)
	    (edate (header-cache-control header))
	    (location (header-field/opt :location header ""))
	    (host (header-field :host header))
	    (nt (header-field :nt header))
	    (usn (header-field :usn header))
	    (nts (header-field :nts header))
	    (server (header-field/opt :server header "")))))

   (define (ssdp-parse-m-search port)
      ;; skip the request line
      (read-line port)
      (multiple-value-bind (header actual-host actual-port cl te auth pauth co)
	 (http-parse-header port #f)
	 (instantiate::ssdp-m-search
	    (header header)
	    (host (header-field :host header))
	    (mx (string->integer (header-field :mx header)))
	    (st (header-field :st header)))))
   
   ;; Match the start line (Section 1.1.1).
   (cond
      ((string-prefix? "HTTP/1.1 200 OK\r\n" buffer)
       (when (ssdp-debug)
	  (tprint "ssdp-parse-response..."))
       (when (procedure? ondiscovery)
	  (ondiscovery (ssdp-parse-response port))))
      ((string-prefix? "NOTIFY * HTTP/1.1\r\n" buffer)
      (when (ssdp-debug)
	 (tprint "ssp-parse-notify..."))
       (when (procedure? ondiscovery)
	  (ondiscovery (ssdp-parse-notify port))))
      ((string-prefix? "M-SEARCH * HTTP/1.1\r\n" buffer)
       (when (ssdp-debug)
	  (tprint "ssdp-parse-m-search..."))
       (when (procedure? onsearch)
	  (onsearch (ssdp-parse-m-search port))))
      (else
       (error "ssdp-parse-header" "Illegal start line" (read-line port)))))

;*---------------------------------------------------------------------*/
;*    ssdp-read-header ...                                             */
;*---------------------------------------------------------------------*/
(define (ssdp-read-header socket buffer::bstring port::input-port
	   ondiscovery onsearch)
   ;; fill the buffer with READ-CHARS! instead of RECEIVE for the timeout
   (read-chars! buffer (string-length buffer) (datagram-socket-input socket))
   ;; reset the input port associated with the buffer
   (input-port-buffer-set! port buffer)
   ;; parse the obtained characters
   (ssdp-parse-header buffer port ondiscovery onsearch))
		
;*---------------------------------------------------------------------*/
;*    ssdp-discover-loop ...                                           */
;*---------------------------------------------------------------------*/
(define (ssdp-discover-loop #!key
	   (buffer-size::int 2048)
	   (timeout 0)
	   ondiscovery
	   onsearch
	   socket)
   (unless (datagram-socket? socket)
      (bigloo-type-error "ssdp-discover-loop" "datagram-socket" socket))
   (datagram-socket-option-set! socket :IP_ADD_MEMBERSHIP "239.255.255.250")
   ;; set the socket timeout
   (when (> timeout 0)
      (input-port-timeout-set! (datagram-socket-input socket) timeout))
   ;; wait for the response and parse it
   (let* ((buffer (make-string buffer-size))
	  (port (open-input-string ""))
	  (ctrl (cons 'ssdp-discover-loop #t)))
      (let liip ()
	 (with-handler
	    (lambda (e)
	       (when (ssdp-debug)
		  (exception-notify e))
	       (liip))
	    (let loop ()
	       (when (cdr ctrl)
		  (ssdp-read-header socket buffer port ondiscovery onsearch)
		  (loop)))))))

;*---------------------------------------------------------------------*/
;*    ssdp-discover-quit ...                                           */
;*---------------------------------------------------------------------*/
(define (ssdp-discover-quit loop)
   (if (and (pair? loop) (eq? (car loop) 'ssdp-discover-loop))
       (set-cdr! loop #f)
       (bigloo-type-error "ssdp-discover-quit" "ssdp-loop" loop)))

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

   (define (stringify body)
      (cond
	 ((null? body)
	  "")
	 ((null? (cdr body))
	  (car body))
	 (else
	  (apply string-append
	     (filter (lambda (s)
			(and (string? s)
			     (not (string=? s "\n"))))
		body)))))

   (call-with-input-file url
      (lambda (ip)
	 (bind-exit (return)
	    (let ((root (instantiate::ssdp-root))
		  (icon '())
		  (iconlist '())
		  (dev '())
		  (svc '()))
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
				       (cons (cons tag (stringify body))
					  dev)))
				   ((serviceList)
				    #unspecified)
				   ((service)
				    (with-access::ssdp-root root (services)
				       (set! services (cons svc services))
				       (set! svc '())))
				   ((serviceType serviceId controlURL eventSubURL SCPDURL)
				    (set! svc
				       (cons (cons tag (stringify body))
					  svc)))
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

