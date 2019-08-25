;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/socket.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 29 18:45:17 1998                          */
;*    Last change :  Sun Aug 25 09:09:27 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Socket handling.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __socket
   
   (import  __error
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __dsssl
	    __bexit
	    __bignum
	    __bit
	    
	    __r4_output_6_10_3
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_control_features_6_9
	    
	    __evenv)
   
   (extern  (macro c-socket?::bool (::obj) "SOCKETP")
            (macro c-socket-hostname::obj (::socket) "SOCKET_HOSTNAME")
	    (macro c-socket-hostip::obj (::socket) "SOCKET_HOSTIP")
	    (macro c-socket-down?::bool (::socket) "SOCKET_DOWNP")
	    (macro c-socket-port-number::int (::socket) "SOCKET_PORT")
	    (macro c-socket-input::input-port (::socket) "SOCKET_INPUT")
	    
	    (macro c-socket-output::output-port (::socket) "SOCKET_OUTPUT")
	    (c-socket-startup::void () "socket_startup")
	    (c-socket-cleanup::void () "socket_cleanup")
	    ($make-client-socket::socket (::bstring ::int ::int ::bstring ::bstring)
					 "bgl_make_client_socket")
	    ($make-unix-socket::socket (::bstring ::int ::bstring ::bstring)
					 "bgl_make_unix_socket")
	    ($make-server-socket::socket (::obj ::int ::int ::bool)
					 "bgl_make_server_socket")
	    ($socket-accept::obj (::socket ::bool ::bstring ::bstring)
				 "bgl_socket_accept")
	    ($socket-accept-many::long (::socket ::bool ::vector ::vector ::vector)
				       "bgl_socket_accept_many")
	    ($socket-host-addr::obj (::socket) "bgl_socket_host_addr")
	    ($socket-local-addr::bstring (::socket) "bgl_socket_local_addr")
	    ($socket-local?::bool (::socket) "bgl_socket_localp")
	    ($socket-host-addr=?::bool (::socket ::bstring) "bgl_socket_host_addr_cmp")
	    ($socket-shutdown::int (::socket ::int) "socket_shutdown")
	    ($socket-close::obj (::socket) "socket_close")
	    (macro c-socket-server?::bool (::obj) "BGL_SOCKET_SERVERP")
	    (macro c-socket-client?::bool (::obj) "BGL_SOCKET_CLIENTP")
	    
	    ($host::bstring (::bstring) "bgl_host")
	    ($hostinfo::pair-nil (::bstring) "bgl_hostinfo")
	    ($resolv::vector (::bstring ::bstring) "bgl_res_query")
	    
	    ($gethostname::bstring () "bgl_gethostname")
	    ($gethostname-by-address::bstring (::bstring) "bgl_gethostname_by_address")
	    ($gethostinterfaces::pair-nil () "bgl_gethostinterfaces")
	    ($getprotoents::pair-nil () "bgl_getprotoents")
	    ($getprotobyname::obj (::string) "bgl_getprotobyname")
	    ($getprotobynumber::obj (::int) "bgl_getprotobynumber")
	    
	    ($getsockopt::obj (::socket ::keyword) "bgl_getsockopt")
	    ($setsockopt!::obj (::socket ::keyword ::obj) "bgl_setsockopt")

	    ($dgetsockopt::obj (::datagram-socket ::keyword) "bgl_getsockopt")
	    ($dsetsockopt!::obj (::datagram-socket ::keyword ::obj) "bgl_setsockopt")

	    (macro $datagram-socket?::bool (::obj)
		   "BGL_DATAGRAM_SOCKETP")
	    (macro $datagram-socket-server?::bool (::obj)
		   "BGL_DATAGRAM_SOCKET_SERVERP")
	    (macro $datagram-socket-client?::bool (::obj)
		   "BGL_DATAGRAM_SOCKET_CLIENTP")
	    ($make-datagram-server-socket::datagram-socket (::int)
	       "bgl_make_datagram_server_socket")
	    ($make-datagram-unbound-socket::datagram-socket (::symbol)
	       "bgl_make_datagram_unbound_socket")

	    ($make-datagram-client-socket::datagram-socket (::bstring ::int ::bool)
	       "bgl_make_datagram_client_socket")
	    (macro $datagram-socket-hostname::obj (::datagram-socket)
		   "BGL_DATAGRAM_SOCKET_HOSTNAME")
	    (macro $datagram-socket-hostip::obj (::datagram-socket)
		   "BGL_DATAGRAM_SOCKET_HOSTIP")
	    (macro $datagram-socket-port-number::int (::datagram-socket)
		   "BGL_DATAGRAM_SOCKET_PORTNUM")
	    (macro $datagram-socket-port::obj (::datagram-socket)
		   "BGL_DATAGRAM_SOCKET_PORT")
	    ($datagram-socket-close::obj (::datagram-socket)
	       "bgl_datagram_socket_close")
	    ($datagram-socket-receive::obj (::datagram-socket ::long)
	       "bgl_datagram_socket_receive")
            ($datagram-socket-send::obj (::datagram-socket ::bstring ::bstring ::int)
	       "bgl_datagram_socket_send"))
   
   (java    (class foreign
	       (method static c-socket?::bool (::obj)
		  "SOCKETP")
	       (method static $make-client-socket::socket (::bstring ::int ::int ::bstring ::bstring)
		  "bgl_make_client_socket")
	       (method static $make-server-socket::socket (::obj ::int ::int ::bool)
		  "bgl_make_server_socket")
	       
	       (method static c-socket-hostname::obj (::socket)
		  "SOCKET_HOSTNAME")
	       (method static c-socket-hostip::obj (::socket)
		  "SOCKET_HOSTIP")
	       (method static c-socket-down?::bool (::socket)
		  "SOCKET_DOWNP")
	       (method static c-socket-port-number::int (::socket)
		  "SOCKET_PORT")
	       (method static c-socket-input::input-port (::socket)
		  "SOCKET_INPUT")
	       (method static c-socket-output::output-port (::socket)
		  "SOCKET_OUTPUT")
	       
	       (method static $socket-accept::socket (::socket ::bool ::bstring ::bstring)
		  "bgl_socket_accept")
	       (method static $socket-host-addr::obj (::socket)
		  "socket_host_addr")
	       (method static $socket-local-addr::bstring (::socket)
		  "socket_local_addr")
	       (method static $socket-shutdown::int (::socket ::int)
		  "socket_shutdown")
	       (method static $socket-close::obj (::socket)
		  "socket_close")
	       (method static c-socket-server?::bool (::obj)
		  "BGL_SOCKET_SERVERP")
	       (method static c-socket-client?::bool (::obj)
		  "BGL_SOCKET_CLIENTP")
	       
	       (method static $host::bstring (::bstring)
		  "bgl_host")
	       (method static $hostinfo::obj (::bstring)
		  "bgl_hostinfo")
	       (method static $gethostname::bstring ()
		  "bgl_gethostname")
	       (method static $gethostname-by-address::bstring (::bstring)
		  "bgl_gethostname_by_address")
	       (method static $gethostinterfaces::pair-nil ()
		  "bgl_gethostinterfaces")

	       (method static $getprotoents::pair-nil ()
		  "bgl_getprotoents")
	       (method static $getprotobyname::obj (::string)
		  "bgl_getprotobyname")
	       (method static $getprotobynumber::obj (::int)
		  "bgl_getprotobynumber")
	       (method static $getsockopt::obj (::socket ::keyword)
		  "bgl_getsockopt")
	       (method static $setsockopt!::obj (::socket ::keyword ::obj)
		  "bgl_setsockopt")
	       (method static $datagram-socket?::bool (::obj)
		  "BGL_DATAGRAM_SOCKETP")
	       (method static $datagram-socket-server?::bool (::obj)
		  "BGL_DATAGRAM_SOCKET_SERVERP")
	       (method static $datagram-socket-client?::bool (::obj)
		   "BGL_DATAGRAM_SOCKET_CLIENTP")
	       (method static $make-datagram-server-socket::datagram-socket (::int)
		  "bgl_make_datagram_server_socket")
	       (method static $make-datagram-unbound-socket::datagram-socket (::symbol)
		  "bgl_make_datagram_unbound_socket")
	       (method static $make-datagram-client-socket::datagram-socket (::bstring ::int ::bool)
		  "bgl_make_datagram_client_socket")
	       (method static $datagram-socket-hostname::obj (::datagram-socket)
		  "BGL_DATAGRAM_SOCKET_HOSTNAME")
	       (method static $datagram-socket-hostip::obj (::datagram-socket)
		   "BGL_DATAGRAM_SOCKET_HOSTIP")
	       (method static $datagram-socket-port-number::int (::datagram-socket)
		   "BGL_DATAGRAM_SOCKET_PORTNUM")
	       (method static $datagram-socket-port::obj (::datagram-socket)
		  "BGL_DATAGRAM_SOCKET_PORT")
	       
	       (method static $datagram-socket-close::obj (::datagram-socket)
		  "bgl_datagram_socket_close")
	       (method static $datagram-socket-receive::obj (::datagram-socket ::long)
		  "bgl_datagram_socket_receive")
	       (method static $datagram-socket-send::long (::datagram-socket ::bstring ::bstring ::int)
		  "bgl_datagram_socket_send")
	       
	       (method static $dgetsockopt::obj (::datagram-socket ::keyword)
		  "bgl_dgetsockopt")
	       (method static $dsetsockopt!::obj (::datagram-socket ::keyword ::obj)
		  "bgl_dsetsockopt")))
	       
   (export  (%socket-init!)
	    (inline socket?::bool ::obj)
	    (inline socket-server?::bool ::obj)
	    (inline socket-client?::bool ::obj)
	    (inline socket-hostname::obj ::socket)
	    (inline socket-host-address::obj ::socket)
	    (inline socket-host-address=?::bool ::socket ::bstring)
	    (inline socket-local-address ::socket)
	    (inline socket-local?::bool ::socket)
	    (inline socket-down?::bool ::socket)
	    (inline socket-port-number::bint ::socket)
	    (inline socket-input::input-port ::socket)
	    (inline socket-output::output-port ::socket)
	    (make-client-socket::socket ::bstring ::int
					#!key
					(domain 'inet)
					(inbuf #t) (outbuf #t)
					(timeout 0))
	    (make-server-socket::socket #!optional (port 0)
					#!key (name #f) (backlog 5) (ipv6 #f))
	    (socket-accept::obj ::socket #!key (inbuf #t) (outbuf #t) (errp #t))
	    (socket-accept-many::obj ::socket ::vector
				     #!key (inbufs #t) (outbufs #t) (errp #t))
	    (socket-shutdown::int ::socket #!optional (how #t))
	    (inline socket-close::obj ::socket)
	    (inline host::bstring ::bstring)
	    (inline hostinfo::pair-nil ::bstring)
	    (hostname::bstring #!optional hostip)
	    (resolv::vector ::bstring ::symbol)
	    (inline get-interfaces::pair-nil)
	    (inline get-protocols::pair-nil)
	    (get-protocol ::obj)
	    (inline socket-option ::socket ::keyword)
	    (inline socket-option-set! ::socket ::keyword ::obj)
	    (inline datagram-socket?::bool ::obj)
	    (inline datagram-socket-server?::bool ::obj)
	    (inline datagram-socket-client?::bool ::obj)
	    (inline make-datagram-server-socket::datagram-socket #!optional (port 0))
	    (inline make-datagram-unbound-socket::datagram-socket #!optional (family::symbol 'inet))
	    (inline make-datagram-client-socket::datagram-socket ::bstring ::int #!optional broadcast)
	    (inline datagram-socket-hostname::obj ::datagram-socket)
	    (inline datagram-socket-host-address::obj ::datagram-socket)
	    (inline datagram-socket-port-number::bint ::datagram-socket)
	    (inline datagram-socket-output::output-port ::datagram-socket)
	    (inline datagram-socket-input::input-port ::datagram-socket)
	    (inline datagram-socket-close ::datagram-socket)
	    (inline datagram-socket-receive ::datagram-socket ::int)
	    (inline datagram-socket-send ::datagram-socket ::bstring ::bstring ::int)
	    (inline datagram-socket-option ::datagram-socket ::keyword)
	    (inline datagram-socket-option-set! ::datagram-socket ::keyword ::obj))
   
   (pragma  (c-socket? nesting fail-safe)
	    (c-socket-hostname nesting fail-safe)
	    (c-socket-hostip nesting fail-safe)
	    (c-socket-down? nesting fail-safe)
	    (c-socket-port-number nesting fail-safe)
	    (c-socket-input nesting fail-safe)
	    (c-socket-output nesting fail-safe))
   
   (cond-expand
      (bigloo-c
       (pragma
	  ($socket-host-addr nesting fail-safe)
	  ($socket-host-addr=? nesting fail-safe)
	  ($socket-local-addr nesting fail-safe)
	  ($socket-local? nesting fail-safe)))))

;*---------------------------------------------------------------------*/
;*    *socket-initialized* ...                                         */
;*---------------------------------------------------------------------*/
(define *socket-initialized* #f)
(define *socket-mutex* (make-mutex "socket"))

;*---------------------------------------------------------------------*/
;*    %socket-init! ...                                                */
;*---------------------------------------------------------------------*/
(define (%socket-init!)
   (synchronize *socket-mutex*
      (unless *socket-initialized*
	 (set! *socket-initialized* #t)
	 (cond-expand (bigloo-c 
		       (c-socket-startup)
		       (register-exit-function! (lambda (x) 
						   (c-socket-cleanup)
						   x))
		       #unspecified)
		      (else
		       #unspecified)))))

;*---------------------------------------------------------------------*/
;*    socket? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (socket? obj)
   (c-socket? obj))

;*---------------------------------------------------------------------*/
;*    socket-server? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (socket-server? obj)
   (c-socket-server? obj))

;*---------------------------------------------------------------------*/
;*    socket-client? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (socket-client? obj)
   (c-socket-client? obj))

;*---------------------------------------------------------------------*/
;*    socket-hostname ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (socket-hostname socket)
   (c-socket-hostname socket))

;*---------------------------------------------------------------------*/
;*    socket-host-address ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (socket-host-address socket)
   ($socket-host-addr socket))

;*---------------------------------------------------------------------*/
;*    socket-local-address ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (socket-local-address socket::socket)
   ($socket-local-addr socket))

;*---------------------------------------------------------------------*/
;*    socket-local? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (socket-local? socket::socket)
   (cond-expand
      (bigloo-c
       ($socket-local? socket))
      (else
       (string=? (socket-local-address socket) (socket-host-address socket)))))
       
;*---------------------------------------------------------------------*/
;*    socket-host-address=? ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (socket-host-address=? socket::socket addr::bstring)
   (cond-expand
      (bigloo-c
       ($socket-host-addr=? socket addr))
      (else
       (string=? (socket-host-address socket) addr))))
      
;*---------------------------------------------------------------------*/
;*    socket-down? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (socket-down?::bool socket::socket)
   (c-socket-down? socket))

;*---------------------------------------------------------------------*/
;*    socket-port-number ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (socket-port-number::bint socket::socket)
   (c-socket-port-number socket))

;*---------------------------------------------------------------------*/
;*    socket-input ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (socket-input socket::socket)
   (c-socket-input socket))

;*---------------------------------------------------------------------*/
;*    socket-output ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (socket-output socket::socket)
   (c-socket-output socket))

;*---------------------------------------------------------------------*/
;*    make-client-socket ...                                           */
;*---------------------------------------------------------------------*/
(define (make-client-socket::socket host port #!key (domain 'inet) (inbuf #t) (outbuf #t) (timeout 0))
   (%socket-init!)
   (let ((inbuf (get-port-buffer "make-client-socket" inbuf 512))
	 (outbuf (get-port-buffer "make-client-socket" outbuf 1024)))
      (case domain
	 ((inet)
	  ($make-client-socket host port timeout inbuf outbuf))
	 ((unix local)
	  (cond-expand
	     (bigloo-c ($make-unix-socket host timeout inbuf outbuf))
	     (else (error "make-client-socket" "Unsupported domain" domain))))
	 (else
	  (error "make-client-socket" "Unknown socket domain" domain)))))

;*---------------------------------------------------------------------*/
;*    make-server-socket ...                                           */
;*---------------------------------------------------------------------*/
(define (make-server-socket::socket #!optional (port 0) #!key (name #f) (backlog 5) (ipv6 #f))
   (%socket-init!)
   ($make-server-socket name port backlog ipv6))

;*---------------------------------------------------------------------*/
;*    socket-accept ...                                                */
;*---------------------------------------------------------------------*/
(define (socket-accept socket::socket #!key (inbuf #t) (outbuf #t) (errp #t))
   (let ((inbuf (get-port-buffer "socket-accept" inbuf 512))
	 (outbuf (get-port-buffer "socket-accept" outbuf 1024)))
      ($socket-accept socket errp inbuf outbuf)))

;*---------------------------------------------------------------------*/
;*    socket-accept-many ...                                           */
;*    -------------------------------------------------------------    */
;*    This function is experimental and currently not documented.      */
;*    Currently, only the C backend implements $socket-accept-many.    */
;*---------------------------------------------------------------------*/
(define (socket-accept-many socket::socket result #!key (inbufs #t) (outbufs #t) (errp #t))
   (unless (vector? inbufs)
      (set! inbufs (make-vector (vector-length result)))
      (let loop ((i 0))
	 (when (<fx i (vector-length result))
	    (let ((buf (get-port-buffer "socket-accept-many" #t 512)))
	       (vector-set! inbufs i buf)
	       (loop (+fx i 1))))))
   (unless (vector? outbufs)
      (set! outbufs (make-vector (vector-length result)))
      (let loop ((i 0))
	 (when (<fx i (vector-length result))
	    (let ((buf (get-port-buffer "socket-accept-many" #t 512)))
	       (vector-set! outbufs i buf)
	       (loop (+fx i 1))))))
   (cond-expand
      (bigloo-c
       ($socket-accept-many socket errp inbufs outbufs result))
      (else
       (let ((sock (socket-accept socket
		      :inbuf (vector-ref inbufs 0)
		      :outbuf (vector-ref outbufs 0))))
	  (vector-set! result 0 sock)
	  1))))

;*---------------------------------------------------------------------*/
;*    socket-shutdown ...                                              */
;*---------------------------------------------------------------------*/
(define (socket-shutdown socket::socket #!optional (how #t))
   (cond
      ((eq? how #t)
       (let ((r ($socket-shutdown socket 2)))
	  (socket-close socket)
	  r))
      ((or (eq? how #f) (eq? how 'RDWR))
       ($socket-shutdown socket 2))
      ((eq? how 'WR)
       ($socket-shutdown socket 1))
      ((eq? how 'RD)
       ($socket-shutdown socket 0))
      (else
       (error "socket-shutdown" "wrong optional argument" how))))

;*---------------------------------------------------------------------*/
;*    socket-close ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (socket-close socket::socket)
   ($socket-close socket))

;*---------------------------------------------------------------------*/
;*    host ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (host hostname)
   (begin
      (%socket-init!)
      ($host hostname)))

;*---------------------------------------------------------------------*/
;*    hostinfo ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (hostinfo hostname)
   (begin
      (%socket-init!)
      ($hostinfo hostname)))

;*---------------------------------------------------------------------*/
;*    hostname ...                                                     */
;*---------------------------------------------------------------------*/
(define (hostname #!optional hostip)
   (%socket-init!)
   (if hostip
       ($gethostname-by-address hostip)
       ($gethostname)))

;*---------------------------------------------------------------------*/
;*    resolv ...                                                       */
;*---------------------------------------------------------------------*/
(define (resolv hostname type)
   (cond-expand
      (bigloo-c
       ($resolv hostname (symbol->string! type)))
      (else
       '#())))

;*---------------------------------------------------------------------*/
;*    get-interfaces ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (get-interfaces)
   (begin
      (%socket-init!)
      ($gethostinterfaces)))

;*---------------------------------------------------------------------*/
;*    get-protocols ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (get-protocols)
   ($getprotoents))

;*---------------------------------------------------------------------*/
;*    get-protocol ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-protocol protocol)
   (cond
      ((integer? protocol)
       ($getprotobynumber protocol))
      ((string? protocol)
       ($getprotobyname protocol))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    socket-option ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (socket-option socket option)
   ($getsockopt socket option))

;*---------------------------------------------------------------------*/
;*    socket-option-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (socket-option-set! socket option val)
   ($setsockopt! socket option val))

;*---------------------------------------------------------------------*/
;*    datagram-socket? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket? obj)
   ($datagram-socket? obj))

;*---------------------------------------------------------------------*/
;*    datagram-socket-server? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-server? obj)
   ($datagram-socket-server? obj))

;*---------------------------------------------------------------------*/
;*    datagram-socket-client? ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-client? obj)
   ($datagram-socket-client? obj))

;*---------------------------------------------------------------------*/
;*    datagram-socket-hostname ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-hostname socket)
   ($datagram-socket-hostname socket))

;*---------------------------------------------------------------------*/
;*    datagram-socket-host-address ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-host-address socket)
   ($datagram-socket-hostip socket))

;*---------------------------------------------------------------------*/
;*    datagram-socket-port-number ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-port-number socket)
   ($datagram-socket-port-number socket))

;*---------------------------------------------------------------------*/
;*    datagram-socket-output ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-output socket)
   (if (output-port? ($datagram-socket-port socket))
       ($datagram-socket-port socket)
       (error "datagram-socket-output"
	  "Datagram-socket has no output port" socket)))

;*---------------------------------------------------------------------*/
;*    datagram-socket-input ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-input socket)
   (if (input-port? ($datagram-socket-port socket))
       ($datagram-socket-port socket)
       (error "datagram-socket-input"
	  "Datagram-socket has no input port" socket)))

;*---------------------------------------------------------------------*/
;*    make-datagram-server-socket ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (make-datagram-server-socket #!optional (port 0))
   (%socket-init!)
   ($make-datagram-server-socket port))

;*---------------------------------------------------------------------*/
;*    make-datagram-unbound-socket ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (make-datagram-unbound-socket #!optional (family::symbol 'inet))
   (%socket-init!) 
   ($make-datagram-unbound-socket family))

;*---------------------------------------------------------------------*/
;*    make-datagram-client-socket ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (make-datagram-client-socket hostname port #!optional broadcast)
   (%socket-init!)
   ($make-datagram-client-socket hostname port broadcast))

;*---------------------------------------------------------------------*/
;*    datagram-socket-close ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-close socket)
   ($datagram-socket-close socket))
   
;*---------------------------------------------------------------------*/
;*    datagram-socket-receive ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-receive socket length)
   ($datagram-socket-receive socket length))

;*---------------------------------------------------------------------*/
;*    datagram-socket-send ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-send socket string host port)
   ($datagram-socket-send socket string host port))

;*---------------------------------------------------------------------*/
;*    datagram-socket-option ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-option socket option)
   ($dgetsockopt socket option))

;*---------------------------------------------------------------------*/
;*    datagram-socket-option-set! ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (datagram-socket-option-set! socket option val)
   ($dsetsockopt! socket option val))

