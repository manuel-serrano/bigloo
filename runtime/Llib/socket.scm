;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/socket.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 29 18:45:17 1998                          */
;*    Last change :  Sun Dec 28 05:15:46 2008 (serrano)                */
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
	    ($make-server-socket::socket (::obj ::int ::int)
					 "bgl_make_server_socket")
	    ($socket-accept::obj (::socket ::bool ::bstring ::bstring)
				 "bgl_socket_accept")
	    ($socket-accept-many::long (::socket ::bool ::vector ::vector ::vector)
				       "bgl_socket_accept_many")
	    (c-socket-local-addr::bstring (::socket) "socket_local_addr")
	    (c-socket-shutdown::obj (::socket ::bool) "socket_shutdown")
	    (c-socket-close::obj (::socket) "socket_close")
	    (macro c-socket-server?::bool (::obj) "BGL_SOCKET_SERVERP")
	    (macro c-socket-client?::bool (::obj) "BGL_SOCKET_CLIENTP")
	    
	    (c-host::bstring (::bstring) "bgl_host")
	    ($hostinfo::pair-nil (::bstring) "bgl_hostinfo")
	    
	    ($gethostname::bstring () "bgl_gethostname")
	    ($getprotoents::pair-nil () "bgl_getprotoents")
	    ($getprotobyname::obj (::string) "bgl_getprotobyname")
	    ($getprotobynumber::obj (::long) "bgl_getprotobynumber")
	    
	    ($getsockopt::obj (::socket ::keyword) "bgl_getsockopt")
	    ($setsockopt!::obj (::socket ::keyword ::obj) "bgl_setsockopt"))
   
   (java    (class foreign
	       (method static c-socket?::bool (::obj)
		       "SOCKETP")
	       
	       (method static $make-client-socket::socket (::bstring ::int ::int ::bstring ::bstring)
		       "bgl_make_client_socket")
	       (method static $make-server-socket::socket (::obj ::int ::int)
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
	       (method static c-socket-local-addr::bstring (::socket)
		       "socket_local_addr")
	       (method static c-socket-shutdown::obj (::socket ::bool)
		       "socket_shutdown")
	       (method static c-socket-close::obj (::socket)
		       "socket_close")
	       (method static c-socket-server?::bool (::obj)
		       "BGL_SOCKET_SERVERP")
	       (method static c-socket-client?::bool (::obj)
		       "BGL_SOCKET_CLIENTP")
	       
	       (method static c-host::bstring (::bstring)
		       "bgl_host")
	       (method static $hostinfo::obj (::bstring)
		       "bgl_hostinfo")
	       (method static $gethostname::bstring ()
		       "bgl_gethostname")
	       (method static $getprotoents::pair-nil ()
		       "bgl_getprotoents")
	       (method static $getprotobyname::obj (::string)
		       "bgl_getprotobyname")
	       (method static $getprotobynumber::obj (::int)
		       "bgl_getprotobynumber")
	       (method static $getsockopt::obj (::socket ::keyword)
		       "bgl_getsockopt")
	       (method static $setsockopt!::obj (::socket ::keyword ::obj)
		       "bgl_setsockopt")))
   
   (export  (%socket-init!)
	    (inline socket?::bool ::obj)
	    (inline socket-server?::bool ::obj)
	    (inline socket-client?::bool ::obj)
	    (inline socket-hostname::obj ::socket)
	    (inline socket-host-address::obj ::socket)
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
					#!key (name #f) (backlog 5))
	    (socket-accept::obj ::socket #!key (inbuf #t) (outbuf #t) (errp #t))
	    (socket-accept-many::obj ::socket ::vector
				     #!key (inbufs #t) (outbufs #t) (errp #t))
	    (inline socket-local-address ::socket)
	    (inline socket-shutdown::obj ::socket #!optional (close #t))
	    (inline socket-close::obj ::socket)
	    (inline host::bstring ::bstring)
	    (inline hostinfo::pair-nil ::bstring)
	    (inline hostname::bstring)
	    (inline get-protocols::pair-nil)
	    (get-protocol ::obj)
	    (inline socket-option ::socket ::keyword)
	    (inline socket-option-set! ::socket ::keyword ::obj))
   
   (pragma  (c-socket? nesting)
	    (c-socket-hostname nesting)
	    (c-socket-hostip nesting)
	    (c-socket-down? nesting)
	    (c-socket-port-number nesting)
	    (c-socket-input nesting)
	    (c-socket-output nesting)))

;*---------------------------------------------------------------------*/
;*    *socket-initialized* ...                                         */
;*---------------------------------------------------------------------*/
(define *socket-initialized* #f)
(define *socket-mutex* (make-mutex "socket"))

;*---------------------------------------------------------------------*/
;*    %socket-init! ...                                                */
;*---------------------------------------------------------------------*/
(define (%socket-init!)
   (mutex-lock! *socket-mutex*)
   (unless *socket-initialized*
      (set! *socket-initialized* #t)
      (cond-expand (bigloo-c 
		    (c-socket-startup)
		    (register-exit-function! (lambda (x) 
						(c-socket-cleanup)
						x))
		    #unspecified)
		   (else
		    #unspecified)))
   (mutex-unlock! *socket-mutex*))

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
   (c-socket-hostip socket))

;*---------------------------------------------------------------------*/
;*    socket-local-address ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (socket-local-address socket::socket)
   (c-socket-local-addr socket))

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
   (let ((inbuf (get-port-buffer 'make-client-socket inbuf 512))
	 (outbuf (get-port-buffer 'make-client-socket outbuf 1024)))
      (case domain
	 ((inet)
	  ($make-client-socket host port timeout inbuf outbuf))
	 ((unix local)
	  (cond-expand
	     (bigloo-c ($make-unix-socket host timeout inbuf outbuf))
	     (else (error 'make-client-socket "Unsupported domain" domain))))
	 (else
	  (error 'make-client-socket "Unknown socket domain" domain)))))

;*---------------------------------------------------------------------*/
;*    make-server-socket ...                                           */
;*---------------------------------------------------------------------*/
(define (make-server-socket::socket #!optional (port 0) #!key (name #f) (backlog 5))
   (%socket-init!)
   ($make-server-socket name port backlog))

;*---------------------------------------------------------------------*/
;*    socket-accept ...                                                */
;*---------------------------------------------------------------------*/
(define (socket-accept socket::socket #!key (inbuf #t) (outbuf #t) (errp #t))
   (let ((inbuf (get-port-buffer 'socket-accept inbuf 512))
	 (outbuf (get-port-buffer 'socket-accept outbuf 1024)))
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
	    (let ((buf (get-port-buffer 'socket-accept-many #t 512)))
	       (vector-set! inbufs i buf)
	       (loop (+fx i 1))))))
   (unless (vector? outbufs)
      (set! outbufs (make-vector (vector-length result)))
      (let loop ((i 0))
	 (when (<fx i (vector-length result))
	    (let ((buf (get-port-buffer 'socket-accept-many #t 512)))
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
(define-inline (socket-shutdown socket::socket #!optional (close #t))
   (c-socket-shutdown socket close))

;*---------------------------------------------------------------------*/
;*    socket-close ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (socket-close socket::socket)
   (c-socket-close socket))

;*---------------------------------------------------------------------*/
;*    host ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (host hostname)
   (begin
      (%socket-init!)
      (c-host hostname)))

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
(define-inline (hostname)
   (begin
      (%socket-init!)
      ($gethostname)))

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
