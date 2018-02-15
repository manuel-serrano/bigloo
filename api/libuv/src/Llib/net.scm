;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/libuv/src/Llib/net.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 07:38:37 2014                          */
;*    Last change :  Thu Feb 15 05:23:06 2018 (serrano)                */
;*    Copyright   :  2014-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV net                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_net
   
   (include "uv.sch")
   
   (import  __libuv_types
	    __libuv_loop
	    __libuv_handle)
   
   (export  (uv-getaddrinfo ::bstring service
	       #!key (family 0) callback (loop (uv-default-loop)))
	    
	    (uv-inet-pton ::bstring
	       #!key (family 4))
	    
	    (uv-stream-write-queue-size::long o::UvStream)
	    (uv-stream-fd::long o::UvStream)
	    (uv-stream-write ::UvStream ::bstring ::long ::long
	       #!key callback (loop (uv-default-loop)))
	    (uv-stream-write2 ::UvStream ::bstring ::long ::long ::obj
	       #!key callback (loop (uv-default-loop)))
	    (uv-stream-read-start ::UvStream 
	       #!key onalloc callback (loop (uv-default-loop)))
	    (uv-stream-read-stop ::UvStream)
	    (uv-stream-shutdown ::UvStream
	       #!key callback (loop (uv-default-loop)))
	    (uv-listen ::UvStream ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-accept ::UvStream ::UvStream)
	    (uv-closing?::bool ::UvStream)
	    (uv-writable?::bool ::UvStream)
	    (uv-readable?::bool ::UvStream)
	    
	    (uv-tcp-connect ::UvTcp ::bstring ::int
	       #!key (family::int 4) callback (loop (uv-default-loop)))
	    (uv-tcp-open::int ::UvTcp ::int)
	    (uv-tcp-bind ::UvTcp ::bstring ::int #!key (family::int 4))
	    (uv-tcp-nodelay::int ::UvTcp ::bool)
	    (uv-tcp-keepalive::int ::UvTcp ::bool ::int)
	    (uv-tcp-simultaneous-accepts::int ::UvTcp ::bool)
	    (uv-tcp-getsockname::obj ::UvTcp)
	    (uv-tcp-getpeername::obj ::UvTcp)
	    
	    (uv-udp-bind ::UvUdp ::bstring ::int #!key (family::int 4) (flags::int 0))
	    (uv-udp-getsockname::obj ::UvUdp)
	    (uv-udp-send::obj ::UvUdp ::bstring ::long ::long ::long ::bstring
	       #!key (family 4) callback (loop (uv-default-loop)))
	    (uv-udp-recv-start ::UvUdp
	       #!key onalloc callback (loop (uv-default-loop)))
	    (uv-udp-recv-stop::int ::UvUdp)
	    (uv-udp-set-ttl handle::UvUdp ::int)
	    (uv-udp-set-multicast-ttl handle::UvUdp ::int)
	    (uv-udp-set-multicast-loop ::UvUdp ::bool)
	    (uv-udp-set-broadcast ::UvUdp ::bool)
	    (uv-udp-set-membership ::UvUdp ::bstring ::obj ::symbol)
	    
	    (uv-tty-mode-set! ::UvTty ::symbol)
	    (uv-tty-get-window-size::vector ::UvTty)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvTcp ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvTcp)
   (with-access::UvTcp o (($tcp $builtin) loop)
      (with-access::UvLoop loop (($loop $builtin))
	 (set! $tcp ($uv-tcp-create ($uv-loop-t $loop) o))
	 o)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvUdp ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvUdp)
   (with-access::UvUdp o (($udp $builtin) loop)
      (with-access::UvLoop loop (($loop $builtin))
	 (set! $udp ($uv-udp-create ($uv-loop-t $loop) o)))
      o))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvTty ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvTty)
   (with-access::UvTty o (($tty $builtin) loop fd readable)
      (with-access::UvLoop loop (($loop $builtin))
	 (set! $tty ($uv-tty-create ($uv-loop-t $loop) o fd readable)))
      o))

;*---------------------------------------------------------------------*/
;*    uv-close ::UvStream ...                                          */
;*---------------------------------------------------------------------*/
(define-method (uv-close o::UvStream #!optional callback)
   (if (procedure? callback)
       (let ((cb callback))
	  (set! callback
	     (lambda ()
		(with-access::UvStream o (loop)
		   (uv-pop-gcmark! loop o))
		(cb))))
       (with-access::UvStream o (loop)
	  (uv-pop-gcmark! loop o)))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    uv-getaddrinfo ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-getaddrinfo node service #!key (family 0)
	   callback (loop (uv-default-loop)))
   (cond
      ((and (string? node) (string? service))
       ($uv-getaddrinfo node service family callback loop))
      ((and (string? node) (not service))
       ($uv-getaddrinfo node $string-nil family callback loop))
      ((and (not node) (string? service))
       ($uv-getaddrinfo $string-nil service family callback loop))
      (else
       (error "uv-getaddrinfo"
	  "at least one of node or service must be a string"
	  (cons node service)))))

;*---------------------------------------------------------------------*/
;*    uv-inet-pton ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-inet-pton addr #!key (family 4))
   ($uv-inet-pton addr family))

;*---------------------------------------------------------------------*/
;*    uv-stream-write-queue-size ...                                   */
;*---------------------------------------------------------------------*/
(define (uv-stream-write-queue-size o::UvStream)
   (with-access::UvStream o ($builtin)
      ($uv-stream-write-queue-size ($uv-stream-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-stream-fd ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-stream-fd o::UvStream)
   (with-access::UvStream o ($builtin)
      (if (isa? o UvUdp)
	  ($uv-udp-fd ($uv-udp-t $builtin))
	  ($uv-stream-fd ($uv-stream-t $builtin)))))

;*---------------------------------------------------------------------*/
;*    uv-stream-write ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-stream-write o::UvStream buf offset len #!key callback (loop (uv-default-loop)))
   (letrec ((cb (lambda (status)
		   ;; make sure buf is referenced to prevent
		   ;; premature collection
		   (unless (eq? buf cb)
		      (uv-pop-gcmark! o cb)
		      (callback status)))))
      (let ((r ($uv-write o buf offset len cb loop)))
	 (when (=fx r 0)
	    (uv-push-gcmark! o cb))
	 r)))

;*---------------------------------------------------------------------*/
;*    uv-stream-write2 ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-stream-write2 o::UvStream buf offset len handle::obj #!key callback (loop (uv-default-loop)))
   (letrec ((cb (lambda (status)
		   ;; make sure buf is referenced to prevent
		   ;; premature collection
		   (unless (eq? buf cb)
		      (uv-pop-gcmark! o cb)
		      (callback status)))))
      (let ((r ($uv-write2 o buf offset len handle cb loop)))
	 (when (=fx r 0)
	    (uv-push-gcmark! o cb))
	 r)))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-start ...                                         */
;*---------------------------------------------------------------------*/
(define (uv-stream-read-start o::UvStream #!key onalloc callback (loop (uv-default-loop)))
   (let ((r ($uv-read-start o onalloc callback loop)))
      (when (=fx r 0)
	 (with-access::UvStream o (%callback)
	    (set! %callback callback))
	 (uv-push-gcmark! loop o))
      r))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-stop ...                                          */
;*---------------------------------------------------------------------*/
(define (uv-stream-read-stop o::UvStream)
   (with-access::UvStream o ($builtin loop)
      (with-access::UvStream o (%callback)
	 (set! %callback #f))
      (uv-pop-gcmark! loop o)
      ($uv-read-stop ($uv-stream-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-stream-shutdown ...                                           */
;*---------------------------------------------------------------------*/
(define (uv-stream-shutdown handle #!key callback (loop (uv-default-loop)))
   (let ((r ($uv-shutdown handle callback loop)))
      (when (=fx r 0)
	 (uv-push-gcmark! handle callback)
	 (uv-push-gcmark! loop handle))
      r))
   
;*---------------------------------------------------------------------*/
;*    uv-listen ...                                                    */
;*---------------------------------------------------------------------*/
(define (uv-listen handle backlog #!key callback (loop (uv-default-loop)))
   (let ((r ($uv-listen handle backlog callback loop)))
      (when (=fx r 0)
	 (uv-push-gcmark! handle callback)
	 (uv-push-gcmark! loop handle))
      r))

;*---------------------------------------------------------------------*/
;*    uv-accept ...                                                    */
;*---------------------------------------------------------------------*/
(define (uv-accept server client)
   (with-access::UvStream server (($sbuiltin $builtin))
      (with-access::UvStream client (($cbuiltin $builtin))
	 ($uv-accept ($uv-stream-t $sbuiltin) ($uv-stream-t $cbuiltin)))))

;*---------------------------------------------------------------------*/
;*    uv-closing? ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-closing? handle)
   (with-access::UvStream handle ($builtin)
      ($uv-is-closing $builtin)))
      
;*---------------------------------------------------------------------*/
;*    uv-writable? ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-writable? handle)
   (with-access::UvStream handle ($builtin)
      ($uv-is-writable ($uv-stream-t $builtin))))
      
;*---------------------------------------------------------------------*/
;*    uv-readable? ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-readable? handle)
   (with-access::UvStream handle ($builtin)
      ($uv-is-readable ($uv-stream-t $builtin))))
      
;*---------------------------------------------------------------------*/
;*    uv-tcp-connect ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-tcp-connect handle host port #!key (family::int 4) callback (loop (uv-default-loop)))
   (let ((r ($uv-tcp-connect handle host port family callback loop)))
      (when (=fx r 0)
	 (uv-push-gcmark! handle callback)
	 (uv-push-gcmark! loop handle))
      r))

;*---------------------------------------------------------------------*/
;*    tcp-servers ...                                                  */
;*---------------------------------------------------------------------*/
(define tcp-servers '())
(define tcp-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    uv-close ::UvTcp ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uv-close o::UvTcp #!optional callback)
   (if (procedure? callback)
       (let ((cb callback))
	  (set! callback
	     (lambda ()
		(synchronize tcp-mutex
		   (set! tcp-servers (remq! o tcp-servers)))
		(cb))))
       (synchronize tcp-mutex
	  (set! tcp-servers (remq! o tcp-servers))))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    uv-tcp-open ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-tcp-open handle fd)
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-open ($uv-tcp-t $builtin) fd)))

;*---------------------------------------------------------------------*/
;*    uv-tcp-bind ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-tcp-bind handle host port #!key (family::int 4))
   (synchronize tcp-mutex
      (set! tcp-servers (cons handle tcp-servers)))
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-bind ($uv-tcp-t $builtin) host port family)))

;*---------------------------------------------------------------------*/
;*    uv-tcp-nodelay ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-tcp-nodelay::int handle enable)
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-nodelay ($uv-tcp-t $builtin) enable)))

;*---------------------------------------------------------------------*/
;*    uv-tcp-keepalive ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-tcp-keepalive::int handle enable delay)
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-keepalive ($uv-tcp-t $builtin) enable delay)))

;*---------------------------------------------------------------------*/
;*    uv-tcp-simultaneous-accepts ...                                  */
;*---------------------------------------------------------------------*/
(define (uv-tcp-simultaneous-accepts::int handle enable)
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-simultaneous-accepts ($uv-tcp-t $builtin) enable)))

;*---------------------------------------------------------------------*/
;*    uv-tcp-getsockname ...                                           */
;*---------------------------------------------------------------------*/
(define (uv-tcp-getsockname::obj handle)
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-getsockname ($uv-tcp-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-tcp-getpeername ...                                           */
;*---------------------------------------------------------------------*/
(define (uv-tcp-getpeername::obj handle)
   (with-access::UvTcp handle ($builtin)
      ($uv-tcp-getpeername ($uv-tcp-t $builtin))))

;*---------------------------------------------------------------------*/
;*    udp-servers ...                                                  */
;*---------------------------------------------------------------------*/
(define udp-servers '())
(define udp-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    uv-close ::UvUdp ...                                             */
;*---------------------------------------------------------------------*/
(define-method (uv-close o::UvUdp #!optional callback)
   (if (procedure? callback)
       (let ((cb callback))
	  (set! callback
	     (lambda ()
		(synchronize udp-mutex
		   (set! udp-servers (remq! o udp-servers)))
		(cb))))
       (synchronize udp-mutex
	  (set! udp-servers (remq! o udp-servers))))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    uv-udp-bind ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-udp-bind handle host port #!key (family::int 4) (flags::int 0))
   (synchronize udp-mutex
      (set! udp-servers (cons handle udp-servers)))
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-bind ($uv-udp-t $builtin) host port family flags)))

;*---------------------------------------------------------------------*/
;*    uv-udp-recv-start ...                                            */
;*---------------------------------------------------------------------*/
(define (uv-udp-recv-start o::UvUdp #!key onalloc callback (loop (uv-default-loop)))
   (with-access::UvUdp o (%procm)
      (set! %procm (cons callback %procm))
      (uv-push-gcmark! loop o)
      ($uv-udp-recv-start o onalloc callback loop)))

;*---------------------------------------------------------------------*/
;*    uv-udp-recv-stop ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-udp-recv-stop handle)
   (with-access::UvUdp handle ($builtin %procm loop)
      (set! %procm '())
      (uv-pop-gcmark! loop handle)
      ($uv-udp-recv-stop ($uv-udp-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-udp-getsockname ...                                           */
;*---------------------------------------------------------------------*/
(define (uv-udp-getsockname::obj handle)
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-getsockname ($uv-udp-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-udp-send ...                                                  */
;*---------------------------------------------------------------------*/
(define (uv-udp-send handle::UvUdp buf offset len port address #!key (family 4) callback (loop (uv-default-loop)))
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-send ($uv-udp-t $builtin) buf
	 offset len
	 port address
	 family callback loop)))

;*---------------------------------------------------------------------*/
;*    uv-udp-set-ttl ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-udp-set-ttl handle::UvUdp ttl)
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-set-ttl ($uv-udp-t $builtin) ttl)))

;*---------------------------------------------------------------------*/
;*    uv-udp-set-multicast-ttl ...                                     */
;*---------------------------------------------------------------------*/
(define (uv-udp-set-multicast-ttl handle::UvUdp ttl)
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-set-multicast-ttl ($uv-udp-t $builtin) ttl)))

;*---------------------------------------------------------------------*/
;*    uv-udp-set-multicast-loop ...                                    */
;*---------------------------------------------------------------------*/
(define (uv-udp-set-multicast-loop handle::UvUdp on)
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-set-multicast-loop ($uv-udp-t $builtin) on)))

;*---------------------------------------------------------------------*/
;*    uv-udp-set-broadcast ...                                         */
;*---------------------------------------------------------------------*/
(define (uv-udp-set-broadcast handle::UvUdp on)
   (with-access::UvUdp handle ($builtin)
      ($uv-udp-set-broadcast ($uv-udp-t $builtin) on)))

;*---------------------------------------------------------------------*/
;*    uv-udp-set-membership ...                                        */
;*---------------------------------------------------------------------*/
(define (uv-udp-set-membership handle::UvUdp addr iface membership)
   (with-access::UvUdp handle ($builtin)
      (let ((act (if (eq? membership 'join-group)
		     $uv-membership-join-group
		     $uv-membership-leave-group)))
	 (if (string? iface)
	     ($uv-udp-set-membership ($uv-udp-t $builtin) addr iface act)
	     ($uv-udp-set-membership ($uv-udp-t $builtin) addr $string-nil act)))))

;*---------------------------------------------------------------------*/
;*    uv-tty-mode-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (uv-tty-mode-set! handle::UvTty mode)
   (with-access::UvTty handle ($builtin)
      ;; uv 1.1.1
      #;($uv-tty-set-mode ($uv-tty-t $builtin)
	(case mode
	   ((normal) $uv-tty-mode-normal)
	   ((raw) $uv-tty-mode-raw)
	   ((io) $uv-tty-mode-io)
	   (else (error "uv-tty-mode" "bad mode" mode))))
      ($uv-tty-set-mode ($uv-tty-t $builtin)
	 (case mode
	    ((normal) 0)
	    ((raw) 1)
	    (else (error "uv-tty-mode" "bad mode" mode))))))

;*---------------------------------------------------------------------*/
;*    uv-tty-get-window-size ...                                       */
;*---------------------------------------------------------------------*/
(define (uv-tty-get-window-size handle::UvTty)
   (with-access::UvTty handle ($builtin)
      ($uv-tty-get-winsize ($uv-tty-t $builtin))))
