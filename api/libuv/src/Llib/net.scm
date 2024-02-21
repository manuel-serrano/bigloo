;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/libuv/src/Llib/net.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 07:38:37 2014                          */
;*    Last change :  Wed Feb 14 18:04:50 2024 (serrano)                */
;*    Copyright   :  2014-24 Manuel Serrano                            */
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
	    (inline uv-stream-write ::UvStream ::bstring ::long ::long
	       #!key loop callback arg0 arg1 arg2 arg3 arg4)
	    (inline uv-stream-write2 ::UvStream ::bstring ::long ::long ::obj
	       #!key loop callback arg0 arg1 arg2 arg3 arg4)
	    (inline uv-stream-read-start ::UvStream 
	       #!key loop onalloc callback)
	    (inline uv-stream-read-stop ::UvStream)
	    (inline uv-stream-shutdown ::UvStream
	       #!key loop callback)
	    (inline uv-listen ::UvStream ::int
	       #!key loop callback)
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
	    (inline uv-udp-recv-start ::UvUdp
	       #!key loop onalloc callback)
	    (inline uv-udp-recv-stop::int ::UvUdp)
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
   ($bgl-uv-stream-close o callback)
   #t)

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
(define-inline (uv-stream-write o::UvStream buf offset len
		  #!key loop callback arg0 arg1 arg2 arg3 arg4)
   ;; loop is kept for backward compatibility but it is unused
   ($uv-write o buf offset len callback arg0 arg1 arg2 arg3 arg4))

;*---------------------------------------------------------------------*/
;*    uv-stream-write2 ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (uv-stream-write2 o::UvStream buf offset len handle::obj
		  #!key loop callback arg0 arg1 arg2 arg3 arg4)
   ;; loop is kept for backward compatibility but it is unused
   ($uv-write2 o buf offset len handle callback arg0 arg1 arg2 arg3 arg4))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-start ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (uv-stream-read-start o::UvStream #!key loop onalloc callback)
   ;; loop is kept for backward compatibility but it is unused
   ($uv-read-start o onalloc callback))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-stop ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (uv-stream-read-stop o::UvStream)
   ($uv-read-stop o))

;*---------------------------------------------------------------------*/
;*    uv-stream-shutdown ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (uv-stream-shutdown handle #!key loop callback)
   ;; loop is kept for backward compatibility but it is unused
   ($uv-shutdown handle callback))
   
;*---------------------------------------------------------------------*/
;*    uv-listen ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (uv-listen handle backlog #!key loop callback)
   ;; loop is kept for backward compatibility but it is unused
   ($uv-listen handle backlog callback))

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
	 (uv-push-gcmark! handle callback "uv-tcp-connect")
	 (uv-push-gcmark! loop handle "uv-tcp-connect"))
      r))

;*---------------------------------------------------------------------*/
;*    tcp-servers ...                                                  */
;*---------------------------------------------------------------------*/
(define tcp-servers '())
(define tcp-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    uv-close ::UvTcp ...                                             */
;*---------------------------------------------------------------------*/
;* (define-method (uv-close o::UvTcp #!optional callback)              */
;*    (if (procedure? callback)                                        */
;*        (let ((cb callback))                                         */
;* 	  (set! callback                                               */
;* 	     (lambda ()                                                */
;* 		(synchronize tcp-mutex                                 */
;* 		   (set! tcp-servers (remq! o tcp-servers)))           */
;* 		(cb))))                                                */
;*        (synchronize tcp-mutex                                       */
;* 	  (set! tcp-servers (remq! o tcp-servers))))                   */
;*    (call-next-method))                                              */

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
;* (define-method (uv-close o::UvUdp #!optional callback)              */
;*    (if (procedure? callback)                                        */
;*        (let ((cb callback))                                         */
;* 	  (set! callback                                               */
;* 	     (lambda ()                                                */
;* 		(synchronize udp-mutex                                 */
;* 		   (set! udp-servers (remq! o udp-servers)))           */
;* 		(cb))))                                                */
;*        (synchronize udp-mutex                                       */
;* 	  (set! udp-servers (remq! o udp-servers))))                   */
;*    (call-next-method))                                              */

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
(define-inline (uv-udp-recv-start o::UvUdp #!key loop onalloc callback)
   ;; loop is kept for backward compatibility but it is unused
   ($uv-udp-recv-start o onalloc callback))

;*---------------------------------------------------------------------*/
;*    uv-udp-recv-stop ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (uv-udp-recv-stop o::UvUdp)
   ($uv-udp-recv-stop o))

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
