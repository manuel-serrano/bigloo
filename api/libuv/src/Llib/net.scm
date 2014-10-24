;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/net.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 07:38:37 2014                          */
;*    Last change :  Fri Oct 24 07:00:15 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV net                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_net
   
   (include "uv.sch")
   
   (import  __libuv_types
	    __libuv_loop)
   
   (export  (uv-getaddrinfo ::bstring service
	       #!key (family 0) callback (loop (uv-default-loop)))
	    
	    (uv-inet-pton ::bstring
	       #!key (family 4))
	    
	    (uv-stream-write-queue-size::long o::UvStream)
	    (uv-stream-fd::long o::UvStream)
	    (uv-stream-write ::UvStream ::bstring ::long ::long
	       #!key callback (loop (uv-default-loop)))
	    (uv-stream-read-start ::UvStream 
	       #!key onalloc callback (loop (uv-default-loop)))
	    (uv-stream-read-stop ::UvStream)
	    (uv-stream-shutdown ::UvStream
	       #!key callback (loop (uv-default-loop)))
	    (uv-listen ::UvStream ::int
	       #!key callback (loop (uv-default-loop)))
	    (uv-accept ::UvStream ::UvTcp)
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
	    (uv-tcp-getpeername::obj ::UvTcp)))

;*---------------------------------------------------------------------*/
;*    %uv-init ::UvTcp ...                                             */
;*---------------------------------------------------------------------*/
(define-method (%uv-init o::UvTcp)
   (with-access::UvTcp o (($tcp $builtin) loop)
      (with-access::UvLoop loop (($loop $builtin))
	 (set! $tcp ($uv-tcp-create ($uv-loop-t $loop) o))
	 o)))

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
      ($uv-stream-fd ($uv-stream-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-stream-write ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-stream-write o::UvStream buf offset length #!key callback (loop (uv-default-loop)))
   ($uv-write o buf offset length callback loop))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-start ...                                         */
;*---------------------------------------------------------------------*/
(define (uv-stream-read-start o::UvStream #!key onalloc callback (loop (uv-default-loop)))
   ($uv-read-start o onalloc callback loop))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-stop ...                                          */
;*---------------------------------------------------------------------*/
(define (uv-stream-read-stop o::UvStream)
   (with-access::UvStream o ($builtin)
      ($uv-read-stop ($uv-stream-t $builtin))))

;*---------------------------------------------------------------------*/
;*    uv-stream-shutdown ...                                           */
;*---------------------------------------------------------------------*/
(define (uv-stream-shutdown handle #!key callback (loop (uv-default-loop)))
   ($uv-shutdown handle callback loop))
   
;*---------------------------------------------------------------------*/
;*    uv-listen ...                                                    */
;*---------------------------------------------------------------------*/
(define (uv-listen handle backlog #!key callback (loop (uv-default-loop)))
   ($uv-listen handle backlog callback loop))

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
   ($uv-tcp-connect handle host port family callback loop))

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
