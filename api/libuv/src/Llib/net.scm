;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/net.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 07:38:37 2014                          */
;*    Last change :  Wed Jul 30 18:23:52 2014 (serrano)                */
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
	    
	    (uv-stream-write-queue-size::long o::UvStream)
	    (uv-stream-fd::long o::UvStream)
	    
	    (uv-inet-pton ::bstring
	       #!key (family 4))
	    (uv-tcp-connect ::UvTcp ::bstring ::int
	       #!key callback (loop (uv-default-loop)))
	    
	    (uv-stream-write ::UvStream ::bstring ::long
	       #!key callback (loop (uv-default-loop)))
	    (uv-stream-read-start ::UvStream 
	       #!key callback (loop (uv-default-loop)))
	    (uv-stream-read-stop ::UvStream)))

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
;*    uv-inet-pton ...                                                 */
;*---------------------------------------------------------------------*/
(define (uv-inet-pton addr #!key (family 4))
   ($uv-inet-pton addr family))

;*---------------------------------------------------------------------*/
;*    uv-tcp-connect ...                                               */
;*---------------------------------------------------------------------*/
(define (uv-tcp-connect handle host port #!key callback (loop (uv-default-loop)))
   ($uv-tcp-connect handle host port callback loop))

;*---------------------------------------------------------------------*/
;*    uv-stream-write ...                                              */
;*---------------------------------------------------------------------*/
(define (uv-stream-write o::UvStream buf length #!key callback (loop (uv-default-loop)))
   ($uv-stream-write o buf length callback loop))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-start ...                                         */
;*---------------------------------------------------------------------*/
(define (uv-stream-read-start o::UvStream #!key callback (loop (uv-default-loop)))
   ($uv-read-start o callback loop))

;*---------------------------------------------------------------------*/
;*    uv-stream-read-stop ...                                         */
;*---------------------------------------------------------------------*/
(define (uv-stream-read-stop o::UvStream)
   (with-access::UvStream o ($builtin)
      ($uv-read-stop ($uv-stream-t $builtin))))
