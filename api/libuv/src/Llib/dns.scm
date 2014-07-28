;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/dns.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 25 07:38:37 2014                          */
;*    Last change :  Fri Jul 25 09:41:06 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV dns                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_dns

   (include "uv.sch")
   
   (import  __libuv_types
	    __libuv_loop)

   (export  (uv-getaddrinfo node service
	       #!key (family 0) callback (loop (uv-default-loop)))))

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
