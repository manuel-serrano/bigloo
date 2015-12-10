;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/src/Llib/os.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 10 11:28:07 2014                          */
;*    Last change :  Thu Dec 10 18:37:01 2015 (serrano)                */
;*    Copyright   :  2014-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    LIBUV os                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libuv_os
   
   (include "uv.sch")
   
   (import  __libuv_types)
   
   (export  (uv-loadavg ::f64vector)
	    (inline uv-get-free-memory::double)
	    (inline uv-get-total-memory::double)
	    (inline uv-get-resident-memory::long)
	    (inline uv-cpus::vector)
	    (uv-uptime::double)
	    (inline uv-exepath::bstring)
	    (inline uv-process-title-init!)
	    (inline uv-set-process-title! ::bstring)
	    (uv-get-process-title::bstring)))

;*---------------------------------------------------------------------*/
;*    uv-loadavg ...                                                   */
;*---------------------------------------------------------------------*/
(define (uv-loadavg buf)
   ($uv-loadavg ($f64vector->double* buf 0))
   buf)

;*---------------------------------------------------------------------*/
;*    uv-get-free-memory ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (uv-get-free-memory)
   ($uv-get-free-memory))

;*---------------------------------------------------------------------*/
;*    uv-get-total-memory ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (uv-get-total-memory)
   ($uv-get-total-memory))

;*---------------------------------------------------------------------*/
;*    uv-get-resident-memory ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (uv-get-resident-memory)
   ($uv-resident-memory))

;*---------------------------------------------------------------------*/
;*    uv-cpus ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (uv-cpus)
   ($uv-cpus))

;*---------------------------------------------------------------------*/
;*    uv-uptime ...                                                    */
;*---------------------------------------------------------------------*/
(define (uv-uptime)
   (let ((uptime::double (pragma::double "0.0")))
      ($uv-uptime (&double uptime))
      uptime))

;*---------------------------------------------------------------------*/
;*    uv-exepath ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (uv-exepath)
   ($uv-exepath))

;*---------------------------------------------------------------------*/
;*    uv-process-title-init! ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (uv-process-title-init!)
   ($uv-process-title-init!))

;*---------------------------------------------------------------------*/
;*    uv-set-process-title! ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (uv-set-process-title! title)
   ($uv-set-process-title! title))

;*---------------------------------------------------------------------*/
;*    uv-get-process-title ...                                         */
;*---------------------------------------------------------------------*/
(define (uv-get-process-title)
   (let ((buf (make-string 512 #\-)))
      (let ((i ($uv-get-process-title buf 512)))
	 (if (=fx i 0)
	     (let ((j (string-index buf #a000)))
		(string-shrink! buf j))
	     ""))))
