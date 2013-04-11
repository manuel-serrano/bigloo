;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/upnp/src/Llib/soap.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 14:07:08 2005                          */
;*    Last change :  Sun Apr  7 07:40:27 2013 (serrano)                */
;*    Copyright   :  2005-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Soap utilities                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __upnp_soap
   (export (soap-envelope begin end args)))

;*---------------------------------------------------------------------*/
;*    soap-envelope ...                                                */
;*---------------------------------------------------------------------*/
(define (soap-envelope start end args)
   (let loop ((l args)
	      (acc '()))
      (if (null? l)
	  (apply string-append start (reverse (cons end acc)))
	  (let ((s (keyword->string (car l))))
	     (loop (cddr l)
		(cons (format "<~a>~a</~a>" s (cadr l) s) acc))))))
