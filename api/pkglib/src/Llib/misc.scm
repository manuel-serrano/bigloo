;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkglib/src/Llib/misc.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 10:40:52 2006                          */
;*    Last change :  Sun Nov 18 15:07:59 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Pkglib misc                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_misc

   (import __pkglib_param)

   (export (class &pkglib-error::&error)
	   (pkglib-error proc msg obj)
	   (pkglib-verb ::int . args)
	   (pkglib-color ::obj ::obj)
	   (assq/default ::obj ::pair-nil #!optional (default #unspecified))
	   (assq*::obj ::obj ::pair-nil #!optional (default #unspecified))))

;*---------------------------------------------------------------------*/
;*    pkglib-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (pkglib-error proc msg obj)
   (raise (instantiate::&pkglib-error
	     (proc proc)
	     (msg msg)
	     (obj obj))))

;*---------------------------------------------------------------------*/
;*    *verb-mutex* ...                                                 */
;*---------------------------------------------------------------------*/
(define *verb-mutex* (make-mutex 'verb))

;*---------------------------------------------------------------------*/
;*    pkglib-verb ...                                                  */
;*---------------------------------------------------------------------*/
(define (pkglib-verb level . args)
   (when (>=fx (pkglib-verbose) level)
      (synchronize *verb-mutex*
	 (for-each display args)
	 (flush-output-port (current-output-port)))))

;*---------------------------------------------------------------------*/
;*    pkglib-color ...                                                 */
;*---------------------------------------------------------------------*/
(define (pkglib-color col msg)
   (let ((c (let ((c (assq col (pkglib-colors))))
	       (if (pair? c)
		   (+fx (modulofx (cadr c) 16) 1)
		   0))))
      (trace-color c msg)))

;*---------------------------------------------------------------------*/
;*    assq/default ...                                                 */
;*---------------------------------------------------------------------*/
(define (assq/default key lst #!optional (default #unspecified))
   (let ((c (assq key lst)))
      (cond
	 ((pair? c)
	  (cdr c))
	 (else
	  default))))

;*---------------------------------------------------------------------*/
;*    assq* ...                                                        */
;*---------------------------------------------------------------------*/
(define (assq* key lst #!optional (default #unspecified))
   (let loop ((lst lst)
	      (res '()))
      (cond
	 ((null? lst)
	  (if (pair? res)
	      (reverse! res)
	      default))
	 ((eq? (caar lst) key)
	  (loop (cdr lst) (append (reverse (cdr (car lst))) res)))
	 (else
	  (loop (cdr lst) res)))))
