;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Llib/mpg123.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Sun Jun 26 15:50:31 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Bigloo binding for the mpg123 library                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_mpg123

   (include "mpg123.sch")
   
   (extern (export mpg123-error "bgl_mpg123_error"))
   
   (export (class mpg123-handle
	      (mpg123-handle-init)
	      ($builtin::$mpg123-handle read-only (default ($bgl-mpg123-new $string-null))))
	   
	   (class &mpg123-error::&error)

	   (generic mpg123-handle-init::mpg123-handle ::mpg123-handle)
	   
	   (mpg123-handle-close ::mpg123-handle)
	   (mpg123-get-format ::mpg123-handle)
	   (mpg123-error::int ::string ::string ::obj)
	   (mpg123-decode ::mpg123-handle ::bstring ::long ::bstring ::long)
	   (mpg123-position::long ::mpg123-handle ::bstring)))

;*---------------------------------------------------------------------*/
;*    mpg123-handle-init ::mpg123-handle ...                           */
;*---------------------------------------------------------------------*/
(define-generic (mpg123-handle-init o::mpg123-handle)
   (with-access::mpg123-handle o ($builtin)
      ($mpg123-open-feed $builtin))
   o)

;*---------------------------------------------------------------------*/
;*    mpg123-handle-close ...                                          */
;*---------------------------------------------------------------------*/
(define (mpg123-handle-close o::mpg123-handle)
   (with-access::mpg123-handle o ($builtin)
      ($mpg123-delete $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    mpg123-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (mpg123-error proc msg obj)
   (raise (instantiate::&mpg123-error
	     (proc proc)
	     (msg msg)
	     (obj obj)))
   0)

;*---------------------------------------------------------------------*/
;*    mpg123-decode ...                                                */
;*---------------------------------------------------------------------*/
(define (mpg123-decode m::mpg123-handle inbuf insz outbuf outsz)
   (with-access::mpg123-handle m ($builtin)
      (multiple-value-bind (status size)
	 ($bgl-mpg123-decode $builtin inbuf insz outbuf outsz)
	 (values (mpg123-decode-status->symbol status) size))))

;*---------------------------------------------------------------------*/
;*    mpg123-get-format ...                                            */
;*---------------------------------------------------------------------*/
(define (mpg123-get-format m::mpg123-handle)
   (with-access::mpg123-handle m ($builtin)
      (multiple-value-bind (rate channels encoding)
	 ($bgl-mpg123-get-format $builtin)
	 (values rate channels (mpg123-format->symbol encoding)))))

;*---------------------------------------------------------------------*/
;*    mpg123-position ...                                              */
;*---------------------------------------------------------------------*/
(define (mpg123-position m::mpg123-handle buf::bstring)
   (with-access::mpg123-handle m ($builtin)
      ($bgl-mpg123-position $builtin)))

;*---------------------------------------------------------------------*/
;*    mpg123-decode-status->symbol ...                                 */
;*---------------------------------------------------------------------*/
(define (mpg123-decode-status->symbol s)
   (cond
      ((=fx s $mpg123-ok) 'ok)
      ((=fx s $mpg123-new-format) 'new-format)
      ((=fx s $mpg123-err) 'err)
      ((=fx s $mpg123-need-more) 'need-more)
      ((=fx s $mpg123-done) 'done)
      (else (raise (instantiate::&mpg123-error
		      (proc "mpg123-decode-status->symbol")
		      (msg "Unknown decode-status")
		      (obj s))))))

;*---------------------------------------------------------------------*/
;*    mpg123-format->symbol ...                                        */
;*---------------------------------------------------------------------*/
(define (mpg123-format->symbol f)
   (cond
      ((=fx f $mpg123-enc-signed-16) 's16)
      ((=fx f $mpg123-enc-unsigned-16) 'u16)
      ((=fx f $mpg123-enc-unsigned-8) 'u8)
      ((=fx f $mpg123-enc-signed-8) 's8)
      ((=fx f $mpg123-enc-alaw-8) 'a-law)
      ((=fx f $mpg123-enc-ulaw-8) 'mu-law)
      ((=fx f $mpg123-enc-signed-32) 's32)
      ((=fx f $mpg123-enc-unsigned-32) 'u32)
      ((=fx f $mpg123-enc-signed-24)
       (if (eq? (bigloo-config 'endianess) 'little-endian) 's24-3le 's24-3be))
      ((=fx f $mpg123-enc-unsigned-24)
       (if (eq? (bigloo-config 'endianess) 'little-endian) 'u24-3le 'u24-3be))
      ((=fx f $mpg123-enc-float-32) 'float)
      ((=fx f $mpg123-enc-float-64) 'float64)
      (else (raise (instantiate::&mpg123-error
		      (proc "mpg123-format->alsa-format")
		      (msg "Unknown format")
		      (obj f))))))

;*---------------------------------------------------------------------*/
;*    Force the initialization                                         */
;*---------------------------------------------------------------------*/
(let ((err ($mpg123-init)))
   (unless (=fx err $mpg123-ok)
      (raise (instantiate::&mpg123-error
		(proc "mpg123")
		(msg ($mpg123-plain-strerror err))
		(obj #f)))))
