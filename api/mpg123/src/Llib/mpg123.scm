;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Llib/mpg123.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Sun Apr 19 09:19:29 2015 (serrano)                */
;*    Copyright   :  2011-15 Manuel Serrano                            */
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
	      ($builtin::$mpg123-handle (default (%$mpg123-handle-nil)))
	      (decoder read-only (default #f))
	      (size::long (default 0)))
	   
	   (class &mpg123-error::&error)

	   (generic mpg123-handle-init::mpg123-handle ::mpg123-handle)
	   (%$mpg123-handle-nil)
	   
	   (mpg123-handle-close ::mpg123-handle)
	   (mpg123-handle-reset! ::mpg123-handle)
	   (mpg123-get-format ::mpg123-handle)
	   (mpg123-param-get ::mpg123-handle ::symbol)
	   (mpg123-error::int ::string ::string ::obj)
	   (mpg123-decode ::mpg123-handle ::bstring ::long ::long ::bstring ::long)
	   (mpg123-decode-status->symbol::symbol ::int)
	   (mpg123-position::long ::mpg123-handle)
	   (mpg123-info::long ::mpg123-handle)
	   (mpg123-seek::long ::mpg123-handle ::long)
	   (mpg123-volume-get::obj ::mpg123-handle)
	   (mpg123-volume-set! ::mpg123-handle ::obj)))

;*---------------------------------------------------------------------*/
;*    %$mpg123-handle-nil ...                                          */
;*---------------------------------------------------------------------*/
(define (%$mpg123-handle-nil)
   $mpg123-handle-nil)

;*---------------------------------------------------------------------*/
;*    mpg123-handle-init ::mpg123-handle ...                           */
;*---------------------------------------------------------------------*/
(define-generic (mpg123-handle-init o::mpg123-handle)
   (with-access::mpg123-handle o ($builtin decoder)
      (let ((d::string (if (string? decoder) decoder $string-null)))
	 (set! $builtin ($bgl-mpg123-new d)))
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
;*    mpg123-handle-reset! ...                                         */
;*---------------------------------------------------------------------*/
(define (mpg123-handle-reset! o::mpg123-handle)
   (with-access::mpg123-handle o ($builtin)
      ($mpg123-close $builtin)
      ($mpg123-open-feed $builtin)))
      
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
(define (mpg123-decode m::mpg123-handle inbuf inoff insz outbuf outsz)
   (let ((status ($bgl-mpg123-decode m inbuf inoff insz outbuf outsz)))
      (values (mpg123-decode-status->symbol status)
	 (with-access::mpg123-handle m (size)
	    size))))

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
(define (mpg123-position m::mpg123-handle)
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
;*       ((=fx f $mpg123-enc-signed-24)                                */
;*        (if (eq? (bigloo-config 'endianess) 'little-endian) 's24-3le 's24-3be)) */
;*       ((=fx f $mpg123-enc-unsigned-24)                              */
;*        (if (eq? (bigloo-config 'endianess) 'little-endian) 'u24-3le 'u24-3be)) */
      ((=fx f $mpg123-enc-float-32) 'float)
      ((=fx f $mpg123-enc-float-64) 'float64)
      (else (raise (instantiate::&mpg123-error
		      (proc "mpg123-format->alsa-format")
		      (msg "Unknown format")
		      (obj f))))))

;*---------------------------------------------------------------------*/
;*    mpg123-param-get ...                                             */
;*---------------------------------------------------------------------*/
(define (mpg123-param-get m::mpg123-handle type)
   (with-access::mpg123-handle m ($builtin)
      ($bgl-mpg123-getparam $builtin (symbol->mpg123-param type))))

;*---------------------------------------------------------------------*/
;*    mpg123-param-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (mpg123-param-set! m::mpg123-handle type val)
   (with-access::mpg123-handle m ($builtin)
      ($bgl-mpg123-param $builtin (mpg123-param->symbol type)
	 (if (fixnum? val) val 0)
	 (if (flonum? val) val 0.))))

;*---------------------------------------------------------------------*/
;*    mpg123-param->symbol ...                                         */
;*---------------------------------------------------------------------*/
(define (mpg123-param->symbol::symbol p::$mpg123-params)
   (cond
      ((=fx p $mpg123-verbose) 'verbose)
      ((=fx p $mpg123-flags) 'flags)
      ((=fx p $mpg123-add-flags) 'add-flags)
      ((=fx p $mpg123-force-rate) 'force-rate)
      ((=fx p $mpg123-down-sample) 'down-sample)
      ((=fx p $mpg123-rva) 'rva)
      ((=fx p $mpg123-downspeed) 'downspeed)
      ((=fx p $mpg123-upspeed) 'upspeed)
      ((=fx p $mpg123-start-frame) 'start-frame)
      ((=fx p $mpg123-decode-frames) 'decode-frames)
      ((=fx p $mpg123-icy-interval) 'icy-interval)
      ((=fx p $mpg123-outscale) 'outscale)
      ((=fx p $mpg123-timeout) 'timeout)
      ((=fx p $mpg123-remove-flags) 'remove-flags)
      ((=fx p $mpg123-resync-limit) 'resync-limit)
      ((=fx p $mpg123-index-size) 'index-size)
;*       ((=fx p $mpg123-preframes) 'preframes)                        */
;*       ((=fx p $mpg123-feedpool) 'feedpool)                          */
;*       ((=fx p $mpg123-feedbuffer) 'feedbuffer)                      */
      (else (error "mpg123-param->symbol" "unknown param" p))))
     
;*---------------------------------------------------------------------*/
;*    symbol->mpg123-param ...                                         */
;*---------------------------------------------------------------------*/
(define (symbol->mpg123-param::$mpg123-params p::symbol)
   (case p
      ((verbose) $mpg123-verbose)
      ((flags) $mpg123-flags)
      ((add-flags) $mpg123-add-flags)
      ((force-rate) $mpg123-force-rate)
      ((down-sample) $mpg123-down-sample)
      ((rva) $mpg123-rva)
      ((downspeed) $mpg123-downspeed)
      ((upspeed) $mpg123-upspeed)
      ((start-frame) $mpg123-start-frame)
      ((decode-frames) $mpg123-decode-frames)
      ((icy-interval) $mpg123-icy-interval)
      ((outscale) $mpg123-outscale)
      ((timeout) $mpg123-timeout)
      ((remove-flags) $mpg123-remove-flags)
      ((resync-limit) $mpg123-resync-limit)
      ((index-size) $mpg123-index-size)
;*       ((preframes) $mpg123-preframes)                               */
;*       ((feedpool) $mpg123-feedpool)                                 */
;*       ((feedbuffer) $mpg123-feedbuffer)                             */
      (else (error "mpg123-param->symbol" "unknown param" p))))
     
;*---------------------------------------------------------------------*/
;*    Force the initialization                                         */
;*---------------------------------------------------------------------*/
(let ((err ($mpg123-init)))
   (unless (=fx err $mpg123-ok)
      (raise (instantiate::&mpg123-error
		(proc "mpg123")
		(msg ($mpg123-plain-strerror err))
		(obj #f)))))

;*---------------------------------------------------------------------*/
;*    mpg123-info ...                                                  */
;*---------------------------------------------------------------------*/
(define (mpg123-info m::mpg123-handle)
   (with-access::mpg123-handle m ($builtin)
      ($bgl-mpg123-info $builtin)))
   
;*---------------------------------------------------------------------*/
;*    mpg123-seek ...                                                  */
;*---------------------------------------------------------------------*/
(define (mpg123-seek m::mpg123-handle sec)
   (with-access::mpg123-handle m ($builtin)
      (let ((f ($mpg123-timeframe $builtin (fixnum->flonum sec))))
	 (tprint "MPG123-SEEK, f=" f)
	 (let ((o ($mpg123-seek-frame $builtin f $mpg123-seek-set)))
	    (tprint "MPG123-SEEK, o=" o)
	    (if (<fx o 0)
		0
;* 		'(raise (instantiate::&mpg123-error                    */
;* 			  (proc "mpg123")                              */
;* 			  (msg ($mpg123-strerror $builtin))            */
;* 			  (obj m)))                                    */
		o)))))
   
;*---------------------------------------------------------------------*/
;*    mpg123-volume-get ...                                            */
;*---------------------------------------------------------------------*/
(define (mpg123-volume-get m::mpg123-handle)
   (with-access::mpg123-handle m ($builtin)
      (let ((vol ($bgl-mpg123-getvolume $builtin)))
	 (if (<fl vol 0.)
	     (raise (instantiate::&mpg123-error
		   (proc "mpg123")
		   (msg "Cannot get volume")
		   (obj m)))
	     (flonum->fixnum (roundfl (*fl 100. (sqrtfl vol))))))))

;*---------------------------------------------------------------------*/
;*    mpg123-volume-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (mpg123-volume-set! m::mpg123-handle vol)
   (with-access::mpg123-handle m ($builtin)
      (let* ((v (/fl (fixnum->flonum vol) 100.))
	     (v2 (*fl v v)))
	 (let ((err ($mpg123-volume $builtin v2)))
	    (when (<fx err 0)
	       (raise (instantiate::&mpg123-error
			 (proc "mpg123")
			 (msg ($mpg123-plain-strerror err))
			 (obj m))))))))

