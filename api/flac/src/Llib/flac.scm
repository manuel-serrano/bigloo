;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Llib/flac.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Mon Jul 11 14:18:03 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Bigloo binding for the flac library                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_flac

   (include "flac.sch")
   
   (extern (export flac-error "bgl_flac_error")
	   (export flac-decoder-read "bgl_flac_decoder_read")
	   (export flac-decoder-write "bgl_flac_decoder_write")
	   (export flac-decoder-metadata "bgl_flac_decoder_metadata")
	   (export flac-decoder-tell "bgl_flac_decoder_tell"))
   
   (export (class flac-decoder
	      (flac-decoder-init)
	      ($builtin::$flac-decoder read-only (default (%$flac-decoder-new)))
	      (%inbuf::string (default ""))
	      (outbuf::bstring (default ""))
	      (%eof::bool (default #f))
	      (port (default #f))
	      (md5check::bool read-only (default #f))
	      (seek (default #f))
	      (length (default #f))
	      (error (default #f)))

	   (class &flac-error::&error)

	   (%$flac-decoder-new::$flac-decoder)

	   (generic flac-decoder-init ::flac-decoder)
	   (generic flac-decoder-close ::flac-decoder)
	   (generic flac-decoder-read ::flac-decoder ::long)
	   (generic flac-decoder-write ::flac-decoder ::long ::long ::long ::long)
	   (generic flac-decoder-metadata ::flac-decoder ::llong ::long ::long ::long)
	   (generic flac-decoder-tell ::flac-decoder)
	   (generic flac-decoder-info::long ::flac-decoder)
	   
	   (generic flac-decoder-decode ::flac-decoder)
	   (generic flac-decoder-reset! ::flac-decoder)
	   (flac-error::int ::string ::string ::obj)
	   ))

;*  	   (flac-handle-reset! ::flac-handle)                          */
;* 	   (flac-get-format ::flac-handle)                             */
;* 	   (flac-decode ::flac-handle ::bstring ::long ::bstring ::long) */
;* 	   (flac-position::long ::flac-handle ::bstring)               */
;* 	   (flac-seek::long ::flac-handle ::long)                      */
;* 	   (flac-volume-get::obj ::flac-handle)                        */
;* 	   (flac-volume-set! ::flac-handle ::obj)))                    */

;*---------------------------------------------------------------------*/
;*    object-print ::flac-decoder ...                                  */
;*---------------------------------------------------------------------*/
(define-method (object-print o::flac-decoder port print-slot)
   (display "#|flac-decoder|" port))
   
;*---------------------------------------------------------------------*/
;*    %$flac-decoder-new ...                                           */
;*---------------------------------------------------------------------*/
(define (%$flac-decoder-new)
   ($flac-decoder-new))

;*---------------------------------------------------------------------*/
;*    flac-decoder-init ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-init o::flac-decoder)
   (with-access::flac-decoder o ($builtin md5check outbuf)
      ($flac-decoder-set-md5-checking
	 $builtin (if md5check $flac-true $flac-false))
      (set! outbuf
	 (make-string (* $flac-max-block-size $flac-max-channels 4)))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-close ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-close o::flac-decoder)
   (with-access::flac-decoder o ($builtin)
      ($flac-decoder-delete $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    flac-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (flac-error proc msg obj)
   (raise (instantiate::&flac-error
	     (proc proc)
	     (msg msg)
	     (obj obj)))
   0)

;*---------------------------------------------------------------------*/
;*    flac-decoder-decode ::flac-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-decode o::flac-decoder)
   (with-access::flac-decoder o ($builtin %eof)
      (unwind-protect
	 (begin
	    (set! %eof #f)
	    ($bgl-flac-decoder-init-stream $builtin o)
	    ($flac-decoder-process-until-end-of-stream $builtin))
	 ($flac-decoder-reset $builtin))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-reset! ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-reset! o::flac-decoder)
   (unless (eq? (flac-decoder-get-state o) 'stream-decoder-uninitialized) 
      (with-access::flac-decoder o ($builtin)
	 ($flac-decoder-reset $builtin)))
   (tprint "reset, state=" (flac-decoder-get-state o)))

;*---------------------------------------------------------------------*/
;*    flac-decoder-get-state ...                                       */
;*---------------------------------------------------------------------*/
(define (flac-decoder-get-state o::flac-decoder)
   (with-access::flac-decoder o ($builtin)
      (flac-decoder-state->symbol ($flac-decoder-get-state $builtin))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-read o::flac-decoder size::long)
   (with-access::flac-decoder o (port %inbuf)
      ;; Don't use any regular input functions because they
      ;; take ::bstring argument while inbuf is a ::string
      ($rgc-blit-string! port %inbuf 0 size)))

;*---------------------------------------------------------------------*/
;*    flac-decoder-write ::flac-decoder ...                            */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-write o::flac-decoder
		   blocksize::long
		   srate::long
		   channels::long
		   bps::long)
   (tprint "write blocksize=" blocksize " sample-rate=" srate " channels="
      channels " bps=" bps)
   #t)

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flac-decoder ...                         */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-metadata o::flac-decoder
		   total::llong
		   srate::long
		   channels::long
		   bps::long)
   (tprint "meta total" total " sample-rate=" srate " channels="
      channels " bps=" bps)
   #t)

;*---------------------------------------------------------------------*/
;*    flac-decoder-tell ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-tell o::flac-decoder)
   (with-access::flac-decoder o (port)
      (if (input-port? port)
	  (input-port-position port)
	  -1)))

;*---------------------------------------------------------------------*/
;*    flac-decoder-info ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-info o::flac-decoder)
   (with-access::flac-decoder o ($builtin)
      (values ($flac-decoder-get-bits-per-sample $builtin)
	 ($flac-decoder-get-sample-rate $builtin))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-state->symbol ...                                   */
;*---------------------------------------------------------------------*/
(define (flac-decoder-state->symbol s::$flac-decoder-state)
   (cond
      ((=fx s $flac-decoder-search-for-metata)
       'decoder-search-for-metata)
      ((=fx s $flac-stream-decoder-read-metadata)
       'stream-decoder-read-metadata)
      ((=fx s $flac-stream-decoder-search-for-frame-sync)
       'stream-decoder-search-for-frame-sync)
      ((=fx s $flac-stream-decoder-read-frame)
       'stream-decoder-read-frame)
      ((=fx s $flac-stream-decoder-end-of-stream)
       'stream-decoder-end-of-stream)
      ((=fx s $flac-STREAM-DECODER-OGG-ERROR)
       'STREAM-DECODER-OGG-ERROR)
      ((=fx s $flac-stream-decoder-seek-error)
       'stream-decoder-seek-error)
      ((=fx s $flac-stream-decoder-aborted)
       'stream-decoder-aborted)
      ((=fx s $flac-stream-decoder-memory-allocation-error)
       'stream-decoder-memory-allocation-error)
      ((=fx s $flac-stream-decoder-uninitialized)
       'stream-decoder-uninitialized)
      (else
       'unknown)))

;* {*---------------------------------------------------------------------*} */
;* {*    flac-handle-reset! ...                                           *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-handle-reset! o::flac-handle)                         */
;*    (with-access::flac-handle o ($builtin)                           */
;*       ($flac-close $builtin)                                        */
;*       ($flac-open-feed $builtin)))                                  */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-decode ...                                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-decode m::flac-handle inbuf insz outbuf outsz)        */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (multiple-value-bind (status size)                            */
;* 	 ($bgl-flac-decode $builtin inbuf insz outbuf outsz)           */
;* 	 (values (flac-decode-status->symbol status) size))))          */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-get-format ...                                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-get-format m::flac-handle)                            */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (multiple-value-bind (rate channels encoding)                 */
;* 	 ($bgl-flac-get-format $builtin)                               */
;* 	 (values rate channels (flac-format->symbol encoding)))))      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-position ...                                                *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-position m::flac-handle buf::bstring)                 */
;*    (with-access::flac-handle m ($builtin)                           */
;*       ($bgl-flac-position $builtin)))                               */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    Force the initialization                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (let ((err ($flac-init)))                                           */
;*    (unless (=fx err $flac-ok)                                       */
;*       (raise (instantiate::&flac-error                              */
;* 		(proc "flac")                                          */
;* 		(msg ($flac-plain-strerror err))                       */
;* 		(obj #f)))))                                           */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-info ...                                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-info m::flac-handle)                                  */
;*    (with-access::flac-handle m ($builtin)                           */
;*       ($bgl-flac-info $builtin)))                                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-seek ...                                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-seek m::flac-handle ms)                               */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (let ((f ($flac-timeframe $builtin (/fl (fixnum->flonum ms) 1000.)))) */
;* 	 ($flac-seek-frame $builtin f $flac-seek-set))))               */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-volume-get ...                                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-volume-get m::flac-handle)                            */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (let ((vol ($bgl-flac-getvolume $builtin)))                   */
;* 	 (if (<fx vol 0)                                               */
;* 	     (raise (instantiate::&flac-error                          */
;* 		   (proc "flac")                                       */
;* 		   (msg ($flac-plain-strerror vol))                    */
;* 		   (obj m)))                                           */
;* 	     vol))))                                                   */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    flac-volume-set! ...                                             *} */
;* {*---------------------------------------------------------------------*} */
;* (define (flac-volume-set! m::flac-handle vol)                       */
;*    (with-access::flac-handle m ($builtin)                           */
;*       (let ((err ($flac-volume $builtin (/ (fixnum->flonum vol) 100.)))) */
;* 	 (when (<fx err 0)                                             */
;* 	    (raise (instantiate::&flac-error                           */
;* 		      (proc "flac")                                    */
;* 		      (msg ($flac-plain-strerror err))                 */
;* 		      (obj m)))))))                                    */
;*                                                                     */
