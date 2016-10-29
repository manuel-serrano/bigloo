;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/src/Llib/flac.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:30:32 2011                          */
;*    Last change :  Sat Oct 29 21:17:16 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo binding for the flac library                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_flac

   (option (set! *dlopen-init-gc* #t))

   (include "flac.sch")

   (library multimedia)
   
   (extern (export flac-error "bgl_flac_error")
	   (export flac-decoder-read "bgl_flac_decoder_read")
	   (export flac-decoder-write "bgl_flac_decoder_write")
	   (export flac-decoder-metadata "bgl_flac_decoder_metadata")
	   (export flac-decoder-tell "bgl_flac_decoder_tell")
	   (export flac-decoder-seek "bgl_flac_decoder_seek")
	   (export flac-decoder-length "bgl_flac_decoder_length"))
   
   (export (abstract-class flac-decoder
	      (flac-decoder-init)
	      ($builtin::$flac-decoder read-only (default (%$flac-decoder-new)))
	      (%flacbuf::custom (default (%$flac-make-custom)))
	      (outbuf::bstring (default ""))
	      (%eof::bool (default #f))
	      (%sample::long (default 0))
	      (%volume::double (default 1.))
	      (md5check::bool read-only (default #f))
	      (%bchecksum::int (default #x80))
	      (%rchecksum::int (default #x80)))

	   (class &flac-error::&error)

	   (%$flac-decoder-new::$flac-decoder)
	   (%$flac-make-custom::custom)

	   (generic flac-decoder-init ::flac-decoder)
	   (generic flac-decoder-close ::flac-decoder)
	   (generic flac-decoder-read ::flac-decoder ::long)
	   (generic flac-decoder-write ::flac-decoder ::long ::long ::long ::long)
	   (generic flac-decoder-metadata ::flac-decoder ::llong ::long ::long ::long)
	   (generic flac-decoder-tell ::flac-decoder)
	   (generic flac-decoder-seek ::flac-decoder ::llong)
	   (generic flac-decoder-length ::flac-decoder)
	   (generic flac-decoder-info::long ::flac-decoder)
	   (generic flac-decoder-position::long ::flac-decoder)
	   
	   (generic flac-decoder-decode ::flac-decoder)
	   (generic flac-decoder-decode16 ::flac-decoder)
	   (generic flac-decoder-reset! ::flac-decoder)
	   
	   (generic flac-volume-get::obj ::flac-decoder)
	   (generic flac-volume-set! ::flac-decoder ::obj)

	   (flac-error::int ::string ::string ::obj)))

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
;*    %$flac-make-custom ...                                           */
;*---------------------------------------------------------------------*/
(define (%$flac-make-custom)
   ($flac-make-custom 0))

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
;*    flac-decoder-decode ::flac-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-decode o::flac-decoder)
   (with-access::flac-decoder o ($builtin %eof)
      (unwind-protect
	 (begin
	    ($bgl-flac-decoder-init-stream $builtin o)
	    ($flac-decoder-process-until-end-of-stream $builtin))
	 (flac-decoder-reset! o))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-decode16 ::flac-decoder ...                         */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-decode16 o::flac-decoder)
   (with-access::flac-decoder o ($builtin %eof)
      (unwind-protect
	 (begin
	    ($bgl-flac-decoder-init-stream16 $builtin o)
	    ($flac-decoder-process-until-end-of-stream $builtin))
	 (flac-decoder-reset! o))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-reset! ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-reset! o::flac-decoder)
   (unless (eq? (flac-decoder-get-state o) 'stream-decoder-uninitialized) 
      (with-access::flac-decoder o ($builtin %eof %sample)
	 (set! %eof #f)
	 (set! %sample 0)
	 ($flac-decoder-reset $builtin))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-get-state ...                                       */
;*---------------------------------------------------------------------*/
(define (flac-decoder-get-state o::flac-decoder)
   (with-access::flac-decoder o ($builtin)
      (flac-decoder-state->symbol ($flac-decoder-get-state $builtin))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-read o::flac-decoder size::long))

;*---------------------------------------------------------------------*/
;*    flac-decoder-write ::flac-decoder ...                            */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-write o::flac-decoder
		   blocksize::long
		   srate::long
		   channels::long
		   bps::long))

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flac-decoder ...                         */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-metadata o::flac-decoder
		   total::llong
		   srate::long
		   channels::long
		   bps::long))

;*---------------------------------------------------------------------*/
;*    flac-decoder-tell ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-tell o::flac-decoder)
   #f)

;*---------------------------------------------------------------------*/
;*    flac-decoder-seek ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-seek o::flac-decoder offset)
   #t)

;*---------------------------------------------------------------------*/
;*    flac-decoder-length ::flac-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-length o::flac-decoder)
   -1)

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

;*---------------------------------------------------------------------*/
;*    flac-error-status ...                                            */
;*---------------------------------------------------------------------*/
(define (flac-error-status s)
   (cond
      ((=fx s $flac-stream-decoder-error-status-lost-sync)
       'lost-sync)
      ((=fx s $flac-stream-decoder-error-status-bad-header)
       'bad-header)
      ((=fx s $flac-stream-decoder-error-status-frame-crc-mismatch)
       'crc-mismatch)
      ((=fx s $flac-stream-decoder-error-status-unparseable-stream)
       'unparseable-stream)
      (else
       'unknown-error)))

;*---------------------------------------------------------------------*/
;*    flac-decoder-position ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (flac-decoder-position m::flac-decoder)
   (with-access::flac-decoder m ($builtin %sample)
      (if (<fx %sample 0)
	  0
	  (let ((srate ($flac-decoder-get-sample-rate $builtin)))
	     (if (=fx srate 0)
		 0
		 (/fx %sample srate))))))

;*---------------------------------------------------------------------*/
;*    flac-volume-get ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (flac-volume-get m::flac-decoder)
   (with-access::flac-decoder m (%volume)
      (flonum->fixnum (*fl (sqrtfl %volume) 100.))))

;*---------------------------------------------------------------------*/
;*    flac-volume-set! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (flac-volume-set! m::flac-decoder vol)
   (with-access::flac-decoder m (%volume)
      (when (and (>=fx vol 0) (<=fx vol 100))
	 (let ((v (/fl (fixnum->flonum vol) 100.)))
	    (set! %volume (*fl v v))))))

;*---------------------------------------------------------------------*/
;*    flac-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (flac-error proc msg obj)
   (raise (instantiate::&flac-error
	     (proc proc)
	     (msg msg)
	     (obj obj)))
   0)
