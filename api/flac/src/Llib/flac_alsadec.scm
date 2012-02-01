;*=====================================================================*/
;*    .../prgm/project/bigloo/api/flac/src/Llib/flac_alsadec.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Wed Feb  1 11:03:05 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    FLAC Alsa decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_alsadec

   (library alsa)

   (import __flac_flac)

   (extern (macro $flac-blit-string!::void
	      (::string ::long ::string ::long ::long) "BGL_FLAC_BLIT_STRING"))
   
   (static (class flac-alsa::flac-decoder
	      (%alsamusic (default #f))
	      (%buffer (default #f))
	      (%decoder (default #f))))

   (export (class flac-alsadecoder::alsadecoder
	      (%flac::obj (default #unspecified)))))

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (bigloo-compiler-debug))

;*---------------------------------------------------------------------*/
;*    object-print ::flac-alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-method (object-print o::flac-alsadecoder port print-slot)
   (display "#|flac-alsadecoder|" port))
   
;*---------------------------------------------------------------------*/
;*    debug                                                            */
;*---------------------------------------------------------------------*/
(define debug ($compiler-debug))

;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-init dec::flac-alsadecoder)
   (with-access::flac-alsadecoder dec (mimetypes %flac)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/flac" "application/x-flac")))
      (set! %flac (instantiate::flac-alsa))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-reset! ::flac-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-reset! o::flac-alsadecoder)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-reset! %flac)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::flac-alsadecoder ...                      */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-position o::flac-alsadecoder abuf)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-position %flac)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-info ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-info o::flac-alsadecoder)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-info %flac)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-volume-set! ::flac-alsadecoder ...                   */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-volume-set! o::flac-alsadecoder vol)
   (with-access::flac-alsadecoder o (%flac)
      (flac-volume-set! %flac vol)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-seek ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-seek o::flac-alsadecoder ms)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-seek %flac ms)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::flac-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode dec::flac-alsadecoder
		  am::alsamusic
		  buffer::alsabuffer)
   (with-access::flac-alsadecoder dec (%flac)
      (with-access::flac-alsa %flac (%buffer %alsamusic %decoder)
	 (set! %buffer buffer)
	 (set! %alsamusic am)
	 (set! %decoder dec)
	 (with-access::alsamusic am (%amutex %status pcm)
	    (unwind-protect
	       (flac-decoder-decode %flac)
	       (with-access::alsabuffer %buffer (%eof)
		  (alsa-snd-pcm-cleanup pcm)
		  (onstate am (if %eof 'ended 'stop))))))))

;*---------------------------------------------------------------------*/
;*    onstate ...                                                      */
;*---------------------------------------------------------------------*/
(define (onstate am st)
   (with-access::alsamusic am (onstate %status)
      (with-access::musicstatus %status (state)
	 (set! state st)
	 (onstate am %status))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flac-alsa ...                            */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-metadata o::flac-alsa total rate channels bps)
   (with-access::flac-alsa o (%alsamusic)
      (with-access::alsamusic %alsamusic (pcm)
	 (let ((encoding (case bps
			    ((8) 's16)
			    ((16) 's16)
			    ((24) 's24-3le)
			    ((32) 's32))))
	    (alsa-snd-pcm-hw-set-params! pcm
	       :rate-resample 1
	       :access 'rw-interleaved
	       :format encoding
	       :channels channels
	       :rate-near rate
	       :buffer-time-near 500000
	       :buffer-size-near (/fx rate 2)
	       :period-size-near (/fx rate 8))
	    (alsa-snd-pcm-sw-set-params! pcm
	       :start-threshold 1
	       :avail-min 1)))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-write ::flac-alsa ...                               */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-write o::flac-alsa size rate channels bps)
   (with-access::flac-alsa o (outbuf %alsamusic %buffer)
      (with-access::alsamusic %alsamusic (pcm %status)
	 (when (>fx size 0)
	    (alsa-snd-pcm-write pcm outbuf size)
	    #t))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-read o::flac-alsa size::long)
   
   (with-access::flac-alsa o (port %flacbuf %buffer (am %alsamusic) %decoder)
      (with-access::alsadecoder %decoder (%!dabort %!dpause %dcondv %dmutex)
	 (with-access::alsabuffer %buffer (%bmutex %bcondv %!bstate
					     %inbuf %inbufp %inlen
					     %!tail %head %eof)
	    
	    (define inlen %inlen)
	    
	    (define flacbuf (custom-identifier %flacbuf))
	    
	    (define (available)
	       (cond
		  ((>fx %head %!tail) (-fx %head %!tail))
		  ((<fx %head %!tail) (+fx (-fx inlen (-fx %!tail 1)) %head))
		  (else 0)))
	    
	    (define (full-state?)
	       (and (=fx %!bstate 2) (>fx inlen (*fx (available) 2))))
	    
	    (define (inc-tail! size)
	       (set! %!tail (+fx %!tail size))
	       (when (=fx %!tail inlen)
		  (set! %!tail 0))
	       (cond
		  ((=fx %!tail %head)
		   ;; set state empty
		   (mutex-lock! %bmutex)
		   (when (>fx debug 0)
		      (tprint "flac_decoder, read.2a, set empty (bs=0) size=" size
			 " %eof=" %eof))
		   (set! %!bstate 0)
		   (condition-variable-broadcast! %bcondv)
		   (mutex-unlock! %bmutex))
		  ((full-state?)
		   ;; set state filled
		   (mutex-lock! %bmutex)
		   (when (>fx debug 1)
		      (tprint "flac_decoder, read.2b, set filled (bs=1) size=" size))
		   (set! %!bstate 1)
		   (condition-variable-broadcast! %bcondv)
		   (mutex-unlock! %bmutex))))
	    
	    (let loop ((size size)
		       (i 0))
	       (when (>fx debug 1)
		  (tprint "flac_decoder, read.1 bs=" %!bstate " tl=" %!tail " hd=" %head
		     " %eof=" %eof " inlen=" inlen))
	       (cond
		  (%!dpause
		   ;;; the decoder is asked to pause
		   (with-access::alsamusic am (%status)
		      (with-access::musicstatus %status (songpos)
			 (set! songpos
			    (alsadecoder-position %decoder %buffer))))
		   (mutex-lock! %dmutex)
		   (if %!dpause
		       (let liip ()
			  (onstate am 'pause)
			  (condition-variable-wait! %dcondv %dmutex)
			  (if %!dpause
			      (liip)
			      (begin
				 (mutex-unlock! %dmutex)
				 (onstate am 'play)
				 (loop size i))))))
		  (%!dabort
		     -1)
		  ((=fx %!bstate 0)
		   ;; buffer empty
		   (if %eof
		       beof
		       (begin
			  ;; buffer empty, wait to be filled
			  (mutex-lock! %bmutex)
			  (when (=fx %!bstate 0)
			     (when (>=fx debug 0)
				(tprint ">>> flac_decoder, wait empty"
				   " %!tail=" %!tail " head=" %head))
			     (condition-variable-wait! %bcondv %bmutex)
			     (when (>=fx debug 0)
				(tprint "<<< flac_decoder, wait empty"
				   " %!tail=" %!tail " head=" %head))
			     (mutex-unlock! %bmutex))
			  (loop size i))))
		  (else
		   (let* ((bsz (if (>fx %head %!tail)
				   (-fx %head %!tail)
				   (-fx inlen %!tail)))
			  (sz (minfx size bsz)))
		      (when (>fx sz 0)
			 ($flac-blit-string! %inbufp %!tail flacbuf i sz))
		      (when (>fx debug 1)
			 (tprint ">>> read.inc-tail sz=" sz " %!tail=" %!tail))
		      (inc-tail! sz)
		      (when (>fx debug 1)
			 (tprint "<<< read.inc-tail sz=" sz " %!tail=" %!tail))
		      (if (<fx sz size)
			  (loop (- size sz) (+fx i sz))
			  (+fx i sz))))))))))
