;*=====================================================================*/
;*    .../prgm/project/bigloo/api/flac/src/Llib/flac_alsadec.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Sat Apr  7 07:26:40 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    FLAC Alsa decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_alsadec

   (cond-expand
      ((library alsa)
       (library alsa)))

   (import __flac_flac)

   (extern (macro $flac-blit-string!::void
	      (::string ::long ::string ::long ::long) "BGL_FLAC_BLIT_STRING"))
   
   (static (class flac-alsa::flac-decoder
	      (%alsamusic (default #f))
	      (%buffer (default #f))
	      (%decoder (default #f))
	      (%has-been-empty-once (default #f))))

   (cond-expand
      ((library alsa)
       (export (class flac-alsadecoder::alsadecoder
	          (%flac::obj (default #unspecified)))))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library alsa)
;;; compile only if alsa available

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (begin (bigloo-compiler-debug) 1))

;*---------------------------------------------------------------------*/
;*    flac-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (flac-debug)
   (when (>fx ($compiler-debug) 0)
      (bigloo-debug)))

;*---------------------------------------------------------------------*/
;*    object-print ::flac-alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-method (object-print o::flac-alsadecoder port print-slot)
   (display "#|flac-alsadecoder|" port))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-init dec::flac-alsadecoder)
   (with-access::flac-alsadecoder dec (mimetypes %flac)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/flac" "application/x-flac")))
      (set! %flac (instantiate::flac-alsa))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-reset! ::flac-alsa ...                              */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-reset! o::flac-alsa)
   (with-access::flac-alsa o (%has-been-empty-once)
      (set! %has-been-empty-once #f)
      (call-next-method)))

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
	 (with-access::alsabuffer buffer (%tail %head %empty %full)
	    (set! %empty (and (not %full) (=fx %tail %head))))
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
   
   (with-access::flac-alsa o (port %flacbuf %buffer (am %alsamusic) %decoder
				%has-been-empty-once)
      (with-access::alsadecoder %decoder (%!dabort %!dpause %dcondv %dmutex)
	 (with-access::alsabuffer %buffer (%bmutex %bcondv 
					     %inbuf %inbufp %inlen
					     %tail %head %eof %full %empty
					     url)
	    
	    (define inlen %inlen)
	    
	    (define flacbuf (custom-identifier %flacbuf))
	    
	    (define (buffer-available)
	       (cond
		  ((>fx %head %tail) (-fx %head %tail))
		  ((<fx %head %tail) (+fx (-fx inlen (-fx %tail 1)) %head))
		  (else 0)))
	    
	    (define (buffer-filled?)
	       ;; filled when > 25%
	       (>fx (*fx 4 (buffer-available)) inlen))
	    
	    (define (buffer-flushed?)
	       ;; flushed when slow fill or buffer fill < 75%
	       (or %has-been-empty-once
		   (>fx (*fx 4 (-fx inlen (buffer-available))) inlen)))
	    
	    (define (debug-inc-tail)
	       (when (>=fx (flac-debug) 3)
		  (tprint "--- flac_decoder, count=" (buffer-available)
		     " (" (/llong (*llong #l100
				     (fixnum->llong (buffer-available)))
			     (fixnum->llong inlen)) "%) eof="
		     %eof " url=" url)))

	    (define (inc-tail! size)
	       ;; increment the tail
	       (let ((ntail (+fx %tail size)))
		  (if (=fx ntail inlen)
		      (set! %tail 0)
		      (set! %tail ntail)))
	       ;; check buffer emptyness
	       (when (=fx %tail %head)
		  (set! %has-been-empty-once #t)
		  (set! %empty #t))
	       ;; notify the buffer no longer full
	       (when (and %full (buffer-flushed?))
		  (mutex-lock! %bmutex)
		  (condition-variable-broadcast! %bcondv)
		  (mutex-unlock! %bmutex))
	       ;; debug
	       (debug-inc-tail))

	    (let loop ((size size)
		       (i 0))
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
			  (mutex-unlock! %dmutex)
			  (onstate am 'pause)
			  (mutex-lock! %dmutex)
			  (condition-variable-wait! %dcondv %dmutex)
			  (if %!dpause
			      (liip)
			      (begin
				 (mutex-unlock! %dmutex)
				 (onstate am 'play)
				 (loop size i))))))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   -1)
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       beof
		       (let ((d0 (current-microseconds)))
			  (when (>=fx (flac-debug) 2)
			     (tprint "!!! flac_decoder, buffer empty url=" url))
			  (onstate am 'buffering)
			  (mutex-lock! %bmutex)
			  (let liip ()
			     ;; wait until the full is 25% filled
			     (unless (or %eof (buffer-filled?))
				(condition-variable-wait! %bcondv %bmutex)
				(liip)))
			  (mutex-unlock! %bmutex)
			  (set! %empty #f)
			  (onstate am 'play)
			  (loop size i))))
		  (else
		   (let ((s (minfx size
			       (if (>fx %head %tail)
				   (-fx %head %tail)
				   (-fx inlen %tail)))))
		      (when (>fx s 0)
			 ($flac-blit-string! %inbufp %tail flacbuf i s)
			 (inc-tail! s))
		      (if (<fx s size)
			  (loop (- size s) (+fx i s))
			  (+fx i s))))))))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
