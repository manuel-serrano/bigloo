;*=====================================================================*/
;*    .../prgm/project/bigloo/api/flac/src/Llib/flac_alsadec.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Thu Jul 19 19:00:23 2012 (serrano)                */
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
(define-method (alsadecoder-seek o::flac-alsadecoder sec)
   (with-access::flac-alsadecoder o (%flac)
      (with-access::flac-alsa %flac ($builtin %eof)
	 (let ((bps ($flac-decoder-get-bits-per-sample $builtin))
	       (srate ($flac-decoder-get-sample-rate $builtin)))
	    ($flac-decoder-seek-absolute $builtin
	       (*llong (fixnum->llong srate) (fixnum->llong sec)))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-tell ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-tell o::flac-alsa)
   (with-access::flac-alsa o (%decoder)
      (with-access::flac-alsadecoder %decoder (%flac)
	 (with-access::flac-alsa %flac (%buffer)
	    (when (isa? %buffer alsabuffer)
	       (alsabuffer-tell %buffer))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-seek ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-seek o::flac-alsa off)
   (with-access::flac-alsa o (%decoder)
      (with-access::flac-alsadecoder %decoder (%flac)
	 (with-access::flac-alsa %flac (%buffer)
	    (when (isa? %buffer alsabuffer)
	       (alsabuffer-seek %buffer off)
	       #t)))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-length ::flac-alsa ...                              */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-length o::flac-alsa)
   (with-access::flac-alsa o (%decoder)
      (with-access::flac-alsadecoder %decoder (%flac)
	 (with-access::flac-alsa %flac (%buffer)
	    (alsabuffer-length %buffer)))))
   
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
   
   (with-access::flac-alsa o (%flacbuf %buffer (am %alsamusic) %decoder
				%has-been-empty-once)
      (with-access::alsadecoder %decoder (%!dabort %!dpause %dcondv %dmutex)
	 (with-access::alsabuffer %buffer (%bmutex %bcondv 
					     %inbuf %inbufp %inlen
					     %tail %head %eof
					     %empty %filled
					     url)
	    
	    (define inlen %inlen)
	    
	    (define flacbuf (custom-identifier %flacbuf))
	    
	    (define (buffer-percentage-filled)
	       (llong->fixnum
		  (/llong (*llong #l100
			     (fixnum->llong (alsabuffer-available %buffer)))
		     (fixnum->llong inlen))))
	    
	    (define (buffer-filled?)
	       ;; filled when > 25%
	       (>fx (*fx 4 (alsabuffer-available %buffer)) inlen))
	    
	    (define (buffer-flushed?)
	       ;; flushed when slow fill or buffer fill < 80%
	       (or %has-been-empty-once
		   (>fx (*fx 5 (-fx inlen (alsabuffer-available %buffer)))
		      inlen)))
	    
	    (define (buffer-full?)
	       (and (=fx %head %tail) (not %empty)))
	    
	    (define (debug-inc-tail)
	       (when (>=fx (flac-debug) 2)
		  (let ((p (buffer-percentage-filled)))
		     (when (or (and (or (< p 75) %has-been-empty-once)
				    (not %eof))
			       (>=fx (flac-debug) 3))
			(tprint "--- FLAC_DECODER, buffer: "
			   (cond
			      ((>= p 80) "")
			      ((> p 25) "[0m[1;33m")
			      (else "[0m[1;32m"))
			   p
			   "%[0m"
			   (if %eof " EOF" "")
			   " url=" url)))))
	    
	    (define (inc-tail! size)
	       ;; increment the tail
	       (let ((ntail (+fx %tail size)))
		  (when (=fx ntail inlen)
		     (set! ntail 0))
		  ;; check buffer emptyness
		  (when (=fx ntail %head)
		     (set! %has-been-empty-once #t)
		     (set! %empty #t)
		     (set! %filled #f))
		  ;; increment the shared %tail
		  (set! %tail ntail))
	       ;; debug
	       (debug-inc-tail)
	       ;; notify the buffer no longer full
	       (when (buffer-flushed?)
		  (mutex-lock! %bmutex)
		  (condition-variable-broadcast! %bcondv)
		  (mutex-unlock! %bmutex)))
	    
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
			     (tprint "!!! FLAC_DECODER, buffer empty "
				(if %eof " EOF" "")
				" url=" url))
			  (mutex-lock! %bmutex)
			  (let liip ()
			     ;; wait until the buffer is filled
			     (unless (or %eof %!dabort (buffer-filled?))
				(condition-variable-wait! %bcondv %bmutex)
				(with-access::alsamusic am (%status)
				   (with-access::musicstatus %status (buffering)
				      (set! buffering
					 (buffer-percentage-filled))))
				(onstate am 'buffering)
				(liip)))
			  (set! %filled #t)
			  (mutex-unlock! %bmutex)
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
