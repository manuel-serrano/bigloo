;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/flac/src/Llib/flac_alsadec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Sat Apr 18 15:36:45 2020 (serrano)                */
;*    Copyright   :  2011-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    FLAC Alsa decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_alsadec

   (library multimedia)
   
   (cond-expand
      ((library alsa)
       (library alsa)))
   
   (import __flac_flac
	   __flac_decoder)

   (static (class flacalsadec::flacdec))
   
   (cond-expand
      ((library alsa)
       (export (class flac-alsadecoder::flacmusicdecoder)))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library alsa)
    
;;; compile only if alsa available
;*---------------------------------------------------------------------*/
;*    musicdecoder-init ::flac-alsadecoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-init dec::flac-alsadecoder)
   (call-next-method)
   (with-access::flac-alsadecoder dec (%flac)
      (unless %flac (set! %flac (instantiate::flacalsadec)))))
    
;*---------------------------------------------------------------------*/
;*    musicdecoder-decode ::flacalsadecdecoder ...                     */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-decode dec::flac-alsadecoder
		  am::musicbuf
		  buffer::musicbuffer)
   (with-access::flac-alsadecoder dec (%flac)
      (with-access::flacdec %flac (%buffer %music %decoder)
	 (set! %buffer buffer)
	 (set! %music am)
	 (set! %decoder dec)
	 (with-access::alsamusic am (%amutex %status pcm)
	    (with-access::musicbuffer buffer (url)
	       (unwind-protect
		  (with-access::alsa-snd-pcm pcm (hwbps)
		     (if (<=fx hwbps 16)
			 (flac-decoder-decode16 %flac)
			 (flac-decoder-decode %flac)))
		  (with-access::musicbuffer %buffer (%eof)
		     (alsa-snd-pcm-cleanup pcm)
		     (music-state-set! am (if %eof 'ended 'stop)))))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flacalsadec ...                          */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-metadata o::flacalsadec total rate channels bps)
   (with-access::flacdec o (%music)
      (with-access::alsamusic %music (pcm)
	 (let ((encoding (case bps
			    ((8) 's16)
			    ((16) 's16)
			    ((24) 's24-3le)
			    ((32) 's32))))
	    (alsa-snd-pcm-reopen pcm)
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
;*    flac-decoder-read ::flacalsadec ...                              */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-read o::flacalsadec size::long)
   (with-access::flacdec o (%flacbuf %buffer (am %music) %decoder
				%rate %rate-max %rate-min %last-percentage)
      (with-access::musicdecoder %decoder (%!dabort %!dpause %dcondv %dmutex)
	 (with-access::musicbuffer %buffer (%bmutex %bcondv
					     %inbufp %inlen
					     %tail %head %eof
					     %empty
					     url)

	    (define inlen %inlen)

	    (define flacbuf::string (custom-identifier %flacbuf))

	    (define (buffer-percentage-filled)
	       (llong->fixnum
		  (/llong (*llong #l100
			     (fixnum->llong (musicbuffer-available %buffer)))
		     (fixnum->llong inlen))))

	    (define (buffer-filled?)
	       ;; filled when > 25%
	       (and (not %empty)
		    (>fx (*fx 4 (musicbuffer-available %buffer)) inlen)))

	    (define (broadcast-not-full p)
	       (synchronize %bmutex
		  (condition-variable-broadcast! %bcondv)))

	    (define (inc-tail! size)
	       ;; increment the tail
	       (let ((ntail (+fx %tail size)))
		  (when (=fx ntail inlen)
		     (set! ntail 0))
		  ;; check buffer emptyness
		  (when (=fx ntail %head)
		     (when (<fx %rate 80) (set! %rate 80))
		     (set! %empty #t))
		  ;; increment the shared %tail
		  (set! %tail ntail))
	       (unless %eof
		  ;; check if we must notify the buffer filler
		  ;; that the buffer is no longer full
		  (let* ((avail (musicbuffer-available %buffer))
			 (p (/fx (*fx avail 100) inlen)))
		     (cond
			((<fx p %rate)
			 (broadcast-not-full p)
			 (when (and (<=fx p %last-percentage) (<fx %rate %rate-max))
			    (set! %rate (+fx %rate 10))))
			((>fx p %rate-min)
			 (when (>fx %rate %rate-min)
			    (set! %rate (-fx %rate 1)))))
		     (set! %last-percentage p)
		     #unspecified)))

	    (let loop ((size size)
		       (i 0))
	       (cond
		  (%!dpause
		   (music-state-set! am 'pause)
		   ;;; the decoder is asked to pause
		   (with-access::alsamusic am (%status)
		      (with-access::musicstatus %status (songpos)
			 (set! songpos
			    (musicdecoder-position %decoder %buffer))))
		   (synchronize %dmutex
		      (let liip ()
			 (when %!dpause
			    (condition-variable-wait! %dcondv %dmutex)
			    (liip))))
		   (music-state-set! am 'play)
		   (loop size i))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   -1)
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       beof
		       (begin
			  (with-access::alsamusic am (%status)
			     (with-access::musicstatus %status (buffering)
				(set! buffering
				   (buffer-percentage-filled))))
			  (music-state-set! am 'buffering)
			  (synchronize %bmutex
			     ;; wait until the buffer is filled
			     (unless (or (not %empty)
					 %eof
					 %!dabort
					 (buffer-filled?))
				(condition-variable-wait! %bcondv %bmutex)))
			  (music-state-set! am 'play)
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
			  (loop (-fx size s) (+fx i s))
			  (+fx i s))))))))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
