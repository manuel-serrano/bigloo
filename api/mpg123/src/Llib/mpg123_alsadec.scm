;*=====================================================================*/
;*    .../project/bigloo/api/mpg123/src/Llib/mpg123_alsadec.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Sat Apr  7 07:38:01 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPG123 Alsa decoder                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_alsadec

   (cond-expand
      ((library alsa)
       (library alsa)))
   
   (import __mpg123_mpg123)

   (cond-expand
      ((library alsa)
       (export (class mpg123-alsadecoder::alsadecoder
		  (outbuf::bstring read-only (default (make-string (*fx 5 1024))))
	          (%mpg123 read-only (default (instantiate::mpg123-handle))))))))

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
;*    mpg123-debug ...                                                 */
;*---------------------------------------------------------------------*/
(define (mpg123-debug)
   (when (>fx ($compiler-debug) 0)
      (bigloo-debug)))

;*---------------------------------------------------------------------*/
;*    object-print ::mpg123-alsadecoder ...                            */
;*---------------------------------------------------------------------*/
(define-method (object-print o::mpg123-alsadecoder port print-slot)
   (display "#|mpg123-alsadecoder|" port))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::mpg123-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-init dec::mpg123-alsadecoder)
   (with-access::mpg123-alsadecoder dec (mimetypes)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/mpeg")))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-close ::mpg123-alsadecoder ...                       */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-close dec::mpg123-alsadecoder)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (mpg123-handle-close %mpg123)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-reset! ::mpg123-alsadecoder ...                      */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-reset! dec::mpg123-alsadecoder)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (mpg123-handle-reset! %mpg123)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::mpg123-alsadecoder ...                    */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-position dec::mpg123-alsadecoder b)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (/fx (mpg123-position %mpg123) 1000)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-info ::mpg123-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-info dec::mpg123-alsadecoder)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (mpg123-info %mpg123)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-seek ::mpg123-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-seek dec::mpg123-alsadecoder ms)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (mpg123-seek %mpg123 ms)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-volume-set! ::mpg123-alsadecoder ...                 */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-volume-set! dec::mpg123-alsadecoder vol)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (mpg123-volume-set! %mpg123 vol)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::mpg123-alsadecoder ...                      */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode dec::mpg123-alsadecoder
		  am::alsamusic
		  buffer::alsabuffer)
   
   (with-access::alsamusic am (pcm %status onerror)
      (with-access::mpg123-alsadecoder dec (%mpg123 %dmutex %dcondv outbuf
					      %!dabort %!dpause %!dseek)
	 (with-access::alsabuffer buffer (%bmutex %bcondv
					    %inbufp %inlen
					    %tail %head %eof %empty %full
					    url)
	    
	    (define inlen %inlen)
	    
	    (define outlen (string-length outbuf))
	    
	    (define decsz (minfx (*fx 2 outlen) inlen))

	    (define (buffer-available)
	       (cond
		  ((>fx %head %tail) (-fx %head %tail))
		  ((<fx %head %tail) (+fx (-fx inlen %tail) %head))
		  (else 0)))

	    (define has-been-empty-once #f)

	    (define (buffer-filled?)
	       ;; filled when > 25%
	       (>fx (*fx 4 (buffer-available)) inlen))
	    
	    (define (buffer-flushed?)
	       ;; flushed when slow fill or buffer fill < 75%
	       (or has-been-empty-once
		   (>fx (*fx 4 (-fx inlen (buffer-available))) inlen)))
	    
	    (define (debug-inc-tail)
	       (when (>=fx (mpg123-debug) 3)
		  (tprint "--- MPG123_DECODER, count=" (buffer-available)
		     " (" (/fx (*fx 100 (buffer-available)) inlen) "%) eof="
		     %eof " url=" url)))

	    (define (inc-tail! size)
	       ;; increment the tail
	       (let ((ntail (+fx %tail size)))
		  (if (=fx ntail inlen)
		      (set! %tail 0)
		      (set! %tail ntail)))
	       ;; check buffer emptyness
	       (when (=fx %tail %head)
		  (set! has-been-empty-once #t)
		  (set! %empty #t))
	       ;; notify the buffer no longer full
	       (when (and %full (buffer-flushed?))
		  (mutex-lock! %bmutex)
		  (condition-variable-broadcast! %bcondv)
		  (mutex-unlock! %bmutex))
	       ;; debug 
	       (debug-inc-tail))

	    (define (onstate am st)
	       (with-access::alsamusic am (onstate %status)
		  (with-access::musicstatus %status (state)
		     (set! state st)
		     (onstate am %status))))

	    (set! %empty (and (not %full) (=fx %tail %head)))
	    
	    (let loop ()
	       (cond
		  (%!dpause
		   ;;; the decoder is asked to pause
		   (with-access::musicstatus %status (songpos)
		      (set! songpos (alsadecoder-position dec buffer)))
		   (mutex-lock! %dmutex)
		   (let liip ()
		      (if %!dpause
			  (begin
			     (mutex-unlock! %dmutex)
			     (onstate am 'pause)
			     (mutex-lock! %dmutex)
			     (condition-variable-wait! %dcondv %dmutex)
			     (liip))
			  (begin
			     (mutex-unlock! %dmutex)
			     (onstate am 'play)
			     (loop)))))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   (onstate am 'stop))
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       (onstate am 'ended)
		       (begin
			  (when (>=fx (mpg123-debug) 2)
			     (tprint "!!! MPG123_DECODER, buffer empty url="
				url))
			  (let ((d0 (current-microseconds)))
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
			     (loop)))))
		  (else
		   ;; the buffer contains available bytes
		   (let flush ((s (minfx decsz
				     (if (>fx %head %tail)
					 (-fx %head %tail)
					 (-fx inlen %tail)))))
		      (let ((status ($bgl-mpg123-decode
				       %mpg123 %inbufp %tail s outbuf outlen)))
			 (when (>=fx (mpg123-debug) 4)
			    (tprint "~~~ MPG123_DECODER, s=" s
			       " tl=" %tail " hd=" %head
			       " -> status="
			       (mpg123-decode-status->symbol status)))
			 (when (>fx s 0)
			    (inc-tail! s))
			 (cond
			    ((>fx %!dseek 0)
			     (alsadecoder-seek dec %!dseek)
			     (with-access::musicstatus %status (songpos)
				(set! songpos %!dseek))
			     (set! %!dseek -1))
			    ((=fx status $mpg123-ok)
			     ;; play and keep decoding
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)
				   (flush 0))))
			    ((=fx status $mpg123-need-more)
			     ;; play and loop to get more bytes
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)))
			     (loop))
			    ((=fx status $mpg123-new-format)
			     ;; a new playback
			     (new-format dec am buffer)
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)))
			     (flush 0))
			    ((=fx status $mpg123-done)
			     ;; done playing
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)))
			     (onstate am 'ended))
			    (else
			     ;; an error occurred
			     (with-access::musicstatus %status (err)
				(set! err "mp3 decoding error"))
			     (onerror am "mp3 decoding error"))))))))))))

;*---------------------------------------------------------------------*/
;*    new-format ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-format dec am buffer)
   (with-access::alsamusic am (%toseek %status pcm)
      (with-access::mpg123-alsadecoder dec (%mpg123
					      buffer-time-near
					      buffer-size-near-ratio
					      period-size-near-ratio)
	 (multiple-value-bind (rate channels encoding)
	    (mpg123-get-format %mpg123)
	    (alsa-snd-pcm-hw-set-params! pcm
	       :access 'rw-interleaved
	       :format encoding
	       :channels channels
	       :rate-near rate
	       :buffer-size-near-ratio buffer-size-near-ratio
	       :period-size-near-ratio period-size-near-ratio)
	    (alsa-snd-pcm-sw-set-params! pcm
	       :start-threshold 1
	       :avail-min 1)))
      (with-access::musicstatus %status (songpos songlength bitrate khz)
	 (set! songpos (alsadecoder-position dec buffer))
	 (set! songlength 0)
	 (when (<fx songlength songpos)
	    (set! songlength songpos))
	 (multiple-value-bind (bitrate rate)
	    (alsadecoder-info dec)
	    (set! bitrate bitrate)
	    (set! khz rate)))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
