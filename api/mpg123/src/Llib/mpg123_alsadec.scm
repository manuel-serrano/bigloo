;*=====================================================================*/
;*    .../project/bigloo/api/mpg123/src/Llib/mpg123_alsadec.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Mon Aug 27 09:15:11 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPG123 Alsa decoder                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_alsadec

   (library multimedia)
   
   (cond-expand
      ((library alsa)
       (library alsa)))
   
   (import __mpg123_mpg123)

   (cond-expand
      ((library alsa)
       (export (class mpg123-alsadecoder::alsadecoder
		  (outbuf::bstring read-only (default (make-string (*fx 5 1024))))
	          (%mpg123 read-only (default (instantiate::mpg123-handle)))
		  (%!dseek::long (default -1)))))))

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
   (bigloo-compiler-debug))

;*---------------------------------------------------------------------*/
;*    mpg123-debug ...                                                 */
;*---------------------------------------------------------------------*/
(define (mpg123-debug)
   (if (>fx ($compiler-debug) 0)
       (bigloo-debug)
       0))

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
(define-method (alsadecoder-seek dec::mpg123-alsadecoder sec)
   (with-access::mpg123-alsadecoder dec (%mpg123 %!dseek)
      (when (>=fx (mpg123-debug) 2)
	 (debug "*** MPG123_SEEK, seek=" sec))
      (when (<fx %!dseek 0)
	 (set! %!dseek sec))))

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
					    %tail %head
					    %eof %empty
					    url)
	    
	    (define inlen %inlen)
	    
	    (define outlen (string-length outbuf))
	    
	    (define decsz (minfx (*fx 2 outlen) inlen))

	    (define has-been-empty-once #f)

	    (define (buffer-percentage-filled)
	       (llong->fixnum
		  (/llong (*llong #l100
			     (fixnum->llong (alsabuffer-available buffer)))
		     (fixnum->llong inlen))))
	    
	    (define (buffer-filled?)
	       ;; filled when > 12%
	       (>fx (*fx 8 (alsabuffer-available buffer)) inlen))
	    
	    (define (buffer-flushed?)
	       ;; flushed when slow fill or buffer fill < 75%
	       (or has-been-empty-once
		   (>fx (*fx 4 (-fx inlen (alsabuffer-available buffer)))
		      inlen)))
	    
	    (define (debug-inc-tail)
	       (when (>=fx (mpg123-debug) 2)
		  (let ((p (buffer-percentage-filled)))
		     (when (or (and (or (< p 75) has-been-empty-once)
				    (not %eof))
			       (>=fx (mpg123-debug) 3))
			(debug "--- MPG123_DECODER, buffer: "
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
		     (set! has-been-empty-once #t)
		     (set! %empty #t))
		  (set! %tail ntail))
	       ;; debug 
	       (debug-inc-tail)
	       ;; notify the buffer no longer full
	       (when (buffer-flushed?)
		  (mutex-lock! %bmutex)
		  (when (>=fx (mpg123-debug) 2)
		     (debug ">>> MPG123_DECODER, broadcast no longer full "
			"(" (current-microseconds) ")"
			" url=" url))
		  (condition-variable-broadcast! %bcondv)
		  (mutex-unlock! %bmutex)))

	    (define (onstate am st)
	       (with-access::alsamusic am (onstate %status)
		  (with-access::musicstatus %status (state)
		     (set! state st)
		     (onstate am %status))))

	    (when (>fx (mpg123-debug) 0) (debug-init!))
	    
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
		       (begin
			  (when (>=fx (mpg123-debug) 2)
			     (debug "!!! MPG123_DECODER, EOF url=" url))
			  (onstate am 'ended))
		       (begin
			  (when (>=fx (mpg123-debug) 1)
			     (debug "!!! MPG123_DECODER, buffer empty ("
				(current-microseconds)
				") url=" url))
			  (if has-been-empty-once
			      (onerror am "empty buffer")
			      (onstate am 'buffering))
			  (mutex-lock! %bmutex)
			  (let liip ()
			     ;; wait until the buffer is filled
			     (unless (or (not %empty) %eof %!dabort (buffer-filled?))
				(when (>=fx (mpg123-debug) 2)
				   (debug "!!! MPG123_DECODER, waiting buffer ("
				      (current-microseconds)
				      ") url=" url))
				(condition-variable-wait! %bcondv %bmutex)
				(with-access::alsamusic am (%status)
				   (with-access::musicstatus %status (buffering)
				      (set! buffering
					 (buffer-percentage-filled))))
				(onstate am 'buffering)
				(liip)))
			  (mutex-unlock! %bmutex)
			  (onstate am 'play)
			  (loop))))
		  (else
		   ;; the buffer contains available bytes
		   (let flush ((s (minfx decsz
				     (if (>fx %head %tail)
					 (-fx %head %tail)
					 (-fx inlen %tail)))))
		      (let ((status ($bgl-mpg123-decode
				       %mpg123 %inbufp %tail s outbuf outlen)))
			 (when (>=fx (mpg123-debug) 4)
			    (debug "~~~ MPG123_DECODER, s=" s
			       " tl=" %tail " hd=" %head
			       " -> status="
			       (mpg123-decode-status->symbol status)))
			 (when (>fx s 0)
			    (inc-tail! s))
			 (cond
			    ((>fx %!dseek 0)
			     (let ((offset (mp3-index (alsabuffer-stream buffer) %!dseek)))
				(alsabuffer-seek buffer offset)
				(mpg123-handle-reset! %mpg123)
				(with-access::musicstatus %status (songpos)
				   (set! songpos %!dseek)
				   (set! %!dseek -1))
				(loop)))
			    ((=fx status $mpg123-ok)
			     ;; play and keep decoding
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>=fx (mpg123-debug) 5)
				   (debug "~~~ MPG123_DECODER, OK s=" s
				      " size=" size))
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)
				   (flush 0))))
			    ((=fx status $mpg123-need-more)
			     ;; play and loop to get more bytes
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>=fx (mpg123-debug) 5)
				   (debug "~~~ MPG123_DECODER, NEED-MORE s=" s
				      " size=" size " %empty=" %empty
				      " tail=" %tail))
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
			     (onerror am "mp3 decoding error"))))))))

	    (when (>fx (mpg123-debug) 0)
	       (debug-stop!))))))

;*---------------------------------------------------------------------*/
;*    new-format ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-format dec am buffer)
   (with-access::alsamusic am (%status pcm)
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
;*    *debug-port* ...                                                 */
;*---------------------------------------------------------------------*/
(define *debug-port* #f)

;*---------------------------------------------------------------------*/
;*    debug-init! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-init!)
   (set! *debug-port* (open-output-file "/tmp/MPG123.log")))

;*---------------------------------------------------------------------*/
;*    debug-stop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-stop!)
   (close-output-port *debug-port*))
   
;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define (debug . args)
   (apply fprint *debug-port* args)
   (flush-output-port *debug-port*))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
