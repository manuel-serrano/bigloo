;*=====================================================================*/
;*    .../project/bigloo/api/mpg123/src/Llib/mpg123_decoder.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Fri Apr  1 08:28:19 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPG123 decoder                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_decoder
   
   (library multimedia)
   
   (import __mpg123_mpg123)
   
   (export (class mpg123-decoder::musicdecoder
	      (outbuf::bstring read-only (default (make-string (*fx 5 1024))))
	      (%mpg123 read-only (default (instantiate::mpg123-handle)))
	      (%!dseek::long (default -1))
	      (%rate::int (default 0))
	      (%channels::int (default 0))
	      (%encoding::symbol (default 'unknown)))))
	   
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
;*    object-print ::mpg123-decoder ...                                */
;*---------------------------------------------------------------------*/
(define-method (object-print o::mpg123-decoder port print-slot)
   (display "#|mpg123-decoder|" port))

;*---------------------------------------------------------------------*/
;*    musicdecoder-init ::mpg123-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-init dec::mpg123-decoder)
   (with-access::mpg123-decoder dec (mimetypes)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/mpeg")))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-close ::mpg123-decoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-close dec::mpg123-decoder)
   (with-access::mpg123-decoder dec (%mpg123)
      (mpg123-handle-close %mpg123)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-reset! ::mpg123-decoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-reset! dec::mpg123-decoder)
   (with-access::mpg123-decoder dec (%mpg123)
      (mpg123-handle-reset! %mpg123)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-position ::mpg123-decoder ...                       */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-position dec::mpg123-decoder b)
   (with-access::mpg123-decoder dec (%mpg123)
      (/fx (mpg123-position %mpg123) 1000)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-info ::mpg123-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-info dec::mpg123-decoder)
   (with-access::mpg123-decoder dec (%mpg123)
      (mpg123-info %mpg123)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-seek ::mpg123-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-seek dec::mpg123-decoder sec)
   (with-access::mpg123-decoder dec (%mpg123 %!dseek)
      (when (>=fx (mpg123-debug) 2)
	 (debug "~~~ MPG123_SEEK, seek=" sec "\n"))
      (when (<fx %!dseek 0)
	 (set! %!dseek sec))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-volume-set! ::mpg123-decoder ...                    */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-volume-set! dec::mpg123-decoder vol)
   (with-access::mpg123-decoder dec (%mpg123)
      (mpg123-volume-set! %mpg123 vol)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-decode ::mpg123-decoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-decode dec::mpg123-decoder am::musicbuf buffer::musicbuffer)
   
   (with-access::music am (%status %error)
      (with-access::mpg123-decoder dec (%mpg123 %dmutex %dcondv outbuf
					  %!dabort %!dpause %!dseek)
	 (with-access::musicbuffer buffer (%bmutex %bcondv
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
			     (fixnum->llong (musicbuffer-available buffer)))
		     (fixnum->llong inlen))))
	    
	    (define (buffer-filled?)
	       ;; filled when > 12%
	       (>fx (*fx 8 (musicbuffer-available buffer)) inlen))
	    
	    (define (buffer-flushed?)
	       ;; flushed when slow fill or buffer fill < 75%
	       (or has-been-empty-once
		   (>fx (*fx 4 (-fx inlen (musicbuffer-available buffer)))
		      inlen)))
	    
	    (define (debug-inc-tail)
	       (when (>=fx (mpg123-debug) 3)
		  (let ((p (buffer-percentage-filled)))
		     (when (or (and (or (< p 75) has-been-empty-once)
				    (not %eof))
			       (>=fx (mpg123-debug) 4))
			(debug "--- MPG123_DECODER, buffer fill: "
			   (cond
			      ((>= p 80) "")
			      ((> p 25) "[0m[1;33m")
			      (else "[0m[1;32m"))
			   p
			   "%[0m"
			   (if %eof " EOF" "")
			   " url=" url "\n")))))
	    
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
	       (when (and (buffer-flushed?) (not %eof))
		  (when (>=fx (mpg123-debug) 2)
		     (debug "--> MPG123_DECODER, broadcast not-full "
			url " " (current-microseconds) "..."))
		  (synchronize %bmutex
		     (condition-variable-broadcast! %bcondv))
		  (when (>=fx (mpg123-debug) 2)
		     (debug (current-microseconds) "\n"))))
	    
	    (when (>fx (mpg123-debug) 0) (debug-init! url))
	    
	    ;; restore last mpg123 pcm configuration
	    (with-access::mpg123-decoder dec (%rate)
	       (when (>fx %rate 0)
		  (musicdecoder-hwparams-set! dec am buffer)))
	    
	    (let loop ()
	       (cond
		  (%!dpause
		   ;;; the decoder is asked to pause
		   (with-access::musicstatus %status (songpos)
		      (set! songpos (musicdecoder-position dec buffer)))
		   (music-state-set! am 'pause)
		   (synchronize %dmutex
		      (let liip ()
			 (when %!dpause
			    (condition-variable-wait! %dcondv %dmutex)
			    (liip))))
		   (music-state-set! am 'play)
		   (loop))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   (music-state-set! am 'stop))
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       (begin
			  (when (>=fx (mpg123-debug) 2)
			     (debug "~~~ MPG123_DECODER, EOF " url "\n"))
			  (music-state-set! am 'ended))
		       (begin
			  (when (>=fx (mpg123-debug) 1)
			     (debug "<-- MPG123_DECODER, wait not-empty " url " "
				(current-microseconds) "..."))
			  (with-access::music am (%status)
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
			  (when (>=fx (mpg123-debug) 1)
			     (debug (current-microseconds) "\n"))
			  (music-state-set! am 'play)
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
			       (mpg123-decode-status->symbol status)
			       "\n"))
			 (when (>fx s 0)
			    (inc-tail! s))
			 (cond
			    ((>fx %!dseek 0)
			     (let ((offset (mp3-index (musicbuffer-stream buffer) %!dseek)))
				(tprint "offset=" offset)
				(musicbuffer-seek buffer offset)
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
				      " size=" size "\n"))
				(when (>fx size 0)
				   ;; (-snd-pcm-write pcm outbuf size)
				   (musicbuf-write am outbuf size)
				   (flush 0))))
			    ((=fx status $mpg123-need-more)
			     ;; play and loop to get more bytes
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>=fx (mpg123-debug) 5)
				   (debug "~~~ MPG123_DECODER, NEED-MORE s=" s
				      " size=" size " %empty=" %empty
				      " tail=" %tail "\n"))
				(when (>fx size 0)
				   ;; (-snd-pcm-write pcm outbuf size)
				   (musicbuf-write am outbuf size)))
			     (loop))
			    ((=fx status $mpg123-new-format)
			     ;; a new playback
			     (new-format dec am buffer)
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>fx size 0)
				   ;; (-snd-pcm-write pcm outbuf size)
				   (musicbuf-write am outbuf size)))
			     (flush 0))
			    ((=fx status $mpg123-done)
			     ;; done playing
			     (with-access::mpg123-handle %mpg123 (size)
				(when (>fx size 0)
				   ;;(-snd-pcm-write pcm outbuf size)
				   (musicbuf-write am outbuf size)))
			     (musicbuf-drain am)
			     (music-state-set! am 'ended))
			    (else
			     ;; an error occurred
			     (music-error-set! am "mp3 decoding error"))))))))
	    
	    (when (>fx (mpg123-debug) 0)
	       (debug-stop! url))))))

;*---------------------------------------------------------------------*/
;*    new-format ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-format dec::mpg123-decoder am buffer)
   (with-access::mpg123-decoder dec (%rate %channels %encoding %mpg123)
      (multiple-value-bind (rate channels encoding)
	 (mpg123-get-format %mpg123)
	 (set! %rate rate)
	 (set! %channels channels)
	 (set! %encoding encoding)
	 (musicdecoder-hwparams-set! dec am buffer)))
   (with-access::music am (%status)
      (with-access::musicstatus %status (songpos songlength bitrate khz)
	 (set! songpos (musicdecoder-position dec buffer))
	 (multiple-value-bind (brate rate)
	    (musicdecoder-info dec)
	    (set! bitrate brate)
	    (set! khz rate)))))

;*---------------------------------------------------------------------*/
;*    *debug-port* ...                                                 */
;*---------------------------------------------------------------------*/
(define *debug-port* #f)

;*---------------------------------------------------------------------*/
;*    debug-init! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-init! url)
   (set! *debug-port* (open-output-file "/tmp/MPG123.log"))
   (debug ">>> MPG123_DECODER init " url " " (current-microseconds) "\n"))

;*---------------------------------------------------------------------*/
;*    debug-stop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-stop! url)
   (debug ">>> MPG123_DECODER stop " url " " (current-microseconds) "\n")
   (close-output-port *debug-port*))
   
;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define (debug . args)
   (for-each (lambda (a) (display a *debug-port*)) args)
   (flush-output-port *debug-port*))
