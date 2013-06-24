;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Llib/wav_alsadec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 21 08:15:23 2013                          */
;*    Last change :  Fri Jun 21 20:21:47 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WAV music decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_alsadec

   (cond-expand
      ((library alsa)
       (library alsa)))

   (library multimedia)
   
   (import __wav_wav)

   (cond-expand
      ((library alsa)
       (export (class wav-alsadecoder::alsadecoder
		  (outbuf::bstring read-only (default (make-string (*fx 2 1024))))
		  (%header::obj (default #f))
		  (%cursor::int (default 0))
		  (%size::int (default 0))
		  (%position::int (default 0)))))))

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
;*    wav-debug ...                                                    */
;*---------------------------------------------------------------------*/
(define (wav-debug)
   (if (>fx ($compiler-debug) 0)
       (bigloo-debug)
       0))

;*---------------------------------------------------------------------*/
;*    object-print ::wav-alsadecoder ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-print o::wav-alsadecoder port print-slot)
   (display "#|wav-alsadecoder|" port))

;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::wav-alsadecoder ...                           */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-init dec::wav-alsadecoder)
   (with-access::wav-alsadecoder dec (mimetypes)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/wav" "audio/x-wav" "audio/x-pn-windows-acm")))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-reset! ::wav-alsadecoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-reset! dec::wav-alsadecoder)
   (with-access::wav-alsadecoder dec (%header)
      (set! %header #f)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::wav-alsadecoder ...                       */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-position dec::wav-alsadecoder b)
   (with-access::wav-alsadecoder dec (%position)
      %position))

;*---------------------------------------------------------------------*/
;*    wav-status ...                                                   */
;*---------------------------------------------------------------------*/
(define $wav-status-new-format 1)
(define $wav-status-done 2)
(define $wav-status-need-more 3)
(define $wav-status-ok 4)

;*---------------------------------------------------------------------*/
;*    wav-decode-status->symbol ...                                    */
;*---------------------------------------------------------------------*/
(define (wav-decode-status->symbol status)
   (cond
      ((=fx $wav-status-new-format status) 'new-format)
      ((=fx $wav-status-done status) 'done)
      ((=fx $wav-status-need-more status) 'need-more)
      ((=fx $wav-status-ok status) 'ok)
      (else 'unknown-status)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::wav-alsadecoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode dec::wav-alsadecoder
		  am::alsamusic
		  buffer::alsabuffer)
   (with-access::alsamusic am (pcm %status onerror %error)
      (with-access::wav-alsadecoder dec (%dmutex %dcondv %header %size
					   %!dabort %!dpause outbuf)
	 (with-access::alsabuffer buffer (%bmutex %bcondv
					    %inlen
					    %tail %head
					    %eof %empty
					    url)

	    (define inlen %inlen)
	    
	    (define outlen (string-length outbuf))
	    
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
	       (>fx (*fx (if has-been-empty-once 2 4)
		       (-fx inlen (alsabuffer-available buffer)))
		  inlen))
	    
	    (define (debug-inc-tail)
	       (when (>=fx (wav-debug) 3)
		  (let ((p (buffer-percentage-filled)))
		     (when (or (and (or (< p 75) has-been-empty-once)
				    (not %eof))
			       (>=fx (wav-debug) 4))
			(debug "--- WAV_DECODER, buffer fill: "
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
		  (when (>=fx (wav-debug) 2)
		     (debug "--> WAV_DECODER, broadcast not-full "
			url " " (current-microseconds) "..."))
		  (synchronize %bmutex
		     (condition-variable-broadcast! %bcondv))
		  (when (>=fx (wav-debug) 2)
		     (debug (current-microseconds) "\n"))))

	    (define (onstate am st)
	       (with-access::alsamusic am (onstate %status)
		  (with-access::musicstatus %status (state)
		     (set! state st)
		     (onstate am %status))))

	    (define (flush sz::int)
	       (when (>fx sz 0)
		  (flush (-fx sz (alsa-snd-pcm-write pcm outbuf sz)))))
	    
	    (when (>fx (wav-debug) 0) (debug-init! url))

	    (let loop ()
	       (cond
		  (%!dpause
		   ;;; the decoder is asked to pause
		   (with-access::musicstatus %status (songpos)
		      (set! songpos (alsadecoder-position dec buffer)))
		   (onstate am 'pause)
		   (synchronize %dmutex
		      (let liip ()
			 (when %!dpause
			    (condition-variable-wait! %dcondv %dmutex)
			    (liip))))
		   (onstate am 'play)
		   (loop))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   (onstate am 'stop))
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       (begin
			  (when (>=fx (wav-debug) 2)
			     (debug "~~~ WAV_DECODER, EOF " url "\n"))
			  (onstate am 'ended))
		       (begin
			  (when (>=fx (wav-debug) 1)
			     (debug "<-- WAV_DECODER, wait not-empty " url " "
				(current-microseconds) "..."))
			  (with-access::alsamusic am (%status)
			     (with-access::musicstatus %status (buffering)
				(set! buffering
				   (buffer-percentage-filled))))
			  (onstate am 'buffering)
			  (synchronize %bmutex
			     ;; wait until the buffer is filled
			     (unless (or (not %empty)
					 %eof
					 %!dabort
					 (buffer-filled?))
				(condition-variable-wait! %bcondv %bmutex)))
			  (when (>=fx (wav-debug) 1)
			     (debug (current-microseconds) "\n"))
			  (onstate am 'play)
			  (loop))))
		  (else
		   ;; the buffer contains available bytes
		   (let ((s (minfx outlen
			       (if (>fx %head %tail)
				   (-fx %head %tail)
				   (-fx inlen %tail)))))
		      (let ((status (wav-decode dec am buffer %tail s)))
			 (when (>=fx (wav-debug) 4)
			    (debug "~~~ WAV_DECODER, s=" s
			       " tl=" %tail " hd=" %head
			       " -> status="
			       (wav-decode-status->symbol status)
			       "\n"))
			 (when (>fx s 0)
			    (inc-tail! s))
			 (cond
;* 			    ((>fx %!dseek 0)                           */
;* 			     (let ((offset (wav-index (alsabuffer-stream buffer) %!dseek))) */
;* 				(alsabuffer-seek buffer offset)        */
;* 				(wav-handle-reset! %wav)               */
;* 				(with-access::musicstatus %status (songpos) */
;* 				   (set! songpos %!dseek)              */
;* 				   (set! %!dseek -1))                  */
;* 				(loop)))                               */
			    ((=fx status $wav-status-ok)
			     ;; play and keep decoding
			     (when (>=fx (wav-debug) 5)
				(debug "~~~ WAV_DECODER, OK s=" s
				   " size=" %size "\n"))
			     (when (>fx %size 0) (flush %size))
			     (loop))
			    ((=fx status $wav-status-need-more)
			     ;; play and loop to get more bytes
			     (when (>=fx (wav-debug) 5)
				(debug "~~~ WAV_DECODER, NEED-MORE s=" s
				   " size=" %size " %empty=" %empty
				   " tail=" %tail "\n"))
			     (loop))
			    ((=fx status $wav-status-new-format)
			     ;; a new playback
			     (new-format dec am buffer)
			     (loop))
			    ((=fx status $wav-status-done)
			     ;; done playing
			     (when (>fx %size 0) (flush %size))
			     (onstate am 'ended))
			    (else
			     ;; an error occurred
			     (with-access::musicstatus %status (err state)
				(set! err "wav decoding error")
				(set! state 'error))
			     (set! %error "wav decoding error")
			     (onerror am "wav decoding error"))))))))

	    (when (>fx (wav-debug) 0)
	       (debug-stop! url))))))

;*---------------------------------------------------------------------*/
;*    wav-decode ...                                                   */
;*---------------------------------------------------------------------*/
(define (wav-decode dec::wav-alsadecoder am::alsamusic buffer::alsabuffer
	   tail::int s::int)
   (with-access::wav-alsadecoder dec (%header %size %position %cursor outbuf)
      (if (not %header)
	  (with-handler
	     (lambda (e)
		(with-access::alsamusic am (%status onerror %error)
		   (with-access::musicstatus %status (err state)
		      (set! err e)
		      (set! state 'error))
		   (set! %error e)
		   (onerror am e)))
	     (let ((str (alsabuffer-substring buffer 0
			   (elong->fixnum (wav-header-size)))))
		(set! %header (wav-parse-header str))
		(tprint "header=" %header)
		(with-access::wavinfo %header (audiofmt)
		   (unless (eq? audiofmt 'pcm)
		      (error "wav-decode"
			 "Cannot play none PCM wav format" audiofmt)))
		(set! %size 0)
		(set! %cursor 0)
		$wav-status-new-format))
	  (with-access::alsabuffer buffer (%tail)
	     (with-access::wavinfo %header (blockalign chunksize)
		(cond
		   ((<fx s blockalign)
		    (alsabuffer-blit-string! buffer %tail outbuf %cursor s)
		    (set! %cursor (+fx %cursor s))
		    $wav-status-need-more)
		   ((>=fx %position chunksize)
		    $wav-status-done)
		   (else
		    (set! %size (*fx blockalign (/fx s blockalign)))
		    (set! %position (+fx %position %size))
		    (alsabuffer-blit-string! buffer %tail outbuf %cursor %size)
		    (set! %cursor 0)
		    $wav-status-ok)))))))
   
;*---------------------------------------------------------------------*/
;*    new-format ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-format dec::wav-alsadecoder am::alsamusic buffer::alsabuffer)
   (with-access::alsamusic am (%status pcm)
      (with-access::wav-alsadecoder dec (%header %position
					   buffer-time-near
					   buffer-size-near-ratio
					   period-size-near-ratio)
	 (set! %position 0)
	 (with-access::wavinfo %header (channels audiofmt bps samplerate)
	    (let ((encoding (case bps
			       ((8) 'u8)
			       ((16) 's16)
			       ((24) 's24-3le)
			       ((32) 's32))))
	       (alsa-snd-pcm-hw-set-params! pcm
		  :access 'rw-interleaved
		  :format encoding
		  :channels channels
		  :rate-near samplerate
		  :buffer-size-near-ratio buffer-size-near-ratio
		  :period-size-near-ratio period-size-near-ratio)
	       (alsa-snd-pcm-sw-set-params! pcm
		  :start-threshold 1
		  :avail-min 1))))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-volume-set! ::wav-alsadecoder ...                    */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-volume-set! dec::wav-alsadecoder vol)
   #f)

;*---------------------------------------------------------------------*/
;*    *debug-port* ...                                                 */
;*---------------------------------------------------------------------*/
(define *debug-port* #f)

;*---------------------------------------------------------------------*/
;*    debug-init! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-init! url)
   (set! *debug-port* (open-output-file "/tmp/WAV.log"))
   (debug ">>> WAV_DECODER init " url " " (current-microseconds) "\n"))

;*---------------------------------------------------------------------*/
;*    debug-stop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-stop! url)
   (debug ">>> WAV_DECODER stop " url " " (current-microseconds) "\n")
   (close-output-port *debug-port*))
   
;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define (debug . args)
   (for-each (lambda (a) (display a *debug-port*)) args)
   (flush-output-port *debug-port*))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
