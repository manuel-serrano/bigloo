;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/src/Llib/wav_decoder.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 21 08:15:23 2013                          */
;*    Last change :  Wed Mar  2 15:28:50 2016 (serrano)                */
;*    Copyright   :  2013-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WAV music decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __wav_decoder

   (library multimedia)
   
   (import __wav_wav)

   (export (class wavmusicdecoder::musicdecoder
	      (outbuf::bstring read-only (default (make-string (*fx 2 1024))))
	      (%header::obj (default #f))
	      (%cursor::int (default 0))
	      (%size::int (default 0))
	      (%position::int (default 0)))))
	  
;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (bigloo-compiler-debug))

;*---------------------------------------------------------------------*/
;*    wav-debug ...                                                    */
;*---------------------------------------------------------------------*/
(define (wav-debug)
   (if (>fx ($compiler-debug) 0)
       (bigloo-debug)
       0))

;*---------------------------------------------------------------------*/
;*    object-print ::wavmusicdecoder ...                               */
;*---------------------------------------------------------------------*/
(define-method (object-print o::wavmusicdecoder port print-slot)
   (display "#|wavmusicdecoder|" port))

;*---------------------------------------------------------------------*/
;*    musicdecoder-init ::wavmusicdecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-init dec::wavmusicdecoder)
   (with-access::wavmusicdecoder dec (mimetypes)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/wav" "audio/x-wav" "audio/x-pn-windows-acm")))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-reset! ::wavmusicdecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-reset! dec::wavmusicdecoder)
   (with-access::wavmusicdecoder dec (%header)
      (set! %header #f)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-position ::wavmusicdecoder ...                      */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-position dec::wavmusicdecoder b)
   (with-access::wavmusicdecoder dec (%position)
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
;*    musicdecoder-decode ::wavmusicdecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-decode dec::wavmusicdecoder
		  am::musicbuf
		  buffer::musicbuffer)
   (with-access::musicbuf am (pcm %status)
      (with-access::wavmusicdecoder dec (%dmutex %dcondv %header %size
					   %!dabort %!dpause outbuf)
	 (with-access::musicbuffer buffer (%bmutex %bcondv
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
			     (fixnum->llong (musicbuffer-available buffer)))
		     (fixnum->llong inlen))))
	    
	    (define (buffer-filled?)
	       ;; filled when > 12%
	       (>fx (*fx 8 (musicbuffer-available buffer)) inlen))
	    
	    (define (buffer-flushed?)
	       ;; flushed when slow fill or buffer fill < 75%
	       (>fx (*fx (if has-been-empty-once 2 4)
		       (-fx inlen (musicbuffer-available buffer)))
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

	    (define (flush sz::int)
	       (when (>fx sz 0)
		  (flush (-fx sz (musicbuf-write am outbuf sz)))))
	    
	    (when (>fx (wav-debug) 0) (debug-init! url))
	    
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
			  (when (>=fx (wav-debug) 2)
			     (debug "~~~ WAV_DECODER, EOF " url "\n"))
			  (music-state-set! am 'ended))
		       (begin
			  (when (>=fx (wav-debug) 1)
			     (debug "<-- WAV_DECODER, wait not-empty " url " "
				(current-microseconds) "..."))
			  (with-access::musicbuf am (%status)
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
			  (when (>=fx (wav-debug) 1)
			     (debug (current-microseconds) "\n"))
			  (music-state-set! am 'play)
			  (loop))))
		  (else
		   ;; the buffer contains available bytes
		   (let ((s (minfx outlen
			       (if (>fx %head %tail)
				   (-fx %head %tail)
				   (-fx inlen %tail)))))
		      (if (=fx s 0)
			  (loop)
			  (let ((status (wav-decode dec am buffer %tail s)))
			     (when (>=fx (wav-debug) 4)
				(debug "~~~ WAV_DECODER, s=" s
				   " tl=" %tail " hd=" %head
				   " -> status="
				   (wav-decode-status->symbol status)
				   "\n"))
			     (cond
				((=fx status $wav-status-ok)
				 ;; play and keep decoding
				 (when (>=fx (wav-debug) 5)
				    (debug "~~~ WAV_DECODER, OK s=" s
				       " size=" %size "\n"))
				 (flush s)
				 (inc-tail! s)
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
				 (musicdecoder-hwparams-set! dec am buffer)
				 (inc-tail! (elong->fixnum (wav-header-size)))
				 (loop))
				((=fx status $wav-status-done)
				 ;; done playing
				 (when (>fx %size 0) (flush %size))
				 (music-state-set! am 'ended))
				(else
				 ;; an error occurred
				 (with-access::musicstatus %status (err state)
				    (set! err "wav decoding error")
				    (set! state 'error))
				 (music-error-set! am "wav decoding error")))))))))
	    (when (>fx (wav-debug) 0)
	       (debug-stop! url))))))

;*---------------------------------------------------------------------*/
;*    wav-decode ...                                                   */
;*---------------------------------------------------------------------*/
(define (wav-decode dec::wavmusicdecoder am::musicbuf buffer::musicbuffer
	   tail::int s::int)
   (with-access::wavmusicdecoder dec (%header %size %position %cursor outbuf)
      (if (not %header)
	  (with-handler
	     (lambda (e)
		(music-error-set! am e))
	     (if (<fx s (wav-header-size))
		 $wav-status-need-more
		 (let ((str (musicbuffer-substring buffer 0
			       (elong->fixnum (wav-header-size)))))
		    (set! %header (wav-parse-header str))
		    (with-access::wavinfo %header (audiofmt)
		       (unless (eq? audiofmt 'pcm)
			  (error "wav-decode"
			     "Cannot play none PCM wav format" audiofmt)))
		    (set! %size 0)
		    (set! %cursor 0)
		    (set! %position 0)
		    $wav-status-new-format)))
	  (with-access::musicbuffer buffer (%tail)
	     (with-access::wavinfo %header (blockalign chunksize)
		(cond
		   ((<fx s blockalign)
		    (musicbuffer-blit-string! buffer %tail outbuf %cursor s)
		    (set! %cursor (+fx %cursor s))
		    $wav-status-need-more)
		   ((>=fx %position chunksize)
		    $wav-status-done)
		   (else
		    (set! %size (*fx blockalign (/fx s blockalign)))
		    (set! %position (+fx %position %size))
		    (musicbuffer-blit-string! buffer %tail outbuf %cursor %size)
		    (set! %cursor 0)
		    $wav-status-ok)))))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-volume-set! ::wavmusicdecoder ...                   */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-volume-set! dec::wavmusicdecoder vol)
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
