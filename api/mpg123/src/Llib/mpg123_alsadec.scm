;*=====================================================================*/
;*    .../project/bigloo/api/mpg123/src/Llib/mpg123_alsadec.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Mon Jan 23 07:52:37 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MPG123 Alsa decoder                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mpg123_alsadec

   (library alsa)
   
   (import __mpg123_mpg123)

   (export (class mpg123-alsadecoder::alsadecoder
	      (%mpg123 read-only (default (instantiate::mpg123-handle))))))

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (bigloo-compiler-debug))

;*---------------------------------------------------------------------*/
;*    debug                                                            */
;*---------------------------------------------------------------------*/
(define debug ($compiler-debug))

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
(define-method (alsadecoder-position dec::mpg123-alsadecoder buf)
   (with-access::mpg123-alsadecoder dec (%mpg123)
      (/fx (mpg123-position %mpg123 buf) 1000)))

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
   
   (with-access::alsamusic am (pcm outbuf %status onerror)
      (with-access::mpg123-alsadecoder dec (%mpg123 %dmutex %dcondv
					      %!dstate %!dabort %!dpause)
	 (with-access::alsabuffer buffer (%bmutex %bcondv %!bstate %inbuf
					    %!tail %head %eof)
	    
	    (define inlen (string-length %inbuf))
	    
	    (define outlen (string-length outbuf))
	    
	    (define decsz (minfx (*fx 2 outlen) inlen))
	    
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
		      (with-access::alsabuffer buffer (profile-lock)
			 (set! profile-lock (+fx 1 profile-lock))
			 (tprint "dec.3, set empty (bs=0) mutex-lock="
			    profile-lock)))
		   (set! %!bstate 0)
		   (condition-variable-broadcast! %bcondv)
		   (mutex-unlock! %bmutex))
		  ((full-state?)
		   ;; set state filled
		   (mutex-lock! %bmutex)
		   (when (>fx debug 0)
		      (with-access::alsabuffer buffer (profile-lock)
			 (set! profile-lock (+fx 1 profile-lock))
			 (tprint "dec.3, set filled (bs=1) mutex-lock="
			    profile-lock)))
		   (set! %!bstate 1)
		   (condition-variable-broadcast! %bcondv)
		   (mutex-unlock! %bmutex))))

	    (define (onstate am st)
	       (with-access::alsamusic am (onstate %status)
		  (with-access::musicstatus %status (state)
		     (set! state st)
		     (onstate am %status))))

	    (let loop ()
	       (when (>fx debug 1)
		  (tprint "dec.1 bs=" %!bstate " %!dstate=" %!dstate
		     " tl=" %!tail " hd=" %head))
	       (cond
		  (%!dpause
		   ;;; the decoder is asked to pause
		   (with-access::musicstatus %status (songpos)
		      (with-access::alsabuffer buffer (%inbuf)
			 (set! songpos (alsadecoder-position dec %inbuf))))
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
				 (loop))))))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   (onstate am 'stop))
		  ((=fx %!bstate 0)
		   ;; the buffer empty...
		   (if %eof
		       ;; ... because we are done playing
		       (onstate am 'ended)
		       (begin
			  ;; ... we have to wait for byte to be available
			  (mutex-lock! %bmutex)
			  (when (=fx %!bstate 0)
			     (condition-variable-wait! %bcondv %bmutex)
			     (mutex-unlock! %bmutex))
			  (loop))))
		  (else
		   ;; the buffer contains available bytes
		   (let flush ((s (minfx decsz
				     (if (>fx %head %!tail)
					 (-fx %head %!tail)
					 (-fx inlen %!tail)))))
		      (let ((status ($bgl-mpg123-decode
				       %mpg123 %inbuf %!tail s outbuf outlen)))
			 (when (>fx debug 3)
			    (tprint "dec.2 s=" s
			       " tl=" %!tail " hd=" %head
			       " -> status=" (mpg123-decode-status->symbol status)
			       " size="
			       (with-access::mpg123-handle %mpg123 (size)
				  size)))
			 (when (>fx s 0) (inc-tail! s))
			 (cond
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
			     ;; an error occured
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
	 (with-access::alsabuffer buffer (%inbuf)
	    (set! songpos (alsadecoder-position dec %inbuf)))
	 (set! songlength 0)
	 (when (<fx songlength songpos)
	    (set! songlength songpos))
	 (multiple-value-bind (bitrate rate)
	    (alsadecoder-info dec)
	    (set! bitrate bitrate)
	    (set! khz rate))
	 (when (>fx %toseek 0)
	    (alsadecoder-seek dec %toseek)
	    (set! songpos %toseek)
	    (set! %toseek 0)))))
