;*=====================================================================*/
;*    .../project/bigloo/api/mpg123/src/Llib/mpg123_alsadec.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Wed Sep 21 16:29:02 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
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
;*    debug                                                            */
;*---------------------------------------------------------------------*/
(define debug 1)

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

   (with-access::alsamusic am (pcm outbuf %status %decoder)
      (with-access::mpg123-alsadecoder %decoder (%!pause %mpg123
						   %dmutex %dcondv)
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
		   (mutex-unlock! %bmutex)))
	       (when (>fx debug 0)
		  (alsabuffer-assert buffer "decode")))

	    (define (musicstatus-set-play!)
	       (with-access::musicstatus %status (state songpos songlength)
		  (set! songpos (alsadecoder-position dec %inbuf))
		  (when (<fx songlength songpos)
		     (set! songlength songpos))
		  (set! state 'play)))

	    (define (abort state)
	       (mutex-lock! %dmutex)
	       (musicstatus-state-set! %status state)
	       (set! %!bstate (if (eq? state 'ended) 4 3))
	       (pcm-cleanup pcm)
	       (condition-variable-broadcast! %dcondv)
	       (mutex-unlock! %dmutex))
	    
	    (let loop ()
	       (when (>fx debug 1)
		  (tprint "dec.1 bs=" %!bstate " pa=" %!pause
		     " tl=" %!tail " hd=" %head))
	       (cond
		  ((=fx %!bstate 3)
		   ;; buffer stop, the buffer is done reading
		   (abort 'stop))
		  (%!pause
		   ;;; user request pause, swith to pause state and wait
		   ;;; to be awaken
		   (musicstatus-state-set! %status 'pause)
		   (mutex-lock! %dmutex)
		   (condition-variable-wait! %dcondv %dmutex)
		   (mutex-unlock! %dmutex)
		   (musicstatus-set-play!)
		   (loop))
		  ((=fx %!bstate 0)
		   ;; buffer empty
		   (if %eof
		       (begin
			  (when (>fx debug 1)
			     (tprint "dec.4 ended"))
			  ;; buffer empty, and eof, we are done
			  (abort 'ended))
		       (begin
			  ;; buffer empty, wait to be filled
			  (mutex-lock! %bmutex)
			  (when (=fx %!bstate 0)
			     ;; a kind of double check locking, correct, is
			     ;; ptr read/write are atomic
			     (condition-variable-wait! %bcondv %bmutex)
			     (mutex-unlock! %bmutex))
			  (loop))))
		  (else
		   ;; decode some more bytes
		   (let flush ((s (minfx decsz
				     (if (>fx %head %!tail)
					 (-fx %head %!tail)
					 (-fx inlen %!tail)))))
		      (let ((status ($bgl-mpg123-decode
				       %mpg123 %inbuf %!tail s outbuf outlen)))
			 (when (>fx debug 3)
			    (tprint "dec.2 s=" s
			       " tl=" %!tail " hd=" %head
			       " -> status=" status " size="
			       (mpg123-handle-size %mpg123)))
			 (when (>fx s 0)
			    (inc-tail! s))
			 (cond
			    ((or (=fx %!bstate 3) %!pause)
			     (loop))
			    ((=fx status $mpg123-ok)
			     ;; play and keep decoding
			     (let ((size (mpg123-handle-size %mpg123)))
				(when (>fx size 0)
				   (musicstatus-set-play!)
				   (alsa-snd-pcm-write pcm outbuf size)
				   (flush 0))))
			    ((=fx status $mpg123-need-more)
			     ;; play and loop to get more bytes
			     (let ((size (mpg123-handle-size %mpg123)))
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)))
			     (loop))
			    ((=fx status $mpg123-new-format)
			     ;; a new playback
			     (new-format dec am buffer)
			     (let ((size (mpg123-handle-size %mpg123)))
				(when (>fx size 0)
				   (musicstatus-set-play!)
				   (alsa-snd-pcm-write pcm outbuf size)))
			     (flush 0))
			    ((=fx status $mpg123-done)
			     ;; done playing
			     (let ((size (mpg123-handle-size %mpg123)))
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size)))
			     (abort 'ended))
			    (else
			     ;; an error occured
			     (musicstatus-err-set! %status "mp3 decoding error")
			     (abort 'error))))))))))))

;*---------------------------------------------------------------------*/
;*    pcm-cleanup ...                                                  */
;*---------------------------------------------------------------------*/
(define (pcm-cleanup pcm)
   (let ((pcm-state (alsa-snd-pcm-get-state pcm)))
      (when (memq pcm-state '(running prepared))
	 (alsa-snd-pcm-drop pcm)))
   (alsa-snd-pcm-cleanup pcm))

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
;* 	       :rate-resample 1                                        */
	       :access 'rw-interleaved
	       :format encoding
	       :channels channels
	       :rate-near rate
;* 	       :buffer-time-near buffer-time-near                          */
	       :buffer-size-near (/fx rate buffer-size-near-ratio)
	       :period-size-near (/fx rate period-size-near-ratio))
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
