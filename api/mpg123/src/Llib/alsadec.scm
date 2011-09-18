;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/src/Llib/alsadec.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 17 07:53:28 2011                          */
;*    Last change :  Sun Sep 18 19:09:25 2011 (serrano)                */
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
      (mpg123-handle-reset! %mpg123)))

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
      (with-access::mpg123-alsadecoder %decoder (%!pause %dmutex %dcondv %mpg123)
	 (with-access::alsabuffer buffer (%bmutex %bcondv %!bstate %inbuf %!tail %head %eof)

	    (define inlen (string-length %inbuf))
	    
	    (define outlen (string-length outbuf))

	    (define decsz (minfx (*fx 4 outlen) inlen))

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
		      (tprint "dec.3, set empty (0)"))
		   (set! %!bstate 0)
		   (condition-variable-broadcast! %bcondv)
		   (mutex-unlock! %bmutex))
		  ((full-state?)
		   ;; set state filled
		   (mutex-lock! %bmutex)
		   (when (>fx debug 0)
		      (tprint "dec.3, set filled (1)"))
		   (set! %!bstate 1)
		   (condition-variable-broadcast! %bcondv)
		   (mutex-unlock! %bmutex)))
	       (alsabuffer-debug buffer "decode"))

	    (define (musicstatus-set-play!)
	       (with-access::musicstatus %status (state songpos songlength)
		  (unless (eq? state 'play)
		     (with-access::alsamusic am (%status %amutex)
			(mutex-unlock! %amutex)
			(set! songpos (alsadecoder-position dec %inbuf))
			(when (<fx songlength songpos)
			   (set! songlength songpos))
			(set! state 'play)
			(mutex-unlock! %amutex)))))
	    
	    (let loop ()
	       (when (>fx debug 1)
		  (with-lock %bmutex
		     (lambda ()
			(tprint "dec.1 bs=" %!bstate " pa=" %!pause
			   " tl=" %!tail " hd=" %head))))
	       (cond
		  ((=fx %!bstate 3)
		   ;; the buffer is done reading
		   (mutex-lock! %dmutex)
		   (musicstatus-state-set! %status 'stop)
		   (pcm-cleanup pcm)
		   (condition-variable-broadcast! %dcondv)
		   (mutex-unlock! %dmutex))
		  (%!pause
		     ;; user request pause, swith to pause state and wait
		     ;; to be awaken
		     (musicstatus-state-set! %status 'pause)
		     (mutex-lock! %dmutex)
		     (condition-variable-wait! %dcondv %dmutex)
		     (mutex-unlock! %dmutex)
		     (musicstatus-set-play!)
		     (loop))
		  ((=fx %!bstate 0)
		   (if %eof
		       (begin
			  (when (>fx debug 0)
			     (with-lock %bmutex
				(lambda ()
				   (set! %!bstate 4)
				   (tprint "dec.4 ended"))))
			  ;; buffer empty, and eof, we are done
			  (musicstatus-state-set! %status 'ended)
			  (pcm-cleanup pcm))
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
		      (multiple-value-bind (status size)
			 (mpg123-decode %mpg123 %inbuf %!tail s outbuf outlen)
			 (when (>fx debug 2)
			    (with-lock %bmutex
			       (lambda ()
				  (tprint "dec.2 s=" s
				     " tl=" %!tail " hd=" %head
				     " -> status=" status " size=" size))))
			 (when (>fx s 0) (inc-tail! s))
			 (when (=fx %!bstate 3) (set! status 'abort))
			 (when %!pause (set! status 'pause))
			 (case status
			    ((ok)
			     ;; play and keep decoding
			     (when (>fx size 0)
				(musicstatus-set-play!)
				(alsa-snd-pcm-write pcm outbuf size)
				(flush 0)))
			    ((need-more)
			     ;; play and loop to get more bytes
			     (when (>fx size 0)
				(alsa-snd-pcm-write pcm outbuf size))
			     (loop))
			    ((new-format)
			     ;; a new playback
			     (new-format dec am buffer)
			     (when (>fx size 0)
				(musicstatus-set-play!)
				(alsa-snd-pcm-write pcm outbuf size))
			     (flush 0))
			    ((done)
			     ;; done playing
			     (when (>fx size 0)
				(musicstatus-set-play!)
				(alsa-snd-pcm-write pcm outbuf size))
			     (musicstatus-state-set! %status 'ended)
			     (pcm-cleanup pcm)
			     (mutex-lock! %bmutex)
			     (set! %!bstate 4)
			     (condition-variable-broadcast! %bcondv)
			     (mutex-unlock! %bmutex))
			    ((abort pause)
			     ;; done playing
			     (loop))
			    (else
			     ;; an error occured
			     (musicstatus-state-set! %status 'error)
			     (musicstatus-err-set! %status "mp3 decoding error")
			     (pcm-cleanup pcm)
			     (mutex-lock! %bmutex)
			     (set! %!bstate 3)
			     (condition-variable-broadcast! %bcondv)
			     (mutex-unlock! %bmutex))))))))))))

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
      (with-access::mpg123-alsadecoder dec (%mpg123)
	 (multiple-value-bind (rate channels encoding)
	    (mpg123-get-format %mpg123)
	    (alsa-snd-pcm-set-params! pcm
	       :format encoding
	       :access 'rw-interleaved
	       :channels channels
	       :rate rate
	       :soft-resample 1
	       :latency 500000)
	    (alsa-snd-pcm-hw-set-params! pcm
	       :channels channels
	       :format encoding
	       :rate-near rate
	       :buffer-size-near (/fx rate 2)
	       :period-size-near (/fx rate 8))))
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
