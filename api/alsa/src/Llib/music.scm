;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/music.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Fri Jul  1 15:50:45 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A (multimedia) music player.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_music
   
   (library multimedia)
   
   (import  __alsa_alsa
	    __alsa_pcm)

   (static  )
   
   (export  (class alsamusic::music
	      (%decoder (default #f))
	      (%playlist::pair-nil (default '()))
	      (%thread::obj (default #f))
	      (%amutex::mutex read-only (default (make-mutex)))
	      (%acondv::condvar read-only (default (make-condition-variable)))
	      (%toseek::long (default 0))
	      (%volume::long (default 100))
	      (%buffer::obj (default #f))
	      (mkthread::procedure read-only)
	      (inbuf::bstring read-only (default (make-string (*fx 1024 1024))))
	      (outbuf::bstring read-only (default (make-string (*fx 8 1024))))
	      (pcm::alsa-snd-pcm read-only (default (instantiate::alsa-snd-pcm)))
	      (decoders::pair read-only))

	    (class alsabuffer
	       (mutex::mutex read-only (default (make-mutex)))
	       (condv::condvar read-only (default (make-condition-variable)))
	       (inbuf::bstring read-only)
	       (inoff::long (default 0))
	       (outoff::long (default 0))
	       (port::input-port read-only)
	       (state::symbol (default 'play))
	       (count::long (default 0))
	       (eof::bool (default #f)))	    

	   (class alsadecoder
	      (alsadecoder-init))

	   (class alsadecoder-host::alsadecoder)
	   (class alsadecoder-client::alsadecoder)

	   (generic alsabuffer-fill! ::alsabuffer ::long)

	   (generic alsadecoder-init ::alsadecoder)
	   (generic alsadecoder-reset! ::alsadecoder)
	   (generic alsadecoder-close ::alsadecoder)
	   (generic alsadecoder-can-play-type? ::alsadecoder ::bstring)
	   (generic alsadecoder-decode ::alsadecoder ::alsamusic ::alsabuffer)
	   (generic alsadecoder-decode-buffer ::alsadecoder ::bstring ::long ::long ::bstring)
	   (generic alsadecoder-position::long ::alsadecoder ::bstring)
	   (generic alsadecoder-info::long ::alsadecoder)
	   (generic alsadecoder-seek::long ::alsadecoder ::long)
	   (generic alsadecoder-volume-set! ::alsadecoder ::long)))

;*---------------------------------------------------------------------*/
;*    debug                                                            */
;*---------------------------------------------------------------------*/
(define debug-buffer 1)
(define debug-decode 2)

;*---------------------------------------------------------------------*/
;*    music-init ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-init o::alsamusic)
   (with-access::alsamusic o (%amutex %status pcm inbuf outbuf)
      (with-lock %amutex
	 (lambda ()
	    (cond
	       ((<fx (string-length outbuf) 8192)
		(raise (instantiate::&alsa-error
			  (proc "alsamusic")
			  (msg "outbuf must be at least 8192 bytes")
			  (obj (string-length inbuf)))))
	       ((<fx (string-length inbuf) (string-length outbuf))
		(raise (instantiate::&alsa-error
			  (proc "alsamusic")
			  (msg "inbuf length must be greater that outbuf length")
			  (obj (cons (string-length inbuf) (string-length outbuf) ))))))
	    (musicstatus-state-set! %status 'uninitialized)))))

;*---------------------------------------------------------------------*/
;*    music-close ::alsamusic ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-close o::alsamusic)
   (unless (music-closed? o)
      (with-access::alsamusic o (pcm decoders)
	 (alsa-snd-pcm-close pcm)
	 (for-each alsadecoder-close decoders))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::alsamusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::alsamusic)
   (with-access::alsamusic o (pcm)
      (eq? (alsa-snd-pcm-get-state pcm) 'disconnected)))

;*---------------------------------------------------------------------*/
;*    music-reset! ::alsamusic ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-reset! o::alsamusic)
   (with-access::alsamusic o (pcm %amutex %status)
      (alsa-snd-pcm-drop pcm)
      (alsa-snd-pcm-reset pcm)
      (musicstatus-state-set! %status 'stop))
   o)

;*---------------------------------------------------------------------*/
;*    music-playlist-get ::alsamusic ...                               */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get o::alsamusic)
   (with-access::alsamusic o (%playlist)
      %playlist))

;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::alsamusic ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! o::alsamusic s)
   (with-access::alsamusic o (%amutex %playlist %status)
      (with-lock %amutex
	 (lambda ()
	    (set! %playlist (append! %playlist (list s)))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (+fx 1 playlistlength)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::alsamusic ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! o::alsamusic n)
   (with-access::alsamusic o (%amutex %playlist %status)
      (with-lock %amutex
	 (lambda ()
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (when (and (>=fx n 0) (<fx n playlistlength))
		  (set! %playlist (remq! (list-ref %playlist n) %playlist))
		  (set! playlistid (+fx 1 playlistid))
		  (set! playlistlength (length %playlist))))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::alsamusic ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! o::alsamusic)
   (with-access::alsamusic o (%amutex %playlist %status)
      (with-lock %amutex
	 (lambda ()
	    (set! %playlist '())
	    (with-access::musicstatus %status (playlistlength song songid)
	       (set! song 0)
	       (set! songid 0)
	       (set! playlistlength 0))))))

;*---------------------------------------------------------------------*/
;*    music-status ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (music-status o::alsamusic)
   (with-access::alsamusic o (%status)
      %status))

;*---------------------------------------------------------------------*/
;*    music-update-status! ::mpg123 ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-update-status! o::alsamusic status)
   (with-access::alsamusic o (%amutex %status %volume)
      (with-lock %amutex
	 (lambda ()
	    (musicstatus-volume-set! status %volume)
	    (musicstatus-state-set! status (musicstatus-state %status))
	    (musicstatus-repeat-set! status (musicstatus-repeat %status))
	    (musicstatus-random-set! status (musicstatus-random %status))
	    (musicstatus-playlistid-set! status (musicstatus-playlistid %status))
	    (musicstatus-playlistlength-set! status (musicstatus-playlistlength %status))
	    (musicstatus-xfade-set! status (musicstatus-xfade %status))
	    (musicstatus-song-set! status (musicstatus-song %status))
	    (musicstatus-songid-set! status (musicstatus-songid %status))
	    (musicstatus-songpos-set! status (musicstatus-songpos %status))
	    (musicstatus-songlength-set! status (musicstatus-songlength %status))
	    (musicstatus-bitrate-set! status (musicstatus-bitrate %status))
	    (musicstatus-khz-set! status (musicstatus-khz %status))
	    (musicstatus-err-set! status (musicstatus-err %status))))))

;*---------------------------------------------------------------------*/
;*    music-can-play-type? ::alsamusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-can-play-type? o::alsamusic mimetype::bstring)
   (with-access::alsamusic o (decoders)
      (any (lambda (d) (alsadecoder-can-play-type? d mimetype)) decoders)))
   
;*---------------------------------------------------------------------*/
;*    music-play ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-play o::alsamusic . s)
   (with-access::alsamusic o (%amutex %status %playlist %amutex)
      (with-access::musicstatus %status (song playlistlength state)
	 (mutex-lock! %amutex)
	 (cond
	    ((eq? state 'pause)
	     (mutex-unlock! %amutex)
	     (music-pause o))
	    ((pair? s)
	     (unless (integer? (car s))
		(bigloo-type-error "music-play ::alsamusic" 'int (car s)))
	     (unwind-protect
		(playlist-load! o (car s))
		(mutex-unlock! %amutex)))
	    ((and (>=fx song 0) (<fx song playlistlength))
	     (unwind-protect
		(playlist-load! o song)
		(mutex-unlock! %amutex)))))))

;*---------------------------------------------------------------------*/
;*    music-seek ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::alsamusic pos . song)
   (with-access::alsamusic o (%amutex %decoder %toseek)
      (with-lock %amutex
	 (lambda ()
	    (if (pair? song)
		(begin
		   (unless (integer? (car song))
		      (bigloo-type-error '|music-seek ::alsamusic| 'int (car song)))
		   (set! %toseek pos)
		   (music-play o song))
		(alsadecoder-seek %decoder pos))))))

;*---------------------------------------------------------------------*/
;*    mime-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (mime-type path)
   (cond
      ((string-suffix? ".mp3" path) "audio/mpeg")
      ((string-suffix? ".ogg" path) "application/ogg")
      ((string-suffix? ".flac" path) "application/x-flac")
      ((string-suffix? ".wav" path) "audio/x-wav")
      ((string-suffix? ".swf" path) "application/x-shockwave-flash")
      ((string-suffix? ".swfl" path) "application/x-shockwave-flash")
      (else "audio/binary")))

;*---------------------------------------------------------------------*/
;*    playlist-load! ...                                               */
;*---------------------------------------------------------------------*/
(define (playlist-load! o::alsamusic n)
   (with-access::alsamusic o (%playlist %status %decoder decoders)
      (when (and (>=fx n 0) (<fx n (length %playlist)))
	 (let* ((url (list-ref %playlist n))
		(mime (mime-type url)))
	    (with-access::musicstatus %status (song songid)
	       (set! song n)
	       (set! songid n))
	    (let loop ((decoders decoders))
	       (when (pair? decoders)
		  (let ((d (car decoders)))
		     (if (alsadecoder-can-play-type? d mime)
			 (let ((p (open-input-file url)))
			    (if (input-port? p)
				(begin
				   (play o d p)
				   (with-access::alsamusic o
					 (%loop-mutex %loop-condv)
				      (with-lock %loop-mutex
					 (lambda ()
					    (condition-variable-broadcast! %loop-condv)))))
				(error "alsamusic" "cannot open file" url)))
			 (loop (cdr decoders))))))))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::alsamusic)
   (with-access::alsamusic o (%amutex %acondv %buffer pcm)
      (with-lock %amutex
	 (lambda ()
	    (when (alsabuffer? %buffer)
	       (with-access::alsabuffer %buffer (state)
		  (if (eq? state 'pause)
		      (begin
			 (set! state 'play)
			 (condition-variable-broadcast! %acondv))
		      (set! state 'pause))))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::alsamusic)
   (with-access::alsamusic o (%amutex pcm)
      (with-lock %amutex
	 (lambda ()
	    (stop o)))))

;*---------------------------------------------------------------------*/
;*    play ...                                                         */
;*---------------------------------------------------------------------*/
(define (play o::alsamusic d::alsadecoder p::input-port)
   (with-access::alsamusic o (%amutex %status %buffer inbuf outbuf mkthread)
      (stop o)
      (alsadecoder-reset! d)
      (set! %buffer
	 (instantiate::alsabuffer
	    (port p)
	    (inbuf inbuf)))
      (thread-start!
	 (mkthread
	    (lambda ()
	       (alsabuffer-fill! %buffer (string-length outbuf)))))
      (thread-start!
	 (mkthread
	    (lambda ()
	       (with-handler
		  (lambda (e)
		     (exception-notify e)
		     (mutex-lock! %amutex)
		     (musicstatus-state-set! %status 'error)
		     (musicstatus-err-set! %status e)
		     (mutex-unlock! %amutex))
		  (alsadecoder-decode d o %buffer)))))))

;*---------------------------------------------------------------------*/
;*    stop ...                                                         */
;*---------------------------------------------------------------------*/
(define (stop o::alsamusic)
   (with-access::alsamusic o (%amutex %buffer)
      (when (alsabuffer? %buffer)
	 (with-access::alsabuffer %buffer (mutex eof port state)
	    (mutex-lock! mutex)
	    (set! eof #t)
	    (set! state 'stop)
	    (close-input-port port)
	    (mutex-unlock! mutex)))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-fill! buffer::alsabuffer outlen::long)
   (with-access::alsabuffer buffer (mutex condv port inbuf inoff outoff eof count)
      (let ((inlen (string-length inbuf)))
	 ;; fill the buffer and enter the loop
	 (flush-output-port (current-error-port))
	 (let loop ((i (read-fill-string! inbuf 0 (minfx inlen (*fx 4 outlen)) port)))
	    (flush-output-port (current-error-port))
	    (mutex-lock! mutex)
	    (if (eof-object? i)
		(begin
		   (set! eof #t)
		   (mutex-unlock! mutex))
		(let ((c count))
		   (set! count (+fx count i))
		   (set! inoff (+fx inoff i))
		   (when (=fx inoff inlen) (set! inoff 0))
		   (when (>fx debug-buffer 2)
		      (tprint "fill: c=" c " count=" count " inlen=" inlen))
		   (when (and (=fx c 0) (>fx i 0))
		      ;; non empty buffer
		      (when (>fx debug-buffer 1)
			 (tprint "fill: signal not empty"))
		      (condition-variable-signal! condv))
		   (when (=fx count inlen)
		      ;; buffer full
		      (when (>fx debug-buffer 2)
			 (tprint "fill: wait full"))
		      (condition-variable-wait! condv mutex))
		   (let ((sz (minfx outlen (-fx inlen count))))
		      (mutex-unlock! mutex)
		      (let ((i (read-fill-string! inbuf inoff sz port)))
			 (when (>fx debug-buffer 0)
			    (tprint "fill, write: sz=" sz " -> i=" i))
			 (loop i)))))))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::alsadecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-decode d::alsadecoder o::alsamusic b::alsabuffer))

;*---------------------------------------------------------------------*/
;*    decode ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode
		  decoder::alsadecoder-host
		  o::alsamusic
		  buffer::alsabuffer)
   (with-access::alsamusic o (pcm %amutex %acondv outbuf %status %toseek)
      (with-access::alsabuffer buffer (mutex condv inbuf inoff outoff eof state count)
	 (let ((outlen (string-length outbuf))
	       (inlen (string-length inbuf)))
	    (let loop ()
	       (mutex-lock! mutex)
	       (case state
		  ((stop)
		   (when (eq? (alsa-snd-pcm-get-state pcm) 'running)
		      (alsa-snd-pcm-reset pcm))
		   (mutex-lock! %amutex)
		   (with-access::musicstatus %status (state songpos songlength)
		      (set! state 'stop)
		      (set! songpos 0)
		      (set! songlength 0))
		   (mutex-unlock! %amutex)
		   (mutex-unlock! mutex))
		  ((pause)
		   (mutex-lock! %amutex)
		   (mutex-unlock! mutex)
		   (musicstatus-state-set! %status 'pause)
		   (condition-variable-wait! %acondv %amutex)
		   (mutex-unlock! %amutex)
		   (loop))
		  (else
		   ;; get new bytes from the buffer
		   (let ((sz (let liip ()
				(cond
				   ((>fx count 0)
				    (when (=fx count inlen)
				       (when (>fx debug-decode 2)
					  (tprint "decode: signal not full"))
				       (condition-variable-signal! condv))
				    (minfx outlen count))
				   (eof 0)
				   (else
				    (when (>fx debug-decode 0)
				       (tprint "decode: wait empty"))
				    (condition-variable-wait! condv mutex)
				    (liip))))))
		      (let ((o outoff))
			 (set! count (-fx count sz))
			 (set! outoff (+fx outoff sz))
			 (when (=fx outoff inlen) (set! outoff 0))
			 (when (>fx debug-decode 1)
			    (tprint "decode: count=" count " sz=" sz))
			 (mutex-unlock! mutex)
			 (multiple-value-bind (status size rate channels encoding)
			    (alsadecoder-decode-buffer decoder inbuf o sz outbuf)
			    (when (eq? status 'new-format)
			       (alsa-snd-pcm-set-params! pcm
				  :format encoding
				  :access 'rw-interleaved
				  :channels channels
				  :rate rate
				  :soft-resample 1
				  :latency 500000)
			       (alsa-snd-pcm-hw-set-params! pcm :channels channels
				  :format encoding
				  :rate-near rate
				  :buffer-size-near (/fx rate 2)
				  :period-size-near (/fx rate 8)))
			    (when (>fx debug-decode 2)
			       (tprint "decode, alsa-write: sz=" sz
				  " size=" size " status="
				  status))
			    (when (>fx size 0)
			       (alsa-snd-pcm-write pcm outbuf size))
			    (cond
			       ((eq? status 'ok)
				(mutex-lock! %amutex)
				(with-access::musicstatus %status
				      (state songpos songlength)
				   (set! songpos (alsadecoder-position decoder inbuf))
				   (when (<fx songlength songpos)
				      (set! songlength songpos))
				   (set! state 'play))
				(mutex-unlock! %amutex)
				(loop))
			       ((eq? status 'new-format)
				(mutex-lock! %amutex)
				(with-access::musicstatus %status
				      (state songpos songlength bitrate khz)
				   (set! songpos (alsadecoder-position decoder inbuf))
				   (set! songlength 0)
				   (when (<fx songlength songpos)
				      (set! songlength songpos))
				   (multiple-value-bind (bitrate rate)
				      (alsadecoder-info decoder)
				      (set! bitrate bitrate)
				      (set! khz rate))
				   (when (>fx %toseek 0)
				      (alsadecoder-seek decoder %toseek)
				      (set! songpos %toseek)
				      (set! %toseek 0))
				   (set! state 'play))
				(mutex-unlock! %amutex)
				(loop))
			       ((or (eq? status 'done)
				    (and (=fx sz 0) (=fx size 0))
				    (and (eq? status 'need-more) eof))
				(mutex-lock! %amutex)
				(musicstatus-state-set! %status 'ended)
				(mutex-unlock! %amutex))
			       ((eq? status 'need-more)
				(loop))
			       (else
				(musicstatus-state-set! %status 'error)
				(musicstatus-err-set! %status "decoding error")))))))))))))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::alsamusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::alsamusic)
   (with-access::alsamusic o (%volume)
      %volume))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::alsamusic vol)
   (with-access::alsamusic o (%volume decoders)
      (for-each (lambda (d) (alsadecoder-volume-set! d vol)) decoders)
      (set! %volume vol)))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::alsadecoder ...                               */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-init o::alsadecoder)
   o)

;*---------------------------------------------------------------------*/
;*    alsadecoder-reset! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-reset! o::alsadecoder)
   #f)

;*---------------------------------------------------------------------*/
;*    alsadecoder-close ::alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-close o::alsadecoder))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-can-play-type? ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-can-play-type? o::alsadecoder mime::bstring)
   #t)

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode-buffer ::alsadecoder ...                      */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-decode-buffer o::alsadecoder inbuf offset insz outbuf))

;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::alsadecoder ...                           */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-position o::alsadecoder inbuf))

;*---------------------------------------------------------------------*/
;*    alsadecoder-info ::alsadecoder ...                               */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-info o::alsadecoder))

;*---------------------------------------------------------------------*/
;*    alsadecoder-seek ::alsadecoder ...                               */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-seek o::alsadecoder ms::long))

;*---------------------------------------------------------------------*/
;*    alsadecoder-volume-set! ::alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-volume-set! o::alsadecoder v::long))
   
