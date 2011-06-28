;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/music.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Tue Jun 28 11:41:11 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A (multimedia) music player.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_music
   
   (library multimedia pthread)
   
   (import  __alsa_alsa
	    __alsa_pcm)
   
   (export (class alsamusic::music
	      (%decoder (default #f))
	      (%playlist::pair-nil (default '()))
	      (%thread::obj (default #f))
	      (%thmutex::mutex read-only (default (make-mutex)))
	      (%thcondv::condvar read-only (default (make-condition-variable)))
	      (%port::obj (default #f))
	      (%toseek::long (default 0))
	      (%volume::long (default 100))
	      (inbuf::bstring read-only (default (make-string 16384)))
	      (outbuf::bstring read-only (default (make-string 16384)))
	      (pcm::alsa-snd-pcm read-only (default (instantiate::alsa-snd-pcm)))
	      (decoders::pair read-only))

	   (class alsadecoder
	      (alsadecoder-init))

	   (generic alsadecoder-init ::alsadecoder)
	   (generic alsadecoder-reset! ::alsadecoder)
	   (generic alsadecoder-close ::alsadecoder)
	   (generic alsadecoder-can-play-type? ::alsadecoder ::bstring)
	   (generic alsadecoder-decode ::alsadecoder ::bstring ::long ::bstring)
	   (generic alsadecoder-position::long ::alsadecoder ::bstring)
	   (generic alsadecoder-info::long ::alsadecoder)
	   (generic alsadecoder-seek::long ::alsadecoder ::long)
	   (generic alsadecoder-volume-set! ::alsadecoder ::long)))

;*---------------------------------------------------------------------*/
;*    music-init ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-init o::alsamusic)
   (with-access::alsamusic o (%thread %thmutex %status)
      (with-lock %thmutex
	 (lambda ()
	    (musicstatus-state-set! %status 'init)
	    (unless (thread? %thread)
	       (set! %thread (make-alsamusic-thread o))
	       (thread-start! %thread))))))

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
   (with-access::alsamusic o (pcm %thmutex %status)
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
   (with-access::alsamusic o (%mutex %playlist %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist (append! %playlist (list s)))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (+fx 1 playlistlength)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::alsamusic ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! o::alsamusic n)
   (with-access::alsamusic o (%mutex %playlist %status)
      (with-lock %mutex
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
   (with-access::alsamusic o (%mutex %playlist %status)
      (with-lock %mutex
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
   (with-access::alsamusic o (%thmutex %status %volume)
      (with-lock %thmutex
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
   (with-access::alsamusic o (%mutex %status %playlist)
      (with-access::musicstatus %status (song playlistlength)
	 (cond
	    ((pair? s)
	     (unless (integer? (car s))
		(bigloo-type-error "music-play ::alsamusic" 'int (car s)))
	     (playlist-load! o (car s)))
	    ((and (>=fx song 0) (<fx song playlistlength))
	     (playlist-load! o song))))))

;*---------------------------------------------------------------------*/
;*    music-seek ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::alsamusic pos . song)
   (with-access::alsamusic o (%mutex %decoder %toseek)
      (with-lock %mutex
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
   (with-access::alsamusic o (%mutex %playlist %status %decoder decoders)
      (with-lock %mutex
	 (lambda ()
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
			       (loop (cdr decoders))))))))))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::alsamusic)
   (with-access::alsamusic o (%thmutex %thcondv %status)
      (with-lock %thmutex
	 (lambda ()
	    (if (eq? (musicstatus-state %status) 'pause)
		(begin
		   (musicstatus-state-set! %status 'play)
		   (condition-variable-broadcast! %thcondv))
		(musicstatus-state-set! %status 'pause))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::alsamusic)
   (with-access::alsamusic o (%thmutex)
      (with-lock %thmutex
	 (lambda ()
	    (stop o)))))

;*---------------------------------------------------------------------*/
;*    stop ...                                                         */
;*---------------------------------------------------------------------*/
(define (stop o::alsamusic)
   (with-access::alsamusic o (%thmutex %port %status %decoder pcm)
      (when (input-port? %port)
	 (close-input-port %port)
	 (alsa-snd-pcm-drop pcm)
	 (alsa-snd-pcm-reset pcm)
	 (alsa-snd-pcm-prepare pcm)
	 (alsa-snd-pcm-start pcm)
	 (alsadecoder-reset! %decoder)
	 (set! %port #f))))

;*---------------------------------------------------------------------*/
;*    play ...                                                         */
;*---------------------------------------------------------------------*/
(define (play o::alsamusic d::alsadecoder p::input-port)
   (with-access::alsamusic o (%thmutex %thcondv %status %port %decoder %status)
      (let loop ()
	 (mutex-lock! %thmutex)
	 (if (eq? (musicstatus-state %status) 'init)
	     (begin
		(mutex-unlock! %thmutex)
		(sleep 1000000)
		(loop))
	     (begin
		(stop o)
		(set! %port p)
		(set! %decoder d)
		(musicstatus-songpos-set! %status 0)
		(condition-variable-signal! %thcondv)
		(mutex-unlock! %thmutex))))))

;*---------------------------------------------------------------------*/
;*    decode ...                                                       */
;*---------------------------------------------------------------------*/
(define (decode o::alsamusic)
   (with-access::alsamusic o (pcm %thmutex %thcondv %port %decoder inbuf outbuf %status %toseek)
      (with-handler
	 (lambda (e)
	    (exception-notify e)
	    (mutex-lock! %thmutex)
	    (musicstatus-state-set! %status 'error)
	    (musicstatus-err-set! %status e)
	    (mutex-unlock! %thmutex))
	 (let loop ()
	    (mutex-lock! %thmutex)
	    (if (input-port? %port)
		(let ((sz (read-chars! inbuf (string-length inbuf) %port)))
		   (mutex-unlock! %thmutex)
		   (multiple-value-bind (status size rate channels encoding)
		      (alsadecoder-decode %decoder inbuf sz outbuf)
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
		      (when (>fx size 0)
			 (alsa-snd-pcm-write pcm outbuf size))
		      (mutex-lock! %thmutex)
		      (cond
			 ((eq? status 'ok)
			  (musicstatus-songpos-set! %status
			     (alsadecoder-position %decoder inbuf))
			  (musicstatus-state-set! %status 'play)
			  (mutex-unlock! %thmutex)
			  (loop))
			 ((eq? status 'new-format)
			  (musicstatus-songlength-set! %status
			     0)
			  (musicstatus-songpos-set! %status
			     (alsadecoder-position %decoder inbuf))
			  (multiple-value-bind (bitrate rate)
			     (alsadecoder-info %decoder)
			     (musicstatus-bitrate-set! %status bitrate)
			     (musicstatus-khz-set! %status rate))
			  (when (>fx %toseek 0)
			     (alsadecoder-seek %decoder %toseek)
			     (musicstatus-songpos-set! %status %toseek)
			     (set! %toseek 0))
			  (musicstatus-state-set! %status 'play)
			  (mutex-unlock! %thmutex)
			  (loop))
			 ((or (eq? status 'done) (and (=fx sz 0) (=fx size 0)))
			  (musicstatus-state-set! %status 'ended)
			  (mutex-unlock! %thmutex))
			 ((eq? (musicstatus-state %status) 'pause)
			  (condition-variable-wait! %thcondv %thmutex)
			  (mutex-unlock! %thmutex)
			  (loop))
			 ((eq? status 'need-more)
			  (mutex-unlock! %thmutex)
			  (loop))
			 (else
			  (musicstatus-state-set! %status 'error)
			  (musicstatus-err-set! %status "mpg123 decoding error")
			  (mutex-unlock! %thmutex)))))
		(begin
		   (musicstatus-state-set! %status 'stop)
		   (mutex-unlock! %thmutex)))))))

;*---------------------------------------------------------------------*/
;*    make-alsamusic-thread ...                                        */
;*---------------------------------------------------------------------*/
(define (make-alsamusic-thread o::alsamusic)
   (with-access::alsamusic o (%thread %thmutex %thcondv %status)
      (instantiate::pthread
	 (body (lambda ()
		  (mutex-lock! %thmutex)
		  (musicstatus-state-set! %status 'stop)
		  (let loop ()
		     (condition-variable-wait! %thcondv %thmutex)
		     (mutex-unlock! %thmutex)
		     (decode o)
		     (loop)))))))

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
;*    alsadecoder-decode ::alsadecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-decode o::alsadecoder inbuf insz outbuf))

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
   
