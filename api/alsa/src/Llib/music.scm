;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/music.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Tue Jul 12 17:07:17 2011 (serrano)                */
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

   (export  (class alsamusic::music
	       (%decoder (default #f))
	       (%playlist::pair-nil (default '()))
	       (%thread::obj (default #f))
	       (%amutex::mutex read-only (default (make-mutex)))
	       (%acondv::condvar read-only (default (make-condition-variable)))
	       (%toseek::long (default 0))
	       (%buffer::obj (default #f))
	       (mkthread::procedure read-only)
	       (inbuf::bstring read-only (default (make-string (*fx 64 1024))))
	       (outbuf::bstring read-only (default (make-string (+fx 4 (*fx 32 1024)))))
	       (pcm::alsa-snd-pcm read-only (default (instantiate::alsa-snd-pcm)))
	       (decoders::pair read-only))
	    
	    (class alsabuffer
	       (mutex::mutex read-only (default (make-mutex)))
	       (condv::condvar read-only (default (make-condition-variable)))
	       (condvs::condvar read-only (default (make-condition-variable)))
	       (astate::symbol (default 'ready))
	       (inbuf::bstring read-only)
	       (outbuf::string (default ""))
	       (inoff::long (default 0))
	       (outoff::long (default 0))
	       (port::input-port read-only)
	       (count::long (default 0))
	       (eof::bool (default #f)))
	    
	    (class alsadecoder
	       (alsadecoder-init))
	    
	    (class alsadecoder-host::alsadecoder)
	    (class alsadecoder-client::alsadecoder)
	    
	    (generic alsabuffer-fill! ::alsabuffer ::long)
	    (generic alsabuffer-blit-string!::long ::alsabuffer ::long ::long)
	    
	    (generic alsadecoder-init ::alsadecoder)
	    (generic alsadecoder-reset! ::alsadecoder)
	    (generic alsadecoder-close ::alsadecoder)
	    (generic alsadecoder-can-play-type? ::alsadecoder ::bstring)
	    (generic alsadecoder-decode ::alsadecoder ::alsamusic ::alsabuffer)
	    
	    (generic alsadecoder-position::long ::alsadecoder ::bstring)
	    (generic alsadecoder-info::long ::alsadecoder)
	    (generic alsadecoder-seek::long ::alsadecoder ::long)
	    (generic alsadecoder-volume-set! ::alsadecoder ::long)
	    
	    (generic alsadecoder-host-decode-buffer ::alsadecoder-host
	       ::bstring ::long ::long ::bstring)))

;*---------------------------------------------------------------------*/
;*    debug                                                            */
;*---------------------------------------------------------------------*/
(define debug-buffer 0)
(define debug-decode 0)

;*---------------------------------------------------------------------*/
;*    music-init ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-init o::alsamusic)
   (with-access::alsamusic o (%amutex %status inbuf outbuf)
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
	    (musicstatus-volume-set! %status 100)
	    (musicstatus-state-set! %status 'uninitialized)))))

;*---------------------------------------------------------------------*/
;*    music-close ::alsamusic ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-close o::alsamusic)
   (unless (music-closed? o)
      (with-access::alsamusic o (pcm decoders)
	 (unless (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	    (alsa-snd-pcm-close pcm)
	    (for-each alsadecoder-close decoders)))))

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
      (unless (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	 (alsa-snd-pcm-drop pcm)
	 (alsa-snd-pcm-reset pcm)
	 (musicstatus-state-set! %status 'stop)))
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
   (with-access::alsamusic o (%amutex %status)
      (with-lock %amutex
	 (lambda ()
	    (musicstatus-volume-set! status (musicstatus-volume %status))
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
		(begin
		   (stop-sans-lock o)
		   (playlist-play! o (car s)))
		(mutex-unlock! %amutex)))
	    ((and (>=fx song 0) (<fx song playlistlength))
	     (unwind-protect
		(begin
		   (stop-sans-lock o)
		   (playlist-play! o song))
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
;*    playlist-play! ...                                               */
;*---------------------------------------------------------------------*/
(define (playlist-play! o::alsamusic n)
   
   (define (play o::alsamusic d::alsadecoder p::input-port)
      (alsadecoder-reset! d)
      (with-access::alsamusic o (%amutex %buffer inbuf outbuf mkthread)
	 (let ((buffer (instantiate::alsabuffer
			  (port p)
			  (inbuf inbuf))))
	    (set! %buffer buffer)
	    (thread-start!
	       (mkthread
		  (lambda ()
		     (alsabuffer-fill! buffer (string-length outbuf)))))
	    (thread-start!
	       (mkthread
		  (lambda ()
		     (with-handler
			(lambda (e)
			   (with-access::alsamusic o (%amutex %status)
			      (tprint "ERROR: " e)
			      (exception-notify e)
			      (mutex-lock! %amutex)
			      (musicstatus-state-set! %status 'error)
			      (musicstatus-err-set! %status e)
			      (mutex-unlock! %amutex)))
			(alsadecoder-decode d o buffer))))))))

   (with-access::alsamusic o (%playlist %status %decoder decoders pcm)
      (when (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	 (alsa-snd-pcm-open pcm))
      (when (and (>=fx n 0) (<fx n (length %playlist)))
	 (let* ((url (list-ref %playlist n))
		(mime (mime-type url)))
	    (with-access::musicstatus %status (state song songpos songlength songid)
	       (set! songpos 0)
	       (set! songlength 0)
	       (let loop ((decoders decoders))
		  (if (pair? decoders)
		      (let ((d (car decoders)))
			 (if (alsadecoder-can-play-type? d mime)
			     (let ((p (open-input-file url)))
				(if (input-port? p)
				    (begin
				       (set! state 'init)
				       (set! song n)
				       (set! songid n)
				       (play o d p)
				       (with-access::alsamusic o
					     (%loop-mutex %loop-condv)
					  (with-lock %loop-mutex
					     (lambda ()
						(condition-variable-broadcast! %loop-condv)))))
				    (error "alsamusic" "cannot open file" url)))
			     (loop (cdr decoders))))
		      (begin
			 (set! state 'skip)
			 (set! song n)
			 (set! songid n)))))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::alsamusic)
   (with-access::alsamusic o (%amutex pcm)
      (with-lock %amutex
	 (lambda ()
	    (unless (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	       (stop-sans-lock o))))))

;*---------------------------------------------------------------------*/
;*    stop-sans-lock ...                                               */
;*---------------------------------------------------------------------*/
(define (stop-sans-lock o::alsamusic)
   (with-access::alsamusic o (%amutex %buffer %status pcm)
      (when (alsabuffer? %buffer)
	 (with-access::alsabuffer %buffer (mutex eof port condv condvs astate)
	    (mutex-lock! mutex)
	    (if (eq? astate 'stopped)
		(mutex-unlock! mutex)
		(begin
		   (set! astate 'stop)
		   (condition-variable-signal! condv)
		   (condition-variable-wait! condvs mutex)
		   (mutex-unlock! mutex)
		   (with-access::musicstatus %status (state songpos songlength)
		      (set! state 'stop)
		      (set! songpos 0)
		      (set! songlength 0))
		   (set! %buffer #f)))))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::alsamusic)
   (with-access::alsamusic o (%buffer %amutex pcm)
      (with-lock %amutex
	 (lambda ()
	    (unless (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	       (when (alsabuffer? %buffer)
		  (with-access::alsabuffer %buffer (condv mutex astate)
		     (with-lock mutex
			(lambda ()
			   (if (eq? astate 'pause)
			       (begin
				  (set! astate 'play)
				  (condition-variable-broadcast! condv))
			       (set! astate 'pause)))))))))))

(define p (when (>fx debug-buffer 1) (open-output-file "/tmp/READ.log")))
(define p2 (when (>fx debug-buffer 1) (open-output-file "/tmp/READ.flac")))
(define p3 (when (>fx debug-buffer 1) (open-output-file "/tmp/READ.mp3")))

;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-fill! buffer::alsabuffer outlen::long)
   (with-access::alsabuffer buffer (mutex condv astate port inbuf inoff outoff eof count)
      (let ((inlen (string-length inbuf)))
	 ;; fill the buffer and enter the loop
	 (let loop ((i (read-fill-string! inbuf 0 (minfx inlen (*fx outlen 2)) port)))
	    (mutex-lock! mutex)
	    (if (or (eof-object? i) (eq? astate 'stopped))
		(begin
		   (set! eof #t)
		   (close-input-port port)
		   (condition-variable-signal! condv)
		   (mutex-unlock! mutex))
		(let ((c count))
		   (when (>fx debug-buffer 1)
		      (display-substring inbuf inoff (+fx inoff i) p)
		      (flush-output-port p))
		   (set! count (+fx count i))
		   (set! inoff (+fx inoff i))
		   (when (=fx inoff inlen) (set! inoff 0))
		   [assert (buffer inlen) (<fx (alsabuffer-inoff buffer) inlen)]
		   [assert (buffer inlen) (<=fx (alsabuffer-count buffer) inlen)]
		   (when (>fx debug-buffer 2)
		      (tprint "aslabuffer-fill.1: i=" i " count=" count " inoff=" inoff))
		   (when (and (=fx c 0) (>fx i 0))
		      ;; non empty buffer
		      (when (>fx debug-buffer 2)
			 (tprint "aslabuffer-fill.2: signal not empty count=" count))
		      (condition-variable-signal! condv))
		   (let waitfull ()
		      (when (=fx count inlen)
			 ;; buffer full
			 (when (>fx debug-buffer 2)
			    (tprint "aslabuffer-fill.3: wait full"))
			 (condition-variable-wait! condv mutex)
			 (waitfull)))
		   (let ((sz (minfx outlen (-fx inlen count))))
		      (when (>fx debug-buffer 1)
			 (tprint "aslabuffer-fill.4: inoff=" inoff " sz=" sz " count=" count))
		      (mutex-unlock! mutex)
		      (loop (read-fill-string! inbuf inoff sz port)))))))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-blit-string! ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-blit-string! buffer::alsabuffer o::long outlen::long)
   (with-access::alsabuffer buffer (mutex condv inbuf inoff outoff eof count outbuf)
      (let ((inlen (string-length inbuf)))
	 (mutex-lock! mutex)
	 (let ((sz (let waitempty ()
		      (cond
			 ((>fx count 0)
			  (minfx outlen (minfx count (-fx inlen outoff))))
			 (eof 0)
			 (else
			  (when (>fx debug-decode 3)
			     (tprint "alsabuffer-blit-string.1: wait empty count=" count
				" inoff=" inoff " outoff=" outoff))
			  (condition-variable-wait! condv mutex)
			  (waitempty))))))
	    (when (>fx sz 0)
	       (when (>fx debug-buffer 2)
		  (tprint "alsabuffer-blit-string.2: count="
		     count " outoff=" outoff " sz=" sz))
	       (when (>fx debug-buffer 1)
		  (display-substring inbuf outoff (+fx outoff sz) p2)
		  (flush-output-port p2))
	       ($snd-blit-string! inbuf outoff outbuf o sz)
	       (set! outoff (+fx outoff sz))
	       (when (=fx outoff inlen) (set! outoff 0))
	       (when (=fx count inlen)
		  (when (>fx debug-decode 3)
		     (tprint "alsabuffer-blit-string.3: signal not full"))
		  (condition-variable-signal! condv))
	       (set! count (-fx count sz)))
	    (mutex-unlock! mutex)
	    sz))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::alsadecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-decode d::alsadecoder o::alsamusic b::alsabuffer))

;*---------------------------------------------------------------------*/
;*    decode ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode
		  dec::alsadecoder-host
		  am::alsamusic
		  buffer::alsabuffer)

   (define (pcm-reset pcm)
      (let loop ()
	 (let ((state (alsa-snd-pcm-get-state pcm)))
	    (case state
	       ((open prepared)
		#f)
	       ((setup)
		(alsa-snd-pcm-prepare pcm)
		(tprint "pcm-reset.setup state=" state)
		(loop))
	       ((xrun)
		(alsa-snd-pcm-drop pcm)
		(tprint "pcm-reset.xrun state=" state)
		(loop))
	       (else
		(unless (eq? state 'running)
		   (tprint "pcm-reset state=" state))
		(with-handler
		   (lambda (e) #f)
		   (alsa-snd-pcm-wait pcm 200))
		(loop))))))
   
   (define (alsadecoder-do-stop dec am buffer)
      (with-access::alsabuffer buffer (mutex astate condvs)
	 (with-lock mutex
	    (lambda ()
	       (with-access::alsamusic am (%status %amutex pcm)
		  (let ((pcm-state (alsa-snd-pcm-get-state pcm)))
		     (when (memq pcm-state '(running prepared))
			(alsa-snd-pcm-drop pcm)))
		  (pcm-reset pcm)
		  (set! astate 'stopped)
		  (condition-variable-signal! condvs))))))
   
   (define (alsadecoder-do-pause dec am buffer)
      (with-access::alsamusic am (%status %amutex)
	 (with-access::alsabuffer buffer (condv mutex astate)
	    (mutex-lock! mutex)
	    (let loop ()
	       (if (eq? astate 'pause)
		   (begin
		      (mutex-lock! %amutex)
		      (musicstatus-state-set! %status 'pause)
		      (mutex-unlock! %amutex)
		      (condition-variable-wait! condv mutex)
		      (loop))
		   (mutex-unlock! mutex))))))
   
   (define (alsadecoder-play dec am buffer)
      (with-access::alsamusic am (%status %amutex)
	 (mutex-unlock! %amutex)
	 (with-access::musicstatus %status (state songpos songlength)
	    (with-access::alsabuffer buffer (inbuf)
	       (set! songpos (alsadecoder-position dec inbuf))
	       (when (<fx songlength songpos)
		  (set! songlength songpos))
	       (set! state 'play)))
	 (mutex-unlock! %amutex)))

   (define (alsadecoder-new-format dec am buffer)
      (with-access::alsamusic am (%status %amutex %toseek)
	 (mutex-lock! %amutex)
	 (with-access::musicstatus %status (state songpos songlength bitrate khz)
	    (with-access::alsabuffer buffer (inbuf)
	       (set! songpos (alsadecoder-position dec inbuf)))
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
	       (set! %toseek 0))
	    (set! state 'play))
	 (mutex-unlock! %amutex)))

   (define (alsadecoder-done dec am buffer)
      (with-access::alsamusic am (pcm %amutex %status)
	 ;; wait for the PCM interface to be done
	 (pcm-reset pcm)
	 (mutex-lock! %amutex)
	 (musicstatus-state-set! %status 'ended)
	 (mutex-unlock! %amutex)
	 (with-access::alsabuffer buffer (mutex astate)
	    (mutex-lock! mutex)
	    (set! astate 'stopped)
	    (mutex-unlock! mutex))))

   (define (alsadecoder-error dec am buffer)
      (with-access::alsamusic am (%status %amutex)
	 (mutex-lock! %amutex)
	 (musicstatus-state-set! %status 'error)
	 (musicstatus-err-set! %status "decoding error")
	 (mutex-unlock! %amutex)))
   
   (with-access::alsamusic am (pcm %amutex %acondv outbuf %status)
      (with-access::alsabuffer buffer (mutex condv astate inbuf inoff outoff eof count)
	 (let ((outlen (string-length outbuf))
	       (inlen (string-length inbuf)))
	    (let loop ((sz 0))
	       (mutex-lock! mutex)
	       (set! outoff (+fx outoff sz))
	       (when (=fx outoff inlen) (set! outoff 0))
	       (when (and (>fx sz 0) (=fx count inlen))
		  (when (>fx debug-decode 3)
		     (tprint "decode: signal not full"))
		  (condition-variable-signal! condv))
	       (set! count (-fx count sz))
	       ;; get new bytes from the buffer
	       (let* ((o outoff)
		      (sz (let waitempty ()
			     (cond
				((>fx count 0)
				 (minfx outlen (minfx count (-fx inlen outoff))))
				((or eof (eq? astate 'stop)) 0)
				(else
				 (when (>fx debug-decode 3)
				    (tprint "decode: wait empty count=" count
				       " inoff=" inoff " outoff=" outoff))
				 (condition-variable-wait! condv mutex)
				 (waitempty))))))
		  (when (>fx debug-decode 1)
		     (tprint "decode: count=" count " sz=" sz
			" ratio=" (/fx (*fx count 100) inlen) "%"))
		  (mutex-unlock! mutex)
		  (let flush ((z sz))
		     (when (>fx debug-buffer 1)
			(display-substring inbuf o (+fx o z) p3)
			(flush-output-port p3))
		     (multiple-value-bind (status size rate channels encoding)
			(alsadecoder-host-decode-buffer dec inbuf o z outbuf)
			(when (>fx debug-decode 1)
			   (tprint "decode, alsa-write: sz=" sz
			      " size=" size " status="
			      status))
			;;(tprint "status=" status " sz=" sz " z=" z " size=" size)
			(case status
			   ((ok)
			    ;; this assumes that reading a point is atomic
			    ;; othwerwise should be
			    ;; (with-lock mutex (lambda () astate))
			    (case astate
			       ((stop)
				(alsadecoder-do-stop dec am buffer))
			       ((pause)
				(alsadecoder-do-pause dec am buffer)
				(flush 0))
			       (else
				(alsadecoder-play dec am buffer)
				(when (>fx size 0)
				   (alsa-snd-pcm-write pcm outbuf size))
				(flush 0))))
			   ((need-more)
			    (cond
			       ((>fx size 0)
				(alsa-snd-pcm-write pcm outbuf size)
				(flush 0))
			       ((and eof (=fx count 0))
				(alsadecoder-done dec am buffer))
			       (else
				(loop sz))))
			   ((new-format)
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
			       :period-size-near (/fx rate 8))
			    (alsadecoder-new-format dec am buffer)
			    (when (>fx size 0)
			       (alsa-snd-pcm-write pcm outbuf size))
			    (flush 0))
			   ((done)
			    (when (>fx size 0)
			       (alsa-snd-pcm-write pcm outbuf size))
			    (alsadecoder-done dec am buffer))
			   (else
			    (alsadecoder-error dec am buffer)))))))))))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::alsamusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::alsamusic)
   (with-access::alsamusic o (%status)
      (musicstatus-volume %status)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::alsamusic vol)
   (with-access::alsamusic o (decoders %status)
      (for-each (lambda (d) (alsadecoder-volume-set! d vol)) decoders)
      (musicstatus-volume-set! %status vol)))
   
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
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-host-decode-buffer ::alsadecoder ...                 */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-host-decode-buffer o::alsadecoder-host
		   inbuf offset insz outbuf))

;*---------------------------------------------------------------------*/
;*    alsadecoder-client-decode ::alsadecoder-client ...               */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-client-decode o::alsadecoder-client))
