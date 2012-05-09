;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/music.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Wed May  9 09:30:22 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
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

   (static  (class alsaportbuffer::alsabuffer
	       (port::input-port read-only)
	       (readsz::long read-only (default 8192))
	       (%inbuf::bstring read-only)
	       (%nexttail::long (default 0)))
	    
	    (class alsammapbuffer::alsabuffer
	       (mmap::mmap read-only)))
	    
   (export  (class alsamusic::music
	       (inbuf::bstring read-only (default (make-string (*fx 1024 1024))))
	       
	       (pcm::alsa-snd-pcm read-only (default (instantiate::alsa-snd-pcm)))
	       (decoders::pair-nil read-only (default '()))
	       (mkthread::procedure read-only (default make-thread))
	       (timeout (default (* 1000 1000 10)))
	       (%decoder (default #f))
	       (%buffer (default #f))
	       (%nextbuffer (default #f))
	       
	       (%playlist::pair-nil (default '()))
	       (%toseek::long (default -1))
	       (%aready::bool (default #t))
	       (%amutex::mutex read-only (default (make-mutex)))
	       (%!playid::int (default -1))
	       (%acondv::condvar read-only (default (make-condition-variable))))

	    (class alsabuffer
	       (url::bstring read-only)
	       (%eof::bool (default #f))
	       (%!babort::bool (default #f))
	       (%bcondv::condvar read-only (default (make-condition-variable)))
	       (%bmutex::mutex read-only (default (make-mutex)))
	       (%inlen::long read-only)
	       (%inbufp::string read-only)
	       (%head::long (default 0))
	       (%tail::long (default 0))
	       (%empty::bool (default #t))
	       (%filled::bool (default #f)))
	    
	    (class alsadecoder
	       (alsadecoder-init)
	       (mimetypes::pair-nil (default '()))
	       (buffer-time-near::int (default 500000))
	       (buffer-size-near-ratio::int (default 2))
	       (period-size-near-ratio::int (default 8))
	       (%!dpause::bool (default #f))
	       (%!dabort::bool (default #f))
	       (%!stop::bool (default #t))
	       (%!dseek::long (default -1))
	       (%dmutex::mutex read-only (default (make-mutex)))
	       (%dcondv::condvar read-only (default (make-condition-variable)))
	       (%doutcondv::condvar read-only (default (make-condition-variable))))
	    
	    (generic alsadecoder-init ::alsadecoder)
	    (generic alsadecoder-reset! ::alsadecoder)
	    (generic alsadecoder-close ::alsadecoder)
	    (generic alsadecoder-can-play-type? ::alsadecoder ::bstring)
	    (generic alsadecoder-decode ::alsadecoder ::alsamusic ::alsabuffer)

	    (generic alsabuffer-available::long ::alsabuffer)
	    
	    (generic alsadecoder-position::long ::alsadecoder ::alsabuffer)
	    (generic alsadecoder-info::long ::alsadecoder)
	    (generic alsadecoder-seek::long ::alsadecoder ::long)
	    (generic alsadecoder-volume-set! ::alsadecoder ::long)))

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (begin (bigloo-compiler-debug) 1))

;*---------------------------------------------------------------------*/
;*    alsa-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (alsa-debug)
   (when (>fx ($compiler-debug) 0)
      (bigloo-debug)))

;*---------------------------------------------------------------------*/
;*    music-init ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-init o::alsamusic)
   (with-access::alsamusic o (%amutex %status inbuf)
      (with-lock %amutex
	 (lambda ()
	    (with-access::musicstatus %status (volume state)
	       (set! volume 100)
	       (set! state 'uninitialized))))))

;*---------------------------------------------------------------------*/
;*    music-close ::alsamusic ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-close o::alsamusic)
   (with-access::alsamusic o (pcm decoders %amutex)
      (with-lock %amutex
	 (lambda ()
	    (unless (eq? (alsa-snd-pcm-get-state pcm) 'disconnected))
	       (alsa-snd-pcm-close pcm)))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::alsamusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::alsamusic)
   (with-access::alsamusic o (%amutex pcm)
      (with-lock %amutex
	 (lambda ()
	    (eq? (alsa-snd-pcm-get-state pcm) 'disconnected)))))

;*---------------------------------------------------------------------*/
;*    music-reset! ::alsamusic ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-reset! o::alsamusic)
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    music-status ::alsamusic ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-status o::alsamusic)
   (with-access::alsamusic o (%amutex %decoder %buffer %status)
      (with-lock %amutex
	 (lambda ()
	    (when (and (isa? %decoder alsadecoder)
		       (isa? %buffer alsabuffer))
	       (with-access::musicstatus %status (songpos)
		  (set! songpos (alsadecoder-position %decoder %buffer))))))
      %status))
      
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
	    (with-access::musicstatus %status (playlistlength song)
	       (set! song 0)
	       (set! playlistlength 0))))))

;*---------------------------------------------------------------------*/
;*    music-can-play-type? ::alsamusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-can-play-type? o::alsamusic mimetype::bstring)
   (with-access::alsamusic o (decoders)
      (any (lambda (d) (alsadecoder-can-play-type? d mimetype)) decoders)))
   
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
		(when (isa? %decoder alsadecoder)
		   (with-access::alsadecoder %decoder (%!dseek)
		      (set! %!dseek pos))))))))

;*---------------------------------------------------------------------*/
;*    open-file ...                                                    */
;*    -------------------------------------------------------------    */
;*    Open a timeouted file.                                           */
;*---------------------------------------------------------------------*/
(define (open-file url o::alsamusic)
   (with-access::alsamusic o (timeout)
      (open-input-file url #f timeout)))

;*---------------------------------------------------------------------*/
;*    music-play ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-play o::alsamusic . s)
   
   (define (find-decoder o url)
      (with-access::alsamusic o (decoders)
	 (let ((mime (mime-type url)))
	    (find (lambda (d) (alsadecoder-can-play-type? d mime))
	       decoders))))
   
   (define (update-song-status! o n)
      (with-access::alsamusic o (%status onstate onvolume)
	 (with-access::musicstatus %status (state song songpos songid songlength playlistid volume)
	    (set! songpos 0)
	    (set! songlength 0)
	    (set! song n)
	    (set! songid (+fx (* 100 playlistid) n))
	    (set! state 'play)
	    (onstate o %status)
	    (onvolume o volume))))
   
   (define (pcm-init o)
      (with-access::alsamusic o (pcm)
	 (when (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	    (alsa-snd-pcm-open pcm))))

   (define (pcm-reset! o)
      (with-access::alsamusic o (pcm)
	 (let ((pcm-state (alsa-snd-pcm-get-state pcm)))
	    (unless (eq? pcm-state 'not-open)
	       (when (memq pcm-state '(running prepared))
		  (alsa-snd-pcm-drop pcm))
	       (alsa-snd-pcm-cleanup pcm)))))

   (define (prepare-next-buffer o buffer playlist::pair-nil)
      (when (pair? playlist)
	 (with-access::alsaportbuffer buffer (%head %tail %inlen %inbuf %inbufp)
	    (with-access::alsamusic o (%amutex %nextbuffer %status)
	       (with-lock %amutex
		  (lambda ()
		     (unless %nextbuffer
			(let* ((url (car playlist))
			       (ip (open-file url o)))
			   (when (input-port? ip)
			      (let ((buf (instantiate::alsaportbuffer
					    (url url)
					    (port ip)
					    (%inlen %inlen)
					    (%inbuf %inbuf)
					    (%inbufp %inbufp)
					    (%head %head)
					    (%tail %tail)
					    (%nexttail %head))))
				 (set! %nextbuffer buf)
				 buf))))))))))
	 
   (define (play-url-port o d::alsadecoder url::bstring
	      playlist::pair-nil notify::bool)
      (let ((ip (open-file url o)))
	 (if (input-port? ip)
	     (with-access::alsamusic o (%amutex inbuf %buffer onevent
					  mkthread %status)
		(let ((buffer (instantiate::alsaportbuffer
				 (url url)
				 (port ip)
				 (%inlen (string-length inbuf))
				 (%inbuf inbuf)
				 (%inbufp inbuf))))
		   (set! %buffer buffer)
		   (mutex-unlock! %amutex)
		   (thread-start!
		      (mkthread
			 (lambda ()
			    (let loop ((buffer buffer)
				       (playlist playlist))
			       (unwind-protect
				  (alsabuffer-fill! buffer o)
				  (close-input-port ip))
			       (let ((nbuffer (prepare-next-buffer o buffer (cdr playlist))))
				  (when (isa? nbuffer alsabuffer)
				     (loop nbuffer (cdr playlist))))))
			 "alsamusic-buffer"))
		   (when notify
		      (with-access::musicstatus %status (playlistid)
			 (onevent o 'playlist playlistid)))
		   (alsadecoder-reset! d)
		   (with-handler
		      (lambda (e)
			 (exception-notify e)
			 (raise e))
		      (alsadecoder-decode d o buffer))))
	     (with-access::alsamusic o (onerror %amutex)
		(mutex-unlock! %amutex)
		(onerror o
		   (instantiate::&io-port-error
		      (proc "music-play")
		      (msg "Cannot open")
		      (obj url)))))))

   (define (play-url-mmap o d::alsadecoder url::bstring
	      playlist::pair-nil notify::bool)
      (let ((mmap (open-mmap url :read #t :write #f)))
	 (if (mmap? mmap)
	     (with-access::alsamusic o (%amutex inbuf %buffer onevent %status)
		(let ((buffer (instantiate::alsammapbuffer
				 (url url)
				 (mmap mmap)
				 (%inlen (elong->fixnum (mmap-length mmap)))
				 (%inbufp (mmap->string mmap)))))
		   (set! %buffer buffer)
		   (mutex-unlock! %amutex)
		   (alsabuffer-fill! buffer o)
		   (when notify
		      (with-access::musicstatus %status (playlistid)
			 (onevent o 'playlist playlistid)))
		   (alsadecoder-reset! d)
		   (alsadecoder-decode d o buffer)
		   (close-mmap mmap)))
	     (with-access::alsamusic o (onerror %amutex)
		(mutex-unlock! %amutex)
		(onerror o
		   (instantiate::&io-port-error
		      (proc "music-play")
		      (msg "Cannot open")
		      (obj url)))))))

   (define (play-url-next o d::alsadecoder url::bstring playlist)
      (with-access::alsamusic o (%amutex %buffer %nextbuffer)
	 (with-access::alsaportbuffer %nextbuffer (%nexttail %tail %head)
	    (set! %tail %nexttail))
	 (set! %buffer %nextbuffer)
	 (set! %nextbuffer #f)
	 (mutex-unlock! %amutex)
	 (alsadecoder-reset! d)
	 (alsadecoder-decode d o %buffer)))

   (define (next-buffer? url)
      (with-access::alsamusic o (%nextbuffer)
	 (when (isa? %nextbuffer alsabuffer)
	    (with-access::alsabuffer %nextbuffer ((u url))
	       (or (eq? u url)
		   (begin
		      (alsabuffer-abort! %nextbuffer)
		      #f))))))
   
   (define (play-url o d::alsadecoder url::bstring playlist notify)
      (cond
	 ((next-buffer? url) (play-url-next o d url playlist))
	 ((file-exists? url) (play-url-mmap o d url playlist notify))
	 (else (play-url-port o d url playlist notify))))
   
   (define (play-urls urls n)
      (with-access::alsamusic o (%amutex %!playid onerror %decoder %toseek)
	 (let ((playid %!playid))
	    (let loop ((l urls)
		       (n n)
		       (notify #t))
	       (when (and (eq? playid %!playid) (pair? l))
		  (let* ((url (car l))
			 (decoder (find-decoder o url)))
		     (if decoder
			 (with-access::alsadecoder decoder (%!dseek)
			    (set! %!dseek %toseek)
			    (set! %toseek -1)
			    (set! %decoder decoder)
			    (update-song-status! o n)
			    ;; play-url unlocks %amutex
			    (play-url o decoder url l notify)
			    (mutex-lock! %amutex)
			    (loop (cdr l) (+fx 1 n) #f))
			 (begin
			    (mutex-unlock! %amutex)
			    (onerror o (format "Illegal format \"~a\"" url))
			    (mutex-lock! %amutex)))))))))

   (define (play-playlist n)
      ;; start playing the playlist
      (with-access::alsamusic o (%playlist %aready %!playid)
	 (let ((playlist %playlist))
	    (when (and (>=fx n 0) (<fx n (length playlist)))
	       ;; init alsa pcm
	       (pcm-init o)
	       ;; wait the the music player to be ready
	       (set! %!playid (+fx 1 %!playid))
	       (let ((playid %!playid))
		  (alsamusic-wait-ready! o)
		  (when (=fx %!playid playid)
		     (set! %aready #f)
		     ;; play the list of urls
		     (play-urls (list-tail playlist n) n)))))))

   (define (resume-from-pause o)
      (with-access::alsamusic o (%decoder)
	 (when (isa? %decoder alsadecoder)
	    (with-access::alsadecoder %decoder (%dmutex %dcondv %!dpause)
	       (mutex-lock! %dmutex)
	       (when %!dpause
		  (set! %!dpause #f)
		  (condition-variable-signal! %dcondv))
	       (mutex-unlock! %dmutex)
	       #t))))
   
   (with-access::alsamusic o (%amutex %acondv
				%decoder %buffer %aready
				%status onerror)
      (with-handler
	 (lambda (e)
	    (mutex-lock! %amutex)
	    (with-access::musicstatus %status (state song)
	       (set! song (+fx 1 song))
	       (set! state 'error))
	    (mutex-unlock! %amutex)
	    (onerror o e)
	    (raise e))
	 (with-lock %amutex
	    (lambda ()
	       (unwind-protect
		  (cond
		     ((pair? s)
		      ;; play the playing from a user index
		      (unless (integer? (car s))
			 (bigloo-type-error "music-play ::alsamusic" 'int (car s)))
		      (play-playlist (car s)))
		     ((resume-from-pause o)
		      #unspecified)
		     (else
		      ;; play the playlist from the current position
		      (with-access::musicstatus %status (song)
			 (play-playlist song))))
		  (begin
		     ;; reset the player state
		     (set! %aready #t)
		     (set! %buffer #f)
		     (set! %decoder #f)
		     (pcm-reset! o)
		     ;; signal if someone waiting for the music player
		     (condition-variable-signal! %acondv))))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::alsamusic)
   (with-access::alsamusic o (%amutex onstate %status %!playid)
      (with-lock %amutex
	 (lambda ()
	    (set! %!playid (+fx 1 %!playid))
	    (alsamusic-wait-ready! o)))))

;*---------------------------------------------------------------------*/
;*    alsamusic-wait-ready! ...                                        */
;*---------------------------------------------------------------------*/
(define (alsamusic-wait-ready! o::alsamusic)
   ;; %amutex already locked
   (with-access::alsamusic o (%decoder %buffer %nextbuffer)
      (when (isa? %decoder alsadecoder)
	 (alsadecoder-abort! %decoder))
      (when (isa? %buffer alsabuffer)
	 (alsabuffer-abort! %buffer))
      (when (isa? %nextbuffer alsabuffer)
	 (alsabuffer-abort! %nextbuffer))
      (with-access::alsamusic o (%aready %acondv %amutex)
	 (unless %aready
	    (let loop ()
	       (unless %aready
		  ;; keep waiting
		  (condition-variable-wait! %acondv %amutex)
		  (loop))))
	 (set! %aready #t))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-abort! ...                                            */
;*---------------------------------------------------------------------*/
(define (alsabuffer-abort! b::alsabuffer)
   (with-access::alsabuffer b (%bmutex %bcondv %!babort)
      (mutex-lock! %bmutex)
      (set! %!babort #t)
      (condition-variable-broadcast! %bcondv)
      (mutex-unlock! %bmutex)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-abort! ...                                           */
;*---------------------------------------------------------------------*/
(define (alsadecoder-abort! d::alsadecoder)
   (with-access::alsadecoder d (%!dabort %!dpause %dmutex %dcondv)
      (mutex-lock! %dmutex)
      (set! %!dpause #f)
      (set! %!dabort #t)
      (condition-variable-signal! %dcondv)
      (mutex-unlock! %dmutex)))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::alsamusic)
   (with-access::alsamusic o (%amutex %decoder)
      (with-lock %amutex
	 (lambda ()
	    (when (isa? %decoder alsadecoder)
	       (alsadecoder-pause %decoder))))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-pause ...                                            */
;*---------------------------------------------------------------------*/
(define (alsadecoder-pause d::alsadecoder)
   (with-access::alsadecoder d (%dmutex %dcondv %!dpause)
      (mutex-lock! %dmutex)
      (if %!dpause
	  (begin
	     (set! %!dpause #f)
	     (condition-variable-signal! %dcondv))
	  (set! %!dpause #t))
      (mutex-unlock! %dmutex)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-fill! buffer::alsabuffer o::alsamusic))
   
;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-fill! buffer::alsaportbuffer o::alsamusic)
   (with-access::alsaportbuffer buffer (%bmutex %bcondv %!babort %head %tail %inbuf %inlen %eof %empty %filled readsz port url)
      
      (define inlen %inlen)

      (define (read-fill-string-debug! %inbuf %head sz port)
	 (tprint ">>> ALSA: read sz=" sz)
	 (let* ((d0 (current-microseconds))
		(r (read-fill-string! %inbuf %head sz port))
		(d1 (current-microseconds)))
	    (tprint "<<< ALSA: read " r "/" sz " (" (-llong d1 d0) "us)")
	    r))
      
      (define (timed-read sz)
	 (if (>=fx (alsa-debug) 4)
	     (read-fill-string-debug! %inbuf %head sz port)
	     (read-fill-string! %inbuf %head sz port)))

      (define (set-eof!)
	 (with-access::alsamusic o (onevent)
	    (mutex-lock! %bmutex)
	    (set! %eof #t)
	    (condition-variable-broadcast! %bcondv)
	    (mutex-unlock! %bmutex)
	    (onevent o 'loaded url)))

      (define (inc-head! i)
	 (let ((nhead (+fx %head i)))
	    (if (=fx nhead inlen)
		(set! %head 0)
		(set! %head nhead))
	    (when (or %empty (not %filled))
	       ;; buffer was empty
	       (mutex-lock! %bmutex)
	       (set! %empty #f)
	       (condition-variable-broadcast! %bcondv)
	       (mutex-unlock! %bmutex))))
      
      (define (abort)
	 (mutex-lock! %bmutex)
	 (condition-variable-broadcast! %bcondv)
	 (mutex-unlock! %bmutex))

      (with-handler
	 (lambda (e)
	    (exception-notify e)
	    (set! %!babort #t)
	    (set! %eof #t)
	    (abort)
	    (with-access::alsamusic o (onerror)
	       (onerror o e)
	       (sleep (*fx 1000 1000))))
	 (let loop ()
	    (cond
	       (%!babort
   	        ;;; external abort
		(abort))
	       (%eof
	        ;;; done looping
		#unspecified)
	       ((and (=fx %head %tail) (not %empty))
		;; buffer full
		(mutex-lock! %bmutex)
		(when (>=fx (alsa-debug) 3)
		   (tprint "!!! ALSA: wait buffer full url=" url))
		(condition-variable-wait! %bcondv %bmutex)
		(mutex-unlock! %bmutex)
		(loop))
	       (else
		(let* ((s (minfx readsz
			     (if (<fx %head %tail)
				 (-fx %tail %head)
				 (-fx inlen %head))))
		       (i (timed-read s)))
		   (cond
		      ((eof-object? i) (set-eof!))
		      ((>fx i 0) (inc-head! i)))
		   (loop))))))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-fill! buffer::alsammapbuffer o::alsamusic)
   (with-access::alsammapbuffer buffer (%head %empty %inbufp %eof mmap url %inlen)
      (set! %inbufp (mmap->string mmap))
      (set! %head 0)
      (set! %eof #t)
      (set! %empty (=fx %inlen 0))
      (with-access::alsamusic o (onevent)
	 (onevent o 'loaded url))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::alsadecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-decode d::alsadecoder o::alsamusic b::alsabuffer))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::alsamusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::alsamusic)
   (with-access::alsamusic o (%status)
      (with-access::musicstatus %status (volume)
	 volume)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::alsamusic ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::alsamusic vol)
   (with-access::alsamusic o (decoders %status onvolume)
      (for-each (lambda (d) (alsadecoder-volume-set! d vol)) decoders)
      (with-access::musicstatus %status (volume)
	 (set! volume vol)
	 (onvolume o vol))))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::alsadecoder ...                               */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-init o::alsadecoder)
   o)

;*---------------------------------------------------------------------*/
;*    alsadecoder-reset! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-reset! o::alsadecoder)
   (with-access::alsadecoder o (%dmutex %!dpause %!dabort)
      (mutex-lock! %dmutex)
      (set! %!dpause #f)
      (set! %!dabort #f)
      (mutex-unlock! %dmutex)
      #f))

;*---------------------------------------------------------------------*/
;*    alsadecoder-close ::alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-close o::alsadecoder))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-can-play-type? ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-can-play-type? o::alsadecoder mime::bstring)
   (with-access::alsadecoder o (mimetypes)
      (member mime mimetypes)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-available ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-available o::alsabuffer)
   (with-access::alsabuffer o (%head %tail %empty %inlen)
      (cond
	 ((>fx %head %tail) (-fx %head %tail))
	 ((<fx %head %tail) (+fx (-fx %inlen (-fx %tail 1)) %head))
	 (%empty 0)
	 (else %inlen))))
	 
;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::alsadecoder ...                           */
;*---------------------------------------------------------------------*/
(define-generic (alsadecoder-position o::alsadecoder b::alsabuffer))

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
;*    mime-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (mime-type path)
   
   (define (mime-type-file path)
      (cond
	 ((string-suffix? ".mp3" path) "audio/mpeg")
	 ((string-suffix? ".ogg" path) "application/ogg")
	 ((string-suffix? ".flac" path) "application/x-flac")
	 ((string-suffix? ".wav" path) "audio/x-wav")
	 ((string-suffix? ".swf" path) "application/x-shockwave-flash")
	 ((string-suffix? ".swfl" path) "application/x-shockwave-flash")
	 (else "audio/binary")))
   
   (if (and (string-prefix? "http" path)
	    (or (string-prefix? "http://" path)
		(string-prefix? "https://" path)))
       (let ((i (string-index-right path #\?)))
	  (if i
	      (mime-type (substring path 6 i))
	      (mime-type-file path)))
       (mime-type-file path)))
