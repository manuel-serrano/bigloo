;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/music.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Sat Mar 28 07:19:31 2015 (serrano)                */
;*    Copyright   :  2011-15 Manuel Serrano                            */
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
	       (%nexttail::long (default 0))
	       (%seek::elong (default #e-1)))
	    
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
	       (%aready::bool (default #t))
	       (%amutex::mutex read-only (default (make-mutex)))
	       (%!pid::int (default -1))
	       (%acondv::condvar read-only (default (make-condition-variable))))

	    (class alsabuffer
	       (url::bstring read-only)
	       (%eof::bool (default #f))
	       (%bcondv::condvar read-only (default (make-condition-variable)))
	       (%bmutex::mutex read-only (default (make-mutex)))
	       (%inlen::long read-only)
	       (%inbufp::string read-only)
	       (%head::long (default 0))
	       (%tail::long (default 0))
	       (%empty::bool (default #t)))
	    
	    (class alsadecoder
	       (alsadecoder-init)
	       (mimetypes::pair-nil (default '()))
	       (buffer-time-near::int (default 500000))
	       (buffer-size-near-ratio::int (default 2))
	       (period-size-near-ratio::int (default 8))
	       (%!dpause::bool (default #f))
	       (%!dabort::bool (default #f))
	       (%!stop::bool (default #t))
	       (%dmutex::mutex read-only (default (make-mutex)))
	       (%dcondv::condvar read-only (default (make-condition-variable)))
	       (%doutcondv::condvar read-only (default (make-condition-variable))))
	    
	    (generic alsadecoder-init ::alsadecoder)
	    (generic alsadecoder-reset! ::alsadecoder)
	    (generic alsadecoder-close ::alsadecoder)
	    (generic alsadecoder-can-play-type? ::alsadecoder ::bstring)
	    (generic alsadecoder-decode ::alsadecoder ::alsamusic ::alsabuffer)

	    (generic alsabuffer-stream::obj ::alsabuffer)
	    (generic alsabuffer-length::belong ::alsabuffer)
	    (generic alsabuffer-available::long ::alsabuffer)
	    (generic alsabuffer-tell::obj ::alsabuffer)
	    (generic alsabuffer-seek ::alsabuffer ::obj)
	    (generic alsabuffer-substring ::alsabuffer ::int ::int)
	    (generic alsabuffer-blit-string! ::alsabuffer ::int ::bstring ::int ::int)
	    
	    (generic alsadecoder-position::long ::alsadecoder ::alsabuffer)
	    (generic alsadecoder-info::long ::alsadecoder)
	    (generic alsadecoder-seek ::alsadecoder ::long)
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
   (if (>fx ($compiler-debug) 0)
       (bigloo-debug)
       0))

;*---------------------------------------------------------------------*/
;*    *error-sleep-duration* ...                                       */
;*---------------------------------------------------------------------*/
(define *error-sleep-duration*
   (* 2 1000 1000))

;*---------------------------------------------------------------------*/
;*    music-init ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-init o::alsamusic)
   (with-access::alsamusic o (%amutex %status inbuf)
      (synchronize %amutex
	 (with-access::musicstatus %status (volume state)
	    (set! volume 100)
	    (set! state 'uninitialized)))))

;*---------------------------------------------------------------------*/
;*    music-close ::alsamusic ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-close o::alsamusic)
   (with-access::alsamusic o (pcm decoders %amutex)
      (synchronize %amutex
	 (unless (eq? (alsa-snd-pcm-get-state pcm) 'disconnected)
	    (alsa-snd-pcm-close pcm)))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::alsamusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::alsamusic)
   (with-access::alsamusic o (%amutex pcm)
      (synchronize %amutex
	 (eq? (alsa-snd-pcm-get-state pcm) 'disconnected))))

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
      (synchronize %amutex
	 (when (and (isa? %decoder alsadecoder)
		    (isa? %buffer alsabuffer))
	    (with-access::musicstatus %status (songpos songlength)
	       (set! songpos (alsadecoder-position %decoder %buffer)))))
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
      (synchronize %amutex
	 (set! %playlist (append! %playlist (list s)))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (set! playlistid (+fx 1 playlistid))
	    (set! playlistlength (+fx 1 playlistlength))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::alsamusic ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! o::alsamusic n)
   (with-access::alsamusic o (%amutex %playlist %status)
      (synchronize %amutex
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (when (and (>=fx n 0) (<fx n playlistlength))
	       (set! %playlist (remq! (list-ref %playlist n) %playlist))
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (length %playlist)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::alsamusic ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! o::alsamusic)
   (with-access::alsamusic o (%amutex %playlist %status)
      (synchronize %amutex
	 (set! %playlist '())
	 (with-access::musicstatus %status (playlistlength song)
	    (set! song 0)
	    (set! playlistlength 0)))))

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
   (with-access::alsamusic o (%amutex %decoder %buffer)
      (synchronize %amutex
	 (if (pair? song)
	     (begin
		(unless (integer? (car song))
		   (bigloo-type-error '|music-seek ::alsamusic| 'int (car song)))
		(music-play o song))
	     (when (isa? %decoder alsadecoder)
		(alsadecoder-seek %decoder pos))))))

;*---------------------------------------------------------------------*/
;*    url-song-length ...                                              */
;*---------------------------------------------------------------------*/
(define (url-song-length url)
   (if (file-exists? url)
       (let ((info (file-musicinfo url)))
	  (if (isa? info musicinfo)
	      (with-access::musicinfo info (duration)
		 duration)
	      0))
       0))

;*---------------------------------------------------------------------*/
;*    open-file ...                                                    */
;*    -------------------------------------------------------------    */
;*    Open a timeouted file.                                           */
;*---------------------------------------------------------------------*/
(define (open-file url o::alsamusic)
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 #f)
      (with-access::alsamusic o (timeout %status)
	 (let ((pi (open-input-file url #f timeout)))
	    (when (input-port? pi)
	       (input-port-timeout-set! pi timeout))
	    pi))))

;*---------------------------------------------------------------------*/
;*    music-play ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-play o::alsamusic . s)

   (define playlist-ended-id #f)
   
   (define (find-decoder o url)
      (with-access::alsamusic o (decoders)
	 (let ((mime (mime-type url)))
	    (find (lambda (d) (alsadecoder-can-play-type? d mime))
	       decoders))))
   
   (define (update-song-status! o n pid url)
      (with-access::alsamusic o (%status)
	 (with-access::musicstatus %status (state song songpos songid songlength playlistid volume playlistid)
	    (set! playlistid pid)
	    (set! songpos 0)
	    (set! song n)
	    (set! songid (+fx (* 100 pid) n))
	    (set! songlength (url-song-length url))
	    (music-state-set! o 'play)
	    (music-volume-set! o volume))))
   
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
   
   (define (prepare-next-buffer o buffer url::bstring)
      (with-handler
	 (lambda (e)
	    ;; ignore that error becase if we cannot open
	    ;; the URL this will be detected again in the
	    ;; next open-file that will occur in the main
	    ;; play-url function
	    #f))
      (with-access::alsaportbuffer buffer (%head %tail %inlen %inbuf %inbufp)
	 (with-access::alsamusic o (%amutex %nextbuffer %status)
	    (synchronize %amutex
	       (unless %nextbuffer
		  (let ((ip (open-file url o)))
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
			   buf))))))))
   
   (define (open-port-buffer o::alsamusic d::alsadecoder url::bstring next::pair-nil)
      (let ((ip (open-file url o)))
	 (if (input-port? ip)
	     (with-access::alsamusic o (inbuf %buffer mkthread)
		(let ((buffer (instantiate::alsaportbuffer
				 (url url)
				 (port ip)
				 (%inlen (string-length inbuf))
				 (%inbuf inbuf)
				 (%inbufp inbuf))))
		   (thread-start!
		      (mkthread
			 (lambda ()
			    (let loop ((buffer buffer)
				       (next next))
			       ;; start filling that buffer
			       (alsabuffer-fill! buffer o)
			       ;; next song
			       (when (pair? next)
				  (let* ((url (car next))
					 (nbuffer (prepare-next-buffer o buffer url)))
				     (when (isa? nbuffer alsabuffer)
					(loop nbuffer (cdr next)))))))
			 "alsamusic-buffer"))
		   buffer))
	     (raise
		(instantiate::&io-port-error
		   (proc "music-play")
		   (msg "Cannot open")
		   (obj url))))))
      
   (define (open-mmap-buffer o::alsamusic d::alsadecoder url::bstring)
      (let ((mmap (open-mmap url :read #t :write #f)))
	 (if (mmap? mmap)
	     (let ((buffer (instantiate::alsammapbuffer
			      (url url)
			      (mmap mmap)
			      (%inlen (elong->fixnum (mmap-length mmap)))
			      (%inbufp (mmap->string mmap)))))
		(alsabuffer-fill! buffer o)
		buffer)
	     (raise
		(instantiate::&io-port-error
		   (proc "music-play")
		   (msg "Cannot open mmap")
		   (obj url))))))
   
   (define (next-port-buffer o::alsamusic url)
      (with-access::alsamusic o (%amutex %nextbuffer)
	 (synchronize %amutex
	    (when (isa? %nextbuffer alsaportbuffer)
	       (with-access::alsaportbuffer %nextbuffer ((u url) %nexttail %tail)
		  (if (eq? u url)
		      (let ((buf %nextbuffer))
			 (set! %tail %nexttail)
			 (set! %nextbuffer #f)
			 buf)
		      (begin
			 (alsabuffer-abort! %nextbuffer)
			 #f)))))))
   
   (define (open-buffer o::alsamusic d::alsadecoder urls::pair)
      (let ((url::bstring (car urls)))
	 (cond
	    ((next-port-buffer o url)
	     =>
	     (lambda (b) b))
	    ((file-exists? url)
	     (open-mmap-buffer o d url))
	    (else
	     (open-port-buffer o d url (cdr urls))))))

   (define (play-error o::alsamusic e::&error)
      (with-access::alsamusic o (%amutex %status)
	 (music-error-set! o e)
	 (sleep *error-sleep-duration*)))
   
   (define (play-url o::alsamusic d::alsadecoder n::int urls::pair pid::int notify::bool)
      (let ((buffer (open-buffer o d urls)))
	 (with-access::alsamusic o (%amutex %!pid %buffer %decoder)
	    (synchronize %amutex
	       (set! %buffer buffer)
	       (set! %decoder d)
	       (set! %!pid pid)
	       (update-song-status! o n pid (car urls)))
	    (when notify
	       (with-access::alsamusic o (%status onevent)
		  (with-access::musicstatus %status (playlistid)
		     (onevent o 'playlist playlistid)))))
	 (unwind-protect
	    (begin
	       (alsadecoder-reset! d)
	       (alsadecoder-decode d o buffer))
	    (alsabuffer-close buffer))))
   
   (define (play-urls urls n)
      (with-access::alsamusic o (%amutex %!pid %decoder %status)
	 (let ((pid %!pid))
	    (let loop ((l urls)
		       (n n)
		       (notify #t))
	       (with-handler
		  (lambda (e)
		     (when (> (alsa-debug) 0)
			(exception-notify e))
		     (play-error o e)
		     (loop (cdr l) (+fx n 1) #f))
		  (let ((action (synchronize %amutex
				   (cond
				      ((not (eq? pid %!pid))
				       ;; the playlist has changed
				       'invalidated)
				      ((null? l)
				       ;; the playlist is exhausted
				       'ended)
				      ((find-decoder o (car l))
				       ;; find a decoder for the file
				       =>
				       (lambda (d) d))
				      (else
				       ;; don't know how to play this file
				       (raise
					  (instantiate::&io-parse-error
					     (proc "music-play")
					     (msg (format
						     "Cannot find decoder (~a)"
						     (mime-type (car l))))
					     (obj (car l)))))))))
		     (cond
			((isa? action alsadecoder)
			 ;; a decoder has been found, let's play
			 (play-url o action n l pid notify)
			 (loop (cdr l) (+fx n 1) #f))
			((eq? action 'ended)
			 ;; playlist exhausted
			 (set! playlist-ended-id pid))
			((eq? action 'invalidated)
			 ;; playlist invalidated, nothing to do
			 #unspecified)
			(else
			 (raise
			    (instantiate::&error
			       (proc "music-play")
			       (msg "Internal error")
			       (obj action)))))))))))

   (define (wait-playlist n)
      (with-access::alsamusic o (%amutex %playlist %aready %!pid)
	 (synchronize %amutex
	    (let ((playlist %playlist))
	       (when (and (>=fx n 0) (<fx n (length playlist)))
		  ;; init alsa pcm
		  (pcm-init o)
		  ;; wait the the music player to be ready
		  (set! %!pid (+fx 1 %!pid))
		  (let ((playid %!pid))
		     (alsamusic-wait-ready! o)
		     (when (=fx %!pid playid)
			(set! %aready #f)
			;; play the list of urls
			(list-tail playlist n))))))))
   
   (define (play-playlist n)
      ;; wait for the player to be ready to play that playlist
      (let ((pl (wait-playlist n)))
	 (when (pair? pl)
	    ;; we got a playlist let's play it
	    (unwind-protect
	       (play-urls pl n)
	       (with-access::alsamusic o (%buffer %decoder %aready %amutex %acondv)
		  (synchronize %amutex
		     ;; reset the player state
		     (set! %aready #t)
		     (set! %buffer #f)
		     (set! %decoder #f)
		     (pcm-reset! o)
		     ;; signal if someone waiting for the music player
		     (condition-variable-broadcast! %acondv)))))))
   
   (define (resume-from-pause o)
      (with-access::alsamusic o (%decoder %amutex)
	 (synchronize %amutex
	    (when (isa? %decoder alsadecoder)
	       (with-access::alsadecoder %decoder (%dmutex %dcondv %!dpause)
		  (synchronize %dmutex
		     (when %!dpause
			(set! %!dpause #f)
			(condition-variable-broadcast! %dcondv)))
		  #t)))))
   
   (with-access::alsamusic o (%status)
      
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
      
      (when playlist-ended-id
	 (with-access::alsamusic o (onevent)
	    (onevent o 'ended playlist-ended-id)))))

;*---------------------------------------------------------------------*/
;*    music-stop ::alsamusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::alsamusic)
   (with-access::alsamusic o (%amutex %status %!pid)
      (synchronize %amutex
	 (set! %!pid (+fx 1 %!pid))
	 (alsamusic-wait-ready! o))))

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
   (with-access::alsabuffer b (%bmutex %bcondv url %eof %empty)
      (synchronize %bmutex
	 (set! %empty #t)
	 (set! %eof #t)
	 (condition-variable-broadcast! %bcondv))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-abort! ...                                           */
;*---------------------------------------------------------------------*/
(define (alsadecoder-abort! d::alsadecoder)
   (with-access::alsadecoder d (%!dabort %!dpause %dmutex %dcondv)
      (synchronize %dmutex
	 (set! %!dpause #f)
	 (set! %!dabort #t)
	 (condition-variable-broadcast! %dcondv))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::alsamusic)
   (with-access::alsamusic o (%amutex %decoder)
      (synchronize %amutex
	 (when (isa? %decoder alsadecoder)
	    (alsadecoder-pause %decoder)))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-pause ...                                            */
;*---------------------------------------------------------------------*/
(define (alsadecoder-pause d::alsadecoder)
   (with-access::alsadecoder d (%dmutex %dcondv %!dpause)
      (synchronize %dmutex
	 (if %!dpause
	     (begin
		(set! %!dpause #f)
		(condition-variable-broadcast! %dcondv))
	     (set! %!dpause #t)))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-fill! buffer::alsabuffer o::alsamusic))
   
;*---------------------------------------------------------------------*/
;*    alsabuffer-fill! ...                                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-fill! buffer::alsaportbuffer o::alsamusic)
   (with-access::alsaportbuffer buffer (%bmutex %bcondv %head %tail %inbuf %inlen %eof %empty %seek readsz port url)
      
      (define inlen %inlen)

      (define (timed-read sz)
	 (read-fill-string! %inbuf %head sz port))

      (define (set-eof!)
	 (synchronize %bmutex
	    (set! %eof #t)
	    (condition-variable-broadcast! %bcondv)))

      (define (inc-head! i)
	 (let ((nhead (+fx %head i)))
	    (if (=fx nhead inlen)
		(set! %head 0)
		(set! %head nhead))
	    (when %empty
	       ;; buffer was empty
	       (synchronize %bmutex
		  (set! %empty #f)
		  (condition-variable-broadcast! %bcondv)))))
      
      (with-handler
	 (lambda (e)
	    (when (>=fx (alsa-debug) 1) (exception-notify e))
	    (set-eof!)
	    (music-error-set! o e))
	 (let loop ()
	    (cond
	       (%eof
	        ;;; done looping
		#unspecified)
	       ((>=elong %seek #e0)
		;; seek buffer
		(set-input-port-position! port %seek)
		(set! %seek #e-1)
		(loop))
	       ((and (=fx %head %tail) (not %empty))
		;; buffer full
		(synchronize %bmutex
		   (when (and (=fx %head %tail) (not %empty))
		      (condition-variable-wait! %bcondv %bmutex)))
		(loop))
	       (else
		(let* ((s (minfx readsz
			     (if (<fx %head %tail)
				 (-fx %tail %head)
				 (-fx inlen %head))))
		       (i (timed-read s)))
		   (cond
		      ((eof-object? i)
		       (set-eof!)
		       (with-access::alsamusic o (onevent)
			  (onevent o 'loaded url)))
		      ((>fx i 0)
		       (inc-head! i)))
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
   (with-access::alsamusic o (decoders %status)
      (with-access::musicstatus %status (volume)
	 (unless (= vol volume)
	    (for-each (lambda (d) (alsadecoder-volume-set! d vol)) decoders))))
   (call-next-method))
   
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
      (synchronize %dmutex
	 (set! %!dpause #f)
	 (set! %!dabort #f))
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
;*    alsabuffer-stream ::alsabuffer ...                               */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-stream o::alsabuffer)
   #f)

;*---------------------------------------------------------------------*/
;*    alsabuffer-stream ::alsaportbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-stream o::alsaportbuffer)
   (with-access::alsaportbuffer o (port)
      port))

;*---------------------------------------------------------------------*/
;*    alsabuffer-stream ::alsammapbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-stream o::alsammapbuffer)
   (with-access::alsammapbuffer o (mmap)
      mmap))

;*---------------------------------------------------------------------*/
;*    alsabuffer-close ::alsabuffer ...                                */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-close o::alsabuffer))

;*---------------------------------------------------------------------*/
;*    alsabuffer-close ::alsaportbuffer ...                            */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-close o::alsaportbuffer)
   (with-access::alsaportbuffer o (port)
      (close-input-port port)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-close ::alsammapbuffer ...                            */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-close o::alsammapbuffer)
   (with-access::alsammapbuffer o (mmap)
      (close-mmap mmap)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-length ::alsabuffer ...                               */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-length::belong o::alsabuffer)
   #e-1)

;*---------------------------------------------------------------------*/
;*    alsabuffer-length ::alsaportbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-length o::alsaportbuffer)
   (with-access::alsaportbuffer o (port)
      (input-port-length port)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-length ::alsammapbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-length o::alsammapbuffer)
   (with-access::alsammapbuffer o (mmap)
      (mmap-length mmap)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-tell ::alsabuffer ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-tell o::alsabuffer)
   #f)

;*---------------------------------------------------------------------*/
;*    alsabuffer-tell ::alsaportbuffer ...                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-tell o::alsaportbuffer)
   (with-access::alsaportbuffer o (port %head %tail)
      (let ((tell (fixnum->elong (input-port-position port))))
	 (-elong tell (fixnum->elong (alsabuffer-available o))))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-tell ::alsammapbuffer ...                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-tell o::alsammapbuffer)
   (with-access::alsammapbuffer o (%tail)
      (fixnum->elong %tail)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-seek ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-seek buffer::alsabuffer offset))

;*---------------------------------------------------------------------*/
;*    alsabuffer-seek ::alsaportbuffer ...                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-seek buffer::alsaportbuffer offset)
   (with-access::alsaportbuffer buffer (%seek port %tail %head %empty
					  %bmutex %bcondv %eof)
      ;; only ports that support seek have a position length
      (when (>fx (input-port-length port) 0)
	 (unless %eof
	    (synchronize %bmutex
	       ;; mark the seek position
	       (set! %empty #t)
	       (set! %seek (if (llong? offset) (llong->fixnum offset) offset))
	       (set! %head %tail)
	       (condition-variable-broadcast! %bcondv)))
	 ;; the seek succeeded
	 #t)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-seek ::alsammapbuffer ...                             */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-seek buffer::alsammapbuffer offset)
   (with-access::alsammapbuffer buffer (%tail mmap)
      (set! %tail
	 (cond
	    ((llong? offset) (llong->fixnum offset))
	    ((elong? offset) (elong->fixnum offset))
	    (else offset)))
      #t))

;*---------------------------------------------------------------------*/
;*    alsabuffer-substring ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-substring buffer::alsabuffer start end))

;*---------------------------------------------------------------------*/
;*    alsabuffer-substring ::alsaportbuffer ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-substring buffer::alsaportbuffer start end)
   (with-access::alsaportbuffer buffer (%bmutex %inbuf)
      (synchronize %bmutex
	 (substring %inbuf start end))))
   
;*---------------------------------------------------------------------*/
;*    alsabuffer-substring ::alsammapbuffer ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-substring buffer::alsammapbuffer start end)
   (with-access::alsammapbuffer buffer (mmap)
      (mmap-substring mmap (fixnum->elong start) (fixnum->elong end))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-blit-string! ::alsabuffer ...                         */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-blit-string! buf::alsabuffer s0 string s1 len))

;*---------------------------------------------------------------------*/
;*    alsabuffer-blit-string! ::alsaportbuffer ...                     */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-blit-string! buf::alsaportbuffer s0 string s1 len)
   (with-access::alsaportbuffer buf (%inbuf)
      (blit-string! %inbuf s0 string s1 len)))

;*---------------------------------------------------------------------*/
;*    alsabuffer-blit-string! ::alsammaptbuffer ...                    */
;*---------------------------------------------------------------------*/
(define-method (alsabuffer-blit-string! buf::alsammapbuffer s0 string s1 len)
   (with-access::alsammapbuffer buf (mmap)
      (let loop ((i 0))
	 (when (<fx i len)
	    (string-set! string (+fx s1 i) (mmap-ref mmap (+fx i s0)))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    alsabuffer-available ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (alsabuffer-available o::alsabuffer)
   (with-access::alsabuffer o (%head %tail %empty %inlen)
      (cond
	 ((>fx %head %tail) (-fx %head %tail))
	 ((<fx %head %tail) (+fx (-fx %inlen %tail) %head))
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
	 (else "audio/mpeg")))
   
   (if (and (string-prefix? "http" path)
	    (or (string-prefix? "http://" path)
		(string-prefix? "https://" path)))
       (let ((i (string-index-right path #\?)))
	  (if i
	      (mime-type (substring path 6 i))
	      (mime-type-file path)))
       (mime-type-file path)))

