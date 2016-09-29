;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/musicbuf.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Wed Sep 28 04:43:47 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A (multimedia) buffer music player.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-musicbuf
   
   (import  __multimedia-music
	    __multimedia-id3)
   
   (export  (class musicbuf::music
	       (inbuf::bstring read-only (default (make-string (*fx 1024 1024))))
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

	    (class musicbuffer
	       (url::bstring read-only)
	       (%eof::bool (default #f))
	       (%bcondv::condvar read-only (default (make-condition-variable)))
	       (%bmutex::mutex read-only (default (make-mutex)))
	       (%inlen::long read-only)
	       %inbufp::string
	       (%head::long (default 0))
	       (%tail::long (default 0))
	       (%empty::bool (default #t)))

	    (class musicportbuffer::musicbuffer
	       (port::input-port read-only)
	       (readsz::long read-only (default 8192))
	       (%inbuf::bstring read-only)
	       (%nexttail::long (default 0))
	       (%seek::elong (default #e-1)))

	    (class musicmmapbuffer::musicbuffer
	       (mmap::mmap read-only))

	    (class musicdecoder
	       (musicdecoder-init)
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

	    (generic musicbuf-init! ::musicbuf)
	    (generic musicbuf-reset! ::musicbuf)
	    (generic musicbuf-write ::musicbuf ::bstring ::long)
	    (generic musicbuf-drain ::musicbuf)

	    (generic musicbuffer-stream::obj ::musicbuffer)
	    (generic musicbuffer-length::belong ::musicbuffer)
	    (generic musicbuffer-available::long ::musicbuffer)
	    (generic musicbuffer-tell::obj ::musicbuffer)
	    (generic musicbuffer-seek ::musicbuffer ::obj)
	    (generic musicbuffer-substring ::musicbuffer ::int ::int)
	    (generic musicbuffer-blit-string! ::musicbuffer ::int ::bstring ::int ::int)

	    (generic musicdecoder-init ::musicdecoder)
	    (generic musicdecoder-reset! ::musicdecoder)
	    (generic musicdecoder-close ::musicdecoder)
	    (generic musicdecoder-can-play-type? ::musicdecoder ::bstring)
	    (generic musicdecoder-decode ::musicdecoder ::musicbuf ::musicbuffer)
	    (generic musicdecoder-position::long ::musicdecoder ::musicbuffer)
	    (generic musicdecoder-info::long ::musicdecoder)
	    (generic musicdecoder-seek ::musicdecoder ::long)
	    (generic musicdecoder-volume-set! ::musicdecoder ::long)
	    (generic musicdecoder-hwparams-set! ::musicdecoder ::music ::musicbuffer)
	    ))

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (begin (bigloo-compiler-debug) 1))

;*---------------------------------------------------------------------*/
;*    musicbuffer-debug ...                                            */
;*---------------------------------------------------------------------*/
(define (musicbuffer-debug)
   (if (>fx ($compiler-debug) 0)
       (bigloo-debug)
       0))

;*---------------------------------------------------------------------*/
;*    *error-sleep-duration* ...                                       */
;*---------------------------------------------------------------------*/
(define *error-sleep-duration*
   (* 2 1000 1000))

;*---------------------------------------------------------------------*/
;*    music-init ::musicbuf ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-init o::musicbuf)
   (with-access::musicbuf o (%amutex %status inbuf)
      (synchronize %amutex
	 (with-access::musicstatus %status (volume state)
	    (set! volume 100)
	    (set! state 'uninitialized)))))

;*---------------------------------------------------------------------*/
;*    music-status ::musicbuf ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-status o::musicbuf)
   (with-access::musicbuf o (%amutex %decoder %buffer %status)
      (synchronize %amutex
	 (when (and (isa? %decoder musicdecoder)
		    (isa? %buffer musicbuffer))
	    (with-access::musicstatus %status (songpos songlength)
	       (set! songpos (musicdecoder-position %decoder %buffer)))))
      %status))

;*---------------------------------------------------------------------*/
;*    music-playlist-get ::musicbuf ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get o::musicbuf)
   (with-access::musicbuf o (%playlist)
      %playlist))

;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::musicbuf ...                               */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! o::musicbuf s)
   (with-access::musicbuf o (%amutex %playlist %status)
      (synchronize %amutex
	 (set! %playlist (append! %playlist (list s)))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (set! playlistid (+fx 1 playlistid))
	    (set! playlistlength (+fx 1 playlistlength))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::musicbuf ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! o::musicbuf n)
   (with-access::musicbuf o (%amutex %playlist %status)
      (synchronize %amutex
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (when (and (>=fx n 0) (<fx n playlistlength))
	       (set! %playlist (remq! (list-ref %playlist n) %playlist))
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (length %playlist)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::musicbuf ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! o::musicbuf)
   (with-access::musicbuf o (%amutex %playlist %status)
      (synchronize %amutex
	 (set! %playlist '())
	 (with-access::musicstatus %status (playlistlength song)
	    (set! song 0)
	    (set! playlistlength 0)))))

;*---------------------------------------------------------------------*/
;*    music-can-play-type? ::musicbuf ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-can-play-type? o::musicbuf mimetype::bstring)
   (with-access::musicbuf o (decoders)
      (any (lambda (d) (musicdecoder-can-play-type? d mimetype)) decoders)))
   
;*---------------------------------------------------------------------*/
;*    music-seek ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::musicbuf pos . song)
   (with-access::musicbuf o (%amutex %decoder %buffer)
      (synchronize %amutex
	 (if (pair? song)
	     (begin
		(unless (integer? (car song))
		   (bigloo-type-error '|music-seek ::musicbuf| 'int (car song)))
		(music-play o (car song)))
	     (when (isa? %decoder musicdecoder)
		(musicdecoder-seek %decoder pos))))))

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
(define (open-file url o::musicbuf)
   (with-handler
      (lambda (e)
	 (exception-notify e)
	 #f)
      (with-access::musicbuf o (timeout %status)
	 (let ((pi (open-input-file url #f timeout)))
	    (when (input-port? pi)
	       (input-port-timeout-set! pi timeout))
	    pi))))

;*---------------------------------------------------------------------*/
;*    music-play ::musicbuf ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-play o::musicbuf . s)

   (define playlist-ended-id #f)
   
   (define (find-decoder o url)
      (with-access::musicbuf o (decoders)
	 (let ((mime (mime-type url)))
	    (find (lambda (d) (musicdecoder-can-play-type? d mime))
	       decoders))))
   
   (define (update-song-status! o n pid url)
      (with-access::musicbuf o (%status)
	 (with-access::musicstatus %status (state song songpos songid songlength playlistid volume playlistid)
	    (set! playlistid pid)
	    (set! songpos 0)
	    (set! song n)
	    (set! songid (+fx (* 100 pid) n))
	    (set! songlength (url-song-length url))
	    (music-state-set! o 'play)
	    (music-volume-set! o volume))))
   
   (define (prepare-next-buffer o buffer url::bstring)
      (with-handler
	 (lambda (e)
	    ;; ignore that error becase if we cannot open
	    ;; the URL this will be detected again in the
	    ;; next open-file that will occur in the main
	    ;; play-url function
	    #f))
      (with-access::musicportbuffer buffer (%head %tail %inlen %inbuf %inbufp)
	 (with-access::musicbuf o (%amutex %nextbuffer %status)
	    (synchronize %amutex
	       (unless %nextbuffer
		  (let ((ip (open-file url o)))
		     (when (input-port? ip)
			(let ((buf (instantiate::musicportbuffer
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
   
   (define (open-port-buffer o::musicbuf d::musicdecoder url::bstring next::pair-nil)
      (let ((ip (open-file url o)))
	 (if (input-port? ip)
	     (with-access::musicbuf o (inbuf %buffer mkthread)
		(let ((buffer (instantiate::musicportbuffer
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
			       (musicbuffer-fill! buffer o)
			       ;; next song
			       (when (pair? next)
				  (let* ((url (car next))
					 (nbuffer (prepare-next-buffer o buffer url)))
				     (when (isa? nbuffer musicbuffer)
					(loop nbuffer (cdr next)))))))
			 "musicbuf-buffer"))
		   buffer))
	     (raise
		(instantiate::&io-port-error
		   (proc "music-play")
		   (msg "Cannot open")
		   (obj url))))))
      
   (define (open-mmap-buffer o::musicbuf d::musicdecoder url::bstring)
      (let ((mmap (open-mmap url :read #t :write #f)))
	 (if (mmap? mmap)
	     (let ((buffer (instantiate::musicmmapbuffer
			      (url url)
			      (mmap mmap)
			      (%inlen (elong->fixnum (mmap-length mmap)))
			      (%inbufp (mmap->string mmap)))))
		(musicbuffer-fill! buffer o)
		buffer)
	     (raise
		(instantiate::&io-port-error
		   (proc "music-play")
		   (msg "Cannot open mmap")
		   (obj url))))))
   
   (define (next-port-buffer o::musicbuf url)
      (with-access::musicbuf o (%amutex %nextbuffer)
	 (synchronize %amutex
	    (when (isa? %nextbuffer musicportbuffer)
	       (with-access::musicportbuffer %nextbuffer ((u url) %nexttail %tail)
		  (if (eq? u url)
		      (let ((buf %nextbuffer))
			 (set! %tail %nexttail)
			 (set! %nextbuffer #f)
			 buf)
		      (begin
			 (musicbuffer-abort! %nextbuffer)
			 #f)))))))
   
   (define (open-buffer o::musicbuf d::musicdecoder urls::pair)
      (let ((url::bstring (car urls)))
	 (cond
	    ((next-port-buffer o url)
	     =>
	     (lambda (b) b))
	    ((file-exists? url)
	     (open-mmap-buffer o d url))
	    (else
	     (open-port-buffer o d url (cdr urls))))))

   (define (play-error o::musicbuf e::&error)
      (with-access::musicbuf o (%amutex %status)
	 (music-error-set! o e)
	 (sleep *error-sleep-duration*)))
   
   (define (play-url o::musicbuf d::musicdecoder n::int urls::pair pid::int notify::bool)
      (let ((buffer (open-buffer o d urls)))
	 (unwind-protect
	    (begin
	       (musicdecoder-reset! d)
	       (with-access::musicbuf o (%amutex %!pid %buffer %decoder)
		  (synchronize %amutex
		     (set! %buffer buffer)
		     (set! %decoder d)
		     (set! %!pid pid)
		     (update-song-status! o n pid (car urls)))
		  (when notify
		     (with-access::musicbuf o (%status onevent)
			(with-access::musicstatus %status (playlistid)
			   (onevent o 'playlist playlistid)))))
	       (musicdecoder-decode d o buffer))
	    (musicbuffer-close buffer))))
   
   (define (play-urls urls n)
      (with-access::musicbuf o (%amutex %!pid %decoder %status)
	 (let ((pid %!pid))
	    (let loop ((l urls)
		       (n n)
		       (notify #t))
	       (with-handler
		  (lambda (e)
		     (when (> (musicbuffer-debug) 0)
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
			((isa? action musicdecoder)
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
      (with-access::musicbuf o (%amutex %playlist %aready %!pid)
	 (synchronize %amutex
	    (let ((playlist %playlist))
	       (when (and (>=fx n 0) (<fx n (length playlist)))
		  ;; init alsa pcm
		  ;; (pcm-init 0)
		  (musicbuf-init! o)
		  ;; wait the the music player to be ready
		  (set! %!pid (+fx 1 %!pid))
		  (let ((playid %!pid))
		     (musicbuf-wait-ready! o)
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
	       (with-access::musicbuf o (%buffer %decoder %aready %amutex %acondv)
		  (synchronize %amutex
		     ;; reset the player state
		     (set! %aready #t)
		     (set! %buffer #f)
		     (set! %decoder #f)
		     ;;(pcm-reset! o)
		     (musicbuf-reset! o)
		     ;; signal if someone waiting for the music player
		     (condition-variable-broadcast! %acondv)))))))
   
   (define (resume-from-pause o)
      (with-access::musicbuf o (%decoder %amutex)
	 (synchronize %amutex
	    (when (isa? %decoder musicdecoder)
	       (with-access::musicdecoder %decoder (%dmutex %dcondv %!dpause)
		  (synchronize %dmutex
		     (when %!dpause
			(set! %!dpause #f)
			(condition-variable-broadcast! %dcondv)))
		  #t)))))
   
   (with-access::musicbuf o (%status)
      
      (cond
	 ((pair? s)
	  ;; play the playing from a user index
	  (unless (integer? (car s))
	     (bigloo-type-error "music-play ::musicbuf" 'int (car s)))
	  (play-playlist (car s)))
	 ((resume-from-pause o)
	  #unspecified)
	 (else
	  ;; play the playlist from the current position
	  (with-access::musicstatus %status (song)
	     (play-playlist song))))
      
      (when playlist-ended-id
	 (with-access::musicbuf o (onevent)
	    (onevent o 'ended playlist-ended-id)))))

;*---------------------------------------------------------------------*/
;*    musicbuf-init! ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (musicbuf-init! o::musicbuf)
   #f)

;*---------------------------------------------------------------------*/
;*    musicbuf-reset! ::musicbuf ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (musicbuf-reset! o::musicbuf)
   #f)

;*---------------------------------------------------------------------*/
;*    musicbuf-write ::musicbuf ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (musicbuf-write o::musicbuf outbuf::bstring sz::long))

;*---------------------------------------------------------------------*/
;*    musicbuf-write ::musicbuf ...                                    */
;*---------------------------------------------------------------------*/
(define-generic (musicbuf-drain o::musicbuf)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    music-stop ::musicbuf ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::musicbuf)
   (with-access::musicbuf o (%amutex %status %!pid)
      (synchronize %amutex
	 (set! %!pid (+fx 1 %!pid))
	 (musicbuf-wait-ready! o))))

;*---------------------------------------------------------------------*/
;*    musicbuf-wait-ready! ...                                         */
;*---------------------------------------------------------------------*/
(define (musicbuf-wait-ready! o::musicbuf)
   ;; %amutex already locked
   (with-access::musicbuf o (%decoder %buffer %nextbuffer)
      (when (isa? %decoder musicdecoder)
	 (musicdecoder-abort! %decoder))
      (when (isa? %buffer musicbuffer)
	 (musicbuffer-abort! %buffer))
      (when (isa? %nextbuffer musicbuffer)
	 (musicbuffer-abort! %nextbuffer))
      (with-access::musicbuf o (%aready %acondv %amutex)
	 (unless %aready
	    (let loop ()
	       (unless %aready
		  ;; keep waiting
		  (condition-variable-wait! %acondv %amutex)
		  (loop))))
	 (set! %aready #t))))

;*---------------------------------------------------------------------*/
;*    musicbuffer-abort! ...                                           */
;*---------------------------------------------------------------------*/
(define (musicbuffer-abort! b::musicbuffer)
   (with-access::musicbuffer b (%bmutex %bcondv url %eof %empty)
      (synchronize %bmutex
	 (set! %empty #t)
	 (set! %eof #t)
	 (condition-variable-broadcast! %bcondv))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-abort! ...                                         */
;*---------------------------------------------------------------------*/
(define (musicdecoder-abort! d::musicdecoder)
   (with-access::musicdecoder d (%!dabort %!dpause %dmutex %dcondv)
      (synchronize %dmutex
	 (set! %!dpause #f)
	 (set! %!dabort #t)
	 (condition-variable-broadcast! %dcondv))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::musicbuf)
   (with-access::musicbuf o (%amutex %decoder)
      (synchronize %amutex
	 (when (isa? %decoder musicdecoder)
	    (musicdecoder-pause %decoder)))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-pause ...                                          */
;*---------------------------------------------------------------------*/
(define (musicdecoder-pause d::musicdecoder)
   (with-access::musicdecoder d (%dmutex %dcondv %!dpause)
      (synchronize %dmutex
	 (if %!dpause
	     (begin
		(set! %!dpause #f)
		(condition-variable-broadcast! %dcondv))
	     (set! %!dpause #t)))))

;*---------------------------------------------------------------------*/
;*    musicbuffer-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-fill! buffer::musicbuffer o::musicbuf))
   
;*---------------------------------------------------------------------*/
;*    musicbuffer-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-fill! buffer::musicportbuffer o::musicbuf)
   (with-access::musicportbuffer buffer (%bmutex %bcondv %head %tail %inbuf %inlen %eof %empty %seek readsz port url)
      
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
	    (when (>=fx (musicbuffer-debug) 1) (exception-notify e))
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
		       (with-access::musicbuf o (onevent)
			  (onevent o 'loaded url)))
		      ((>fx i 0)
		       (inc-head! i)))
		   (loop))))))))

;*---------------------------------------------------------------------*/
;*    musicbuffer-fill! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-fill! buffer::musicmmapbuffer o::musicbuf)
   (with-access::musicmmapbuffer buffer (%head %empty %inbufp %eof mmap url %inlen)
      (set! %inbufp (mmap->string mmap))
      (set! %head 0)
      (set! %eof #t)
      (set! %empty (=fx %inlen 0))
      (with-access::musicbuf o (onevent)
	 (onevent o 'loaded url))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-decode ::musicdecoder ...                           */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-decode d::musicdecoder o::musicbuf b::musicbuffer))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::musicbuf ...                                  */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::musicbuf)
   (with-access::musicbuf o (%status)
      (with-access::musicstatus %status (volume)
	 volume)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::musicbuf ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::musicbuf vol)
   (with-access::musicbuf o (decoders %status)
      (with-access::musicstatus %status (volume)
	 (unless (= vol volume)
	    (for-each (lambda (d) (musicdecoder-volume-set! d vol))
	       decoders))))
   (call-next-method))
   
;*---------------------------------------------------------------------*/
;*    musicdecoder-init ::musicdecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-init o::musicdecoder)
   o)

;*---------------------------------------------------------------------*/
;*    musicdecoder-reset! ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-reset! o::musicdecoder)
   (with-access::musicdecoder o (%dmutex %!dpause %!dabort)
      (synchronize %dmutex
	 (set! %!dpause #f)
	 (set! %!dabort #f))
      #f))

;*---------------------------------------------------------------------*/
;*    musicdecoder-close ::musicdecoder ...                            */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-close o::musicdecoder))
   
;*---------------------------------------------------------------------*/
;*    musicdecoder-can-play-type? ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-can-play-type? o::musicdecoder mime::bstring)
   (with-access::musicdecoder o (mimetypes)
      (member mime mimetypes)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-stream ::musicbuffer ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-stream o::musicbuffer)
   #f)

;*---------------------------------------------------------------------*/
;*    musicbuffer-stream ::musicportbuffer ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-stream o::musicportbuffer)
   (with-access::musicportbuffer o (port)
      port))

;*---------------------------------------------------------------------*/
;*    musicbuffer-stream ::musicmmapbuffer ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-stream o::musicmmapbuffer)
   (with-access::musicmmapbuffer o (mmap)
      mmap))

;*---------------------------------------------------------------------*/
;*    musicbuffer-close ::musicbuffer ...                              */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-close o::musicbuffer))

;*---------------------------------------------------------------------*/
;*    musicbuffer-close ::musicportbuffer ...                          */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-close o::musicportbuffer)
   (with-access::musicportbuffer o (port)
      (close-input-port port)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-close ::musicmmapbuffer ...                          */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-close o::musicmmapbuffer)
   (with-access::musicmmapbuffer o (mmap)
      (close-mmap mmap)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-length ::musicbuffer ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-length::belong o::musicbuffer)
   #e-1)

;*---------------------------------------------------------------------*/
;*    musicbuffer-length ::musicportbuffer ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-length o::musicportbuffer)
   (with-access::musicportbuffer o (port)
      (input-port-length port)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-length ::musicmmapbuffer ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-length o::musicmmapbuffer)
   (with-access::musicmmapbuffer o (mmap)
      (mmap-length mmap)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-tell ::musicbuffer ...                               */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-tell o::musicbuffer)
   #f)

;*---------------------------------------------------------------------*/
;*    musicbuffer-tell ::musicportbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-tell o::musicportbuffer)
   (with-access::musicportbuffer o (port %head %tail)
      (let ((tell (fixnum->elong (input-port-position port))))
	 (-elong tell (fixnum->elong (musicbuffer-available o))))))

;*---------------------------------------------------------------------*/
;*    musicbuffer-tell ::musicmmapbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-tell o::musicmmapbuffer)
   (with-access::musicmmapbuffer o (%tail)
      (fixnum->elong %tail)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-seek ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-seek buffer::musicbuffer offset))

;*---------------------------------------------------------------------*/
;*    musicbuffer-seek ::musicportbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-seek buffer::musicportbuffer offset)
   (with-access::musicportbuffer buffer (%seek port %tail %head %empty
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
;*    musicbuffer-seek ::musicmmapbuffer ...                           */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-seek buffer::musicmmapbuffer offset)
   (with-access::musicmmapbuffer buffer (%tail mmap)
      (set! %tail
	 (cond
	    ((llong? offset) (llong->fixnum offset))
	    ((elong? offset) (elong->fixnum offset))
	    (else offset)))
      #t))

;*---------------------------------------------------------------------*/
;*    musicbuffer-substring ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-substring buffer::musicbuffer start end))

;*---------------------------------------------------------------------*/
;*    musicbuffer-substring ::musicportbuffer ...                      */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-substring buffer::musicportbuffer start end)
   (with-access::musicportbuffer buffer (%bmutex %inbuf)
      (synchronize %bmutex
	 (substring %inbuf start end))))
   
;*---------------------------------------------------------------------*/
;*    musicbuffer-substring ::musicmmapbuffer ...                      */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-substring buffer::musicmmapbuffer start end)
   (with-access::musicmmapbuffer buffer (mmap)
      (mmap-substring mmap (fixnum->elong start) (fixnum->elong end))))

;*---------------------------------------------------------------------*/
;*    musicbuffer-blit-string! ::musicbuffer ...                       */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-blit-string! buf::musicbuffer s0 string s1 len))

;*---------------------------------------------------------------------*/
;*    musicbuffer-blit-string! ::musicportbuffer ...                   */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-blit-string! buf::musicportbuffer s0 string s1 len)
   (with-access::musicportbuffer buf (%inbuf)
      (blit-string! %inbuf s0 string s1 len)))

;*---------------------------------------------------------------------*/
;*    musicbuffer-blit-string! ::musicmmaptbuffer ...                  */
;*---------------------------------------------------------------------*/
(define-method (musicbuffer-blit-string! buf::musicmmapbuffer s0 string s1 len)
   (with-access::musicmmapbuffer buf (mmap)
      (let loop ((i 0))
	 (when (<fx i len)
	    (string-set! string (+fx s1 i) (mmap-ref mmap (+fx i s0)))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    musicbuffer-available ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (musicbuffer-available o::musicbuffer)
   (with-access::musicbuffer o (%head %tail %empty %inlen)
      (cond
	 ((>fx %head %tail) (-fx %head %tail))
	 ((<fx %head %tail) (+fx (-fx %inlen %tail) %head))
	 (%empty 0)
	 (else %inlen))))

;*---------------------------------------------------------------------*/
;*    musicdecoder-position ::musicdecoder ...                         */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-position o::musicdecoder b::musicbuffer))

;*---------------------------------------------------------------------*/
;*    musicdecoder-info ::musicdecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-info o::musicdecoder))

;*---------------------------------------------------------------------*/
;*    musicdecoder-seek ::musicdecoder ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-seek o::musicdecoder ms::long))

;*---------------------------------------------------------------------*/
;*    musicdecoder-volume-set! ::musicdecoder ...                      */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-volume-set! o::musicdecoder v::long))

;*---------------------------------------------------------------------*/
;*    musicdecoder-hwparams-set! ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (musicdecoder-hwparams-set! dec::musicdecoder am buffer)
   #f)

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
	      (let ((base (substring path 6 i)))
		 (if (string-index base #\.)
		     ;; there is something that looks like a suffix in base url
		     (mime-type base)
		     ;; there is suffix, try in the arguments
		     (mime-type-file (substring path (+fx i 1)))))
	      (mime-type-file path)))
       (mime-type-file path)))

