;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstmm_music.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 31 07:15:14 2008                          */
;*    Last change :  Sun Nov 18 15:12:04 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This module implements a Gstreamer backend for the               */
;*    multimedia MUSIC class.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_multimedia_music
   
   (library multimedia pthread)
   
   (include "gst.sch")
   
   (use	    __gstreamer_gstreamer
	    __gstreamer_gstobject
	    __gstreamer_gstelement
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstpad
	    __gstreamer_gstbus
	    __gstreamer_gstbin
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstmessage)

   (import  __gstreamer_gstregistry
	    __gstreamer_gstpipeline)
   
   (export  (class gstmusic::music
	       
	       (%audiosrc (default #unspecified))
	       (%audiosink (default #unspecified))
	       (%audiomixer (default #unspecified))
	       (%audiodecode (default #unspecified))
	       (%audioconvert (default #unspecified))
	       (%audioresample (default #unspecified))
	       (%pipeline (default #f))
	       
	       (%playlist::pair-nil (default '()))
	       (%meta::pair-nil (default '()))
	       (%tag::obj (default '()))
	       (%gcondv::condvar read-only (default (make-condition-variable)))
	       (%gready::bool (default #t))
	       (%!gabort::bool (default #f)))))

;*---------------------------------------------------------------------*/
;*    music-init ::gstmusic ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-init o::gstmusic)
   (with-access::gstmusic o (%pipeline
			       %audiosrc
			       %audiodecode %audioconvert %audioresample
			       %audiomixer %audiosink
			       %mutex)
      (call-next-method)
      (synchronize %mutex
	 ;; initialize the pipeline
	 (unless (isa? %pipeline gst-element)
	    (unless (isa? %audiosrc gst-element)
	       (set! %audiosrc (gst-element-factory-make "bglportsrc"))
	       (unless (isa? %audiosrc gst-element)
		  (error "music-init ::gstmusic" "Cannot create audiosrc" o)))
	    (unless (isa? %audiosink gst-element)
	       (let ((f (gst-element-factory-find "autoaudiosink")))
		  (if (isa? f gst-element-factory)
		      (set! %audiosink (gst-element-factory-create f))
		      (set! %audiosink (find-best-ranked-audio-sink))))
	       (unless (isa? %audiosink gst-element)
		  (error "music-init ::gstmusic" "Cannot create audiosink" o)))
	    (unless (isa? %audiomixer gst-element)
	       (let ((f (gst-element-factory-make "volume")))
		  (set! %audiomixer (gst-element-factory-make "volume")))
	       (unless (isa? %audiomixer gst-element)
		  (error "music-init ::gstmusic" "Cannot create audiomixer" o)))
	    (unless (isa? %audiodecode gst-element)
	       (set! %audiodecode (gst-element-factory-make "decodebin"))
	       (unless (isa? %audiodecode gst-element)
		  (error "music-init ::gstmusic" "Cannot create audiodecode" o)))
	    (unless (isa? %audioconvert gst-element)
	       (set! %audioconvert (gst-element-factory-make "audioconvert"))
	       (unless (isa? %audioconvert gst-element)
		  (error "music-init ::gstmusic" "Cannot create audioconvert" o)))
	    (unless (isa? %audioresample gst-element)
	       (set! %audioresample (gst-element-factory-make "audioresample"))
	       (unless (isa? %audioresample gst-element)
		  (error "music-init ::gstmusic" "Cannot create audioresampler" o)))
	    (set! %pipeline (instantiate::gst-pipeline))
	    (unless (isa? %audioresample gst-element)
	       (error "music-init ::gstmusic" "Cannot create pipeline" o))
	    
	    (gst-bin-add! %pipeline
	       %audiosrc
	       %audiodecode
	       %audioconvert
	       %audioresample
	       %audiomixer
	       %audiosink)
	    
	    (gst-element-link! %audiosrc
	       %audiodecode)
	    (gst-element-link! %audioconvert
	       %audioresample
	       %audiomixer
	       %audiosink)
	    
	    (gst-object-connect! %audiodecode
	       "pad-added"
	       (lambda (el pad)
		  (let ((p (gst-element-pad %audioconvert "sink")))
		     (gst-pad-link! pad p))))))))

;*---------------------------------------------------------------------*/
;*    find-best-ranked-audio-sink ...                                  */
;*---------------------------------------------------------------------*/
(define (find-best-ranked-audio-sink)
   (let* ((lall (gst-registry-element-factory-list))
	  (lsinkaudio (filter (lambda (f)
				 (with-access::gst-element-factory f (klass)
				    (string-ci=? klass "sink/audio")))
			 lall))
	  (lrank (sort lsinkaudio
		       (lambda (f1 f2)
			  (with-access::gst-element-factory f1 ((rank1 rank))
			     (with-access::gst-element-factory f2 ((rank2 rank))
				(>fx rank1 rank2)))))))
      (if (null? lrank)
	  (error "music-init"
	     "Cannot find audio sink"
	     (map (lambda (e) (with-access::gst-element-factory e (name) name)) lall))
	  (gst-element-factory-create (car lrank) "gstmm-sink"))))

;*---------------------------------------------------------------------*/
;*    music-close ::gstmusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-close o::gstmusic)
   (with-access::gstmusic o (%pipeline %mutex)
      (let ((closed (synchronize %mutex (music-closed? o))))
	 (unless closed
	    (call-next-method)
	    (synchronize %mutex
	       (when (isa? %pipeline gst-element)
		  (gst-element-state-set! %pipeline 'null)))))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::gstmusic ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::gstmusic)
   (with-access::gstmusic o (%pipeline)
      (not (isa? %pipeline gst-element))))

;*---------------------------------------------------------------------*/
;*    music-reset! ::gstmusic ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-reset! o::gstmusic)
   (music-close o))
   
;*---------------------------------------------------------------------*/
;*    music-playlist-get ::gstmusic ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get gstmusic::gstmusic)
   (with-access::gstmusic gstmusic (%playlist)
      %playlist))

;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::gstmusic ...                               */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! gstmusic::gstmusic n)
   (call-next-method)
   (with-access::gstmusic gstmusic (%mutex %playlist %status)
      (synchronize %mutex
	 (set! %playlist (append %playlist (list n)))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (set! playlistid (+fx 1 playlistid))
	    (set! playlistlength (+fx 1 playlistlength))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::gstmusic ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! gstmusic::gstmusic n)
   (with-access::gstmusic gstmusic (%mutex %playlist %status)
      (synchronize %mutex
	 (set! %playlist (delete! n %playlist string=?))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (when (and (>=fx n 0) (<fx n playlistlength))
	       (set! %playlist (remq! (list-ref %playlist n) %playlist))
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (length %playlist)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::gstmusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! gstmusic::gstmusic)
   (with-access::gstmusic gstmusic (%mutex %playlist %status)
      (synchronize %mutex
	 (set! %playlist '())
	 (with-access::musicstatus %status (playlistlength song)
	    (set! song 0)
	    (set! playlistlength 0)))))

;*---------------------------------------------------------------------*/
;*    set-song! ...                                                    */
;*---------------------------------------------------------------------*/
(define (set-song! o i)
   (with-access::gstmusic o (%status %playlist)
      (cond
	 ((<fx i 0)
	  (raise
	   (instantiate::&io-error
	      (proc 'set-song!)
	      (msg (format "No such song: ~a" i))
	      (obj %playlist))))
	 ((>=fx i (length %playlist))
	  #f)
	 (else
	  (let ((m (list-ref %playlist i)))
	     (with-access::musicstatus %status (song)
		(set! song i))
	     m)))))

;*---------------------------------------------------------------------*/
;*    gstmm-charset-convert ...                                        */
;*---------------------------------------------------------------------*/
(define-macro (gstmm-charset-convert str)
   (case (os-charset)
      ((UTF-8)
       str)
      ((UCS-2)
       `(utf8-string->ucs2-string ,str))
      ((CP-1252)
       `(utf8->cp1252 ,str))
      (else
       `(utf8->iso-latin ,str))))

;*---------------------------------------------------------------------*/
;*    music-play ::gstmusic ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-play o::gstmusic . s)
   
   (define (update-song-status! o n)
      (with-access::gstmusic o (%status onstate onvolume)
	 (with-access::musicstatus %status (state song songpos songid songlength playlistid volume)
	    (set! songpos 0)
	    (set! songlength 0)
	    (set! song n)
	    (set! songid (+fx (* 100 playlistid) n))
	    (onvolume o volume))))
   
   (define (init-url o::gstmusic url::bstring playlist)
      (with-access::gstmusic o (%mutex %pipeline %audiosrc)
	 (let ((uri (gstmm-charset-convert url)))
	    (gst-element-state-set! %pipeline 'null)
	    (gst-element-state-set! %pipeline 'ready)
	    (gst-object-property-set! %audiosrc :uri uri)
	    (gst-element-state-set! %pipeline 'playing))))
   
   (define (play-playlist n)
      (with-access::gstmusic o (%playlist %gready %pipeline onevent %mutex)
	 (let ((playlist %playlist))
	    (when (and (>=fx n 0) (<fx n (length playlist)))
	       ;; wait the music player to be ready
	       (synchronize %mutex
		  (gstmusic-wait-ready! o)
		  (set! %gready #f))
	       ;; play the list of urls
	       (let ((urls (list-tail playlist n)))
		  (with-access::gstmusic o (%!gabort)
		     (let loop ((l urls)
				(n n))
			(let ((url (synchronize %mutex
				      (unless %!gabort
					 (when (pair? l)
					    (update-song-status! o n)
					    (init-url o (car l)
					       (and (eq? l urls) urls)))))))
			   (when %!gabort
			      (when playlist (onevent o 'playlist playlist))
			      (music-play-loop o)
			      (loop (cdr l) (+fx 1 n)))))))))))
   
   (define (resume-from-pause o)
      (with-access::gstmusic o (%mutex %gcondv %pipeline %status)
	 (synchronize %mutex
	    (with-access::musicstatus %status (state)
	       (if (eq? state 'pause)
		   (begin
		      (gst-element-state-set! %pipeline 'playing)
		      #t)
		   (begin
		      (gst-element-state-set! %pipeline 'paused)
		      #f))))))
   
   (with-access::gstmusic o (%mutex %gcondv %pipeline %audiosrc %status %gready)
      (unless (isa? %pipeline gst-element)
	 (error "music-play ::gstmusic" "Player closed (or badly initialized)" o))
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
	 (synchronize %mutex
	    (set! %gready #t)
	    (condition-variable-signal! %gcondv)))))

;*---------------------------------------------------------------------*/
;*    gstmusic-wait-ready! ...                                         */
;*---------------------------------------------------------------------*/
(define (gstmusic-wait-ready! o::gstmusic)
   ;; %amutex already locked
   (with-access::gstmusic o (%pipeline %gready %gcondv %mutex %!gabort)
      (when (isa? %pipeline gst-element)
	 (gst-element-state-set! %pipeline 'null)
	 (gst-element-state-set! %pipeline 'ready))
      (unless %gready
	 (set! %!gabort #t)
	 (let loop ()
	    (unless %gready
	       ;; keep waiting
	       (condition-variable-wait! %gcondv %mutex)
	       (loop))))
      (set! %!gabort #f)
      (set! %gready #t)))

;*---------------------------------------------------------------------*/
;*    music-play-loop ...                                              */
;*---------------------------------------------------------------------*/
(define (music-play-loop o::gstmusic)
   (with-access::gstmusic o (%mutex %pipeline %status %meta %!gabort
			       onstate onevent onerror)
      (with-access::musicstatus %status (volume playlistid state
					   song songpos songlength
					   bitrate err playlistlength)
	 (set! state 'init)
	 (with-access::gst-pipeline %pipeline (bus)
	    ;; store the current volume level
	    (music-volume-get o)
	    (let loop ((vol volume)
		       (pid playlistid)
		       (meta #f))
	       (let* ((msg (gst-bus-poll bus))
		      (nvol volume)
		      (npid playlistid))
		  (cond
		     (%!gabort
		      ;; time out
		      (set! state 'stop)
		      (onstate o %status))
		     ((gst-message-eos? msg)
		      ;; end of stream
		      (synchronize %mutex
			 (set! state 'ended)
			 (set! songpos 0)
			 (set! %meta '())
			 (set! meta #f))
		      (onstate o %status))
		     ((gst-message-state-changed? msg)
		      ;; state changed
		      (let ((nstate (case (gst-message-new-state msg)
				       ((playing) 'play)
				       ((paused) 'pause)
				       ((ready) 'stop)
				       ((null) 'stop))))
			 (unless (eq? nstate state)
			    (set! state nstate)
			    (when (isa? %pipeline gst-element)
			       (set! songpos (music-position o))
			       (set! songlength (music-duration o)))
			    (onstate o %status)
			    ;; at this moment we don't know if we will
			    ;; see tags, so we emit a fake onmeta
			    (when (and (not meta) (eq? state 'play))
			       (let* ((plist (music-playlist-get o))
				      (file (list-ref plist song)))
				  (onevent o 'meta (or (file-musictag file) file)))))
			 (begin
			    (when (and (eq? nstate 'play) (>=fx volume 0))
			       ;; Some gstreamer player are wrong and
			       ;; tend to forget the volume level. As a
			       ;; workaround, we enforce it each time we
			       ;; receive a play state change message.
			       (music-volume-set! o volume))))
		      (loop nvol npid meta))
		     ((gst-message-tag? msg)
		      ;; tag found
		      (let ((notify #f))
			 (for-each (lambda (tag)
				      (let ((key (string->symbol (car tag))))
					 (case key
					    ((bitrate)
					     (set! bitrate
						(elong->fixnum
						   (/elong (cdr tag) #e1000))))
					    ((artist title album year)
					     (set! notify #t)
					     (set! %meta
						(cons (cons key (cdr tag))
						   %meta)))
					    (else
					     #unspecified))))
			    (gst-message-tag-list msg))
			 (when notify
			    (set! meta #t)
			    (onevent o 'meta %meta)))
		      (loop nvol npid meta))
		     ((gst-message-warning? msg)
		      ;; warning
		      (set! err (gst-message-warning-string msg))
		      (onerror o err)
		      (loop nvol npid meta))
		     ((gst-message-error? msg)
		      ;; error
		      (set! err (gst-message-error-string msg))
		      (onerror o err)
		      (loop nvol npid meta))
		     (else
		      (loop nvol npid meta)))))))))

;*---------------------------------------------------------------------*/
;*    music-seek ::gstmusic ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::gstmusic pos . song)
   (with-access::gstmusic o (%mutex %pipeline)
      (synchronize %mutex
	 (when (pair? song)
	    (if (not (integer? (car song)))
		(bigloo-type-error "music-seek ::gstmusic" 'int (car song))
		(set-song! o (car song))))
	 (when (isa? %pipeline gst-element)
	    (gst-element-seek %pipeline
	       (*llong (fixnum->llong pos) #l1000000000))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::gstmusic ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::gstmusic)
   (with-access::gstmusic o (%mutex %pipeline)
      (synchronize %mutex
	 (when (isa? %pipeline gst-element)
	    (gst-element-state-set! %pipeline 'null)
	    (gst-element-state-set! %pipeline 'ready)))))
   
;*---------------------------------------------------------------------*/
;*    music-pause ::gstmusic ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::gstmusic)
   (with-access::gstmusic o (%mutex %pipeline %status)
      (synchronize %mutex
	 (when (isa? %pipeline gst-element)
	    (with-access::musicstatus %status (state)
	       (if (eq? state 'pause)
		   (gst-element-state-set! %pipeline 'playing)
		   (gst-element-state-set! %pipeline 'paused)))))))

;*---------------------------------------------------------------------*/
;*    music-position ...                                               */
;*---------------------------------------------------------------------*/
(define (music-position o)
   ;; this function assumes that %pipeline is still valid (i.e., the
   ;; gstmm music player has not been closed yet)
   (with-access::gstmusic o (%pipeline)
      (llong->fixnum
	 (/llong (gst-element-query-position %pipeline) #l1000000000))))

;*---------------------------------------------------------------------*/
;*    music-duration ...                                               */
;*---------------------------------------------------------------------*/
(define (music-duration o)
   ;; this function assumes that %pipeline is still valid (i.e., the
   ;; gstmm music player has not been closed yet)
   (with-access::gstmusic o (%pipeline)
      (llong->fixnum
	 (/llong (gst-element-query-duration %pipeline) #l1000000000))))

;*---------------------------------------------------------------------*/
;*    music-song ::gstmusic ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-song o::gstmusic)
   (with-access::gstmusic o (%mutex %status)
      (synchronize %mutex
	 (with-access::musicstatus %status (song) song))))

;*---------------------------------------------------------------------*/
;*    music-songpos ::gstmusic ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-songpos o::gstmusic)
   ;; this function assumes that %pipeline is still valid (i.e., the
   ;; gstmm music player has not been closed yet)
   (with-access::gstmusic o (%pipeline)
      (llong->fixnum
	 (/llong (gst-element-query-position %pipeline) #l1000000000))))

;*---------------------------------------------------------------------*/
;*    music-meta ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-meta o::gstmusic)
   (with-access::gstmusic o (%meta)
      %meta))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::gstmusic ...                                  */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::gstmusic)
   (with-access::gstmusic o (%status %audiomixer)
      (if (isa? %audiomixer gst-element)
	  (let ((vol (inexact->exact
		      (* 100 (gst-object-property %audiomixer :volume)))))
	     (with-access::musicstatus %status (volume)
		(set! volume vol))
	     vol)
	  0)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::gstmusic ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::gstmusic vol)
   (with-access::gstmusic o (%status %audiomixer)
      (when (isa? %audiomixer gst-element)
	 (gst-object-property-set! %audiomixer :volume (/ vol 100))
	 (with-access::musicstatus %status (volume)
	    (set! volume vol)))))
