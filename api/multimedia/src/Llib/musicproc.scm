;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/src/Llib/musicproc.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 10 10:45:58 2007                          */
;*    Last change :  Sun Nov 18 15:06:37 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The MUSICPROC abstract class for "external" music players        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-musicproc
   
   (import __multimedia-music)
   
   (export (class musicproc::music
	      (charset::symbol (default 'ISO-LATIN-1))
	      (%process (default #unspecified))
	      (%playlist::pair-nil (default '()))
	      (%playid::int (default 0))
	      (%quote-uri::bool (default #f))
	      (%command-volume (default #unspecified))
	      (%command-stop (default #unspecified))
	      (%command-loadpaused (default #unspecified))
	      (%command-load (default #unspecified))
	      (%command-pause (default #unspecified))
	      (%command-seek-format (default #unspecified))
	      (%user-state::symbol (default 'play))
	      (%pmutex::mutex read-only (default (make-mutex)))
	      (%pcondv::condvar read-only (default (make-condition-variable)))
	      (%inexec::bool (default #f)))

	   (musicproc-exec ::musicproc ::bool ::bstring #!optional arg)
	   
	   (generic musicproc-loadpaused ::musicproc ::bstring)
	   (generic musicproc-load ::musicproc ::bstring)
	   (generic musicproc-start ::musicproc)
	   (generic musicproc-connect! ::musicproc)
	   (generic musicproc-parse ::musicproc)))

;*---------------------------------------------------------------------*/
;*    musicproc-exec ...                                               */
;*---------------------------------------------------------------------*/
(define (musicproc-exec o::musicproc wait command #!optional arg)
   
   (define (exec o)
      (with-access::musicproc o (%process)
	 (when (and (process? %process) (process-alive? %process))
	    (let ((p (process-input-port %process)))
	       (display command p)
	       (when arg
		  (display " " p) 
		  (display arg p))
	       (newline p)
	       (flush-output-port p)))))

   (define (parse)
      (with-handler
	 (lambda (e)
	    (exception-notify e))
	 (musicproc-parse o)))
   
   (with-access::musicproc o (%process %pmutex %pcondv %inexec)
      (if (not wait)
	  (exec o)
	  (unless (synchronize %pmutex
		     (if %inexec
			 (begin
			    (exec o)
			    (let loop ()
			       (when %inexec
				  (condition-variable-wait! %pcondv %pmutex)
				  (loop)))
			    #t)
			 (begin
			    (set! %inexec #t)
			    (exec o)
			    #f)))
	     (parse)
	     (synchronize %pmutex
		(set! %inexec #f)
		(condition-variable-broadcast! %pcondv))))))

;*---------------------------------------------------------------------*/
;*    musicproc-start ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-start o::musicproc))

;*---------------------------------------------------------------------*/
;*    music-status ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (music-status o::musicproc)
   (with-access::music o (%status)
      %status))

;*---------------------------------------------------------------------*/
;*    musicproc-connect! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-connect! o::musicproc)
   (with-access::musicproc o (%process)
      ;; start a new extern process
      (unless (and (process? %process) (process-alive? %process))
	 (set! %process (musicproc-start o)))))

;*---------------------------------------------------------------------*/
;*    music-close ::musicproc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-close o::musicproc)
   (with-access::musicproc o (%mutex %process)
      ;; abort the event loop
      (synchronize %mutex
	 (when %process
	    ;; quit the process
	    (musicproc-exec o #f "quit")
	    ;; kill the process
	    (when (and (process? %process) (process-alive? %process))
	       (process-kill %process))
	    (set! %process #f)))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::musicproc ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::musicproc)
   (with-access::musicproc o (%process)
      (not %process)))

;*---------------------------------------------------------------------*/
;*    music-reset! ::musicproc ...                                     */
;*---------------------------------------------------------------------*/
(define-method (music-reset! musicproc::musicproc)
   (music-close musicproc))

;*---------------------------------------------------------------------*/
;*    music-playlist-get ::musicproc ...                               */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-get o::musicproc)
   (with-access::musicproc o (%playlist)
      %playlist))

;*---------------------------------------------------------------------*/
;*    music-playlist-add! ::musicproc ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-add! o::musicproc s)
   (call-next-method)
   (with-access::musicproc o (%mutex %playlist %status)
      (synchronize %mutex
	 (set! %playlist (append! %playlist (list s)))
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (set! playlistid (+fx 1 playlistid))
	    (set! playlistlength (+fx 1 playlistlength))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::musicproc ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! o::musicproc n)
   (with-access::musicproc o (%mutex %playlist %status)
      (synchronize %mutex
	 (with-access::musicstatus %status (playlistid playlistlength)
	    (when (and (>=fx n 0) (<fx n playlistlength))
	       (set! %playlist (remq! (list-ref %playlist n) %playlist))
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (length %playlist)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::musicproc ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! o::musicproc)
   (with-access::musicproc o (%mutex %playlist %status)
      (synchronize %mutex
	 (set! %playlist '())
	 (with-access::musicstatus %status (playlistlength song songid)
	    (set! song 0)
	    (set! songid 0)
	    (set! playlistlength 0)))))

;*---------------------------------------------------------------------*/
;*    musicproc-loadpaused ::musicproc ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-loadpaused o::musicproc m::bstring)
   (with-access::musicproc o (%process %mutex %command-loadpaused %quote-uri)
      (let ((uri (if %quote-uri
		     (string-append "\"" m "\"")
		     m)))
	 (musicproc-exec o #t %command-loadpaused uri))))

;*---------------------------------------------------------------------*/
;*    musicproc-load ::musicproc ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-load o::musicproc m::bstring)
   (with-access::musicproc o (%process %mutex %command-load %quote-uri)
      (let ((uri (if %quote-uri
		     (string-append "\"" m "\"")
		     m)))
	 (musicproc-exec o #t %command-load uri))))

;*---------------------------------------------------------------------*/
;*    playlist-load-inner! ...                                         */
;*---------------------------------------------------------------------*/
(define (playlist-load-inner! o i proccmd)
   (with-access::musicproc o (%playlist %playid %status %user-state %mutex charset onevent)
      (with-access::musicstatus %status (song songid songpos songlength playlistlength playlistid)
	 (if (or (<fx i 0) (>=fx i playlistlength))
	     (raise
		(instantiate::&io-error
		   (proc 'playlist-load!)
		   (msg (format "No such song: ~a" i))
		   (obj %playlist)))
	     (let ((playlist %playlist)
		   (playid (+fx 1 %playid)))
		(set! %playid playid)
		(let loop ((i i)
			   (pid playlistid))
		   (unless (eq? %user-state 'stop)
		      (when (<fx i playlistlength)
			 (let ((m (list-ref playlist i)))
			    (set! song i)
			    (set! songid i)
			    (set! songpos 0)
			    (set! songlength 0)
			    (mutex-unlock! %mutex)
			    (when pid (onevent o 'playlist pid))
			    (with-handler
			       (lambda (e) #f)
			       (proccmd o (music-charset-convert m charset)))
			    (mutex-lock! %mutex)
			    (when (=fx %playid playid)
			       (loop (+fx i 1) #f)))))))))))
   
;*---------------------------------------------------------------------*/
;*    playlist-load! ...                                               */
;*---------------------------------------------------------------------*/
(define (playlist-load! o i)
   (with-access::musicproc o (%command-stop %process)
      (musicproc-exec o #t %command-stop)
      (playlist-load-inner! o i musicproc-load)))

;*---------------------------------------------------------------------*/
;*    music-play ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-play o::musicproc . s)
   (with-access::musicproc o (%mutex %process %user-state %status
				%command-pause %command-stop)
      (with-access::musicstatus %status (song playlistlength)
	 (synchronize %mutex
	    (cond
	       ((pair? s)
		(unless (integer? (car s))
		   (bigloo-type-error '|music-play ::musicproc| 'int (car s)))
		(set! %user-state 'play)
		(musicproc-connect! o)
		(playlist-load! o (car s)))
	       ((eq? %user-state 'pause)
		(musicproc-connect! o)
		(musicproc-exec o #f %command-pause)
		(set! %user-state 'play))
	       ((and (>=fx song 0) (<fx song playlistlength))
		(set! %user-state 'play)
		(musicproc-connect! o)
		(playlist-load! o song)))))))

;*---------------------------------------------------------------------*/
;*    music-seek ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::musicproc pos . s)
   
   (define (playlist-load-paused! o i)
      (playlist-load-inner! o i musicproc-loadpaused))
   
   (with-access::musicproc o (%mutex %process %command-seek-format %command-pause %user-state)
      (synchronize %mutex
	 (musicproc-connect! o)
	 (if (pair? s)
	     (if (not (integer? (car s)))
		 (bigloo-type-error '|music-seek ::musicproc| 'int (car s))
		 (begin
		    (playlist-load-paused! o (car s))
		    (musicproc-exec o #f (format %command-seek-format pos))
		    (musicproc-exec o #f %command-pause)))
	     (musicproc-exec o #f (format %command-seek-format pos)))
	 (set! %user-state 'play))))

;*---------------------------------------------------------------------*/
;*    music-stop ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::musicproc)
   (with-access::musicproc o (%mutex %process %command-stop %user-state)
      (synchronize %mutex
	 (musicproc-connect! o)
	 (musicproc-exec o #t %command-stop)
	 (set! %user-state 'stop))))

;*---------------------------------------------------------------------*/
;*    music-pause ::musicproc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::musicproc)
   (with-access::musicproc o (%mutex %process %command-pause %user-state)
      (synchronize %mutex
	 (if (eq? %user-state 'pause)
	     (set! %user-state 'play)
	     (set! %user-state 'pause))
	 (musicproc-connect! o)
	 (musicproc-exec o #f %command-pause))))

;*---------------------------------------------------------------------*/
;*    music-next ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-next o::musicproc)
   (with-access::musicproc o (%mutex %process %command-stop %user-state %status)
      (with-access::musicstatus %status (song playlistlength)
	 (synchronize %mutex
	    (unless (>=fx song (-fx playlistlength 1))
	       (musicproc-connect! o)
	       (musicproc-exec o #t %command-stop)
	       (set! %user-state 'play)
	       (playlist-load! o (+fx 1 song)))))))

;*---------------------------------------------------------------------*/
;*    music-prev ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-prev o::musicproc)
   (with-access::musicproc o (%mutex %process %command-stop %user-state %status)
      (with-access::musicstatus %status (song playlistlength)
	 (synchronize %mutex
	    (unless (or (<=fx song 0) (=fx playlistlength 0))
	       (musicproc-connect! o)
	       (musicproc-exec o #t %command-stop)
	       (set! %user-state 'play)
	       (playlist-load! o (-fx song 1)))))))

;*---------------------------------------------------------------------*/
;*    music-crossfade ::musicproc ...                                  */
;*---------------------------------------------------------------------*/
(define-method (music-crossfade o::musicproc sec::int)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    music-random-set! ::musicproc ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-random-set! o::musicproc flag::bool)
   (with-access::musicproc o (%status)
      (with-access::musicstatus %status (random)
	 (set! random flag))))

;*---------------------------------------------------------------------*/
;*    music-repeat-set! ::musicproc ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-repeat-set! o::musicproc flag::bool)
   (with-access::musicproc o (%status)
      (with-access::musicstatus %status (repeat)
	 (set! repeat flag))))

;*---------------------------------------------------------------------*/
;*    music-song ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-song o::musicproc)
   (with-access::musicproc o (%mutex %playlist %status)
      (with-access::musicstatus %status (song)
	 (synchronize %mutex
	    (if (pair? %playlist)
		song
		0)))))

;*---------------------------------------------------------------------*/
;*    music-songpos ::musicproc ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-songpos o::musicproc)
   (with-access::musicproc o (%mutex %status)
      (synchronize %mutex
	 (with-access::musicstatus %status (songpos)
	    songpos))))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::musicproc ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::musicproc)
   (with-access::musicproc o (%status)
      (with-access::musicstatus %status (volume)
	 volume)))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::musicproc ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::musicproc v)
   (with-access::musicproc o (%mutex %status %process %command-volume onvolume)
      (synchronize %mutex
	 (musicproc-connect! o)
	 (musicproc-exec o #f %command-volume v)
	 (with-access::musicstatus %status (volume)
	    (set! volume v)))
      (onvolume o v)
      v))

;*---------------------------------------------------------------------*/
;*    musicproc-parse ::musicproc ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-parse o::musicproc))
   

      
