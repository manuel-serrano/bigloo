;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/src/Llib/musicproc.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 10 10:45:58 2007                          */
;*    Last change :  Sat Jan  3 19:50:49 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The MUSICPROC abstract class for "external" music players        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-musicproc
   
   (import __multimedia-music
	   __multimedia-music-event-loop)
   
   (export (class musicproc::music
	      (charset::symbol (default 'ISO-LATIN-1))
	      (%process (default #unspecified))
	      (%playlist::pair-nil (default '()))
	      (%quote-uri::bool (default #f))
	      (%command-volume (default #unspecified))
	      (%command-stop (default #unspecified))
	      (%command-loadpaused (default #unspecified))
	      (%command-load (default #unspecified))
	      (%command-pause (default #unspecified))
	      (%command-seek-format (default #unspecified))
	      (%user-state::symbol (default 'play)))

	   (musicproc-exec ::obj ::bstring #!optional arg)
	   
	   (generic musicproc-loadpaused ::musicproc ::bstring)
	   (generic musicproc-load ::musicproc ::bstring)
	   (generic musicproc-start ::musicproc)
	   (generic musicproc-connect! ::musicproc)))

;*---------------------------------------------------------------------*/
;*    musicproc-exec ...                                               */
;*    -------------------------------------------------------------    */
;*    The mutex must already be locked.                                */
;*---------------------------------------------------------------------*/
(define (musicproc-exec proc command #!optional arg)
   (when (and (process? proc) (process-alive? proc))
      (let ((p (process-input-port proc)))
	 (display command p)
	 (tprint "* " command " " (if arg arg ""))
	 (when arg
	    (display " " p) 
	    (display arg p))
	 (newline p)
	 (flush-output-port p))))

;*---------------------------------------------------------------------*/
;*    musicproc-start ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-start o::musicproc))

;*---------------------------------------------------------------------*/
;*    music-status ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (music-status o::musicproc)
   (music-%status o))

;*---------------------------------------------------------------------*/
;*    musicproc-connect! ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-connect! o::musicproc)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
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
      (music-event-loop-abort! o)
      (with-lock %mutex
	 (lambda ()
	    (when %process
	       ;; quit the process
	       (musicproc-exec %process "quit")
	       ;; kill the process
	       (when (and (process? %process) (process-alive? %process))
		  (tprint "music-close...process-kill")
		  (process-kill %process))
	       (set! %process #f))))))

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
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist (append! %playlist (list s)))
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (set! playlistid (+fx 1 playlistid))
	       (set! playlistlength (+fx 1 playlistlength)))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-delete! ::musicproc ...                           */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-delete! o::musicproc n)
   (with-access::musicproc o (%mutex %playlist %status)
      (with-lock %mutex
	 (lambda ()
	    (with-access::musicstatus %status (playlistid playlistlength)
	       (when (and (>=fx n 0) (<fx n playlistlength))
		  (set! %playlist (remq! (list-ref %playlist n) %playlist))
		  (set! playlistid (+fx 1 playlistid))
		  (set! playlistlength (length %playlist))))))))

;*---------------------------------------------------------------------*/
;*    music-playlist-clear! ::musicproc ...                            */
;*---------------------------------------------------------------------*/
(define-method (music-playlist-clear! o::musicproc)
   (with-access::musicproc o (%mutex %playlist %status)
      (with-lock %mutex
	 (lambda ()
	    (set! %playlist '())
	    (with-access::musicstatus %status (playlistlength song songid)
	       (set! song 0)
	       (set! songid 0)
	       (set! playlistlength 0))))))

;*---------------------------------------------------------------------*/
;*    musicproc-loadpaused ::musicproc ...                             */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-loadpaused o::musicproc m::bstring)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
   (with-access::musicproc o (%process %mutex %command-loadpaused %quote-uri)
      (let ((uri (if %quote-uri
		     (string-append "\"" m "\"")
		     m)))
	 (musicproc-exec %process %command-loadpaused uri))))

;*---------------------------------------------------------------------*/
;*    musicproc-load ::musicproc ...                                   */
;*---------------------------------------------------------------------*/
(define-generic (musicproc-load o::musicproc m::bstring)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
   (with-access::musicproc o (%process %mutex %command-load %quote-uri)
      (let ((uri (if %quote-uri
		     (string-append "\"" m "\"")
		     m)))
	 (musicproc-exec %process %command-load uri))))

;*---------------------------------------------------------------------*/
;*    playlist-load-inner! ...                                         */
;*---------------------------------------------------------------------*/
(define (playlist-load-inner! o i proccmd)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
   (with-access::musicproc o (%playlist %status charset)
      (with-access::musicstatus %status (song songid playlistlength)
	 (if (or (<fx i 0) (>=fx i playlistlength))
	     (raise
	      (instantiate::&io-error
		 (proc 'playlist-load!)
		 (msg (format "No such song: ~a" i))
		 (obj %playlist)))
	     (let ((m (list-ref %playlist i)))
		(set! song i)
		(set! songid i)
		(proccmd o (music-charset-convert m charset))
		m)))))
   
;*---------------------------------------------------------------------*/
;*    playlist-load-paused! ...                                        */
;*---------------------------------------------------------------------*/
(define (playlist-load-paused! o i)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
   (playlist-load-inner! o i musicproc-loadpaused))

;*---------------------------------------------------------------------*/
;*    playlist-load! ...                                               */
;*---------------------------------------------------------------------*/
(define (playlist-load! o i)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
   (playlist-load-inner! o i musicproc-load))

;*---------------------------------------------------------------------*/
;*    music-play ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-play o::musicproc . s)
   (with-access::musicproc o (%mutex %process %playlist %user-state)
      (with-access::musicstatus (musicproc-%status o) (song playlistlength)
	 (with-lock %mutex
	    (lambda ()
	       (set! %user-state 'play)
	       (musicproc-connect! o)
	       (cond
		  ((pair? s)
		   (unless (integer? (car s))
		      (bigloo-type-error '|music-play ::musicproc| 'int (car s)))
		   (playlist-load! o (car s)))
		  ((and (>=fx song 0) (<fx song playlistlength))
		   (playlist-load! o song))))))))

;*---------------------------------------------------------------------*/
;*    music-seek ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::musicproc pos . s)
   (with-access::musicproc o (%mutex %process %command-seek-format %command-pause %user-state)
      (with-lock %mutex
	 (lambda ()
	    (set! %user-state 'play)
	    (musicproc-connect! o)
	    (if (pair? s)
		(if (not (integer? (car s)))
		    (bigloo-type-error '|music-seek ::musicproc| 'int (car s))
		    (begin
		       (playlist-load-paused! o (car s))
		       (musicproc-exec %process (format %command-seek-format pos))
		       (musicproc-exec %process %command-pause)))
		(musicproc-exec %process (format %command-seek-format pos)))))))

;*---------------------------------------------------------------------*/
;*    music-stop ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-stop o::musicproc)
   (with-access::musicproc o (%mutex %process %command-stop %user-state)
      (with-lock %mutex
	 (lambda ()
	    (set! %user-state 'stop)
	    (musicproc-connect! o)
	    (musicproc-exec %process %command-stop)))))

;*---------------------------------------------------------------------*/
;*    music-pause ::musicproc ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::musicproc)
   (with-access::musicproc o (%mutex %process %command-pause %user-state)
      (with-lock %mutex
	 (lambda ()
	    (if (eq? %user-state 'pause)
		(set! %user-state 'play)
		(set! %user-state 'pause))
	    (musicproc-connect! o)
	    (musicproc-exec %process %command-pause)))))

;*---------------------------------------------------------------------*/
;*    music-next ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-next o::musicproc)
   (with-access::musicproc o (%mutex %process %playlist %user-state)
      (with-access::musicstatus (musicproc-%status o) (song playlistlength)
	 (with-lock %mutex
	    (lambda ()
	       (set! %user-state 'play)
	       (unless (>=fx song (-fx playlistlength 1))
		  (musicproc-connect! o)
		  (playlist-load! o (+fx 1 song))))))))

;*---------------------------------------------------------------------*/
;*    music-prev ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-prev o::musicproc)
   (with-access::musicproc o (%mutex %process %playlist %user-state)
      (with-access::musicstatus (musicproc-%status o) (song)
	 (with-lock %mutex
	    (lambda ()
	       (set! %user-state 'play)
	       (unless (or (<=fx song 0) (null? %playlist))
		  (musicproc-connect! o)
		  (playlist-load! o (-fx song 1))))))))

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
      (musicstatus-random-set! %status flag)))

;*---------------------------------------------------------------------*/
;*    music-repeat-set! ::musicproc ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-repeat-set! o::musicproc flag::bool)
   (with-access::musicproc o (%status)
      (musicstatus-repeat-set! %status flag)))

;*---------------------------------------------------------------------*/
;*    music-song ::musicproc ...                                       */
;*---------------------------------------------------------------------*/
(define-method (music-song o::musicproc)
   (with-access::musicproc o (%mutex %playlist)
      (with-access::musicstatus (musicproc-%status o) (song)
	 (with-lock %mutex
	    (lambda ()
	       (if (pair? %playlist)
		   song
		   0))))))

;*---------------------------------------------------------------------*/
;*    music-songpos ::musicproc ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-songpos o::musicproc)
   (with-access::musicproc o (%mutex %status)
      (with-lock %mutex
	 (lambda ()
	    (musicstatus-songpos %status)))))

;*---------------------------------------------------------------------*/
;*    music-volume-get ::musicproc ...                                 */
;*---------------------------------------------------------------------*/
(define-method (music-volume-get o::musicproc)
   (with-access::musicstatus (musicproc-%status o) (volume)
      volume))

;*---------------------------------------------------------------------*/
;*    music-volume-set! ::musicproc ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-volume-set! o::musicproc v)
   (with-access::musicproc o (%mutex %status %process %command-volume)
      (with-lock %mutex
	 (lambda ()
	    (musicproc-connect! o)
	    (musicproc-exec %process %command-volume v)
	    (musicstatus-volume-set! %status v)))
      v))

;*---------------------------------------------------------------------*/
;*    music-event-loop-abort! ::musicproc ...                          */
;*---------------------------------------------------------------------*/
(define-method (music-event-loop-abort! o::musicproc)
   (with-access::musicproc o (%process %mutex %abort-loop %loop-mutex %loop-condv)
      (mutex-lock! %loop-mutex)
      (with-lock %mutex
	 (lambda ()
	    (set! %abort-loop #t)
	    (when (process? %process)
	       (process-kill %process)
	       (set! %process #f))))
      (condition-variable-wait! %loop-condv %loop-mutex)
      (mutex-unlock! %loop-mutex)))
      
   
