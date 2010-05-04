;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/mplayer.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 10 10:45:58 2007                          */
;*    Last change :  Wed Mar 10 07:50:39 2010 (serrano)                */
;*    Copyright   :  2007-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The MPLAYER Bigloo binding                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mplayer
   
   (import __multimedia-music
	   __multimedia-musicproc
	   __multimedia-music-event-loop)
   
   (export (class mplayer::musicproc
	      (frequency::long (default 2000000))
	      (path::bstring read-only (default "mplayer"))
	      (args::pair-nil read-only (default '("-vo" "null" "-quiet" "-slave" "-idle")))
	      (ao::obj read-only (default #unspecified))
	      (ac::obj read-only (default #unspecified))
	      (%close::bool (default #f))
	      (%result-acknowledge::bstring read-only (default "MPlayer ")))))

;*---------------------------------------------------------------------*/
;*    music-init ::mplayer ...                                         */
;*---------------------------------------------------------------------*/
(define-method (music-init o::mplayer)
   (with-access::mplayer o (%command-volume
			    %command-pause
			    %command-seek-format
			    %command-load
			    %command-stop
			    %quote-uri)
      (call-next-method)
      (set! %quote-uri #t)
      (unless (string? %command-volume)
	 (set! %command-volume "pausing_keep set_property volume"))
      (unless (string? %command-pause)
	 (set! %command-pause "pause"))
      (unless (string? %command-load)
	 (set! %command-load "loadfile"))
      (unless (string? %command-seek-format)
	 (set! %command-seek-format "pausing_keep seek ~a 2"))
      (unless (string? %command-stop)
	 (set! %command-stop "loadfile stop"))))

;*---------------------------------------------------------------------*/
;*    musicproc-loadpaused ::mplayer ...                               */
;*---------------------------------------------------------------------*/
(define-method (musicproc-loadpaused o::mplayer m::bstring)
   [assert (o) (not (symbol? (mutex-state (musicproc-%mutex o))))]
   (with-access::musicproc o (%mutex %process %command-load %command-pause)
      (musicproc-exec %process %command-load m)
      (musicproc-exec %process %command-pause m)))

;*---------------------------------------------------------------------*/
;*    music-close ::mplayer ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-close o::mplayer)
   (with-access::mplayer o (%mutex %close)
      (call-next-method)
      (mutex-lock! %mutex)
      (set! %close #t)
      (mutex-unlock! %mutex)))

;*---------------------------------------------------------------------*/
;*    music-closed? ::mplayer ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::mplayer)
   (with-access::mplayer o (%close)
      %close))

;*---------------------------------------------------------------------*/
;*    number-grammar ...                                               */
;*---------------------------------------------------------------------*/
(define number-grammar
   (regular-grammar ()
      (#\space (ignore))
      ((+ digit) (the-fixnum))
      ((: (+ digit) #\. (* digit)) (the-fixnum))
      (else (the-failure))))

;*---------------------------------------------------------------------*/
;*    read-answer ...                                                  */
;*    -------------------------------------------------------------    */
;*    The mutex is already aquired.                                    */
;*---------------------------------------------------------------------*/
(define (read-answer prefix pi skip-blank-line)
   (let loop ()
      (let ((line (read-line pi)))
	 (cond
	    ((eof-object? line)
	     (raise (instantiate::&io-error
		       (proc 'read-answer)
		       (msg "Illegal end of file")
		       (obj line))))
	    ((=fx (string-length line) 0)
	     (if skip-blank-line
		 (loop)
		 (raise 'end-of-song)))
	    ((substring-ci-at? line prefix 0)
	     (substring line (string-length prefix) (string-length line)))
	    (else
	     (loop))))))

;*---------------------------------------------------------------------*/
;*    read-playing ...                                                 */
;*---------------------------------------------------------------------*/
(define (read-playing o)
   [assert (o) (not (symbol? (mutex-state (mplayer-%mutex o))))]
   (with-access::mplayer o (%process frequency)
      (let ((p (process-output-port %process)))
	 ;; timeout for input-port
	 (input-port-timeout-set! p (*fx frequency 10))
	 ;; flush the welcome output
	 (read-answer "Playing " p #t)
	 ;; timeout for input-port
	 (input-port-timeout-set! p (/fx frequency 2)))))

;*---------------------------------------------------------------------*/
;*    cmd-integer ...                                                  */
;*---------------------------------------------------------------------*/
(define (cmd-integer cmd ans po pi)
   (display "pausing_keep " pi)
   (display cmd pi)
   (newline pi)
   (flush-output-port pi)
   (let ((n (read-answer ans po #f)))
      (if (string? n)
	  (string->integer n)
	  0)))

;*---------------------------------------------------------------------*/
;*    cmd-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (cmd-string cmd ans po pi)
   (display "pausing_keep " pi)
   (display cmd pi)
   (newline pi)
   (flush-output-port pi)
   (let ((n (read-answer ans po #f)))
      (when (string? n)
	 (substring n 1 (-fx (string-length n) 1)))))

;*---------------------------------------------------------------------*/
;*    cmd-value ...                                                    */
;*---------------------------------------------------------------------*/
(define (cmd-value cmd ans po pi)
   (display "pausing_keep " pi)
   (display cmd pi)
   (newline pi)
   (flush-output-port pi)
   (read-answer ans po #f))

;*---------------------------------------------------------------------*/
;*    music-meta ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-meta o::mplayer)
   (with-access::mplayer o (%process %mutex)
      (with-lock %mutex
	 (lambda ()
	    (if (and (process? %process) (process-alive? %process))
		(let ((po (process-output-port %process))
		      (pi (process-input-port %process)))
		   `((path . ,(cmd-value "get_property path" "ANS_path=" po pi))
		     (title . ,(cmd-string "get_meta_title" "ANS_META_TITLE=" po pi))
		     (track . ,(cmd-integer "get_meta_track" "ANS_META_TRACK=" po pi))
		     (year . ,(cmd-integer "get_meta_year" "ANS_META_YEAR=" po pi))
		     (artist . ,(cmd-string "get_meta_artist" "ANS_META_ARTIST=" po pi))
		     (album . ,(cmd-string "get_meta_album" "ANS_META_ALBUM=" po pi))
		     (genre . ,(cmd-string "get_meta_genre" "ANS_META_GENRE=" po pi)))))))))

;*---------------------------------------------------------------------*/
;*    musicproc-start ...                                              */
;*---------------------------------------------------------------------*/
(define-method (musicproc-start o::mplayer)
   [assert (o) (not (symbol? (mutex-state (mplayer-%mutex o))))]
   (with-access::mplayer o (%process
			    path args ao ac charset
			    %result-acknowledge
			    %command-volume %status)
      (let ((proc (apply run-process
			 path
			 input: pipe: output: pipe: error: "/dev/null"
			 wait: #f fork: #t
			 (append args
				 (if (string? ao) (list "-ao" ao) '())
				 (if (string? ac) (list "-ac" ac) '())))))
	 (if (not (process-alive? proc))
	     (raise
	      (instantiate::&io-error
		 (proc 'mplayer-start)
		 (msg "Can't start process")
		 (obj (format "~a ~a" path args))))
	     (let* ((p (process-output-port proc))
		    (l (read-line p)))
		(set! %process proc)
		(if (not (substring-ci-at? l %result-acknowledge 0))
		    (raise
		     (instantiate::&io-parse-error
			(proc 'mplayer-start)
			(msg "Illegal MPLAYER acknowledge")
			(obj l)))
		    (let ((vol (musicstatus-volume %status)))
		       ;; the lock is already acquired so don't call volume-set!
		       (musicproc-exec proc %command-volume vol)
		       proc)))))))

;*---------------------------------------------------------------------*/
;*    music-play ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-play o::mplayer . s)
   (with-access::mplayer o (%user-state %mutex)
      (set! %user-state 'play)
      (when (call-next-method)
	 (with-lock %mutex
	    (lambda ()
	       (read-playing o))))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::mplayer)
   (with-access::mplayer o (%user-state %mutex)
      (mutex-lock! %mutex)
      (if (eq? %user-state 'pause)
	  (set! %user-state 'play)
	  (set! %user-state 'pause))
      (mutex-unlock! %mutex)))

;*---------------------------------------------------------------------*/
;*    music-seek ::mplayer ...                                         */
;*---------------------------------------------------------------------*/
(define-method (music-seek o::mplayer pos . s)
   (with-access::mplayer o (%mutex)
      (when (call-next-method)
	 (with-lock %mutex
	    (lambda ()
	       (when (pair? s) (read-playing o)))))))
   
;*---------------------------------------------------------------------*/
;*    music-next ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-next o::mplayer)
   (with-access::mplayer o (%mutex)
      (when (call-next-method)
	 (with-lock %mutex
	    (lambda ()
	       (read-playing o))))))

;*---------------------------------------------------------------------*/
;*    music-prev ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (music-prev o::mplayer)
   (with-access::mplayer o (%mutex)
      (when (call-next-method)
	 (with-lock %mutex
	    (lambda ()
	       (read-playing o))))))

;*---------------------------------------------------------------------*/
;*    music-update-status! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (music-update-status! o::mplayer status)
   
   (define (update-inner! o)
      (with-access::mplayer o (%process %status %user-state)
	 (with-access::musicstatus %status (volume
					    state err
					    songpos songlength 
					    bitrate khz
					    playlistid playlistlength
					    song songid)
	    (cond
	       ((not (process? %process))
		(set! err #f)
		(set! songpos 0)
		(set! songlength 0)
		(set! bitrate 0)
		(set! khz 0)
		(set! state 'stop))
	       ((not (process-alive? %process))
		(set! err (format "MPlayer process dead: ~a" %process))
		(set! state 'error))
	       ((eq? %user-state 'stop)
		(set! err #f)
		(set! songpos 0)
		(set! songlength 0)
		(set! bitrate 0)
		(set! khz 0)
		(set! state 'stop))
	       (else
		(let* ((po (process-output-port %process))
		       (pi (process-input-port %process))
		       (oslen songlength)
		       (ospos songpos)
		       (nslen (cmd-integer "get_time_length"
					   "ANS_LENGTH=" po pi))
		       (nspos (cmd-integer "get_time_pos"
					   "ANS_TIME_POSITION=" po pi))
		       (nbr (cmd-integer "get_audio_bitrate"
					 "ANS_AUDIO_BITRATE='" po pi))
		       (nvol (cmd-integer "get_property volume"
					  "ANS_volume=" po pi)))
		   (set! songpos nspos)
		   (if (and (<=fx nslen 0) (>fx nspos 0))
		       (set! songlength nspos)
		       (set! songlength nslen))
		   (set! err #f)
		   (set! volume nvol)
		   (set! bitrate nbr)
		   (set! state
			 (cond
			    ((eq? %user-state 'pause) 'pause)
			    ((=fx oslen nslen) 'play)
			    (else 'start))))))
	    (unless (eq? status %status)
	       (musicstatus-volume-set! status volume)
	       (musicstatus-state-set! status state)
	       (musicstatus-err-set! status err)
	       (musicstatus-song-set! status song)
	       (musicstatus-songid-set! status songid)
	       (musicstatus-songpos-set! status songpos)
	       (musicstatus-songlength-set! status songlength)
	       (musicstatus-bitrate-set! status bitrate)
	       (musicstatus-playlistid-set! status playlistid)
	       (musicstatus-playlistlength-set! status playlistlength)))
	 %status))

   (define (err e)
      (with-access::musicstatus (mplayer-%status o) (state err)
	 (set! state 'error)
	 (set! err (with-error-to-string (lambda () (error-notify e))))))
   
   (with-access::mplayer o (%process %status %user-state %mutex)
      (mutex-lock! %mutex)
      (let ((v (with-handler
		  (lambda (e) e)
		  (update-inner! o))))
	 (mutex-unlock! %mutex)
	 (cond
	    ((musicstatus? v)
	     v)
	    ((or (eq? v 'end-of-song) (&io-timeout-error? v))
	     (with-access::musicstatus %status (state err
						      song songpos songlength 
						      playlistlength
						      repeat (ran random))
		(if (and (>=fx songpos (-fx songlength 2))
			 (eq? %user-state 'play))
		    (with-handler
		       (lambda (e) (err e))
		       (cond
			  (repeat
			   (music-play o))
			  (ran
			   (music-play o (random playlistlength)))
			  (else
			   (music-next o))))
		    (set! state 'stop))))
	    (else
	     (err v))))))

