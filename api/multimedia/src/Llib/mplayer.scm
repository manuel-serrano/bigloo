;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/mplayer.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 10 10:45:58 2007                          */
;*    Last change :  Thu Dec 13 19:25:21 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The MPLAYER Bigloo binding                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mplayer
   
   (import __multimedia-music
	   __multimedia-musicproc)
   
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
   (with-access::musicproc o (%mutex %process %command-load %command-pause)
      (musicproc-exec %process #f %command-load m)
      (musicproc-exec %process #t %command-pause m)))

;*---------------------------------------------------------------------*/
;*    music-close ::mplayer ...                                        */
;*---------------------------------------------------------------------*/
(define-method (music-close o::mplayer)
   (with-access::mplayer o (%mutex %close)
      (call-next-method)
      (synchronize %mutex
	 (set! %close #t))))

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
      (synchronize %mutex
	 (if (and (process? %process) (process-alive? %process))
	     (let ((po (process-output-port %process))
		   (pi (process-input-port %process)))
		`((path . ,(cmd-value "get_property path" "ANS_path=" po pi))
		  (title . ,(cmd-string "get_meta_title" "ANS_META_TITLE=" po pi))
		  (track . ,(cmd-integer "get_meta_track" "ANS_META_TRACK=" po pi))
		  (year . ,(cmd-integer "get_meta_year" "ANS_META_YEAR=" po pi))
		  (artist . ,(cmd-string "get_meta_artist" "ANS_META_ARTIST=" po pi))
		  (album . ,(cmd-string "get_meta_album" "ANS_META_ALBUM=" po pi))
		  (genre . ,(cmd-string "get_meta_genre" "ANS_META_GENRE=" po pi))))
	     '()))))

;*---------------------------------------------------------------------*/
;*    musicproc-start ...                                              */
;*---------------------------------------------------------------------*/
(define-method (musicproc-start o::mplayer)
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
		    (with-access::musicstatus %status (volume)
		       ;; the lock is already acquired so don't call volume-set!
		       (musicproc-exec o #f %command-volume volume)
		       proc)))))))

;*---------------------------------------------------------------------*/
;*    music-pause ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (music-pause o::mplayer)
   (with-access::mplayer o (%user-state %mutex %status %process onstate)
      (synchronize %mutex
	 (when (eq? %user-state 'play)
	    (musicproc-exec o #f "get_time_pos")))
      (call-next-method)
      (with-access::musicstatus %status (state)
	 (synchronize %mutex
	    (set! state %user-state))
	 (onstate o %status))))

;*---------------------------------------------------------------------*/
;*    musicproc-parse ::mplayer ...                                    */
;*---------------------------------------------------------------------*/
(define-method (musicproc-parse o::mplayer)
   (with-access::mplayer o (%process %status onstate onerror onvolume onevent)
      (when (process? %process)
	 (let ((p (process-output-port %process)))
	    (let loop ((eol #f))
	       (let ((line (read-line p)))
		  (cond
		     ((eof-object? line)
		      'eof)
		     ((=fx (string-length line) 0)
		      (unless eol (loop #f)))
		     ((substring-at? line "ANS_" 0)
		      (cond
;* 			 ((substring-at? line "ANS_ERROR=" 0)          */
;* 			  (loop eol))                                  */
			 ((substring-at? line "ANS_TIME_POSITION=" 0)
			  (let ((p (string->real (substring line 18))))
			     (with-access::mplayer o (%status)
				(with-access::musicstatus %status (songpos)
				   (set! songpos (flonum->fixnum (round p)))))
			     (loop eol)))
			 (else
			  (loop eol))))
		     ((string=? line "Playing stop.")
		      (with-access::mplayer o (%status)
			 (with-access::musicstatus %status (state song songpos songlength)
			    (set! state 'stop)
			    (onstate o %status))))
		     ((string=? line "Starting playback...")
		      (with-access::mplayer o (%status)
			 (with-access::musicstatus %status (state song songpos songlength)
			    (set! state 'play)
			    (onstate o %status)
			    (loop #t)
			    (set! state 'ended)
			    (onstate o %status))))
		     (else
		      (loop eol)))))))))
