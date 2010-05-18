;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/mpg123.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 10 10:45:58 2007                          */
;*    Last change :  Wed Mar 10 07:51:15 2010 (serrano)                */
;*    Copyright   :  2007-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The MPG123 Bigloo binding                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mpg123
   
   (import __multimedia-music
	   __multimedia-musicproc
	   __multimedia-music-event-loop
	   __multimedia-id3)
   
   (export (class mpg123::musicproc
	      
	      (path::bstring read-only (default "mpg123"))
	      (args::pair-nil read-only (default '("--remote")))

	      (%result-acknowledge::bstring read-only (default "@R MPG123")))))

;*---------------------------------------------------------------------*/
;*    music-init ::mpg123 ...                                          */
;*---------------------------------------------------------------------*/
(define-method (music-init o::mpg123)
   (with-access::mpg123 o (%command-volume
			   %command-stop
			   %command-loadpaused
			   %command-pause
			   %command-seek-format
			   %command-load
			   %status)
      (call-next-method)
      (unless (eq? %status (musicstatus-nil))
	 (set! %status (instantiate::musicstatus)))
      (unless (string? %command-volume)
	 (set! %command-volume "VOLUME"))
      (unless (string? %command-stop)
	 (set! %command-stop "STOP"))
      (unless (string? %command-loadpaused)
	 (set! %command-loadpaused "LOADPAUSED"))
      (unless (string? %command-pause)
	 (set! %command-pause "PAUSE"))
      (unless (string? %command-seek-format)
	 (set! %command-seek-format "JUMP ~as"))
      (unless (string? %command-load)
	 (set! %command-load "LOAD"))))

;*---------------------------------------------------------------------*/
;*    musicproc-start ...                                              */
;*---------------------------------------------------------------------*/
(define-method (musicproc-start o::mpg123)
   [assert (o) (not (symbol? (mutex-state (mpg123-%mutex o))))]
   (with-access::mpg123 o (path args %result-acknowledge)
      (let ((proc (apply run-process
			 path
			 input: pipe: output: pipe: error: "/dev/null"
			 wait: #f fork: #t
			 args)))
	 (if (not (process-alive? proc))
	     (raise
	      (instantiate::&io-error
		 (proc 'mpg123-start)
		 (msg "Can't start process")
		 (obj (format "~a ~a" path args))))
	     (let ((l (read-line (process-output-port proc))))
		(if (not (substring-at? l %result-acknowledge 0))
		    (raise
		     (instantiate::&io-parse-error
			(proc 'mpg123-start)
			(msg "Illegal MPG123 acknowledge")
			(obj l)))
		    proc))))))

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
;*    mpg123-grammar ...                                               */
;*---------------------------------------------------------------------*/
(define mpg123-grammar
   (regular-grammar ((int (+ digit))
		     (float (: (+ digit) #\. (* digit)))
		     mpg123 onstate onmeta onerror onvolume)
      ("@E"
       (let ((reason (read-line (the-port)))
	     (terr #f))
	  (with-access::mpg123 mpg123 (%mutex %status)
	     (mutex-lock! %mutex)
	     (musicstatus-err-set! %status reason)
	     (mutex-unlock! %mutex)
	     (when onerror (onerror reason)))
	  (ignore)))
      ((: "@F")
       ;; Status message during playing
       (let* ((a (read/rp number-grammar (the-port)))
	      (b (read/rp number-grammar (the-port)))
	      (c (read/rp number-grammar (the-port)))
	      (d (read/rp number-grammar (the-port))))
	  (with-access::mpg123 mpg123 (%mutex %status)
	     (mutex-lock! %mutex)
	     (let ((tstate (not (eq? (musicstatus-state %status) 'play)))
		   (song (musicstatus-song %status)))
		(musicstatus-songpos-set! %status c)
		(musicstatus-songlength-set! %status (+fx c d))
		(musicstatus-state-set! %status 'play)
		(mutex-unlock! %mutex)
		(when tstate
		   (when onstate (onstate %status))
		   (when (and onmeta tstate)
		      (let* ((plist (music-playlist-get mpg123))
			     (file (list-ref plist song)))
			 (if (file-exists? file)
			     (let ((tag (mp3-id3 file)))
				(onmeta (or tag file)))
			     (onmeta file))))))))
       (ignore))
      ((: "@S ")
       ;; Stream info at beginning of playback
       (let* ((mpeg-type (read (the-port)))
	      (layer (read (the-port)))
	      (sampling (read (the-port)))
	      (mode (read (the-port)))
	      (mode-ext (read (the-port)))
	      (framesize (read (the-port)))
	      (stereo (read (the-port)))
	      (copyright (read (the-port)))
	      (err (read (the-port)))
	      (emphasis (read (the-port)))
	      (bitrate (read (the-port))))
	  (read-line (the-port))
	  (with-access::mpg123 mpg123 (%mutex %status)
	     (mutex-lock! %mutex)
	     (musicstatus-state-set! %status 'start)
	     (mutex-unlock! %mutex)
	     (when onstate (onstate %status))
	     (ignore))))
      ((: "@P " digit)
       ;; Playing status
       (with-access::mpg123 mpg123 (%mutex %status %user-state)
	  (case (integer->char (the-byte-ref 3))
	     ((#\0)
	      (mutex-lock! %mutex)
	      (musicstatus-state-set! %status 'stop)
	      (mutex-unlock! %mutex)
	      (when onstate (onstate %status))
	      (when (eq? %user-state 'play)
		 (cond
		    ((musicstatus-repeat %status)
		     (music-play mpg123))
		    ((musicstatus-random %status)
		     (music-play mpg123
				 (random (musicstatus-playlistlength %status))))
		    (else
		     (music-next mpg123)))))
	     ((#\1)
	      (mutex-lock! %mutex)
	      (musicstatus-state-set! %status 'pause)
	      (mutex-unlock! %mutex)
	      (when onstate (onstate %status)))
	     ((#\2)
	      (mutex-lock! %mutex)
	      (musicstatus-state-set! %status 'start)
	      (mutex-unlock! %mutex)
	      (when onstate (onstate %status)))))
       (ignore))
      ((: "@I" (+ all))
       (ignore))
      ((: "@V " (+ (or digit #\.)) #\%)
       (with-access::mpg123 mpg123 (%mutex %status)
	  (let ((v (inexact->exact
		    (round
		     (string->number (the-substring 3 -1))))))
	     (let ((t #f))
		(mutex-lock! %mutex)
		(set! t (=fx (musicstatus-volume %status) v))
		(musicstatus-volume-set! %status v)
		(mutex-unlock! %mutex)
		(when (and (not t) onvolume)
		   (onvolume v)))))
       (ignore))
      ((: "@" (or (in "JV") "RVA" "bass") (+ all))
       (ignore))
      ((: #\@ (in alpha))
       (tprint (the-string) (read-line (the-port)))
       (ignore))
      ((+ #\Newline)
       (ignore))
      (else
       (let ((c (the-failure)))
	  (unless (eof-object? c)
	     (raise
	      (instantiate::&io-parse-error
		 (proc 'mpg123)
		 (msg "Illegal response")
		 (obj (format "{~a}~a" c (read-line (the-port)))))))))))

;*---------------------------------------------------------------------*/
;*    music-update-status! ::mpg123 ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-update-status! o::mpg123 status)
   (mutex-lock! (mpg123-%mutex o))
   (with-access::mpg123 o (%status)
      (musicstatus-state-set! status (musicstatus-state %status))
      (musicstatus-volume-set! status (musicstatus-volume %status))
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
      (musicstatus-err-set! status (musicstatus-err %status)))
   (mutex-unlock! (mpg123-%mutex o)))
   
;*---------------------------------------------------------------------*/
;*    music-event-loop ::mpg123 ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-event-loop-inner o::mpg123 frequency::long onstate onmeta onerror onvol)
   (with-access::mpg123 o (%process %mutex %loop-mutex %abort-loop %status)
      (let loop ()
	 (if (process? %process)
	     (let ((p (process-output-port %process)))
		(with-lock %loop-mutex
		   (lambda ()
		      (read/rp mpg123-grammar p o onstate onmeta onerror onvol))))
	     (begin
		(mutex-lock! %mutex)
		(let ((abort %abort-loop))
		   (mutex-unlock! %mutex)
		   (unless (or abort (music-closed? o))
		      (sleep frequency)
		      (loop))))))))

;*---------------------------------------------------------------------*/
;*    music-can-play-type? ::mpg123 ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-can-play-type? m::mpg123 mimetype)
   (string=? mimetype "audio/mpeg"))
