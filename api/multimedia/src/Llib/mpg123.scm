;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/mpg123.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 10 10:45:58 2007                          */
;*    Last change :  Wed Jan 25 17:41:35 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The MPG123 Bigloo binding                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mpg123
   
   (import __multimedia-music
	   __multimedia-musicproc
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
      (unless (nil? %status)
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
		(if (not (and (string? l)
			      (substring-at? l %result-acknowledge 0)))
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
		     mpg123 onstate onerror onvolume onevent
		     playlistid)
      ("@E"
       (let ((reason (read-line (the-port)))
	     (terr #f))
	  (tprint (the-string))
	  (with-access::mpg123 mpg123 (%status)
	     (with-access::musicstatus %status (err) (set! err reason))
	     (onerror mpg123 reason))
	  (ignore)))
      ((: "@F")
       ;; Status message during playing
       (let* ((a (read/rp number-grammar (the-port)))
	      (b (read/rp number-grammar (the-port)))
	      (c (read/rp number-grammar (the-port)))
	      (d (read/rp number-grammar (the-port))))
	  (with-access::mpg123 mpg123 (%status)
	     (with-access::musicstatus %status (state song songpos songlength)
		(let ((tstate (not (eq? state 'play))))
		   (set! songpos c)
		   (set! songlength (+fx c d))
		   (set! state 'play)
		   (when tstate
		      (onstate mpg123 %status)
		      (let* ((plist (music-playlist-get mpg123))
			     (file (list-ref plist song)))
			 (if (file-exists? file)
			     (let ((tag (mp3-id3 file)))
				(onevent mpg123 'meta (or tag file)))
			     (onevent mpg123 'meta file))))))))
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
	  (with-access::mpg123 mpg123 (%status)
	     (with-access::musicstatus %status (state)
		(set! state 'play))
	     (onstate mpg123 %status)
	     (with-access::musicstatus %status ((mplaylistid playlistid))
		(unless (eq? playlistid mplaylistid)
		   (set! playlistid mplaylistid)
		   (onevent mpg123 'playlist playlistid)))))
       (ignore))
      ((: "@P " digit)
       (tprint (the-string))
       ;; Playing status
       (with-access::mpg123 mpg123 (%status)
	  (case (integer->char (the-byte-ref 3))
	     ((#\0)
	      (with-access::musicstatus %status (state repeat (mrandom random)
						   playlistlength)
		 
		 
		 (set! state 'ended))
	      (onstate mpg123 %status))
	     ((#\1)
	      (with-access::musicstatus %status (state)
		 (set! state 'pause))
	      (onstate mpg123 %status)
	      (ignore))
	     ((#\2)
	      (with-access::musicstatus %status (state)
		 (set! state 'play))
	      (onstate mpg123 %status)
	      (ignore)))))
      ((: "@I" (+ all))
       (ignore))
      ((: "@V " (+ (or digit #\.)) #\%)
       (with-access::mpg123 mpg123 (%status)
	  (let ((v (inexact->exact
		      (round
			 (string->number (the-substring 3 -1))))))
	     (let ((t #f))
		(with-access::musicstatus %status (volume)
		   (set! t (=fx volume v))
		   (set! volume v))
		(unless t
		   (onvolume mpg123 v)))))
       (ignore))
      ((: "@" (or (in "JV") "RVA" "bass") (+ all))
       (ignore))
      ((: #\@ (+ (in alpha)))
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
;*    music-can-play-type? ::mpg123 ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-can-play-type? m::mpg123 mimetype)
   (string=? mimetype "audio/mpeg"))

;*---------------------------------------------------------------------*/
;*    musicproc-parse ::mpg123 ...                                     */
;*---------------------------------------------------------------------*/
(define-method (musicproc-parse o::mpg123)
   (with-access::mpg123 o (%process %status onstate onerror onvolume onevent)
      (when (process? %process)
	 (let ((p (process-output-port %process)))
	    (read/rp mpg123-grammar p o onstate onerror onvolume onevent
	       (with-access::musicstatus %status (playlistid)
		  playlistid))))))
