;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/examples/bgst-mm.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 31 11:44:28 2008                          */
;*    Last change :  Fri Mar 12 10:37:36 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A simple music player                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library pthread multimedia gstreamer)
   (main main)
   (eval (export m)))

;*---------------------------------------------------------------------*/
;*    m ...                                                            */
;*---------------------------------------------------------------------*/
(define m #f)

;*---------------------------------------------------------------------*/
;*    verbose ...                                                      */
;*---------------------------------------------------------------------*/
(define verbose 1)

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (version)
   (bigloo-config 'release-number))

;*---------------------------------------------------------------------*/
;*    verb ...                                                         */
;*---------------------------------------------------------------------*/
(define (verb level . args)
   (when (>=fx verbose level)
      (apply print args)))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (eval '(library-load 'multimedia))
   (eval '(library-load 'gstreamer))
   (let ((files '())
	 (backend 'gstreamer)
	 (command #f)
	 (volume #f)
	 (mpcport 6600)
	 (mpchost "localhost")
	 (frequency 2000000))
      (args-parse (cdr args)
	 (("--mpg123" (help "Select the mpg123 back-end"))
	  (set! backend 'mpg123))
	 (("--mpc" (help "Select the mpc back-end"))
	  (set! backend 'mpc))
	 (("--mpc-host" ?host (help "mpc host (default localhost)"))
	  (set! mpchost host))
	 (("--mpc-port" ?port (help "mpc port (default 6600)"))
	  (set! mpcport port))
	 (("--mplayer" (help "Select the mplayer back-end"))
	  (set! backend 'mplayer))
	 (("--gstreamer" (help "Select the gstreamer back-end"))
	  (set! backend 'gstreamer))
	 (("--gstreamer-bt" (help "Select the gstreamer+bluetooth back-end"))
	  (set! backend 'gstreamer-bt))
	 (("--command" ?cmd (help "Set the command path"))
	  (set! command cmd))
	 ((("-V" "--verbose") (help "Increase verbosity"))
	  (set! verbose (+ 1 verbose)))
	 ((("-q" "--quiet") (help "Silent mode"))
	  (set! verbose 0))
	 ((("-v" "--volume") ?vol (help "Set volume"))
	  (set! volume (string->integer vol)))
	 (("--frequency" ?freq (help "Set the event loop frequency (default 2000000)"))
	  (set! frequency (string->integer freq)))
	 (("--help" (help "This help"))
	  (print "usage: music [options] file ...")
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! files (cons else files))))
      
      (set! m
	    (case backend
	       ((gstreamer)
		(instantiate::gstmusic))
	       ((gstreamer-bt)
		(instantiate::gstmusic
		   (%audiosink (gst-element-factory-make "alsasink" :device "pcm.bluetooth"))))
	       ((mpg123)
		(if command
		    (instantiate::mpg123
		       (path command))
		    (instantiate::mpg123)))
	       ((mplayer)
		(if command
		    (instantiate::mplayer
		       (path command))
		    (instantiate::mplayer)))
	       ((mpc)
		(instantiate::mpc
		   (host mpchost)
		   (port mpcport)))))

      (when volume (music-volume-set! m volume))
	    
      (let loop ((files (reverse files)))
	 (when (pair? files)
	    (if (directory? (car files))
		(loop (append (sort (directory->path-list (car files))
				    (lambda (p1 p2)
				       (< (string-natural-compare3 p1 p2) 0)))
			      (cdr files)))
		(begin
		   (music-playlist-add! m (car files))
		   (loop (cdr files))))))
      
      (thread-start!
       (make-thread
	(lambda ()
	   (music-event-loop
	    m
	    :frequency frequency
	    :onmeta (lambda (meta)
		       (if (list? meta)
			   (for-each (lambda (meta)
					(verb 2
					      "  " (car meta) ": " (cdr meta)))
				     meta)
			   (verb 2 meta)))
	    :onstate (lambda (status)
			(with-access::musicstatus status (state song playlistlength)
			   (when (eq? state 'play)
			      (verb 1 "Playing: "
				    (list-ref (music-playlist-get m) song)))))
	    :onerror (lambda (err)
			(print "*** ERROR: " err)
			(print (list-ref (music-playlist-get m)
					 (musicstatus-song (music-status m)))))))))
	    
      (verb 3 "bgst-mm: " (version) " " backend)
      (verb 3 "gst-version: " (gst-version) "\n")
      (verb 1 "The music object is bound to the variable `m'.")
      
      (music-play m)
      (repl)))
