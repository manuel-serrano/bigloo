;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/examples/musicplay.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 31 11:44:28 2008                          */
;*    Last change :  Tue Jan 24 17:55:30 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A simple music player                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let ((files '())
	 (backend 'mpc)
	 (command #f)
	 (mpcport 6600)
	 (mpchost "localhost"))
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
	 (("--command" ?cmd (help "Set the command path"))
	  (set! command cmd))
	 (("--help" (help "This help"))
	  (print "usage: music [options] file ...")
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! files (cons else files))))
      (let ((player (case backend
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
			   (port mpcport))))))
	 (music-playlist-clear! player)

	 (let loop ((files (reverse files)))
	    (when (pair? files)
	       (if (directory? (car files))
		   (loop (append (sort (directory->path-list (car files))
				    (lambda (p1 p2)
				       (< (string-natural-compare3 p1 p2) 0)))
			    (cdr files)))
		   (begin
		      (music-playlist-add! player (car files))
		      (loop (cdr files))))))
	 
	 (with-access::music player (onevent onstate onerror onvolume)
	    (set! onevent (lambda (m evt val)
			     (print evt ": " val)))
	    (set! onstate (lambda (m status)
			     (with-access::musicstatus status (state song playlistlength)
				(cond
				   ((eq? state 'play)
				    (print "Playing: "
				       (list-ref (music-playlist-get m) song)))
				   (else
				    (print state))))))
	    (set! onerror (lambda (m err)
			     (print "*** ERROR: " err)))
	    (set! onvolume (lambda (m vol)
			      (print "volume: " vol))))
	 (print "Using player: " backend)
	 (music-play player))))

