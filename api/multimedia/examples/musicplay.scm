;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/examples/musicplay.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 31 11:44:28 2008                          */
;*    Last change :  Fri Mar 12 10:36:22 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
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
	 (("--command" ?cmd (help "Set the command path"))
	  (set! command cmd))
	 (("--frequency" ?freq (help "Set the event loop frequency (default 2000000)"))
	  (set! frequency (string->integer freq)))
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
;* 		       ((xmms)                                         */
;* 			(instantiate::xmms)))))                        */
	 (music-playlist-clear! player)
	 (for-each (lambda (p) (music-playlist-add! player p)) (reverse files))
	 (music-play player)
	 (print "Using player: " backend)
	 (music-event-loop player
	    :frequency frequency
            :onstate (lambda (status)
			(with-access::musicstatus status (state song volume
								songpos
								songlength)
			   (print "state: " state)
			   (print "song : " song
				  " [" songpos "/" songlength "]")
			   (newline)))
	    :onmeta (lambda (meta playlist)
		       (print "meta    : " meta)
		       (print "playlist: ")
		       (for-each (lambda (s) (print "  " s)) playlist)
		       (newline))
	    :onvolume (lambda (volume)
			 (print "volume: " volume)
			 (newline))))))

