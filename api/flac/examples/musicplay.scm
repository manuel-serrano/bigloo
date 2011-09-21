;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/examples/musicplay.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 26 07:30:16 2011                          */
;*    Last change :  Mon Sep 19 11:24:57 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A multimedia MUSIC player built on top of FLAC and ALSA.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia pthread alsa flac)
   (main main))

;*---------------------------------------------------------------------*/
;*    directory->files ...                                             */
;*---------------------------------------------------------------------*/
(define (directory->files path)
   (if (directory? path)
       (append-map (lambda (p)
		      (directory->files (make-file-name path p)))
	  (sort (lambda (s1 s2)
		   (>fx (string-natural-compare3 s1 s2) 0))
	     (directory->list path)))
       (list path)))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   
   (let ((files '())
	 (volume 100)
	 (device "default"))
      
      (args-parse (cdr args)
	 ((("-h" "--help") (help "This message"))
	  (print "musicplay")
	  (print "usage: musicplay [options] file1 file2 ...")
	  (newline)
	  (args-parse-usage #f)
	  (exit 0))
	 ((("-v" "--volume") ?vol (help "Set volume"))
	  (set! volume (string->integer vol)))
	 ((("-d" "--device") ?dev (help "Select device"))
	  (set! device dev))
	 (else
	  (set! files (append (directory->files else) files))))
      
      (co-instantiate
	    ((pcm (instantiate::alsa-snd-pcm
		     (device device)))
	     (decoder (instantiate::flac-alsadecoder
			 (mimetypes '("audio/flac" "application/x-flac"))))
	     (player (instantiate::alsamusic
			(decoders (list decoder))
			(pcm pcm))))
	 (music-volume-set! player volume)
	 (music-playlist-clear! player)
	 (for-each (lambda (p) (music-playlist-add! player p)) (reverse files))
	 (music-play player)
	 (music-event-loop player
	    :frequency 20000
	    :onstate (lambda (status)
			(with-access::musicstatus status (state song volume
							    songpos
							    songlength)
			   (print "state: " state)
			   (case state
			      ((play)
			       (print "song: " (list-ref (music-playlist-get player)  song)
				  " [" songpos "/" songlength "]"))
			      ((ended)
			       (newline)
			       (when (=fx song (-fx (length files) 1))
				  (exit 0))))))
	    :onmeta (lambda (meta)
		       (print "meta: " meta)
		       (print "playlist: " (length (music-playlist-get player))))
	    :onerror (lambda (err)
			(print "error: " err))
	    :onvolume (lambda (volume)
			 (print "volume: " volume))))))
   

