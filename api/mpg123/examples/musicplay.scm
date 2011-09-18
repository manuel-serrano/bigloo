;*=====================================================================*/
;*    .../prgm/project/bigloo/api/mpg123/examples/musicplay.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 26 07:30:16 2011                          */
;*    Last change :  Sun Sep 18 09:12:08 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A multimedia MUSIC player built on top of MPG123 and ALSA.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia pthread alsa mpg123)
   (main main))

;*---------------------------------------------------------------------*/
;*    directory->files ...                                             */
;*---------------------------------------------------------------------*/
(define (directory->files path)
   (cond
      ((directory? path)
       (append-map (lambda (p)
		      (directory->files (make-file-name path p)))
	  (sort (lambda (s1 s2)
		   (>fx (string-natural-compare3 s1 s2) 0))
	     (reverse (directory->list path)))))
      ((string-suffix? ".m3u" path)
       (filter-map (lambda (p)
		      (let ((path (car p)))
			 (with-handler
			    (lambda (e) #f)
			    (call-with-input-file path (lambda (ip) path)))))
	  (reverse (call-with-input-file path read-m3u))))
      (else
       (list path))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   
   (let ((files '())
	 (volume 80)
	 (device "plughw:0,0"))
      
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

      (when (pair? files)
	 (let* ((pcm (instantiate::alsa-snd-pcm
			(device device)))
		(decoder (instantiate::mpg123-alsadecoder
			    (mimetypes '("audio/mpeg"))))
		(player (instantiate::alsamusic
			   (decoders (list decoder))
			   (pcm pcm))))
	    (music-volume-set! player volume)
	    (music-playlist-clear! player)
	    (print "playing: ")
	    (for-each (lambda (p)
			 (print "  " p)
			 (music-playlist-add! player p))
	       (reverse files))
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
			    (print "volume: " volume)))))))

