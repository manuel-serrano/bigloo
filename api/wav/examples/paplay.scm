;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/wav/examples/paplay.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 26 07:30:16 2011                          */
;*    Last change :  Thu Jan 28 03:43:36 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A multimedia MUSIC player built on top of MPG123 and ALSA.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia pthread alsa pulseaudio wav)
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
	 (volume 80))
      
      (args-parse (cdr args)
	 ((("-h" "--help") (help "This message"))
	  (print "musicplay")
	  (print "usage: musicplay [options] file1 file2 ...")
	  (newline)
	  (args-parse-usage #f)
	  (exit 0))
	 ((("-v" "--volume") ?vol (help "Set volume"))
	  (set! volume (string->integer vol)))
	 (else
	  (set! files (append (directory->files else) files))))

      (define (onstate o state)
	 (tprint "state: " state))

      (define (onerror o e)
	 (tprint "error: " e))

      (define (onvolume o v)
	 (tprint "volume: " v))

      (define (onevent o e v)
	 (tprint "event: " e " " v))
      
      (when (pair? files)
	 (let* ((decoder (instantiate::wav-pulseaudiodecoder))
		(player (instantiate::pulseaudiomusic
			   (onstate onstate)
			   (onerror onerror)
			   (onvolume onvolume)
			   (onevent onevent)
			   (inbuf (make-string (* 16 1024)))
			   (decoders (list decoder))
			   (name "bigloo"))))
	    (music-volume-set! player volume)
	    (music-playlist-clear! player)
	    (print "playing: ")
	    (for-each (lambda (p)
			 (print "  " p " " (file-musicinfo p))
			 (music-playlist-add! player p))
	       (reverse files))
	    (music-play player)))))

