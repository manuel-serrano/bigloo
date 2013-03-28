;*=====================================================================*/
;*    .../prgm/project/bigloo/api/mpg123/examples/musicplay.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 26 07:30:16 2011                          */
;*    Last change :  Sat Feb 23 16:43:46 2013 (serrano)                */
;*    Copyright   :  2011-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A multimedia MUSIC player built on top of MPG123 and ALSA.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia pthread alsa mpg123)
   (main main))

(define debug 0)

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
	 (device (or (getenv "ALSADEVICE") "default")))
      
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

      (define (onstate o state)
	 (tprint "state: " state))

      (define (onerror o e)
	 (tprint "error: " e))

      (define (onvolume o v)
	 (tprint "volume: " v))

      (define (onevent o e v)
	 (tprint "event: " e " " v))
      
      (when (pair? files)
	 (let* ((pcm (instantiate::alsa-snd-pcm
			(device device)))
		(decoder (instantiate::mpg123-alsadecoder
			    (outbuf (make-string (* 16 1024)))
			    (mimetypes '("audio/mpeg"))))
		(player (instantiate::alsamusic
			   (onstate onstate)
			   (onerror onerror)
			   (onvolume onvolume)
			   (onevent onevent)
			   (inbuf (make-string (* 16 1024)))
			   (decoders (list decoder))
			   (pcm pcm))))
	    (music-volume-set! player volume)
	    (music-playlist-clear! player)
	    (print "playing: ")
	    (for-each (lambda (p)
			 (print "  " p)
			 (music-playlist-add! player p))
	       (reverse files))
	    (music-play player)))))

