;*=====================================================================*/
;*    .../prgm/project/bigloo/api/mpg123/examples/musicplay.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 26 07:30:16 2011                          */
;*    Last change :  Fri Jul  1 07:40:52 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A multimedia MUSIC player built on top of MPG123 and ALSA.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia pthread alsa mpg123)
   (static (class mpg123decoder::alsadecoder-host
	      (handle::mpg123-handle read-only (default (instantiate::mpg123-handle)))))
   (main main))

;*---------------------------------------------------------------------*/
;*    alsadecoder-close ::mpg123decoder ...                            */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-close o::mpg123decoder)
   (with-access::mpg123decoder o (handle)
      (mpg123-handle-close handle)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-can-play-type? ::mpg123decoder ...                   */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-can-play-type? o::mpg123decoder mime)
   (string=? mime "audio/mpeg"))

;*---------------------------------------------------------------------*/
;*    alsadecoder-decode-buffer ::mpg123-decoder ...                   */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode-buffer o::mpg123decoder inbuf inoff insz outbuf)
   (with-access::mpg123decoder o (handle)
      (multiple-value-bind (status size)
	 (mpg123-decode handle inbuf inoff insz outbuf (string-length outbuf))
	 (if (eq? status 'new-format)
	     (multiple-value-bind (rate channels encoding)
		(mpg123-get-format handle)
		(values status size rate channels encoding))
	     (values status size #f #f #f)))))

;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::mpg123decoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-position o::mpg123decoder buf)
   (with-access::mpg123decoder o (handle)
      (mpg123-position handle buf)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-info ::mpg123decoder ...                             */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-info o::mpg123decoder)
   (with-access::mpg123decoder o (handle)
      (mpg123-info handle)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-volume-set! ::mpg123decoder ...                      */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-volume-set! o::mpg123decoder vol)
   (with-access::mpg123decoder o (handle)
      (mpg123-volume-set! handle vol)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-seek ::mpg123decoder ...                             */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-seek o::mpg123decoder ms)
   (with-access::mpg123decoder o (handle)
      (mpg123-seek handle ms)))

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

      (let* ((pcm (instantiate::alsa-snd-pcm
		     (device device)))
	     (decoder (instantiate::mpg123decoder))
	     (player (instantiate::alsamusic
			(decoders (list decoder))
			(mkthread (lambda (b) (instantiate::pthread (body b))))
			(pcm pcm))))
	 (music-volume-set! player volume)
	 (alsa-snd-pcm-sw-set-params! pcm :start-threshold 1 :avail-min 1)
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
			   (print "song : " (list-ref (music-playlist-get player)  song)
			      " [" songpos "/" songlength "]")
			   (newline)
			   (when (and (eq? state 'ended)
				      (=fx song (-fx (length files) 1)))
			      (exit 0))))
	    :onmeta (lambda (meta)
		       (print "meta    : " meta)
		       (print "playlist: ")
		       (for-each (lambda (s) (print "  " s))
			  (music-playlist-get player))
		       (newline))
	    :onerror (lambda (err)
			(print "error   : " err)
			(newline))
	    :onvolume (lambda (volume)
			 (print "volume: " volume)
			 (newline))))))

