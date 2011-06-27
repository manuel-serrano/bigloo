;*=====================================================================*/
;*    .../prgm/project/bigloo/api/mpg123/examples/musicplay.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 26 07:30:16 2011                          */
;*    Last change :  Sun Jun 26 15:59:25 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A multimedia MUSIC player built on top of MPG123 and ALSA.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module musicplay
   (library multimedia pthread alsa mpg123)
   (static (class mpg123decoder::alsadecoder
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
;*    alsadecoder-decode ::mpg123-decoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode o::mpg123decoder inbuf insz outbuf)
   (with-access::mpg123decoder o (handle)
      (multiple-value-bind (status size)
	 (mpg123-decode handle inbuf insz outbuf (string-length outbuf))
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
   (let* ((files (append-map directory->files (cdr args)))
	  (pcm (instantiate::alsa-snd-pcm))
	  (decoder (instantiate::mpg123decoder))
	  (player (instantiate::alsamusic
		     (decoders (list decoder))
		     (pcm pcm))))
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
			(print "song : " song
			   " [" songpos "/" songlength "]")
			(newline)))
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
		      (newline)))))

