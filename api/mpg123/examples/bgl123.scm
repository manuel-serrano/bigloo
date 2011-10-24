;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/examples/bgl123.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:20:46 2011                          */
;*    Last change :  Mon Oct 24 07:25:33 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A simple music player. Requires  both MPG123 *and* ALSA libs.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bgl123
   (library alsa mpg123 pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    global constants                                                 */
;*---------------------------------------------------------------------*/
(define inbufsiz 16384)
(define outbufsiz 16384)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (print "cards=" (alsa-snd-cards-list))
   (let* ((m (instantiate::mpg123-handle))
	  (dev (or (getenv "ALSADEVICE") "default"))
	  (pcm (instantiate::alsa-snd-pcm
		  (device dev))))
      (alsa-snd-pcm-open pcm)
      (let ((inbuf (make-string inbufsiz))
	    (outbuf (make-string outbufsiz)))
	 (for-each (lambda (p) (play-mp3 p m pcm inbuf outbuf)) (cdr argv)))
      (mpg123-handle-close m)))

;*---------------------------------------------------------------------*/
;*    new-format ...                                                   */
;*---------------------------------------------------------------------*/
(define (new-format dec pcm)
   (multiple-value-bind (rate channels encoding)
      (mpg123-get-format dec)
      (alsa-snd-pcm-hw-set-params! pcm
	 :access 'rw-interleaved
	 :format encoding
	 :channels channels
	 :rate-near rate
	 :buffer-size-near-ratio 2
	 :period-size-near-ratio 8)
      (alsa-snd-pcm-sw-set-params! pcm
	 :start-threshold 1
	 :avail-min 1)))

;*---------------------------------------------------------------------*/
;*    play-mp3 ...                                                     */
;*---------------------------------------------------------------------*/
(define (play-mp3 path m pcm inbuf outbuf)
   (print "playing " path "... pcm=" pcm)
   (let ((p (open-input-file path)))
      (let loop ()
	 (let ((sz (read-chars! inbuf (string-length inbuf) p)))
	    (multiple-value-bind (status size)
	       (mpg123-decode m inbuf 0 sz outbuf (string-length outbuf))
	       (when (eq? status 'new-format)
		  (new-format m pcm))
	       (when (>fx size 0)
		  (alsa-snd-pcm-write pcm outbuf size))
	       (unless (or (eq? status 'done) (and (=fx sz 0) (=fx size 0)))
		  (loop)))))))
