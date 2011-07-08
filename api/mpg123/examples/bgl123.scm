;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mpg123/examples/bgl123.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:20:46 2011                          */
;*    Last change :  Tue Jul  5 15:40:50 2011 (serrano)                */
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
   (let ((m (instantiate::mpg123-handle))
	 (pcm (instantiate::alsa-snd-pcm)))
      (alsa-snd-pcm-sw-set-params! pcm :start-threshold 1 :avail-min 1)
      (let ((inbuf (make-string inbufsiz))
	    (outbuf (make-string outbufsiz)))
	 (for-each (lambda (p) (play-mp3 p m pcm inbuf outbuf)) (cdr argv)))
      (mpg123-handle-close m)))

;*---------------------------------------------------------------------*/
;*    play-mp3 ...                                                     */
;*---------------------------------------------------------------------*/
(define (play-mp3 path m pcm inbuf outbuf)
   (print "playing " path "...")
   (let ((p (open-input-file path)))
      (let loop ()
	 (let ((sz (read-chars! inbuf (string-length inbuf) p)))
	    (multiple-value-bind (status size)
	       (mpg123-decode m inbuf 0 sz outbuf (string-length outbuf))
	       (when (eq? status 'new-format)
		  (multiple-value-bind (rate channels encoding)
		     (mpg123-get-format m)
		     (tprint "rate=" rate " channels=" channels " encoding=" encoding)
		     (alsa-snd-pcm-set-params! pcm
			:format encoding
			:access 'rw-interleaved
			:channels channels
			:rate rate
			:soft-resample 1
			:latency 500000)
		     (alsa-snd-pcm-hw-set-params! pcm :channels channels
			:format encoding
			:rate-near rate
			:buffer-size-near (/fx rate 2)
			:period-size-near (/fx rate 8))))
	       (when (>fx size 0)
		  (alsa-snd-pcm-write pcm outbuf size))
	       (unless (or (eq? status 'done) (and (=fx sz 0) (=fx size 0)))
		  (loop)))))))
