;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/examples/bgl123.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:20:46 2011                          */
;*    Last change :  Tue Jul 12 09:03:03 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A simple music player. Requires  both FLAC *and* ALSA libs.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bgl123
   (library alsa flac pthread)
   (static (class flac-alsa::flac-decoder
	      (pcm::alsa-snd-pcm read-only)))
   (main main))

;*---------------------------------------------------------------------*/
;*    global constants                                                 */
;*---------------------------------------------------------------------*/
(define inbufsiz 16384)
(define outbufsiz 16384)
(define decode-pos 0)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((pcm (instantiate::alsa-snd-pcm))
	  (f (instantiate::flac-alsa
		(pcm pcm))))
      (alsa-snd-pcm-open pcm)
      (alsa-snd-pcm-sw-set-params! pcm :start-threshold 1 :avail-min 1)
      (let ((inbuf (make-string inbufsiz))
	    (outbuf (make-string outbufsiz)))
	 (for-each (lambda (p) (play-flac p f pcm inbuf outbuf)) (cdr argv)))
      (flac-decoder-close f)))

;*---------------------------------------------------------------------*/
;*    play-flac ...                                                    */
;*---------------------------------------------------------------------*/
(define (play-flac path f pcm inbuf outbuf)
   (let ((p (open-input-file path)))
      (when (input-port? p)
	 (print "playing " path "...")
	 (with-access::flac-decoder f (port)
	    (set! port p))
	 (set! decode-pos 0)
	 (flac-decoder-decode f))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flac-alsa ...                            */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-metadata o::flac-alsa total rate channels bps)
   (with-access::flac-alsa o (pcm)
      (let ((encoding (case bps
			 ((8) 's16)
			 ((16) 's16)
			 ((24) 's24-3le)
			 ((32) 's32))))
	 (tprint "meta total=" total " rate=" rate " channels=" channels " bps=" bps " encoding=" encoding)
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
	    :period-size-near (/fx rate 8)))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-write ::flac-alsa ...                               */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-write o::flac-alsa size rate channels bps)
   (with-access::flac-alsa o (outbuf pcm)
      (when (>fx size 0)
	 (when (> (flac-decoder-position o) decode-pos)
	    (set! decode-pos (flac-decoder-position o))
	    (display ".")
	    (flush-output-port (current-output-port)))
	 (alsa-snd-pcm-write pcm outbuf size)
	 #t)))
