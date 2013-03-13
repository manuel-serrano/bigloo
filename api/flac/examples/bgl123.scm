;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/flac/examples/bgl123.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 24 16:20:46 2011                          */
;*    Last change :  Tue Mar 12 15:17:09 2013 (serrano)                */
;*    Copyright   :  2011-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A simple music player. Requires  both FLAC *and* ALSA libs.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bgl123
   (library alsa flac pthread)
   (extern (macro $memcpy::void (::string ::string ::long) "memcpy"))
   (static (class flac-alsa::flac-decoder
	      (port (default #f))
	      (pcm::alsa-snd-pcm read-only)
	      (%inbuf::bstring read-only (default (make-string (*fx 16 1024))))))
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
   (let* ((dev (or (getenv "ALSADEVICE") "default"))
	  (pcm (instantiate::alsa-snd-pcm
		  (device dev)))
	  (f (instantiate::flac-alsa
		(pcm pcm))))
      (alsa-snd-pcm-open pcm)
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
	 (with-access::flac-alsa f (port)
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
	 (alsa-snd-pcm-hw-set-params! pcm
	    :access 'rw-interleaved
	    :format encoding
	    :channels channels
	    :rate-near rate
	    :buffer-size-near-ratio 2
	    :period-size-near-ratio 8)
	 (alsa-snd-pcm-sw-set-params! pcm
	    :start-threshold 1
	    :avail-min 1))))

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

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-read o::flac-alsa size::long)
   (with-access::flac-alsa o (port %flacbuf %inbuf)
      (let ((flacbuf (custom-identifier %flacbuf))
	    (sz (minfx size (string-length %inbuf))))
	 (let ((sz2 (read-chars! %inbuf sz port)))
	    ($memcpy flacbuf %inbuf sz2)
	    (when (>=fx (flac-debug) 1)
	       (with-access::flac-alsa o (%bchecksum)
		  (set! %bchecksum
		     (flac-checksum-debug
			%bchecksum %inbuf 0 sz2))))
	    sz2))))
