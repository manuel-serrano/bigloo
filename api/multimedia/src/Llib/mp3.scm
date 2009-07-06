;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/mp3.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 25 08:37:41 2007                          */
;*    Last change :  Sun Dec 30 16:42:42 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    MP3 info extractor                                               */
;*    -------------------------------------------------------------    */
;*    Information about MP3 frame header can be found at:              */
;*       http://en.wikipedia.org/wiki/MP3                              */
;*       http://mpgedit.org/mpgedit/mpeg_format/MP3Format.html         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-mp3

   (export (class mp3frame
	      (version::real (default 0.))
	      (layer::int (default 0))
	      (bitrate::int (default 0))
	      (sampling::int (default 0))
	      (samples::int (default 0))
	      (padding::bool (default #t))
	      (channel::bstring (default ""))
	      (crc16::int (default 0))
	      (length::int (default 0))
	      (time::int (default 0))
	      (offset (default 0)))

	   (mp3-info ::input-port #!key (minframes 25) (maxoffset 5000))
	   (mp3-all-frames ::input-port)
	   (mp3-frame ::input-port)))

;*---------------------------------------------------------------------*/
;*    mp3-info ...                                                     */
;*---------------------------------------------------------------------*/
(define (mp3-info ip #!key (minframes 25) (maxoffset 5000))
   (let ((info (mp3-frame ip)))
      (when (and (mp3frame? info) (<fx (mp3frame-offset info) maxoffset))
	 (with-access::mp3frame info (time length)
	    (let ((aux (instantiate::mp3frame)))
	       (let loop ((n 0))
		  (let ((frame (mp3-read-frame! ip aux)))
		     (cond
			((mp3frame? frame)
			 (set! time (+fx time (mp3frame-time aux)))
			 (set! length (+fx length (mp3frame-length aux)))
			 (loop (+fx n 1)))
			((<fx n minframes)
			 #f)
			(else
			 info)))))))))
   
;*---------------------------------------------------------------------*/
;*    mp3-all-frames ...                                               */
;*---------------------------------------------------------------------*/
(define (mp3-all-frames ip)
   (let ((frame (mp3-frame ip)))
      (when (mp3frame? frame)
	 (let loop ((frames (list frame)))
	    (let ((frame (mp3-read-frame! ip (instantiate::mp3frame))))
	       (cond
		  ((mp3frame? frame)
		   (loop (cons frame frames)))
		  ((eof-object? frame)
		   (reverse! frames))
		  (else
		   #f)))))))
   
;*---------------------------------------------------------------------*/
;*    mp3-frame ...                                                    */
;*---------------------------------------------------------------------*/
(define (mp3-frame ip)
   (mp3-read-frame! ip (instantiate::mp3frame)))

;*---------------------------------------------------------------------*/
;*    mp3-read-frame! ...                                              */
;*---------------------------------------------------------------------*/
(define (mp3-read-frame! ip frame)
   
   (define (bit->mpeg-version bb)
      (case bb
	 ((0) 2.5)
	 ((2) 2.0)
	 ((3) 1.0)
	 (else -1.0)))
   
   (define (bit->layer cc)
      (case cc
	 ((1) 3)
	 ((2) 2)
	 ((3) 1)
	 (else -1)))
   
   (define (bit->bitrate eeee version layer)
      
      (define v1l1-table
	 '#(0 32 64 96 128 160 192 224 256 288 320 352 384 416 448 -1))
      (define v1l2-table
	 '#(0 32 48 56 64 80 96 112 128 160 192 224 256 320 384 -1))
      (define v1l3-table
	 '#(0 32 40 48 56 64 80 96 112 128 160 192 224 256 320 -1))
      (define v2l1-table
	 '#(0 32 48 56 64 80 96 112 128 144 160 176 192 224 256 -1))
      (define v2l2-table
	 '#(0 8 14 24 32 40 48 56 64 80 96 112 128 144 160 -1))
      (define v2l3-table
	 v2l2-table)
      
      (let ((table (if (>=fl version 2.0)
		       (if (>=fx layer 2) v2l2-table v2l1-table)
		       (case layer
			  ((1) v1l1-table)
			  ((2) v1l2-table)
			  (else v1l3-table)))))
	 (vector-ref table eeee)))
   
   (define (bit->sampling ff version)
      (cond
	 ((>fl version 2.0)
	  (case ff
	     ((0) 11025)
	     ((1) 12000)
	     ((2) 8000)
	     (else -1)))
	 ((>fl version 1.0)
	  (case ff
	     ((0) 22050)
	     ((1) 24000)
	     ((2) 16000)
	     (else -1)))
	 (else
	  (case ff
	     ((0) 44100)
	     ((1) 48000)
	     ((2) 32000)
	     (else -1)))))
   
   (define (bit->channel ii)
      (case ii
	 ((0) "stereo")
	 ((1) "joint stereo")
	 ((2) "dual channel")
	 ((3) "mono")))
   
   (define (sample layer version)
      (cond
	 ((> version 1.0)
	  (case layer
	     ((0) 0)
	     ((1) 384)
	     ((2) 1152)
	     (else 576)))
	 (else
	  (case layer
	     ((0) 0)
	     ((1) 384)
	     ((2) 1152)
	     (else 1152)))))
   
   (define (read-crc ip)
      (let* ((n3 (read-byte ip))
	     (n4 (read-byte ip)))
	 (if (or (eof-object? n3) (eof-object? n4))
	     -1
	     (+fx (bit-lsh n3 8) n4))))
   
   (define (frame-length layer padding samples bitrate sampling)
      (if (=fx layer 1)
	  (*fx (*fx 4 (+fx padding 4))
	       (/fx (*fx 12 bitrate) sampling))
	  (+fx padding (/fx (*fx samples (* 1000 bitrate)) (*fx 8 sampling)))))
   
   (define (frame-time samples sampling)
      (/fx (*fx samples 1000) sampling))
   
   (let loop ((byte (read-byte ip)))
      (cond
	 ((eof-object? byte)
	  byte)
	 ((=fx byte #xff)
	  (let ((n0 (read-byte ip)))
	     (cond
		((eof-object? n0)
		 n0)
		((=fx (bit-and n0 #b11100000) #b11100000)
		 (let* ((n1 (read-byte ip))
			(n2 (read-byte ip)))
		    (cond
		       ((eof-object? n1)
			n1)
		       ((eof-object? n2)
			n2)
		       (else
			;; gotcha, a MP3 file..
			;; a mp3 byte header is
			;; n0: aaabbccd
			;; n1: eeeeffgh
			;; n2: iijjklmm
			(let* ((bb (bit-and (bit-rsh n0 3) #b11))
			       (cc (bit-and (bit-rsh n0 1) #b11))
			       (d (bit-and n0 #b1))
			       (eeee (bit-rsh n1 4))
			       (ff (bit-and (bit-rsh n1 2) #b11))
			       (g (bit-and (bit-rsh n1 1) #b1))
			       (h (bit-and n1 #b1))
			       (ii (bit-and (bit-rsh n2 6) #b11))
			       (jj (bit-and (bit-rsh n2 4) #b11))
			       (k (bit-and (bit-rsh n2 4) #b1))
			       (l (bit-and (bit-rsh n2 3) #b1))
			       (mm (bit-and n2 #b11))
			       (version (bit->mpeg-version bb))
			       (layer (bit->layer cc))
			       (bitr (bit->bitrate eeee version layer)))
			   (if (or (<fl version 0.0)
				   (=fx layer -1)
				   (=fx bitr -1))
			       (loop n2)
			       (let* ((sampling (bit->sampling ff version))
				      (samples (sample layer version))
				      (offset (- (input-port-position ip) 4))
				      (len (frame-length layer g samples bitr sampling))
				      (crc (if (=fx d 0) (read-crc ip) 0))
				      (channel (bit->channel ii))
				      (time (frame-time samples sampling))
				      (plen (-fx len (if (=fx d 0) 6 4))))
				  (if (and (>fx plen 0)
					   (not (=fx crc -1))
					   (>fx len 0))
				      (begin
					 (mp3frame-version-set! frame version)
					 (mp3frame-layer-set! frame layer)
					 (mp3frame-bitrate-set! frame bitr)
					 (mp3frame-sampling-set! frame sampling)
					 (mp3frame-samples-set! frame samples)
					 (mp3frame-padding-set! frame (=fx g 1))
					 (mp3frame-channel-set! frame channel)
					 (mp3frame-crc16-set! frame crc)
					 (mp3frame-length-set! frame len)
					 (mp3frame-time-set! frame time)
					 (mp3frame-offset-set! frame offset)
					 (let ((payload (read-chars plen ip)))
					    (when (=fx d 0)
					       ;; to be completed
					       (crc16-string payload)))
					 frame)
				      (loop n2)))))))))
		(else
		 (loop (read-byte ip))))))
	 (else
	  (loop (read-byte ip))))))
