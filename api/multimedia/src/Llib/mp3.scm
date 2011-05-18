;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/mp3.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 25 08:37:41 2007                          */
;*    Last change :  Wed May 18 16:43:41 2011 (serrano)                */
;*    Copyright   :  2007-11 Manuel Serrano                            */
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
	      (offset::elong (default #e0))
	      (version::real (default 0.))
	      (layer::int (default 0))
	      (crc::int (default 0))
	      (bitrate::int (default 0))
	      (samplerate::int (default 0))
	      (padding::bool (default #t))
	      (channels::int (default 2))
	      (length::elong (default #e0))
	      (duration::float (default 0.)))

	   (read-mp3-frame ::mmap ::elong ::mp3frame)))

;*---------------------------------------------------------------------*/
;*    frame-duration ...                                               */
;*---------------------------------------------------------------------*/
(define (frame-duration len bitrate)
   (/fl (fixnum->flonum (elong->fixnum len))
      (*fl (fixnum->flonum bitrate) 125.)))

;*---------------------------------------------------------------------*/
;*    mp3frame-same-constant? ...                                      */
;*---------------------------------------------------------------------*/
(define (mp3frame-same-constant? f1 f2)
   (and #f
	(=fl (mp3frame-version f1) (mp3frame-version f2))
	(=fx (mp3frame-layer f1) (mp3frame-layer f2))
	(=fx (mp3frame-crc f1) (mp3frame-crc f2))
	(=fx (mp3frame-samplerate f1) (mp3frame-samplerate f2))))

;*---------------------------------------------------------------------*/
;*    mmap-ref-byte ...                                                */
;*---------------------------------------------------------------------*/
(define (mmap-ref-byte mm i)
   (char->integer (mmap-ref mm i)))

;*---------------------------------------------------------------------*/
;*    byte-bits ...                                                    */
;*---------------------------------------------------------------------*/
(define (byte-bits byte lo mo)
   (bit-and (bit-rsh byte lo) (-fx (bit-lsh 1 (-fx (+fx 1 mo) lo)) 1)))

;*---------------------------------------------------------------------*/
;*    read-mp3-frame ...                                               */
;*---------------------------------------------------------------------*/
(define (read-mp3-frame mm offset::elong frame)
   
   (define bitrates-v1l1
      '#(0 32 64 96 128 160 192 224 256 288 320 352 384 416 448 -1))
   (define bitrates-v1l2
      '#(0 32 38 56 64 80 96 112 128 160 192 224 256 320 384 -1))
   (define bitrates-v1l3
      '#(0 32 40 48 56 64 80 96 112 128 160 192 224 256 320 -1))
   (define bitrates-v2l1
      '#(0 32 48 56 64 80 96 112 128 144 160 176 176 192 224 256 -1))
   (define bitrates-v2l23
      '#(0 8 16 24 32 40 48 56 64 80 96 112 128 144 160 -1))
   (define bratesv1
      (vector #f bitrates-v1l3 bitrates-v1l2 bitrates-v1l1))
   (define bratesv2
      (vector #f bitrates-v2l23 bitrates-v2l23 bitrates-v2l1))
   (define brates
      (vector bratesv2 #f bratesv2 bratesv1))
   
   (define (bitrate rate mversion layer)
      (vector-ref (vector-ref (vector-ref brates mversion) layer) rate))
   
   (define (frame-length layer ver brate srate padding)
      (fixnum->elong
	 (+ (/fx (* (vector-ref '#(72000 72000 24000 0) layer)
		    (+fx 1 (bit-and ver 1))
		    brate)
	       srate)
	    padding)))
   
   (let ((len (mmap-length mm)))
      (let loop ((i offset))
	 (cond
	    ((>=elong i (-elong len #e4))
	     #f)
	    ((>=elong (-elong i offset) #e8192)
	     #f)
	    ((char=? (mmap-ref mm i) #a255)
	     (let ((b1 (mmap-ref-byte mm (+ i #e1))))
		(if (=fx (bit-and b1 #xe0) #xe0)
		    (let* ((b2 (mmap-ref-byte mm (+ i #e2)))
			   (b3 (mmap-ref-byte mm (+ i #e3)))
			   (bitver (byte-bits b1 3 4))
			   (bitlayer (byte-bits b1 1 2))
			   (crc (byte-bits b1 0 0))
			   (bitbrate (byte-bits b2 4 7))
			   (bitsrate (byte-bits b2 2 3))
			   (padding (byte-bits b2 1 1))
			   (bitchannels (byte-bits b3 6 7)))
		       (if (or (=fx bitlayer 00)
			       (=fx bitsrate 3)
			       (=fx bitver 1))
			   (loop (+elong i 1))
			   (let* ((layer (vector-ref '#(0 3 2 1) bitlayer) )
				  (version (vector-ref '#(2.5 0. 2. 1.) bitver))
				  (brate (bitrate bitbrate bitver bitlayer))
				  (srate (vector-ref
					    (vector-ref '#(#(11025 12000 8000 0)
							   #f
							   #(22050 24000 16000 0)
							   #(44100 48000 32000 0))
					       bitver)
					    bitsrate))
				  (len (frame-length bitlayer bitver brate srate padding)))
			      (if (<elong len #e21)
				  (loop (+elong i 1))
				  (begin
				     (mp3frame-offset-set! frame i)
				     (mp3frame-version-set! frame
					(vector-ref '#(2.5 0. 2. 1.) bitver))
				     (mp3frame-layer-set! frame layer)
				     (mp3frame-crc-set! frame crc)
				     (mp3frame-bitrate-set! frame brate)
				     (mp3frame-samplerate-set! frame srate)
				     (mp3frame-padding-set! frame padding)
				     (mp3frame-channels-set! frame
					(vector-ref '#(2 2 1 1) bitchannels))
				     (mp3frame-length-set! frame len)
				     (mp3frame-duration-set! frame
					(frame-duration len brate))
				     frame)))))
		    (loop (+elong i 1)))))
	    (else
	     (loop (+elong i 1)))))))
