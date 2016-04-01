;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/mp3.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 25 08:37:41 2007                          */
;*    Last change :  Fri Apr  1 08:26:08 2016 (serrano)                */
;*    Copyright   :  2007-16 Manuel Serrano                            */
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

	   (read-mp3-frame-mmap ::mmap ::elong ::mp3frame)
	   (read-mp3-frame-input-port ::input-port ::elong ::mp3frame)
	   (read-mp3-frame ::obj ::elong ::mp3frame)

	   (mp3-index::elong ::obj ::elong)))

;*---------------------------------------------------------------------*/
;*    frame-duration ...                                               */
;*---------------------------------------------------------------------*/
(define (frame-duration len bitrate)
   (/fl (fixnum->flonum (elong->fixnum len))
      (*fl (fixnum->flonum bitrate) 125.)))

;*---------------------------------------------------------------------*/
;*    byte-bits ...                                                    */
;*---------------------------------------------------------------------*/
(define (byte-bits byte lo mo)
   (bit-and (bit-rsh byte lo) (-fx (bit-lsh 1 (-fx (+fx 1 mo) lo)) 1)))

;*---------------------------------------------------------------------*/
;*    fill-mp3-frame! ...                                              */
;*---------------------------------------------------------------------*/
(define (fill-mp3-frame! frame i b1 b2 b3)
   
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
   
   (let* ((bitver (byte-bits b1 3 4))
	  (bitlayer (byte-bits b1 1 2))
	  (crc (byte-bits b1 0 0))
	  (bitbrate (byte-bits b2 4 7))
	  (bitsrate (byte-bits b2 2 3))
	  (pding (byte-bits b2 1 1))
	  (bitchannels (byte-bits b3 6 7)))
      (unless (or (=fx bitlayer 00) (=fx bitsrate 3) (=fx bitver 1))
	 (let* ((layer (vector-ref '#(0 3 2 1) bitlayer))
		(version (vector-ref '#(2.5 0. 2. 1.) bitver))
		(brate (bitrate bitbrate bitver bitlayer))
		(srate (vector-ref
			  (vector-ref '#(#(11025 12000 8000 0)
					 #f
					 #(22050 24000 16000 0)
					 #(44100 48000 32000 0))
			     bitver)
			  bitsrate))
		(len (frame-length bitlayer bitver brate srate pding)))
	    (unless (<elong len #e21)
	       (with-access::mp3frame frame (offset version layer crc
					       bitrate samplerate padding
					       channels length duration)
		  (set! offset i)
		  (set! version (vector-ref '#(2.5 0. 2. 1.) bitver))
		  (set! layer layer)
		  (set! crc crc)
		  (set! bitrate brate)
		  (set! samplerate srate)
		  (set! padding pding)
		  (set! channels (vector-ref '#(2 2 1 1) bitchannels))
		  (set! length len)
		  (set! duration (frame-duration len brate))
		  frame))))))

;*---------------------------------------------------------------------*/
;*    read-mp3-frame-mmap ...                                          */
;*---------------------------------------------------------------------*/
(define (read-mp3-frame-mmap mm::mmap offset::elong frame)
   
   (define (mmap-ref-byte mm i)
      (char->integer (mmap-ref mm i)))

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
			   (b3 (mmap-ref-byte mm (+ i #e3))))
		       (or (fill-mp3-frame! frame i b1 b2 b3)
			   (loop (+elong i 1))))
		    (loop (+elong i 1)))))
	    (else
	     (loop (+elong i 1)))))))

;*---------------------------------------------------------------------*/
;*    read-mp3-frame ...                                               */
;*---------------------------------------------------------------------*/
(define (read-mp3-frame-input-port ip::input-port offset::elong frame)

   (define (skip-chars len)
      (let loop ((len (elong->fixnum len)))
	 (when (>fx len 0)
	    (read-byte ip)
	    (loop (-fx len 1)))))
   
   (let ((len (input-port-length ip))
	 (buf (make-string 3)))
      (when (>=elong len #e0)
	 (let loop ((i offset))
	    (cond
	       ((>=elong i (-elong len #e4))
		#f)
	       ((>=elong (-elong i offset) #e8192)
		#f)
	       ((=fx (read-byte ip) 255)
		(let ((b1 (read-byte ip)))
		   (if (=fx (bit-and b1 #xe0) #xe0)
		       (let* ((b2 (read-byte ip))
			      (b3 (read-byte ip)))
			  (if (fill-mp3-frame! frame i b1 b2 b3)
			      (with-access::mp3frame frame (length)
				 (skip-chars (-fx length 4))
				 frame)
			      (begin
				 (string-set! buf 0 (integer->char b1))
				 (string-set! buf 1 (integer->char b2))
				 (string-set! buf 2 (integer->char b3))
				 (unread-string! buf ip)
				 (loop (+elong i 1)))))
		       (begin
			  (unread-char! (integer->char b1))
			  (loop (+elong i 1))))))
	       (else
		(loop (+elong i 1))))))))

;*---------------------------------------------------------------------*/
;*    read-mp3-frame ...                                               */
;*---------------------------------------------------------------------*/
(define (read-mp3-frame obj offset::elong frame)
   (cond
      ((mmap? obj)
       (read-mp3-frame-mmap obj offset frame))
      ((input-port? obj)
       (read-mp3-frame-input-port obj offset frame))
      (else
       (bigloo-type-error "read-mp3-frame" "input-port of mmap" obj))))
      
;*---------------------------------------------------------------------*/
;*    mp3-index ...                                                    */
;*    -------------------------------------------------------------    */
;*    Returns the position in a stream (i.e., a byte number) of a      */
;*    temporal position (i.e., a second number).                       */
;*---------------------------------------------------------------------*/
(define (mp3-index obj pos)

   (define (mp3-index->offset obj)
      (let ((f (instantiate::mp3frame)))
	 (with-access::mp3frame f (offset length duration)
	    (let loop ((pos (exact->inexact pos))
		       (i #e0))
	       (if (<=fl pos 0.)
		   offset
		   (begin
		      (if (read-mp3-frame obj i f)
			  (loop (-fl pos duration) (+ i length))
			  (loop pos (+ i 1)))))))))

   (cond
      ((mmap? obj)
       (mp3-index->offset obj))
      ((input-port? obj)
       (mp3-index->offset obj))
      ((string? obj)
       (if (file-exists? obj)
	   (let ((mm (open-mmap obj :read #t :write #f)))
	      (unwind-protect
		 (mp3-index->offset mm)
		 (close-mmap mm)))
	   (call-with-input-file obj mp3-index->offset)))
      (else
       (bigloo-type-error "mp3-offset" "string, mmap, or input-port" obj))))

