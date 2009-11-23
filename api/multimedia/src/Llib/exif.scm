;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/exif.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 29 05:30:36 2004                          */
;*    Last change :  Fri Nov 20 13:09:33 2009 (serrano)                */
;*    Copyright   :  2004-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Jpeg Exif information                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-exif 

   (export (class exif
	      (version (default #f))
	      (jpeg-encoding (default #f))
	      (jpeg-compress (default #f))
	      (comment (default #f))
	      (%commentpos (default #f))
	      (%commentlen (default #f))
	      (date (default #f))
	      (make (default #f))
	      (model (default #f))
	      (orientation (default 'landscape))
	      (%orientationpos (default #f))
	      (width (default #f))
	      (height (default #f))
	      (ewidth (default #f))
	      (eheight (default #f))
	      (xresolution (default #f))
	      (yresolution (default #f))
	      (resolution-unit (default #f))
	      (focal-length (default #f))
	      (flash (default #f))
	      (fnumber (default #f))
	      (iso (default #f))
	      (shutter-speed-value (default #f))
	      (exposure-time (default #f))
	      (exposure-bias-value (default #f))
	      (aperture (default #f))
	      (metering-mode (default #f))
	      (cdd-width (default #f))
	      (focal-plane-xres (default #f))
	      (focal-plane-units (default #f))
	      (thumbnail (default #f))
	      (thumbnail-path (default #f))
	      (thumbnail-offset (default #f))
	      (thumbnail-length (default #f)))
	   
	   (jpeg-exif ::bstring)
	   (jpeg-exif-comment-set! ::bstring ::bstring)
	   (jpeg-exif-orientation-set! ::bstring ::symbol)

	   (parse-exif-date::date ::bstring)))

;*---------------------------------------------------------------------*/
;*    exif-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (exif-error proc msg obj)
   (error/errno $errno-io-parse-error proc msg obj))

;*---------------------------------------------------------------------*/
;*    get16u ...                                                       */
;*    -------------------------------------------------------------    */
;*    Get a 16 bits unsigned integer                                   */
;*---------------------------------------------------------------------*/
(define (get16u::elong en::bool bytes::bstring o::int)
   (fixnum->elong
    (if en
	(bit-or (bit-lsh (char->integer (string-ref bytes o)) 8)
		(char->integer (string-ref bytes (+fx 1 o))))
	(bit-or (bit-lsh (char->integer (string-ref bytes (+fx 1 o))) 8)
		(char->integer (string-ref bytes o))))))

;*---------------------------------------------------------------------*/
;*    get32u ...                                                       */
;*    -------------------------------------------------------------    */
;*    Get a 32 bits unsigned integer                                   */
;*---------------------------------------------------------------------*/
(define (get32u::elong en::bool bytes::bstring o::int)
   (let ((e0 (fixnum->elong (char->integer (string-ref bytes o))))
	 (e1 (fixnum->elong (char->integer (string-ref bytes (+fx 1 o)))))
	 (e2 (fixnum->elong (char->integer (string-ref bytes (+fx 2 o)))))
	 (e3 (fixnum->elong (char->integer (string-ref bytes (+fx 3 o))))))
      (if en
	  (bit-orelong
	   (bit-lshelong e0 24)
	   (bit-orelong
	    (bit-lshelong e1 16)
	    (bit-orelong (bit-lshelong e2 8) e3)))
	  (bit-orelong
	   (bit-lshelong e3 24)
	   (bit-orelong
	    (bit-lshelong e2 16)
	    (bit-orelong (bit-lshelong e1 8) e0))))))

;*---------------------------------------------------------------------*/
;*    getformat ...                                                    */
;*    -------------------------------------------------------------    */
;*    Get a number according to the specified format.                  */
;*---------------------------------------------------------------------*/
(define (getformat::obj en::bool bytes::bstring o::int fmt::int)
   (case fmt
      ((1 6) 
       ;; FMT_BYTE, FMT_SBYTE
       (char->integer (string-ref bytes o)))
      ((3 8)
       ;; FMT_USHORT, FMT_SSHORT
       (get16u en bytes o))
      ((4 9)
       ;; FMT_ULONG, FMT_SLONG
       (get32u en bytes o))
      ((5 10)
       ;; FMT_URATIONAL, FMT_SRATIONAL
       (let ((num (get32u en bytes o))
	     (den (get32u en bytes (+fx o 4))))
	  (if (=elong den #e0)
	      0
	      (cons num den))))
      (else
       (exif-error 'exif "Unsupported number format" fmt))))

;*---------------------------------------------------------------------*/
;*    getformat/fx ...                                                 */
;*---------------------------------------------------------------------*/
(define (getformat/fx::int en::bool bytes::bstring o::int fmt::int)
   (let ((res (getformat en bytes o fmt)))
      (cond
	 ((fixnum? res)
	  res)
	 ((elong? res)
	  (elong->fixnum res))
	 (else
	  0))))

;*---------------------------------------------------------------------*/
;*    *exif-formats-size* ...                                          */
;*---------------------------------------------------------------------*/
(define *exif-formats-size*
   '#(_ 1 1 2 4 8 1 1 2 4 8 4 8))

;*---------------------------------------------------------------------*/
;*    *jpeg-markers* ...                                               */
;*---------------------------------------------------------------------*/
(define *jpeg-markers*
   (let ((v (make-vector 256 #f)))
      ;;
      (vector-set! v #xc0 'M_SOFO)
      (vector-set! v #xc1 'M_SOF1)
      (vector-set! v #xc2 'M_SOF2)
      (vector-set! v #xc3 'M_SOF3)
      (vector-set! v #xc5 'M_SOF5)
      (vector-set! v #xc6 'M_SOF6)
      (vector-set! v #xc7 'M_SOF7)
      (vector-set! v #xc9 'M_SOF9)
      (vector-set! v #xca 'M_SOFA)
      (vector-set! v #xcb 'M_SOFB)
      (vector-set! v #xcc 'M_SOFC)
      (vector-set! v #xcd 'M_SOFD)
      (vector-set! v #xce 'M_SOFE)
      (vector-set! v #xcf 'M_SOFF)
      ;;
      (vector-set! v #xd8 'M_SOI)
      (vector-set! v #xd9 'M_EOI)
      (vector-set! v #xda 'M_SOS)
      (vector-set! v #xe0 'M_JFIF)
      (vector-set! v #xe1 'M_EXIF)
      (vector-set! v #xfe 'M_COM)
      v))

;*---------------------------------------------------------------------*/
;*    jpeg-marker ...                                                  */
;*---------------------------------------------------------------------*/
(define (jpeg-marker m)
   (vector-ref *jpeg-markers* (char->integer m)))

;*---------------------------------------------------------------------*/
;*    read-jpeg-marker ...                                             */
;*    -------------------------------------------------------------    */
;*    Returns the marker (a char) or #f on failure.                    */
;*---------------------------------------------------------------------*/
(define (read-jpeg-marker p)
   (if (not (char=? (mmap-get-char p) #a255))
       #f
       (jpeg-marker (mmap-get-char p))))

;*---------------------------------------------------------------------*/
;*    remove-trailing-spaces! ...                                      */
;*---------------------------------------------------------------------*/
(define (remove-trailing-spaces! s)
   (let ((len (string-length s)))
      (if (=fx len 0)
	  s
	  (let ((start (-fx len 1)))
	     (let loop ((i start))
		(cond
		   ((char=? (string-ref s i) #\space)
		    (loop (-fx i 1)))
		   ((=fx i 0)
		    "")
		   (else
		    (if (=fx i start)
			s
			(string-shrink! s (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    process-exif-dir! ...                                            */
;*---------------------------------------------------------------------*/
(define (process-exif-dir! en::bool bytes start::int base::int exif o0)
   (define (strncpy o max)
      (let loop ((i 0))
	 (if (=fx i max)
	     (let ((s (make-string i)))
		(blit-string! bytes o s 0 i)
		s)
	     (let ((c (string-ref bytes (+fx i o))))
		(if (char=? c #a000)
		    (let ((s (make-string i)))
		       (blit-string! bytes o s 0 i)
		       s)
		    (loop (+fx i 1)))))))
   (let ((dnum (elong->fixnum (get16u en bytes start))))
      (let loop ((de 0))
	 (when (<fx de dnum)
	    (let* ((da (+ start 2 (*fx 12 de)))
		   (tag (elong->fixnum (get16u en bytes da)))
		   (fmt (elong->fixnum (get16u en bytes (+fx 2 da))))
		   (cmp (get32u en bytes (+fx 4 da))))
	       (let* ((bcount (*fx (elong->fixnum cmp)
				   (vector-ref *exif-formats-size*
					       (elong->fixnum fmt))))
		      (valptr (if (> bcount 4)
				  (let ((ov (get32u en bytes (+fx 8 da))))
				     (+fx base (elong->fixnum ov)))
				  (+fx 8 da))))
		  (case tag
		     ((#x103)
		      ;; TAG_COMPRESS
		      (let ((c (getformat en bytes valptr fmt)))
			 (exif-jpeg-compress-set! exif c)))
		     ((#x10f)
		      ;; TAG_MAKE
		      (exif-make-set! exif (strncpy valptr 31)))
		     ((#x110)
		      ;; TAG_MODEL
		      (exif-model-set! exif (strncpy valptr 39)))
		     ((#x112)
		      ;; TAG_ORIENTATION
		      (let ((o (getformat en bytes valptr fmt)))
			 (exif-orientation-set! exif
						(case o
						   ((#e1)
						    'landscape)
						   ((#e6)
						    'portrait)
						   ((#e8)
						    'upsidedown)
						   (else
						    'seascape)))))
		     ((#x201)
		      ;; TAG_THUMBNAIL_OFFSET
		      (let* ((ol (getformat/fx en bytes valptr fmt))
			     (of (+fx ol base)))
			 (exif-thumbnail-offset-set! exif of)))
		     ((#x202)
		      ;; TAG_THUMBNAIL_LENGTH
		      (let ((le (getformat/fx en bytes valptr fmt)))
			 (exif-thumbnail-length-set! exif le)))
		     ((#x11a)
		      ;; TAG_XRESOLUTION
		      (let ((xr (getformat en bytes valptr fmt)))
			 (exif-xresolution-set! exif xr)))
		     ((#x11b)
		      ;; TAG_YRESOLUTION
		      (let ((yr (getformat en bytes valptr fmt)))
			 (exif-xresolution-set! exif yr)))
		     ((#x128)
		      ;; TAG_RESOLUTION_UNIT
		      (let ((ru (getformat en bytes valptr fmt)))
			 (exif-resolution-unit-set! exif ru)))
		     ((#x132)
		      ;; TAG_DATE_TIME
		      (let ((dt (strncpy valptr 31)))
			 (exif-date-set! exif dt)))
		     ((#x213)
		      ;; TAG_YCbCrPositioning
		      'todo)
		     ((#x829A)
		      ;; TAG_EXPOSURETIME
		      (let ((et (getformat en bytes valptr fmt)))
			 (exif-exposure-time-set! exif et)))
		     ((#x829D)
		      ;; TAG_FNUMBER
		      (let ((fn (getformat en bytes valptr fmt)))
			 (exif-fnumber-set! exif fn)))
		     ((#x8769 #xa005)
		      ;; TAG_EXIF_OFFSET, TAG_INTEROP_OFFSET
		      (let ((ss (+fx base (elong->fixnum (get32u en bytes valptr)))))
			 (process-exif-dir! en bytes ss base exif o0)))
		     ((#x8827)
		      (let ((is (getformat en bytes valptr fmt)))
			 (exif-iso-set! exif is)))
		     ((#x9000)
		      ;; TAG_EXIF_VERSION
		      'todo)
		     ((#x9003 #x9004)
		      ;; TAG_DATE_TIME_ORIGINAL, TAG_DATE_TIME_DIGITIZED
		      (exif-date-set! exif (strncpy valptr 19)))
		     ((#x9201)
		      ;; TAG_SHUTTER_SPEED_VALUE
		      (let ((sv (getformat en bytes valptr fmt)))
			 (exif-shutter-speed-value-set! exif sv)))
		     ((#x9202 #x9205)
		      ;; TAG_APERTURE
		      (let ((ap (getformat en bytes valptr fmt)))
			 (exif-aperture-set! exif ap)))
		     ((#x9204)
		      ;; TAG_EXPOSURE_BIAS_VALUE
		      (let ((bv (getformat en bytes valptr fmt)))
			 (exif-exposure-bias-value-set! exif bv)))
		     ((#x9207)
		      ;; TAG_METERING_MODE
		      (let ((mm (case (getformat/fx en bytes valptr fmt)
				   ((2) "center weight")
				   ((3) "spot")
				   ((5) "matrix")
				   (else "???"))))
			 (exif-metering-mode-set! exif mm)))
		     ((#x920a)
		      ;; TAG_FOCALLENGTH
		      (let ((fl (getformat en bytes valptr fmt)))
			 (exif-focal-length-set! exif fl)))
		     ((#x9209)
		      ;; TAG_FLASH
		      (let* ((fl (getformat/fx en bytes valptr fmt))
			     (f (not (=fx (bit-and fl 7) 0))))
			 (exif-flash-set! exif f)))
		     ((#x9286)
		      ;; TAG_USERCOMMENT
		      (exif-%commentpos-set! exif (+ valptr o0))
		      (exif-%commentlen-set! exif 199)
		      (when (substring-at? bytes "ASCII\000\000\000" valptr)
			 (exif-comment-set! exif
					    (remove-trailing-spaces!
					     (strncpy (+fx 8 valptr) 191)))))
		     ((#xa002)
		      ;; TAG_EXIF_IMAGEWIDTH
		      (let ((w (getformat/fx en bytes valptr fmt)))
			 (exif-ewidth-set! exif w)))
		     ((#xa003)
		      ;; TAG_EXIF_IMAGELENGTH
		      (let ((w (getformat/fx en bytes valptr fmt)))
			 (exif-eheight-set! exif w)))
		     ((#xa20e)
		      ;; TAG_FOCALPLANEXRES
		      (let ((r (getformat en bytes valptr fmt)))
			 (exif-focal-plane-xres-set! exif
						     (if (pair? r)
							 (/ (car r) (cdr r))
							 r))))
		     ((#xa210)
		      ;; TAG_FOCALPLANEUNITS
		      (let ((fpu (case (getformat/fx en bytes valptr fmt)
				    ((1) 25.4)
				    ((2) 25.4)
				    ((3) 10)
				    ((4) 1)
				    ((5) .001))))
			 (exif-focal-plane-units-set! exif fpu)))
		     (else
		      ;; TAG_UNKNOWN
		      'unknown)))
	       (loop (+fx de 1)))))
      (when (< (+ start 2 4 (*fx 12 dnum)) (string-length bytes))
	 (let ((of (get32u en bytes (+ start 2 (*fx 12 dnum)))))
	    (if (>elong of #e0)
		(process-exif-dir! en bytes
				   (+fx (elong->fixnum of) base)
				   base
				   exif
				   o0))))))

;*---------------------------------------------------------------------*/
;*    read-jpeg-exif! ...                                              */
;*---------------------------------------------------------------------*/
(define (read-jpeg-exif! exif bytes pos)
   (define (exif-endianess)
      (cond
	 ((substring-at? bytes "II" 6)
	  ;; Intel is big endian
	  #f)
	 ((substring-at? bytes "MM" 6)
	  ;; Intel is little endian
	  #t)
	 (else
	  (warning 'read-jpeg-exif
		   "Unknown exif endianess, assuminug big endian")
	  #f)))
   (if (and (char=? (string-ref bytes 4) #a000)
	    (char=? (string-ref bytes 5) #a000))
       (let* ((en (exif-endianess))
	      (hd (get16u en bytes 8)))
	  (if (not (=elong hd #e42))
	      (exif-error 'read-jpeg-exif "Illegal exif header" hd)
	      (let ((fo (elong->fixnum (get32u en bytes 10))))
		 (if (or (<fx fo 8) (>fx fo 16))
		     (exif-error 'read-jpeg-exit
				   "Suspicious offset of first IFD value"
				   fo)
		     (begin
			(process-exif-dir! en bytes (+fx 6 fo) 6 exif pos)
			;; CDD width
			(if (and (number? (exif-ewidth exif))
				 (number? (exif-focal-plane-xres exif))
				 (number? (exif-focal-plane-units exif)))
			    (let ((w (/ (* (exif-ewidth exif)
					   (exif-focal-plane-units exif))
					(exif-focal-plane-xres exif))))	
			       (exif-cdd-width-set! exif w)))
			;; thumbnail
			(let ((s (exif-thumbnail-offset exif))
			      (l (exif-thumbnail-length exif)))
			   (if (and (integer? s) (integer? l))
			       (let ((th (make-string l)))
				  (blit-string! bytes s th 0 l)
				  (exif-thumbnail-set! exif th)
				  exif)
			       (exif-thumbnail-set! exif #f))))))))))

;*---------------------------------------------------------------------*/
;*    read-COM! ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-COM! exif bytes pos)
   (let ((len (string-length bytes)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     (exif-comment-set! exif bytes))
	    ((char=? (string-ref bytes i) #a000)
	     (let ((s (make-string i)))
		(blit-string! bytes 0 s 0 i)
		(exif-comment-set! exif s)
		(exif-%commentpos-set! exif pos)
		(exif-%commentlen-set! exif i)))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    read-SOFn! ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-SOFn! exif bytes encoding)
   (exif-width-set! exif (elong->fixnum (get16u #t bytes 3)))
   (exif-height-set! exif (elong->fixnum (get16u #t bytes 1)))
   (exif-jpeg-encoding-set! exif encoding)
   exif)

;*---------------------------------------------------------------------*/
;*    read-jpeg-section ...                                            */
;*---------------------------------------------------------------------*/
(define (read-jpeg-section mm::mmap path)
   ;; padding bytes and section marker
   (let loop ((a 0)
	      (m (mmap-get-char mm)))
      (if (char=? m #a255)
	  (if (>= a 6)
	      (exif-error 'read-jpeg-section "Too many padding bytes" a)
	      (loop (+ a 1) (mmap-get-char mm)))
	  ;; the section length
	  (let* ((lh (char->integer (mmap-get-char mm)))
		 (ll (char->integer (mmap-get-char mm)))
		 (l (fixnum->elong (bit-or (bit-lsh lh 8) ll))))
	     (cond
		((<elong l #e2)
		 (exif-error 'read-jpeg-section "Section too small" a))
		((>=elong (+elong l (mmap-read-position mm)) (mmap-length mm))
		 (exif-error 'read-jpeg-section
			     (format "Premature end of section read: ~s"
				     (-elong (mmap-length mm)
					     (mmap-read-position mm)))
			     (format ", expected: ~s" (- l 2))))
		(else
		 (values (jpeg-marker m)
			 (mmap-get-string mm (-elong l #e2)))))))))

;*---------------------------------------------------------------------*/
;*    read-jpeg-sections ...                                           */
;*---------------------------------------------------------------------*/
(define (read-jpeg-sections exif mm::mmap path)
   (let ((m (read-jpeg-marker mm)))
      (if (not (eq? m 'M_SOI))
	  (exif-error 'read-jpeg-sections "Illegal section marker" m)
	  (let loop ()
	     (multiple-value-bind (m bytes)
		(read-jpeg-section mm path)
		(case m
		   ((M_SOS)
		    'sos)
		   ((M_EOI)
		    'eoi)
		   ((M_COM)
		    (read-COM! exif
			       bytes
			       (- (mmap-read-position mm)
				  (string-length bytes)))
		    (loop))
		   ((M_EXIF)
		    (if (substring=? bytes "Exif" 4)
			(read-jpeg-exif! exif
					 bytes
					 (- (mmap-read-position mm)
					    (string-length bytes))))
		    (loop))
		   ((M_SOFO)
		    (read-SOFn! exif bytes "baseline")
		    (loop))
		   ((M_SOF1)
		    (read-SOFn! exif bytes "extended sequential")
		    (loop))
		   ((M_SOF2)
		    (read-SOFn! exif bytes "progressive")
		    (loop))
		   ((M_SOF3)
		    (read-SOFn! exif bytes "lossless")
		    (loop))
		   ((M_SOF5)
		    (read-SOFn! exif bytes "differential sequential")
		    (loop))
		   ((M_SOF6)
		    (read-SOFn! exif bytes "differential progressive")
		    (loop))
		   ((M_SOF7)
		    (read-SOFn! exif bytes "differential lossless")
		    (loop))
		   ((M_SOF9)
		    (read-SOFn! exif bytes "extended sequential, arithmetic coding")
		    (loop))
		   ((M_SOFA)
		    (read-SOFn! exif bytes "progressive, arithmetic coding")
		    (loop))
		   ((M_SOFB)
		    (read-SOFn! exif bytes "lossless, arithmetic coding")
		    (loop))
		   ((M_SOFC)
		    (read-SOFn! exif bytes "differential sequential, arithmetic coding")
		    (loop))
		   ((M_SOFD)
		    (read-SOFn! exif bytes "differential progressive, arithmetic coding")
		    (loop))
		   ((M_SOFE)
		    (read-SOFn! exif bytes "differential lossless, arithmetic coding")
		    (loop))
		   ((M_SOFF)
		    (read-SOFn! exif bytes "?")
		    (loop))
		   (else
		    (loop)))))))
   exif)

;*---------------------------------------------------------------------*/
;*    jpeg-exif ...                                                    */
;*    -------------------------------------------------------------    */
;*    This is the main function which reads a JPEG image a             */
;*    returns an EXIF structure.                                       */
;*---------------------------------------------------------------------*/
(define (jpeg-exif path)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
		    'jpeg-exif "Can't find file" path)
       (let ((mm (open-mmap path write: #f))
	     (exif (instantiate::exif)))
	  (unwind-protect
	     (when (> (mmap-length mm) 0)
		(read-jpeg-sections exif mm path))
	     (close-mmap mm))
	  exif)))

;*---------------------------------------------------------------------*/
;*    jpeg-exif-comment-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (jpeg-exif-comment-set! path comment)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
		    'jpeg-exif-comment-set! "Can't find file" path)
       (let ((mm (open-mmap path))
	     (exif (instantiate::exif))
	     (mtime #f))
	  (unwind-protect
	     (when (> (mmap-length mm) 0)
		(read-jpeg-sections exif mm path)
		(with-access::exif exif (%commentpos %commentlen)
		   (and %commentpos
			(let* ((len (string-length comment))
			       (s (if (<fx len %commentlen)
				      comment
				      (substring comment 0 %commentlen))))
			   (mmap-write-position-set! mm %commentpos)
			   (mmap-put-string! mm "ASCII\000\000\000")
			   (mmap-put-string! mm s)
			   (mmap-put-string! mm "\000")
			   (set! mtime #t)
			   s))))
	     (begin
		(close-mmap mm)
		;; the glibc is buggous, since mmap does not update mtime,
		;; we force the update here
		(when mtime
		   (let ((pr (open-input-file path))
			 (pw (append-output-file path)))
		      (let ((c (read-char pr)))
			 (set-output-port-position! pw 0)
			 (write-char c pw))
		      (close-input-port pr)
		      (close-output-port pw))))))))
   
;*---------------------------------------------------------------------*/
;*    jpeg-exif-orientation-set! ...                                   */
;*---------------------------------------------------------------------*/
(define (jpeg-exif-orientation-set! path orientation)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
		    'jpeg-exif-comment-set! "Can't find file" path)
       (let ((mm (open-mmap path))
	     (exif (instantiate::exif))
	     (mtime #f))
	  (unwind-protect
	     (when (> (mmap-length mm) 0)
		(read-jpeg-sections exif mm path)
		(with-access::exif exif (%orientationpos)
		   (when %orientationpos
		      (mmap-write-position-set! mm %orientationpos)
		      (case orientation
			 ((landscape) (mmap-put-string! mm "\001"))
			 ((portrait) (mmap-put-string! mm "\006"))
			 ((upsidedonw) (mmap-put-string! mm "\010"))
			 ((seascape) (mmap-put-string! mm "\001")))
		      (set! mtime #t)
		      orientation)))
	     (begin
		(close-mmap mm)
		;; the glibc is buggous, since mmap does not update mtime,
		;; we force the update here
		(when mtime
		   (let ((pr (open-input-file path))
			 (pw (append-output-file path)))
		      (let ((c (read-char pr)))
			 (set-output-port-position! pw 0)
			 (write-char c pw))
		      (close-input-port pr)
		      (close-output-port pw))))))))
   
;*---------------------------------------------------------------------*/
;*    parse-exif-date ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-exif-date d)
   
   (define (parse-error d i)
      (raise (instantiate::&io-parse-error
		(proc 'parse-exif-date)
		(msg "Illegal syntax")
		(obj (format "~a{~a}~a"
			     (substring d 0 i)
			     (string-ref d i)
			     (substring d (+fx i 1) (string-length d)))))))
   
   (define (substring->int d i len)
      (let ((len (+fx i len))
	    (zero (char->integer #\0)))
	 (let loop ((i i)
		    (acc 0))
	    (if (=fx i len)
		acc
		(let ((v (-fx (char->integer (string-ref d i)) zero)))
		   (if (or (<fx v 0) (>fx v 9))
		       (parse-error d i)
		       (loop (+fx i 1) (+fx v (*fx acc 10)))))))))
   
   (if (and (=fx (string-length d) 19)
	    (char=? (string-ref d 4) #\:)
	    (char=? (string-ref d 7) #\:)
	    (char=? (string-ref d 10) #\space)
	    (char=? (string-ref d 13) #\:)
	    (char=? (string-ref d 16) #\:))
       (make-date
	:sec (substring->int d 17 2)
	:min (substring->int d 14 2)
	:hour (substring->int d 11 2)
	:day (substring->int d 8 2)
	:month (substring->int d 5 2)
	:year (substring->int d 0 4))
       (parse-error d 0)))
