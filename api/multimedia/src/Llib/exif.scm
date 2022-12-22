;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/multimedia/src/Llib/exif.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 29 05:30:36 2004                          */
;*    Last change :  Sun Dec  4 06:38:54 2022 (serrano)                */
;*    Copyright   :  2004-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Jpeg Exif information                                            */
;*    -------------------------------------------------------------    */
;*    See https://exiftool.org/TagNames/EXIF.html                      */
;*        https://exiftool.org/TagNames/GPS.html                       */
;*        https://enqtran.com/standard-exif-tags/                      */
;*        https://learn.microsoft.com/en-us/windows/win32/gdiplus      */
;*           /-gdiplus-constant-property-item-                         */
;*           descriptions?redirectedfrom=MSDN                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-exif 

   (export (class exif
	      (version (default #f))
	      (jpeg-encoding (default #f))
	      (jpeg-compress (default #f))
	      (software (default #f))
	      (comment (default #f))
	      (%commentpos (default #f))
	      (%commentlen (default #f))
	      (description (default #f))
	      (date (default #f))
	      (offset-time (default #f))
	      (offset-time-original (default #f))
	      (offset-time-digitized (default #f))
	      (make (default #f))
	      (model (default #f))
	      (unique-camera-model (default #f))
	      (camera-serial-number (default #f))
	      (orientation (default 'landscape))
	      (%orientationpos (default #f))
	      (width (default #f))
	      (height (default #f))
	      (iwidth (default #f))
	      (ilength (default #f))
	      (ewidth (default #f))
	      (eheight (default #f))
	      (xresolution (default #f))
	      (yresolution (default #f))
	      (resolution-unit (default #f))
	      (focal-length (default #f))
	      (focal-length35 (default #f))
	      (custom-render (default #f))
	      (exposure-mode (default #f))
	      (white-balance (default #f))
	      (digital-zoom-ratio (default #f))
	      (light-source (default #f))
	      (flash (default #f))
	      (fnumber (default #f))
	      (iso (default #f))
	      (shutter-speed-value (default #f))
	      (exposure-program (default #f))
	      (exposure-time (default #f))
	      (exposure-bias-value (default #f))
	      (aperture (default #f))
	      (max-aperture (default #f))
	      (metering-mode (default #f))
	      (cdd-width (default #f))
	      (focal-plane-xres (default #f))
	      (focal-plane-yres (default #f))
	      (focal-plane-units (default #f))
	      (gps-tag (default #f))
	      (brightness-value (default #f))
	      (subject-distance (default #f))
	      (colorspace (default #f))
	      (sensing-method (default #f))
	      (composite-image (default #f))
	      (lens-make (default #f))
	      (lens-model (default #f))
	      (digitalZoomRatio (default #f))
	      (scene-capture-type (default "???"))
	      (gain-control (default "???"))
	      (contrast (default "???"))
	      (saturation (default "???"))
	      (sharpness (default "???"))
	      (distance-subject-range (default #f))
	      (new-subfile-type (default #f))
	      (YCbCr-coef (default #f))
	      (YCbCr-subsampling (default #f))
	      (YCbCr-positioning (default #f))
	      (refere (default #f))
	      (rating (default #f))
	      (rating-percentage (default #f))
	      (copyright (default #f))
	      (artist (default #f))
	      (maker-node (default #f))
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
(define (get16u::elong en::bool bytes o::int)
   
   (define (string-get16u::elong en::bool bytes::bstring o::int)
      (fixnum->elong
	 (if en
	     (bit-or (bit-lsh (char->integer (string-ref bytes o)) 8)
		(char->integer (string-ref bytes (+fx 1 o))))
	     (bit-or (bit-lsh (char->integer (string-ref bytes (+fx 1 o))) 8)
		(char->integer (string-ref bytes o))))))
   
   (define (mmap-get16u::elong en::bool bytes::mmap o::long)
      (fixnum->elong
	 (if en
	     (bit-or (bit-lsh (char->integer (mmap-ref bytes o)) 8)
		(char->integer (mmap-ref bytes (+fx 1 o))))
	     (bit-or (bit-lsh (char->integer (mmap-ref bytes (+fx 1 o))) 8)
		(char->integer (mmap-ref bytes o))))))
   
   (if (string? bytes)
       (string-get16u en bytes o)
       (mmap-get16u en bytes o)))

;*---------------------------------------------------------------------*/
;*    get32u ...                                                       */
;*    -------------------------------------------------------------    */
;*    Get a 32 bits unsigned integer                                   */
;*---------------------------------------------------------------------*/
(define (get32u::elong en::bool bytes o::int)
   
   (define (string-get32u::elong en::bool bytes::bstring o::int)
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
   
   (define (mmap-get32u::elong en::bool bytes::mmap o::long)
      (let ((e0 (fixnum->elong (char->integer (mmap-ref bytes o))))
	    (e1 (fixnum->elong (char->integer (mmap-ref bytes (+fx 1 o)))))
	    (e2 (fixnum->elong (char->integer (mmap-ref bytes (+fx 2 o)))))
	    (e3 (fixnum->elong (char->integer (mmap-ref bytes (+fx 3 o))))))
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
   
   (if (string? bytes)
       (string-get32u en bytes o)
       (mmap-get32u en bytes o)))

;*---------------------------------------------------------------------*/
;*    getformat ...                                                    */
;*    -------------------------------------------------------------    */
;*    Get a number according to the specified format.                  */
;*---------------------------------------------------------------------*/
(define (getformat::obj en::bool bytes o::int fmt::int)
   (case fmt
      ((1 6) 
       ;; FMT_BYTE, FMT_SBYTE
       (char->integer
	  (if (string? bytes)
	      (string-ref bytes o)
	      (mmap-ref bytes (fixnum->elong o)))))
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
      ((11)
       ;; FMT_SINGLE 32bit
       (exif-error "exif" "Unsupported number format" fmt))
      ((12)
       ;; FMT_DOUBLE 64bit
       (exif-error "exif" "Unsupported number format" fmt))
      (else
       0)))
;*        (exif-error "exif" "Unsupported number format" fmt))))       */

;*---------------------------------------------------------------------*/
;*    getformat/fx ...                                                 */
;*---------------------------------------------------------------------*/
(define (getformat/fx::int en::bool bytes o::int fmt::int)
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

      (define (string-strncpy o max)
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
      
      (define (mmap-strncpy o max)
	 (let loop ((i 0))
	    (if (=fx i max)
		(mmap-substring bytes o (+fx o i))
		(let ((c (mmap-ref bytes (+fx i o))))
		   (if (char=? c #a000)
		       (mmap-substring bytes o (+fx i o))
		       (loop (+fx i 1)))))))
      
      (if (string? bytes)
	  (string-strncpy o max)
	  (mmap-strncpy o max)))

   (define (process-exif-gps-tag o bcount)
      '(let* ((tag (elong->fixnum (get16u en bytes o))))
	(tprint "TAG=" (integer->string tag 16) " "))
      #f)
      
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
		     ((#x1)
		      ;; INTEROPINDEX
		      #unspecified)
		     ((#x1)
		      ;; TAG_GPS_LATITUDE_REF
		      (let ((lr (strncpy valptr bcount)))
			 (tprint "gps latitude ref")
			 #unspecified))
		     ((#x2)
		      ;; TAG_GPS_LATITUDE
		      (let ((la (strncpy valptr bcount)))
			 (tprint "gps latitude")
			 #unspecified))
		     ((#x3)
		      ;; TAG_GPS_LONGITUDE_REF
		      (let ((lr (strncpy valptr bcount)))
			 (tprint "gps longitude ref")
			 #unspecified))
		     ((#x4)
		      ;; TAG_GPS_LONGITUDE_REF
		      (tprint "tag gps longitude...")
		      #unspecified)
		     ((#x5)
		      ;; TAG_GPS_ALTITUDE_REF
		      (tprint "tag gps altitude ref")
		      #unspecified)
		     ((#x6)
		      ;; TAG_GPS_ALTITUDE
		      (tprint "tag gps altitude")
		      #unspecified)
		     ((#x7)
		      ;; TAG_GPS_TIMESPTAMP
		      (tprint "tag gps timstamp")
		      #unspecified)
		     ((#x8)
		      ;; TAG_GPS_SATELITE
		      (tprint "tag gps satelite [" (strncpy valptr bcount) "]")
		      #unspecified)
		     ((#x9)
		      ;; TAG_GPS_STATUS
		      (tprint "tag gps status [" (strncpy valptr bcount) "]")
		      #unspecified)
		     ((#xa)
		      ;; TAG_GPS_MEASURE_EMODE
		      (tprint "tag gps measure mode [" (strncpy valptr bcount) "]")
		      #unspecified)
		     ((#xb)
		      ;; TAG_GPS_DOP
		      #unspecified)
		     ((#xc)
		      ;; TAG_GPS_SPEED_REF
		      #unspecified)
		     ((#xd)
		      ;; TAG_GPS_SPEED
		      #unspecified)
		     ((#xe)
		      ;; TAG_GPS_TRACK_REF
		      #unspecified)
		     ((#xf)
		      ;; TAG_GPS_TRACK
		      #unspecified)
		     ((#x10)
		      ;; TAG_GPS_IMG_DIRECTION_REF
		      #unspecified)
		     ((#x11)
		      ;; TAG_GPS_IMG_DIRECTION
		      #unspecified)
		     ((#x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1a
			 #x1b #x1c #x1d #x1e #x1f)
		      ;; TAG_GPS mix
		      #unspecified)
		     ((#xfe)
		      ;; NEW SUBFILE TYPE
		      (let ((s (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (new-subfile-type)
			    (set! new-subfile-type s))))
		     ((#x100)
		      ;; IMAGE_WIDTH
		      (let ((w (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (iwidth)
			    (set! iwidth w))))
		     ((#x101)
		      ;; IMAGE_LENGTH
		      (let ((l (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (ilength)
			    (set! ilength l))))
		     ((#x102)
		      ;; BIT_PER_SAMPLE
		      'todo)
		     ((#x103)
		      ;; COMPRESSION
		      (let ((c (getformat en bytes valptr fmt)))
			 (with-access::exif exif (jpeg-compress)
			    (set! jpeg-compress c))))
		     ((#x106)
		      ;; TAG_COMPRESS
		      (let ((c (getformat en bytes valptr fmt)))
			 (with-access::exif exif (jpeg-compress)
			    (set! jpeg-compress c))))
		     ((#x10e)
		      ;; TAG_DESCRIPTION
		      (with-access::exif exif (description)
			 (set! description (strncpy valptr bcount))))
		     ((#x10f)
		      ;; TAG_MAKE
		      (with-access::exif exif (make)
			 (set! make (strncpy valptr 31))))
		     ((#x110)
		      ;; TAG_MODEL
		      (with-access::exif exif (model)
			 (set! model (strncpy valptr 39))))
		     ((#x111)
		      ;; STRIP_OFFSET
		      'todo)
		     ((#x112)
		      ;; TAG_ORIENTATION
		      (let ((o (getformat en bytes valptr fmt)))
			 (with-access::exif exif (orientation)
			    (set! orientation
			       (case o
				  ((#e1) 'landscape)
				  ((#e6) 'portrait)
				  ((#e8) 'upsidedown)
				  (else 'seascape))))))
		     ((#x115)
		      ;; SAMPLE_PER_PIXEL
		      'todo)
		     ((#x116)
		      ;; ROWS_PER_STRIP
		      'todo)
		     ((#x117)
		      ;; STRIP_BYTE_COUNT
		      'todo)
		     ((#x11a)
		      ;; TAG_XRESOLUTION
		      (let ((xr (getformat en bytes valptr fmt)))
			 (with-access::exif exif (xresolution)
			    (set! xresolution xr))))
		     ((#x11b)
		      ;; TAG_YRESOLUTION
		      (let ((yr (getformat en bytes valptr fmt)))
			 (with-access::exif exif (yresolution)
			    (set! yresolution yr))))
		     ((#x11c)
		      ;; PLANAR_CONFIGURATION
		      'todo)
		     ((#x128)
		      ;; TAG_RESOLUTION_UNIT
		      (let ((ru (getformat en bytes valptr fmt)))
			 (with-access::exif exif (resolution-unit)
			    (set! resolution-unit ru))))
		     ((#x131)
		      ;; TAG_SOFTWARE
		      (with-access::exif exif (software)
			 (set! software (strncpy valptr bcount))))
		     ((#x132)
		      ;; TAG_DATE_TIME
		      (let ((dt (strncpy valptr 31)))
			 (with-access::exif exif (date)
			    (set! date (parse-exif-date dt)))))
		     ((#x13b)
		      ;; ARTIST
		      (with-access::exif exif (artist)
			 (set! artist (strncpy valptr bcount))))
		     ((#x14a)
		      ;; TIFF_SUB_ID
		      'ignored)
		     ((#x201)
		      ;; TAG_THUMBNAIL_OFFSET
		      (let* ((ol (getformat/fx en bytes valptr fmt))
			     (of (+fx ol base)))
			 (with-access::exif exif (thumbnail-offset)
			    (set! thumbnail-offset of))))
		     ((#x202)
		      ;; TAG_THUMBNAIL_LENGTH
		      (let ((le (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (thumbnail-length)
			    (set! thumbnail-length le))))
		     ((#x203)
		      ;; JPEGR
		      'todo)
		     ((#x205)
		      ;; JPEGLo
		      'todo)
		     ((#x206)
		      ;; JPEGP
		      'todo)
		     ((#x207)
		      ;; JPEGQ
		      'todo)
		     ((#x208)
		      ;; JPEGD
		      'todo)
		     ((#x208)
		      ;; JPEGA
		      'todo)
		     ((#x211)
		      ;; TAG_YCbCrcoef
		      (let ((co (getformat en bytes valptr fmt)))
			 (with-access::exif exif (YCbCr-coef)
			    (set! YCbCr-coef co))))
		     ((#x212)
		      ;; TAG_YCbCrsubsampling
		      (let ((ss (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (YCbCr-subsampling)
			    (set! YCbCr-subsampling ss))))
		     ((#x213)
		      ;; TAG_YCbCrPositioning
		      (let ((pos (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (YCbCr-positioning)
			    (set! YCbCr-positioning pos))))
		     ((#x214)
		      ;; REFERE
		      (let ((ref (getformat en bytes valptr fmt)))
			 (with-access::exif exif (refere)
			    (set! refere ref))))
		     ((#x4746)
		      ;; RATING
		      (let ((ra (getformat en bytes valptr fmt)))
			 (with-access::exif exif (rating)
			    (set! rating ra))))
		     ((#x4749)
		      ;; RATING PERCENT
		      (let ((ra (getformat en bytes valptr fmt)))
			 (with-access::exif exif (rating-percentage)
			    (set! rating-percentage ra))))
		     ((#x8298)
		      ;; COPYRIGHT
		      (with-access::exif exif (copyright)
			 (set! copyright (strncpy valptr bcount))))
		     ((#x829A)
		      ;; TAG_EXPOSURETIME
		      (let ((et (getformat en bytes valptr fmt)))
			 (with-access::exif exif (exposure-time)
			    (set! exposure-time et))))
		     ((#x829D)
		      ;; TAG_FNUMBER
		      (let ((fn (getformat en bytes valptr fmt)))
			 (with-access::exif exif (fnumber)
			    (set! fnumber fn))))
		     ((#x8769)
		      ;; TAG_EXIF_OFFSET
		      (let ((ss (+fx base (elong->fixnum (get32u en bytes valptr)))))
			 (process-exif-dir! en bytes ss base exif o0)))
		     ((#x8822)
		      ;; EXPOSURE_PROGRAM
		      (let ((e (getformat en bytes valptr fmt)))
			 (with-access::exif exif (exposure-program)
			    (set! exposure-program e))))
		     ((#x8825)
		      ;; GPS_TAG
		      (process-exif-gps-tag valptr bcount)
		      (let ((t (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (gps-tag)
			    (set! gps-tag t))))
		     ((#x8827)
		      ;; ISO
		      (let ((is (getformat en bytes valptr fmt)))
			 (with-access::exif exif (iso)
			    (set! iso is))))
		     ((#x9000)
		      ;; TAG_EXIF_VERSION
		      'todo)
		     ((#x9003 #x9004)
		      ;; TAG_DATE_TIME_ORIGINAL, TAG_DATE_TIME_DIGITIZED
		      (with-access::exif exif (date)
			 (set! date
			    (parse-exif-date
			       (strncpy valptr 19)))))
		     ((#x9010)
		      ;; OFFSET_TIME
		      (with-access::exif exif (offset-time)
			 (set! offset-time (strncpy valptr bcount))))
		     ((#x9011)
		      ;; OFFSET_TIME_ORIGINAL
		      (with-access::exif exif (offset-time-original)
			 (set! offset-time-original (strncpy valptr bcount))))
		     ((#x9012)
		      ;; OFFSET_TIME_DIGITIZED
		      (with-access::exif exif (offset-time-digitized)
			 (set! offset-time-digitized (strncpy valptr bcount))))
		     ((#x9101)
		      ;; COMPONENT_CONFIGURATION
		      #unspecified)
		     ((#x9102)
		      ;; COMPRESS_BIT_PER_PIXEL
		      #unspecified)
		     ((#x9201)
		      ;; TAG_SHUTTER_SPEED_VALUE
		      (let ((sv (getformat en bytes valptr fmt)))
			 (with-access::exif exif (shutter-speed-value)
			    (set! shutter-speed-value sv))))
		     ((#x9203)
		      ;; TAG_BRIGHTNESS_VALUE
		      (let ((bv (getformat en bytes valptr fmt)))
			 (with-access::exif exif (brightness-value)
			    (set! brightness-value bv))))
		     ((#x9202)
		      ;; TAG_APERTURE
		      (let ((ap (getformat en bytes valptr fmt)))
			 (with-access::exif exif (aperture)
			    (set! aperture ap))))
		     ((#x9204)
		      ;; TAG_EXPOSURE_BIAS_VALUE
		      (let ((bv (getformat en bytes valptr fmt)))
			 (with-access::exif exif (exposure-bias-value)
			    (set! exposure-bias-value bv))))
		     ((#x9205)
		      ;; TAG_MAX_APERTURE
		      (let ((ap (getformat en bytes valptr fmt)))
			 (with-access::exif exif (max-aperture)
			    (set! max-aperture ap))))
		     ((#x9206)
		      ;; TAG_SUBJECT_DISTANCE
		      (let ((sd (getformat en bytes valptr fmt)))
			 (with-access::exif exif (subject-distance)
			    (set! subject-distance sd))))
		     ((#x9207)
		      ;; TAG_METERING_MODE
		      (let ((mm (case (getformat/fx en bytes valptr fmt)
				   ((2) "center weight")
				   ((3) "spot")
				   ((5) "matrix")
				   (else "???"))))
			 (with-access::exif exif (metering-mode)
			    (set! metering-mode mm))))
		     ((#x9208)
		      ;; TAG_LIGHTSOURCE
		      (let* ((fl (getformat/fx en bytes valptr fmt))
			     (f (not (=fx (bit-and fl 7) 0))))
			 (with-access::exif exif (light-source)
			    (set! light-source f))))
		     ((#x9209)
		      ;; TAG_FLASH
		      (let* ((fl (getformat/fx en bytes valptr fmt))
			     (f (not (=fx (bit-and fl 7) 0))))
			 (with-access::exif exif (flash)
			    (set! flash f))))
		     ((#x920a)
		      ;; TAG_FOCALLENGTH
		      (let ((fl (getformat en bytes valptr fmt)))
			 (with-access::exif exif (focal-length)
			    (set! focal-length fl))))
		     ((#x9286)
		      ;; TAG_USERCOMMENT
		      (with-access::exif exif (%commentpos
						 %commentlen
						 comment)
			 (set! %commentpos (+ valptr o0))
			 (set! %commentlen 199)
			 (when (substring-at? bytes "ASCII\000\000\000" valptr)
			    (set! comment
			       (remove-trailing-spaces!
				  (strncpy (+fx 8 valptr) 191))))))
		     ((#x927c)
		      ;; MAKER NOTE
		      (with-access::exif exif (maker-note)
			 (strncpy valptr bcount)))
		     ((#x9290)
		      ;; SUB_SEC_TIME (ascii)
		      #unspecified)
		     ((#x9291)
		      ;; SUB_SEC_TIME_ORIGINAL (ascii)
		      #unspecified)
		     ((#x9292)
		      ;; SUB_SEC_TIME_DIGITIZED (ascii)
		      #unspecified)
		     ((#xa000)
		      ;; TAG_FLASH_PIX_VERSION
		      #unspecified)
		     ((#xa001)
		      ;; TAG_COLOR_SPACE
		      (let ((cs (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (colorspace)
			    (set! colorspace cs))))
		     ((#xa002)
		      ;; TAG_EXIF_IMAGEWIDTH
		      (let ((w (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (ewidth)
			    (set! ewidth w))))
		     ((#xa003)
		      ;; TAG_EXIF_IMAGELENGTH
		      (let ((w (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (eheight)
			    (set! eheight w))))
		     ((#xa005)
		      ;; TAG_INTEROP_OFFSET
		      #unspecified)
		     ((#xa20e)
		      ;; TAG_FOCALPLANEXRES
		      (let ((r (getformat en bytes valptr fmt)))
			 (with-access::exif exif (focal-plane-xres)
			    (set! focal-plane-xres
			       (if (pair? r) (/ (car r) (cdr r)) r)))))
		     ((#xa20f)
		      ;; TAG_FOCALPLANEYRES
		      (let ((r (getformat en bytes valptr fmt)))
			 (with-access::exif exif (focal-plane-yres)
			    (set! focal-plane-yres
			       (if (pair? r) (/ (car r) (cdr r)) r)))))
		     ((#xa210)
		      ;; TAG_FOCALPLANEUNITS
		      (let ((fpu (case (getformat/fx en bytes valptr fmt)
				    ((1) 25.4)
				    ((2) 25.4)
				    ((3) 10)
				    ((4) 1)
				    ((5) .001))))
			 (with-access::exif exif (focal-plane-units)
			    (set! focal-plane-units fpu))))
		     ((#xa217)
		      ;; TAG_SENSING_METHOD
		      (let ((sm (getformat en bytes valptr fmt)))
			 (with-access::exif exif (sensing-method)
			    (set! sensing-method sm))))
		     ((#xa300)
		      ;; FILE_SOURCE
		      #unspecified)
		     ((#xa301)
		      ;; TAG_SCENE_TYPE
		      #unspecified)
		     ((#xa401)
		      ;; CUSTOM_RENDER
		      (let ((cr (case (getformat/fx en bytes valptr fmt)
				   ((0) "normal")
				   ((1) "custom")
				   ((2) "HDR (no original saved)")
				   ((3) "HDR (original saved)")
				   ((4) "original (for HDR)")
				   ((6) "panorama")
				   ((7) "portrait HDR")
				   ((8) "portraitauto")
				   (else "???"))))
			 (with-access::exif exif (custom-render)
			    (set! custom-render cr))))
		     ((#xa402)
		      ;; EXPOSURE_MODE
		      (let ((em (case (getformat/fx en bytes valptr fmt)
				   ((0) "auto")
				   ((1) "manual")
				   ((1) "auto bracket")
				   (else "???"))))
			 (with-access::exif exif (exposure-mode)
			    (set! exposure-mode em))))
		     ((#xa403)
		      ;; WHITE_BALANCE
		      (let ((wb (case (getformat/fx en bytes valptr fmt)
				   ((0) "auto")
				   ((1) "manual")
				   (else "???"))))
			 (with-access::exif exif (white-balance)
			    (set! white-balance wb))))
		     ((#xa404)
		      ;; DIGITAL_ZOOM_RATIO
		      (let ((dz (getformat en bytes valptr fmt)))
			 (with-access::exif exif (digital-zoom-ratio)
			    (set! digital-zoom-ratio dz))))
		     ((#xa405)
		      ;; FOCAL_LENGTH_IN_35MM
		      (let ((fl (getformat en bytes valptr fmt)))
			 (with-access::exif exif (focal-length35)
			    (set! focal-length35 fl))))
		     ((#xa406)
		      ;; SCENE_CAPTURE_TYPE
		      (let ((mm (case (getformat/fx en bytes valptr fmt)
				   ((0) "standard")
				   ((1) "landscape")
				   ((2) "portrait")
				   ((3) "night")
				   ((3) "other")
				   (else "???"))))
			 (with-access::exif exif (scene-capture-type)
			    (set! scene-capture-type mm))))
		     ((#xa407)
		      ;; GAIN_CONTROL
		      (let ((gc (case (getformat/fx en bytes valptr fmt)
				   ((0) "none")
				   ((1) "low gain up")
				   ((2) "high gain up")
				   ((3) "low gain down")
				   ((4) "high gain down")
				   (else "???"))))
			 (with-access::exif exif (gain-control)
			    (set! gain-control gc))))
		     ((#xa408)
		      ;; CONTRAST
		      (let ((mm (case (getformat/fx en bytes valptr fmt)
				   ((0) "normal")
				   ((1) "low")
				   ((2) "hight")
				   (else "???"))))
			 (with-access::exif exif (contrast)
			    (set! contrast mm))))
		     ((#xa409)
		      ;; SATURATION
		      (let ((mm (case (getformat/fx en bytes valptr fmt)
				   ((0) "normal")
				   ((1) "low")
				   ((2) "hight")
				   (else "???"))))
			 (with-access::exif exif (saturation)
			    (set! saturation mm))))
		     ((#xa40a)
		      ;; SHARPNESS
		      (let ((mm (case (getformat/fx en bytes valptr fmt)
				   ((0) "normal")
				   ((1) "soft")
				   ((2) "hard")
				   (else "???"))))
			 (with-access::exif exif (sharpness)
			    (set! sharpness mm))))
		     ((#xa40c)
		      ;; DISTANCE_SUBJECT_RANGE
		      (let ((d (case (getformat/fx en bytes valptr fmt)
				  ((0) "unkown")
				  ((1) "macro")
				  ((2) "close")
				  ((3) "distant")
				  (else "???"))))
			 (with-access::exif exif (distance-subject-range)
			    (set! distance-subject-range d))))
		     ((#xa433)
		      ;; LENS_MAKE
		      (with-access::exif exif (lens-make)
			 (set! lens-make (strncpy valptr bcount))))
		     ((#xa434)
		      ;; LENS_MODEL
		      (with-access::exif exif (lens-model)
			 (set! lens-model (strncpy valptr bcount))))
		     ((#xa460)
		      ;; COMPOSITE_IMAGE
		      (with-access::exif exif (composite-image)
			 (set! composite-image (strncpy valptr bcount))))
		     ((#xc612 #xc613)
		      ;; DNG version
		      'ignored)
		     ((#xc614)
		      ;; UNIQUE CAMERA MODEL
		      (with-access::exif exif (unique-camera-model)
			 (set! unique-camera-model (strncpy valptr bcount))))
		     ((#xc615)
		      ;; LOCALIZED UNIQUE CAMERA MODEL
		      'ignored)
		     ((#xc621 #xc622 #xc623)
		      ;; CAMERA CALIBRAIION ...
		      'ignored)
		     ((#xc624)
		      ;; CAMERA CALIBRAIION2
		      'ignored)
		     ((#xc625)
		      ;; RESOLUTION MATRIX1
		      'ignored)
		     ((#xc626)
		      ;; RESOLUTION MATRIX2
		      'ignored)
		     ((#xc627)
		      ;; ANALOG BALANCE
		      'ignored)
		     ((#xc628)
		      ;; AS SHOT
		      'ignored)
		     ((#xc629)
		      ;; AS SHOT
		      'ignored)
		     ((#xc62a)
		      ;; BASELINE EXPOSURE
		      'ignored)
		     ((#xc62b)
		      ;; BASELINE NOISE
		      'ignored)
		     ((#xc62c)
		      ;; BASELINE SHARPNESS
		      'ignored)
		     ((#xc62d)
		      ;; BAYER GREEN SPLIT
		      'ignored)
		     ((#xc62e)
		      ;; LINEAR RESPONSE LIMIT
		      'ignored)
		     ((#xc62f)
		      ;; CAMERA SERIAL NUMBER
		      (with-access::exif exif (camera-serial-number)
			 (set! camera-serial-number (strncpy valptr bcount))))
		     ((#xc633)
		      ;; SHADOW SCALE
		      'ignored)
		     ((#xc634)
		      ;; DNGPrivateData
		      'ignored)
		     ((#xc635)
		      ;; MakerNoteSafety
		      'ignored)
		     ((#xc65a #xc65b #xc65c)
		      ;; SHORT ILLUMINATION ILLUMINANT1 ...
		      'ignored)
		     ((#xc65d)
		      ;; BYTE DATA UNIQUE ID
		      'ignored)
		     ((#xc6f3)
		      ;; CAMERA CALIBRATION SIGNATURE
		      'ignored)
		     ((#xc6f4)
		      ;; PROFILE CALIBRATION SIGNATURE
		      'ignored)
		     ((#xc6f6)
		      ;; AS SHOT PROFILE NAME
		      'ignored)
		     ((#xc6f7)
		      ;; NOISE REDUCTION APPLIED
		      'ignored)
		     ((#xc6f8)
		      ;; PROFILE NAME
		      'ignored)
		     ((#xc761)
		      ;; NOISE PROFILE
		      'ignored)
		     ((#xc6f9 #xc6fa #xc6fb #xc6fc #xc6fd)
		      ;; HUE_SAT_MAP_DIMS...
		      'ignored)
		     ((#xc6fe)
		      ;; CAMERA PROFILE COPYRIGHT
		      'ignored)
		     ((#xa302)
		      ;; CFAPattern
		      'ignored)
		     ((#xc714 #xc715 #xc716 #xc717 #xc718 #xc719 #xc71a)
		      'ignored)
		     ((#xc71b)
		      ;; PREVIEW DATE TIME
		      'ignored)
		     ((#xc725  #xc726)
		      ;; LOOK_TABLE_DIMS
		      'ignored)
		     ((#xc7a7)
		      ;; NEW RAW IMAGE DIGEST
		      'ignored)
		     (else
		      ;; TAG_UNKNOWN
		      '(tprint "TAG_UNKNOWN: " (integer->string tag 16))
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
	  (warning "'read-jpeg-exif"
	     "Unknown exif endianess, assuminug big endian")
	  #f)))
   
   (if (and (char=? (string-ref bytes 4) #a000)
	    (char=? (string-ref bytes 5) #a000))
       (let* ((en (exif-endianess))
	      (hd (get16u en bytes 8)))
	  (if (not (=elong hd #e42))
	      (exif-error "read-jpeg-exif" "Illegal exif header" hd)
	      (let ((fo (elong->fixnum (get32u en bytes 10))))
		 (if (or (<fx fo 8) (>fx fo 16))
		     (exif-error "read-jpeg-exit"
			"Suspicious offset of first IFD value"
			fo)
		     (with-access::exif exif ((s thumbnail-offset)
					      (l thumbnail-length)
					      cdd-width
					      ewidth
					      focal-plane-xres
					      focal-plane-units
					      thumbnail)
			(process-exif-dir! en bytes (+fx 6 fo) 6 exif pos)
			;; CDD width
			(if (and (number? ewidth)
				 (number? focal-plane-xres)
				 (number? focal-plane-units))
			    (let ((w (/ (* ewidth focal-plane-units)
					focal-plane-xres)))
			       (set! cdd-width w)))
			;; thumbnail
			(if (and (integer? s) (integer? l))
			    (let ((th (make-string l)))
			       (blit-string! bytes s th 0 l)
			       (set! thumbnail th)
			       exif)
			    (set! thumbnail #f)))))))))

;*---------------------------------------------------------------------*/
;*    read-COM! ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-COM! exif bytes pos)
   (with-access::exif exif (comment %commentpos %commentlen)
      (let ((len (string-length bytes)))
	 (let loop ((i 0))
	    (cond
	       ((=fx i len)
		(set! comment bytes))
	       ((char=? (string-ref bytes i) #a000)
		(let ((s (make-string i)))
		   (blit-string! bytes 0 s 0 i)
		   (set! comment s)
		   (set! %commentpos pos)
		   (set! %commentlen i)))
	       (else
		(loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    read-SOFn! ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-SOFn! exif bytes encoding)
   (with-access::exif exif (width height jpeg-encoding)
      (set! width (elong->fixnum (get16u #t bytes 3)))
      (set! height (elong->fixnum (get16u #t bytes 1)))
      (set! jpeg-encoding encoding))
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
	      (exif-error "read-jpeg-section" "Too many padding bytes" a)
	      (loop (+ a 1) (mmap-get-char mm)))
	  ;; the section length
	  (let* ((lh (char->integer (mmap-get-char mm)))
		 (ll (char->integer (mmap-get-char mm)))
		 (l (fixnum->elong (bit-or (bit-lsh lh 8) ll))))
	     (cond
		((<elong l #e2)
		 (exif-error "read-jpeg-section" "Section too small" a))
		((>=elong (+elong l (mmap-read-position mm)) (mmap-length mm))
		 (exif-error "read-jpeg-section"
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
   (mmap-read-position-set! mm 0)
   (let ((m (read-jpeg-marker mm)))
      (if (not (eq? m 'M_SOI))
	  (exif-error "read-jpeg-sections" "Illegal section marker"
	     (-elong (mmap-read-position mm) 1))
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
;*    read-tiff-sections ...                                           */
;*    -------------------------------------------------------------    */
;*    https://www.awaresystems.be/imaging/tiff/specification/TIFF6.pdf */
;*---------------------------------------------------------------------*/
(define (read-tiff-sections exif mm::mmap path)

   (define (tiff-endianess)
      ;; big endian
      (char=? (mmap-ref mm 0) #a077))

   (define (strncpy o max)
      (let loop ((i 0))
	 (if (=fx i max)
	     (mmap-substring mm o (+fx o i))
	     (let ((c (mmap-ref mm (+fx i o))))
		(if (char=? c #a000)
		    (mmap-substring mm o (+fx i o))
		    (loop (+fx i 1)))))))

   (define (read-entry en offset)
      (let* ((tag (elong->fixnum (get16u en mm (+fx 0 offset))))
	     (typ (get16u en mm (+fx 2 offset)))
	     (cnt (get32u en mm (+fx 4 offset)))
	     (voff (get32u en mm (+fx 8 offset))))
	 (tprint "tag=" tag " " (fixnum->string tag 16)
	    " typ=" typ " cnt=" cnt " voff=" voff " "
	    (if (=fx typ 2)
		(strncpy voff cnt)))))
			       
   (define (read-ifd en offset)
      '(let loop ((dnum (elong->fixnum (get16u en mm (elong->fixnum offset))))
		 (offset (+fx offset 2)))
	 (if (>fx dnum 0)
	     (begin
		(read-entry en offset)
		(loop (-fx dnum 1)
		   (+fx offset 12)))
	     (begin
		(tprint "NEXT=" (get32u en mm offset))
		(tprint "OFFSET=" offset " " (integer->string offset 16)))))
      (process-exif-dir! en mm (elong->fixnum offset) 0 exif 0))
			       
   (let* ((en (tiff-endianess))
	  (offset (get32u en mm 4)))
      (read-ifd en offset)))
		
;*---------------------------------------------------------------------*/
;*    read-rw2-sections ...                                            */
;*    -------------------------------------------------------------    */
;*    https://www.awaresystems.be/imaging/tiff/specification/TIFF6.pdf */
;*---------------------------------------------------------------------*/
(define (read-rw2-sections exif mm::mmap path)

   (define (tiff-endianess)
      ;; big endian
      (char=? (mmap-ref mm 0) #a077))

   (define (strncpy o max)
      (let loop ((i 0))
	 (if (=fx i max)
	     (mmap-substring mm o (+fx o i))
	     (let ((c (mmap-ref mm (+fx i o))))
		(if (char=? c #a000)
		    (mmap-substring mm o (+fx i o))
		    (loop (+fx i 1)))))))

   (define (read-entry en offset)
      (let* ((tag (elong->fixnum (get16u en mm (+fx 0 offset))))
	     (typ (get16u en mm (+fx 2 offset)))
	     (cnt (get32u en mm (+fx 4 offset)))
	     (voff (get32u en mm (+fx 8 offset))))
	 (tprint "tag=" tag " 0x" (fixnum->string tag 16)
	    " typ=" typ " cnt=" cnt " voff=" voff " "
	    (if (=fx typ 2) (strncpy voff cnt)))))
			       
   (define (read-ifd en offset)
      (let loop ((dnum (elong->fixnum (get16u en mm (elong->fixnum offset))))
		 (offset (+fx offset 2)))
	 (if (>fx dnum 0)
	     (begin
		(read-entry en offset)
		(loop (-fx dnum 1)
		   (+fx offset 12)))
	     (begin
		(tprint "NEXT=" (get32u en mm offset))
		(tprint "OFFSET=" offset " " (integer->string offset 16)))))
      (process-exif-dir! en mm (elong->fixnum offset) 0 exif 0))

   (tprint "RW2 " mm)
   (let* ((en (tiff-endianess))
	  (offset (get32u en mm 4)))
      (read-ifd en offset)))
		
;*---------------------------------------------------------------------*/
;*    tiff? ...                                                        */
;*---------------------------------------------------------------------*/
(define (tiff? mm::mmap)
   ;; 32bit dng is denoted by the #a042 tag
   ;; 64bit dng is denoted by the #a043 tag, which is currently not supported
   (let ((res (case (mmap-ref mm 0)
		 ((#a073)
		  ;; #x49
		  (and (char=? (mmap-get-char mm) #a073)
		       ;; 32bit dng
		       (char=? (mmap-get-char mm) #a042) 
		       (char=? (mmap-get-char mm) #a000)))
		 ((#a077)
		  ;; #x4d
		  (and (char=? (mmap-get-char mm) #a077)
		       (char=? (mmap-get-char mm) #a000)
		       ;; 32bit dng
		       (char=? (mmap-get-char mm) #a042)))
		 (else
		  #f))))
      (unless res
	 (mmap-read-position-set! mm 0))
      res))
		
;*---------------------------------------------------------------------*/
;*    rw2? ...                                                         */
;*---------------------------------------------------------------------*/
(define (rw2? mm::mmap)
   (and (char=? (mmap-ref mm 0) #a073)
	(char=? (mmap-get-char mm) #a073)
	(char=? (mmap-get-char mm) #a085)
	(char=? (mmap-get-char mm) #a000)
	(memq (mmap-get-char mm) '(#a008 #a024))
	(char=? (mmap-get-char mm) #a000)
	(char=? (mmap-get-char mm) #a000)
	(char=? (mmap-get-char mm) #a000)))
		
;*---------------------------------------------------------------------*/
;*    jpeg-exif ...                                                    */
;*    -------------------------------------------------------------    */
;*    This is the main function which reads a JPEG image a             */
;*    returns an EXIF structure.                                       */
;*---------------------------------------------------------------------*/
(define (jpeg-exif path)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
	  "jpeg-exif" "Can't find file" path)
       (let ((mm (open-mmap path write: #f))
	     (exif (instantiate::exif)))
	  (unwind-protect
	     (when (> (mmap-length mm) 0)
		(cond
		   ((tiff? mm)
		    (read-tiff-sections exif mm path))
		   ((rw2? mm)
		    (read-tiff-sections exif mm path))
		   (else
		    (read-jpeg-sections exif mm path))))
	     (close-mmap mm))
	  exif)))
		
;*---------------------------------------------------------------------*/
;*    jpeg-exif-comment-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (jpeg-exif-comment-set! path comment)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
	  "jpeg-exif-comment-set!" "Can't find file" path)
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
	  "jpeg-exif-comment-set!" "Can't find file" path)
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
