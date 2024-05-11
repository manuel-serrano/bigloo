;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/multimedia/src/Llib/exif.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 29 05:30:36 2004                          */
;*    Last change :  Sat May 11 08:00:39 2024 (serrano)                */
;*    Copyright   :  2004-24 Manuel Serrano                            */
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

;; TIFF
;; ====
;; 
;; Specification
;;   http://partners.adobe.com/public/developer/en/tiff/TIFF6.pdf
;;   http://partners.adobe.com/public/developer/en/tiff/TIFFPM6.pdf
;;   http://partners.adobe.com/public/developer/en/tiff/TIFFphotoshop.pdf
;; Metadata tags
;;   http://www.digitizationguidelines.gov/guidelines/TIFF_Metadata_Final.pdf
;;   http://www.digitalpreservation.gov/formats/content/tiff_tags.shtml

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
	      (cdd-height (default #f))
	      (bits-per-sample (default #f))
	      (samples-per-pixel (default #f))
	      (focal-plane-xres (default #f))
	      (focal-plane-yres (default #f))
	      (focal-plane-units (default #f))
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
	      (maker-note (default #f))
	      (thumbnail (default #f))
	      (%thumbnail-path (default #f))
	      (%thumbnail-offset (default #f))
	      (%thumbnail-length (default #f))
	      (endianess (default #f))
	      (file-source (default "???"))
	      (%gps-tag (default #f))
	      (%gps-latitude-ref (default #f))
	      (gps-latitude (default #f))
	      (gps-longitude (default #f))
	      (%gps-longitude-ref (default #f))
	      (gps-altitude (default #f))
	      (%gps-altitude-ref (default #f))
	      (gps-satelites (default #f))
	      (gps-measure-mode (default #f))
	      (gps-status (default #f))
	      (gps-time-stamp (default #f))
	      (gps-date-stamp (default #f)))
	   
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
;*    getbuffer ...                                                    */
;*    -------------------------------------------------------------    */
;*    This is a plain alias to substring in order to let hop2js        */
;*    compiles this using a Buffer instead of a string.                */
;*---------------------------------------------------------------------*/
(define-inline (getbuffer bytes start end)
   (if (string? bytes)
       (substring bytes start end)
       (mmap-substring bytes start end)))

;*---------------------------------------------------------------------*/
;*    getascii ...                                                     */
;*---------------------------------------------------------------------*/
(define (getascii::bstring en::bool bytes o::int)
   
   (define (string-getascii bytes::bstring o)
      (let loop ((i o))
	 (if (char=? (string-ref bytes i) #a000)
	     (substring bytes o i)
	     (loop (+fx i 1)))))
   
   (define (mmap-getascii bytes::mmap o)
      (let loop ((i (fixnum->elong o)))
	 (if (char=? (mmap-ref bytes i) #a000)
	     (mmap-substring bytes (fixnum->elong o) i)
	     (loop (+elong i 1)))))

   (if (string? bytes)
       (string-getascii bytes o)
       (mmap-getascii bytes o)))
   
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
;*    Get a value (e.g., a number) according to the specified format.  */
;*---------------------------------------------------------------------*/
(define (getformat::obj en::bool bytes o::int fmt::int)
   (case fmt
      ((1 6 7) 
       ;; FMT_BYTE, FMT_SBYTE, UNKNOWN_BYTE
       (char->integer
	  (if (string? bytes)
	      (string-ref bytes o)
	      (mmap-ref bytes (fixnum->elong o)))))
      ((2)
       ;; ASCII
       (getascii en bytes o))
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
	  (cons num den)))
      ((11)
       ;; FMT_SINGLE 32bit
       (exif-error "exif" "Unsupported number format" fmt))
      ((12)
       ;; FMT_DOUBLE 64bit
       (exif-error "exif" "Unsupported number format" fmt))
      ((257)
       ;; PANASONIC bool
       (char->integer
	  (if (string? bytes)
	      (string-ref bytes o)
	      (mmap-ref bytes (fixnum->elong o)))))
      ((258)
       ;; PANASONIC 1|2|3
       (char->integer
	  (if (string? bytes)
	      (string-ref bytes o)
	      (mmap-ref bytes (fixnum->elong o)))))
      (else
       (tprint "unknown format " fmt)
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
;*    getformatRat3 ...                                                */
;*---------------------------------------------------------------------*/
(define (getformatRat3 en bytes valptr fmt)
   (let* ((sz (vector-ref *exif-formats-size* fmt))
	  (r0 (getformat en bytes valptr fmt))
	  (r1 (getformat en bytes (+fx valptr sz) fmt))
	  (r2 (getformat en bytes (+fx valptr (+fx sz sz)) fmt)))
      (+fl (/fl (elong->flonum (car r0)) (elong->flonum (cdr r0)))
	 (+fl (/fl (elong->flonum (car r1)) (*fl 60. (elong->flonum (cdr r1))))
	    (/fl (elong->flonum (car r2)) (*fl 3600. (elong->flonum (cdr r2))))))))
      
;*---------------------------------------------------------------------*/
;*    *exif-formats-size* ...                                          */
;*---------------------------------------------------------------------*/
(define *exif-formats-size*
   '#(_ 1 1 2 4 8 1 1 2 4 8 4 8))

;*---------------------------------------------------------------------*/
;*    read-jpeg-marker ...                                             */
;*    -------------------------------------------------------------    */
;*    Returns the marker (a char) or #f on failure.                    */
;*---------------------------------------------------------------------*/
(define (read-jpeg-marker p)
   (if (not (char=? (mmap-get-char p) #a255))
       #f
       (char->integer (mmap-get-char p))))

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
(define (process-exif-dir! en::bool bytes start::int base::int exif o0 tag-base::long read-next::bool extension)

   (define (strncpy o max)

      (define (string-strncpy o max)
	 (let loop ((i 0))
	    (if (=fx i max)
		(substring bytes o (+fx o i))
		(let ((c (string-ref bytes (+fx i o))))
		   (if (char=? c #a000)
		       (substring bytes o (+fx o i))
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

   (define (strncpy2 o max)

      (define (string-strncpy o max)
	 (let loop ((i 0))
	    (if (=fx i max)
		(substring bytes o (+fx o i))
		(let ((c (string-ref bytes (+fx i o))))
		   (if (char=? c #a000)
		       (substring bytes o (+fx o i))
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

   (let ((dnum (elong->fixnum (get16u en bytes start))))
      (let loop ((de 0))
	 (when (<fx de dnum)
	    (let* ((da (+ start 2 (*fx 12 de)))
		   (tag (elong->fixnum (get16u en bytes da)))
		   (fmt (elong->fixnum (get16u en bytes (+fx 2 da))))
		   (cmp (get32u en bytes (+fx 4 da))))
	       (let* ((bcount (*fx (elong->fixnum cmp)
				 (if (>elong fmt (vector-length *exif-formats-size*))
				     1
				     (vector-ref *exif-formats-size*
					(elong->fixnum fmt)))))
		      (valptr (if (>fx bcount 4)
				  (let ((ov (get32u en bytes (+fx 8 da))))
				     (+fx base (elong->fixnum ov)))
				  (+fx 8 da))))
		  ;; the tag-base is used to avoid tag confusion between
		  ;; various extensions (e.g. GPS, see below)
		  '(tprint "TAG=" (integer->string tag 16) " " de "/" dnum
		     " " valptr " -> " (getformat en bytes valptr fmt))
		  (case (+fx tag tag-base)
		     ((#x1)
		      ;; INTEROPINDEX
		      (let ((o (strncpy valptr bcount)))
			 #unspecified))
		     ((#x10001)
		      ;; TAG_GPS_LATITUDE_REF
		      (with-access::exif exif (%gps-latitude-ref)
			 (set! %gps-latitude-ref (strncpy valptr bcount))))
		     ((#x10002)
		      ;; TAG_GPS_LATITUDE
		      (with-access::exif exif (gps-latitude)
			 (set! gps-latitude (getformatRat3 en bytes valptr fmt))))
		     ((#x10003)
		      ;; TAG_GPS_LONGITUDE_REF
		      (with-access::exif exif (%gps-longitude-ref)
			 (set! %gps-longitude-ref (strncpy valptr bcount))))
		     ((#x10004)
		      ;; TAG_GPS_LONGITUDE
		      (with-access::exif exif (gps-longitude)
			 (set! gps-longitude (getformatRat3 en bytes valptr fmt))))
		     ((#x10005)
		      ;; TAG_GPS_ALTITUDE_REF
		      (with-access::exif exif (%gps-altitude-ref)
			 (set! %gps-altitude-ref (getformat/fx en bytes valptr fmt))))
		     ((#x10006)
		      ;; TAG_GPS_ALTITUDE
		      (with-access::exif exif (gps-altitude)
			 (set! gps-altitude (getformat en bytes valptr fmt))))
		     ((#x10007)
		      ;; TAG_GPS_TIMESPTAMP
		      (with-access::exif exif (gps-time-stamp)
			 (set! gps-time-stamp (getformatRat3 en bytes valptr fmt))))
		     ((#x10008)
		      ;; TAG_GPS_SATELITE
		      (with-access::exif exif (gps-satelites)
			 (set! gps-satelites (strncpy valptr bcount))))
		     ((#x10009)
		      ;; TAG_GPS_STATUS
		      (with-access::exif exif (gps-status)
			 (set! gps-status (strncpy valptr bcount))))
		     ((#x1000a)
		      ;; TAG_GPS_MEASURE_EMODE
		      (with-access::exif exif (gps-measure-mode)
			 (set! gps-measure-mode (strncpy valptr bcount))))
		     ((#x1001b #x1001c)
		      ;; TAG_GPS_SPEED_REF
		      #unspecified)
		     ((#x1001d)
		      ;; TAG_GPS_DATE_STAMP
		      (with-access::exif exif (gps-date-stamp)
			 (set! gps-date-stamp (strncpy valptr bcount))))
		     ((#x1000b)
		      ;; TAG_GPS_DOP
		      #unspecified)
		     ((#x1000c)
		      ;; TAG_GPS_SPEED_REF
		      #unspecified)
		     ((#x1000d)
		      ;; TAG_GPS_SPEED
		      #unspecified)
		     ((#x1000e)
		      ;; TAG_GPS_TRACK_REF
		      #unspecified)
		     ((#x1000f)
		      ;; TAG_GPS_TRACK
		      #unspecified)
		     ((#x10010)
		      ;; TAG_GPS_IMG_DIRECTION_REF
		      #unspecified)
		     ((#x10011)
		      ;; TAG_GPS_IMG_DIRECTION
		      #unspecified)
		     ((#xfe)
		      ;; NEW SUBFILE TYPE
		      (let ((s (getformat/fx en bytes valptr fmt)))
			 '(tprint "NEW_SUBFILE TYPE=" s)
			 (with-access::exif exif (new-subfile-type)
			    (set! new-subfile-type s))))
		     ((#xff)
		      ;; SUBFILE TYPE
		      (let ((s (getformat/fx en bytes valptr fmt)))
			 '(tprint "SUBFILE TYPE=" s)))
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
		      (let ((bps (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (bits-per-sample)
			    (set! bits-per-sample bps))))
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
		      '(tprint "STRIP_OFFSET=" (getformat en bytes valptr fmt)))
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
		      ;; SAMPLES_PER_PIXEL
		      (let ((spp (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (samples-per-pixel)
			    (set! samples-per-pixel spp))))
		     ((#x116)
		      ;; ROWS_PER_STRIP
		      '(tprint "ROWS_PER_STRIP" (getformat en bytes valptr fmt)))
		     ((#x117)
		      ;; STRIP_BYTE_COUNT
		      '(tprint "STRIP_BYTE_COUNT" (getformat en bytes valptr fmt)))
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
		      ;; https://web.archive.org/web/20060114005938/http://partners.adobe.com/public/developer/en/tiff/TIFFPM6.pdf
		      'ignored)
		     ((#x15a)
		      ;; TIFF_INDEXED_IMAGE
		      'ignored)
		     ((#x201)
		      ;; TAG_THUMBNAIL_OFFSET
		      (let* ((ol (getformat/fx en bytes valptr fmt))
			     (of (+fx ol base)))
			 (with-access::exif exif (%thumbnail-offset)
			    (set! %thumbnail-offset of))))
		     ((#x202)
		      ;; TAG_THUMBNAIL_LENGTH
		      (let ((le (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (%thumbnail-length)
			    (set! %thumbnail-length le))))
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
			 (process-exif-dir! en bytes ss base exif o0 0 #t extension)))
		     ((#x8822)
		      ;; EXPOSURE_PROGRAM
		      (let ((e (getformat en bytes valptr fmt)))
			 (with-access::exif exif (exposure-program)
			    (set! exposure-program e))))
		     ((#x8825)
		      ;; GPS_TAG
		      (let ((t (getformat/fx en bytes valptr fmt)))
			 (with-access::exif exif (%gps-tag)
			    (set! %gps-tag t))))
		     ((#x8827)
		      ;; ISO
		      (let ((is (getformat en bytes valptr fmt)))
			 (with-access::exif exif (iso)
			    (set! iso is))))
		     ((#x9000)
		      ;; TAG_EXIF_VERSION
		      (with-access::exif exif (version)
			 (set! version (strncpy2 valptr bcount))))
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
			 (let ((buf (if (string? bytes)
					(substring bytes valptr (+fx valptr bcount))
					(mmap-substring bytes valptr (+fx valptr bcount)))))
			    (set! maker-note (strncpy valptr bcount))
			    (read-maker-note en buf exif))))
		     ((#x9290)
		      ;; SUB_SEC_TIME (ascii)
		      '(tprint "SUB_SEC_TIME " (strncpy valptr bcount) " " bcount)
		      #unspecified)
		     ((#x9291)
		      ;; SUB_SEC_TIME_ORIGINAL (ascii)
		      '(tprint "SUB_SEC_TIME_ORIGINAL " (strncpy valptr bcount)
			" " bcount)
		      #unspecified)
		     ((#x9292)
		      ;; SUB_SEC_TIME_DIGITIZED (ascii)
		      '(tprint "SUB_SEC_TIME_DIGITIZED " (strncpy valptr bcount))
		      #unspecified)
		     ((#xa000)
		      ;; TAG_FLASH_PIX_VERSION
		      '(tprint "FLASH_PIX_VERSION " (getformat en bytes valtr fmt))
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
		      (with-access::exif exif (file-source)
			 (set! file-source
			    (case (getformat en bytes valptr fmt)
			       ((1) "Film Scanner")
			       ((2) "Reflection Print Scanner")
			       ((3) "Digital Camera")
			       (else "unknown")))))
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
		      (extension exif tag en bytes fmt bcount valptr)))
		  (loop (+fx de 1))))))
      (when (and read-next
		 (< (+ start 2 4 (*fx 12 dnum)) (if (string? bytes) (string-length bytes) (mmap-length bytes))))
	 (let ((of (get32u en bytes (+ start 2 (*fx 12 dnum)))))
	    (when (>elong of #e0)
		(process-exif-dir! en bytes
		   (+fx (elong->fixnum of) base)
		   base exif o0 0 read-next extension))))))

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
	     "Unknown exif endianess, assuming big endian")
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
		     (with-access::exif exif ((s %thumbnail-offset)
					      (l %thumbnail-length)
					      thumbnail
					      cdd-width
					      cdd-height
					      ewidth
					      eheight
					      focal-plane-xres
					      focal-plane-units
					      endianess)
			(set! endianess en)
			(process-exif-dir! en bytes (+fx 6 fo) 6 exif pos 0 #t
			   (lambda (exif tag en bytes fmt bcount valptr)
			      '(tprint "jpeg-extension tag=" tag " " valptr)
			      'unknown))
			;; CDD width
			(when (and (number? ewidth)
				   (number? eheight)
				   (number? focal-plane-xres)
				   (number? focal-plane-units))
			   (let ((w (/ (* ewidth focal-plane-units)
				       focal-plane-xres)))
			      (set! cdd-width w))
			   (let ((h (/ (* eheight focal-plane-units)
				       focal-plane-xres)))
			      (set! cdd-height h)))
			;; thumbnail
			(unless thumbnail
			   (when (and (integer? s) (integer? l))
			      (let ((th (getbuffer bytes s (+fx s l))))
				 (set! thumbnail th))))
			exif)))))))

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
		(let ((s (substring bytes 0 i)))
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
      (unless width (set! width (elong->fixnum (get16u #t bytes 3))))
      (unless height (set! height (elong->fixnum (get16u #t bytes 1))))
      (set! jpeg-encoding encoding))
   exif)

;*---------------------------------------------------------------------*/
;*    read-jpeg-section ...                                            */
;*---------------------------------------------------------------------*/
(define (read-jpeg-section mm::mmap)
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
		 (let ((offset (mmap-read-position mm)))
		    (values (char->integer m)
		       (mmap-get-string mm (-elong l #e2))
		       (elong->fixnum offset)))))))))

;*---------------------------------------------------------------------*/
;*    read-jpeg-sections ...                                           */
;*---------------------------------------------------------------------*/
(define (read-jpeg-sections exif mm::mmap)
   (let ((m (read-jpeg-marker mm)))
      (if (not (eq? m #xd8)) ;; M_SOI
	  (exif-error "read-jpeg-sections" "Illegal section marker"
	     (-elong (mmap-read-position mm) 1))
	  (let loop ()
	     (multiple-value-bind (m bytes offset)
		(read-jpeg-section mm)
		(case m
		   ((#xda) ;; M_SOS
		    'sos)
		   ((#xd9) ;; M_EOI
		    'eoi)
		   ((#xfe) ;; M_COM
		    (read-COM! exif
		       bytes
		       (- (mmap-read-position mm)
			  (string-length bytes)))
		    (loop))
		   ((#xe0) ;; MJFIF
		    ;; not implemented
		    (loop))
		   ((#xe1) ;; M_EXIF
		    (when (substring=? bytes "Exif\000\000" 6)
		       (with-access::exif exif (%gps-tag endianess)
			  (read-jpeg-exif! exif
			     bytes
			     (- (mmap-read-position mm)
				(string-length bytes)))
			  ;; gps
			  (when %gps-tag
			     (process-exif-dir! endianess bytes
				(+fx %gps-tag 6) 6 exif 0 #x10000 #t
				(lambda (exif tag en bytes fmt bcount valptr)
				   'unknown))
			     (with-access::exif exif (gps-altitude
							gps-latitude
							gps-longitude
							%gps-altitude-ref
							%gps-latitude-ref
							%gps-longitude-ref)
				;; patch the relative values
				(when (and (integer? %gps-altitude-ref)
					   (=fx %gps-altitude-ref 1))
				   (set-car! gps-altitude
				      (- (car gps-altitude))))
				(when (equal? %gps-latitude-ref "S")
				   (set! gps-latitude (negfl gps-latitude)))
				(when (equal? %gps-longitude-ref "W")
				   (set! gps-longitude (negfl gps-longitude)))))))
		    (loop))
		   ((#xc0) ;; M_SOFO
		    (read-SOFn! exif bytes "baseline")
		    (loop))
		   ((#xc1) ;; M_SOF1
		    (read-SOFn! exif bytes "extended sequential")
		    (loop))
		   ((#xc2) ;; M_SOF2
		    (read-SOFn! exif bytes "progressive")
		    (loop))
		   ((#xc3) ;; M_SOF3
		    (read-SOFn! exif bytes "lossless")
		    (loop))
		   ((#xc5) ;; M_SOF5
		    (read-SOFn! exif bytes "differential sequential")
		    (loop))
		   ((#xc6) ;; M_SOF6
		    (read-SOFn! exif bytes "differential progressive")
		    (loop))
		   ((#xc7) ;; M_SOF7
		    (read-SOFn! exif bytes "differential lossless")
		    (loop))
		   ((#xc9) ;; M_SOF9
		    (read-SOFn! exif bytes "extended sequential, arithmetic coding")
		    (loop))
		   ((#xca) ;; M_SOFA
		    (read-SOFn! exif bytes "progressive, arithmetic coding")
		    (loop))
		   ((#xcb) ;; M_SOFB
		    (read-SOFn! exif bytes "lossless, arithmetic coding")
		    (loop))
		   ((#xcc) ;; M_SOFC
		    (read-SOFn! exif bytes "differential sequential, arithmetic coding")
		    (loop))
		   ((#xcd) ;; M_SOFD
		    (read-SOFn! exif bytes "differential progressive, arithmetic coding")
		    (loop))
		   ((#xce) ;; M_SOFE
		    (read-SOFn! exif bytes "differential lossless, arithmetic coding")
		    (loop))
		   ((#xcf) ;; M_SOFF
		    (read-SOFn! exif bytes "?")
		    (loop))
		   (else
		    (loop)))))))
   exif)

;*---------------------------------------------------------------------*/
;*    read-maker-note ...                                              */
;*---------------------------------------------------------------------*/
(define (read-maker-note en bytes::bstring exif::exif)
   (cond
      ((string-prefix? "Panasonic" bytes)
       (process-exif-dir! en bytes 12 0 exif 0 20000 #f
	  (lambda (exif tag en bytes fmt bcount valptr)
	     (case tag
		((#x004b)
		 (with-access::exif exif (width)
		    (set! width (getformat en bytes valptr fmt))))
		((#x004c)
		 (with-access::exif exif (height)
		    (set! height (getformat en bytes valptr fmt))))
		((#x00d1)
		 '(tprint "ISO=" (getformat en bytes valptr fmt)))
		(else
		 '(tprint "Panasonic extension " (integer->string tag 16) " valptr=" valptr
		   " fmt=" fmt " bcount=" bcount
		   " -> " (getformat en bytes valptr fmt)))))))))

;*---------------------------------------------------------------------*/
;*    read-tiff-sections ...                                           */
;*    -------------------------------------------------------------    */
;*    https://www.awaresystems.be/imaging/tiff/specification/TIFF6.pdf */
;*---------------------------------------------------------------------*/
(define (read-tiff-sections exif::exif mm::mmap)

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

   (define (read-ifd en offset)
      (process-exif-dir! en mm (elong->fixnum offset) 0 exif 0 0 #t
	 (lambda (exif tag en bytes fmt bcount valptr)
	    (tprint "read-ifd extension" tag " " valptr)
	    'unknown)))
			       
   (let* ((en (tiff-endianess))
	  (offset (get32u en mm 4)))
      (read-ifd en offset)))

;*---------------------------------------------------------------------*/
;*    read-rw2-sections ...                                            */
;*    -------------------------------------------------------------    */
;*    https://www.exiftool.org/TagNames/PanasonicRaw.html              */
;*---------------------------------------------------------------------*/
(define (read-rw2-sections exif mm::mmap)
   
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
   
   (define camera-ifd 0)
   
   (define (extension exif tag en bytes fmt bcount valptr)
      (case tag
	 ((#x0001)
	  '(tprint "panasonicrawversion " (getformat en bytes valptr fmt)) 
	  #unspecified)
	 ((#x0002)
	  (let ((w (getformat en bytes valptr fmt)))
	     (with-access::exif exif (cdd-width)
		(set! cdd-width w))))
	 ((#x0003)
	  (let ((h (getformat en bytes valptr fmt)))
	     (with-access::exif exif (cdd-height)
		(set! cdd-height h))))
	 ((#x0004 #x0005 #x0006 #x0007)
	  '(tprint "sensorborder " tag " " (getformat en bytes valptr fmt)))
	 ((#x0008)
	  (let ((spp (getformat/fx en bytes valptr fmt)))
	     (with-access::exif exif (samples-per-pixel)
		(set! samples-per-pixel spp))))
	 ((#x0009)
	  '(tprint "cfapattern"))
	 ((#x000a)
	  (let ((bps (getformat en bytes valptr fmt)))
	     (with-access::exif exif (bits-per-sample)
		(set! bits-per-sample bps))))
	 ((#x000b)
	  (let ((c (getformat en bytes valptr fmt)))
	     (with-access::exif exif (jpeg-compress)
		(set! jpeg-compress c))))
	 ((#x000e #x000f #x0010 #x0011 #x0012)
	  '(tprint "red-green-blue " tag " " (getformat en bytes valptr fmt)))
	 ((#x0013)
	  ;; WBInfo (see WBInfow2)
	  '(tprint "WBInfo"))
	 ((#x0017)
	  (let ((is (getformat/fx en bytes valptr fmt)))
	     (with-access::exif exif (iso)
		(set! iso is))))
	 ((#x0018 #x0019 #x001a)
	  '(tprint "iso multipliers " tag " " (getformat en bytes valptr fmt)))
	 ((#x001b)
	  '(tprint "noise reduction"))
	 ((#x001c #x001d #x001e #x0024 #x0025 #x0026)
	  '(tprint "black levels " tag " " (getformat en bytes valptr fmt)))
	 ((#x0027)
	  ;; WBINFO2 (the data is directly encoded into in the buffer at vallptr
	  ;; (let* ((is (getformat/fx en bytes valptr fmt))
          ;;		 (buf (if (string? bytes)
          ;;			  (substring bytes valptr (+fx valptr bcount))
          ;;			  (mmap-substring bytes valptr (+fx valptr bcount)))))
          ;;    (tprint (map (lambda (c) (integer->string (char->integer c) 16)) (string->list buf)))))
	  #unspecified)
	 ((#x002d)
	  '(tprint "raw format " (getformat en bytes valptr fmt)))
	 ((#x002e)
	  (with-access::exif exif (%thumbnail-offset %thumbnail-length thumbnail)
	     (set! %thumbnail-offset valptr)
	     (set! %thumbnail-length bcount)
	     (set! thumbnail (getbuffer bytes valptr (+fx valptr bcount)))
	     (read-jpeg-sections exif (string->mmap thumbnail))))
	 ((#x002f #x0030 #x0031 #x0032)
	  '(tprint "crop " tag " " (getformat en bytes valptr fmt)))
	 ((#x004b)
	  (tprint "Panasonic image width " (getformat en bytes valptr fmt)))
	 ((#x004c)
	  (tprint "Panasonic image height " (getformat en bytes valptr fmt)))
	 ((#x0116)
	  '(tprint "RowsPerStrip " (getformat en bytes valptr fmt)))
	 ((#x0117)
	  (tprint "StripByteCounts " (getformat en bytes valptr fmt)))
	 ((#x0118)
	  '(tprint "RawDataOffset " (getformat en bytes valptr fmt)))
	 ((#x0119)
	  '(tprint "DistortionInof"))
	 ((#x0120)
	  (set! camera-ifd valptr))
	 ((#x0121)
	  '(tprint "multishot"))
	 ((#x0127)
	  '(tprint "jpegfromraw2"))
	 ((#x013b)
	  (with-access::exif exif (artist)
	     (set! artist (strncpy valptr bcount))))
	 ((#x02bc)
	  '(tprint "application nnodes"))
	 ((#x83bb)
	  '(tprint "iptc-naa"))
	 (else
	  (let ((val (getformat en bytes valptr fmt)))
	     '(tprint "read-rw2-extension " (integer->string tag 16)
	       " bcount=" bcount " valptr=" valptr " val=" val)
	     'unknown))))
   
   (define (camera-ifd-extension exif tag en bytes fmt bcount valptr)
      (case tag
	 ((#x1100)
	  '(tprint "FocusStepNear"))
	 ((#x1101)
	  '(tprint "FocusStepCount"))
	 ((#x1102)
	  '(tprint "FlashFired " (getformat en bytes valptr fmt)))
	 ((#x1105)
	  '(tprint "ZoomPosition " (getformat en bytes valptr fmt)))
	 ((#x1200)
	  '(tprint "LensAttached " (getformat en bytes valptr fmt)))
	 ((#x1201)
	  (with-access::exif exif (lens-make)
	     (set! lens-make (strncpy valptr bcount))))
	 ((#x1202)
	  (with-access::exif exif (lens-model)
	     (set! lens-model (strncpy valptr bcount))))
	 ((#x1203)
	  (with-access::exif exif (focal-length35)
	     (set! focal-length35 (getformat en bytes valptr fmt))))
	 ((#x1301)
	  (let ((v (getformat en bytes valptr fmt)))
	     (with-access::exif exif (aperture)
		(set! aperture (expt 2 (/ v 512))))))
	 ((#x1302)
	  (let ((v (getformat en bytes valptr fmt)))
	     (with-access::exif exif (shutter-speed-value)
		(set! shutter-speed-value
		   (if (< (/ v 256) 100)
		       (expt 2 (/ (- v) 256))
		       0)))))
	 ((#x1303)
	  '(tprint "SensitivityValue "
	    (/ (getformat en bytes valptr fmt) 256)))
	 ((#x1305)
	  '(tprint "HighIsoMode"))
	 ((#x1412)
	  '(tprint "FacesDetected "
	    (getformat en bytes valptr fmt)))
	 ((#x3200 3201 3202)
	  '(tprint "WC_CFA" tag " "
	    (getformat en bytes valptr fmt)))
	 ((#x3300)
	  '(tprint "WhiteBalanceSet"))
	 ((#x3420 #x3421)
	  '(tprint "WB_RED/BLUE" tag " "
	    (getformat en bytes valptr fmt)))
	 ((#x3501)
	  (let ((o (getformat en bytes valptr fmt)))
	     (with-access::exif exif (orientation)
		(set! orientation
		   (case o
		      ((#e1 #e2) 'landscape)
		      ((#e3) 'upsidedown)
		      ((#e5 #e6 #e7 #e8) 'portrait)
		      (else 'seascape))))))
	 ((#x3600)
	  '(tprint "WhiteBalanceDetected"))
	 (else
	  '(tprint "CAMERA-IFD unsupported tag"
	    (integer->string tag 16)))))
   
   (define (read-ifd en offset)
      (process-exif-dir! en mm (elong->fixnum offset) 0 exif 0 0 #t
	 extension))
   
   (let* ((en (tiff-endianess))
	  (offset (get32u en mm 4)))
      (read-ifd en offset)
      (when (>fx camera-ifd 0)
	 (let* ((offset (elong->fixnum (get32u en mm (+fx camera-ifd 4))))
		(start (+fx camera-ifd offset)))
	    (process-exif-dir! en mm start 0 exif 0 0 #f
	       camera-ifd-extension)))
      exif))
		
;*---------------------------------------------------------------------*/
;*    read-raf-sections ...                                            */
;*    -------------------------------------------------------------    */
;*    https://libopenraw.freedesktop.org/formats/raf/                  */
;*---------------------------------------------------------------------*/
(define (read-raf-sections exif mm::mmap)
   (let* ((pos (mmap-read-position mm))
	  (fmtv (mmap-substring mm pos (+elong pos 4)))
	  (id (mmap-substring mm (+elong pos 4) (+elong pos 12))))
      ; skip 8 bytes
      (set! pos (mmap-read-position mm))
      ;; read the camera string
      (let* ((buf (mmap-substring mm pos (+elong pos 32)))
	     (i (string-index buf #a000))
	     (model (substring buf 0 i)))
	 (with-access::exif exif (make unique-camera-model camera-serial-number)
	    (set! make "FUJIFILM")
	    (set! unique-camera-model model)
	    (set! camera-serial-number id)
	    (let* ((version (mmap-get-string mm 4))
		   (unknown (mmap-get-string mm 20))
		   (jpegoff (mmap-get-string mm 4))
		   (metaoff (mmap-get-string mm 4))
		   (metalen (mmap-get-string mm 4))
		   (cfaoff (mmap-get-string mm 4))
		   (cfalen (mmap-get-string mm 4))
		   (unknownlen (mmap-get-string mm 12))
		   (someoff (mmap-get-string mm 4)))
	       (mmap-read-position-set! mm (+elong (mmap-read-position mm) 28))
	       (read-jpeg-sections exif mm))))))
		
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
   (let ((res (and (char=? (mmap-ref mm 0) #a073)
		   (char=? (mmap-get-char mm) #a073)
		   (char=? (mmap-get-char mm) #a085)
		   (char=? (mmap-get-char mm) #a000)
		   (memq (mmap-get-char mm) '(#a008 #a024))
		   (char=? (mmap-get-char mm) #a000)
		   (char=? (mmap-get-char mm) #a000)
		   (char=? (mmap-get-char mm) #a000))))
      (unless res
	 (mmap-read-position-set! mm 0))
      res))

;*---------------------------------------------------------------------*/
;*    raf? ...                                                         */
;*    -------------------------------------------------------------    */
;*    Fujifilm Raw Format                                              */
;*      https://libopenraw.freedesktop.org/formats/raf/                */
;*---------------------------------------------------------------------*/
(define (raf? mm::mmap)
   (let* ((sig "FUJIFILMCCD-RAW ")
	  (mark (mmap-substring mm 0 (string-length sig)))
	  (res (string=? mark sig)))
      (unless res
	 (mmap-read-position-set! mm 0))
      res))

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
	     (exf (instantiate::exif)))
	  (unwind-protect
	     (when (> (mmap-length mm) 64)
		(cond
		   ((tiff? mm)
		    (read-tiff-sections exf mm))
		   ((rw2? mm)
		    (read-rw2-sections exf mm))
		   ((raf? mm)
		    (read-raf-sections exf mm))
		   (else
		    (mmap-read-position-set! mm 0)
		    (read-jpeg-sections exf mm))))
	     (close-mmap mm))
	  exf)))
		
;*---------------------------------------------------------------------*/
;*    jpeg-exif-comment-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (jpeg-exif-comment-set! path comment)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
	  "jpeg-exif-comment-set!" "Can't find file" path)
       (let ((mm (open-mmap path))
	     (exf (instantiate::exif))
	     (mtime #f))
	  (unwind-protect
	     (when (> (mmap-length mm) 0)
		(read-jpeg-sections exf mm)
		(with-access::exif exf (%commentpos %commentlen)
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
	     (exf (instantiate::exif))
	     (mtime #f))
	  (unwind-protect
	     (when (> (mmap-length mm) 0)
		(read-jpeg-sections exf mm)
		(with-access::exif exf (%orientationpos)
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
   
   (define (substring->int d i l)
      (let ((len (+fx i l))
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
