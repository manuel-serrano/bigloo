;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/id3.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 10 16:21:17 2005                          */
;*    Last change :  Sat Jan  3 07:39:11 2009 (serrano)                */
;*    Copyright   :  2005-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MP3 ID3 tags                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-id3

   (export (class id3
	      version::bstring
	      (title::bstring read-only)
	      (artist::bstring read-only)
	      (orchestra::obj read-only (default #f))
	      (conductor::obj read-only (default #f))
	      (interpret::obj read-only (default #f))
	      (album::bstring read-only)
	      (year::int read-only)
	      (recording read-only (default #f))
	      (comment::bstring read-only)
	      (genre::bstring read-only)
	      (track::int (default 0))
	      (cd::obj (default #f)))

	   (mp3-id3 ::bstring)))

;*---------------------------------------------------------------------*/
;*    *id3v2-genres* ...                                               */
;*---------------------------------------------------------------------*/
(define *id3v2-genres*
   '#("Blues"
      "Classic Rock"
      "Country"
      "Dance"
      "Disco"
      "Funk"
      "Grunge"
      "Hip-Hop"
      "Jazz"
      "Metal"
      "New Age"
      "Oldies"
      "Other"
      "Pop"
      "R&B"
      "Rap"
      "Reggae"
      "Rock"
      "Techno"
      "Industrial"
      "Alternative"
      "Ska"
      "Death Metal"
      "Pranks"
      "Soundtrack"
      "Euro-Techno"
      "Ambient"
      "Trip-Hop"
      "Vocal"
      "Jazz+Funk"
      "Fusion"
      "Trance"
      "Classical"
      "Instrumental"
      "Acid"
      "House"
      "Game"
      "Sound Clip"
      "Gospel"
      "Noise"
      "AlternRock"
      "Bass"
      "Soul"
      "Punk"
      "Space"
      "Meditative"
      "Instrumental Pop"
      "Instrumental Rock"
      "Ethnic"
      "Gothic"
      "Darkwave"
      "Techno-Industrial"
      "Electronic"
      "Pop-Folk"
      "Eurodance"
      "Dream"
      "Southern Rock"
      "Comedy"
      "Cult"
      "Gangsta"
      "Top 40"
      "Christian Rap"
      "Pop/Funk"
      "Jungle"
      "Native American"
      "Cabaret"
      "New Wave"
      "Psychadelic"
      "Rave"
      "Showtunes"
      "Trailer"
      "Lo-Fi"
      "Tribal"
      "Acid Punk"
      "Acid Jazz"
      "Polka"
      "Retro"
      "Musical"
      "Rock & Roll"
      "Hard Rock"
      "Folk"
      "Folk-Rock"
      "National Folk"
      "Swing"
      "Fast Fusion"
      "Bebob"
      "Latin"
      "Revival"
      "Celtic"
      "Bluegrass"
      "Avantgarde"
      "Gothic Rock"
      "Progressive Rock"
      "Psychedelic Rock"
      "Symphonic Rock"
      "Slow Rock"
      "Big Band"
      "Chorus"
      "Easy Listening"
      "Acoustic"
      "Humour"
      "Speech"
      "Chanson"
      "Opera"
      "Chamber Music"
      "Sonata"
      "Symphony"
      "Booty Bass"
      "Primus"
      "Porn Groove"
      "Satire"
      "Slow Jam"
      "Club"
      "Tango"
      "Samba"
      "Folklore"
      "Ballad"
      "Power Ballad"
      "Rhythmic Soul"
      "Freestyle"
      "Duet"
      "Punk Rock"
      "Drum Solo"
      "Acapella"
      "Euro-House"
      "Dance Hall"))

;*---------------------------------------------------------------------*/
;*    string-cut! ...                                                  */
;*---------------------------------------------------------------------*/
(define (string-cut! s)
   (let ((i (string-index s #a000)))
      (cond
	 ((not i)
	  s)
	 ((=fx i 0)
	  "")
	 (else
	  (string-shrink! s i)))))

;*---------------------------------------------------------------------*/
;*    mmap-substring/len ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (mmap-substring/len mm start len)
   (mmap-substring mm start (+elong start len)))

;*---------------------------------------------------------------------*/
;*    id3v1? ...                                                       */
;*---------------------------------------------------------------------*/
(define (id3v1? mm)
   (and (>elong (mmap-length mm) 128)
	(string=? (mmap-substring/len mm (-elong (mmap-length mm) 128) 3)
		  "TAG")))

;*---------------------------------------------------------------------*/
;*    id3v1.1? ...                                                     */
;*---------------------------------------------------------------------*/
(define (id3v1.1? mm)
   (and (id3v1? mm)
	(char=? (mmap-ref mm (-elong (mmap-length mm) #e3)) #a000)
	(not (char=? (mmap-ref mm (-elong (mmap-length mm) #e2)) #a000))))

;*---------------------------------------------------------------------*/
;*    mp3-id3v1 ...                                                    */
;*---------------------------------------------------------------------*/
(define (mp3-id3v1 mm)
   (instantiate::id3
      (version "id3v1")
      (title (string-cut! (mmap-substring/len mm (-elong (mmap-length mm) #e125) 30)))
      (artist (string-cut! (mmap-substring/len mm (-elong (mmap-length mm) #e95) 30)))
      (album (string-cut! (mmap-substring/len mm (-elong (mmap-length mm) #e65) 30)))
      (year (string->integer (mmap-substring/len mm (-elong (mmap-length mm) #e35) 4)))
      (comment (string-cut! (mmap-substring/len mm (-elong (mmap-length mm) #e31) 4)))
      (genre (case (mmap-ref mm (-elong (mmap-length mm) #e1))
		(else "unknown")))))

;*---------------------------------------------------------------------*/
;*    mp3-id3v1 ...                                                    */
;*---------------------------------------------------------------------*/
(define (mp3-id3v1.1 mm)
   (let ((id (mp3-id3v1 mm)))
      (with-access::id3 id (version track)
	 (set! version "id3v1.1")
	 (set! track (char->integer (mmap-ref mm (-elong (mmap-length mm) #e2))))
	 id)))

;*---------------------------------------------------------------------*/
;*    id3v2.2? ...                                                     */
;*---------------------------------------------------------------------*/
(define (id3v2.2? mm)
   (and (>elong (mmap-length mm) #e3)
	(string=? (mmap-substring/len mm #e0 #e5) "ID3\002\000")))

;*---------------------------------------------------------------------*/
;*    id3v2.3? ...                                                     */
;*---------------------------------------------------------------------*/
(define (id3v2.3? mm)
   (and (>elong (mmap-length mm) 3)
	(string=? (mmap-substring/len mm #e0 #e5) "ID3\003\000")))

;*---------------------------------------------------------------------*/
;*    id3v2-size ...                                                   */
;*---------------------------------------------------------------------*/
(define (id3v2-size mm o)
   (let ((n0 (char->integer (mmap-ref mm o)))
	 (n1 (char->integer (mmap-ref mm (+ 1 o))))
	 (n2 (char->integer (mmap-ref mm (+ 2 o))))
	 (n3 (char->integer (mmap-ref mm (+ 3 o)))))
      (fixnum->elong
       (+fx (bit-lsh n0 (*fx 3 7))
	    (+fx (bit-lsh n1 (*fx 2 7))
		 (+fx (bit-lsh n2 7)
		      n3))))))

;*---------------------------------------------------------------------*/
;*    get-ucs2-string ...                                              */
;*---------------------------------------------------------------------*/
(define (get-ucs2-string mm o::elong sz::elong)
   ;; get the bom
   (let* ((n0 (char->integer (mmap-ref mm o)))
	  (n1 (char->integer (mmap-ref mm (+ o 1))))
	  (len (- sz 2))
	  (res (make-ucs2-string (elong->fixnum len))))
      (if (and (=fx n0 #xfe) (=fx n1 #xff))
	  ;; big-endian
	  (let loop ((i 0)
		     (j (+ sz 2)))
	     (if (= i len)
		 res
		 (let* ((u0 (char->integer (mmap-ref mm j)))
			(u1 (char->integer (mmap-ref mm (+ j 1))))
			(u (+fx (bit-lsh u0 8) u1)))
		    (ucs2-string-set! res i (integer->ucs2 u))
		    (loop (+ i 1) (+ j 2)))))
	  ;; little-endian
	  (let loop ((i 0)
		     (j (+ sz 2)))
	     (if (= i len)
		 res
		 (let* ((u0 (char->integer (mmap-ref mm j)))
			(u1 (char->integer (mmap-ref mm (+ j 1))))
			(u (+fx (bit-lsh u1 8) u0)))
		    (ucs2-string-set! res i (integer->ucs2 u))
		    (loop (+ i 1) (+ j 2))))))))
	  
;*---------------------------------------------------------------------*/
;*    id3v2-get-string ...                                             */
;*---------------------------------------------------------------------*/
(define (id3v2-get-string mm o::elong sz::elong)
   (if (= sz 1)
       ""
       (if (char=? (mmap-ref mm o) #a000)
	   ;; an IS 8859-1 string
	   (mmap-substring/len mm (+ o 1) (- sz 1))
	   ;; an UCS2 string
	   (get-ucs2-string mm (+ o 2) (- sz 1)))))
 
;*---------------------------------------------------------------------*/
;*    id3v2.2-frame ...                                                */
;*---------------------------------------------------------------------*/
(define (id3v2.2-frame mm o::elong)
   (let ((n0 (char->integer (mmap-ref mm (+ o 3))))
	 (n1 (char->integer (mmap-ref mm (+ o 4))))
	 (n2 (char->integer (mmap-ref mm (+ o 5)))))
      (values (mmap-substring/len mm o 3)
	      (fixnum->elong
	       (+fx (bit-lsh n0 (*fx 2 7))
		    (+fx (bit-lsh n1 7)
			 n2))))))

;*---------------------------------------------------------------------*/
;*    id3v2.2-frames ...                                               */
;*---------------------------------------------------------------------*/
(define (id3v2.2-frames mm)
   (let* ((size (id3v2-size mm 6))
	  (end (+ #e11 size))
	  (flags (mmap-ref mm 4)))
      (let loop ((i #e10)
		 (frames '()))
	 (if (>=elong i end)
	     frames
	     (multiple-value-bind (id sz flag)
		(id3v2.2-frame mm i)
		(if (or (=elong sz #e0) (>elong (+elong i sz) end))
		    frames
		    (case (string-ref id 0)
		       ((#\T)
			(loop (+elong i (+elong sz 6))
			      (cons (cons id (id3v2-get-string mm (+ i 6) sz))
				    frames)))
		       (else
			(loop (+elong i (+elong sz 6)) frames)))))))))
      
;*---------------------------------------------------------------------*/
;*    id3v2.3-frame ...                                                */
;*---------------------------------------------------------------------*/
(define (id3v2.3-frame mm o::elong)
   (let ((n0 (char->integer (mmap-ref mm (+ o 4))))
	 (n1 (char->integer (mmap-ref mm (+ o 5))))
	 (n2 (char->integer (mmap-ref mm (+ o 6))))
	 (n3 (char->integer (mmap-ref mm (+ o 7)))))
      (values (mmap-substring/len mm o 4)
	      (fixnum->elong
	       (+fx (bit-lsh n0 (*fx 3 7))
		    (+fx (bit-lsh n1 (*fx 2 7))
			 (+fx (bit-lsh n2 7)
			      n3))))
	      (mmap-substring/len mm (+ o 8) 2))))

;*---------------------------------------------------------------------*/
;*    id3v2.3-frames ...                                               */
;*---------------------------------------------------------------------*/
(define (id3v2.3-frames mm)
   (let* ((size (id3v2-size mm 6))
	  (end (+ 11 size))
	  (flags (mmap-ref mm 4)))
      (let loop ((i #e10)
		 (frames '()))
	 (if (>= i end)
	     frames
	     (multiple-value-bind (id sz flag)
		(id3v2.3-frame mm i)
		(if (or (= sz 0) (> (+ i sz) end))
		    frames
		    (case (string-ref id 0)
		       ((#\T)
			(loop (+ i (+ sz 10))
			      (cons (cons id (id3v2-get-string mm (+ i 10) sz))
				    frames)))
		       (else
			(loop (+ i (+ sz 10)) frames)))))))))

;*---------------------------------------------------------------------*/
;*    id3v2-get-frame ...                                              */
;*---------------------------------------------------------------------*/
(define (id3v2-get-frame key frames default)
   (let ((f (assoc key frames)))
      (if (pair? f)
	  (cdr f)
	  default)))

;*---------------------------------------------------------------------*/
;*    mp3-id3v2.2 ...                                                  */
;*---------------------------------------------------------------------*/
(define (mp3-id3v2.2 mm)
   (let ((frames (id3v2.2-frames mm)))
      (instantiate::id3
	 (version "id3v2.3")
	 (title (id3v2-get-frame "TT1" frames "???"))
	 (artist (id3v2-get-frame "TPE1" frames "???"))
	 (orchestra (id3v2-get-frame "TP2" frames #f))
	 (conductor (id3v2-get-frame "TP3" frames #f))
	 (interpret (id3v2-get-frame "TP4" frames #f))
	 (album (id3v2-get-frame "TAL" frames "???"))
	 (year (string->integer (id3v2-get-frame "TYE" frames "-1")))
	 (recording (id3v2-get-frame "TRD" frames #f))
	 (comment (id3v2-get-frame "COM" frames ""))
	 (genre (id3v2-genre (id3v2-get-frame "TCO" frames "-")))
	 (track (string->integer (id3v2-get-frame "TRK" frames "-1")))
	 (cd (id3v2-get-frame "MCI" frames #f)))))

;*---------------------------------------------------------------------*/
;*    id3v2-genre ...                                                  */
;*---------------------------------------------------------------------*/
(define (id3v2-genre str)
   (if (string=? str "")
       "unknown"
       (string-case str
	  ((: "(" (+ digit) ")")
	   (let ((n (string->integer (the-substring 1 -1))))
	      (cond
		 ((<fx n 0)
		  "unknown")
		 ((>=fx n (vector-length *id3v2-genres*))
		  "unknown")
		 (else
		  (vector-ref *id3v2-genres* n)))))
	  (else "unknown"))))

;*---------------------------------------------------------------------*/
;*    mp3-id3v2.3 ...                                                  */
;*---------------------------------------------------------------------*/
(define (mp3-id3v2.3 mm)
   (let ((frames (id3v2.3-frames mm)))
      (instantiate::id3
	 (version "id3v2.3")
	 (title (id3v2-get-frame "TIT2" frames "???"))
	 (artist (id3v2-get-frame "TPE1" frames "???"))
	 (orchestra (id3v2-get-frame "TPE2" frames #f))
	 (conductor (id3v2-get-frame "TPE3" frames #f))
	 (interpret (id3v2-get-frame "TPE4" frames #f))
	 (album (id3v2-get-frame "TALB" frames "???"))
	 (year (string->integer (id3v2-get-frame "TYER" frames "-1")))
	 (recording (id3v2-get-frame "TRDA" frames #f))
	 (comment (id3v2-get-frame "COMM" frames ""))
	 (genre (id3v2-genre (id3v2-get-frame "TCON" frames "-")))
	 (track (string->integer (id3v2-get-frame "TRCK" frames "-1")))
	 (cd (id3v2-get-frame "MCDI" frames #f)))))

;*---------------------------------------------------------------------*/
;*    mp3-id3 ...                                                      */
;*---------------------------------------------------------------------*/
(define (mp3-id3 path)
   (if (not (file-exists? path))
       (error/errno $errno-io-file-not-found-error
		    'mp3-id3 "Can't find file" path)
       (let ((mm (open-mmap path :write #f)))
	  (unwind-protect
	     (cond
		((id3v2.3? mm)
		 (mp3-id3v2.3 mm))
		((id3v2.2? mm)
		 (mp3-id3v2.2 mm))
		((id3v1.1? mm)
		 (mp3-id3v1.1 mm))
		((id3v1? mm)
		 (mp3-id3v1 mm))
		(else
		 #f))
	     (close-mmap mm)))))

