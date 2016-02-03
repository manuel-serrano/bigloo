;*=====================================================================*/
;*    .../prgm/project/bigloo/api/flac/src/Llib/flac_decoder.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Wed Jan 27 17:30:18 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    FLAC decoder                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_decoder
   
   (extern (macro $dump::int (::string ::string ::int ::int)  "bgl_flac_dump")
	   (macro $memcpy::void (::string ::string ::long) "memcpy")
	   (macro $flac-blit-string!::void
	      (::string ::long ::string ::long ::long) "BGL_FLAC_BLIT_STRING")
	   (macro $ref::byte
	      (::string ::long) "BGL_FLAC_STRING_REF"))
   
   (library multimedia)
   
   (import __flac_flac)
   
   (export (abstract-class flacdec::flac-decoder
	      (%music (default #f))
	      (%buffer (default #f))
	      (%decoder (default #f))
	      (%rate::int (default 80))
	      (%rate-max::int (default 80))
	      (%rate-min::int (default 50))
	      (%last-percentage::int (default 0)))

	   (class flacmusicdecoder::musicdecoder
	      (%flac::obj (default #f))
	      (%inseek (default #f)))

	   (flac-debug::int)
	   (flac-checksum-debug::int ::long ::string ::long ::long)))

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (bigloo-compiler-debug))

;*---------------------------------------------------------------------*/
;*    flac-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (flac-debug)
   (begin
      (if (>fx ($compiler-debug) 0)
	  (bigloo-debug)
	  0)
      1))

;*---------------------------------------------------------------------*/
;*    flac-checksum-debug ...                                          */
;*---------------------------------------------------------------------*/
(define (flac-checksum-debug::int c::long buffer::string i::long s::long)
   (let loop ((n 0)
	      (c c))
      (if (=fx n s)
	  c
	  (loop (+fx n 1) (bit-xor c ($ref buffer (+fx i n)))))))
	 
;*---------------------------------------------------------------------*/
;*    musicdecoder-init ::flacmusicdecoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-init dec::flacmusicdecoder)
   (with-access::flacmusicdecoder dec (mimetypes %flac)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/flac" "application/x-flac" "audio/x-flac")))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-reset! ::flacdec ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-reset! o::flacdec)
   (with-access::flacdec o (%rate %bchecksum %rchecksum %last-percentage)
      (set! %rate 80)
      (set! %last-percentage 0)
      (set! %bchecksum #x80)
      (set! %rchecksum #x80)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    flac-decoder-write ::flacdec ...                                 */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-write o::flacdec size rate channels bps)
   (with-access::flacdec o (outbuf %music %buffer)
      (when (>fx size 0)
	 (musicbuf-write %music outbuf size)
	 #t)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-reset! ::flacmusicdecoder ...                       */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-reset! o::flacmusicdecoder)
   (with-access::flacmusicdecoder o (%flac)
      (flac-decoder-reset! %flac)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-position ::flacmusicdecoder ...                     */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-position o::flacmusicdecoder abuf)
   (with-access::flacmusicdecoder o (%flac)
      (flac-decoder-position %flac)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-info ::flacmusicdecoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-info o::flacmusicdecoder)
   (with-access::flacmusicdecoder o (%flac)
      (flac-decoder-info %flac)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-volume-set! ::flacmusicdecoder ...                  */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-volume-set! o::flacmusicdecoder vol)
   (with-access::flacmusicdecoder o (%flac)
      (flac-volume-set! %flac vol)))

;*---------------------------------------------------------------------*/
;*    musicdecoder-seek ::flacmusicdecoder ...                         */
;*---------------------------------------------------------------------*/
(define-method (musicdecoder-seek o::flacmusicdecoder sec)
   (with-access::flacmusicdecoder o (%flac %inseek)
      (unless %inseek
	 (set! %inseek #t)
	 (with-access::flacdec %flac ($builtin %eof)
	    (let* ((bps ($flac-decoder-get-bits-per-sample $builtin))
		   (srate ($flac-decoder-get-sample-rate $builtin))
		   (off (*llong (fixnum->llong srate) (fixnum->llong sec))))
	       ($flac-decoder-seek-absolute $builtin off))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-read o::flacdec size::long)
   (with-access::flacdec o (%flacbuf %buffer (am %music) %decoder
			      %rate %rate-max %rate-min %last-percentage)
      (with-access::musicdecoder %decoder (%!dabort %!dpause %dcondv %dmutex)
	 (with-access::musicbuffer %buffer (%bmutex %bcondv
					      %inbufp %inlen
					      %tail %head %eof
					      %empty
					      url)
	    
	    (define inlen %inlen)
	    
	    (define flacbuf::string (custom-identifier %flacbuf))
	    
	    (define (buffer-percentage-filled)
	       (llong->fixnum
		  (/llong (*llong #l100
			     (fixnum->llong (musicbuffer-available %buffer)))
		     (fixnum->llong inlen))))
	    
	    (define (buffer-filled?)
	       ;; filled when > 25%
	       (and (not %empty)
		    (>fx (*fx 4 (musicbuffer-available %buffer)) inlen)))
	    
	    (define (broadcast-not-full p)
	       (synchronize %bmutex
		  (condition-variable-broadcast! %bcondv)))
	    
	    (define (inc-tail! size)
	       ;; increment the tail
	       (let ((ntail (+fx %tail size)))
		  (when (=fx ntail inlen)
		     (set! ntail 0))
		  ;; check buffer emptyness
		  (when (=fx ntail %head)
		     (when (<fx %rate 80) (set! %rate 80))
		     (set! %empty #t))
		  ;; increment the shared %tail
		  (set! %tail ntail))
	       (unless %eof
		  ;; check if we must notify the buffer filler
		  ;; that the buffer is no longer full
		  (let* ((avail (musicbuffer-available %buffer))
			 (p (/fx (*fx avail 100) inlen)))
		     (cond
			((<fx p %rate)
			 (broadcast-not-full p)
			 (when (and (<=fx p %last-percentage) (<fx %rate %rate-max))
			    (set! %rate (+fx %rate 10))))
			((>fx p %rate-min)
			 (when (>fx %rate %rate-min)
			    (set! %rate (-fx %rate 1)))))
		     (set! %last-percentage p)
		     #unspecified)))
	    
	    (let loop ((size size)
		       (i 0))
	       (cond
		  (%!dpause
		   (music-state-set! am 'pause)
		   ;;; the decoder is asked to pause
		   (with-access::music am (%status)
		      (with-access::musicstatus %status (songpos)
			 (set! songpos
			    (musicdecoder-position %decoder %buffer))))
		   (synchronize %dmutex
		      (let liip ()
			 (when %!dpause
			    (condition-variable-wait! %dcondv %dmutex)
			    (liip))))
		   (music-state-set! am 'play)
		   (loop size i))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   -1)
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       beof
		       (begin
			  (with-access::music am (%status)
			     (with-access::musicstatus %status (buffering)
				(set! buffering
				   (buffer-percentage-filled))))
			  (music-state-set! am 'buffering)
			  (synchronize %bmutex
			     ;; wait until the buffer is filled
			     (unless (or (not %empty)
					 %eof
					 %!dabort
					 (buffer-filled?))
				(condition-variable-wait! %bcondv %bmutex)))
			  (music-state-set! am 'play)
			  (loop size i))))
		  (else
		   (let ((s (minfx size
			       (if (>fx %head %tail)
				   (-fx %head %tail)
				   (-fx inlen %tail)))))
		      (when (>fx s 0)
			 ($flac-blit-string! %inbufp %tail flacbuf i s)
			 (inc-tail! s))
		      (if (<fx s size)
			  (loop (-fx size s) (+fx i s))
			  (+fx i s))))))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-tell ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-tell o::flacdec)
   (with-access::flacdec o (%decoder)
      (with-access::flacmusicdecoder %decoder (%flac)
	 (with-access::flacdec %flac (%buffer)
	    (when (isa? %buffer musicbuffer)
	       (musicbuffer-tell %buffer))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-seek ::flac-decoder ...                             */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-seek o::flacdec off)
   (with-access::flacdec o (%decoder %buffer)
      (with-access::flacmusicdecoder %decoder (%inseek)
	 (when (isa? %buffer musicbuffer)
	    (musicbuffer-seek %buffer off)
	    (set! %inseek #f)
	    #t))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-length ::flac-decoder ...                           */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-length o::flacdec)
   (with-access::flacdec o (%decoder)
      (with-access::flacmusicdecoder %decoder (%flac)
	 (with-access::flacdec %flac (%buffer)
	    (musicbuffer-length %buffer)))))

;*---------------------------------------------------------------------*/
;*    *debug-port* ...                                                 */
;*---------------------------------------------------------------------*/
(define *debug-port* #f)

;*---------------------------------------------------------------------*/
;*    debug-init! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-init! url)
   (set! *debug-port* (open-output-file "/tmp/FLAC.log"))
   (debug ">>> FLAC_DECODER init " url " " (current-microseconds) "\n"))

;*---------------------------------------------------------------------*/
;*    debug-stop! ...                                                  */
;*---------------------------------------------------------------------*/
(define (debug-stop! url)
   (debug ">>> FLAC_DECODER stop " url " " (current-microseconds) "\n")
   (close-output-port *debug-port*))
   
;*---------------------------------------------------------------------*/
;*    debug ...                                                        */
;*---------------------------------------------------------------------*/
(define (debug . args)
   (for-each (lambda (a) (display a *debug-port*)) args)
   (flush-output-port *debug-port*))
