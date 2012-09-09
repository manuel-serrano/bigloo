;*=====================================================================*/
;*    .../prgm/project/bigloo/api/flac/src/Llib/flac_alsadec.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Sun Sep  9 12:51:56 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    FLAC Alsa decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_alsadec
   
   (cond-expand
      ((library alsa)
       (library alsa)))
   
   (import __flac_flac)
   
   (extern (macro $flac-blit-string!::void
	      (::string ::long ::string ::long ::long) "BGL_FLAC_BLIT_STRING")
	   (macro $ref::byte
	      (::string ::long) "BGL_FLAC_STRING_REF")
	   (export flac-checksum-debug "bgl_flac_checksum_debug"))
   
   (static (class flac-alsa::flac-decoder
	      (%alsamusic (default #f))
	      (%buffer (default #f))
	      (%decoder (default #f))
	      (%rate::int (default 80))
	      (%rate-max::int (default 80))
	      (%rate-min::int (default 30))))
   
   (cond-expand
      ((library alsa)
       (export (class flac-alsadecoder::alsadecoder
	          (%flac::obj (default #unspecified))
		  (%inseek (default #f)))
	       (flac-checksum-debug::int ::int ::string ::int ::int)))))

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((library alsa)
;;; compile only if alsa available

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (begin (bigloo-compiler-debug) 1))

;*---------------------------------------------------------------------*/
;*    flac-debug ...                                                   */
;*---------------------------------------------------------------------*/
(define (flac-debug)
   (if (>fx ($compiler-debug) 0)
       (bigloo-debug)
       0))

;*---------------------------------------------------------------------*/
;*    flac-checksum-debug ...                                          */
;*---------------------------------------------------------------------*/
(define (flac-checksum-debug::int c::int buffer::string i::int s::int)
   (if (>=fx (flac-debug) 1)
       (let loop ((n 0)
		  (c c))
	  (if (=fx n s)
	      c
	      (loop (+fx n 1) (bit-xor c ($ref buffer (+fx i n))))))
       c))
	 
;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-init dec::flac-alsadecoder)
   (with-access::flac-alsadecoder dec (mimetypes %flac)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/flac" "application/x-flac")))
      (set! %flac (instantiate::flac-alsa))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-reset! ::flac-alsa ...                              */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-reset! o::flac-alsa)
   (with-access::flac-alsa o (%rate %bchecksum %rchecksum)
      (set! %rate 80)
      (set! %bchecksum #x80)
      (set! %rchecksum #x80)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-reset! ::flac-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-reset! o::flac-alsadecoder)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-reset! %flac)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-position ::flac-alsadecoder ...                      */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-position o::flac-alsadecoder abuf)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-position %flac)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-info ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-info o::flac-alsadecoder)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-info %flac)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-volume-set! ::flac-alsadecoder ...                   */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-volume-set! o::flac-alsadecoder vol)
   (with-access::flac-alsadecoder o (%flac)
      (flac-volume-set! %flac vol)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-seek ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-seek o::flac-alsadecoder sec)
   (with-access::flac-alsadecoder o (%flac %inseek)
      (unless %inseek
	 (set! %inseek #t)
	 (with-access::flac-alsa %flac ($builtin %eof)
	    (let* ((bps ($flac-decoder-get-bits-per-sample $builtin))
		   (srate ($flac-decoder-get-sample-rate $builtin))
		   (off (*llong (fixnum->llong srate) (fixnum->llong sec))))
	       ($flac-decoder-seek-absolute $builtin off))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-tell ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-tell o::flac-alsa)
   (with-access::flac-alsa o (%decoder)
      (with-access::flac-alsadecoder %decoder (%flac)
	 (with-access::flac-alsa %flac (%buffer)
	    (when (isa? %buffer alsabuffer)
	       (alsabuffer-tell %buffer))))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-seek ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-seek o::flac-alsa off)
   (with-access::flac-alsa o (%decoder %buffer)
      (with-access::flac-alsadecoder %decoder (%flac %inseek)
	 (when (isa? %buffer alsabuffer)
	    (alsabuffer-seek %buffer off)
	    (set! %inseek #f)
	    #t))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-length ::flac-alsa ...                              */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-length o::flac-alsa)
   (with-access::flac-alsa o (%decoder)
      (with-access::flac-alsadecoder %decoder (%flac)
	 (with-access::flac-alsa %flac (%buffer)
	    (alsabuffer-length %buffer)))))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::flac-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode dec::flac-alsadecoder
		  am::alsamusic
		  buffer::alsabuffer)
   (with-access::flac-alsadecoder dec (%flac)
      (with-access::flac-alsa %flac (%buffer %alsamusic %decoder)
	 (set! %buffer buffer)
	 (set! %alsamusic am)
	 (set! %decoder dec)
	 (with-access::alsamusic am (%amutex %status pcm)
	    (with-access::alsabuffer buffer (url)
	       (when (>=fx (flac-debug) 1) (debug-init! url))
	       (unwind-protect
		  (flac-decoder-decode %flac)
		  (with-access::alsabuffer %buffer (%eof)
		     (alsa-snd-pcm-cleanup pcm)
		     (onstate am (if %eof 'ended 'stop))
		     (when (>=fx (flac-debug) 1)
			(with-access::flac-alsa %flac (%bchecksum %rchecksum)
			   (debug "--- FLAC_DECODER, buffer checksum: "
			      %bchecksum
			      " read checksum: " %rchecksum))
			(debug-stop! url)))))))))

;*---------------------------------------------------------------------*/
;*    onstate ...                                                      */
;*---------------------------------------------------------------------*/
(define (onstate am st)
   (with-access::alsamusic am (onstate %status)
      (with-access::musicstatus %status (state)
         (set! state st)
         (onstate am %status))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-metadata ::flac-alsa ...                            */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-metadata o::flac-alsa total rate channels bps)
   (with-access::flac-alsa o (%alsamusic)
      (with-access::alsamusic %alsamusic (pcm)
	 (let ((encoding (case bps
			    ((8) 's16)
			    ((16) 's16)
			    ((24) 's24-3le)
			    ((32) 's32))))
	    (alsa-snd-pcm-hw-set-params! pcm
	       :rate-resample 1
	       :access 'rw-interleaved
	       :format encoding
	       :channels channels
	       :rate-near rate
	       :buffer-time-near 500000
	       :buffer-size-near (/fx rate 2)
	       :period-size-near (/fx rate 8))
	    (alsa-snd-pcm-sw-set-params! pcm
	       :start-threshold 1
	       :avail-min 1)))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-write ::flac-alsa ...                               */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-write o::flac-alsa size rate channels bps)
   (with-access::flac-alsa o (outbuf %alsamusic %buffer)
      (with-access::alsamusic %alsamusic (pcm %status)
	 (when (>fx size 0)
	    (alsa-snd-pcm-write pcm outbuf size)
	    #t))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-read o::flac-alsa size::long)
   (with-access::flac-alsa o (%flacbuf %buffer (am %alsamusic) %decoder
				%rate %rate-max %rate-min)
      (with-access::alsadecoder %decoder (%!dabort %!dpause %dcondv %dmutex)
	 (with-access::alsabuffer %buffer (%bmutex %bcondv 
					     %inbuf %inbufp %inlen
					     %tail %head %eof
					     %empty
					     url)
	    
	    (define inlen %inlen)
	    
	    (define flacbuf (custom-identifier %flacbuf))
	    
	    (define (buffer-percentage-filled)
	       (llong->fixnum
		  (/llong (*llong #l100
			     (fixnum->llong (alsabuffer-available %buffer)))
		     (fixnum->llong inlen))))
	    
	    (define (buffer-filled?)
	       ;; filled when > 25%
	       (and (not %empty)
		    (>fx (*fx 4 (alsabuffer-available %buffer)) inlen)))
	    
	    (define (broadcast-not-full p)
	       (when (>=fx (flac-debug) 2)
		  (debug "--> FLAC_DECODER, broadcast not-full "
		     url " " p "%" " " (current-microseconds)))
	       (mutex-lock! %bmutex)
	       (condition-variable-broadcast! %bcondv)
	       (mutex-unlock! %bmutex)
	       (when (>=fx (flac-debug) 2)
		  (debug (current-microseconds) "\n")))
	    
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
		  (let* ((avail (alsabuffer-available %buffer))
			 (p (/fx (*fx avail 100) inlen)))
		     (cond
			((<fx p %rate)
			 (broadcast-not-full p)
			 (when (<fx %rate %rate-max)
			    (set! %rate (+fx %rate 10))))
			((>fx p %rate-min)
			 (when (>fx %rate %rate-min)
			    (set! %rate (-fx %rate 1))))))))

	    (when (>=fx (flac-debug) 1)
	       (with-access::flac-alsa o (%bchecksum %rchecksum)
		  (unless (=fx %bchecksum %rchecksum)
		     (tprint "FLAC CHECKSUM ERROR: bchecksum="
			%bchecksum " rchecksum=" %rchecksum)
		     (debug "!!! FLAC_DECODER, CHECKSUM_ERROR"
			" bchecksum=" %bchecksum
			" rchecksum=" %rchecksum
			" %tail=" %tail " %head=" %head
			" size=" size "\n")
		     (exit -1))))
	    
	    (let loop ((size size)
		       (i 0))
	       (cond
		  (%!dpause
		   ;;; the decoder is asked to pause
		   (with-access::alsamusic am (%status)
		      (with-access::musicstatus %status (songpos)
			 (set! songpos
			    (alsadecoder-position %decoder %buffer))))
		   (mutex-lock! %dmutex)
		   (if %!dpause
		       (let liip ()
			  (mutex-unlock! %dmutex)
			  (onstate am 'pause)
			  (mutex-lock! %dmutex)
			  (condition-variable-wait! %dcondv %dmutex)
			  (if %!dpause
			      (liip)
			      (begin
				 (mutex-unlock! %dmutex)
				 (onstate am 'play)
				 (loop size i))))))
		  (%!dabort
		   ;;; the decoder is asked to abort
		   -1)
		  (%empty
		   ;;; buffer empty, unless eof wait for it to be filled
		   (if %eof
		       (begin
			  (when (>=fx (flac-debug) 1)
			     (debug "--- FLAC_DECODER, EOF " url "\n"))
			  beof)
		       (begin
			  (when (>=fx (flac-debug) 1)
			     (debug "<-- FLAC_DECODER, wait not-empty " url
				" " (current-microseconds) "..."))
			  (mutex-lock! %bmutex)
			  (when (>=fx (flac-debug) 1)
			     (debug "empty=" %empty " eof=" %eof "..."))
			  (let liip ()
			     ;; wait until the buffer is filled
			     (unless (or (not %empty) %eof %!dabort (buffer-filled?))
				(with-access::alsamusic am (%status)
				   (with-access::musicstatus %status (buffering)
				      (set! buffering
					 (buffer-percentage-filled))))
				(onstate am 'buffering)
				(condition-variable-wait! %bcondv %bmutex)
				(liip)))
			  (mutex-unlock! %bmutex)
			  (when (>=fx (flac-debug) 1)
			     (debug (current-microseconds) "\n"))
			  (onstate am 'play)
			  (loop size i))))
		  (else
		   (let ((s (minfx size
			       (if (>fx %head %tail)
				   (-fx %head %tail)
				   (-fx inlen %tail)))))
		      (when (>fx s 0)
			 (when (>=fx (flac-debug) 1)
			    (with-access::flac-alsa o (%bchecksum)
;* 			       (toberemoved-debug-bcheck %inbufp %tail s) */
			       (set! %bchecksum
				  (flac-checksum-debug
				     %bchecksum %inbufp %tail s))))
			 ($flac-blit-string! %inbufp %tail flacbuf i s)
			 (inc-tail! s))
		      (if (<fx s size)
			  (loop (- size s) (+fx i s))
			  (let ((r (+fx i s)))
			     (when (and (>=fx (flac-debug) 1) (=fx r 0))
				(debug "!!! FLAC_DECODER, read 0 chars "
				   url " " (current-microseconds)))
			     r))))))))))

;*---------------------------------------------------------------------*/
;*    toberemoved-debug-bcheck ...                                     */
;*---------------------------------------------------------------------*/
(define foo #f)

(define (toberemoved-debug-bcheck buf::string i s)
   (unless (binary-port? foo)
      (set! foo (open-output-binary-file "/tmp/DEBUG_BCHECK")))
   (let loop ((n 0))
      (when (<fx n s)
	 (output-byte foo ($ref buf (+ i n)))
	 (loop (+fx n 1))))
   (flush-binary-port foo))

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

;*---------------------------------------------------------------------*/
;*    alsa dependency                                                  */
;*---------------------------------------------------------------------*/
))
