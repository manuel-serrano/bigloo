;*=====================================================================*/
;*    .../prgm/project/bigloo/api/flac/src/Llib/flac_alsadec.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Sep 18 19:18:08 2011                          */
;*    Last change :  Wed Sep 21 15:06:16 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    FLAC Alsa decoder                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __flac_alsadec

   (library alsa)

   (import __flac_flac)

   (extern (macro $flac-blit-string!::void
	      (::string ::long ::string ::long ::long) "BGL_FLAC_BLIT_STRING"))
   
   (static (class flac-alsa::flac-decoder
	      (%alsamusic (default #f))
	      (%buffer (default #f))))

   (export (class flac-alsadecoder::alsadecoder
	      (%flac::obj (default #unspecified)))))

;*---------------------------------------------------------------------*/
;*    $compiler-debug ...                                              */
;*---------------------------------------------------------------------*/
(define-macro ($compiler-debug)
   (bigloo-compiler-debug))

;*---------------------------------------------------------------------*/
;*    object-print ::flac-alsadecoder ...                              */
;*---------------------------------------------------------------------*/
(define-method (object-print o::flac-alsadecoder port print-slot)
   (display "#|flac-alsadecoder|" port))
   
;*---------------------------------------------------------------------*/
;*    debug                                                            */
;*---------------------------------------------------------------------*/
(define debug ($compiler-debug))

;*---------------------------------------------------------------------*/
;*    alsadecoder-init ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-init dec::flac-alsadecoder)
   (with-access::flac-alsadecoder dec (mimetypes %flac)
      (when (null? mimetypes)
	 (set! mimetypes '("audio/flac" "application/x-flac")))
      (set! %flac (instantiate::flac-alsa))))

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
(define-method (alsadecoder-position o::flac-alsadecoder buf)
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
   '(with-access::flac-alsadecoder o (%flac)
      (flac-volume-set! %flac vol)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-seek ::flac-alsadecoder ...                          */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-seek o::flac-alsadecoder ms)
   (with-access::flac-alsadecoder o (%flac)
      (flac-decoder-seek %flac ms)))

;*---------------------------------------------------------------------*/
;*    alsadecoder-stop ...                                             */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-stop o::flac-alsadecoder am::alsamusic)
   (with-access::flac-alsadecoder o (%flac)
      (with-access::alsamusic am (%buffer)
	 (when (alsabuffer? %buffer)
	    (with-access::alsabuffer %buffer (%bmutex %!bstate)
	       (with-lock %bmutex
		  (lambda ()
		     (set! %!bstate 3))))))))
   
;*---------------------------------------------------------------------*/
;*    alsadecoder-decode ::flac-alsadecoder ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsadecoder-decode dec::flac-alsadecoder
		  am::alsamusic
		  buffer::alsabuffer)
   (with-access::flac-alsadecoder dec (%flac)
      (with-access::flac-alsa %flac (%buffer %alsamusic)
	 (set! %buffer buffer)
	 (set! %alsamusic am)
	 (with-access::alsamusic am (%amutex %status pcm)
	    (with-access::musicstatus %status (state songpos songlength)
	       (flac-decoder-decode %flac)
	       (alsa-snd-pcm-cleanup pcm)
	       (with-access::alsabuffer %buffer (%!bstate)
		  (if (=fx %!bstate 3)
		      (set! state 'stop)
		      (begin
			 (set! %!bstate 4)
			 (set! state 'ended)))))))))

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
   (with-access::flac-alsa o (outbuf %alsamusic)
      (with-access::alsamusic %alsamusic (pcm %status %amutex)
	 (mutex-lock! %amutex)
	 (musicstatus-state-set! %status 'play)
	 (mutex-unlock! %amutex)
	 (when (>fx size 0)
	    (alsa-snd-pcm-write pcm outbuf size)
	    #t))))

;*---------------------------------------------------------------------*/
;*    flac-decoder-read ::flac-alsa ...                                */
;*---------------------------------------------------------------------*/
(define-method (flac-decoder-read o::flac-alsa size::long)
   (with-access::flac-alsa o (port %flacbuf %buffer %alsamusic)
      (with-access::alsabuffer %buffer (%bmutex %bcondv %!bstate %inbuf
					  %!tail %head %eof)

	 (define inlen (string-length %inbuf))
	 
	 (define flacbuf (custom-identifier %flacbuf))

	 (define (available)
	    (cond
	       ((>fx %head %!tail) (-fx %head %!tail))
	       ((<fx %head %!tail) (+fx (-fx inlen (-fx %!tail 1)) %head))
	       (else 0)))

	 (define (full-state?)
	    (and (=fx %!bstate 2) (>fx inlen (*fx (available) 2))))
	 
	 (define (inc-tail! size)
	    (set! %!tail (+fx %!tail size))
	    (when (=fx %!tail inlen)
	       (set! %!tail 0))
	    (cond
	       ((=fx %!tail %head)
		;; set state empty
		(mutex-lock! %bmutex)
		(when (>fx debug 0)
		   (with-access::alsabuffer %buffer (profile-lock)
		      (set! profile-lock (+fx 1 profile-lock))
		      (tprint "read.2, set empty (bs=0) size=" size
			 " %eof=" %eof " mutex-lock=" profile-lock)))
		(set! %!bstate 0)
		(condition-variable-broadcast! %bcondv)
		(mutex-unlock! %bmutex))
	       ((full-state?)
		;; set state filled
		(mutex-lock! %bmutex)
		(when (>fx debug 0)
		   (with-access::alsabuffer %buffer (profile-lock)
		      (set! profile-lock (+fx 1 profile-lock))
		      (tprint "read.2, set filled (bs=1) size=" size
			 " mutex-lock=" profile-lock)))
		(set! %!bstate 1)
		(condition-variable-broadcast! %bcondv)
		(mutex-unlock! %bmutex)))
	    (when (>fx debug 0)
	       (alsabuffer-assert %buffer "decode")))
	 
	 (let loop ()
	    (when (>fx debug 1)
	       (tprint "read.1 bs=" %!bstate " tl=" %!tail " hd=" %head
		  " %eof=" %eof " inlen=" inlen))
	    (cond
	       ((=fx %!bstate 3)
		;; buffer sop, the buffer is done reading
		-1)
	       ((=fx %!bstate 0)
		;; buffer empty
		(if %eof
		    (begin
		       (when (>fx debug 1)
			  (tprint "read.3 ended"))
		       ;; buffer empty, and eof, we are done
		       beof)
		    (begin
		       ;; buffer empty, wait to be filled
		       (mutex-lock! %bmutex)
		       (when (=fx %!bstate 0)
			  ;; a kind of double check locking, correct, is
			  ;; ptr read/write are atomic
			  (when (>fx debug 0)
			     (tprint ">>> read.2 bs=" 0))
			  (condition-variable-wait! %bcondv %bmutex)
			  (when (>fx debug 0)
			     (tprint "<<< read.2 bs=" 0))
			  (mutex-unlock! %bmutex))
		       (loop))))
	       (else
		(let ((sz (minfx size
			     (if (>fx %head %!tail)
				 (-fx %head %!tail)
				 (-fx inlen %!tail)))))
		   (when (>fx sz 0)
		      (with-access::alsamusic %alsamusic (%status)
			 (musicstatus-state-set! %status 'play))
		      ($flac-blit-string! %inbuf %!tail flacbuf 0 sz))
		   (when (>fx debug 0)
		      (tprint ">>> read.inc-tail sz=" sz))
		   (inc-tail! sz)
		   (when (>fx debug 0)
		      (tprint "<<< read.inc-tail sz=" sz))
		   sz)))))))
