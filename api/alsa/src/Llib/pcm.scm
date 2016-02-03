;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/pcm.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:08:52 2011                          */
;*    Last change :  Thu Jan 28 15:43:01 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    PCM interface                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_pcm

   (include "alsa.sch")

   (import __alsa_alsa)

   (export (class alsa-snd-pcm::alsa-object
	      ($builtin::$snd-pcm read-only (default (%$snd-pcm-nil)))
	      (name::bstring (default ""))
	      (device::bstring read-only (default "default"))
	      (stream::symbol read-only (default 'playback))
	      (mode::symbol read-only (default 'default))
	      (hwbps::int (default 32))
	      (hwsrate::int (default 384000)))

	   (%$snd-pcm-nil)
	   
	   (alsa-snd-pcm-open ::alsa-snd-pcm)
	   (alsa-snd-pcm-reopen ::alsa-snd-pcm)
	   (alsa-snd-pcm-close ::alsa-snd-pcm)
	   (alsa-snd-pcm-get-state ::alsa-snd-pcm)
	   (alsa-snd-pcm-avail::long ::alsa-snd-pcm)
	   (alsa-snd-pcm-avail-update::long ::alsa-snd-pcm)
	   (alsa-snd-pcm-set-params! ::alsa-snd-pcm
	      #!key format access channels rate soft-resample latency)
	   (alsa-snd-pcm-writei::long ::alsa-snd-pcm ::string ::long)
	   (alsa-snd-pcm-pause ::alsa-snd-pcm ::bool)
	   (alsa-snd-pcm-wait ::alsa-snd-pcm ::int)
	   (alsa-snd-pcm-reset ::alsa-snd-pcm)
	   (alsa-snd-pcm-recover ::alsa-snd-pcm #!optional (err 0))
	   (alsa-snd-pcm-prepare ::alsa-snd-pcm)
	   (alsa-snd-pcm-start ::alsa-snd-pcm)
	   (alsa-snd-pcm-drop ::alsa-snd-pcm)
	   (alsa-snd-pcm-drain ::alsa-snd-pcm)
	   (alsa-snd-pcm-cleanup ::alsa-snd-pcm)
	   (alsa-snd-pcm-hw-card-detect! ::alsa-snd-pcm ::int)
	   (alsa-snd-pcm-hw-free! ::alsa-snd-pcm)
	   (alsa-snd-pcm-hw-set-params! ::alsa-snd-pcm . rest)
	   (alsa-snd-pcm-hw-test-params? ::alsa-snd-pcm . rest)
	   (alsa-snd-pcm-hw-params-get-buffer-size::int ::alsa-snd-pcm)
	   (alsa-snd-pcm-hw-params-get-buffer-time::int ::alsa-snd-pcm)
	   (alsa-snd-pcm-hw-params-get-rates ::alsa-snd-pcm)
	   (alsa-snd-pcm-sw-set-params! ::alsa-snd-pcm . rest)

	   (alsa-snd-pcm-write::long ::alsa-snd-pcm ::string ::long)
	   (alsa-snd-pcm-flush ::alsa-snd-pcm)))

;*---------------------------------------------------------------------*/
;*    object-print ::alsa-snd-pcm ...                                  */
;*---------------------------------------------------------------------*/
(define-method (object-print o::alsa-snd-pcm port print-slot)
   (with-access::alsa-snd-pcm o (name device)
      (display "#|alsa-snd-pcm name=" port)
      (print-slot name port)
      (display " device=" port)
      (print-slot device port)
      (display "|" port)))
   
;*---------------------------------------------------------------------*/
;*    %$snd-pcm-nil ...                                                */
;*---------------------------------------------------------------------*/
(define (%$snd-pcm-nil)
   ($snd-pcm-nil))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-open ...                                            */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-open o::alsa-snd-pcm)
   (with-access::alsa-snd-pcm o ($builtin device stream mode name)
      (if ($snd-pcm-nil? $builtin)
	  (let ((err ($bgl-snd-pcm-open
			o
			device
			(symbol->stream stream)
			(symbol->pcm-mode mode))))
	     (if (<fx err 0)
		 (raise (instantiate::&alsa-error
			   (proc "alsa-snd-pcm-open")
			   (msg ($snd-strerror err))
			   (obj device)))
		 (set! name ($snd-pcm-name $builtin))))
	  (raise (instantiate::&alsa-error
		    (proc "alsa-snd-pcm-open")
		    (msg "pcm device already open")
		    (obj o))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-close ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-close pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (unless ($snd-pcm-nil? $builtin)
	 (unless (eq? (alsa-snd-pcm-get-state pcm) 'disconnected)
	    ($bgl-snd-pcm-close pcm)))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-reopen ...                                          */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-reopen pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin device stream mode name)
      (let ((err ($bgl-snd-pcm-reopen
		    pcm
		    device
		    (symbol->stream stream)
		    (symbol->pcm-mode mode))))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-open")
		       (msg ($snd-strerror err))
		       (obj device)))
	     (set! name ($snd-pcm-name $builtin))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-get-state ...                                       */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-get-state pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (if ($snd-pcm-nil? $builtin)
	  'not-open
	  (let ((s ($snd-pcm-get-state $builtin)))
	     (cond
		((=fx s $snd-pcm-state-open) 'open)
		((=fx s $snd-pcm-state-setup) 'setup)
		((=fx s $snd-pcm-state-prepared) 'prepared)
		((=fx s $snd-pcm-state-running) 'running)
		((=fx s $snd-pcm-state-xrun) 'xrun)
		((=fx s $snd-pcm-state-draining) 'draining)
		((=fx s $snd-pcm-state-paused) 'paused)
		((=fx s $snd-pcm-state-suspended) 'suspended)
		((=fx s $snd-pcm-state-disconnected) 'disconnected)
		(else (raise (instantiate::&alsa-error
				(proc "alsa-pcm-state")
				(msg "Unknown state")
				(obj pcm)))))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-avail ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-avail pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      ($snd-pcm-avail $builtin)))
   
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-avail-update ...                                    */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-avail-update pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      ($snd-pcm-avail-update $builtin)))
   
;*---------------------------------------------------------------------*/
;*    symbol->stream ...                                               */
;*---------------------------------------------------------------------*/
(define (symbol->stream::$snd-pcm-stream s::symbol)
   (case s
      ((playback) $snd-pcm-stream-playback)
      ((capture) $snd-pcm-stream-capture)
      (else (raise (instantiate::&alsa-error
		      (proc "alsa-pcm")
		      (msg "Unknown stream direction")
		      (obj s))))))

;*---------------------------------------------------------------------*/
;*    symbol->pcm-mode ...                                             */
;*---------------------------------------------------------------------*/
(define (symbol->pcm-mode::int s::symbol)
   (case s
      ((default) 0)
      ((nonblock) $snd-pcm-nonblock)
      ((async) $snd-pcm-async)
      (else (raise (instantiate::&alsa-error
		      (proc "alsa-pcm")
		      (msg "Unknown mode")
		      (obj s))))))

;*---------------------------------------------------------------------*/
;*    symbol->format ...                                               */
;*---------------------------------------------------------------------*/
(define (symbol->format::$snd-pcm-format s::symbol)
   (case s
      ((unknown) $snd-pcm-format-unknown)
      ((s8) $snd-pcm-format-s8)
      ((u8) $snd-pcm-format-u8)
      ((s16) $snd-pcm-format-s16)
      ((s16-le) $snd-pcm-format-s16-le)
      ((s16-be) $snd-pcm-format-s16-be)
      ((u16) $snd-pcm-format-u16)
      ((u16-le) $snd-pcm-format-u16-le)
      ((u16-be) $snd-pcm-format-u16-be)
      ((s24) $snd-pcm-format-s24)
      ((s24-le) $snd-pcm-format-s24-le)
      ((s24-be) $snd-pcm-format-s24-be)
      ((s24-3le) $snd-pcm-format-s24-3le)
      ((s24-3be) $snd-pcm-format-s24-3be)
      ((u24) $snd-pcm-format-u24)
      ((u24-le) $snd-pcm-format-u24-le)
      ((u24-be) $snd-pcm-format-u24-be)
      ((u24-3le) $snd-pcm-format-u24-3le)
      ((u24-3be) $snd-pcm-format-u24-3be)
      ((s32) $snd-pcm-format-s32)
      ((s32-le) $snd-pcm-format-s32-le)
      ((s32-be) $snd-pcm-format-s32-be)
      ((u32) $snd-pcm-format-u32)
      ((u32-le) $snd-pcm-format-u32-le)
      ((u32-be) $snd-pcm-format-u32-be)
      ((float-le) $snd-pcm-format-float-le)
      ((float-be) $snd-pcm-format-float-be)
      ((float64-le) $snd-pcm-format-float64-le)
      ((float64-be) $snd-pcm-format-float64-be)
      ((iec958-subframe-le) $snd-pcm-format-iec958-subframe-le)
      ((iec958-subframe-be) $snd-pcm-format-iec958-subframe-be)
      ((mu-law) $snd-pcm-format-mu-law)
      ((a-law) $snd-pcm-format-a-law)
      ((ima-adpcm) $snd-pcm-format-ima-adpcm)
      ((mpeg) $snd-pcm-format-mpeg)
      ((gsm) $snd-pcm-format-gsm)
      ((special) $snd-pcm-format-special)
      ((s20-3le) $snd-pcm-format-s20-3le)
      ((s20-3be) $snd-pcm-format-s20-3be)
      ((u20-3le) $snd-pcm-format-u20-3le)
      ((u20-3be) $snd-pcm-format-u20-3be)
      ((s18-3le) $snd-pcm-format-s18-3le)
      ((s18-3be) $snd-pcm-format-s18-3be)
      ((u18-3le) $snd-pcm-format-u18-3le)
      ((u18-3be) $snd-pcm-format-u18-3be)
      ((float) $snd-pcm-format-float)
      ((float64) $snd-pcm-format-float64)
      ((iec958-subframe) $snd-pcm-format-iec958-subframe)
      (else (raise (instantiate::&alsa-error
		      (proc "symbol->format")
		      (msg "Unknown format")
		      (obj s))))))

;*---------------------------------------------------------------------*/
;*    symbol->access ...                                               */
;*---------------------------------------------------------------------*/
(define (symbol->access::$snd-pcm-access s::symbol)
   (case s
      ((mmap-interleaved) $snd-pcm-access-mmap-interleaved)
      ((mmap-noninterleaved) $snd-pcm-access-mmap-noninterleaved)
      ((mmap-complex) $snd-pcm-access-mmap-complex)
      ((rw-interleaved) $snd-pcm-access-rw-interleaved)
      ((rw-noninterleaved) $snd-pcm-access-rw-noninterleaved)
      (else (raise (instantiate::&alsa-error
		      (proc "symbol->access")
		      (msg "Unknown access")
		      (obj s))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-set-params! ...                                     */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-set-params! pcm::alsa-snd-pcm
	   #!key format access channels rate soft-resample latency)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-set-params! $builtin
		    (symbol->format format)
		    (symbol->access access)
		    channels
		    rate
		    soft-resample
		    latency)))
	 (when (<fx err 0)
	    (raise (instantiate::&alsa-error
		      (proc "alsa-snd-pcm-set-params!")
		      (msg ($snd-strerror err))
		      (obj pcm)))))))
   
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-writei ...                                          */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-writei::long pcm::alsa-snd-pcm buffer::string sz::long)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-writei $builtin buffer sz)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-writei")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-pause ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-pause pcm::alsa-snd-pcm v::bool)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-pause $builtin (if v 1 0))))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-pause")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-wait ...                                            */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-wait pcm::alsa-snd-pcm tmt::int)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-wait $builtin tmt)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-wait")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-reset ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-reset pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-reset $builtin)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-reset")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-recover ...                                         */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-recover pcm::alsa-snd-pcm #!optional (err 0))
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-recover $builtin err 0)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-recover")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-prepare ...                                         */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-prepare pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-prepare $builtin)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-prepare")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-start ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-start pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-start $builtin)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-start")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-drop ...                                            */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-drop pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-drop $builtin)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-drop")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-drain ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-drain pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-drain $builtin)))
	 (if (<fx err 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-pcm-drain")
		       (msg ($snd-strerror err))
		       (obj pcm)))
	     err))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-cleanup ...                                         */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-cleanup pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (unless ($snd-pcm-nil? $builtin)
	 (let loop ()
	    (let ((state (alsa-snd-pcm-get-state pcm)))
	       ;; (tprint "alsa-snd-pcm-cleanup state=" state)
	       (case state
		  ((open prepared)
		   #f)
		  ((setup)
		   (alsa-snd-pcm-prepare pcm)
		   (loop))
		  ((xrun)
		   (alsa-snd-pcm-drop pcm)
		   (loop))
		  ((running)
		   (with-handler
		      (lambda (e)
			 ;; (tprint "alsa-snd-pcm-cleanup drain error: " e)
			 #f)
		      (alsa-snd-pcm-drain pcm))
		   (loop))
		  (else
		   (with-handler
		      (lambda (e)
			 ;; (tprint "alsa-snd-pcm-cleanup wait error: " e)
			 #f)
		      (alsa-snd-pcm-wait pcm 1000))
		   (loop))))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-card-detect! ...                                 */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-card-detect! pcm::alsa-snd-pcm card::int)
   
   (define (alsa-string-bps str)
      (let ((i (string-index str #\_)))
	 (string-downcase! (if i (substring str 0 i) str))))
   
   (define (alsa-try? card pcm #!key access format channels rate-near)
      ;; CARE MS: 28 Mar 2015
      ;; The following is not robust and should be radically changed.
      ;; As of Mars 2015, I do know any better method.
      ;; Currently, the following imprecise heuristics is used to check if a
      ;; hardware (sound card) supports or not a certain configuration:
      ;;   1- first, tests what alsa says (in some situation (I suspect
      ;;      when there is not over/under-rating specified in the .asoundrc
      ;;      file).
      ;;   2- if alsa agrees, check if there is a /proc/asound file associated;
      ;;      if there one, tries to set the card in the tested configuration 
      ;;      and parse the /proc file to check if the configuration succeeded
      (when (alsa-snd-pcm-hw-test-params? pcm
	       :access access
	       :format format
	       :channels channels
	       :rate-near rate-near)
	 (if (not card)
	     #t
	     (let ((path ((@ format __r4_output_6_10_3)
			  "/proc/asound/card~a/pcm0p/sub0/hw_params" card))
		   (bps (alsa-string-bps (symbol->string! format))))
		(if (not (file-exists? path))
		    #t
		    (begin
		       (alsa-snd-pcm-hw-set-params! pcm
			  :access access
			  :format format
			  :channels channels
			  :rate-near rate-near)
		       (let* ((s (call-with-input-file path read-string))
			      (f (pregexp-match "format: (.+)" s))
			      (r (pregexp-match "rate: ([0-9]+)" s)))
			  (cond
			     ((string=? s "no setup\n")
			      #f)
			     ((or (not f) (not r))
			      #t)
			     (else
			      (and (string>=? (alsa-string-bps (cadr f)) bps)
				   (=fx (string->integer (cadr r)) rate-near)))))))))))
   
   (with-access::alsa-snd-pcm pcm (device hwbps hwsrate)
      (with-handler
	 (lambda (e)
	    #f)
	 (alsa-snd-pcm-open pcm)
	 (unwind-protect
	    (cond
	       ((alsa-try? card pcm
		   :access 'rw-interleaved
		   :format 's32
		   :channels 2
		   :rate-near 384000)
		(set! hwbps 32)
		(set! hwsrate 384000))
	       ((alsa-try? card pcm
		   :access 'rw-interleaved
		   :format 's24-3le
		   :channels 2
		   :rate-near 384000)
		(set! hwbps 24)
		(set! hwsrate 384000))
	       ((alsa-try? card pcm
		   :access 'rw-interleaved
		   :format 's24-3le
		   :channels 2
		   :rate-near 192000)
		(set! hwbps 24)
		(set! hwsrate 192000))
	       ((alsa-try? card pcm
		   :access 'rw-interleaved
		   :format 's24-3le
		   :channels 2
		   :rate-near 96000)
		(set! hwbps 24)
		(set! hwsrate 96000))
	       ((alsa-try? card pcm
		   :access 'rw-interleaved
		   :format 's24-3le
		   :channels 2
		   :rate-near 48000)
		(set! hwbps 24)
		(set! hwsrate 48000))
	       ((alsa-try? card pcm
		   :access 'rw-interleaved
		   :format 's16
		   :channels 2
		   :rate-near 48000)
		(set! hwbps 16)
		(set! hwsrate 48000))
	       (else
		(set! hwbps 16)
		(set! hwsrate 44100)))
	    (alsa-snd-pcm-close pcm)))))
	 
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-free! ...                                        */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-free! pcm::alsa-snd-pcm)
   (with-access::alsa-snd-pcm pcm ($builtin)
      (let ((err ($snd-pcm-hw-free! $builtin)))
	 (when (<fx err 0)
	    (raise (instantiate::&alsa-error
		      (proc "alsa-snd-pcm-hw-free!")
		      (msg ($snd-strerror err))
		      (obj pcm)))))))
   
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-set-params! ...                                  */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-set-params! pcm::alsa-snd-pcm . rest)
   
   (define (check-error arg err)
      (when (<fx err 0)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-pcm-hw-set-params!")
		   (msg (if (keyword? arg)
			    (string-append (keyword->string arg)
			       ": " ($snd-strerror err))
			    ($snd-strerror err)))
		   (obj pcm)))))

   (let (($hw::$snd-pcm-hw-params ($bgl-snd-pcm-hw-params-malloc))
	 (rate 0)
	 (bufsize 0))
      (unwind-protect
	 (with-access::alsa-snd-pcm pcm ($builtin)
	    ($snd-pcm-hw-params-any $builtin $hw)
	    (let loop ((rest rest))
	       (when (pair? rest)
		  (when (null? (cdr rest))
		     (raise (instantiate::&alsa-error
			       (proc "alsa-snd-pcm-hw-set-params!")
			       (msg (format "Missing value for param \"~a\"" (car rest)))
			       (obj pcm))))
		  (check-error
		     (car rest)
		     (case (car rest)
			((:rate-resample)
			 ($snd-pcm-hw-params-set-rate-resample!
			    $builtin $hw (cadr rest)))
			((:access)
			 ($snd-pcm-hw-params-set-access!
			    $builtin $hw (symbol->access (cadr rest))))
			((:format)
			 ($snd-pcm-hw-params-set-format!
			    $builtin $hw (symbol->format (cadr rest))))
			((:channels)
			 ($snd-pcm-hw-params-set-channels!
			    $builtin $hw (cadr rest)))
			((:rate)
			 (set! rate (cadr rest))
			 ($snd-pcm-hw-params-set-rate!
			    $builtin $hw (cadr rest) 0))
			((:rate-near)
			 (let ((r ($bgl-snd-pcm-hw-params-set-rate-near!
				     $builtin $hw (cadr rest))))
			    (set! rate r)
			    (unless (=fx r (cadr rest))
			       (raise (instantiate::&alsa-error
					 (proc "alsa-snd-pcm-hw-set-params!")
					 (msg (format "Rate doesn't match (requested ~aHz, get ~aHz" (cadr rest) r))
					 (obj pcm))))
			    r))
			((:buffer-size)
			 ($snd-pcm-hw-params-set-buffer-size!
			    $builtin $hw (cadr rest)))
			((:buffer-size-near)
			 ($bgl-snd-pcm-hw-params-set-buffer-size-near!
			    $builtin $hw (cadr rest)))
			((:buffer-size-near-ratio)
			 (set! bufsize
			    ($bgl-snd-pcm-hw-params-set-buffer-size-near!
			       $builtin $hw (/fx rate (cadr rest))))
			 bufsize)
			((:period-size)
			 ($snd-pcm-hw-params-set-period-size!
			    $builtin $hw (cadr rest) 0))
			((:buffer-time-near)
			 ($bgl-snd-pcm-hw-params-set-buffer-time-near!
			    $builtin $hw (cadr rest)))
			((:period-size-near)
			 ($bgl-snd-pcm-hw-params-set-period-size-near!
			    $builtin $hw (cadr rest)))
			((:period-size-near-ratio)
			 ($bgl-snd-pcm-hw-params-set-period-size-near!
			    $builtin $hw (/fx bufsize (cadr rest))))
			(else
			 (raise (instantiate::&alsa-error
				   (proc "alsa-snd-pcm-hw-set-params!")
				   (msg (format "Unknown parameter \"~a\"" (car rest)))
				   (obj pcm)))
			 0)))
		  (loop (cddr rest))))
	    ($snd-pcm-hw-params $builtin $hw))
	 ($bgl-snd-pcm-hw-params-free $hw))
      #unspecified))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-test-params? ...                                 */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-test-params? pcm::alsa-snd-pcm . rest)
   (let (($hw::$snd-pcm-hw-params ($bgl-snd-pcm-hw-params-malloc)))
      (unwind-protect
	 (with-access::alsa-snd-pcm pcm ($builtin)
	    ($snd-pcm-hw-params-any $builtin $hw)
	    (let loop ((rest rest))
	       (if (null? rest)
		   #t
		   (case (car rest)
		      ((:access)
		       (unless ($snd-pcm-hw-params-test-access?
				$builtin $hw (symbol->access (cadr rest)))
			  (loop (cddr rest))))
		      ((:format)
		       (unless ($snd-pcm-hw-params-test-format?
				$builtin $hw (symbol->format (cadr rest)))
			  (loop (cddr rest))))
		      ((:channels)
		       (unless ($snd-pcm-hw-params-test-channels?
				$builtin $hw (cadr rest))
			  (loop (cddr rest))))
		      ((:rate)
		       (unless ($snd-pcm-hw-params-test-rate?
				$builtin $hw (cadr rest) 0)
			  (loop (cddr rest))))
		      (else
		       (loop (cddr rest)))))))
	 ($bgl-snd-pcm-hw-params-free $hw))))
		       
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-params-get-buffer-size ...                       */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-params-get-buffer-size pcm::alsa-snd-pcm)
   
   (define (check-error err)
      (when (<fx err 0)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-pcm-hw-params-get-buffer-size")
		   (msg ($snd-strerror err))
		   (obj pcm)))))
   
   (with-access::alsa-snd-pcm pcm ($builtin)
      (check-error ($bgl-snd-pcm-hw-params-get-buffer-size $builtin))))
		       
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-params-get-buffer-time ...                       */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-params-get-buffer-time pcm::alsa-snd-pcm)
   
   (define (check-error err)
      (when (<fx err 0)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-pcm-hw-params-get-buffer-time")
		   (msg ($snd-strerror err))
		   (obj pcm)))))

   (with-access::alsa-snd-pcm pcm ($builtin)
      (check-error ($bgl-snd-pcm-hw-params-get-buffer-time $builtin))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-hw-params-get-rates ...                             */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-hw-params-get-rates pcm::alsa-snd-pcm)
   
   (define (check-error err)
      (when (<fx err 0)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-pcm-hw-params-get-rates")
		   (msg ($snd-strerror err))
		   (obj pcm)))))
   
   (with-access::alsa-snd-pcm pcm ($builtin)
      (multiple-value-bind (cur min max)
	 ($bgl-snd-pcm-hw-params-get-rates $builtin)
	 (check-error min)
	 (values cur min max))))
		       
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-sw-set-params! ...                                  */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-sw-set-params! pcm::alsa-snd-pcm . rest)
   
   (define (check-error arg err)
      (when (<fx err 0)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-pcm-sw-set-params!")
		   (msg (if (keyword? arg)
			    (string-append (keyword->string arg)
			       ":" ($snd-strerror err))
			    ($snd-strerror err)))
		   (obj pcm)))))

   (let (($sw::$snd-pcm-sw-params ($bgl-snd-pcm-sw-params-malloc)))
      (unwind-protect
	 (with-access::alsa-snd-pcm pcm ($builtin)
	    ($snd-pcm-sw-params-current $builtin $sw)
	    (let loop ((rest rest))
	       (when (pair? rest)
		  (when (null? (cdr rest))
		     (raise (instantiate::&alsa-error
			       (proc "alsa-snd-pcm-sw-set-params!")
			       (msg (format "Missing value for param \"~a\"" (car rest)))
			       (obj pcm))))
		  (check-error
		     (car rest)
		     (case (car rest)
			((:start-threshold)
			 ($snd-pcm-sw-params-set-start-threshold!
 			    $builtin $sw (cadr rest)))
			((:avail-min)
			 ($snd-pcm-sw-params-set-avail-min!
			    $builtin $sw (cadr rest)))
			(else
			 (raise (instantiate::&alsa-error
				   (proc "alsa-snd-pcm-sw-set-params!")
				   (msg (format "Unknown parameter \"~a\"" (car rest)))
				   (obj pcm)))
			 0)))
		  (loop (cddr rest))))
	    (check-error #f ($snd-pcm-sw-params $builtin $sw)))
	 ($bgl-snd-pcm-sw-params-free $sw))
      #unspecified))
		       
;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-write ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-write pcm::alsa-snd-pcm buffer sz)
   ($bgl-snd-pcm-write pcm buffer sz))

;*---------------------------------------------------------------------*/
;*    alsa-snd-pcm-flush ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-pcm-flush pcm::alsa-snd-pcm)
   ($bgl-snd-pcm-flush pcm))
