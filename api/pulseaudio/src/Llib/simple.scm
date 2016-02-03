;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pulseaudio/src/Llib/simple.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 26 14:18:50 2016                          */
;*    Last change :  Thu Jan 28 14:33:50 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    PulseAudio simple interface                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pulseaudio_simple
   
   (include "pulseaudio.sch")
   
   (import  __pulseaudio_pulseaudio)
   
   (export  (class pulseaudio-simple
	       (pulseaudio-simple-init)
	       ($builtin::$pa-simple (default $pa-simple-nil))
	       (srvname read-only (default #f))
	       (name::bstring read-only (default "bigloo"))
	       (stream read-only (default #f))
	       (rate::long read-only (default 44000))
	       (channels::long read-only (default 2))
	       (bps::int (default 0))
	       (format::symbol read-only (default 's16)))
	    
	    (pulseaudio-simple-init ::pulseaudio-simple)
	    (pulseaudio-simple-state ::pulseaudio-simple)
	    (pulseaudio-simple-close ::pulseaudio-simple)
	    (pulseaudio-simple-get-latency::double ::pulseaudio-simple)
	    (inline pulseaudio-simple-write::long ::pulseaudio-simple ::bstring ::long)
	    (inline pulseaudio-simple-drain ::pulseaudio-simple)
	    (inline pulseaudio-simple-flush ::pulseaudio-simple)))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-init ...                                       */
;*---------------------------------------------------------------------*/
(define (pulseaudio-simple-init o)
   (with-access::pulseaudio-simple o ($builtin)
      (with-access::pulseaudio-simple o (srvname name stream rate channels format bps)
	 (let ((fmt (if (>fx bps 0)
			(bps-format bps)
			(multiple-value-bind (format fbps)
			   (pa-format format)
			   (set! bps fbps)
			   format))))
	    (set! $builtin
	       ($bgl-pa-simple-new
		  (if (string? srvname) srvname $string-null)
		  name
		  (if (string? stream) stream $string-null)
		  fmt rate channels)))
	 o)))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-state ...                                      */
;*---------------------------------------------------------------------*/
(define (pulseaudio-simple-state o::pulseaudio-simple)
   (with-access::pulseaudio-simple o ($builtin)
      (if ($pa-simple-nil? $builtin)
	  'closed
	  'opened)))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-close ...                                      */
;*---------------------------------------------------------------------*/
(define (pulseaudio-simple-close o::pulseaudio-simple)
   (with-access::pulseaudio-simple o ($builtin)
      ($pa-simple-free $builtin)
      o))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-get-latency ...                                */
;*---------------------------------------------------------------------*/
(define (pulseaudio-simple-get-latency o::pulseaudio-simple)
   (with-access::pulseaudio-simple o ($builtin)
      (let ((err::int 0))
	 (let ((f ($pa-simple-get-latency $builtin (&int err))))
	    (when (<fx err 0)
	       (pulseaudio-error "pulseaudio-simple-get-latency"
		  ($pa_strerror err)
		  err))
	    f))))

;*---------------------------------------------------------------------*/
;*    bps-format ...                                                   */
;*---------------------------------------------------------------------*/
(define (bps-format bps)
   (case bps
      ((8) $PA_SAMPLE_ALAW)
      ((16) $PA_SAMPLE_S16LE)
      ((24) $PA_SAMPLE_S24_32LE)
      ((32) $PA_SAMPLE_S32LE)
      (else $PA_SAMPLE_INVALID)))

;*---------------------------------------------------------------------*/
;*    pa-format ...                                                    */
;*---------------------------------------------------------------------*/
(define (pa-format format)
   (case format
      ((s16 s16le)
       (values $PA_SAMPLE_S16LE 16))
      ((s16be)
       (values $PA_SAMPLE_S16BE 16))
      ((s24 s24le)
       (values $PA_SAMPLE_S24LE 24))
      ((s24be)
       (values $PA_SAMPLE_S24BE 24))
      ((s32 s32le)
       (values $PA_SAMPLE_S32LE 32))
      ((s32be)
       (values $PA_SAMPLE_S32BE 32))
      ((s24-3le)
       (values $PA_SAMPLE_S24_32LE 24))
      ((s24-3be)
       (values $PA_SAMPLE_S24_32BE 24))
      ((float-le)
       (values 32 $PA_SAMPLE_FLOAT32LE))
      ((float-be)
       (values $PA_SAMPLE_FLOAT32BE 32))
      ((u8)
       (values $PA_SAMPLE_U8 8))
      ((s8)
       (values $PA_SAMPLE_ALAW 8))
      (else
       (values $PA_SAMPLE_INVALID 0))))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-write ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (pulseaudio-simple-write::long o::pulseaudio-simple outbuf size)
   (with-access::pulseaudio-simple o ($builtin)
      (let ((err::int 0))
	 ($pa-simple-write $builtin outbuf size (&int err))
	 (when (<fx err 0)
	    (pulseaudio-error "pulseaudio-simple-write"
	       ($pa_strerror err)
	       err))
	 size)))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-drain ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (pulseaudio-simple-drain o::pulseaudio-simple)
   (with-access::pulseaudio-simple o ($builtin)
      (let ((err::int 0))
	 ($pa-simple-drain $builtin (&int err))
	 (when (<fx err 0)
	    (pulseaudio-error "pulseaudio-simple-drain"
	       ($pa_strerror err)
	       err)))))

;*---------------------------------------------------------------------*/
;*    pulseaudio-simple-flush ...                                      */
;*---------------------------------------------------------------------*/
(define-inline (pulseaudio-simple-flush o::pulseaudio-simple)
   (with-access::pulseaudio-simple o ($builtin)
      (let ((err::int 0))
	 ($pa-simple-flush $builtin (&int err))
	 (when (<fx err 0)
	    (pulseaudio-error "pulseaudio-simple-flush"
	       ($pa_strerror err)
	       err)))))
