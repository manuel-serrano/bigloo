;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/mixer.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 12 10:19:55 2011                          */
;*    Last change :  Tue Jul 12 10:41:24 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    ALSA mixer wrapper                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_mixer

   (include "alsa.sch")

   (import  __alsa_alsa)

   (export  (class alsa-snd-mixer::alsa-object
	       ($builtin::$snd-mixer read-only (default (%$snd-mixer-nil))))
	    
	    (%$snd-mixer-nil::$snd-mixer)

	    (alsa-snd-mixer-open ::alsa-snd-mixer)
	    (alsa-snd-mixer-close ::alsa-snd-mixer)
	    (alsa-snd-mixer-attach ::alsa-snd-mixer ::bstring)
	    (alsa-snd-mixer-load ::alsa-snd-mixer)
	    (alsa-snd-mixer-get-count ::alsa-snd-mixer)))

;*---------------------------------------------------------------------*/
;*    %$snd-mixer-nil ...                                              */
;*---------------------------------------------------------------------*/
(define (%$snd-mixer-nil)
   ($snd-mixer-nil))

;*---------------------------------------------------------------------*/
;*    alsa-snd-mixer-open ...                                          */
;*---------------------------------------------------------------------*/
(define (alsa-snd-mixer-open o::alsa-snd-mixer)
   (with-access::alsa-snd-mixer o ($builtin)
      (if ($snd-mixer-nil? $builtin)
	  (let ((err ($bgl-snd-mixer-open o)))
	     (if (<fx err 0)
		 (raise (instantiate::&alsa-error
			   (proc "alsa-snd-mixer-open")
			   (msg ($snd-strerror err))
			   (obj o)))
		 o))
	  (raise (instantiate::&alsa-error
		    (proc "alsa-snd-mixer-open")
		    (msg "mixer device already open")
		    (obj o))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-mixer-close ...                                         */
;*---------------------------------------------------------------------*/
(define (alsa-snd-mixer-close mixer::alsa-snd-mixer)
   (with-access::alsa-snd-mixer mixer ($builtin)
      (unless ($snd-mixer-nil? $builtin)
	 ($snd-mixer-close $builtin))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-mixer-attach ...                                        */
;*---------------------------------------------------------------------*/
(define (alsa-snd-mixer-attach mixer::alsa-snd-mixer card::bstring)
   (with-access::alsa-snd-mixer mixer ($builtin)
      (alsa-snd-mixer-open mixer)
      (let ((err ($snd-mixer-attach $builtin card)))
	 (when (<fx err 0)
	    (raise (instantiate::&alsa-error
		      (proc "alsa-snd-mixer-attach")
		      (msg ($snd-strerror err))
		      (obj mixer)))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-mixer-load ...                                          */
;*---------------------------------------------------------------------*/
(define (alsa-snd-mixer-load mixer::alsa-snd-mixer)
   (with-access::alsa-snd-mixer mixer ($builtin)
      (when ($snd-mixer-nil? $builtin)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-mixer-load")
		   (msg "mixer not open")
		   (obj mixer))))
      (let ((err ($snd-mixer-load $builtin)))
	 (when (<fx err 0)
	    (raise (instantiate::&alsa-error
		      (proc "alsa-snd-mixer-load")
		      (msg ($snd-strerror err))
		      (obj mixer)))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-mixer-get-count ...                                     */
;*---------------------------------------------------------------------*/
(define (alsa-snd-mixer-get-count mixer::alsa-snd-mixer)
   (with-access::alsa-snd-mixer mixer ($builtin)
      (when ($snd-mixer-nil? $builtin)
	 (raise (instantiate::&alsa-error
		   (proc "alsa-snd-mixer-load")
		   (msg "mixer not open")
		   (obj mixer))))
      (let ((count ($snd-mixer-get-count $builtin)))
	 (if (<fx count 0)
	     (raise (instantiate::&alsa-error
		       (proc "alsa-snd-mixer-load")
		       (msg ($snd-strerror count))
		       (obj mixer)))
	     count))))

