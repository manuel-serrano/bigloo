;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/alsa/src/Llib/control.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 12 09:10:53 2011                          */
;*    Last change :  Mon Mar  4 08:20:28 2019 (serrano)                */
;*    Copyright   :  2011-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    ALSA control wrapper                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_control

   (include "alsa.sch")

   (import  __alsa_alsa)

   (export  (class alsa-snd-ctl::alsa-object
	       ($builtin::$snd-ctl read-only (default (%$snd-ctl-nil)))
	       (card::bstring read-only (default "default"))
	       (mode::symbol read-only (default 'default)))

	    (class alsa-snd-ctl-card-info::alsa-object
	       (ctl::alsa-snd-ctl read-only)
	       (card::int read-only (default 0))
	       (id::bstring read-only (default ""))
	       (driver::bstring read-only (default ""))
	       (name::bstring read-only (default ""))
	       (longname::bstring read-only (default ""))
	       (mixername::bstring read-only (default ""))
	       (components::bstring read-only (default "")))

	    (class alsa-snd-ctl-rawmidi-info::alsa-object
	       (ctl::alsa-snd-ctl read-only)
	       (card::int read-only (default 0))
	       (id::bstring read-only (default ""))
	       (name::bstring read-only (default ""))
	       (device::int read-only (default 0))
	       (subdevice::int read-only (default 0))
	       (subdevicescount::int read-only (default 0)))

	    (%$snd-ctl-nil::$snd-ctl)

	    (alsa-snd-ctl-open ::alsa-snd-ctl)
	    (alsa-snd-ctl-close ::alsa-snd-ctl)

	    (alsa-snd-ctl-rawmidi-devices::pair-nil ::alsa-snd-ctl)))
					 
;*---------------------------------------------------------------------*/
;*    %$snd-ctl-nil ...                                                */
;*---------------------------------------------------------------------*/
(define (%$snd-ctl-nil)
   ($snd-ctl-nil))

;*---------------------------------------------------------------------*/
;*    %$snd-ctl-card-info-nil ...                                      */
;*---------------------------------------------------------------------*/
(define (%$snd-ctl-card-info-nil)
   ($snd-ctl-card-info-nil))

;*---------------------------------------------------------------------*/
;*    alsa-init ::alsa-snd-ctl-card-info ...                           */
;*---------------------------------------------------------------------*/
(define-method (alsa-init o::alsa-snd-ctl-card-info)
   ($bgl-snd-ctl-card-info-init o))

;*---------------------------------------------------------------------*/
;*    alsa-init ::alsa-snd-ctl-rawmidi-info ...                        */
;*---------------------------------------------------------------------*/
(define-method (alsa-init o::alsa-snd-ctl-rawmidi-info)
   ($bgl-snd-ctl-rawmidi-info-init o))

;*---------------------------------------------------------------------*/
;*    alsa-snd-ctl-open ...                                            */
;*---------------------------------------------------------------------*/
(define (alsa-snd-ctl-open o::alsa-snd-ctl)
   (with-access::alsa-snd-ctl o ($builtin mode card)
      (if ($snd-ctl-nil? $builtin)
	  (let ((err ($bgl-snd-ctl-open o card (symbol->ctl-mode mode))))
	     (if (<fx err 0)
		 (raise (instantiate::&alsa-error
			   (proc "alsa-snd-ctl-open")
			   (msg ($snd-strerror err))
			   (obj card)))
		 o))
	  (raise (instantiate::&alsa-error
		    (proc "alsa-snd-ctl-open")
		    (msg "ctl device already open")
		    (obj o))))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-ctl-close ...                                           */
;*---------------------------------------------------------------------*/
(define (alsa-snd-ctl-close ctl::alsa-snd-ctl)
   (with-access::alsa-snd-ctl ctl ($builtin)
      (unless ($snd-ctl-nil? $builtin)
	 ($snd-ctl-close $builtin))))

;*---------------------------------------------------------------------*/
;*    alsa-snd-ctl-rawmidi-devices ...                                 */
;*---------------------------------------------------------------------*/
(define (alsa-snd-ctl-rawmidi-devices ctl::alsa-snd-ctl)
   (let loop ((d -1))
      (let ((dev ($bgl-snd-ctl-rawmidi-next-device ctl d)))
	 (if (>=fx dev 0)
	     (cons (instantiate::alsa-snd-ctl-rawmidi-info
		      (ctl ctl)
		      (device dev))
		(loop (+fx d 1)))
	     '()))))
   
;*---------------------------------------------------------------------*/
;*    symbol->ctl-mode ...                                             */
;*---------------------------------------------------------------------*/
(define (symbol->ctl-mode::int s::symbol)
   (case s
      ((default) 0)
      ((nonblock) $snd-ctl-nonblock)
      ((async) $snd-ctl-async)
      (else (raise (instantiate::&alsa-error
		      (proc "alsa-ctl")
		      (msg "Unknown mode")
		      (obj s))))))

