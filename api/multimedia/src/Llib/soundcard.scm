;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/src/Llib/soundcard.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 12 08:19:14 2005                          */
;*    Last change :  Tue Nov 15 19:12:22 2011 (serrano)                */
;*    Copyright   :  2005-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The SOUNDCARD mixer API implementation                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-soundcard

   (extern ($open-mixer::obj (::string) "bgl_open_mixer")
	   ($close-mixer::obj (::obj) "bgl_close_mixer")
	   ($mixer-volume::int (::obj ::int ::bool) "bgl_mixer_read_vol")
	   ($mixer-volume-set!::obj (::obj ::int ::int) "bgl_mixer_write_vol")
	   ($mixer-device-name::string (::obj ::int) "bgl_mixer_dev_name")
	   ($mixer-device-number::int (::obj) "bgl_mixer_dev_num")
	   ($mixer-device-support?::bool (::obj ::int) "bgl_mixer_devp"))
	   
   (import __multimedia-mixer)
   
   (export (soundcard-open ::soundcard)
	   (class soundcard::mixer
	      (soundcard-open)
	      (device::bstring read-only)
	      (%devices::pair-nil (default '()))
	      (%mixer (default #unspecified)))))

;*---------------------------------------------------------------------*/
;*    soundcard-open ...                                               */
;*---------------------------------------------------------------------*/
(define (soundcard-open m::soundcard)
   (cond-expand
      (bigloo-c
       (with-access::soundcard m (device %mixer %devices devices)
	  ;; open the mixer
	  (let ((mix ($open-mixer device)))
	     (set! %mixer mix)
	     ;; compute the list of devices supported by this soundcard
	     (set! %devices (map! (lambda (n)
				     (cons ($mixer-device-name mix n) n))
				  (iota ($mixer-device-number mix) 0)))
	     (set! devices (map car %devices)))
	  m))
      (else
       (error "mixer-open" "Only supported by C back-end" m))))

;*---------------------------------------------------------------------*/
;*    mixer-close ::soundcard ...                                      */
;*---------------------------------------------------------------------*/
(define-method (mixer-close m::soundcard)
   (cond-expand
      (bigloo-c
       (with-access::soundcard m (%mixer)
	  ;; open the mixer
	  ($close-mixer %mixer)))
      (else
       (error "mixer-close" "Only supported by C back-end" m))))

;*---------------------------------------------------------------------*/
;*    soundcard-find-device ...                                        */
;*---------------------------------------------------------------------*/
(define (soundcard-find-device m::soundcard dev)
   (cond-expand
      (bigloo-c
       ;;;
       (with-access::soundcard m (%devices)
	  (let ((c (assoc dev %devices)))
	     (if (pair? c)
		 (cdr c)
		 (error 'soundcard "Unknown device" dev)))))
      (else
       (error "soundcard-find-device" "Only supported by C back-end" m))))

;*---------------------------------------------------------------------*/
;*    mixer-volume-set! ::soundcard ...                                */
;*---------------------------------------------------------------------*/
(define-method (mixer-volume-set! m::soundcard dev::bstring l::int r::int)
   (cond-expand
      (bigloo-c
       ;;;
       (with-access::soundcard m (%mixer)
	  (let ((vol (+fx (*fx 256 r) l)))
	     ($mixer-volume-set! %mixer (soundcard-find-device m dev) vol))))
      (else
       (error "mixer-volume-set!" "Only supported by C back-end" m))))

;*---------------------------------------------------------------------*/
;*    mixer-volume-get ::soundcard ...                                 */
;*---------------------------------------------------------------------*/
(define-method (mixer-volume-get m::soundcard dev::bstring)
   (cond-expand
      (bigloo-c
       ;;;
       (with-access::soundcard m (%mixer)
	  (let ((vol ($mixer-volume %mixer (soundcard-find-device m dev) #t)))
	     (values (remainderfx vol 256) (quotientfx vol 256)))))
      (else
       (error "mixer-volume-set!" "Only supported by C back-end" m))))

