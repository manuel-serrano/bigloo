;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phone/src/Llib/phone.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 14:07:08 2005                          */
;*    Last change :  Wed Dec  1 08:21:36 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generic phone API                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phone

   (export (abstract-class phone
	      (phone-init)
	      (sensor-ttl::int (default 100)))
	   
	   (generic phone-init ::phone)

	   (generic phone-locales::pair-nil ::phone)
	   (generic phone-current-locale ::phone)
	   (generic phone-current-locale-set! ::phone ::obj)
	   
	   (generic phone-vibrate ::phone ::obj ::obj)
	   (generic phone-vibrate-stop ::phone)

	   (generic phone-sensor-list ::phone)
	   (generic phone-sensor ::phone ::symbol . delay)

	   (generic phone-sms-send ::phone ::bstring ::bstring)

	   (generic phone-contact::pair-nil ::phone)
	   (generic phone-contact-add! ::phone ::obj)
	   (generic phone-contact-update! ::phone ::obj)
	   (generic phone-contact-remove! ::phone ::obj)
	   
	   (generic phone-call-log::pair-nil ::phone . num)))

;*---------------------------------------------------------------------*/
;*    Generic functions                                                */
;*---------------------------------------------------------------------*/
(define-generic (phone-init p::phone))

;*---------------------------------------------------------------------*/
;*    phone-locales ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (phone-locales p::phone))
(define-generic (phone-current-locale p::phone))
(define-generic (phone-current-locale-set! p::phone locale::obj))

;*---------------------------------------------------------------------*/
;*    vibrate                                                          */
;*---------------------------------------------------------------------*/
(define-generic (phone-vibrate p::phone vibration::obj repeat::obj))
(define-generic (phone-vibrate-stop p::phone))

;*---------------------------------------------------------------------*/
;*    sensors                                                          */
;*---------------------------------------------------------------------*/
(define-generic (phone-sensor-list p::phone)
   '())
(define-generic (phone-sensor p::phone t::symbol . delay))

;*---------------------------------------------------------------------*/
;*    sms                                                              */
;*---------------------------------------------------------------------*/
(define-generic (phone-sms-send p::phone no::bstring message::bstring))

;*---------------------------------------------------------------------*/
;*    phone-contact ::phone ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (phone-contact p::phone))
(define-generic (phone-contact-add! p::phone o::obj))
(define-generic (phone-contact-update! p::phone o::obj))
(define-generic (phone-contact-remove! p::phone o::obj))

;*---------------------------------------------------------------------*/
;*    phone-call-log ::phone ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (phone-call-log p::phone . num))
   
