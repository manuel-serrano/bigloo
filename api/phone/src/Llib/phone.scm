;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phone/src/Llib/phone.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 14:07:08 2005                          */
;*    Last change :  Sun Oct 17 19:03:40 2010 (serrano)                */
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
	   
	   (generic phone-vibrate ::phone ::obj ::obj)
	   (generic phone-vibrate-stop ::phone)

	   (generic phone-sensor-list ::phone)
	   (generic phone-sensor ::phone ::symbol . delay)

	   (generic phone-sms-send ::phone ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    Generic functions                                                */
;*---------------------------------------------------------------------*/
(define-generic (phone-init p::phone))

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
