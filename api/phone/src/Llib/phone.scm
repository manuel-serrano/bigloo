;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/phone/src/Llib/phone.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 14:07:08 2005                          */
;*    Last change :  Tue Oct 12 11:50:10 2010 (serrano)                */
;*    Copyright   :  2005-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generic phone API                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phone

   (export (abstract-class phone
	      (phone-init))
	   
	   (generic phone-init ::phone)
	   (generic phone-vibrate ::phone ::obj)))

;*---------------------------------------------------------------------*/
;*    Generic functions                                                */
;*---------------------------------------------------------------------*/
(define-generic (phone-init p::phone))

(define-generic (phone-vibrate p::phone duration::obj))
