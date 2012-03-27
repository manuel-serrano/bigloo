;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgphidget.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 16:54:10 2010                          */
;*    Last change :  Tue Mar 27 11:06:24 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Phidget objects                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_phidget
   
   (include "pdg.sch")
   
   (extern (export $make-phidget "bgl_phidget_new"))
   
   (import __phidget_types
	   __phidget
	   __phidget_event)
   
   (export  (class phidget::%phidget
	       ($builtin::$pdg-phidget (default (%$pdg-phidget-nil)))
	       (name::bstring
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-get-device-name
				$builtin)))))
	       (serial-number::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-get-serial-number
				$builtin)))))
	       (device-version::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-get-device-version
				$builtin)))))
	       (device-type::obj
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-get-device-type
				$builtin)))))
	       (device-id::obj
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-get-device-id
				$builtin)))))
	       (server-id::obj
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-get-server-id
				$builtin))))))

	    ($make-phidget::obj ::$pdg-phidget)
	    (%$pdg-phidget-nil)
	    (phidget-open ::phidget #!optional (serial-number -1))
	    (phidget-close ::phidget)
	    (phidget-wait-for-attachment ::phidget ::int)))

;*---------------------------------------------------------------------*/
;*    %$pdg-phidget-nil ...                                            */
;*---------------------------------------------------------------------*/
(define (%$pdg-phidget-nil)
   ($pdg-phidget-nil))

;*---------------------------------------------------------------------*/
;*    $make-phidget ...                                                */
;*---------------------------------------------------------------------*/
(define ($make-phidget phidget::$pdg-phidget)
   (instantiate::phidget
      ($builtin phidget)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget ...                        */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget event proc)
   (with-access::phidget o ($builtin)
      (phidget-return
       ($pdg-phidget-phidget-add-event-listener! $builtin event o proc)
       "phidget-add-event-listener!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-open ...                                                 */
;*---------------------------------------------------------------------*/
(define (phidget-open o::phidget #!optional (serial-number -1))
   (with-access::phidget o ($builtin)
      (phidget-return
       ($pdg-phidget-open $builtin serial-number)
       "phidget-open" o)))

;*---------------------------------------------------------------------*/
;*    phidget-close ...                                                */
;*---------------------------------------------------------------------*/
(define (phidget-close o::phidget)
   (with-access::phidget o ($builtin)
      (phidget-return
       ($pdg-phidget-close $builtin)
       "phidget-close" o)))

;*---------------------------------------------------------------------*/
;*    phidget-wait-for-attachment ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-wait-for-attachment o::phidget ms)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-wait-for-attachment $builtin ms)
	 "phidget-wait-for-attachment" o)))
