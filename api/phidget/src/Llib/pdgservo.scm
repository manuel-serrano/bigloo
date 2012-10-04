;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgservo.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 27 09:11:24 2012                          */
;*    Last change :  Wed Apr  4 11:39:23 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget Servo                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_servo
   
   (include "pdg.sch")

   (extern (export $make-servo "bgl_servo_new")
	   (macro $pdg-servo-builtin&::$pdg-servo* (::phidget-servo)
		  "BGL_PHIDGET_SERVO_BUILTIN"))

   (import __phidget_types
	   __phidget_ctypes
	   __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-servo::phidget
	       (motor-count::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-servo-get-motor-count
				($pdg-phidget->servo $builtin) o))))))

      (phidget-servo-position::double ::phidget-servo ::int)
      (phidget-servo-position-set! ::phidget-servo ::int ::double)
      (phidget-servo-position-max::double ::phidget-servo ::int)
      (phidget-servo-position-min::double ::phidget-servo ::int)
      
      (phidget-servo-engaged::bool ::phidget-servo ::int)
      (phidget-servo-engaged-set! ::phidget-servo ::int ::bool)
      
      (phidget-servo-parameters-set! ::phidget-servo ::int ::double ::double ::double)

      (phidget-servo-type::symbol ::phidget-servo ::int)
      (phidget-servo-type-set! ::phidget-servo ::int ::symbol)

      ($make-servo::obj ::$pdg-servo)))

;*---------------------------------------------------------------------*/
;*    $make-servo ...                                                  */
;*---------------------------------------------------------------------*/
(define ($make-servo servo::$pdg-servo)
   (instantiate::phidget-servo
      ($builtin ($pdg-servo->phidget servo))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-servo)
   (phidget-init!)
   (with-access::phidget-servo o ($builtin)
      (phidget-return
       ($pdg-servo-create ($pdg-servo-builtin& o))
       "phidget-servo" o)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-servo ...                  */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-servo event proc)
   (with-access::phidget-servo o ($builtin)
      (phidget-return
       ($pdg-phidget-servo-add-event-listener!
	($pdg-phidget->servo $builtin) event o proc)
       "phidget-add-event-listener!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-position ...                                       */
;*---------------------------------------------------------------------*/
(define (phidget-servo-position o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-servo-get-position
	 ($pdg-phidget->servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-position-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-servo-position-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-servo-set-position!
	    ($pdg-phidget->servo $builtin)
	    i d)
	 "phidget-servo-position-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-position-max ...                                   */
;*---------------------------------------------------------------------*/
(define (phidget-servo-position-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-servo-get-position-max
	 ($pdg-phidget->servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-position-min ...                                   */
;*---------------------------------------------------------------------*/
(define (phidget-servo-position-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-servo-get-position-min
	 ($pdg-phidget->servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-engaged ...                                        */
;*---------------------------------------------------------------------*/
(define (phidget-servo-engaged o i)
   (with-access::phidget o ($builtin)
      (pdgbool->bool
	 ($pdg-phidget-servo-get-engaged
	    ($pdg-phidget->servo $builtin)
	    i o))))

;*---------------------------------------------------------------------*/
;*    phidget-servo-engaged-set! ...                                   */
;*---------------------------------------------------------------------*/
(define (phidget-servo-engaged-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-servo-set-engaged!
	    ($pdg-phidget->servo $builtin)
	    i (if d $pdg-true $pdg-false))
	 "phidget-servo-engaged-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-parameters-set! ...                                */
;*---------------------------------------------------------------------*/
(define (phidget-servo-parameters-set! o i d0 d1 d2)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-servo-set-parameters!
	    ($pdg-phidget->servo $builtin)
	    i d0 d1 d2)
	 "phidget-servo-parameters-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-servo-type ...                                           */
;*---------------------------------------------------------------------*/
(define (phidget-servo-type o i)
   (with-access::phidget o ($builtin)
      (servo-type->symbol
	 ($pdg-phidget-servo-get-servo-type
	    ($pdg-phidget->servo $builtin)
	    i o))))

;*---------------------------------------------------------------------*/
;*    phidget-servo-type-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (phidget-servo-type-set! o i t)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-servo-set-servo-type!
	    ($pdg-phidget->servo $builtin)
	    i (symbol->servo-type t))
	 "phidget-servo-type-set!" o)))


