;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgaservo.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 27 09:11:24 2012                          */
;*    Last change :  Wed Apr  4 13:05:00 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget Advanced Servo                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_advanced-servo
   
   (include "pdg.sch")

   (extern (export $make-advanced-servo "bgl_advanced_servo_new")
	   (macro $pdg-advanced-servo-builtin&::$pdg-advanced-servo*
	      (::phidget-advanced-servo)
	      "BGL_PHIDGET_ADVANCED_SERVO_BUILTIN"))

   (import __phidget_types
           __phidget_ctypes
	   __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-advanced-servo::phidget
	       (motor-count::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-advanced-servo-get-motor-count
				($pdg-phidget->advanced-servo $builtin) o))))))

      (phidget-advanced-servo-acceleration::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-acceleration-set!
	 ::phidget-advanced-servo ::int ::double)
      (phidget-advanced-servo-acceleration-max::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-acceleration-min::double
	 ::phidget-advanced-servo ::int)
      
      (phidget-advanced-servo-velocity::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-velocity-limit::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-velocity-limit-set!
	 ::phidget-advanced-servo ::int ::double)
      (phidget-advanced-servo-velocity-max::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-velocity-min::double
	 ::phidget-advanced-servo ::int)
      
      (phidget-advanced-servo-position::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-position-set!
	 ::phidget-advanced-servo ::int ::double)
      (phidget-advanced-servo-position-max::double
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-position-min::double
	 ::phidget-advanced-servo ::int)
      
      (phidget-advanced-servo-engaged::bool
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-engaged-set!
	 ::phidget-advanced-servo ::int ::bool)
      
      (phidget-advanced-servo-speed-ramping-on::bool
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-speed-ramping-on-set!
	 ::phidget-advanced-servo ::int ::bool)
      
      (phidget-advanced-servo-stopped::bool
	 ::phidget-advanced-servo ::int)
      
      (phidget-advanced-servo-current::double
	 ::phidget-advanced-servo ::int)

      (phidget-advanced-servo-parameters-set!
	 ::phidget-advanced-servo ::int ::double ::double ::double ::double)

      (phidget-advanced-servo-type::symbol
	 ::phidget-advanced-servo ::int)
      (phidget-advanced-servo-type-set!
	 ::phidget-advanced-servo ::int ::symbol)

      ($make-advanced-servo::obj ::$pdg-advanced-servo)))

;*---------------------------------------------------------------------*/
;*    $make-advanced-servo ...                                         */
;*---------------------------------------------------------------------*/
(define ($make-advanced-servo servo::$pdg-advanced-servo)
   (instantiate::phidget-advanced-servo
      ($builtin ($pdg-advanced-servo->phidget servo))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-advanced-servo)
   (phidget-init!)
   (with-access::phidget-advanced-servo o ($builtin)
      (phidget-return
       ($pdg-advanced-servo-create ($pdg-advanced-servo-builtin& o))
       "phidget-advanced-servo" o)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-advanced-servo ...         */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-advanced-servo event proc)
   (with-access::phidget-advanced-servo o ($builtin)
      (phidget-return
       ($pdg-phidget-advanced-servo-add-event-listener!
	($pdg-phidget->advanced-servo $builtin) event o proc)
       "phidget-add-event-listener!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-acceleration ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-acceleration o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-acceleration
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-acceleration-set! ...                     */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-acceleration-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-acceleration!
	    ($pdg-phidget->advanced-servo $builtin)
	    i d)
	 "phidget-advanced-servo-acceleration-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-acceleration-max ...                      */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-acceleration-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-acceleration-max
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-acceleration-min ...                      */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-acceleration-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-acceleration-min
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-velocity-limit ...                        */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-velocity-limit o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-velocity-limit
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-velocity-limit-set! ...                   */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-velocity-limit-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-velocity-limit!
	    ($pdg-phidget->advanced-servo $builtin)
	    i d)
	 "phidget-advanced-servo-velocity-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-velocity ...                              */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-velocity o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-velocity
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-velocity-max ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-velocity-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-velocity-max
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-velocity-min ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-velocity-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-velocity-min
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-position ...                              */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-position o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-position
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-position-set! ...                         */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-position-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-position!
	    ($pdg-phidget->advanced-servo $builtin)
	    i d)
	 "phidget-advanced-servo-position-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-position-max ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-position-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-position-max
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-position-min ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-position-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-position-min
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-engaged ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-engaged o i)
   (with-access::phidget o ($builtin)
      (pdgbool->bool
	 ($pdg-phidget-advanced-servo-get-engaged
	    ($pdg-phidget->advanced-servo $builtin)
	    i o))))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-engaged-set! ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-engaged-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-engaged!
	    ($pdg-phidget->advanced-servo $builtin)
	    i (if d $pdg-true $pdg-false))
	 "phidget-advanced-servo-engaged-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-speed-ramping-on ...                     */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-speed-ramping-on o i)
   (with-access::phidget o ($builtin)
      (pdgbool->bool
	 ($pdg-phidget-advanced-servo-get-speed-ramping-on
	    ($pdg-phidget->advanced-servo $builtin)
	    i o))))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-speed-ramping-on-set! ...                 */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-speed-ramping-on-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-speed-ramping-on!
	    ($pdg-phidget->advanced-servo $builtin)
	    i (if d $pdg-true $pdg-false))
	 "phidget-advanced-servo-speed-ramping-on-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-stopped ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-stopped o i)
   (with-access::phidget o ($builtin)
      (+fx $pdg-true ($pdg-phidget-advanced-servo-get-stopped
			($pdg-phidget->advanced-servo $builtin)
			i o))))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-current ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-current o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-advanced-servo-get-current
	 ($pdg-phidget->advanced-servo $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-parameters-set! ...                       */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-parameters-set! o i d0 d1 d2 d3)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-parameters!
	    ($pdg-phidget->advanced-servo $builtin)
	    i d0 d1 d2 d3)
	 "phidget-advanced-servo-parameters-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-type ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-type o i)
   (with-access::phidget o ($builtin)
      (servo-type->symbol
	 ($pdg-phidget-advanced-servo-get-servo-type
	    ($pdg-phidget->advanced-servo $builtin)
	    i o))))

;*---------------------------------------------------------------------*/
;*    phidget-advanced-servo-type-set! ...                             */
;*---------------------------------------------------------------------*/
(define (phidget-advanced-servo-type-set! o i t)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-advanced-servo-set-servo-type!
	    ($pdg-phidget->advanced-servo $builtin)
	    i (symbol->servo-type t))
	 "phidget-advanced-servo-type-set!" o)))
