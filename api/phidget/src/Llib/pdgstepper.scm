;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgstepper.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Cyprien Nicolas                                   */
;*    Creation    :  Thu Oct  4 17:58:10 2012                          */
;*    Last change :  Thu Oct  4 18:00:42 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget stepper.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_stepper

   (include "pdg.sch")

   (extern (export $make-stepper "bgl_stepper_new")
           (macro $pdg-stepper-builtin&::$pdg-stepper*
	      (::phidget-stepper)
	      "BGL_PHIDGET_STEPPER_BUILTIN"))

   (import __phidget_types
           __phidget_ctypes
           __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-stepper::phidget
	       (input-count::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-stepper-get-input-count
				($pdg-phidget->stepper $builtin) o)))))
	       (motor-count::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-stepper-get-motor-count
				($pdg-phidget->stepper $builtin) o))))))

      (phidget-stepper-input-state::int
	 ::phidget-stepper ::int)

      (phidget-stepper-acceleration::double
	 ::phidget-stepper ::int)
      (phidget-stepper-acceleration-set!
	 ::phidget-stepper ::int ::double)
      (phidget-stepper-acceleration-max::double
	 ::phidget-stepper ::int)
      (phidget-stepper-acceleration-min::double
	 ::phidget-stepper ::int)

      (phidget-stepper-velocity::double
	 ::phidget-stepper ::int)
      (phidget-stepper-velocity-limit::double
	 ::phidget-stepper ::int)
      (phidget-stepper-velocity-limit-set!
	 ::phidget-stepper ::int ::double)
      (phidget-stepper-velocity-max::double
	 ::phidget-stepper ::int)
      (phidget-stepper-velocity-min::double
	 ::phidget-stepper ::int)

      (phidget-stepper-target-position::llong
	 ::phidget-stepper ::int)
      (phidget-stepper-target-position-set!
	 ::phidget-stepper ::int ::llong)
      (phidget-stepper-current-position::llong
	 ::phidget-stepper ::int)
      (phidget-stepper-current-position-set!
	 ::phidget-stepper ::int ::llong)
      (phidget-stepper-position-max::llong
	 ::phidget-stepper ::int)
      (phidget-stepper-position-min::llong
	 ::phidget-stepper ::int)

      (phidget-stepper-current-limit::double
	 ::phidget-stepper ::int)
      (phidget-stepper-current-limit-set!
	 ::phidget-stepper ::int ::double)
      (phidget-stepper-current::double
	 ::phidget-stepper ::int)
      (phidget-stepper-current-max::double
	 ::phidget-stepper ::int)
      (phidget-stepper-current-min::double
	 ::phidget-stepper ::int)

      (phidget-stepper-engaged::bool
	 ::phidget-stepper ::int)
      (phidget-stepper-engaged-set!
	 ::phidget-stepper ::int ::bool)

      (phidget-stepper-stopped::bool
	 ::phidget-stepper ::int)

      ($make-stepper::obj ::$pdg-stepper)))

;*---------------------------------------------------------------------*/
;*    $make-stepper ...                                                */
;*---------------------------------------------------------------------*/
(define ($make-stepper servo::$pdg-stepper)
   (instantiate::phidget-stepper
      ($builtin ($pdg-stepper->phidget servo))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-stepper)
   (phidget-init!)
   (with-access::phidget-stepper o ($builtin)
      (phidget-return
       ($pdg-stepper-create ($pdg-stepper-builtin& o))
       "phidget-stepper" o)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-stepper ...                */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-stepper event proc)
   (with-access::phidget-stepper o ($builtin)
      (phidget-return
       ($pdg-phidget-stepper-add-event-listener!
	($pdg-phidget->stepper $builtin) event o proc)
       "phidget-add-event-listener!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-input-state ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-input-state o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-input-state
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-acceleration ...                                 */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-acceleration o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-acceleration
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-acceleration-set! ...                            */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-acceleration-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-stepper-set-acceleration!
	    ($pdg-phidget->stepper $builtin)
	    i d)
	 "phidget-stepper-acceleration-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-acceleration-max ...                             */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-acceleration-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-acceleration-max
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-acceleration-min ...                             */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-acceleration-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-acceleration-min
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-velocity-limit ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-velocity-limit o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-velocity-limit
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-velocity-limit-set! ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-velocity-limit-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-stepper-set-velocity-limit!
	    ($pdg-phidget->stepper $builtin)
	    i d)
	 "phidget-stepper-velocity-limit-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-velocity ...                                     */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-velocity o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-velocity
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-velocity-max ...                                 */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-velocity-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-velocity-max
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-velocity-min ...                                 */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-velocity-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-velocity-min
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-target-position ...                              */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-target-position o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-target-position
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-target-position-set! ...                         */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-target-position-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-stepper-set-target-position!
	    ($pdg-phidget->stepper $builtin)
	    i d)
	 "phidget-stepper-target-position-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current-position ...                             */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current-position o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-current-position
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current-position-set! ...                        */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current-position-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-stepper-set-current-position!
	    ($pdg-phidget->stepper $builtin)
	    i d)
	 "phidget-stepper-current-position-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-position-max ...                                 */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-position-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-position-max
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-position-min ...                                 */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-position-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-position-min
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current-limit ...                                */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current-limit o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-current-limit
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current-limit-set! ...                           */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current-limit-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-stepper-set-current-limit!
	    ($pdg-phidget->stepper $builtin)
	    i d)
	 "phidget-stepper-current-limit-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current ...                                      */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-current
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current-max ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current-max o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-current-max
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-current-min ...                                  */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-current-min o i)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-stepper-get-current-min
	 ($pdg-phidget->stepper $builtin)
	 i o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-engaged ...                                      */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-engaged o i)
   (with-access::phidget o ($builtin)
      (pdgbool->bool
	 ($pdg-phidget-stepper-get-engaged
	    ($pdg-phidget->stepper $builtin)
	    i o))))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-engaged-set! ...                                 */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-engaged-set! o i d)
   (with-access::phidget o ($builtin)
      (phidget-return
	 ($pdg-phidget-stepper-set-engaged!
	    ($pdg-phidget->stepper $builtin)
	    i (if d $pdg-true $pdg-false))
	 "phidget-stepper-engaged-set!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-stepper-stopped ...                                      */
;*---------------------------------------------------------------------*/
(define (phidget-stepper-stopped o i)
   (with-access::phidget o ($builtin)
      (pdgbool->bool
	 ($pdg-phidget-stepper-get-stopped
	    ($pdg-phidget->stepper $builtin)
	    i o))))
