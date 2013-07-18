;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgmotorcontrol.scm */
;*    -------------------------------------------------------------    */
;*    Author      :  Ludovic Courtès                                   */
;*    Creation    :  Tue Mar 27 09:11:24 2012                          */
;*    Last change :  Fri Jul 12 10:37:46 CEST 2013                     */
;*    Copyright   :  2013 Inria                                        */
;*    -------------------------------------------------------------    */
;*    Phidget Motor Control                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_motor-control

   (include "pdg.sch")

   (extern (export $make-motor-control "bgl_motor_control_new")
	   (macro $pdg-motor-control-builtin&::$pdg-motor-control*
	      (::phidget-motor-control)
	      "BGL_PHIDGET_MOTOR_CONTROL_BUILTIN"))

   (import __phidget_types
	   __phidget_ctypes
	   __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-motor-control::phidget
	       (motor-count::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-motor-control-get-motor-count
				($pdg-phidget->motor-control $builtin) o))))))

            (phidget-motor-control-acceleration::double
               ::phidget-motor-control ::int)
            (phidget-motor-control-acceleration-set!
               ::phidget-motor-control ::int ::double)
            (phidget-motor-control-acceleration-max::double
               ::phidget-motor-control ::int)
            (phidget-motor-control-acceleration-min::double
               ::phidget-motor-control ::int)

            (phidget-motor-control-velocity::double
               ::phidget-motor-control ::int)
            (phidget-motor-control-velocity-set!
               ::phidget-motor-control ::int ::double)

            (phidget-motor-control-encoder-count::int
               ::phidget-motor-control)
            (phidget-motor-control-encoder-position::int
               ::phidget-motor-control ::int)
            (phidget-motor-control-encoder-position-set!
               ::phidget-motor-control ::int ::int)

            (phidget-motor-control-braking::double
               ::phidget-motor-control ::int)
            (phidget-motor-control-braking-set!
               ::phidget-motor-control ::int ::double)

            (phidget-motor-control-current::double
               ::phidget-motor-control ::int)

            ($make-motor-control::obj ::$pdg-motor-control)))

;*---------------------------------------------------------------------*/
;*    $make-motor-control ...                                         */
;*---------------------------------------------------------------------*/
(define ($make-motor-control mc::$pdg-motor-control)
   (instantiate::phidget-motor-control
      ($builtin ($pdg-motor-control->phidget mc))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-motor-control)
   (phidget-init!)
   (phidget-return
      ($pdg-motor-control-create ($pdg-motor-control-builtin& o))
      "phidget-motor-control" o))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-motor-control ...          */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-motor-control event proc)
   (with-access::phidget-motor-control o ($builtin)
      (phidget-return
       ($pdg-phidget-motor-control-add-event-listener!
	($pdg-phidget->motor-control $builtin) event o proc)
       "phidget-add-event-listener!" o)))


(define-syntax define-getter
   (syntax-rules ()
      ((_ name dollar-name)
       (define (name o i)
          (with-access::phidget o ($builtin)
             (dollar-name ($pdg-phidget->motor-control $builtin)
                i o))))))

(define-syntax define-setter
   (syntax-rules ()
      ((_ name dollar-name)
       (define (name o i v)
          (with-access::phidget o ($builtin)
             (dollar-name ($pdg-phidget->motor-control $builtin)
                i v))))))

(define-getter phidget-motor-control-acceleration
   $pdg-phidget-motor-control-get-acceleration)
(define-setter phidget-motor-control-acceleration-set!
   $pdg-phidget-motor-control-set-acceleration!)
(define-getter phidget-motor-control-acceleration-max
   $pdg-phidget-motor-control-get-acceleration-max)
(define-getter phidget-motor-control-acceleration-min
   $pdg-phidget-motor-control-get-acceleration-min)
(define-getter phidget-motor-control-velocity
   $pdg-phidget-motor-control-get-velocity)
(define-setter phidget-motor-control-velocity-set!
   $pdg-phidget-motor-control-set-velocity!)
(define-getter phidget-motor-control-encoder-position
   $pdg-phidget-motor-control-get-encoder-position)
(define-setter phidget-motor-control-encoder-position-set!
   $pdg-phidget-motor-control-set-encoder-position!)
(define-getter phidget-motor-control-braking
   $pdg-phidget-motor-control-get-braking)
(define-setter phidget-motor-control-braking-set!
   $pdg-phidget-motor-control-set-braking!)
(define-getter phidget-motor-control-current
   $pdg-phidget-motor-control-get-current)

;*---------------------------------------------------------------------*/
;*    phidget-motor-control-encoder-count ...                          */
;*---------------------------------------------------------------------*/
(define (phidget-motor-control-encoder-count o)
   (with-access::phidget o ($builtin)
      ($pdg-phidget-motor-control-encoder-count
	 ($pdg-phidget->motor-control $builtin) o)))
