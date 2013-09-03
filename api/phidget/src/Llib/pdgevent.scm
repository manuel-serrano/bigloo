;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgevent.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 15:52:42 2010                          */
;*    Last change :  Sun Nov 18 15:07:31 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Phidget listener machinery (complex because of Phidget           */
;*    multithreading).                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_event

   (library pthread)

   (include "pdg.sch")

   (import __phidget_types
	   __phidget_phidget)

   (extern (export %phidget-lock! "bgl_phidget_lock")
	   (export %phidget-unlock! "bgl_phidget_unlock")
	   (export %phidget-signal "bgl_phidget_signal")
	   (export phidget-event-error-new "bgl_phidget_event_error_new")
	   (export phidget-event-attach-new "bgl_phidget_event_attach_new")
	   (export phidget-event-detach-new "bgl_phidget_event_detach_new")
	   (export phidget-event-inputchange-new "bgl_phidget_event_inputchange_new")
	   (export phidget-event-outputchange-new "bgl_phidget_event_outputchange_new")
	   (export phidget-event-sensorchange-new "bgl_phidget_event_sensorchange_new")
	   (export phidget-event-serverconnect-new "bgl_phidget_event_serverconnect_new")
	   (export phidget-event-serverdisconnect-new "bgl_phidget_event_serverdisconnect_new")
	   (export phidget-event-spatialdata-new "bgl_phidget_event_spatialdata_new")
	   (export phidget-event-servoposition-new "bgl_phidget_event_servoposition_new")
	   (export phidget-event-servovelocity-new "bgl_phidget_event_servovelocity_new")
	   (export phidget-event-servocurrent-new "bgl_phidget_event_servocurrent_new")
	   (export phidget-event-stepperinput-new "bgl_phidget_event_stepperinput_new")
	   (export phidget-event-steppervelocity-new "bgl_phidget_event_steppervelocity_new")
	   (export phidget-event-stepperposition-new "bgl_phidget_event_stepperposition_new")
	   (export phidget-event-steppercurrent-new "bgl_phidget_event_steppercurrent_new")
           (export phidget-event-motorcontrolvelocity-new "bgl_phidget_event_motorcontrolvelocity_new")
	   (export phidget-event-motorcontrolcurrent-new "bgl_phidget_event_motorcontrolcurrent_new")
           (export phidget-event-encoderinput-new "bgl_phidget_event_encoderinput_new")
           (export phidget-event-encoderposition-new "bgl_phidget_event_encoderposition_new")
           (export phidget-event-encoderindex-new "bgl_phidget_event_encoderindex_new"))

   (export (%phidget-thread-init!)
	   (%phidget-lock!)
	   (%phidget-unlock!)
	   (%phidget-signal)
	   
	   (generic phidget-add-event-listener! ::%phidget ::bstring ::procedure)
	   
	   (phidget-event-error-new::obj ::obj ::int ::string)
	   (phidget-event-attach-new::obj ::obj ::$pdg-phidget)
	   (phidget-event-detach-new::obj ::obj ::$pdg-phidget)
	   (phidget-event-inputchange-new::obj ::obj ::int ::bool)
	   (phidget-event-outputchange-new::obj ::obj ::int ::bool)
	   (phidget-event-sensorchange-new::obj ::obj ::int ::int)
	   (phidget-event-serverconnect-new::obj ::obj ::$pdg-phidget)
	   (phidget-event-serverdisconnect-new::obj ::obj ::$pdg-phidget)
	   (phidget-event-spatialdata-new::obj ::obj ::int ::int
	      ::double ::double ::double
	      ::double ::double ::double
	      ::double ::double ::double)
	   (phidget-event-servoposition-new::obj ::obj ::int ::double)
	   (phidget-event-servovelocity-new::obj ::obj ::int ::double)
	   (phidget-event-servocurrent-new::obj ::obj ::int ::double)
	   (phidget-event-stepperinput-new::obj ::obj ::int ::int)
	   (phidget-event-steppervelocity-new::obj ::obj ::int ::double)
	   (phidget-event-stepperposition-new::obj ::obj ::int ::llong)
	   (phidget-event-steppercurrent-new::obj ::obj ::int ::double)
           (phidget-event-motorcontrolvelocity-new::obj ::obj ::int ::double)
	   (phidget-event-motorcontrolcurrent-new::obj ::obj ::int ::double)

           (phidget-event-encoderinput-new::obj ::obj ::int ::bool)
           (phidget-event-encoderposition-new::obj ::obj ::int ::int ::int)
           (phidget-event-encoderindex-new::obj ::obj ::int ::int)))

;*---------------------------------------------------------------------*/
;*    *phidget-mutex* ...                                              */
;*---------------------------------------------------------------------*/
(define *phidget-mutex* (make-mutex 'phidget))
(define *phidget-condv* (make-condition-variable 'phidget))

;*---------------------------------------------------------------------*/
;*    %phidget-lock! ...                                               */
;*---------------------------------------------------------------------*/
(define (%phidget-lock!)
   (mutex-lock! *phidget-mutex*))

;*---------------------------------------------------------------------*/
;*    %phidget-unlock! ...                                             */
;*---------------------------------------------------------------------*/
(define (%phidget-unlock!)
   (mutex-unlock! *phidget-mutex*))

;*---------------------------------------------------------------------*/
;*    %phidget-signal ...                                              */
;*---------------------------------------------------------------------*/
(define (%phidget-signal)
   (condition-variable-signal! *phidget-condv*))

;*---------------------------------------------------------------------*/
;*    *phidget-thread* ...                                             */
;*---------------------------------------------------------------------*/
(define *phidget-thread* #unspecified)

;*---------------------------------------------------------------------*/
;*    phidget-initializedp ...                                         */
;*---------------------------------------------------------------------*/
(define phidget-initializedp #f)

;*---------------------------------------------------------------------*/
;*    %phidget-thread-init! ...                                        */
;*---------------------------------------------------------------------*/
(define (%phidget-thread-init!)
   (unless (isa? *phidget-thread* thread)
      ;; the thread in charge of executing all callbacks
      (set! *phidget-thread*
	 (instantiate::pthread
	    (body (lambda ()
		     (synchronize *phidget-mutex*
			(let loop ()
			   (condition-variable-wait!
			      *phidget-condv* *phidget-mutex*)
			   ($pdg-invoke-callbacks)
			   (loop)))))))
      (thread-start! *phidget-thread*)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ...                                  */
;*---------------------------------------------------------------------*/
(define-generic (phidget-add-event-listener! obj event proc)
   (raise
    (instantiate::&phidget-error
       (proc "phidget-add-event-listener!")
       (msg "event not supported")
       (obj obj))))

;*---------------------------------------------------------------------*/
;*    phidget-event-error-new ...                                      */
;*---------------------------------------------------------------------*/
(define (phidget-event-error-new target code::int msg::string)
   (instantiate::phidget-error-event
      (target target)
      (code code)
      (message msg)))

;*---------------------------------------------------------------------*/
;*    phidget-event-attach-new ...                                     */
;*---------------------------------------------------------------------*/
(define (phidget-event-attach-new target phid::$pdg-phidget)
   (instantiate::phidget-attach-event
      (target target)
      (phidget ($make-phidget phid))))

;*---------------------------------------------------------------------*/
;*    phidget-event-detach-new ...                                     */
;*---------------------------------------------------------------------*/
(define (phidget-event-detach-new target phid::$pdg-phidget)
   (instantiate::phidget-detach-event
      (target target)
      (phidget ($make-phidget phid))))

;*---------------------------------------------------------------------*/
;*    phidget-event-inputchange-new ...                                */
;*---------------------------------------------------------------------*/
(define (phidget-event-inputchange-new target index::int state::bool)
   (instantiate::phidget-inputchange-event
      (target target)
      (index index)
      (state state)))

;*---------------------------------------------------------------------*/
;*    phidget-event-outputchange-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-outputchange-new target index::int state::bool)
   (instantiate::phidget-outputchange-event
      (target target)
      (index index)
      (state state)))

;*---------------------------------------------------------------------*/
;*    phidget-event-sensorchange-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-sensorchange-new target index::int state::int)
   (instantiate::phidget-sensorchange-event
      (target target)
      (index index)
      (state state)))

;*---------------------------------------------------------------------*/
;*    phidget-event-serverconnect-new ...                              */
;*---------------------------------------------------------------------*/
(define (phidget-event-serverconnect-new target phid::$pdg-phidget)
   (instantiate::phidget-serverconnect-event
      (target target)
      (phidget ($make-phidget phid))))

;*---------------------------------------------------------------------*/
;*    phidget-event-serverdisconnect-new ...                           */
;*---------------------------------------------------------------------*/
(define (phidget-event-serverdisconnect-new target phid::$pdg-phidget)
   (instantiate::phidget-serverdisconnect-event
      (target target)
      (phidget ($make-phidget phid))))

;*---------------------------------------------------------------------*/
;*    phidget-event-spatialdata-new ...                                */
;*---------------------------------------------------------------------*/
(define (phidget-event-spatialdata-new target s ms acx acy acz anx any anz mfx mfy mfz)
   (instantiate::phidget-spatialdata-event
      (target target)
      (seconds s)
      (microseconds ms)
      (acx acx)
      (acy acy)
      (acz acz)
      (anx anx)
      (any any)
      (anz anz)
      (mfx mfx)
      (mfy mfy)
      (mfz mfz)))
      
;*---------------------------------------------------------------------*/
;*    phidget-event-servoposition-new ...                              */
;*---------------------------------------------------------------------*/
(define (phidget-event-servoposition-new target index position)
   (instantiate::phidget-servoposition-event
      (target target)
      (index index)
      (position position)))

;*---------------------------------------------------------------------*/
;*    phidget-event-servovelocity-new ...                              */
;*---------------------------------------------------------------------*/
(define (phidget-event-servovelocity-new target index velocity)
   (instantiate::phidget-servovelocity-event
      (target target)
      (index index)
      (velocity velocity)))

;*---------------------------------------------------------------------*/
;*    phidget-event-servocurrent-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-servocurrent-new target index current)
   (instantiate::phidget-servocurrent-event
      (target target)
      (index index)
      (current current)))

;*---------------------------------------------------------------------*/
;*    phidget-event-stepperinput-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-stepperinput-new target index state)
   (instantiate::phidget-stepperinput-event
      (target target)
      (index index)
      (state state)))

;*---------------------------------------------------------------------*/
;*    phidget-event-steppervelocity-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-steppervelocity-new target index velocity)
   (instantiate::phidget-steppervelocity-event
      (target target)
      (index index)
      (velocity velocity)))

;*---------------------------------------------------------------------*/
;*    phidget-event-stepperposition-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-stepperposition-new target index position)
   (instantiate::phidget-stepperposition-event
      (target target)
      (index index)
      (position position)))

;*---------------------------------------------------------------------*/
;*    phidget-event-steppercurrent-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-steppercurrent-new target index current)
   (instantiate::phidget-steppercurrent-event
      (target target)
      (index index)
      (current current)))

;*---------------------------------------------------------------------*/
;*    phidget-event-motorcontrolvelocity-new ...                       */
;*---------------------------------------------------------------------*/
(define (phidget-event-motorcontrolvelocity-new target index velocity)
   (instantiate::phidget-motorcontrolvelocity-event
      (target target)
      (index index)
      (velocity velocity)))

;*---------------------------------------------------------------------*/
;*    phidget-event-motorcontrolcurrent-new ...                        */
;*---------------------------------------------------------------------*/
(define (phidget-event-motorcontrolcurrent-new target index current)
   (instantiate::phidget-motorcontrolcurrent-event
      (target target)
      (index index)
      (current current)))

;*---------------------------------------------------------------------*/
;*    phidget-event-encoderinput-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-encoderinput-new target index state)
   (instantiate::phidget-encoderinput-event
      (target target)
      (index index)
      (state state)))

;*---------------------------------------------------------------------*/
;*    phidget-event-encoderposition-new ...                            */
;*---------------------------------------------------------------------*/
(define (phidget-event-encoderposition-new target index time position)
   (instantiate::phidget-encoderposition-event
      (target target)
      (index index)
      (time time)
      (position position)))

;*---------------------------------------------------------------------*/
;*    phidget-event-encoderindex-new ...                               */
;*---------------------------------------------------------------------*/
(define (phidget-event-encoderindex-new target index position)
   (instantiate::phidget-encoderindex-event
      (target target)
      (index index)
      (position position)))
