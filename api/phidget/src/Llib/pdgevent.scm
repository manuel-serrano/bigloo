;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgevent.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 15:52:42 2010                          */
;*    Last change :  Tue Mar 27 10:48:56 2012 (serrano)                */
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
	   (export phidget-event-spatialdata-new "bgl_phidget_event_spatialdata_new"))

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
	      ::double ::double ::double)))

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
			(mutex-lock! *phidget-mutex*)
			(let loop ()
			   (condition-variable-wait!
			    *phidget-condv* *phidget-mutex*)
			   ($pdg-invoke-callbacks)
			   (loop))))))
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
      
