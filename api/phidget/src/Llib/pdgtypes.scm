;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgtypes.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 10:06:00 2010                          */
;*    Last change :  Wed Apr  4 09:34:11 2012 (serrano)                */
;*    Copyright   :  2010-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Phidget types                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_types

   (export  (class &phidget-error::&error)
	    (class &phidget-log-error::&io-error)
	    
	    (abstract-class %phidget
	       (%phidget-init))
	    
	    (abstract-class %phidget-event
	       (target::obj read-only))

	    (abstract-class phidget-event::%phidget-event
	       (phidget::%phidget read-only))
	    
	    (class phidget-error-event::%phidget-event
	       (code::int read-only)
	       (message::string read-only))
	    
	    (class phidget-attach-event::phidget-event)
	    (class phidget-detach-event::phidget-event)

	    (class phidget-change-event::%phidget-event
	       (index::int read-only))

	    (class phidget-inputchange-event::phidget-change-event
	       (state::bool read-only))
	    (class phidget-outputchange-event::phidget-change-event
	       (state::bool read-only))
	    (class phidget-sensorchange-event::phidget-change-event
	       (state::int read-only))
	    
	    (class phidget-serverconnect-event::phidget-event)
	    (class phidget-serverdisconnect-event::phidget-event)

	    (class phidget-spatialdata-event::%phidget-event
	       (seconds::int read-only)
	       (microseconds::int read-only)
	       (acx::double read-only)
	       (acy::double read-only)
	       (acz::double read-only)
	       (anx::double read-only)
	       (any::double read-only)
	       (anz::double read-only)
	       (mfx::double read-only)
	       (mfy::double read-only)
	       (mfz::double read-only))

	    (class phidget-servoposition-event::%phidget-event
	       (index::int read-only)
	       (position::double read-only))
	    
	    (class phidget-servovelocity-event::%phidget-event
	       (index::int read-only)
	       (velocity::double read-only))
	    
	    (class phidget-servocurrent-event::%phidget-event
	       (index::int read-only)
	       (current::double read-only))
	    
	    (class phidget-stepperinput-event::phidget-inputchange-event)
	    (class phidget-steppervelocity-event::%phidget-event
	       (index::int read-only)
	       (velocity::double read-only))
	    (class phidget-stepperposition-event::%phidget-event
	       (index::int read-only)
	       (position::llong read-only))
	    (class phidget-steppercurrent-event::%phidget-event
	       (index::int read-only)
	       (current::double read-only))

	    (class phidget-motorcontrolvelocity-event::%phidget-event
	       (index::int read-only)
	       (velocity::double read-only))

	    (class phidget-motorcontrolcurrent-event::%phidget-event
	       (index::int read-only)
	       (current::double read-only))

            (class phidget-encoderinput-event::%phidget-event
               (index::int read-only)
               (state::bool read-only))

            (class phidget-encoderposition-event::%phidget-event
               (index::int read-only)
               (time::int read-only)
               (position::int read-only))

            (class phidget-encoderindex-event::%phidget-event
               (index::int read-only)
               (position::int read-only))

	    (generic %phidget-init ::%phidget)))

;*---------------------------------------------------------------------*/
;*    %phidget-init ::%phidget ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (%phidget-init o::%phidget)
   o)
