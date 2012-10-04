;*=====================================================================*/
;*    .../bigloo/api/phidget/examples/advancedservo_simple.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 27 09:08:28 2012                          */
;*    Last change :  Wed Apr  4 13:07:57 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Spatial simple                                                   */
;*    -------------------------------------------------------------    */
;*    This simple example creates an spatial handle, initializes it,   */
;*    hooks the event handlers and opens it.  It then waits for a      */
;*    spatial to be attached and waits for events to be fired.         */
;*    We preset the data rate to 16ms, but can be set higher           */
;*    (eg. 200) in order to slow down the events to make them more     */
;*    visible.                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module spatial_simple
   (library pthread phidget)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((stepper (instantiate::phidget-stepper)))

      (phidget-add-event-listener!
	 stepper "attach" (lambda (e) (tprint "attached: " e)))
      (phidget-add-event-listener!
	 stepper "detach" (lambda (e) (tprint "detached: " e)))
      (phidget-add-event-listener!
	 stepper "error" (lambda (e) (tprint "error: " e)))

      ;; Basic handlers
      (for-each
	 (lambda (event)
	    (phidget-add-event-listener!
	       stepper event (lambda (e) (tprint event ": " e))))
	 (list "input" "velocity" "position" "current"))

      (phidget-open stepper)

      (tprint "Waiting for the Phidget to be attached....")
      (phidget-wait-for-attachment stepper 10000)

      (tprint "stepper=" stepper)

      (let ((min-accel (phidget-stepper-acceleration-min stepper 0)))
	 (tprint "min-accel=" min-accel)
	 (phidget-stepper-acceleration-set! stepper 0 (* min-accel 2)))
      (let ((max-vel (phidget-stepper-velocity-max stepper 0)))
	 (tprint "max-vel=" max-vel)
	 (phidget-stepper-velocity-limit-set! stepper 0 (/ max-vel 2)))

      (let ((curr-pos (phidget-stepper-current-position stepper 0)))
	 (print "Motor: 0 > Current Position: " curr-pos))

      (print "Press any key to continue")
      (read-char)

      (print "Set to position 0 and engage. Press any key to Continue")
      (read-char)

      (phidget-stepper-current-position-set! stepper 0 #l0)
      (phidget-stepper-engaged-set! stepper 0 #t)

      (print "Move to position 200. Press any key to Continue")
      (read-char)
      (phidget-stepper-target-position-set! stepper 0 #l200)

      (printf "Move to position -1200. Press any key to Continue")
      (read-char)
      (phidget-stepper-target-position-set! stepper 0 #l-1200)

      (print "Reset to 0 and disengage motor. Press any key to Continue")
      (read-char)

      (phidget-stepper-target-position-set! stepper 0 #l0)

      (let loop ()
	 (unless (phidget-stepper-stopped stepper 0)
	    (sleep (* 100 1000))
	    (loop)))

      (phidget-stepper-engaged-set! stepper 0 #f)

      (print "Press any key to end\n")
      (read-char)

      (print "Closing...")
      (phidget-close stepper)))


