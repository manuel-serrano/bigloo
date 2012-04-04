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
   (let ((aservo (instantiate::phidget-advanced-servo)))
      
      (phidget-add-event-listener!
	 aservo "attach" (lambda (e) (tprint "attached: " e)))
      (phidget-add-event-listener!
	 aservo "detach" (lambda (e) (tprint "detached: " e)))
      (phidget-add-event-listener!
	 aservo "error" (lambda (e) (tprint "error: " e)))
      
      (phidget-add-event-listener!
	 aservo "position" (lambda (e) (tprint "position: " e)))
      
      (phidget-open aservo)
      (phidget-wait-for-attachment aservo 10000)
      
      (tprint "servo=" aservo)
      
      (let ((min-accel (phidget-advanced-servo-acceleration-min aservo 0)))
	 (tprint "min-accel=" min-accel)
	 (phidget-advanced-servo-acceleration-set! aservo 0 (* min-accel 2)))
      (let ((max-vel (phidget-advanced-servo-velocity-max aservo 0)))
	 (tprint "max-vel=" max-vel)
	 (phidget-advanced-servo-velocity-limit-set! aservo 0 (/ max-vel 2)))

      (with-handler
	 (lambda (e)
	    #f)
	 (let ((curr-pos (phidget-advanced-servo-position aservo 0)))
	    (print "Motor: 0 > Current Position: " curr-pos)))
      
      (print "Move to position 40.00 and engage. Press any key to Continue")
      (read-char)
      
      (phidget-advanced-servo-position-set! aservo 0 40.0)
      (phidget-advanced-servo-engaged-set! aservo 0 #t)
      
      (for-each (lambda (p)
		   (printf "Move to position ~a and engage. Press any key to Continue\n" p)
		   (read-char)
		   (phidget-advanced-servo-position-set! aservo 0 p))
	 '(50.0 100.0 150.0 200.0 70.))
      
      (print "Disengage Servo. Press any key to Continue")
      (read-char)
      
      (phidget-advanced-servo-engaged-set! aservo 0 #f)
      
      (print "Press any key to end\n")
      (read-char)
      
      (print "Closing...")
      (phidget-close aservo)))
   
   
