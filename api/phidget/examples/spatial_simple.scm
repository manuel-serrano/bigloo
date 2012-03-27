;*=====================================================================*/
;*    .../project/bigloo/api/phidget/examples/spatial_simple.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 27 09:08:28 2012                          */
;*    Last change :  Tue Mar 27 11:13:59 2012 (serrano)                */
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
   (let ((spatial (instantiate::phidget-spatial)))
      (tprint "spatial=" spatial)
      
      (phidget-add-event-listener!
	 spatial "attach" (lambda (e) (tprint "attached: " e)))
      (phidget-add-event-listener!
	 spatial "detach" (lambda (e) (tprint "detached: " e)))
      (phidget-add-event-listener!
	 spatial "error" (lambda (e) (tprint "error: " e)))

      (phidget-add-event-listener!
	 spatial "spatialdata" (lambda (e) (tprint "spatialdata: " e)))

      (phidget-open spatial)
      (phidget-wait-for-attachment spatial 10000)
      (with-access::phidget-spatial spatial (datarate)
	 (set! datarate 400))

      (tprint "spatial=" spatial)
      (print "Press any key to end")
      (read-char)
      
      (phidget-close spatial)))
   
   
