;*=====================================================================*/
;*    .../project/bigloo/api/phidget/examples/servo_simple.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 27 09:08:28 2012                          */
;*    Last change :  Tue Apr  3 16:40:10 2012 (serrano)                */
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
   (let ((servo (instantiate::phidget-servo)))
      (tprint "servo=" servo)
      (phidget-wait-for-attachment servo 10000)

      (tprint "servo=" servo)
      (print "Press any key to end")
      (read-char)
      
      (phidget-close servo)))
   
   
