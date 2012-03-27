;*=====================================================================*/
;*    .../project/bigloo/api/phidget/examples/manager_simple.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 11:41:59 2010                          */
;*    Last change :  Wed Sep 22 11:55:02 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Manager simple                                                   */
;*    -------------------------------------------------------------    */
;*    This is a simple example showing how to setup a phidget          */
;*    manager and display a list of the currently connected Phidgets   */
;*    devices to the PC.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module manager_simple
   (library pthread phidget)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (phidget-enable-logging 'verbose)
   (let ((phidget (instantiate::phidget-manager)))
      
      (phidget-add-event-listener!
       phidget "attach" (lambda (e) (tprint "attached: " e)))
      (phidget-add-event-listener!
       phidget "detach" (lambda (e) (tprint "detached: " e)))
      (phidget-add-event-listener!
       phidget "error" (lambda (e) (tprint "error: ")))
				   
      (phidget-manager-open phidget)
      (print "Press any key to end")
      (read-char)
      
      (phidget-manager-close phidget)))
   
