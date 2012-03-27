;*=====================================================================*/
;*    .../bigloo/api/phidget/examples/interfacekit_simple.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 11:41:59 2010                          */
;*    Last change :  Fri Nov 19 10:54:34 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Interfacekit simple                                              */
;*    -------------------------------------------------------------    */
;*    This simple example simply creates an InterfaceKit handle, hooks */
;*    the event handlers and opens it. It then waits for an            */
;*    InterfaceKit to be attached and waits for events to be fired.    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module interfacekit_simple
   (library pthread phidget)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((ifkit (instantiate::phidget-ifkit)))
      (tprint "ifkit=" ifkit)
      
      (phidget-add-event-listener!
       ifkit "attach" (lambda (e) (tprint "attached: " e)))
      (phidget-add-event-listener!
       ifkit "detach" (lambda (e) (tprint "detached: " e)))

      (phidget-add-event-listener!
       ifkit "inputchange" (lambda (e) (tprint "intput change: " e)))
      (phidget-add-event-listener!
       ifkit "sensorchange" (lambda (e) (tprint "sensor change: " e)))
      (phidget-add-event-listener!
       ifkit "outputchange" (lambda (e) (tprint "output change: " e)))
      
      (phidget-open ifkit)
      (print "Press any key to end")
      (read-char)
      
      (phidget-close ifkit)))
