;*=====================================================================*/
;*    Author      :  Ludovic Courtès                                   */
;*    Creation    :  Thu Jul 18 11:31:05 CEST 2013                     */
;*    Last change :                                                    */
;*    Copyright   :  2013 Inria                                        */
;*    -------------------------------------------------------------    */
;*    Motor control example                                            */
;*    -------------------------------------------------------------    */
;*    Control motors such as the Phidget Motor Control HC 1064.        */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module motorcontrol
   (library pthread phidget)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (define lock
      (make-mutex))
   (define halted
      (make-condition-variable))

   (let ((mc    (instantiate::phidget-motor-control))
         (index (match-case argv
                            ((?-)
                             0)
                            ((?- ?index)
                             (string->number index))
                            (?-
                             (display "Usage: motor-control MOTOR-INDEX\n"
                                      (current-error-port))
                             (exit 1)))))
     (phidget-open mc)
     (phidget-wait-for-attachment mc 1000)

     ;; Note: We actually need to use the CPhidgetEncoder API to access the
     ;; Phidget 1047 encoders.
     (tprint "mc=" mc " "
        (phidget-motor-control-encoder-count mc)
        " encoders")

     (phidget-motor-control-acceleration-set! mc index 4.)
     (phidget-motor-control-velocity-set! mc index 50.)
     (phidget-motor-control-braking-set! mc index 0.)
     (phidget-add-event-listener! mc "velocity"
        (lambda (event)
           (with-access::phidget-motorcontrolvelocity-event event
                 (index velocity)
              (when (zero? velocity)
                 (condition-variable-signal! halted))
              (printf "motor ~a: velocity = ~5d; current = ~12d~%"
                 index velocity
                 (phidget-motor-control-current mc index)))))
     (sleep 1000000)

     (synchronize lock
        (phidget-motor-control-velocity-set! mc index 0.)
        (condition-variable-wait! halted lock))
     (print "done\n")
     (phidget-motor-control-braking-set! mc index 100.)
     #f))
