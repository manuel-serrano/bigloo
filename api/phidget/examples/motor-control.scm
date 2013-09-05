;*=====================================================================*/
;*    Author      :  Ludovic Courtès                                   */
;*    Creation    :  Thu Jul 18 11:31:05 CEST 2013                     */
;*    Last change :                                                    */
;*    Copyright   :  2013 Inria                                        */
;*    -------------------------------------------------------------    */
;*    Motor control example                                            */
;*    -------------------------------------------------------------    */
;*    Control motors such as the Phidget Motor Control HC 1064,        */
;*    and check encoders such as the Phidget 1047.                     */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module motorcontrol
   (library pthread phidget)
   (main main))

(define (parse-arguments argv)
   ;; Return the chosen motor index and encoder index.
   (match-case argv
      ((?-)
       (values 0 0))
      ((?- ?index)
       (values (string->number index) 0))
      ((?- ?motor-index ?encoder-index)
       (values (string->number motor-index) (string->number encoder-index)))
      (?-
       (display "Usage: motor-control [MOTOR-INDEX [ENCODER-INDEX]]\n"
          (current-error-port))
       (exit 1))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (define lock
      (make-mutex))
   (define halted
      (make-condition-variable))

   (receive (motor-index encoder-index)
      (parse-arguments argv)
      (let ((mc (instantiate::phidget-motor-control))
            (e  (instantiate::phidget-encoder)))
         (phidget-open mc)
         (phidget-wait-for-attachment mc 1000)

         ;; Attempt to use an encoder, if available.
         (with-handler
            (lambda (x)
               (print "x=" x)
               (tprint "encoder not found, not using it")
               (set! e #f))
            (begin
               (phidget-open e)
               (phidget-wait-for-attachment e 1000)
               (phidget-encoder-enable! e encoder-index)
               (tprint "encoder enabled? "
                  (phidget-encoder-enabled? e encoder-index))))

         ;; Note: We actually need to use the CPhidgetEncoder API to access the
         ;; Phidget 1047 encoders.
         (tprint "mc=" mc " "
            (phidget-motor-control-encoder-count mc)
            " on-motor encoders; "
            (if e
                (with-access::phidget-encoder e (encoder-count)
                   encoder-count)
                -1)
            " encoders")

         (phidget-motor-control-acceleration-set! mc motor-index 4.)
         (phidget-motor-control-velocity-set! mc motor-index 50.)
         (phidget-motor-control-braking-set! mc motor-index 0.)
         (phidget-add-event-listener! mc "velocity"
            (lambda (event)
               (with-access::phidget-motorcontrolvelocity-event event
                     (index velocity)
                  (when (zero? velocity)
                     (condition-variable-signal! halted))
                  (printf "motor ~a: velocity = ~5d; current = ~12d; position = ~5d~%"
                     motor-index velocity
                     (phidget-motor-control-current mc index)
                     (if e
                         (phidget-encoder-position e encoder-index)
                         -1)))))
         (sleep 1000000)

         (synchronize lock
            (phidget-motor-control-velocity-set! mc motor-index 0.)
            (condition-variable-wait! halted lock))
         (tprint "done")
         (phidget-motor-control-braking-set! mc motor-index 100.)
         #f)))
