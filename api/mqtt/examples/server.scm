;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/examples/server.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:59:12 2022                          */
;*    Last change :  Sun Mar 13 19:47:15 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An MQTT server example.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mqtt-server
   (library mqtt pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    example                                                          */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((port (if (pair? (cdr argv)) (string->integer (cadr argv)) 1883))
	  (srv (make-server-socket port)))
      (unwind-protect
	 (let loop ((s (socket-accept srv)))
	    (let ((p (mqtt-read-connect-packet (socket-input s))))
	       (if (isa? p mqtt-connect-packet)
		   (begin
		      (mqtt-interact-client s p)
		      (loop (socket-accept srv)))
		   (socket-close s))))
	 (socket-close srv))))

;*---------------------------------------------------------------------*/
;*    mqtt-interact-client ...                                         */
;*---------------------------------------------------------------------*/
(define (mqtt-interact-client s p)
   (thread-start!
      (instantiate::pthread
	 (body (lambda ()
		  (with-access::mqtt-connect-packet p (version client-id)
		     (tprint "New client connected as " client-id)
		     (tprint "sending CONNACK to " client-id)
		     (mqtt-write-connack-packet (socket-output s) 0)
		     (let loop ()
			(let ((pk (mqtt-read-server-packet (socket-input s) version)))
			   (unless (eof-object? pk)
			      (cond
				 ((isa? pk mqtt-publish-packet)
				  (tprint "Received PUBLISH from " client-id))
				 ((isa? pk mqtt-subscribe-packet)
				  (tprint "Received SUBSCRIBE from " client-id)
				  (with-access::mqtt-subscribe-packet pk (ident payload)
				     (tprint "Sending SUBACK to " client-id)
				     (mqtt-write-suback-packet (socket-output s)
					ident
					(map cdr payload))))
				 (else
				  (tprint "not implemented")))
			      (loop))))
		     (socket-close s)
		     (tprint "client closed " p)))))))
