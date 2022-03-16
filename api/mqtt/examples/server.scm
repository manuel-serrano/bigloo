;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/examples/server.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:59:12 2022                          */
;*    Last change :  Wed Mar 16 18:25:27 2022 (serrano)                */
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
			      (with-access::mqtt-control-packet pk (type)
				 (tprint "Received "
				    (mqtt-control-packet-type-name type)
				    " from " client-id)
				 (cond
				    ((=fx type (MQTT-CPT-PUBLISH))
				     (with-access::mqtt-publish-packet pk (ident payload topic)
					(when (string=? topic "zigbee2mqtt/0x00124b00246cd6a7")
					   (tprint "SENDING ON")
					   (mqtt-write-publish-packet (socket-output s)
					      #f 0 #f
					      "zigbee2mqtt/0x00124b0024c106d9/set" -1
					      "{\"state\": \"ON\"}"))
					(tprint "  payload=" (string-for-read payload)))
				     (loop))
				    ((=fx type (MQTT-CPT-SUBSCRIBE))
				     (with-access::mqtt-subscribe-packet pk (ident payload)
					(tprint "Sending SUBACK to " client-id)
					(mqtt-write-suback-packet (socket-output s)
					   ident
					   (map cdr payload)))
				     (loop))
				    ((=fx type (MQTT-CPT-PINGREQ))
				     (mqtt-write-pingresp-packet (socket-output s))
				     (loop))
				    ((=fx type (MQTT-CPT-PUBREC))
				     (loop))
				    ((=fx type (MQTT-CPT-DISCONNECT))
				     'close)
				    (else
				     (tprint "not implemented")
				     (loop)))))))
		     (socket-close s)
		     (tprint "client closed " p)))))))
