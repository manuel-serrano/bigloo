;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/examples/client.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:59:12 2022                          */
;*    Last change :  Fri Apr  1 14:00:31 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An MQTT client example.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mqtt-client
   (library mqtt pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    example                                                          */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((srv (if (null? (cdr argv)) '("localhost" "1883") (string-split (cadr argv) ":")))
	  (host (car srv))
	  (port (string->integer (cadr srv))))
      (let ((sock (make-client-socket host port)))
	 (thread-join!
	    (thread-start-joinable!
	       (instantiate::pthread
		  (body (lambda ()
			   (mqtt-loop (mqtt-make-client sock))))))))))

;*---------------------------------------------------------------------*/
;*    mqtt-loop ...                                                    */
;*---------------------------------------------------------------------*/
(define (mqtt-loop client)
   (lambda ()
      (mqtt-client-loop client
	 (lambda (pk)
	    (with-access::mqtt-control-packet pk (type)
	       (tprint "pk received "
		  (mqtt-control-packet-type-name type))
	       (cond
		  ((=fx type (MQTT-CPT-CONNACK))
		   (mqtt-client-subscribe client "zigbee2mqtt/bridge/devices")
		   (mqtt-client-subscribe client "zigbee2mqtt/+"))
		  ((=fx type (MQTT-CPT-PUBLISH))
		   (with-access::mqtt-publish-packet pk (topic payload)
		      (tprint "PUBLISH " topic " " payload)))))))))
