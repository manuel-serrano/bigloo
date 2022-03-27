;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/examples/client.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:59:12 2022                          */
;*    Last change :  Sun Mar 27 17:41:44 2022 (serrano)                */
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
      (let* ((sock (make-client-socket host port))
	     (client (mqtt-connect sock
			(lambda (pk)
			   (with-access::mqtt-control-packet pk (type)
			      (tprint "pk received "
				 (mqtt-control-packet-type-name type))
			      (cond
				 ((=fx type (MQTT-CPT-CONNACK))
				  (mqtt-write-subscribe-packet (socket-output sock)
				     533 '(("zigbee2mqtt/bridge/devices" . 0)))
				  (mqtt-write-subscribe-packet (socket-output sock)
				     534 '(("zigbee2mqtt/+" . 0))))
				 ((=fx type (MQTT-CPT-PUBLISH))
				  (with-access::mqtt-publish-packet pk (topic payload)
				     (tprint "PUBLISH " topic " " payload)))))))))
	 (with-access::mqtt-client client (%thread)
	    (thread-join! %thread)))))
;* 	 (print "sending connect")                                     */
;* 	 (let ((conn (mqtt-connect sock :username "foo-bar")))         */
;* 	    (print "conn=" conn)                                       */
;* 	    (mqtt-write-subscribe-packet (socket-output sock)          */
;* 	       533 '(("zigbee2mqtt/bridge/devices" . 0)))              */
;* 	    (mqtt-write-publish-packet (socket-output sock)            */
;* 	       #f 0 #f                                                 */
;* 	       "zigbee2mqtt/0x00124b0024c106d9" -1                     */
;* 	       "{\"state\": \"ON\"}")                                  */
;* 	    (socket-close sock)))))                                    */
