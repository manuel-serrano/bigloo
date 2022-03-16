;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/examples/client.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:59:12 2022                          */
;*    Last change :  Wed Mar 16 21:22:29 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    An MQTT client example.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mqtt-client
   (library mqtt)
   (main main))

;*---------------------------------------------------------------------*/
;*    example                                                          */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((srv (if (null? (cdr argv)) '("localhost" "1883") (string-split (cadr argv) ":")))
	  (host (car srv))
	  (port (string->integer (cadr srv))))
      (let ((sock (make-client-socket host port)))
	 (print "sending connect")
	 (let ((conn (mqtt-connect sock :username "foo-bar")))
	    (print "conn=" conn)
	    (mqtt-write-publish-packet (socket-output sock)
	       #f 0 #f
	       "zigbee2mqtt/0x00124b0024c106d9" -1
	       "{\"state\": \"ON\"}")
	    (socket-close sock)))))
