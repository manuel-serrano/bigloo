;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/src/Llib/client.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:42:42 2022                          */
;*    Last change :  Sun Mar 13 15:20:12 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    MQTT client side                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mqtt_client
   
   (import __mqtt_mqtt)
   
   (export (mqtt-connect ::socket
	      #!key (version "3.1.1") (keep-alive 0)
	      (clientid "bigloomqtt") username password)))

;*---------------------------------------------------------------------*/
;*    mqtt-protocol-version ...                                        */
;*---------------------------------------------------------------------*/
(define (mqtt-protocol-version version)
   (cond
      ((string=? version "5") 5)
      ((string=? version "3.1.1") 4)
      (else (error "mqtt" "Unsupported protocol version" version))))

;*---------------------------------------------------------------------*/
;*    mqtt-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (mqtt-connect sock #!key (version "3.1.1") (keep-alive 0)
	   (clientid "bigloomqtt") username password)
   (let ((version (mqtt-protocol-version version)))
      (mqtt-write-connect-packet (socket-output sock)
	 version keep-alive clientid username password)
      (mqtt-read-connack-packet (socket-input sock) version)))



      
