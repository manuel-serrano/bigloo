;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/src/Llib/mqtt.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 12 14:57:58 2001                          */
;*    Last change :  Sun Mar 27 09:06:50 2022 (serrano)                */
;*    Copyright   :  2001-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MQTT protocol                                                    */
;*    -------------------------------------------------------------    */
;*    See https://mqtt.org/mqtt-specification/                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mqtt_mqtt
   
   (option (set! *dlopen-init-gc* #t))
   
   (import __mqtt_common
	   __mqtt_server
	   __mqtt_client))
