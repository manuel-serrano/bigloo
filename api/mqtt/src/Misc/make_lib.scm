;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/mqtt/src/Misc/make_lib.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Mar 29 14:12:52 2022 (serrano)                */
;*    Copyright   :  2001-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file and the _e library        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mqtt_makelib
   
   (import __mqtt_common
	   __mqtt_server
	   __mqtt_client)
   
   (eval   (export-all)
      
           (class mqtt-control-packet)
	   (class mqtt-connect-packet)
	   (class mqtt-publish-packet)
	   (class mqtt-client)))

