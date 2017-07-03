;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/dbus/src/Llib/dbus.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 17:11:45 2009                          */
;*    Last change :  Sun Nov  8 14:39:59 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Direct use of DBUS functions and types                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directive                                                    */
;*---------------------------------------------------------------------*/
(directives
   
   (extern
    (include "dbus/dbus.h")
    (include "bgldbus.h")
    
    ;; dbustype
    (type $dbus-bustype int "DBusBusType")
    (macro $dbus-bus-type-session::$dbus-bustype "DBUS_BUS_SESSION")
    (macro $dbus-bus-type-system::$dbus-bustype "DBUS_BUS_SYSTEM")

    ;; connection
    (type $dbus-connection* void* "DBusConnection *")
    (infix macro $dbus-connection-nil::$dbus-connection* () "0L")
    (macro $dbus-bus-get-private::$dbus-connection*
     ($dbus-bustype) "bgl_dbus_bus_get_private")
    (macro $dbus-connection-get-connected::bool
       ($dbus-connection*) "dbus_connection_get_is_connected")
    (macro $dbus-connection-close::void
       ($dbus-connection*) "dbus_connection_close")
    (macro $dbus-bus-name-has-owner::bool
       ($dbus-connection* string obj) "bgl_dbus_bus_name_has_owner")
    (macro $dbus-bus-release-name::bool
       ($dbus-connection* string obj) "bgl_dbus_bus_release_name")
    ))

;*                                                                     */
;*     (macro $dbus-set-path-connection::obj                           */
;*        ($dbus-bus* string) "setPathConnection")                     */
;*     (macro $dbus-set-interface-connection::obj                      */
;*        ($dbus-bus* string) "setInterfaceConnection")                */
;*     (macro $dbus-connect-remote-bus::$dbus-bus*                     */
;*        (string) "connectRemoteBus")                                 */
;*     (macro $dbus-is-connected::obj                                  */
;*        ($dbus-bus*) "isConnected")                                  */
;*     (macro $dbus-service-name-has-owner::obj                        */
;*        ($dbus-bus* string) "serviceNameHasOwner")                   */
;*     (macro $dbus-handle-service::$dbus-service*                     */
;*        ($dbus-bus* string) "handleService")                         */
;*     (macro $dbus-handle-object::$dbus-object*                       */
;*        ($dbus-service* string string) "handleObject")               */
;*     (macro $dbus-unsubscribe::obj                                   */
;*        (string $dbus-bus*) "unsubscribe")                           */
;*     (macro $dbus-subscribe::obj                                     */
;*        (string $dbus-bus*) "subscribe")                             */
;*     (macro $dbus-release-service-name::obj                          */
;*        ($dbus-bus*) "releaseServiceName")                           */
;*     (macro $dbus-set-service-name::obj                              */
;*        ($dbus-bus* string uint) "setServiceName")))                 */
   
