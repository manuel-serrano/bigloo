;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/dbus/src/Llib/dbus.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 16:58:59 2009                          */
;*    Last change :  Sun Nov  8 14:42:52 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Bigloo DBUS binding                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __dbus
   
   (include "dbus.sch")

   (extern (export $dbus-error "bgl_dbus_error"))
   
   (export (class dbus-connection
	      ($builtin::$dbus-connection* read-only (default ($dbus-connection-nil))))
	   
	   (class &dbus-error::&error)
	   ($dbus-error ::string ::string ::string)
	   
	   (dbus-connect::dbus-connection ::symbol
					  #!key
					  (path "/")
					  (interface "bgl.dbus"))
	   (dbus-close ::dbus-connection)
	   (dbus-connected?::bool ::dbus-connection)
	   (dbus-bus-name-has-owner?::bool ::dbus-connection ::bstring)
	   (dbus-bus-name-set!::bool ::dbus-connection ::bstring)
	   (dbus-bus-name-release!::bool ::dbus-connection ::bstring)
	   ))
;* 	   (dbus-connect-remote-bus::dbus-bus str::bstring)            */
;* 	   (dbus-connected?::bool ::dbus-bus)                          */
;* 	   (dbus-set-path-connection! ::dbus-bus ::bstring)            */
;* 	   (dbus-set-interface-connection! ::dbus-bus ::bstring)))     */

;*---------------------------------------------------------------------*/
;*    $dbus-error ...                                                  */
;*---------------------------------------------------------------------*/
(define ($dbus-error proc msg obj)
   (raise
    (instantiate::&dbus-error
       (proc (string->symbol proc))
       (msg msg)
       (obj obj)))
   0)

;*---------------------------------------------------------------------*/
;*    dbus-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (dbus-connect type #!key (path "/") (interface "bgl.dbus"))
   (unless (memq type '(session system))
      (raise
       (instantiate::&dbus-error
	  (proc 'dbus-connect)
	  (msg "Argument type must be either 'session or 'system")
	  (obj type))))
   (let* ((cbus ($dbus-bus-get-private
		 (if (eq? type 'session)
		     $dbus-bus-type-session
		     $dbus-bus-type-system)))
	  (bus (instantiate::dbus-connection ($builtin cbus))))
;* 	  (dbus-set-path-connection! bus path)                         */
;* 	  (dbus-set-interface-connection! bus interface)               */
      bus))

;*---------------------------------------------------------------------*/
;*    dbus-close ...                                                   */
;*---------------------------------------------------------------------*/
(define (dbus-close bus::dbus-connection)
   ($dbus-connection-close (dbus-connection-$builtin bus)))

;*---------------------------------------------------------------------*/
;*    dbus-connected? ...                                              */
;*---------------------------------------------------------------------*/
(define (dbus-connected? bus::dbus-connection)
   ($dbus-connection-get-connected (dbus-connection-$builtin bus)))

;*---------------------------------------------------------------------*/
;*    dbus-bus-name-has-owner? ...                                     */
;*---------------------------------------------------------------------*/
(define (dbus-bus-name-has-owner? bus::dbus-connection name::bstring)
   ($dbus-bus-name-has-owner (dbus-connection-$builtin bus) name bus))

;*---------------------------------------------------------------------*/
;*    dbus-bus-name-set! ...                                           */
;*---------------------------------------------------------------------*/
(define (dbus-bus-name-set! bus::dbus-connection name::bstring)
   ($dbus-bus-set-name (dbus-connection-$builtin bus) name bus))

;*---------------------------------------------------------------------*/
;*    dbus-bus-name-relase! ...                                        */
;*---------------------------------------------------------------------*/
(define (dbus-bus-name-release! bus::dbus-connection name::bstring)
   ($dbus-bus-release-name (dbus-connection-$builtin bus) name bus))

;*---------------------------------------------------------------------*/
;* {*    dbus-connect-remote-bus ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define (dbus-connect-remote-bus str)                               */
;*    (instantiate::dbus-connection ($builtin ($dbus-connect-remote-bus str)))) */

;* {*---------------------------------------------------------------------*} */
;* {*    dbus-set-path-connection! ...                                    *} */
;* {*---------------------------------------------------------------------*} */
;* (define (dbus-set-path-connection! bus path)                        */
;*    ($dbus-set-path-connection (dbus-bus-$builtin bus) path))         */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    dbus-set-interface-connection! ...                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define (dbus-set-interface-connection! bus inter)                  */
;*    ($dbus-set-interface-connection (dbus-bus-$builtin bus) inter))   */

