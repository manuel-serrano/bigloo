;*=====================================================================*/
;*    .../bigloo/api/gstreamer/src/Llib/gstplugin-bglports.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 23 07:05:21 2008                          */
;*    Last change :  Sun Jan 25 17:46:29 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Utility functions for the Bigloo ports gstreamer plugin.         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_plugin_bglports
   
   (extern (export $gst-register-port! "bglgst_register_port")
	   (export $gst-unregister-port! "bglgst_unregister_port")
	   (export $gst-open-input-file "bglgst_open_input_file"))
   
   (export ($gst-register-port! ::obj)
	   ($gst-unregister-port! ::obj)
	   ($gst-open-input-file ::string)))

;*---------------------------------------------------------------------*/
;*    *registered-ports*                                               */
;*---------------------------------------------------------------------*/
(define *registered-ports* '())

;*---------------------------------------------------------------------*/
;*    $gst-register-port! ...                                          */
;*---------------------------------------------------------------------*/
(define ($gst-register-port! port)
   (set! *registered-ports* (cons port *registered-ports*)))

;*---------------------------------------------------------------------*/
;*    $gst-unregister-port! ...                                        */
;*---------------------------------------------------------------------*/
(define ($gst-unregister-port! port)
   (set! *registered-ports* (remq! port *registered-ports*)))

;*---------------------------------------------------------------------*/
;*    $gst-open-input-file ...                                         */
;*---------------------------------------------------------------------*/
(define ($gst-open-input-file obj)
   (open-input-file obj))

