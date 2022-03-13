;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/src/Llib/server.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:41:15 2022                          */
;*    Last change :  Sun Mar 13 16:47:45 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    MQTT server side                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mqtt_server
   
   (option (set! *dlopen-init-gc* #t))
   
   (import __mqtt_mqtt)
   
   (export (mqtt-read-connect-packet::obj ::input-port)
	   (mqtt-read-server-packet ip::input-port ::long)))

;*---------------------------------------------------------------------*/
;*    read-connect-payload ...                                         */
;*---------------------------------------------------------------------*/
(define (read-connect-payload ip::input-port pk::mqtt-control-packet)

   (define (has-flags? flags flag)
      (=fx (bit-and flags flag) flag))
   
   (define (read-will-topic ip flags)
      (if (has-flags? flags (MQTT-CONFLAG-WILL-FLAG))
	  (read-utf8 ip)
	  ""))

   (define (read-will-message ip flags)
      (when (has-flags? flags (MQTT-CONFLAG-WILL-FLAG))
	 (read-chars (read-int16 ip) ip)))

   (define (read-username ip flags)
      (if (has-flags? flags (MQTT-CONFLAG-USERNAME-FLAG))
	  (read-utf8 ip)
	  ""))
      
   (define (read-password ip flags)
      (when (has-flags? flags (MQTT-CONFLAG-PASSWORD-FLAG))
	 (read-chars (read-int16 ip) ip)))
      
   (with-trace 'mqtt "read-connect-payload"
      (with-access::mqtt-connect-packet pk (connect-flags
					      client-id will-topic
					      will-message username
					      password)
	 ;; mandatory client identifier
	 (set! client-id (read-utf8 ip))
	 (set! will-topic (read-will-topic ip connect-flags))
	 (set! will-message (read-will-message ip connect-flags))
	 (set! username (read-username ip connect-flags))
	 (set! password (read-password ip connect-flags))
	 (trace-item "client-id=" client-id)
	 (trace-item "will-topic=" will-topic)
	 (trace-item "will-message=" will-message)
	 (trace-item "username=" username)
	 (trace-item "password=" password)
	 pk)))

;*---------------------------------------------------------------------*/
;*    read-connect-variable-header ...                                 */
;*---------------------------------------------------------------------*/
(define (read-connect-variable-header ip::input-port pk::mqtt-control-packet)
   (with-trace 'mqtt "read-connect-variable-header"
      (with-access::mqtt-connect-packet pk (version connect-flags keep-alive
					      properties)
	 (let* ((protocol-name (read-utf8 ip))
		(protocol-version (read-byte ip))
		(conn-flags (read-byte ip))
		(kalive (read-int16 ip)))
	    (trace-item "name=" protocol-name)
	    (trace-item "version="protocol-version)
	    (trace-item "connect-flags=" conn-flags)
	    (trace-item "keep-alive=" kalive)
	    (set! version protocol-version)
	    (set! connect-flags conn-flags)
	    (set! keep-alive kalive)
	    (when (=fx protocol-version 5)
	       (set! properties (read-properties ip)))
	    pk))))

;*---------------------------------------------------------------------*/
;*    mqtt-read-connect-packet ...                                     */
;*---------------------------------------------------------------------*/
(define (mqtt-read-connect-packet ip::input-port)
   (with-trace 'mqtt "mqtt-read-packet"
      (multiple-value-bind (ptype pflags length)
	 (read-fixed-header ip)
	 (if (eof-object? ptype)
	     ptype
	     (begin
		(trace-item "header=" (mqtt-control-packet-type-name ptype)
		   " flags=" pflags)
		(trace-item "length=" length)
		(unless (=fx ptype (MQTT-CPT-CONNECT))
		   (error "mqtt" "CONNECT packet expected"
		      (mqtt-control-packet-type-name ptype)))
		(call-with-input-string (read-chars length ip)
		   (lambda (vip)
		      (let ((packet (instantiate::mqtt-connect-packet
				       (type ptype)
				       (flags pflags))))
			 (read-connect-variable-header vip packet)
			 (read-connect-payload vip packet)
			 packet))))))))

;*---------------------------------------------------------------------*/
;*    mqtt-read-server-packet ...                                      */
;*---------------------------------------------------------------------*/
(define (mqtt-read-server-packet ip::input-port version)
   (with-trace 'mqtt "mqtt-read-server-packet"
      (let ((header (read-byte ip)))
	 (if (eof-object? header)
	     header
	     (let ((ptype (bit-rsh header 4)))
		(trace-item "type=" (mqtt-control-packet-type-name ptype))
		(unread-char! (integer->char header) ip)
		(cond
		   ((=fx ptype (MQTT-CPT-CONNECT))
		    (mqtt-read-connect-packet ip))
		   ((=fx ptype (MQTT-CPT-PUBLISH))
		    (mqtt-read-publish-packet ip version))
		   ((=fx ptype (MQTT-CPT-SUBSCRIBE))
		    (mqtt-read-subscribe-packet ip version))
		   (else
		    (error "mqtt:server" "Illegal packet type"
		       (mqtt-control-packet-type-name ptype)))))))))
