;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/src/Llib/client.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:42:42 2022                          */
;*    Last change :  Tue Mar 29 13:08:00 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    MQTT client side                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mqtt_client

   (library pthread)
   
   (import __mqtt_common)
   
   (export (class mqtt-client
	      (sock::socket read-only)
	      (lock read-only (default (make-mutex "mqtt-client-conn")))
	      (version::long read-only)
	      (keep-alive::long read-only)
	      (onpacket::procedure read-only)
	      (%thread (default #f)))

	   (mqtt-connect ::socket ::procedure ::procedure ::procedure
	      #!key (version "3.1.1") (keep-alive 0)
	      (clientid "bigloomqtt") username password)

	   (mqtt-client-close ::mqtt-client)
	   (mqtt-client-publish ::mqtt-client ::bstring ::bstring
	      #!key (retain #f) (qos 0) (dup #f) (pid -1))

	   (mqtt-read-client-packet ip::input-port version::long)))

;*---------------------------------------------------------------------*/
;*    micro-seconds ...                                                */
;*---------------------------------------------------------------------*/
(define (micro-seconds s)
   (*fx s (*fx 1000 1000)))

;*---------------------------------------------------------------------*/
;*    mqtt-protocol-version ...                                        */
;*---------------------------------------------------------------------*/
(define (mqtt-protocol-version version)
   (cond
      ((integer? version) version)
      ((string=? version "5") 5)
      ((string=? version "3.1.1") 4)
      (else (error "mqtt" "Unsupported protocol version" version))))

;*---------------------------------------------------------------------*/
;*    mqtt-connect ...                                                 */
;*---------------------------------------------------------------------*/
(define (mqtt-connect sock onconnect onpacket onclose
	   #!key (version "3.1.1") (keep-alive 0)
	   (clientid "bigloomqtt") username password)
   (unless (correct-arity? onpacket 1)
      (error "mqtt-connect" "wrong procedure" onpacket))
   (let ((version (mqtt-protocol-version version)))
      (mqtt-write-connect-packet (socket-output sock)
	 version keep-alive clientid username password)
      (let ((pk (mqtt-read-connack-packet (socket-input sock) version)))
	 (with-access::mqtt-control-packet pk (flags)
	    (when (=fx flags 0)
	       (let ((client (instantiate::mqtt-client
			      (sock sock)
			      (version version)
			      (keep-alive keep-alive)
			      (onpacket onpacket))))
		  ;; 3.1.2.10 Keep Alive
		  (when (>fx keep-alive 0)
		     (mqtt-client-keep-alive-loop client keep-alive)
		     (input-port-timeout-set! (socket-input sock)
			(micro-seconds keep-alive)))
		  ;; invoke the connack
		  (onconnect pk)
		  ;; client loop
		  (mqtt-client-loop client version onclose)
		  ;; return the server connection
		  client))))))

;*---------------------------------------------------------------------*/
;*    mqtt-client-keep-alive-loop ...                                  */
;*---------------------------------------------------------------------*/
(define (mqtt-client-keep-alive-loop client::mqtt-client keep-alive)
   (with-access::mqtt-client client (sock lock %thread)
      (thread-start!
	 (instantiate::pthread
	    (name "mqtt-client-keep-alive-loop")
	    (body (lambda ()
		     (let ((op (socket-output sock)))
			(let loop ()
			   (sleep (micro-seconds keep-alive))
			   (when (synchronize lock
				    (unless (socket-down? sock)
				       (mqtt-write-pingreq-packet op)
				       #t))
			      (loop))))))))))

;*---------------------------------------------------------------------*/
;*    mqtt-client-loop ...                                             */
;*---------------------------------------------------------------------*/
(define (mqtt-client-loop client::mqtt-client version::long onclose)
   (with-access::mqtt-client client (sock lock onpacket %thread)
      (set! %thread
	 (instantiate::pthread
	    (name "mqtt-client-loop")
	    (body (lambda ()
		     (let ((ip (socket-input sock)))
			(let loop ()
			   (tprint "reading...")
			   (let ((pk (mqtt-read-client-packet ip version)))
			      (tprint "pk=" pk)
			      (if (isa? pk mqtt-control-packet)
				  (begin
				     (onpacket pk)
				     (loop))
				  (begin
				     (socket-close sock)
				     (onclose pk))))))))))
      (thread-start-joinable! %thread)))

;*---------------------------------------------------------------------*/
;*    mqtt-client-close ...                                            */
;*---------------------------------------------------------------------*/
(define (mqtt-client-close cl::mqtt-client)
   (with-access::mqtt-client cl (sock)
      (socket-close sock)))

;*---------------------------------------------------------------------*/
;*    mqtt-client-publish ...                                          */
;*---------------------------------------------------------------------*/
(define (mqtt-client-publish cl::mqtt-client topic::bstring message::bstring
	   #!key (retain #f) (qos 0) (dup #f) (pid -1))
   
   (with-access::mqtt-client cl (sock)
      (mqtt-write-publish-packet (socket-output sock)
	 retain qos dup topic pid message)))

;*---------------------------------------------------------------------*/
;*    mqtt-read-client-packet ...                                      */
;*---------------------------------------------------------------------*/
(define (mqtt-read-client-packet ip::input-port version::long)
   (with-trace 'mqtt "mqtt-read-client-packet"
      (let ((header (read-byte ip)))
	 (tprint "h=" header)
	 (if (eof-object? header)
	     header
	     (let ((ptype (bit-rsh header 4)))
		(trace-item "type=" (mqtt-control-packet-type-name ptype))
		(unread-char! (integer->char header) ip)
		(tprint "ptype=" ptype)
		(cond
		   ((=fx ptype (MQTT-CPT-PUBLISH))
		    ;; 3.3
		    (mqtt-read-publish-packet ip version))
		   ((=fx ptype (MQTT-CPT-UNSUBACK))
		    ;; 3.11
		    (mqtt-read-unsuback-packet ip version))
		   ((=fx ptype (MQTT-CPT-PINGRESP))
		    ;; 3.13
		    (mqtt-read-pingreq-packet ip version))
		   (else
		    (error "mqtt:client" "Illegal packet type"
		       (mqtt-control-packet-type-name ptype)))))))))

