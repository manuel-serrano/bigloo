;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/src/Llib/client.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:42:42 2022                          */
;*    Last change :  Sat Apr  2 17:00:04 2022 (serrano)                */
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
	      (clientid::bstring read-only)
	      (username::obj (default #f) read-only)
	      (password::obj (default #f) read-only)
	      (%status (default #f)))

	   (mqtt-make-client ::socket
	      #!key (version "3.1.1") (keep-alive 0)
	      (clientid "bigloomqtt") username password)
	   (mqtt-client-connect ::mqtt-client)
	   (mqtt-client-close ::mqtt-client)
	   (mqtt-client-loop ::mqtt-client ::procedure)
	   (mqtt-read-client-packet ip::input-port version::long)

	   (mqtt-client-subscribe ::mqtt-client ::bstring
	      #!key (qos 0) (pid -1))
	   (mqtt-client-publish ::mqtt-client ::bstring ::bstring
	      #!key (retain #f) (qos 0) (dup #f) (pid -1))

	   ))

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
;*    mqtt-make-client ...                                             */
;*---------------------------------------------------------------------*/
(define (mqtt-make-client sock
	   #!key (version "3.1.1") (keep-alive 0)
	   (clientid "bigloomqtt") username password)
   (let* ((version (mqtt-protocol-version version))
	  (client (instantiate::mqtt-client
		    (sock sock)
		    (version version)
		    (keep-alive keep-alive)
		    (clientid clientid)
		    (username username)
		    (password password)
		    (%status "connected"))))
      client))

;*---------------------------------------------------------------------*/
;*    mqtt-client-connect ...                                          */
;*---------------------------------------------------------------------*/
(define (mqtt-client-connect client::mqtt-client)
   (with-access::mqtt-client client (sock version keep-alive clientid
				       username password)
      (mqtt-write-connect-packet (socket-output sock)
	 version keep-alive clientid username password)
      (let ((pk (mqtt-read-connack-packet (socket-input sock) version)))
	 (with-access::mqtt-control-packet pk (flags)
	    (when (=fx flags 0)
	       (let ()
		  ;; 3.1.2.10 Keep Alive
		  (when (>fx keep-alive 0)
		     (mqtt-client-keep-alive-loop client keep-alive)
		     (input-port-timeout-set! (socket-input sock)
			(micro-seconds keep-alive)))
		  ;; return the server connection
		  pk))))))

;*---------------------------------------------------------------------*/
;*    mqtt-client-close ...                                            */
;*---------------------------------------------------------------------*/
(define (mqtt-client-close client::mqtt-client)
   (with-access::mqtt-client client (sock %status lock)
      (synchronize lock
	 (set! %status "close")
	 (socket-close sock))))
   
;*---------------------------------------------------------------------*/
;*    mqtt-client-keep-alive-loop ...                                  */
;*---------------------------------------------------------------------*/
(define (mqtt-client-keep-alive-loop client::mqtt-client keep-alive)
   (with-access::mqtt-client client (sock lock)
      (thread-start!
	 (instantiate::pthread
	    (name "mqtt-client-keep-alive-loop")
	    (body (lambda ()
		     ;; ignore network errors
		     (with-handler
			(lambda (e)
			   #f)
			(let ((op (socket-output sock)))
			   (let loop ()
			      (sleep (micro-seconds keep-alive))
			      (when (synchronize lock
				       (unless (socket-down? sock)
					  (mqtt-write-pingreq-packet op)
					  #t))
				 (loop)))))))))))

;*---------------------------------------------------------------------*/
;*    mqtt-client-loop ...                                             */
;*---------------------------------------------------------------------*/
(define (mqtt-client-loop client::mqtt-client on)
   (unless (correct-arity? on 2)
      (error "mqtt-client-loop" "wrong procedure" on))
   (with-access::mqtt-client client (sock lock version %status)
      (let ((ip (socket-input sock)))
	 (with-handler
	    (lambda (e)
	       (unless (string=? %status "close")
		  (on 'error e)))
	    (let loop ()
	       (let ((pk (mqtt-read-client-packet ip version)))
		  (when (isa? pk mqtt-control-packet)
		     (on 'packet pk)
		     (loop)))))
	 (synchronize lock
	    (unless (string=? %status "close")
	       (on 'error
		  (instantiate::&io-error
		     (proc "mqtt-client-loop")
		     (msg "Illegal input")
		     (obj ip))))))))

;*---------------------------------------------------------------------*/
;*    mqtt-client-subscribe ...                                        */
;*---------------------------------------------------------------------*/
(define (mqtt-client-subscribe cl::mqtt-client topic::bstring
	   #!key (qos 0) (pid -1))
   (with-access::mqtt-client cl (sock)
      (mqtt-write-subscribe-packet (socket-output sock)
	 pid (list (cons topic qos)))))
      
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
	 (if (eof-object? header)
	     header
	     (let ((ptype (bit-rsh header 4)))
		(trace-item "type=" (mqtt-control-packet-type-name ptype))
		(unread-char! (integer->char header) ip)
		(cond
		   ((=fx ptype (MQTT-CPT-PUBLISH))
		    ;; 3.3
		    (mqtt-read-publish-packet ip version))
		   ((=fx ptype (MQTT-CPT-PUBACK))
		    ;; 3.4
		    (mqtt-read-puback-packet ip version))
		   ((=fx ptype (MQTT-CPT-PUBREC))
		    ;; 3.5
		    (mqtt-read-pubrec-packet ip version))
		   ((=fx ptype (MQTT-CPT-SUBACK))
		    ;; 3.9
		    (mqtt-read-suback-packet ip version))
		   ((=fx ptype (MQTT-CPT-UNSUBACK))
		    ;; 3.11
		    (mqtt-read-unsuback-packet ip version))
		   ((=fx ptype (MQTT-CPT-PINGRESP))
		    ;; 3.13
		    (mqtt-read-pingreq-packet ip version))
		   (else
		    (error "mqtt:client" "Illegal packet type"
		       (mqtt-control-packet-type-name ptype)))))))))

