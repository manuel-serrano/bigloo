;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mqtt/src/Llib/server.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 13 06:41:15 2022                          */
;*    Last change :  Wed Apr 27 18:06:08 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    MQTT server side                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mqtt_server

   (library pthread)
   
   (import __mqtt_common)
   
   (export (class mqtt-server
	      (lock read-only (default (make-mutex "mqtt-server")))
	      (socket::socket read-only)
	      (subscriptions::pair-nil (default '()))
	      (retains::pair-nil (default '()))
	      (debug::long (default 0)))
	   
	   (class mqtt-client-conn
	      (sock::socket read-only)
	      (lock read-only (default (make-mutex "mqtt-server-conn")))
	      (version::long read-only)
	      (connpk::mqtt-connect-packet read-only))

	   (mqtt-make-server ::obj #!key (debug 0))
	   (mqtt-server-loop ::mqtt-server ::procedure)
	   (mqtt-read-server-packet ip::input-port ::long)))

;*---------------------------------------------------------------------*/
;*    topic ...                                                        */
;*---------------------------------------------------------------------*/
(define-struct topic name regexp qos)

;*---------------------------------------------------------------------*/
;*    conn-id ...                                                      */
;*---------------------------------------------------------------------*/
(define (conn-id conn::mqtt-client-conn)
   (with-access::mqtt-client-conn conn (sock lock version connpk)
      (with-access::mqtt-connect-packet connpk (client-id)
	 client-id)))

;*---------------------------------------------------------------------*/
;*    flag? ...                                                        */
;*---------------------------------------------------------------------*/
(define (flag? flags flag)
   (=fx (bit-and flags flag) flag))

;*---------------------------------------------------------------------*/
;*    mqtt-make-server ...                                             */
;*---------------------------------------------------------------------*/
(define (mqtt-make-server socket #!key (debug 0))
   (instantiate::mqtt-server
      (socket socket)
      (debug debug)))

;*---------------------------------------------------------------------*/
;*    mqtt-server-loop ...                                             */
;*---------------------------------------------------------------------*/
(define (mqtt-server-loop srv::mqtt-server on)
   (unless (correct-arity? on 2)
      (error "mqtt-server-loop" "wrong procedure" on))
   (with-access::mqtt-server srv (socket)
      (unwind-protect
	 (let loop ()
	    (let* ((sock (socket-accept socket))
		   (pk (mqtt-read-connect-packet (socket-input sock))))
	       (when (isa? pk mqtt-connect-packet)
		  (with-access::mqtt-connect-packet pk (version client-id flags)
		     (let ((conn (instantiate::mqtt-client-conn
				    (sock sock)
				    (version version)
				    (connpk pk))))
			(on 'connect client-id)
			(mqtt-conn-loop srv conn on))))
	       (loop)))
	 (mqtt-server-close srv))))

;*---------------------------------------------------------------------*/
;*    mqtt-server-close ...                                            */
;*---------------------------------------------------------------------*/
(define (mqtt-server-close srv::mqtt-server)
   (with-access::mqtt-server srv (socket lock)
      #f))

;*---------------------------------------------------------------------*/
;*    mqtt-server-debug ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (mqtt-server-debug srv thunk)
   `(with-access::mqtt-server ,srv (debug)
       (when (>fx debug 0)
	  (,thunk))))

;*---------------------------------------------------------------------*/
;*    mqtt-conn-loop ...                                               */
;*---------------------------------------------------------------------*/
(define (mqtt-conn-loop srv::mqtt-server conn::mqtt-client-conn on)
   (with-access::mqtt-client-conn conn (sock lock version connpk)
      (mqtt-server-debug srv
	 (lambda ()
	    (tprint "New client connected as " (conn-id conn))
	    (tprint "sending CONNACK to " (conn-id conn))))
      (mqtt-write-connack-packet (socket-output sock) 0)
      (thread-start!
	 (instantiate::pthread
	    (name "mqtt-server-loop")
	    (body (lambda ()
		     (with-trace 'mqtt "mqtt-connloop"
			(trace-item "connid=" (conn-id conn))
			(let ((ip (socket-input sock))
			      (op (socket-output sock)))
			   (let loop ()
			      (let ((pk (mqtt-read-server-packet ip version)))
				 (if (not (isa? pk mqtt-control-packet))
				     (mqtt-server-will srv on conn)
				     (with-access::mqtt-control-packet pk (type)
					(on 'packet pk)
					(cond
					   ((=fx type (MQTT-CPT-CONNECT))
					    ;; 3.1, error
					    (mqtt-server-will srv on conn)
					    #f)
					   ((=fx type (MQTT-CPT-PUBLISH))
					    ;; 3.3
					    (mqtt-server-publish srv conn on pk)
					    (loop))
					   ((=fx type (MQTT-CPT-PUBACK))
					    ;; 3.4
					    (loop))
					   ((=fx type (MQTT-CPT-PUBREC))
					    ;; 3.5
					    (loop))
					   ((=fx type (MQTT-CPT-SUBSCRIBE))
					    ;; 3.8
					    (mqtt-server-subscribe srv on conn pk)
					    (loop))
					   ((=fx type (MQTT-CPT-UNSUBSCRIBE))
					    ;; 3.10
					    (mqtt-server-unsubscribe srv conn pk)
					    (loop))
					   ((=fx type (MQTT-CPT-PINGREQ))
					    ;; 3.12
					    (mqtt-write-pingresp-packet op)
					    (loop))
					   ((=fx type (MQTT-CPT-DISCONNECT))
					    ;; 3.14
					    #unspecified)
					   (else
					    (loop)))))))
			   (trace-item "closing " (conn-id conn))
			   (with-access::mqtt-server srv (lock subscriptions retains)
			      (synchronize lock
				 (set! subscriptions
				    (filter (lambda (sub)
					       (not (eq? (car sub) conn)))
				       subscriptions))
				 (set! retains
				    (filter (lambda (pub)
					       (not (eq? (car pub) conn)))
				       retains))))
			   (on 'disconnect (conn-id conn))
			   ;; remove all connection subscriptions
			   (socket-close sock)))))))))

;*---------------------------------------------------------------------*/
;*    mqtt-server-will ...                                             */
;*---------------------------------------------------------------------*/
(define (mqtt-server-will srv::mqtt-server on conn::mqtt-client-conn)
   (with-access::mqtt-client-conn conn (connpk)
      (with-access::mqtt-connect-packet connpk (flags will-topic will-message)
	 (when (flag? flags (MQTT-CONFLAG-WILL-FLAG))
	    (let* ((flags (if (flag? flags (MQTT-CONFLAG-WILL-RETAIN))
			      1 0))
		   (qos (bit-rsh
			   (bit-or
			      (bit-and flags (MQTT-CONFLAG-WILL-QOSL))
			      (bit-and flags (MQTT-CONFLAG-WILL-QOSH)))
			   3))
		   (pk (instantiate::mqtt-publish-packet
			  (type (MQTT-CPT-PUBLISH))
			  (topic will-topic)
			  (flags flags)
			  (qos qos)
			  (payload will-message))))
	       (mqtt-server-publish srv conn on pk))))))

;*---------------------------------------------------------------------*/
;*    mqtt-server-publish ...                                          */
;*---------------------------------------------------------------------*/
(define (mqtt-server-publish srv::mqtt-server conn on pk::mqtt-publish-packet)
   (with-trace 'mqtt "mqtt-server-publish"
      (with-access::mqtt-server srv (lock subscriptions retains)
	 (with-access::mqtt-publish-packet pk (flags topic pid)
	    (trace-item "topic=" topic " retain=" (=fx (bit-and flags 1) 1))
	    (when (=fx (bit-and flags 1) 1)
	       ;; 3.3.1.3 RETAIN
	       (synchronize lock
		  (cond
		     ((null? retains)
		      (set! retains (list (cons conn pk))))
		     ((find (lambda (p)
			       (and (eq? (car p) conn)
				    (with-access::mqtt-publish-packet (cdr p) ((t topic))
				       (string=? t topic))))
			 retains)
		      =>
		      (lambda (p)
			 (set-cdr! p pk)))
		     (else
		      (set! retains (cons (cons conn pk) retains))))))
	    ;; qos
	    (with-access::mqtt-client-conn conn (sock)
	       (cond
		  ((=fx (bit-and flags 2) 2)
		   (mqtt-write-puback-packet (socket-output sock) pid -1 '()))
		  ((=fx (bit-and flags 4) 4)
		   (mqtt-write-pubrec-packet (socket-output sock) pid -1 '()))))
	    (for-each (lambda (subscription)
			 (let ((connsub (car subscription))
			       (topics (cdr subscription)))
			    (unless (eq? connsub conn)
			       (mqtt-conn-publish connsub topics on pk))))
	       subscriptions)))))

;*---------------------------------------------------------------------*/
;*    mqtt-conn-publish ...                                            */
;*---------------------------------------------------------------------*/
(define (mqtt-conn-publish conn topics on pk::mqtt-publish-packet)
   (with-trace 'mqtt "mqtt-conn-publish"
      (with-access::mqtt-publish-packet pk (topic payload)
	 (trace-item "conn=" (conn-id conn))
	 (trace-item "topics=" topics)
	 (trace-item "retain=" topic)
	 (for-each (lambda (t)
		      (when (mqtt-topic-match? (topic-regexp t) topic)
			 (with-access::mqtt-client-conn conn (sock connpk)
			    (with-handler
			       (lambda (e)
				  (with-access::mqtt-connect-packet connpk (client-id)
				     (fprintf (current-error-port)
					"Could not publish ~s to client ~a"
					topic
					client-id))
				  (exception-notify e))
			       (begin
				  (mqtt-write-publish-packet
				     (socket-output sock)
				     #f 0 #f topic 0 payload)
				  (when on (on 'publish (cons (conn-id conn) topic))))))))
	    topics))))

;*---------------------------------------------------------------------*/
;*    mqtt-server-subscribe ...                                        */
;*---------------------------------------------------------------------*/
(define (mqtt-server-subscribe srv::mqtt-server on conn pk::mqtt-control-packet)
   
   (define (payload->topic payload)
      (topic (car payload)
	 (topic-filter->regexp (car payload))
	 (cdr payload)))
   
   (with-trace 'mqtt "mqtt-server-subscribe"
      (with-access::mqtt-server srv (lock subscriptions retains)
	 (with-access::mqtt-control-packet pk (payload pid qos)
	    (let ((subtopics (map payload->topic payload)))
	       ;; add the subscription
	       (synchronize lock
		  (trace-item "subscribe=" payload)
		  (trace-item "conn=" (conn-id conn))
		  (let ((cell (assq conn subscriptions)))
		     (if (not cell)
			 (set! subscriptions
			    (cons (cons conn subtopics)
			       subscriptions))
			 (for-each (lambda (payload)
				      (unless (find (lambda (t)
						       (string=? (topic-name t)
							  (car payload)))
						 (cdr cell))
					 (set-cdr! cell
					    (cons (payload->topic payload)
					       (cdr cell)))))
			    payload))))
	       (with-access::mqtt-client-conn conn (sock)
		  (mqtt-write-suback-packet (socket-output sock) pid '()))
	       (for-each (lambda (rt)
			    (mqtt-conn-publish conn subtopics on (cdr rt)))
		  retains))))))

;*---------------------------------------------------------------------*/
;*    mqtt-server-unsubscribe ...                                      */
;*---------------------------------------------------------------------*/
(define (mqtt-server-unsubscribe srv::mqtt-server conn pk::mqtt-control-packet)
   (with-trace 'mqtt "mqtt-server-unsubscribe"
      (with-access::mqtt-server srv (lock subscriptions)
	 (with-access::mqtt-control-packet pk (payload pid)
	    (synchronize lock
	       (let ((cell (assq conn subscriptions)))
		  (when (pair? cell)
		     (set-cdr! cell
			(filter! (lambda (topic)
				    (not (member (topic-name topic) payload)))
			   (cdr cell))))))
	    (with-access::mqtt-client-conn conn (sock)
	       ;; 3.10.4 Response
	       (mqtt-write-unsuback-packet (socket-output sock) pid))))))

;*---------------------------------------------------------------------*/
;*    mqtt-read-server-packet ...                                      */
;*---------------------------------------------------------------------*/
(define (mqtt-read-server-packet ip::input-port version::long)
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
		   ((=fx ptype (MQTT-CPT-UNSUBSCRIBE))
		    (mqtt-read-unsubscribe-packet ip version))
		   ((=fx ptype (MQTT-CPT-PUBREC))
		    (mqtt-read-pubrec-packet ip version))
		   ((=fx ptype (MQTT-CPT-PINGREQ))
		    (mqtt-read-pingreq-packet ip version))
		   ((=fx ptype (MQTT-CPT-DISCONNECT))
		    (mqtt-read-disconnect-packet ip version))
		   (else
		    (error "mqtt:server" "Illegal packet type"
		       (mqtt-control-packet-type-name ptype)))))))))
