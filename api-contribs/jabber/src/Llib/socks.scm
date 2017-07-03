(module socks
   	(library pthread)
	(export  (socks-connect sock vers nbMethod m . L)
		 (socks-request sock vers cmd atyp addr port port2)
		 (socks-rcv-method sock)
		 (socks-send-select-method sock vers method)
		 (socks-rcv-request sock)
		 (socks-send-response sock vers rep atyp addr port1 port2)
		 )
	)

;;;=====================================================================
;;;                socks connection
;;;=====================================================================

(define (socks-connect sock vers nbMethod m . L)
  (write-byte vers (socket-output sock))
  (write-byte nbMethod  (socket-output sock))
  (write-byte m  (socket-output sock))
   (if (not (null? L))
       (let loop ((x  L))
 	(write-byte (car x) (socket-output sock))
 	(if (not (null? (cdr x)))
 	    (loop (cdr x)))))
  (flush-output-port (socket-output sock))
  (let* ((vers (read-byte  (socket-input sock)))(method (read-byte  (socket-input sock))))
    (list (cons 'vers vers) (cons 'method method)))
  )

;;;=====================================================================
;;;                     send socks request
;;;=====================================================================

(define (socks-request sock vers cmd atyp addr port port2)
  (let ((size (string-length addr)))
    (write-byte vers  (socket-output sock))
    (write-byte cmd (socket-output sock))
    (write-byte #x00 (socket-output sock))
    (write-byte atyp (socket-output sock))
    (write-byte (bit-or size #x00)  (socket-output sock))
    (display addr (socket-output sock))
    (write-byte port  (socket-output sock))
    (write-byte port2  (socket-output sock))
    (flush-output-port (socket-output sock))
    (let* ((ver (read-byte (socket-input sock)))(rep (read-byte (socket-input sock)))(res (read-byte (socket-input sock)))(atyp (read-byte (socket-input sock)))
	   (len (read-byte (socket-input sock)))(addr (read-chars len  (socket-input sock))) (port1 (read-byte (socket-input sock)))(port2 (read-byte (socket-input sock))))
      (list (cons 'vers ver) (cons 'rep rep) (cons 'atyp atyp) (cons 'addr addr) (cons 'port1 port1) (cons 'port2 port2)))))

;;;=====================================================================
;;;                   receive socks method
;;;=====================================================================

(define (socks-rcv-method sock)
  (let* ((vers (read-byte (socket-input sock)))
	(nbMethod (read-byte (socket-input sock)))
	(result (list (cons 'vers vers) (cons 'nbMethod nbMethod)))
	(method '()))
    (do ((i 0 (+ i 1)))
	((= i nbMethod) (cons (cons 'method method) result))
      (let ((m (read-byte (socket-input sock))))
	(set! method (cons m method))))))

;;;=====================================================================
;;;               send selected method
;;;=====================================================================

(define (socks-send-select-method sock vers method)
    (write-byte vers  (socket-output sock))
    (write-byte method (socket-output sock))
    (flush-output-port (socket-output sock)))

;;;=====================================================================
;;;               receive socks request
;;;=====================================================================

(define (socks-rcv-request sock)
  (let* ((vers (read-byte (socket-input sock)))
	(cmd (read-byte (socket-input sock)))
	(rsv (read-byte (socket-input sock)))
	(atyp (read-byte (socket-input sock)))
	(addr-length (read-byte (socket-input sock)))
	(addr (open-output-string)))
    (send-chars (socket-input sock) addr addr-length)
    (let* ((port1  (read-byte (socket-input sock)))
	  (port2  (read-byte (socket-input sock))))
      (list (cons 'vers vers) (cons 'cmd cmd) (cons 'atyp atyp) (cons 'addr (close-output-port addr)) (cons 'port1 port1) (cons 'port2 port2)))))  

;;;=====================================================================
;;;                send socks response
;;;=====================================================================

(define (socks-send-response sock vers rep atyp addr port1 port2)
 (let ((size (string-length addr)))
   (write-byte vers  (socket-output sock))
   (write-byte rep (socket-output sock))
   (write-byte #x00 (socket-output sock))
   (write-byte atyp (socket-output sock))
   (write-byte (bit-or size #x00)  (socket-output sock))
   (display addr (socket-output sock))
   (write-byte port1  (socket-output sock))
   (write-byte port2  (socket-output sock))
   (flush-output-port (socket-output sock))))
