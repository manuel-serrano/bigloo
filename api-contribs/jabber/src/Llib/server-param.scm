(module server-param
	(library pthread)
	(export
	 NB_MAX_CLIENT
	 PORT
	 PORT_SSL
	 FILE_DIR
	 clients
	 serverMessageThread
	 cv-connection
	 cv-remove
	 cv-send	 
	 mutex-send
	 mutex-connection
	 mutex-rm
	 mutex-numClient
	 list-file-name
	 mutex-list-file-name
	 (class Client
	    socket
	    jid
	    serverJabber
	    sasl
	    timer
	    message
	    error
	    subscribe
	    presence
	    rcv-file?
	    file-rcv
	    file-send
	    )))

(define NB_MAX_CLIENT 1000)
(define PORT 5222)
(define PORT_SSL 5223)
(define FILE_DIR "tmp")

(define clients (make-vector NB_MAX_CLIENT #f))
(define serverMessageThread (make-vector NB_MAX_CLIENT #f))
(define mutex-send  (make-vector NB_MAX_CLIENT #f))
(define cv-connection (make-vector NB_MAX_CLIENT #f))
(define mutex-connection  (make-vector NB_MAX_CLIENT #f))
(define cv-remove  (make-vector NB_MAX_CLIENT #f))
(define mutex-rm  (make-vector NB_MAX_CLIENT #f))
(define cv-send  (make-vector NB_MAX_CLIENT #f))
(define mutex-numClient (make-mutex))
(define list-file-name (make-vector NB_MAX_CLIENT '()))
(define mutex-list-file-name (make-vector NB_MAX_CLIENT #f))

(let loop ((i 0))
  (if (< i NB_MAX_CLIENT)
      (begin
	(vector-set! mutex-send i (make-mutex))
	(vector-set! mutex-connection i (make-mutex))
	(vector-set! mutex-rm i (make-mutex))
	(vector-set!  cv-connection i (make-condition-variable))
	(vector-set!  cv-send i (make-condition-variable))
	(vector-set!  cv-remove i (make-condition-variable))
	(vector-set! mutex-list-file-name i (make-mutex))
	(loop (+ i 1)))))
