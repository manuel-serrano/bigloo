(module server-dialog
	(library web
		 ssl
		 pthread)
	(export (open-session-dialogue num)
		(open-register-dialogue num))
	(import server-param
		tools
		xml		
		digest
		socks))

;;;==========================================================================================
;;;                           register dialogue
;;;==========================================================================================

(define (open-register-dialogue num)
 (with-handler (lambda(e) (print "register dialog error") (cv-set-value cv-connection 1 num)(unlock  cv-connection mutex-connection num) (close-connection num))
  (xml-parse (socket-input (Client-socket (vector-ref clients num))) 
	     :content-length -1
	     :procedure
	     (lambda (markup attributes children) 
	       (cond  ((eq? markup 'iq) (processIq attributes children num) children)		     
		      ((eq? markup 'stream:features) (cv-set-value cv-connection 0 num)
						     (unlock cv-connection mutex-connection num) children)   
		      ((eq? markup 'stream:stream)
		       (socket-shutdown (Client-socket (vector-ref clients num)))
		       (cv-set-value cv-connection 1 num)
		       (unlock cv-connection mutex-connection num)
		       (thread-terminate! (current-thread))  children)
		      (else (list markup attributes children)))))))

;;;==========================================================================================
;;;                           session dialogue
;;;==========================================================================================

(define (open-session-dialogue num)
 (with-handler 
   (lambda(e) (print "session dialog error") (cv-set-value cv-connection 1 num)(unlock cv-connection mutex-connection num) ((Client-error (vector-ref clients num)) num)  (close-connection num) )
   (xml-parse (socket-input (Client-socket (vector-ref clients num))) 
	      :content-length -1
	      :procedure
	      (lambda (markup attributes children) 
		 (cond  ((eq? markup 'message) (processMessage attributes children num)  children)
		       ((eq? markup 'iq) (processIq attributes children num) children)
		       ((eq? markup 'stream:features) (processFeatures children num) children)
		       ((eq? markup 'presence) (processPresence attributes children num) children)
		       ((eq? markup 'proceed) (start-tls num) children)
		       ((eq? markup 'stream:stream)
			(socket-shutdown (Client-socket (vector-ref clients num)))
			(cv-set-value cv-connection 1 num)			
			(unlock cv-connection mutex-connection num)
			((Client-error (vector-ref clients num)) num) (thread-terminate! (current-thread))  children)
		       (else (list markup attributes children)))))))

;;;==========================================================================================
;;;                           sasl dialogue
;;;==========================================================================================

(define (open-dialog-sasl num)
   (let ((auth? #f)(server (Client-serverJabber (vector-ref clients num)) ))
     (xml-parse (socket-input (Client-socket (vector-ref clients num))) 
		:content-length -1
		:procedure 
		(lambda (markup attributes children)
		   (cond  ((eq? markup 'message) (processMessage attributes children num)  children)
			 ((eq? markup 'iq) (processIq attributes children num) children)		     
			 ((eq? markup 'stream:features) (if (not auth?) 
							    (begin
							      (cv-set-value cv-connection 0 num)
							      (unlock cv-connection mutex-connection num))
							    (send BIND num)) (set! auth? #t)   children) 

			 ((eq? markup 'presence) (processPresence attributes children num) children)
			 ((eq? markup 'success) (send (format NEWSTREAM server) num)  children)
			 ((eq? markup 'failure) (cv-set-value cv-connection 1 num) (unlock cv-connection mutex-connection num))
			 ((eq? markup 'stream:stream)
			  (socket-shutdown (Client-socket (vector-ref clients num)))
			  (cv-set-value cv-connection 1 num)
			  (unlock cv-connection mutex-connection num)
			  ((Client-error (vector-ref clients num)) num) (thread-terminate! (current-thread))  children)
			 (else (list markup attributes children)))))))

;;;==================================================================================
;;;                              features process
;;;==================================================================================

(define (processFeatures children num)
  (let ((starttls? (search-node 'starttls children))(m (search-node 'mechanisms children))(sasl? #f))
    (if m 
	(let ((xmlns (assoc 'xmlns (cadr m)))) (if (and xmlns (string=? (cdr xmlns) "urn:ietf:params:xml:ns:xmpp-sasl") (plain? (caddr m)))
						   (set! sasl? #t))))
    (cond (sasl? (start-sasl num))
	  (starttls? (send STARTTLS num))
	  (else	(cv-set-value cv-connection 0 num)  (unlock cv-connection mutex-connection num)))))

;;;==================================================================================
;;;                              start SASL
;;;==================================================================================

(define (start-sasl num)
   (socket-shutdown (Client-socket (vector-ref clients num)))
   (socket-close (Client-socket (vector-ref clients num)))
   (let* ((server (Client-serverJabber (vector-ref clients num)))(sock (make-ssl-client-socket server PORT_SSL)))
     (Client-socket-set! (vector-ref clients num) sock)
     (Client-sasl-set!  (vector-ref clients num) #t)
     (send (format NEWSTREAM server) num)
     (open-dialog-sasl num)))
  
;;;==================================================================================
;;;                              SASL PLAIN?
;;;==================================================================================

(define (plain? children)
  (if (null? children)
      #f
      (let ((node (car children)))
	(cond ((or (null? node) (not (pair? node))) (plain? (cdr children)))
	      ((and (not (null? (caddr node))) (string? (caaddr node)) (string=? (caaddr node) "PLAIN")) #t)
	      (else (plain? (cdr children)))))))	

;;;================================================================================
;;;                               start TLS
;;;================================================================================

(define (start-tls num)
   (client-socket-use-ssl! (Client-socket (vector-ref clients num)) :protocol 'tls)
   (send (format NEWSTREAM (Client-serverJabber (vector-ref clients num))) num))

;;;============================================================================================
;;;                               Message process
;;;============================================================================================

(define (processMessage attr children num)
  (let ((type (assoc 'type attr)))
    (if type
	(cond ((string=? (cdr type) "chat")(let ((body (search-node 'body children))(html (search-node 'html children))(from (assoc 'from attr)))
					     (if body
						 (if html
						     (let* ((xhtml (search-node 'body (caddr html))))
						       (receive-message (cdr from) (caaddr body) (list->xhtml (caddr xhtml)) num))
						     (receive-message (cdr from) (caaddr body) #f num)))))))))
  
;;;============================================================================================
;;;                                Presence process
;;;============================================================================================

(define (processPresence attr children num)
  (let ((type (assoc 'type attr))(from (assoc 'from attr))(show (search-node 'show children))(status (search-node 'status children))(contact '()))
    (if from
	(begin
	  (set! contact (cons  (cons 'jid (cdr from)) contact))
	  (if type
	      (cond ((string=? (cdr type) "subscribe") ((Client-subscribe (vector-ref clients num)) contact num))
		    ((string=? (cdr type) "unavailable") (set! contact (cons (cons 'show "offline") contact)) ((Client-presence (vector-ref clients num)) contact num))
		    ((string=? (cdr type) "error") (print "error presence : " (cdr from))))
	      (begin
		(if status
		    (set! contact (cons (cons 'status (caaddr status)) contact)))
		(if show
		    (set! contact (cons (cons 'show (caaddr show)) contact)))
		((Client-presence (vector-ref clients num)) contact num))))))) 

;;;============================================================================================
;;;                                              iq process
;;;============================================================================================

(define (processIq attr children num)
  (let ((type (assoc 'type attr))(id (assoc 'id attr))(from (assoc 'from attr)))
    (if id (set! id (cdr id)))
    (if from (set! from (cdr from)))
    (cond ((string=? (cdr type) "error") (cond ((or (string=? id "contacts")(string=? id "addContact")(string=? id "log")(string=? id "register")(string=? id "session"))
						 (cv-set-value cv-connection  1 num)(unlock cv-connection mutex-connection num))
					       ((string=? id "getSupported") (cv-set-value cv-send 1 num) (unlock cv-send mutex-send num))
					       ((send-file? id)  (cv-set-value cv-send 1 num)  (unlock cv-send mutex-send num))
					       ((string=? id "remove") (cv-set-value cv-remove 1 num) (unlock cv-remove mutex-rm num))))
	   
	  ((string=? (cdr type) "set") (let* ((query (search-node 'query children))(si (search-node 'si children)))
					 (cond (si (processSi id from (cadr si) (caddr si) num))
					       (query (processQuery  id from (cadr query) (caddr query) num)))))

	  ((string=? (cdr type) "result")(cond  ((string=? id "contacts") (let ((cl (make-contacts-list (caddr (search-node 'query children))))) 
									     (cv-set-value cv-connection  cl num)
									     (unlock cv-connection mutex-connection num)))
						((string=? id "getSupported") (transfer-proto? children num))
						((or (string=? id "addContact") (string=? id "log")(string=? id "register")(string=? id "session"))
						  (cv-set-value cv-connection 0 num)(unlock cv-connection mutex-connection num))					
						((string=? id "bind")(send SESSION num))
						((string=? "streamhost" id)(cv-set-value cv-send 0 num) (unlock cv-send mutex-send num))
						((send-file? id) (send-ok? (get-sid id) children num))
						((string=? id "remove") (cv-set-value cv-remove 0 num)  (unlock cv-remove  mutex-rm num))))

	  ((string=? (cdr type) "get") (let* ((query (search-node 'query children)))
					 (cond (query (processQuery id from (cadr query) (caddr query) num))))))))

;;;========================================================================
;;;                            query process
;;;========================================================================

(define (processQuery id from attr children num)
  (let ((xmlns (assoc 'xmlns attr)))
    (cond ((and xmlns (string=? (cdr xmlns) "http://jabber.org/protocol/disco#info")) (send-supported id from num))
	  ((and xmlns (string=? (cdr xmlns) "http://jabber.org/protocol/bytestreams")) (let ((sid (assoc 'sid attr)))
											(processStreamhost id (cdr sid) from children num))))))

;;;=========================================================================
;;;                            si process
;;;=========================================================================

(define (processSi id from attr children num)
  (let ((profile (assoc 'profile attr))(file (search-node 'file children))(sid (assoc 'id attr)))
    (if (and (string=? (cdr profile) "http://jabber.org/protocol/si/profile/file-transfer") file (bytestreamsOption? children))
	(processFile id (cdr sid) from (cadr file) (caddr file) num))))

;;;=========================================================================
;;;                            file process
;;;=========================================================================
	
(define (processFile id sid from attr children num)
  (let ((filename (cdr (assoc 'name attr)))(size (cdr (assoc 'size attr))))
    (with-lock (vector-ref mutex-list-file-name num)
	       (lambda()
		 (vector-set! list-file-name num (cons (cons (string->symbol sid) filename) (vector-ref list-file-name num)))))
    ((Client-rcv-file? (vector-ref clients num)) from id filename size num)))

;;;=========================================================================
;;;                            streamhost process
;;;=========================================================================

(define (processStreamhost id sid from sh-list num)
  (let loop ((sh sh-list))
    (cond ((null? sh) (send (format IQ_ERROR id from) num)) 
	  ((and (pair? (car sh)) (eq? (caar sh) 'streamhost)) 
	   (let* ((streamhost (car sh))
		  (port (cdr (assoc 'port (cadr streamhost))))
		  (host (cdr (assoc 'host (cadr streamhost))))
		  (sock (test-connection host (string->number port)))
		  (jid (cdr (assoc 'jid (cadr streamhost)))))
	     (if (socket? sock) 
		 (let ((thread (make-thread (lambda()(receive-file sock from id jid sid  num)) 'receive-file)))
		   (thread-start! thread) #t)
		 (loop (cdr sh)))))
	  (else (loop (cdr sh))))))

(define (test-connection host port)
  (with-handler (lambda(e)  #f)
		(let ((sock (make-client-socket host port)))
		  (if (socket? sock)
		      sock
		      #f))))

;;;======================================================
;;;                      transfer?
;;;======================================================

 (define (transfer-proto? children num)
   (define (iter x)   
     (cond  ((null? x) (cv-set-value cv-send 1 num))
	    ((or (not (pair? (car x))) (null? (car x))) (iter (cdr x)))
	    ((eq? (caar x) 'feature)(let ((var (assoc 'var (cadar x))))
				      (if (and var (string=? "http://jabber.org/protocol/si/profile/file-transfer" (cdr var)))
					 (begin (cv-set-value cv-send 0 num)  (unlock cv-send mutex-send num))
					 (iter (cdr x)))))
	    (else (iter (cdr x)))))	  
   (let ((query (search-node 'query children)))
     (if query
	 (iter (caddr query))
	 (begin  (cv-set-value cv-send 1 num)  (unlock cv-send mutex-send num)))))

;;;=======================================================
;;;                    send ok ?
;;;=======================================================

(define (send-ok? id children num)
  (let ((si (search-node 'si children)))
    (if si
	(let ((feature (search-node 'feature (caddr si))))
	  (if feature
	      (let ((x (search-node 'x (caddr feature))))
		(if x
		    (let ((type (assoc 'type (cadr x))))
		      (if (and type (string=? (cdr type) "submit"))
			  (cv-set-value cv-send id num))))))))
    (unlock cv-send mutex-send num)))		  
			  
;;;==========================================================
;;;                       receive file
;;;==========================================================

(define (receive-file sock from id jid sid num)  
  (let* ((sha1  (digest-string (string-append sid from (Client-jid (vector-ref clients num))) 'sha-1 'hex))
	(connect (socks-connect sock #x05 #x01 #x00))
	(method (assoc 'method connect)))
    (if (and method (eq? #x00 (cdr method)))    
	(let* ((request (socks-request sock #x05 #x01 #x03 sha1 #x00 #x00))
	       (ok? (assoc 'rep request)))
	  (if (and ok? (eq? #x00 (cdr ok?)))
	      (begin (send (format INIT_STREAM from id jid) num)	       
		     (let* ((filename (get-file-name sid num))(pf (open-output-file (string-append FILE_DIR "/" filename)))) 		   
		       (send-chars (socket-input sock) pf)
		       (close-output-port pf)
		       ((Client-file-rcv (vector-ref clients num)) filename num))))))))

;;;===================================================================================
;;;                                    create contacts list
;;;===================================================================================

(define (make-contacts-list item-list)
  (cond ((null? item-list) '())
	((null? (car item-list)) (make-contacts-list (cdr item-list)))
	((eq? (caar item-list) 'item)(let* ((attr (cadar item-list))(group (search-node 'group (caddr (car item-list)))))
				       (if group 
					   (cons (cons (cons 'group (caaddr group)) attr)  (make-contacts-list (cdr item-list)))
					   (cons attr (make-contacts-list (cdr item-list))))))
	  (else (make-contacts-list (cdr item-list)))))

;;;==========================================================================================
;;;                                     receive message
;;;==========================================================================================

(define (receive-message from text html num)
  (Client-timer-set! (vector-ref clients num) (current-date))
  ((Client-message (vector-ref clients num))  from text html num))

;;;===========================================================================================
;;;                               send supported protocol
;;;===========================================================================================

(define (send-supported id to num) 
  (let ((xmlrequest (format SUPPORTED to id)))
    (send xmlrequest num)))

;;;===========================================================================================
;;;                          bytestream option ?
;;;===========================================================================================

(define (bytestreamsOption? children)
  (let ((feature (search-node 'feature children))(res #f))
    (if feature
	(let ((x (search-node 'x (caddr feature))))
	  (if x
	      (let ((field (search-node 'field (caddr x))))
		(if field
		    (do ((c (caddr field) (cdr c)))
			((null? c)  res)
		      (if (pair? (car c))
			  (let ((option (car c)))	
			    (if (and (not (null? option)) (eq? (car option) 'option) (not (null? (caddr option))))
				(let ((value (search-node 'value (caddr option))))
				  (if (and (not (null? value)) (eq? (car value) 'value) (not (null? (caddr value))) (string? (caaddr value)) (string=? (caaddr value) "http://jabber.org/protocol/bytestreams"))
				      (set! res #t)))))))
		    #f))
		#f))
	      #f)))			   

;;;===========================================================================================
;;;                                     close connection
;;;===========================================================================================

(define (close-connection num)
  (with-handler 
   (lambda(e)  (print "error disconnect"))
   (socket-shutdown (Client-socket (vector-ref clients num)))
   (vector-set!  clients num  #f)))
