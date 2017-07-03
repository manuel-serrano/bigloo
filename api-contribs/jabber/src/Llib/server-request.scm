(module bigloojabber
	(option (set! *dlopen-init* "jabber_s"))
	(export (open-connection server #!key (register? #f) (message print) (error print) (subscribe print) (presence print) (rcv-file? print) (file-rcv print) (file-send print))
		(identification login password  num) 
		(register login password server num)
		(add jid num #!key (name #f) (group #f))
		(get-contacts num)
		(sendMessage text to num #!key (xhtml ""))
		(authorize jid auth num)
		(change-state status num #!key (msg #f))
		(remove jid num)
		(subscribe jid num)
		(verify-ident jid numClient)		 
		(close-connection num)
		(accept from id accept? num)
		(send-file-to filename to num)		
		(get-jid numClient))
	(library ssl
		 pthread)
	(import server-param
		xml
		socks
		digest
		tools
		server-dialog))

;;;==============================================================================================
;;;                                   server connection
;;;==============================================================================================

(define (open-connection server #!key (register? #f) (message print) (error print) (subscribe print) (presence print) (rcv-file? print) (file-rcv print) (file-send print))
  (with-handler (lambda(e) (mutex-unlock! mutex-numClient) (print "Connection error to : " server) #f)
		(mutex-lock! mutex-numClient)
		(let ((num (get-num))(xmlrequest (format CONNECTION server))(sock (make-client-socket server PORT :timeout 10000000)))
		  (if (and num (socket? sock))
		      (begin
			(init-cv num)
			(vector-set! clients num (instantiate::Client (socket sock) (jid #f) (serverJabber server) (sasl #f) (timer (current-date)) 
								      (message message) (error error) (subscribe subscribe) (presence presence) (rcv-file? rcv-file?)
								      (file-rcv file-rcv)(file-send file-send)))
			(mutex-unlock! mutex-numClient)
			(display xmlrequest (socket-output sock))
			(flush-output-port  (socket-output sock))
			(if register?
			    (vector-set! serverMessageThread  num (make-thread (lambda()(open-register-dialogue num)) 'thread-session))
			    (vector-set! serverMessageThread  num (make-thread (lambda()(open-session-dialogue num)) 'thread-session)))
			(with-lock (vector-ref mutex-connection num)
				   (lambda()
				     (thread-start! (vector-ref serverMessageThread num))
				     (condition-variable-wait! (vector-ref cv-connection num) (vector-ref mutex-connection num) 20000)
				     (if (eq? 0 (cv-get-value cv-connection num))
					 (begin (cv-set-value cv-connection #f num) num)
					 #f))))
			(begin (mutex-unlock! mutex-numClient) #f)))))

;;;============================================================================================
;;;                                 identification
;;;============================================================================================

(define (identification login password num) 
  (with-handler
   (lambda(e)  (print "Authentification error : " login) #f)
   (let ((sasl? (Client-sasl (vector-ref clients num)))
	 (xmlrequest "")
	 (server (Client-serverJabber (vector-ref clients num))))
     (if sasl?
	 (let ((res (open-output-string)))
	   (write-char #\null res)
	   (display (iso-latin->utf8 login) res)
	   (write-char #\null res )
	   (display (iso-latin->utf8 password) res)
	   (set! res (iso-latin->utf8 (close-output-port res)))     
	   (set! xmlrequest (format AUTH (base64-encode res))))	  
	 (set! xmlrequest (format LOG_INFO server login password)))    
     (with-lock  (vector-ref mutex-connection num)
		 (lambda()
		   (send xmlrequest num)
		   (condition-variable-wait! (vector-ref cv-connection num) (vector-ref mutex-connection num) 20000)
		   (if (eq? 0 (cv-get-value cv-connection num))
		       (begin (Client-jid-set! (vector-ref clients num) (format "~a@~a/Hop" login server)) (cv-set-value cv-connection #f num) #t)
		       #f))))))
   
;;;===========================================================================================
;;;                                     close connection
;;;===========================================================================================

(define (close-connection num)
  (with-handler 
   (lambda(e)  (print "error disconnect"))
   (thread-terminate! (vector-ref serverMessageThread num)) 
   (socket-shutdown (Client-socket (vector-ref clients num)))
   (vector-set!  clients num  #f)))

;;;===========================================================================================
;;;                                        register
;;;===========================================================================================

(define (register login password server num)
  (with-handler 
   (lambda(e)  (print "register error") #f)
   (let ((xmlrequest (format REGISTER server login password)))
     (with-lock (vector-ref mutex-connection num)
		(lambda()
		  (send xmlrequest num)
		  (condition-variable-wait! (vector-ref cv-connection num) (vector-ref mutex-connection num) 20000)
		  (if (eq? 0 (cv-get-value cv-connection num))
		      #t
		      #f))))))

;;;===========================================================================================
;;;                                       change status
;;;===========================================================================================

(define (change-state status num #!key (msg #f))
  (with-handler 
   (lambda(e) (print "status error") ((Client-error (vector-ref clients num)) num)  (close-connection num) )
   (let ((xmlrequest ""))
     (if (not msg)
	 (case status
	   ((0) (set!  xmlrequest "<presence type=\"unavailable\" />"))
	   ((1) (set! xmlrequest (format STATUS "away")))
	   ((2) (set! xmlrequest (format STATUS "dnd")))
	   ((3)(set! xmlrequest (format STATUS "chat"))))
	 (case status
	   ((0) (set!  xmlrequest "<presence type=\"unavailable\" />"))
	   ((1) (set! xmlrequest (format STATUS_MESS "away" msg)))
	   ((2) (set! xmlrequest (format STATUS_MESS "dnd" msg)))
	   ((3)(set! xmlrequest (format STATUS_MESS "chat" msg)))))
     (Client-timer-set! (vector-ref clients num) (current-date))
     (send xmlrequest num))))

;;;==========================================================================================
;;;                                       send message
;;;==========================================================================================

(define (sendMessage text to num #!key (xhtml ""))
  (with-handler 
   (lambda(e) (print "Send message error") ((Client-error (vector-ref clients num)) num)  (close-connection num))
   (let* ((text-clean (clean-xml text)))
     (Client-timer-set! (vector-ref clients num) (current-date))
     (if (string=? xhtml "")
	 (send  (format MESSAGE to text-clean text-clean) num)
	 (send (format MESSAGE to text-clean xhtml) num)))))

;;;==========================================================================================
;;;                                    get contact list
;;;==========================================================================================

(define (get-contacts num)
  (with-handler 
   (lambda(e) (print "Contacts list error") ((Client-error (vector-ref clients num)) num)  (close-connection num) #f)
   (with-lock (vector-ref mutex-connection num)
	      (lambda()
		(send GET_CONTACT num)
		(condition-variable-wait! (vector-ref cv-connection num) (vector-ref mutex-connection num) 20000)
		(if (list? (cv-get-value cv-connection num))
		    (let ((contacts (cv-get-value cv-connection num))) (cv-set-value cv-connection #f num) contacts)
		    #f)))))
				
;;;==========================================================================================
;;;                                add conctact
;;;==========================================================================================

(define (add jid num #!key (name #f) (group #f))
  (with-handler 
   (lambda(e) (print "Add contact error") #f)
   (let ((contact (format "<item jid='~a' " jid)))
     (if name
	 (set! contact (string-append contact (format "name='~a' " name))))
     (if group
	 (set! contact (string-append contact (format "><group>~a</group></item>" group)))
	 (set! contact (string-append contact " />")))
     (with-lock (vector-ref mutex-connection num)
		(lambda()
		  (send  (format ADD_CONTACT contact) num)
		  (condition-variable-wait! (vector-ref cv-connection num) (vector-ref mutex-connection num) 20000)
		  (if (eq? 0 (cv-get-value cv-connection num))
		      (begin (cv-set-value cv-connection #f num) (subscribe jid num) #t)
		      (begin (cv-set-value cv-connection #f num) #f)))))))

(define (subscribe jid num)
  (let ((xmlrequest (format SUSCRIBE jid)))
    (Client-timer-set! (vector-ref clients num) (current-date))
    (send  xmlrequest num)))
  
;;;===========================================================================================
;;;                               remove contact
;;;===========================================================================================

(define (remove jid num)
  (with-handler 
   (lambda(e) (print "remove contact error") #f)
   (let ((xmlrequest (format REMOVE_CONTACT jid)))
     (Client-timer-set! (vector-ref clients num) (current-date))
     (with-lock (vector-ref mutex-rm num)
		(lambda()
		  (send  xmlrequest num)
		  (condition-variable-wait! (vector-ref cv-remove num) (vector-ref mutex-rm num) 20000)
		  (let ((res  (cv-get-value cv-remove num)))
		    (cv-set-value cv-remove #f num) 
		    (if (eq? 0 res) #t #f)))))))  

;;;===========================================================================================
;;;                              authorize contact
;;;===========================================================================================
	
(define (authorize jid auth? num)
  (with-handler (lambda(e) (print "ERROR AUTH") #f)
  (let ((xmlrequest ""))
    (if auth?
	(set! xmlrequest (format AUTHORIZE "subscribed" jid))
	(set! xmlrequest (format AUTHORIZE "unsubscribed" jid)))
    (Client-timer-set! (vector-ref clients num) (current-date))
    (send  xmlrequest num))))  

;;;=========================================================================================
;;;                                verify ident
;;;=========================================================================================

(define (verify-ident jid numClient)
 (and (Client? (vector-ref clients numClient))  (string=? jid (Client-jid (vector-ref clients numClient)))))

;;;==================================================================================
;;;                               accept file
;;;==================================================================================

(define (accept from id accept? num)
  (if accept?
      (send (format RECEIVE_OK "result" from id) num)
      (send (format "<iq type='error' to='~a' id='~a' />" from id) num)))

;;;=================================================================================
;;;                        send file
;;;=================================================================================

(define (send-file-to path to num)
  (with-handler
   (lambda(e) (print "send file error") #f)
   (if (file-exists? path)
       (let ((size (file-size path))(sid (symbol->string (gensym)))(filename (filename-filter path)))
	 (with-lock (vector-ref mutex-send num)
		    (lambda() 
		      (send (format GET_SUPPORTED to) num)
		      (condition-variable-wait! (vector-ref cv-send num) (vector-ref mutex-send num) 20000)
		      (let ((result (cv-get-value cv-send num)))
			(cv-set-value cv-send #f num)				   
			(if (eq? result 0)					
			    (begin
			      (send (format SEND_FILE_INFO sid to sid filename size) num)
			      (condition-variable-wait! (vector-ref cv-send num) (vector-ref mutex-send num) 30000)
			      (let ((result (cv-get-value cv-send num)))
				(cv-set-value cv-send #f num)
				(if (and (string? result) (string=? sid result))
				    (let* ((sock (make-server-socket))
					   (port (socket-port-number sock))
					   (host (socket-local-address (Client-socket (vector-ref clients num))))
					   (from (Client-jid (vector-ref clients num)))
					   (thread (make-thread (lambda() (open-streamhost from to host path sid sock num)))))					 
				      (thread-start! thread)
				      (send (format STREAMHOST from to sid from host port) num)
				      (condition-variable-wait!  (vector-ref cv-send num) (vector-ref mutex-send num) 30000)
				      (let ((result (cv-get-value cv-send num)))
					(cv-set-value cv-send #f num)
					(if (eq? result 0)
					    #t
					    #f)))
				    #f)))		  
			    #f)))))
       #f)))

;;;==============================================================================
;;;                   create new connection for transfer with socks 5 protocol
;;;==============================================================================

(define (open-streamhost from to host path sid sock num)
  (let ((target (socket-accept sock)))
    (let* ((rcv (socks-rcv-method target))(method (assoc 'method rcv)))
      (if (and (pair? method) (pair? (cdr method)) (member #x00 (cdr method)))
	  (begin 
	    (socks-send-select-method target #x05 #x00)
	    (let* ((request (socks-rcv-request target))
		   (addr (cdr (assoc 'addr request)))
		   (sha1 (digest-string (string-append sid from to) 'sha-1 'hex)))
	      (if (string=? sha1 addr)
		  (begin		
		    (socks-send-response target #x05 #x00 #x03 host #x00 #x00)
		    (socket-shutdown sock)
		    (start-transfer target path num))		      
		  (begin 
		    (socks-send-response target #x05 #x05 #x03 host #x00 #x00)
		    #f))))
	  (begin
	    (socks-send-select-method target #x05 #xFF)
	    #f)))))

;;;================================================================================
;;;                     start transfer
;;;================================================================================

(define (start-transfer target path num)
  (let ((pf (open-input-file path)))
    (send-chars pf (socket-output target))
    (close-input-port pf)
    (socket-shutdown target)	
    ((Client-file-send (vector-ref clients num)) path num)))


;;;==================================================================================
;;;                      client number
;;;==================================================================================

(define (get-num)
  (define (iter i)
    (cond  ((>= i NB_MAX_CLIENT) #f)	 
	   ((eq? #f (vector-ref clients i)) i)
	   ((and (Client? (vector-ref clients i)) (free-socket? i))  ((Client-error (vector-ref clients i)) i) (close-connection i) i)
	   (else (iter (+ i 1)))))
  (iter 0))

;;;==================================================================================
;;;                             get jid
;;;==================================================================================

(define (get-jid numClient)
  (Client-jid (vector-ref clients numClient)))
