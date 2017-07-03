(module bigloodbus

	(option (set! *dlopen-init* "dbus_s"))
	
	(extern
	 (export mylist->vector "bgl_list_to_vector")
	 (export get-signature "bgl_get_signature")
	 (export myerror "bgl_error")
	 (export list->dbus-struct "bgl_list_to_dbus_struct")
	 (export my-dbus-struct-pred "bgl_is_dbus_structure")
	 (export dbus-struct->list "bgl_extract_list_from_struct")
	 (export my-empty-array-pred "bgl_is_dbus_empty_array")
	 (export get-sign-from-empty-arr "bgl_get_sign_from_empty_arr"))
	
	(export (mylist->vector::vector ::pair-nil)
		(get-signature ::obj)
		(myerror ::obj ::obj)
		(my-dbus-struct-pred ::obj)
		(my-empty-array-pred ::obj)
		(get-sign-from-empty-arr ::obj)

		(class %message
		       builtin::$messagep)
		(class %object
		       builtin::$objectp)
		(class %bus
		       builtin::$busp)
		(class %service
		       builtin::$servicep)
		(class %dbus-struct
		       builtin::obj)
		(class %dbus-empty-array
		       signature::bstring)

		(integer->dbus-byte i)
		(integer->dbus-int16 i)
		(integer->dbus-uint16 i)
		(integer->dbus-uint32 i)
		(integer->dbus-int64 i)
		(integer->dbus-uint64 i)
		(string->dbus-object-path str)
		(string->dbus-signature str)
		(obj->dbus-variant obj)
		(list->dbus-struct lst)
		(dbus-struct->list obj)
		(is-dbus-struct? obj)
		(create-dbus-empty-vector content)

		(dbus-connect type #!key (path "/") (interface "bgl.dbus")) ;-- done
		(dbus-connect-remote-bus str) 
		(dbus-set-path bus path) ;-- done
		(dbus-set-interface bus inter) ;-- done
		(dbus-close bus) ;-- done
		(dbus-is-connected bus) ;-- done
		(dbus-service-name-has-owner bus sname) ;-- done
		(dbus-subscribe rule bus)
		(dbus-unsubscribe rule bus)
		(dbus-release-service-name bus) ;-- done
		(dbus-set-service-name bus sname #!key (flag 'replace_existing)) ;-- done

		(dbus-handle-service bus sname) ;-- done
		(dbus-handle-object serv path interface) ;-- done

		(dbus-get-args-from-message msg) ;-- done

		(dbus-receive-message-from-object obj #!key (t 1000)) ;-- done
		(dbus-receive-message-from-bus bus #!key (t 1000)) ;-- done
		(dbus-receive-message-with-rule bus rule #!key (t 1000)) ;-- done

		(dbus-send-signal bus signame #!optional args) ;-- done
		(dbus-send-error bus replyto errtype errmsg #!optional args)
		(dbus-send-method-call obj mname #!optional args #!key (t -1) (reply #t))
		(dbus-send-introspect-return bus path replyto)

		(dbus-introspect-object obj #!key (t -1))
		(dbus-send-method-call-return bus replyto #!optional args)

		(dbus-message-type msg)
		(dbus-get-member msg)
		(dbus-get-sender msg)
		(dbus-get-path msg))

	(include "types.sch")
	(include "connection.sch")
	(include "message.sch")
	(include "service.sch"))


;*----------------------------------------------------------------------*
;* String contenant le nom de la derniere fonction appellee             *
;*----------------------------------------------------------------------*

(define lastCalledFunction "")

;*----------------------------------------------------------------------*
;* Fonctions exportees pour C                                           *
;*----------------------------------------------------------------------*

(define (mylist->vector lst)
  (list->vector lst))

(define (myerror b c)
  (error (string-append "bigloodbus:" lastCalledFunction)
	 b c))

(define (my-dbus-struct-pred obj)
  (%dbus-struct? obj))

(define (my-empty-array-pred obj)
  (%dbus-empty-array? obj))

(define (get-sign-from-empty-arr obj)
  (%dbus-empty-array-signature obj))

(define (get-signature obj)
  (cond ((string? obj) "s")
	((integer? obj) "i")
	((boolean? obj) "b")
	((real? obj) "d")
	((and (pair? obj) (equal? 'byt (car obj))) "y")
	((and (pair? obj) (equal? 'i16 (car obj))) "n")
	((and (pair? obj) (equal? 'u16 (car obj))) "q")
	((and (pair? obj) (equal? 'u32 (car obj))) "u")
	((and (pair? obj) (equal? 'i64 (car obj))) "x")
	((and (pair? obj) (equal? 'u64 (car obj))) "t")
	((and (pair? obj) (equal? 'op  (car obj))) "o")
	((and (pair? obj) (equal? 'sig (car obj))) "g")
	((and (pair? obj) (equal? 'var (car obj))) "v")
	((vector? obj)
	 (string-append "a"
			(get-signature (vector-ref obj 0))))
	((and (pair? obj) (null? (cddr obj)))
	 (string-append "{"
			(get-signature (car obj))
			(get-signature (cadr obj))
			"}"))
	((%dbus-struct? obj)
	 (let ((res "("))
	   (do ((lst (%dbus-struct-builtin obj) (cdr lst)))
	       ((null? lst) (string-append res ")"))
	     (set! res (string-append res (get-signature (car lst)))))))))


;*----------------------------------------------------------------------*
;* Fonctions de conversion                                              *
;*----------------------------------------------------------------------*

(define (integer->dbus-byte i)
  (if (integer? i) 
      (cons 'byt i)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an integer" i)))

(define (integer->dbus-int16 i)
  (if (integer? i) 
      (cons 'i16 i)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an integer" i)))

(define (integer->dbus-uint16 i)
  (if (integer? i) 
      (cons 'u16 i)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an integer" i)))

(define (integer->dbus-uint32 i)
  (if (integer? i) 
      (cons 'u32 i)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an integer" i)))

(define (integer->dbus-int64 i)
  (if (integer? i) 
      (cons 'i64 i)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an integer" i)))

(define (integer->dbus-uint64 i)
  (if (integer? i) 
      (cons 'u64 i)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an integer" i)))

(define (string->dbus-object-path str)
  (if (string? str)
      (cons 'op str)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an string" str)))

(define (string->dbus-signature str)
  (if (string? str)
      (cons 'sig str)
      (error "bigloodbus:integer->dbus-byte"
	     "Argument must be an string" str)))

(define (obj->dbus-variant obj)
  (cons 'var obj))

(define (is-dbus-struct? obj)
  (%dbus-struct? obj))

(define (list->dbus-struct lst)
  (if (list? lst)
      (instantiate::%dbus-struct (builtin lst))
      (error "bigloodbus:list->dbus-struct"
	     "Argument must be a list" lst)))

(define (dbus-struct->list obj)
    (if (%dbus-struct? obj)
	(%dbus-struct-builtin obj)
	(error "bigloodbus:dbus-struct->list"
	       "Argument must be a dbus-struct" obj)))

(define (integer->boolean i)
  (not (= 0 i)))

(define (create-dbus-empty-vector type)
  (set! lastCalledFunction "create-dbus-empty-vector")
  (instantiate::%dbus-empty-array
   (signature (get-signature-of-empty-array type))))



;*----------------------------------------------------------------------*
;* Fonctions de verification                                            *
;*----------------------------------------------------------------------*

(define (verif-string s fname pname)
  (if (not (and (string? s) (> (string-length s) 0)))
      (myerror
       (string-append "Parameter " pname " is not a string or is empty string")
       s)))

(define (verif-bus bus fname)
  (if (not (%bus? bus))
      (myerror
       "Parameter bus not a bus object"
       bus)))

(define (verif-service serv fname)
  (if (not (%service? serv))
      (myerror
       "Parameter service is not a service object"
       serv)))

(define (verif-message msg fname pname)
  (if (not (%message? msg))
      (myerror
       (string-append "Parameter " pname" is not a message object")
       msg)))

(define (verif-timeout t name)
  (if (not (and (integer? t) (> t -2)))
      (myerror
       "Parameter timeout must be an integer strictly superior to -2"
       t)))

(define (verif-object obj fname)
  (if (not (%object? obj))
      (myerror
       "Parameter object is not an object object"
       obj)))

(define (verif-flag-symbol p)
  (if (not (memq p '(allow_replacement do_not_queue replace_existing)))
      (myerror
       "Unknown flag. Flags are allow_replacement do_not_queue replace_existing"
       p)))

(define (verif-flag-list p)
  (if (memq (car p) (cdr p))
      (myerror "Parameter list cannot contain two times the same flag"
	       p))
  (if (null? (cdr p))
      (verif-flag-symbol (car p))
      (begin
	(verif-flag-symbol (car p))
	(verif-flag-symbol (cdr p)))))

(define (verif-flag-parameter p)
  (cond ((symbol? p) (verif-flag-symbol p))
	((and (list? p) (< (length p) 3)) (verif-flag-list p))
	(else (myerror
	       "Parameter flag must be a symbol or list of maximum length 3"
	       p))))


;*----------------------------------------------------------------------*
;* Fonctions "utilitaires"                                              *
;*----------------------------------------------------------------------*

(define (get-int-from-flag s)
  (cond ((equal? s 'allow_replacement) 1)
	((equal? s 'do_not_queue) 2)
	((equal? s 'replace_existing) 3)))

(define (get-int-from-flag-parameter p)
  (if (symbol? p)
      (get-int-from-flag p)
      (apply + (map get-int-from-flag p))))

(define (get-signature-of-empty-array content #!optional struct-list?)
  (if (null? content) ""
      (begin
	(cond ((equal? 'byte        content) "y")
	      ((equal? 'boolean     content) "b")
	      ((equal? 'int16       content) "y")
	      ((equal? 'uint16      content) "q")
	      ((equal? 'int32       content) "i")
	      ((equal? 'uint32      content) "u")
	      ((equal? 'int64       content) "x")
	      ((equal? 'uint64      content) "t")
	      ((equal? 'double      content) "d")
	      ((equal? 'string      content) "s")
	      ((equal? 'object-path content) "o")
	      ((equal? 'signature   content) "g")
	      ((equal? 'variant     content) "v")
	      (struct-list? (string-append (get-signature-of-empty-array (car content))
					   (get-signature-of-empty-array (cdr content) #t)))
	      ((and (vector? content) (= 1 (vector-length content)))
	       (string-append "a" (get-signature-of-empty-array (vector-ref content 0))))
	      ((and (pair? content) (null? (cddr content)))
	       (string-append "{" (get-signature-of-empty-array (car content))
			      (get-signature-of-empty-array (cadr content)) "}"))
	      ((%dbus-struct? content)
	       (string-append "(" (get-signature-of-empty-array (%dbus-struct-builtin content) #t)
			      ")"))
	      (else (myerror "Unknown type" content))))))


;*----------------------------------------------------------------------*
;* Fonctions DBUS                                                       *
;*----------------------------------------------------------------------*

(define (dbus-connect type #!key (path "/") (interface "bgl.dbus"))
  (set! lastCalledFunction "dbus-connect")
  (if (not (memq type '(session system)))
      (error "dbus-connect" "Argument type must be session or system" type))
  (let* ((cbus (connectBus (if (equal? type 'session) 0 1)))
	 (bus (instantiate::%bus (builtin cbus))))
    (dbus-set-path bus path)
    (dbus-set-interface bus interface)
    bus))

(define (dbus-connect-remote-bus str)
  (set! lastCalledFunction "dbus-connect-remote-bus")
  (verif-string str "dbus-connect-remote-bus" "adress")
  (instantiate::%bus (builtin (connectRemoteBus str))))

(define (dbus-set-path bus path)
  (set! lastCalledFunction "dbus-set-path")
  (verif-bus bus "dbus-set-path")
  (verif-string path "dbus-set-path" "path")
  (integer->boolean (setPathConnection (%bus-builtin bus) path)))

(define (dbus-set-interface bus inter)
  (set! lastCalledFunction "dbus-set-interface")
  (verif-bus bus "dbus-set-interface")
  (verif-string inter "dbus-set-interface" "interface")
  (integer->boolean (setPathConnection (%bus-builtin bus) inter)))

(define (dbus-close bus)
  (set! lastCalledFunction "dbus-close")
  (verif-bus bus "dbus-close")
  (closeBus (%bus-builtin bus)))

(define (dbus-is-connected bus)
  (set! lastCalledFunction "dbus-is-connected")
  (verif-bus bus "dbus-is-connected")
  (integer->boolean (isConnected (%bus-builtin bus))))

(define (dbus-service-name-has-owner bus sname)
  (set! lastCalledFunction "dbus-service-name-has-owner")
  (verif-bus bus "dbus-service-name-has-owner")
  (verif-string sname "dbus-service-name-has-owner" "service name")
  (integer->boolean (serviceNameHasOwner (%bus-builtin bus) sname)))

(define (dbus-handle-service bus sname)
  (set! lastCalledFunction "dbus-handle-service")
  (verif-bus bus "handle-service")
  (verif-string sname "dbus-handle-service" "service name")
  (instantiate::%service (builtin
			  (handleService (%bus-builtin bus) sname))))

(define (dbus-handle-object serv path interface)
  (set! lastCalledFunction "dbus-handle-object")
  (verif-service serv "dbus-handle-object")
  (verif-string path "dbus-handle-object" "path")
  (verif-string interface "dbus-handle-object" "interface")
  (instantiate::%object (builtin
			 (handleObject (%service-builtin serv) path interface))))

(define (dbus-subscribe rule bus)
  (set! lastCalledFunction "dbus-subscribe")
  (verif-bus bus "dbus-subscribe")
  (if (not (string? rule))
      (error "dbus-subscribe"
	     "Parameter rule is not a string"
	     rule))
  (integer->boolean (subscribe rule (%bus-builtin bus))))

(define (dbus-unsubscribe rule bus)
  (set! lastCalledFunction "dbus-unsubscribe")
  (verif-bus bus "dbus-unsubscribe")
  (if (not (string? rule))
      (error "dbus-unsubscribe"
	     "Parameter rule is not a string"
	     rule))
  (integer->boolean (unsubscribe rule (%bus-builtin bus))))

(define (dbus-release-service-name bus)
  (set! lastCalledFunction "dbus-release-service-name")
  (verif-bus bus "dbus-release-service-name")
  (integer->boolean (releaseServiceName (%bus-builtin bus))))


(define (dbus-set-service-name bus sname #!key (flag 'replace_existing))
  (set! lastCalledFunction "dbus-set-service-name")
  (verif-bus bus "dbus-set-service-name")
  (verif-string sname "dbus-set-service-name" "service name")
  (verif-flag-parameter flag)
  (integer->boolean (setServiceName (%bus-builtin bus)
				    sname
				    (get-int-from-flag-parameter flag))))

(define (dbus-get-args-from-message msg)
  (set! lastCalledFunction "dbus-get-args-from-message")
  (verif-message msg "dbus-get-args-from-message" "message")
  (getArgsMessage (%message-builtin msg)))


(define (dbus-receive-message-from-object obj #!key (t 1000))
  (set! lastCalledFunction "dbus-receive-message-from-object")
  (verif-object obj "dbus-receive-message-from-object")
  (verif-timeout t "dbus-receive-message-from-object")
  (let ((res (receiveMessageFromObject (%object-builtin obj) t)))
    (if (void*-null? res)
	#f
	(instantiate::%message (builtin res)))))

(define (dbus-receive-message-from-bus bus #!key (t 1000))
  (set! lastCalledFunction "dbus-receive-message-from-bus")
  (verif-bus bus "dbus-receive-message-from-bus")
  (verif-timeout t "dbus-receive-message-from-bus")
  (let ((res (receiveMessageFromBus (%bus-builtin bus) t)))
    (if (void*-null? res)
	#f
	(instantiate::%message (builtin res)))))

(define (dbus-receive-message-with-rule bus rule #!key (t 1000))
  (set! lastCalledFunction "dbus-receive-message-with-rule")
  (verif-bus bus "dbus-receive-message-with-rule")
  (verif-string rule "dbus-receive-message-with-rule" "rule")
  (verif-timeout t "dbus-receive-message-with-rule")
  (let ((res (receiveMessageWithRule (%bus-builtin bus) t rule)))
    (if (void*-null? res)
	#f
	(instantiate::%message (builtin res)))))

(define (dbus-send-signal bus signame #!optional args)
  (set! lastCalledFunction "dbus-send-signal")
  (verif-bus bus "dbus-send-signal")
  (verif-string signame "dbus-send-signal" "signal name")
  (integer->boolean (sendSignal (%bus-builtin bus) signame (if args args '()))))

(define (dbus-introspect-object obj #!key (t -1))
  (set! lastCalledFunction "dbus-introspect-object")
  (verif-object obj "dbus-introspect-object")
  (getMethodObject (%object-builtin obj) t))

(define (dbus-send-method-call-return bus replyto #!optional args)
  (set! lastCalledFunction "dbus-send-method-call-return")
  (verif-bus bus "dbus-send-method-call-return")
  (verif-message replyto "dbus-send-method-call-return" "reply to")
  (integer->boolean (sendMethodCallReturn (%bus-builtin bus)
					  (%message-builtin replyto)
					  (if args args '()))))


(define (dbus-send-error bus replyto errtype errmsg #!optional args)
  (set! lastCalledFunction "dbus-send-error")
  (verif-bus bus "dbus-send-error")
  (verif-message replyto "dbus-send-error" "reply to")
  (verif-string errtype "dbus-send-error" "error type")
  (verif-string errmsg "dbus-send-error" "error message")
  (integer->boolean (sendError (%bus-builtin bus)
			       (%message-builtin replyto)
			       errtype errmsg
			       (if args args '()))))

(define (dbus-send-method-call obj mname #!optional args #!key (t -1) (reply #t))
  (set! lastCalledFunction "dbus-send-method-call")
  (verif-object obj "dbus-send-method-call")
  (verif-string mname "dbus-send-method-call" "method name")
  (let ((replyMsg (instantiate::%message
		   (builtin
		    (sendMethodCall (%object-builtin obj) mname t
				    (if reply 1 0)
				    (if args args '()))))))
    (if reply replyMsg #f)))

(define (dbus-send-introspect-return bus path replyto)
  (set! lastCalledFunction "dbus-send-introspect-return")
  (verif-bus bus "dbus-send-introspect-return")
  (verif-string path "dbus-send-introspect-return" "path")
  (verif-message replyto "dbus-send-introspect-return" "reply to")
  (integer->boolean (sendIntrospectReturn (%bus-builtin bus) path (%message-builtin replyto))))


(define (dbus-message-type msg)
  (set! lastCalledFunction "dbus-message-type")
  (verif-message msg "dbus-message-type" "message")
  (getMessageType (%message-builtin msg)))

(define (dbus-get-member msg)
  (set! lastCalledFunction "dbus-get-member")
  (verif-message msg "dbus-get-member" "message")
  (getMember (%message-builtin msg)))

(define (dbus-get-sender msg)
  (set! lastCalledFunction "dbus-get-sender")
  (verif-message msg "dbus-get-sender" "message")
  (getSender (%message-builtin msg)))

(define (dbus-get-path msg)
  (set! lastCalledFunction "dbus-get-path")
  (verif-message msg "dbus-get-path" "message")
  (getPath (%message-builtin msg)))
