(module xml
   (library pthread)
	(export CONNECTION
		LOG_INFO
		REGISTER
		MESSAGE
		GET_CONTACT
		ADD_CONTACT
		SUSCRIBE
		REMOVE_CONTACT
		STATUS_MESS
		STATUS
		AUTHORIZE 
		AUTH
		RECEIVE_OK
		SEND_FILE_INFO
		GET_SUPPORTED
		STREAMHOST
		INIT_STREAM
		STARTTLS
		NEWSTREAM
		BIND
		SESSION
		SUPPORTED
		IQ_ERROR
		)
)

(define CONNECTION "<?xml version='1.0'?><stream:stream  to='~a'  xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>")
(define LOG_INFO  "<iq type='set' id='log' to='~a' ><query xmlns='jabber:iq:auth'><username>~a</username><password>~a</password><resource>Hop</resource></query></iq>")
(define REGISTER "<iq type='set' to='~a' id='register' ><query xmlns=\"jabber:iq:register\"><username>~a</username><password>~a</password></query></iq>")
(define MESSAGE  "<message to='~a' type='chat'><body>~a</body><html xmlns='http://jabber.org/protocol/xhtml-im'><body xmlns='http://www.w3.org/1999/xhtml'>~a</body></html></message>")
(define GET_CONTACT "<iq type='get' id='contacts' ><query xmlns='jabber:iq:roster'/></iq>")
(define ADD_CONTACT "<iq type='set' id='addContact' ><query xmlns='jabber:iq:roster'>~a</query></iq>")
(define SUSCRIBE "<presence type='subscribe' to='~a'></presence>")
(define REMOVE_CONTACT "<iq type='set' id='remove' ><query xmlns='jabber:iq:roster'><item subscription='remove' jid='~a' /></query></iq>")
(define AUTHORIZE  "<presence type='~a' to='~a' />")
(define STATUS_MESS "<presence><show>~a</show><status>~a</status><priority>1</priority></presence>")
(define STATUS "<presence><show>~a</show><priority>1</priority></presence>")
(define AUTH "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl'  mechanism='PLAIN'>~a</auth>")
(define RECEIVE_OK "<iq type='~a' to='~a' id='~a'><si xmlns='http://jabber.org/protocol/si'><feature xmlns='http://jabber.org/protocol/feature-neg'><x xmlns='jabber:x:data' type='submit'><field var='stream-method'><value>http://jabber.org/protocol/bytestreams</value></field></x></feature></si></iq>")
(define SEND_FILE_INFO "<iq type='set' id='sendFile:~a' to='~a'><si xmlns='http://jabber.org/protocol/si' id='~a' mime-type='text/plain' profile='http://jabber.org/protocol/si/profile/file-transfer'><file xmlns='http://jabber.org/protocol/si/profile/file-transfer' name='~a' size='~a'/><feature xmlns='http://jabber.org/protocol/feature-neg'><x xmlns='jabber:x:data' type='form'><field var='stream-method' type='list-single'><option><value>http://jabber.org/protocol/bytestreams</value></option></field></x></feature></si></iq>")
(define GET_SUPPORTED "<iq type='get' id='getSupported' to='~a'><query xmlns='http://jabber.org/protocol/disco#info'/></iq>")

(define SUPPORTED "<iq type='result' to='~a' id='~a'><query xmlns='http://jabber.org/protocol/disco#info'><feature var='http://jabber.org/protocol/xhtml-im'/><feature var='http://jabber.org/protocol/si/profile/file-transfer'/></query></iq>")
(define INIT_STREAM "<iq type='result' to='~a' id='~a'><query xmlns='http://jabber.org/protocol/bytestreams'><streamhost-used jid='~a'/></query></iq>")
(define STARTTLS "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
(define NEWSTREAM "<stream:stream  xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' to='~a'  version='1.0'>")
(define BIND "<iq type='set' id='bind'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'><resource>Hop</resource></bind></iq>")
(define SESSION "<iq type='set' id='session'><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>")
(define STREAMHOST "<iq type='set' from='~a' to='~a' id='streamhost'><query xmlns='http://jabber.org/protocol/bytestreams' sid='~a' mode='tcp'><streamhost jid='~a' host='~a' port='~a'/></query></iq>")
(define IQ_ERROR "<iq type='error' id='~a' to='~a'/>")
