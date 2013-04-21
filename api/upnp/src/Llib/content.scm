;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/upnp/src/Llib/content.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr  7 07:26:01 2013                          */
;*    Last change :  Sat Apr 20 16:54:10 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Upnp contentdirectory facilities                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __upnp_content-directory

   (library web)
   
   (import __upnp_soap)
   
   (export (upnp-content-directory-browse #!key host port (path "/")
	      (content-type "text/xml")
	      #!rest args)
	   (upnp-content-directory-browse-parse-response ::input-port ::obj
	      #!optional htmldecode))
   )

;*---------------------------------------------------------------------*/
;*    browse-body-start ...                                            */
;*---------------------------------------------------------------------*/
(define browse-body-start
   "<?xml version=\"1.0\" encoding=\"utf-8\" ?> 
<soap:Envelope 
    xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" 
    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" 
    xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\">
 <soap:Body>
  <u:Browse xmlns:u=\"urn:schemas-upnp-org:service:ContentDirectory:1\">")

;*---------------------------------------------------------------------*/
;*    browse-body-end ...                                              */
;*---------------------------------------------------------------------*/
(define browse-body-end
   "</u:Browse></soap:Body></soap:Envelope>")

;*---------------------------------------------------------------------*/
;*    browse-body ...                                                  */
;*---------------------------------------------------------------------*/
(define (browse-body args)
   (soap-envelope browse-body-start browse-body-end args))

;*---------------------------------------------------------------------*/
;*    content-directory-soapaction ...                                 */
;*---------------------------------------------------------------------*/
(define (content-directory-soapaction action)
   (format "\"urn:schemas-upnp-org:service:ContentDirectory:1#~a\"" action))

;*---------------------------------------------------------------------*/
;*    upnp-content-directory-browse-parse-response ...                 */
;*---------------------------------------------------------------------*/
(define (upnp-content-directory-browse-parse-response ip content-length #!optional htmldecode)

   (define (container? o)
      (and (pair? o) (eq? (car o) 'container)))

   (define (item? o)
      (and (pair? o) (eq? (car o) 'item)))

   (define (parser is content-length)
      (bind-exit (return)
	 (xml-parse is
	    :content-length content-length
	    :procedure (lambda (tag attrs body)
			  (case tag
;* 			     ((s:Body s:Envelope)                      */
;* 			      (return body))                           */
;* 			     ((u:BrowseResponse)                       */
;* 			      (tprint tag " -> "                       */
;* 				 (call-with-output-string              */
;* 				    (lambda (p)                        */
;* 				       (write body p))))               */
;* 			      body)                                    */
;* 			     ((Result)                                 */
;* 			      (filter container?                       */
;* 				 (apply append (filter pair? body))))  */
			     ((DIDL-Lite)
			      (return (filter (lambda (o) (or (container? o) (item? o))) body)))
			     ((container)
			      `(container ,@attrs ,@(filter pair? body)))
			     ((res)
			      (cons 'resource (car body)))
			     ((item)
			      `(item ,@attrs ,@(filter pair? body)))
			     ((dc:title)
			      (cons 'title (if (pair? body) (car body) "")))
			     ((dc:creator)
			      (cons 'creator (if (pair? body) (car body) "")))
			     ((dc:date)
			      (cons 'date (if (pair? body) (car body) "")))
			     ((upnp:class)
			      (cons 'class (if (pair? body) (car body) "")))
			     ((upnp:albumArtURI)
			      (cons 'art (car body)))
			     ((upnp:originalTrackNumber)
			      (cons 'tracknumber (car body)))
			     ((upnp:album)
			      (cons 'album (car body)))
			     ((upnp:albumArtist)
			      (cons 'albumartist (car body)))
			     ((upnp:artist)
			      (cons 'artist (car body)))
			     ((upnp:genre)
			      (cons 'genre (car body)))
			     (else
			      (tprint "TAG=[" tag "] " body)))))))

   (if htmldecode
       (let ((s (html-string-decode (read-chars content-length ip))))
	  (call-with-input-string s (lambda (ip) (parser ip (string-length s)))))
       (parser ip content-length)))
		       
;*---------------------------------------------------------------------*/
;*    upnp-content-directory-browse ...                                */
;*---------------------------------------------------------------------*/
(define (upnp-content-directory-browse #!key host port (path "/")
	   (content-type "text/xml")
	   #!rest
	   args)
   (let* ((body (browse-body args))
	  (sock (http :method 'post
		   :host host
		   :port port
		   :content-type "text/xml"
		   :path path
		   :header `((soapaction: ,(content-directory-soapaction "Browse")))
		   :body body))
	  (ip (socket-input sock))
	  (op (socket-output sock)))
      
      (define (parser ip status header clen type)
;* 	 (print "type: " type)                                         */
;* 	 (print "status: " status)                                     */
;* 	 (print "header: " header)                                     */
	 (upnp-content-directory-browse-parse-response ip clen #t))

      (http-parse-response ip op parser)))
      
