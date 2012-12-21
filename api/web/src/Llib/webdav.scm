;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/webdav.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 15 15:05:11 2007                          */
;*    Last change :  Fri Dec 21 08:30:18 2012 (serrano)                */
;*    Copyright   :  2007-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WebDAV client side support.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_webdav
   
   (import __web_xml)
   
   (export (class &webdav-access-control-exception::&access-control-exception
	      (header::pair-nil read-only))

	   (webdav-directory->path-list::pair-nil ::bstring
						  #!key (timeout 0) (proxy #f))
	   (webdav-directory->prop-list::pair-nil ::bstring
						  #!key (timeout 0) (proxy #f))
	   (webdav-directory->list::pair-nil ::bstring
					     #!key (timeout 0) (proxy #f))
	   (webdav-file-exists?::bool ::bstring
				      #!key (timeout 0) (proxy #f))
	   (webdav-directory?::bool ::bstring
				    #!key (timeout 0) (proxy #f))
	   (webdav-file-modification-time::elong ::bstring
						 #!key (timeout 0) (proxy #f))
	   (webdav-file-size::elong ::bstring
				    #!key (timeout 0) (proxy #f))
	   (webdav-delete-file::bool ::bstring
				     #!key (timeout 0) (proxy #f))
	   (webdav-delete-directory::bool ::bstring
					  #!key (timeout 0) (proxy #f))
	   (webdav-make-directory::bool ::bstring
					#!key (timeout 0) (proxy #f))
	   (webdav-make-directories::bool ::bstring
					  #!key (timeout 0) (proxy #f))
	   (webdav-rename-file::bool ::bstring ::bstring
				     #!key (timeout 0) (proxy #f))
	   (webdav-copy-file::bool ::bstring ::bstring
				   #!key (timeout 0) (proxy #f))
	   (webdav-put-file::bool ::bstring ::obj
				  #!key (timeout 0) (proxy #f))))

;*---------------------------------------------------------------------*/
;*    webdav-prop ...                                                  */
;*---------------------------------------------------------------------*/
(define-struct webdav-prop path modified size type)

;*---------------------------------------------------------------------*/
;*    webdav-header ...                                                */
;*---------------------------------------------------------------------*/
(define (webdav-header)
   '())

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define cache-host #unspecified)
(define cache-port 0)
(define cache-socket #f)

(define cache-mutex (make-mutex 'webdav))

;*---------------------------------------------------------------------*/
;*    cache-get ...                                                    */
;*---------------------------------------------------------------------*/
(define (cache-get host port)
   (synchronize cache-mutex
      (if (and (socket? cache-socket)
	       (not (socket-down? cache-socket))
	       (=fx cache-port port)
	       (string=? cache-host host))
	  (let ((socket cache-socket))
	     (set! cache-socket #f)
	     socket)
	  #f)))
        
;*---------------------------------------------------------------------*/
;*    cache-offer ...                                                  */
;*---------------------------------------------------------------------*/
(define (cache-offer host port socket)
   (synchronize cache-mutex
      (when (socket? cache-socket)
	 (socket-close cache-socket))
      (set! cache-host host)
      (set! cache-port port)
      (set! cache-socket socket)))

;*---------------------------------------------------------------------*/
;*    webdav-propfind ...                                              */
;*---------------------------------------------------------------------*/
(define (webdav-propfind url timeout proxy #!optional (header (webdav-header)))
   (let loop ((url url))
      (multiple-value-bind (proto login host port abspath)
	 (url-parse url)
	 (unless (string? host)
	    (raise
	     (instantiate::&io-malformed-url-error
		(proc 'webdav-propfind)
		(msg "missing host")
		(obj url))))
	 (let liip ((socket (cache-get host port)))
	    (let ((socket (http :method 'PROPFIND
			     :host host
			     :port port
			     :socket socket
			     :timeout timeout
			     :path abspath
			     :header header
			     :proxy proxy
			     :login login
			     :connection "keep-alive"
			     :body "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<propfind xmlns=\"DAV:\">
 <prop>
  <resourcetype xmlns=\"DAV:\"/>
  <getlastmodified xmlns=\"DAV:\"/>
  <getcontentlength xmlns=\"DAV:\"/>
 </prop>
</propfind>")))
	       (let ((l (with-handler
			   (lambda (e)
			      (socket-close socket)
			      (cond
				 ((and (socket? socket)
				       (isa? e &io-parse-error))
				  (liip #f))
				 ((isa? e &http-redirection)
				  (with-access::&http-redirection e (url)
				     (loop url)))
				 (else
				  (raise e))))
			   (http-parse-response (socket-input socket)
						(socket-output socket)
						(make-webdav-response-parser url)))))
		  (cache-offer host port socket)
		  l))))))

;*---------------------------------------------------------------------*/
;*    webdav-directory->path-list ...                                  */
;*    -------------------------------------------------------------    */
;*    Returns the list of files of a webdav repository. The            */
;*    files can be obtained with regular HTTP GET commands.            */
;*---------------------------------------------------------------------*/
(define (webdav-directory->path-list url #!key (timeout 0) (proxy #f))
   (multiple-value-bind (protocol userinfo host port _)
      (url-parse url)
      (map (lambda (x)
	      (if userinfo
		  (format "~a://~a@~a:~a~a" protocol userinfo host port
			  (webdav-prop-path x))
		  (format "~a://~a:~a~a" protocol host port
			  (webdav-prop-path x))))
	   (webdav-propfind url timeout proxy))))
       
;*---------------------------------------------------------------------*/
;*    webdav-directory->prop-list ...                                  */
;*    -------------------------------------------------------------    */
;*    Returns the list of properties of a webdav repository. The       */
;*    files can be obtained with regular HTTP GET commands.            */
;*---------------------------------------------------------------------*/
(define (webdav-directory->prop-list url #!key (timeout 0) (proxy #f))
   (multiple-value-bind (protocol userinfo host port _)
      (url-parse url)
      (map (lambda (x)
	      (let ((href (if userinfo
			      (format "~a://~a@~a:~a~a" protocol userinfo host port
				      (webdav-prop-path x))
			      (format "~a://~a:~a~a" protocol host port
				      (webdav-prop-path x)))))
		 (list href
		       :type (webdav-prop-type x)
		       :modified (webdav-prop-modified x)
		       :size (webdav-prop-size x))))
	   (webdav-propfind url timeout proxy))))
      
;*---------------------------------------------------------------------*/
;*    webdav-directory->list ...                                       */
;*---------------------------------------------------------------------*/
(define (webdav-directory->list url #!key (timeout 0) (proxy #f))
   (map (lambda (x)
	   (basename (webdav-prop-path x)))
	(webdav-propfind url timeout proxy)))

;*---------------------------------------------------------------------*/
;*    webdav-file-exists? ...                                          */
;*---------------------------------------------------------------------*/
(define (webdav-file-exists? url #!key (timeout 0) (proxy #f))
   (pair? (webdav-propfind url timeout proxy '((depth: 0)))))

;*---------------------------------------------------------------------*/
;*    webdav-directory? ...                                            */
;*---------------------------------------------------------------------*/
(define (webdav-directory? url #!key (timeout 0) (proxy #f))
   (let ((r (webdav-propfind url timeout proxy '((depth: 0)))))
      (and (pair? r) (eq? (webdav-prop-type (car r)) 'directory))))

;*---------------------------------------------------------------------*/
;*    webdav-file-modification-time ...                                */
;*---------------------------------------------------------------------*/
(define (webdav-file-modification-time url #!key (timeout 0) (proxy #f))
   (let ((r (webdav-propfind url timeout proxy '((depth: 1)))))
      (if (pair? r)
	  (date->seconds (rfc2822-date->date (webdav-prop-modified (car r))))
	  #e-1)))

;*---------------------------------------------------------------------*/
;*    webdav-file-size ...                                             */
;*---------------------------------------------------------------------*/
(define (webdav-file-size url #!key (timeout 0) (proxy #f))
   (let ((r (webdav-propfind url timeout proxy '((depth: 1)))))
      (if (pair? r)
	  (string->elong (webdav-prop-size (car r)))
	  #e-1)))

;*---------------------------------------------------------------------*/
;*    webdav-request ...                                               */
;*---------------------------------------------------------------------*/
(define (webdav-request method return url timeout proxy header body)
   (let loop ((url url))
      (multiple-value-bind (proto login host port abspath)
	 (url-parse url)
	 (unless (string? host)
	    (raise
	     (instantiate::&io-malformed-url-error
		(proc 'webdav-propfind)
		(msg "missing host")
		(obj url))))
	 (let liip ((socket (cache-get host port)))
	    (let ((socket (http :method method
			     :host host
			     :port port
			     :socket socket
			     :timeout timeout
			     :path abspath
			     :proxy proxy
			     :header header
			     :login login
			     :body body)))
	       (let ((l (with-handler
			   (lambda (e)
			      (socket-close socket)
			      (cond
				 ((and (socket? socket)
				       (isa? e &io-parse-error))
				  (liip #f))
				 ((isa? e &http-redirection)
				  (with-access::&http-redirection e (url)
				     (loop url)))
				 (else
				  (raise e))))
			   (http-parse-response (socket-input socket)
						(socket-output socket)
						(lambda (ip status header clen tenc)
						   (if (memq status return)
						       #t #unspecified))))))
		  (cache-offer host port socket)
		  l))))))

;*---------------------------------------------------------------------*/
;*    webdav-delete-file ...                                           */
;*---------------------------------------------------------------------*/
(define (webdav-delete-file url #!key (timeout 0) (proxy #f))
   (if (and (webdav-file-exists? url :timeout timeout :proxy proxy)
	    (not (webdav-directory? url :timeout timeout :proxy proxy)))
       (eq? (webdav-request 'DELETE '(200) url timeout proxy (webdav-header) #f)
	    #t)))

;*---------------------------------------------------------------------*/
;*    webdav-directory ...                                             */
;*---------------------------------------------------------------------*/
(define (webdav-delete-directory url #!key (timeout 0) (proxy #f))
   (if (and (webdav-file-exists? url :timeout timeout :proxy proxy)
	    (webdav-directory? url :timeout timeout :proxy proxy)
	    (null? (webdav-directory->path-list url :timeout timeout :proxy proxy)))
       (eq? (webdav-request 'DELETE '(200) url timeout proxy (webdav-header) #f)
	    #t)))

;*---------------------------------------------------------------------*/
;*    webdav-make-directory ...                                        */
;*---------------------------------------------------------------------*/
(define (webdav-make-directory url #!key (timeout 0) (proxy #f))
   (let ((len (string-length url)))
      (if (=fx len 0)
	  #f
	  (let ((url (if (char=? (string-ref url (-fx len 1)) #\/)
			 url
			 (string-append url "/"))))
	     (eq? (webdav-request 'MKCOL '(201) url timeout proxy (webdav-header) #f)
		  #t)))))

;*---------------------------------------------------------------------*/
;*    webdav-make-directories ...                                      */
;*---------------------------------------------------------------------*/
(define (webdav-make-directories url #!key (timeout 0) (proxy #f))
   (or (webdav-make-directory url)
       (multiple-value-bind (proto login host port abspath)
	  (url-parse url)
	  (let ((dpath (dirname abspath))
		(dname (dirname url)))
	     (if (or (string=? dpath "") (webdav-file-exists? dname))
		 #f
		 (begin
		    (webdav-make-directories dname)
		    (webdav-make-directory url)))))))

;*---------------------------------------------------------------------*/
;*    webdav-rename-file ...                                           */
;*---------------------------------------------------------------------*/
(define (webdav-rename-file url dst #!key (timeout 0) (proxy #f))
   (eq? (webdav-request 'MOVE '(201 204) url timeout proxy
			`((destination: ,dst) ,@(webdav-header)) #f) #t))

;*---------------------------------------------------------------------*/
;*    webdav-copy-file ...                                             */
;*---------------------------------------------------------------------*/
(define (webdav-copy-file url dst #!key (timeout 0) (proxy #f))
   (when (and (webdav-file-exists? url)
	      (not (webdav-directory? url)))
      (eq? (webdav-request 'COPY '(201 204) url timeout proxy
			   `((destination: ,dst) ,@(webdav-header)) #f) #t)))

;*---------------------------------------------------------------------*/
;*    webdav-put-file ...                                              */
;*---------------------------------------------------------------------*/
(define (webdav-put-file url obj #!key (timeout 0) (proxy #f))
   (eq? (webdav-request 'PUT '(201 204) url timeout proxy (webdav-header) obj)
	#t))

;*---------------------------------------------------------------------*/
;*    make-webdav-response-parser ...                                  */
;*---------------------------------------------------------------------*/
(define (make-webdav-response-parser url)
   (lambda (ip status header clen tenc)
      (let ((x (xml-parse ip
			  :content-length clen
			  :encoding 'ISO-8859-1
			  :procedure vector)))
	 (case status
	    ((207) (webdav-responses x))
	    ((200) (webdav-response x '()))
	    ((401) (raise (instantiate::&webdav-access-control-exception
			     (message "Authentication required")
			     (obj url)
			     (permission 401)
			     (header header))))
	    (else '())))))

;*---------------------------------------------------------------------*/
;*    xml-attribute-namespace ...                                      */
;*---------------------------------------------------------------------*/
(define (xml-attribute-namespace attributes)
   (let loop ((a attributes))
      (when (pair? a)
	 (let ((s (symbol->string (caar a))))
	    (if (substring-at? s "xmlns:" 0)
		(cons (string->symbol (substring s 6 (string-length s)))
		      (cdar a))
		(loop (cdr a)))))))

;*---------------------------------------------------------------------*/
;*    xml-markup/namespace ...                                         */
;*---------------------------------------------------------------------*/
(define (xml-markup/namespace m xmlns)
   (let* ((s (symbol->string m))
	  (i (string-index s #\:)))
      (if i
	  (let* ((prefix (string->symbol (substring s 0 i)))
		 (base (substring s (+fx i 1) (string-length s)))
		 (ns (assq prefix xmlns)))
	     (if (pair? ns)
		 (string->symbol (string-append (cdr ns) base))
		 m))
	  m)))

;*---------------------------------------------------------------------*/
;*    webdav-find-node ...                                             */
;*---------------------------------------------------------------------*/
(define (webdav-find-node xml xmlns search default)
   (let loop ((xml xml)
	      (xmlns xmlns)
	      (err #t))
      (cond
	 ((vector? xml)
	  (let* ((el xml)
		 (att (vector-ref el 1))
		 (ns (xml-attribute-namespace att))
		 (xmlns (if ns (cons ns xmlns) xmlns))
		 (m (xml-markup/namespace (vector-ref el 0) xmlns))
		 (body (vector-ref el 2)))
	     (if (eq? m search)
		 (values body xmlns)
		 (loop body xmlns #f))))
	 ((pair? xml)
	  (or (loop (car xml) xmlns #f)
	      (loop (cdr xml) xmlns err)))
	 (err
	  (or default
	      (raise (instantiate::&io-parse-error
			(obj xml)
			(proc 'webdav)
			(msg (format "Cannot find ~a node" search))))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    webdav-response ...                                              */
;*    -------------------------------------------------------------    */
;*    Traverse the response tree, handling the namespaces and          */
;*    stopping at a DAV:response markup.                               */
;*---------------------------------------------------------------------*/
(define (webdav-response xml xmlns)
   (multiple-value-bind (body xmlns)
      (webdav-find-node xml xmlns 'DAV:response #f)
      (let ((href (multiple-value-bind (href _)
		     (webdav-find-node body xmlns 'DAV:href #f)
		     (car href))))
	 (multiple-value-bind (prop xmlns2)
	    (webdav-find-node body xmlns 'DAV:propstat #f)
	    (let ((status (multiple-value-bind (status _)
			     (webdav-find-node prop xmlns2 'DAV:status #f)
			     (car status))))
	       (multiple-value-bind (_ code _)
		  (http-parse-status-line (open-input-string status))
		  (case code
		     ((200)
		      (let ((lm (multiple-value-bind (lastmodified _)
				   (webdav-find-node
				    prop xmlns2 'DAV:getlastmodified #f)
				   (car lastmodified)))
			    (clen (multiple-value-bind (clength _)
				     (webdav-find-node
				      prop xmlns2 'DAV:getcontentlength #f)
				     (car clength)))
			    (ty (multiple-value-bind (ty xmlns3)
				   (webdav-find-node
				    prop xmlns2 'DAV:resourcetype #f)
				   (cond
				      ((not ty)
				       'file)
				      ((multiple-value-bind (d _)
					  (webdav-find-node
					   ty xmlns3 'DAV:collection #t)
					  (not (eq? d #t)))
				       'directory)
				      (else
				       'file)))))
			 (webdav-prop href lm clen ty)))
		     ((404)
		      #f)
		     (else
		      (raise (instantiate::&io-parse-error
				(obj status)
				(proc 'webdav)
				(msg "Illegal status code")))))))))))

;*---------------------------------------------------------------------*/
;*    webdav-responses ...                                             */
;*---------------------------------------------------------------------*/
(define (webdav-responses xml)
   (multiple-value-bind (n xmlns)
      (webdav-find-node xml '() 'DAV:multistatus #f)
      (filter-map (lambda (n) (webdav-response n xmlns)) n)))
