;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/http.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  9 15:02:05 2007                          */
;*    Last change :  Thu Oct 13 11:56:13 2016 (serrano)                */
;*    Copyright   :  2007-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dealing with HTTP requests                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __http

   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
	   __bit
	   __bignum
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_numbers_6_5_flonum_dtoa
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __r4_input_6_10_2
	   __r4_output_6_10_3
	   __r5_control_features_6_4
	   __foreign
	   __error
	   __evenv
	   __os
	   __structure
	   __param
	   __reader)

   (import __url
	   __rgc
	   __base64
	   __socket)
   
   (export (class &http-error::&error)
	   
	   (class &http-redirection-error::&http-error)
	   (class &http-status-error::&http-error
	      (status::int read-only))
	   
	   (class &http-redirection::&exception
	      (port::input-port read-only)
	      (url::bstring read-only))
	   
	   (http #!key
		 (in #f) (out #f) (socket #f)
		 (protocol 'http)
		 (method 'get)
		 (timeout 0)
		 (proxy #f)
		 (host "localhost") (port 80)
		 (path "/")
		 (login #f) (authorization #f) (username #f) (password #f)
		 (http-version "HTTP/1.1")
		 (content-type #f)
		 (connection #unspecified)
		 (header '((user-agent: "Mozilla/5.0")))
		 (args '())
		 (body #f))

	   (http-read-line ::input-port)
	   (http-read-crlf ::input-port)
	   (http-parse-status-line ::input-port)
	   (http-parse-header ::input-port ::obj)
	   (http-parse-response ::input-port ::obj ::procedure)
	   (http-response-body->port::input-port ::input-port ::output-port)
	   (http-chunks->procedure::procedure ::input-port)
	   (http-chunks->port::input-port ::input-port)
	   (http-send-chunks ::input-port ::output-port ::bool)))

;*---------------------------------------------------------------------*/
;*    display-line ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (display-line . args)
   (let* ((sgra (reverse args))
	  (port (car sgra))
	  (vals (reverse (cdr sgra))))
      `(begin
	  ,@(map (lambda (a) `(display ,a ,port)) vals)
	  (display "\r\n" ,port))))

;*---------------------------------------------------------------------*/
;*    http ...                                                         */
;*    -------------------------------------------------------------    */
;*    Establishes a HTTP connection with a remote host                 */
;*---------------------------------------------------------------------*/
(define (http #!key
	   (in #f)
	   (out #f)
	   (socket #f)
	   (protocol 'http)
	   (method 'get)
	   (timeout 0)
	   (proxy #f)
	   (host "localhost") (port 80)
	   (path "/")
	   (login #f) (authorization #f) (username #f) (password #f)
	   (http-version "HTTP/1.1")
	   (content-type #f)
	   (connection #unspecified)
	   (header '((user-agent: "Mozilla/5.0")))
	   (args '())
	   (body #f))
   ;; preliminary checks
   (cond
      (socket
       (set! in (socket-input socket))
       (set! out (socket-output socket)))
      ((and (not in) (not out))
       (unless (and host port)
	  (error "http" "Missing either \"host\" or \"port\" argument" host))
       (set! socket (make-http-socket host port proxy timeout))
       (set! in (socket-input socket))
       (set! out (socket-output socket)))
      ((not in)
       (error "http" "Missing either \"in\" or \"socket\" argument" in))
      ((not out)
       (error "http" "Missing \"out\" argument" out)))
   ;; header line
   (if (string? proxy)
       (display-proxy-method method host port path http-version out)
       (display-direct-method method host port path http-version out))
   ;; host and port
   (if (or (and (=fx port 80) (eq? protocol 'http))
	   (and (=fx port 443) (eq? protocol 'https)))
       (display-line "Host: " host out)
       (display-line "Host: " host ":" port out))
   ;; user additional header
   (for-each (lambda (h)
		(display-line (keyword->string (car h)) ": "
		   (if (pair? (cdr h)) (cadr h) (cdr h))
		   out))
      header)
   ;; authentication
   (cond
      ((string? login)
       (display-authentication login out))
      ((string? authorization)
       (display-line "Authorization: " authorization out))
      ((and (string? username) (string? password))
       (display-authentication (string-append username ":" password) out)))
   ;; connection keep-alive
   (when (string? connection)
      (display-line "Connection: " connection out))
   (cond
      ((and (or (eq? method 'post) (eq? method 'POST))
	    (or (eq? content-type 'multipart/form-data)
		(pair? args)))
       ;; post method
       (cond
	  ((eq? content-type 'multipart/form-data)
	   (let* ((boundary (generate-http-boundary))
		  (content (make-http-post-body boundary args))
		  (content-length (apply + (map string-length content))))
	      (display-line "Content-Length: " content-length out)
	      (display-line "Content-Type: multipart/form-data; boundary="
		 (substring boundary 2 (string-length boundary)) out)
	      (display-line out)
	      (for-each (lambda (o) (display-string o out)) content)))
	  (else
	   (let ((content (x-www-form-urlencode args))
		 (ct (or content-type "application/x-www-form-urlencoded")))
	      (display-line "Content-Type: " ct out)
	      (display-line "Content-Length: " (string-length content) out)
	      (display-line out)
	      (display content out)
	      (display-line out)))))
      ((string? body)
       ;; a request with a fixed length body
       (display-line "Content-Length: " (string-length body) out)
       (display-line out)
       (display body out))
      ((input-port? body)
       ;; a request with a variable length body
       (display-line out)
       (send-chars body out))
      ((procedure? body)
       (display-line out)
       (body out))
      (else
       ;; a request without a body
       (display-line out)))
   (flush-output-port out)
   socket)

;*---------------------------------------------------------------------*/
;*    make-http-socket ...                                             */
;*---------------------------------------------------------------------*/
(define (make-http-socket host port proxy timeout)
   (when (string? proxy)
      (let ((i (string-index proxy #\:)))
	 (if i
	     (begin
		(set! host (substring proxy 0 i))
		(set! port (string->integer
			      (substring proxy (+fx i 1) (string-length proxy)))))
	     (begin
		(set! host proxy)
		(set! port 80)))))
   (cond
      ((not (string? host))
       (bigloo-type-error 'http "string" host))
      ((not (integer? port))
       (bigloo-type-error 'http "integer" port))
      (else
       (let ((s (make-client-socket host port :timeout timeout)))
	  ;(socket-option-set! s :SO_RCVTIMEO timeout)
	  s))))

;*---------------------------------------------------------------------*/
;*    generate-http-post-body ...                                      */
;*---------------------------------------------------------------------*/
(define (generate-http-post-body boundary args)
   (let ((port (open-output-string)))
      (if (null? args)
	  (begin
	     (display-line port)
	     (close-output-port port))
	  (let loop ((args args))
	     (if (null? args)
		 (begin
		    (display-line boundary "--" port)
		    (close-output-port port))
		 (let ((a (car args)))
		    (display-line boundary port)
		    (if (pair? (car a))
			(display-line "Content-Disposition: form-data; name=\""
			   (caar a) "\";" (cadar a) port)
			(display-line "Content-Disposition: form-data; name=\""
			   (car a) "\"" port))
		    (when (pair? (cddr a)) (display-line (caddr a) port))
		    (display-line port)
		    (display-line (cadr a) port)
		    (loop (cdr args))))))))

;*---------------------------------------------------------------------*/
;*    make-http-post-body ...                                          */
;*---------------------------------------------------------------------*/
(define (make-http-post-body boundary args)
   
   (define (->string a)
      (if (string? a)
	  a
	  (call-with-output-string (lambda (p) (display a p)))))
   
   (if (null? args)
       '("\r\n")
       (let loop ((args args))
	  (if (null? args)
	      (list boundary "--" "\r\n")
	      (let* ((a (car args))
		     (body (cons* (->string (cadr a)) "\r\n"
			      (loop (cdr args))))
		     (hd (if (pair? (cddr a))
			     (cons* (caddr a) "\r\n\r\n" body)
			     (cons "\r\n" body)))
		     (disp (if (pair? (car a))
			       (cons* "Content-Disposition: form-data; name=\""
				  (caar a) "\";" (cadar a) "\r\n"
				  hd)
			       (cons* "Content-Disposition: form-data; name=\""
				  (car a) "\"\r\n"
				  hd))))
		 (cons* boundary "\r\n" disp))))))
   
;*---------------------------------------------------------------------*/
;*    generate-http-boundary ...                                       */
;*---------------------------------------------------------------------*/
(define (generate-http-boundary)
   (let ((s (make-string 22 #\-))
	 (chars "0123456789abcdef"))
      (let loop ((i 2))
	 (when (<fx i 22)
	    (let ((num (random 16)))
	       (string-set! s i (string-ref chars num))
	       (loop (+fx i 1)))))
      s))

;*---------------------------------------------------------------------*/
;*    display-proxy-method ...                                         */
;*---------------------------------------------------------------------*/
(define (display-proxy-method method server port path http-version out)
   (display (string-upcase (symbol->string! method)) out)
   (display-line " http://" server ":" port path " " http-version out))

;*---------------------------------------------------------------------*/
;*    display-direct-method ...                                        */
;*---------------------------------------------------------------------*/
(define (display-direct-method method server port path http-version out)
   (display (string-upcase (symbol->string! method)) out)
   (display-line " " path " " http-version out))
   
;*---------------------------------------------------------------------*/
;*    display-authentication ...                                       */
;*---------------------------------------------------------------------*/
(define (display-authentication login out)
   (let ((uinfo (base64-encode login -1)))
      (display-line "Authorization: Basic " uinfo out)))
		 
;*---------------------------------------------------------------------*/
;*    http-parse-error-msg ...                                         */
;*---------------------------------------------------------------------*/
(define (http-parse-error-msg c port)
   (if (char? c)
       (let ((line (http-read-line port)))
	  (string-for-read
	     (string-append "{" (string c) "}" (if (string? line) line ""))))
       c))

;*---------------------------------------------------------------------*/
;*    status-line-grammar ...                                          */
;*---------------------------------------------------------------------*/
(define status-line-grammar
   (regular-grammar ((SP #\Space)
		     (HTTP (: (+ (in "httpsHTTPS"))
			      #\/ (+ digit) #\. (+ digit)))
		     (ICY "ICY")
		     (CODE (+ (in digit)))
		     (line (or (: (+ all) "\r\n") (: (+ all) "\n") (+ all))))
      ((: (or HTTP ICY) SP)
       (let ((http (the-substring 0 (-fx (the-length) 1))))
	  (let ((code (http-read-fixnum (the-port))))
	     (http-skip-blank (the-port))
	     (values http code (http-read-line (the-port))))))
      (else
       (let ((c (the-failure)))
	  (raise 
	     (if (eof-object? c)
		 (instantiate::&io-parse-error
		    (obj (the-port))
		    (proc "http-parse-status-line")
		    (msg "Illegal status line, premature end of input"))
		 (instantiate::&io-parse-error
		    (obj (http-parse-error-msg c (the-port)))
		    (proc "http-parse-status-line")
		    (msg "Illegal status line"))))))))

;*---------------------------------------------------------------------*/
;*    http-parse-status-line ...                                       */
;*    -------------------------------------------------------------    */
;*    The syntax of the status (section 6.1 of http/1.1) is defined    */
;*    as follows:                                                      */
;*    Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF  */
;*---------------------------------------------------------------------*/
(define (http-parse-status-line ip)
   (read/rp status-line-grammar ip))

;*---------------------------------------------------------------------*/
;*    http-read-line ...                                               */
;*---------------------------------------------------------------------*/
(define (http-read-line p)
   (read/rp (regular-grammar ()
	       ((or (: (+ all) "\r\n") (: (+ all) "\n") (+ all))
		(the-string))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       c
		       (the-string)))))
      p))

;*---------------------------------------------------------------------*/
;*    http-skip-blank ...                                              */
;*---------------------------------------------------------------------*/
(define (http-skip-blank p)
   (read/rp (regular-grammar ()
	       ((+ (in " \t")) #unspecified)
	       (else
		(raise
		   (instantiate::&io-parse-error
		      (obj (http-parse-error-msg (the-failure) (the-port)))
		      (proc "http")
		      (msg "Illegal separator")))))
      p))

;*---------------------------------------------------------------------*/
;*    http-skip-line ...                                               */
;*---------------------------------------------------------------------*/
(define (http-skip-line p)
   (read/rp (regular-grammar ()
	       ((or (: (+ all) "\r\n") (: (+ all) "\n") (+ all))
		#f)
	       (else
		(let ((c (the-failure)))
		   (when (eof-object? c) c))))
      p))

;*---------------------------------------------------------------------*/
;*    http-read-fixnum ...                                             */
;*---------------------------------------------------------------------*/
(define (http-read-fixnum p)
   (read/rp (regular-grammar ((DIGIT (in ("09"))))
	       ((+ DIGIT) (the-fixnum))
	       ((+ (in " \t")) (ignore))
	       (else
		(raise
		   (instantiate::&io-parse-error
		      (obj (http-parse-error-msg (the-failure) (the-port)))
		      (proc "http")
		      (msg "Illegal integer")))))
      p))

;*---------------------------------------------------------------------*/
;*    http-parse-header ...                                            */
;*---------------------------------------------------------------------*/
(define (http-parse-header p po)
   
   (define value-grammar
      (regular-grammar ()
	 ((+ (in " \t"))
	  (ignore))
	 ((: (out " \t\r\n") (* (or (out "\r\n") (: "\r" (out "\n")))) "\r\n")
	  (the-substring 0 -2))
	 ((: (out " \t\r\n") (* (or (out "\r\n") (: "\r" (out "\n")))) "\n")
	  (the-substring 0 -1))
	 ((: (? #\Return) #\Newline)
	  "")
	 (else
	  "")))
   
   (define blank-grammar
      (regular-grammar ()
	 ((+ (in " \t")) (ignore))))
   
   (define hostname-grammar
      (regular-grammar ()
	 ((: (+ (out ":\n\r\t ")) #\:)
	  (let* ((h (the-substring 0 -1))
		 (p (http-read-fixnum (the-port))))
	     (values h p)))
	 ((+ (out ":\n\r\t "))
	  (values (the-string) #f))
	 ((+ (in " \t"))
	  (ignore))))
   
   (define name-grammar
      (regular-grammar ()
	 ((+ (out "\n\r\t ")) (the-string))
	 ((+ (in " \t")) (ignore))))
   
   (define elong-grammar
      (regular-grammar ((DIGIT (in ("09"))))
	 ((+ DIGIT) (fixnum->elong (the-fixnum)))
	 ((+ (in " \t")) (ignore))))
   
   (define symbol-grammar
      (regular-grammar ()
	 ((+ (or alpha #\-)) (the-downcase-symbol))
	 ((+ (in " \t")) (ignore))))
   
   (define symbol+-grammar
      (regular-grammar ()
	 ((: (+ (or alpha #\-)) "\r\n")
	  (the-downcase-subsymbol 0 -2))
	 ((: (+ (or alpha #\-))
	     (* (: "," (* (in " \t")) (+ (or alpha #\-)))) "\r\n")
	  (the-downcase-subsymbol 0 -2))
	 ((+ (in " \t"))
	  (ignore))
	 (else
	  '||)))
   
   (define auth-grammar
      (regular-grammar ()
	 ((: (+ (in #\Space #\Tab)))
	  (ignore))
	 ((: (out #\Space #\Tab) (* (out "\n\r")))
	  (the-string))
	 (else
	  #f)))
   
   (define crlf-grammar
      (regular-grammar ()
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  #unspecified)
	 (else
	  #f)))
   
   (define upto-crlf-grammar
      (regular-grammar ()
	 ((: (* (out #\Return #\Newline)) (? #\Return) #\Newline)
	  #unspecified)
	 (else
	  #f)))
   
   (define header-grammar
      (regular-grammar (po
			header
			hostname port content-length transfer-encoding
			authorization proxy-authorization connection)
	 ((: (+ (or (out " :\r\n\t") (: #\space (out #\:)))) #\:)
	  (let* ((k (the-downcase-keyword)))
	     (case k
		((host:)
		 (multiple-value-bind (h p)
		    (read/rp hostname-grammar (the-port))
		    (set! hostname h)
		    (set! port p)
		    (read/rp crlf-grammar (the-port))
		    (let ((host (if (fixnum? p)
				    (string-append h ":" (integer->string p))
				    h)))
		       (set! header (cons (cons k host) header))
		       (ignore))))
		((content-length:)
		 ;; Some web server uses extra characters after the length
		 ;; in bytes (wakka uses things such as 12345bytes). This
		 ;; is incorrect with respect to HTTP/1.1 but it seems that
		 ;; regular web crawlers accept this extension...
		 (set! content-length (read/rp elong-grammar (the-port)))
		 (read/rp upto-crlf-grammar (the-port))
		 (set! header (cons (cons k content-length) header))
		 (ignore))
		((transfer-encoding:)
		 (set! transfer-encoding (read/rp symbol-grammar (the-port)))
		 (read/rp crlf-grammar (the-port))
		 (set! header (cons (cons k transfer-encoding) header))
		 (ignore))
		((authorization:)
		 (set! authorization (read/rp auth-grammar (the-port)))
		 (read/rp crlf-grammar (the-port))
		 (set! header (cons (cons k authorization) header))
		 (ignore))
		((connection:)
		 (set! connection (read/rp symbol+-grammar (the-port)))
		 (set! header (cons (cons k connection) header))
		 (ignore))
		((proxy-authorization:)
		 (set! proxy-authorization (read/rp auth-grammar (the-port)))
		 (read/rp crlf-grammar (the-port))
		 ;; don't store the proxy-authorization in the header
		 (ignore))
		((expect:)
		 (let ((e (read/rp value-grammar (the-port))))
		    (cond
		       ((not (output-port? po))
			(error "expect-header"
			   "Cannot honnor message because output-port is #f"
			   po))
		       ((string=? e "100-continue")
			(fprint po "HTTP/1.1 100 Continue\r\n\r\n")
			(flush-output-port po)
			(ignore))
		       (else
			(fprint po "HTTP/1.1 417 Expectation Failed\r\n\r\n")
			(flush-output-port po)
			(raise
			   (instantiate::&io-parse-error
			      (obj (the-port))
			      (proc "expect-header")
			      (msg (format "Expectation failed (~a)" e))))))))
		(else
		 (let ((v (read/rp value-grammar (the-port))))
		    (set! header (cons (cons k v) header))
		    (ignore))))))
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  (values (reverse! header)
	     hostname
	     port
	     content-length
	     transfer-encoding
	     authorization
	     proxy-authorization
	     connection))
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 ;; some (bugous?) HTTP server don't send the appropriate
		 ;; CRLF when the body is empty
		 (values (reverse! header)
		    hostname
		    port
		    content-length
		    transfer-encoding
		    authorization
		    proxy-authorization
		    connection)
		 (raise (instantiate::&io-parse-error
			   (obj (list (reverse! header) hostname
				   port content-length
				   transfer-encoding authorization
				   proxy-authorization connection))
			   (proc "http-parse-header")
			   (msg (format "Illegal characters: ~a"
				   (http-parse-error-msg
				      (the-failure) (the-port)))))))))))
   
   (read/rp header-grammar p
      po    ;; output port
      '()   ;; header
      #f    ;; hostname
      #f    ;; port
      #e-1  ;; content-length
      #f    ;; transfer-encoding
      #f    ;; authorization
      #f    ;; proxy-authorization
      #f))  ;; connection

;*---------------------------------------------------------------------*/
;*    http-parse-response ...                                          */
;*---------------------------------------------------------------------*/
(define (http-parse-response ip op proc)
   (multiple-value-bind (_1 status _2)
      (http-parse-status-line ip)
      (multiple-value-bind (header _host _port clen tenc _aut _paut _conn)
	 (http-parse-header ip op)
	 (case status
	    ((200 207)
	     ;; ok
 	     (cond
 		((eq? tenc 'chunked)
		 (proc (http-chunks->port ip) status header clen tenc))
		(else
		 (proc ip status header clen tenc))))
	    ((201 204 304)
	     ;; no message body
	     (proc #f status header clen tenc))
	    ((301 302 303 307)
	     ;; redirection
	     (let ((loc (assq location: header)))
		(if (not (pair? loc))
		    (raise (instantiate::&http-redirection-error
			      (proc 'http-parse-body)
			      (msg "No URL for redirection!")
			      (obj ip)))
		    (raise (instantiate::&http-redirection
			      (port ip)
			      (url (cdr loc)))))))
	    (else
	     (or (proc ip status header clen tenc)
		 (raise (instantiate::&http-status-error
			   (proc 'http-parse-response)
			   (msg (format "Bad status code: ~a" status))
			   (obj ip)
			   (status status)))))))))

;*---------------------------------------------------------------------*/
;*    http-response-body->port ...                                     */
;*---------------------------------------------------------------------*/
(define (http-response-body->port ip op)
   (define (parse-body ip status-code header clen tenc)
      (cond
	 ((not (input-port? ip))
	  (open-input-string ""))
	 (clen
	  (let ((p (open-input-procedure (barrier-port ip clen))))
	     (input-port-close-hook-set! p (lambda (in) (close-input-port ip)))
	     p))
	 (else
	  ip)))
   (http-parse-response ip op parse-body))

;*---------------------------------------------------------------------*/
;*    http-read-crlf ...                                               */
;*---------------------------------------------------------------------*/
(define (http-read-crlf p)
   (define crlf-grammar
      (regular-grammar ()
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  "\r\n")
	 (else
	  (raise (instantiate::&io-parse-error
		    (proc 'http-read-crlf)
		    (msg "Illegal character")
		    (obj (http-parse-error-msg (the-failure) (the-port))))))))
   (read/rp crlf-grammar p))

;*---------------------------------------------------------------------*/
;*    *chunk-size-grammar* ...                                         */
;*---------------------------------------------------------------------*/
(define *chunk-size-grammar*
   (regular-grammar ((SZ (+ xdigit))
		     (BLANK (in " \t"))
		     (CRLF "\r\n")
		     op)
      ((: SZ (* BLANK) #\;)
       (when op (display (the-string) op))
       (let ((sz (string->integer
		    (the-substring 0 (-fx (the-length) 1))
		    16)))
	  (read/rp (regular-grammar ((CRLF "\r\n"))
		      ((: (+ (or (+ (out "\r")) (+ (: "\r" (out "\n"))))) CRLF)
		       (when op (display (the-string) op)))
		      (else
		       (raise (instantiate::&io-parse-error
				 (proc 'chunks)
				 (msg "Illegal character")
				 (obj (http-parse-error-msg
					 (the-failure) (the-port)))))))
	     (the-port))
	  sz))
      ((: SZ (* BLANK) CRLF)
       (when op (display (the-string) op))
       (let ((l (the-length)))
	  (string->integer (the-substring 0 (-fx l 2)) 16)))
      (else
       (let* ((c1 (the-failure))
	      (c2 (read-char (the-port)))
	      (c3 (read-char (the-port)))
	      (c4 (read-char (the-port)))
	      (c5 (read-char (the-port))))
	  (raise (instantiate::&io-parse-error
		    (proc 'chunks)
		    (msg "Illegal chunk size")
		    (obj (if (or (eof-object? c1)
				 (eof-object? c2)
				 (eof-object? c3)
				 (eof-object? c4)
				 (eof-object? c5))
			     "#<eof-object>"
			     (string-for-read (string c1 c2 c3 c4 c5))))))))))

;*---------------------------------------------------------------------*/
;*    *buffer-length* ...                                              */
;*---------------------------------------------------------------------*/
(define *buffer-length* 8192)

;*---------------------------------------------------------------------*/
;*    barrier-port ...                                                 */
;*---------------------------------------------------------------------*/
(define (barrier-port port content-length)
   (let ((buf (make-string *buffer-length*)))
      (lambda ()
	 (when (>elong content-length #e0)
	    (let* ((n (minfx *buffer-length* (elong->fixnum content-length)))
		   (m (read-chars! buf n port)))
	       (set! content-length (-elong content-length (fixnum->elong m)))
	       (if (<fx m *buffer-length*)
		   (substring buf 0 m)
		   buf))))))

;*---------------------------------------------------------------------*/
;*    http-chunks->procedure ...                                       */
;*---------------------------------------------------------------------*/
(define (http-chunks->procedure ip::input-port)
   (let* ((state 'size)
	  (sz 0)
	  (bufsz 512)
	  (buffer (make-string bufsz #a000)))
      (lambda ()
 	 (let loop ()
	    (case state
	       ((eof)
		#f)
	       ((trailer)
		(let ((l (http-read-line ip)))
		   (cond
		      ((eof-object? l) 
		       (set! state 'eof)
		       "")
		      ((or (string=? l "\r\n") (string=? l "\n"))
		       (set! state 'eof)
		       l)
		      (else
		       l))))
	       ((chunk)
		(cond
		   ((=fx sz 0)
		    (http-read-crlf ip)
		    (set! state 'size)
		    (loop))
		   ((<fx sz bufsz)
		    (let ((s (read-chars sz ip)))
		       (set! sz (-fx sz (string-length s)))
		       s))
		   (else
		    (let ((s (read-chars! buffer bufsz ip)))
		       (set! sz (-fx sz s))
		       (if (=fx s bufsz)
			   buffer
			   (substring buffer 0 s))))))
	       (else
		(set! sz (read/rp *chunk-size-grammar* ip #f))
		(if (>fx sz 0)
		    ;; a regular chunk
		    (begin
		       (set! state 'chunk)
		       (loop))
		    ;; the last chunk starting with an optional trailer
		    (begin
		       (set! state 'trailer)
		       (loop)))))))))

;*---------------------------------------------------------------------*/
;*    http-chunks->port ...                                            */
;*---------------------------------------------------------------------*/
(define (http-chunks->port ip)
   (let ((ip2 (open-input-procedure (http-chunks->procedure ip))))
      (input-port-close-hook-set! ip (lambda (in) (close-input-port ip)))
      ip2))

;*---------------------------------------------------------------------*/
;*    http-send-chunks ...                                             */
;*---------------------------------------------------------------------*/
(define (http-send-chunks ip::input-port op::output-port trailer::bool)
   (let loop ()
      (let ((sz (read/rp *chunk-size-grammar* ip op)))
	 (if (>fx sz 0)
	     ;; a regular chunk
	     (begin
		(let loop ((sz sz))
		   (when (>fx sz 0)
		      (let ((s (send-chars ip op sz)))
			 (when (>fx s 0)
			    (loop (-fx sz s))))))
		(flush-output-port op)
		(let ((s (http-read-crlf ip)))
		   (display s op)
		   (loop)))
	     ;; the last chunk starting with an optional trailer
	     (if trailer
		 (let loop ()
		    (let ((l (http-read-line ip)))
		       (if (eof-object? l)
			   (flush-output-port op)
			   (begin
			      (display l op)
			      (if (>fx (string-length l) 2)
				  (loop)
				  (flush-output-port op))))))
		 (begin
		    (display (http-read-line ip) op)
		    (flush-output-port op)))))))
