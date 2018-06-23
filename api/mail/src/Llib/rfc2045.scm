;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/mail/src/Llib/rfc2045.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 30 12:51:46 2007                          */
;*    Last change :  Sat Jun 23 07:37:27 2018 (serrano)                */
;*    Copyright   :  2007-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This module implements encoder/decoder for quoted-printable as   */
;*    defined by the RFC 2045:                                         */
;*      Multipurpose Internet Mail Extensions                          */
;*      (MIME) Part One:                                               */
;*      Format of Internet Message Bodies                              */
;*    RFC 2045, may be found at:                                       */
;*      http://tools.ietf.org/html/rfc2045                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_rfc2045
 
   (import __mail_rfc2822)
   
   (export (quoted-printable-encode-port ::input-port ::output-port)
	   (quoted-printable-decode-port ::input-port ::output-port
					 #!optional rfc2047)
	   (quoted-printable-encode::bstring ::bstring)
	   (quoted-printable-decode::bstring ::bstring)

	   (mime-content-type-decode-port::pair-nil ::input-port)
	   (mime-content-type-decode::pair-nil ::bstring)

	   (mime-content-disposition-decode-port::pair-nil ::input-port)
	   (mime-content-disposition-decode::pair-nil ::bstring)

	   (mime-multipart-decode-port::pair-nil ::input-port ::bstring
						 #!optional recursive quiet)
	   (mime-multipart-decode::pair-nil ::bstring ::bstring
					    #!optional recursive quiet)))

;*---------------------------------------------------------------------*/
;*    quoted-printable-encode-port ...                                 */
;*---------------------------------------------------------------------*/
(define (quoted-printable-encode-port in out)
   (let loop ((c (read-byte in))
	      (count 0))
      (cond
	 ((eof-object? c)
	  #unspecified)
	 ((>fx count 72)
	  (display "=\r\n" out)
	  (loop c 0))
	 ((=fx c #x3d)
	  ;; "="
	  (display "=3D" out)
	  (loop (read-byte in) (+ count 3)))
	 ((and (>=fx count 72) (or (=fx c #x20) (=fx c #x09)))	
	  ;; space or tab
	  (write-byte c out)
	  (display "=\r\n" out)
	  (loop (read-byte in) 0))
	 ((=fx c #x0d)
	  ;; cr
	  (let ((c1 (read-byte in)))
	     (display "\r\n" out)
	     (loop (if (=fx c1 #x0a)
		       (read-byte in)
		       c1)
		   0)))
	 ((=fx c #x0a)
	  ;; lf
	  (display "\r\n" out)
	  (loop (read-byte in) 0))
	 ((and (<=fx #x21 c) (<=fx c #x7e))
	  ;; printable character
	  (write-byte c out)
	  (loop (read-byte in) (+ count 1)))
	 (else
	  (if (<fx c #x10)
	      (begin
		 (display "=0" out)
		 (display (string-ref "0123456789ABCDEF" c) out))
	      (begin
		 (display "=" out)
		 (display (string-ref "0123456789ABCDEF" (/fx c 16)) out)
		 (display (string-ref "0123456789ABCDEF" (modulofx c 16)) out)))
	  (loop (read-byte in) (+ count 3))))))

;*---------------------------------------------------------------------*/
;*    hexa ...                                                         */
;*---------------------------------------------------------------------*/
(define (hexa c)
   (cond
      ((and (<=fx c (char->integer #\9)) (>=fx c (char->integer #\0)))
       (-fx c (char->integer #\0)))
      ((and (<=fx c (char->integer #\F)) (>=fx c (char->integer #\A)))
       (+fx 10 (-fx c (char->integer #\A))))
      (else
       (+fx 10 (-fx c (char->integer #\a))))))

;*---------------------------------------------------------------------*/
;*    quoted-printable-grammar ...                                     */
;*---------------------------------------------------------------------*/
(define quoted-printable-grammar
   (regular-grammar ((XDIGIT (or digit (or (in ("AF")) (in ("af")))))
		     out rfc2047)
      ((+ (out #\= #\Newline #\Return #\?))
       (display (the-string) out)
       (ignore))
      ((+ (or #\Newline #\Return))
       (display (the-string) out)
       (ignore))
      ((: #\= XDIGIT XDIGIT)
       (let ((num1 (hexa (the-byte-ref 1)))
	     (num2 (hexa (the-byte-ref 2))))
	  (write-byte (+fx (*fx num1 16) num2) out)
	  (ignore)))
      ((: #\? #\= XDIGIT XDIGIT)
       (if rfc2047
	   (display (the-substring 2 (the-length)) out)
	   (let ((num1 (hexa (the-byte-ref 2)))
		 (num2 (hexa (the-byte-ref 3))))
	      (write-char #\? out)
	      (write-byte (+fx (*fx num1 16) num2) out)))
       (ignore))
      ((: #\= #\Newline)
       (ignore))
      ((: #\= (? #\Return) #\Newline)
       (ignore))
      ((: #\= (+ (or #\Tab #\Space)) (? #\Return) #\Newline)
       (ignore))
      (#\=
       (write-char #\= out)
       (ignore))
      (#\?
       (write-char #\? out)
       (ignore))
      ((: "?=" (? #\Return) #\Newline)
       (if rfc2047
	   #unspecified
	   (begin
	      (display "?" out)
	      (display (the-substring 2 (the-length)) out)
	      (ignore))))
      ("?="
       (if rfc2047
	   #unspecified
	   (begin
	      (display "?=" out)
	      (ignore))))
      (else
       #unspecified)))
       
;*---------------------------------------------------------------------*/
;*    quoted-printable-decode-port ...                                 */
;*---------------------------------------------------------------------*/
(define (quoted-printable-decode-port in out #!optional rfc2047)
   (read/rp quoted-printable-grammar in out rfc2047))

;*---------------------------------------------------------------------*/
;*    quoted-printable-encode ...                                      */
;*---------------------------------------------------------------------*/
(define (quoted-printable-encode str)
   (let ((pout (open-output-string))
	 (pin (open-input-string str)))
      (quoted-printable-encode-port pin pout)
      (close-input-port pin)
      (close-output-port pout)))

;*---------------------------------------------------------------------*/
;*    quoted-printable-decode ...                                      */
;*---------------------------------------------------------------------*/
(define (quoted-printable-decode str)
   (let ((pout (open-output-string))
	 (pin (open-input-string str)))
      (quoted-printable-decode-port pin pout)
      (close-input-port pin)
      (close-output-port pout)))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error proc message obj port)
   (raise
    (instantiate::&io-parse-error
       (proc proc)
       (msg message)
       (obj (if (char? obj)
		(string-append "{" (string obj) "}"
			       (let ((o (read-line port)))
				  (if (eof-object? o)
				      ""
				      o)))
		obj))
       (fname (input-port-name port))
       (location (input-port-position port)))))

;*---------------------------------------------------------------------*/
;*    token-grammar ...                                                */
;*---------------------------------------------------------------------*/
(define token-grammar
   (regular-grammar ((str (* (or (out #\" #\\) (: #\\ all)))) name)
      ((+ (in "+*-_./'%&" ("azAZ09")))
       (the-string))
      ((: #\" str #\")
       (the-substring 1 -1))
      ((: (+ (in #\Tab #\Space)) #\" str #\")
       (let* ((s (the-substring 1 -1))
	      (i (string-index s #\")))
	  (substring s i (string-length s))))
      (else
       (parse-error "mime-content-type-decode"
	  (format "Illegal value (~s)" name)
	  (the-failure)
	  (the-port)))))

;*---------------------------------------------------------------------*/
;*    parameter-grammar ...                                            */
;*---------------------------------------------------------------------*/
(define parameter-grammar
   (regular-grammar ()
      ((+ (in " \t\n\r"))
       (ignore))
      (#\;
       (ignore))
      ((: (+ (out "; \t\n\r=")) #\=)
       (let* ((name (string-downcase! (the-substring 0 -1)))
	      (val (read/rp token-grammar (the-port) name)))
	  (cons (cons (string->symbol name) val) (ignore))))
      ((: (+ (out "; \t\n\r=")) #\= (+ (or #\Space #\Return #\Newline)))
       (let* ((str (the-substring 0 -2))
	      (i (string-index str " \n"))
	      (name (if i
			(string-downcase! (substring str 0 i))
			(string-downcase! str)))
	      (val (read/rp token-grammar (the-port) name)))
	  (cons (cons (string->symbol name) val) (ignore))))
      ((: (+ (out "; \t\n\r="))
	  (+ (or #\Space #\Return #\Newline))
	  #\= (* (or #\Space #\Return #\Newline)))
       (let* ((str (the-substring 0 -1))
	      (i (string-index str " \n"))
	      (name (string-downcase! (substring str 0 i)))
	      (val (read/rp token-grammar (the-port) name)))
	  (cons (cons (string->symbol name) val) (ignore))))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      '()
	      (parse-error "mime-content-type-decode"
		 "Illegal parameter name"
		 (the-failure)
		 (the-port)))))))
	  
;*---------------------------------------------------------------------*/
;*    content-type-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define content-type-grammar
   (regular-grammar ()
      ((in " \t\n\r")
       (ignore))
      ((: (+ (in "-_." ("azAZ09"))) "/")
       (let* ((ty (string-downcase! (the-substring 0 -1)))
	      (sty (string-downcase! (read/rp token-grammar (the-port) ty)))
	      (parameters (read/rp parameter-grammar (the-port))))
	  (list (string->symbol ty) (string->symbol sty) parameters)))
      ((: "=?" (+ (out #\?)) "?" (in ("azAZ")) "?")
       ;; rfc2047 header to be skipped
       (ignore))
      (else
       (parse-error "mime-content-type-decode"
	  "Illegal main type"
	  (the-failure)
	  (the-port)))))
	  
;*---------------------------------------------------------------------*/
;*    mime-content-type-decode-port ...                                */
;*---------------------------------------------------------------------*/
(define (mime-content-type-decode-port in)
   (read/rp content-type-grammar in))

;*---------------------------------------------------------------------*/
;*    mime-content-type-decode ...                                     */
;*---------------------------------------------------------------------*/
(define (mime-content-type-decode str)
   (let ((p (open-input-string str)))
      (unwind-protect
	 (read/rp content-type-grammar p)
	 (close-input-port p))))

;*---------------------------------------------------------------------*/
;*    content-disposition-grammar ...                                  */
;*---------------------------------------------------------------------*/
(define content-disposition-grammar
   (regular-grammar ()
      ((in " \t\n\r")
       (ignore))
      ((: (+ (in "-_." ("azAZ09"))) ";")
       (let* ((dispo (string-downcase! (the-substring 0 -1)))
	      (parameters (read/rp parameter-grammar (the-port))))
	  (list (string->symbol dispo)  parameters)))
      ((: (+ (in "-_." ("azAZ09"))))
       (let ((dispo (string-downcase! (the-string))))
	  (list (string->symbol dispo) '())))
      (else
       (parse-error "parse-content-disposition"
	  "Illegal token"
	  (the-failure)
	  (the-port)))))

;*---------------------------------------------------------------------*/
;*    mime-content-disposition-decode-port ...                         */
;*---------------------------------------------------------------------*/
(define (mime-content-disposition-decode-port in)
   (read/rp content-disposition-grammar in))

;*---------------------------------------------------------------------*/
;*    mime-content-disposition-decode ...                              */
;*---------------------------------------------------------------------*/
(define (mime-content-disposition-decode str)
   (let ((p (open-input-string str)))
      (unwind-protect
	 (read/rp content-disposition-grammar p)
	 (close-input-port p))))

;*---------------------------------------------------------------------*/
;*    fill-line! ...                                                   */
;*---------------------------------------------------------------------*/
(define (fill-line! buffer port)
   (let ((len (string-length buffer)))
      (let loop ((i 0))
	 (if (>=fx i (-fx len 2))
	     (values i #f #f)
	     (let ((c (read-char port)))
		(if (eof-object? c)
		    (values i #f #t)
		    (begin
		       (string-set! buffer i c)
		       (cond
			  ((char=? c #\Return)
			   (let ((c2 (read-char port)))
			      (string-set! buffer (+fx i 1) c2)
			      (if (char=? c2 #\Newline)
				  (values i "\r\n" #f)
				  (loop (+fx i 2)))))
			  ((char=? c #\Newline)
			   (values i "\n" #f))
			  (else
			   (loop (+fx i 1)))))))))))
   
;*---------------------------------------------------------------------*/
;*    flush-line ...                                                   */
;*---------------------------------------------------------------------*/
(define (flush-line port)
   (let ((grammar (regular-grammar ((xall (or (out #\Return)
					      (: #\Return (out #\Newline))
					      #a000)))
		     ((: (* xall) #\Return #\Newline)
		      (the-substring 0 -2))
		     ((+ xall)
		      (the-string))
		     (else
		      (the-failure)))))
      (read/rp grammar port)))

;*---------------------------------------------------------------------*/
;*    is-boundary? ...                                                 */
;*---------------------------------------------------------------------*/
(define (is-boundary? line boundary)
   (and (>=fx (string-length line) (+fx 2 (string-length boundary)))
	(char=? (string-ref line 0) #\-)
	(char=? (string-ref line 1) #\-)
	(substring-at? line boundary 2)))

;*---------------------------------------------------------------------*/
;*    last-boundary? ...                                               */
;*---------------------------------------------------------------------*/
(define (last-boundary? line boundary)
   (let ((len (string-length boundary)))
      (and (>=fx (string-length line) (+fx 4 len))
	   (char=? (string-ref line 0) #\-)
	   (char=? (string-ref line 1) #\-)
	   (char=? (string-ref line (+fx 2 len)) #\-)
	   (char=? (string-ref line (+fx 3 len)) #\-))))

;*---------------------------------------------------------------------*/
;*    multipart-read-up-to-boundary ...                                */
;*---------------------------------------------------------------------*/
(define (multipart-read-up-to-boundary buffer in boundary)
   (let loop ((lines '())
	      (first #t))
      (multiple-value-bind (len crlf eof)
	 (fill-line! buffer in)
	 (if (or (is-boundary? buffer boundary) eof)
	     (begin
		(unless crlf (flush-line in))
		(values (or (last-boundary? buffer boundary) eof)
			(apply string-append (reverse! lines))))
	     (if crlf
		 (loop (cons* crlf (substring buffer 0 len) lines) #f)
		 (loop (cons (substring buffer 0 len) lines) #f))))))

;*---------------------------------------------------------------------*/
;*    multipart-parse-entry ...                                        */
;*---------------------------------------------------------------------*/
(define (multipart-parse-entry buffer in boundary recursive quiet)
   (let loop ((entries '()))
      (multiple-value-bind (len crlf eof)
	 (fill-line! buffer in)
	 (if (is-boundary? buffer boundary)
	     (let laap ((entries entries))
		(let* ((header (with-handler
				  ;; skip silently incorrect headers
				  (lambda (e) '())
				  (mail-header->list in)))
		       (enc (let ((c (assq 'content-transfer-encoding header)))
			       (if (pair? c)
				   (string->symbol
				    (string-downcase (cdr c)))
				   'plain)))
		       (ctype (let ((c (assq 'content-type header)))
				 (if (pair? c)
				     (with-handler
					(lambda (e)
					   (if (isa? e &io-parse-error)
					       (begin
						  (when (and (>fx (bigloo-warning) 0) (not quiet))
						     (display "*** WARNING:multipart"
							(current-error-port))
						     (display " Illegal mimetype ("
							(current-error-port))
						     (if (isa? e &error)
							 (with-access::&error e (msg obj)
							    (display msg (current-error-port))
							    (display " [" (current-error-port))
							    (write obj (current-error-port))
							    (display " ]" (current-error-port)))
							 (display (typeof e) (current-error-port)))
						     (display ") \""
							(current-error-port))
						     (display (cdr c)
							(current-error-port))
						     (display "\", assuming text/plain.\n"
							(current-error-port)))
						  '(text plain))
					       (raise e)))
					(mime-content-type-decode (cdr c)))
				     '(text plain))))
		       (dispo (let ((c (assq 'content-disposition header)))
				 (if (and (pair? c) (string? (cdr c)))
				     (mime-content-disposition-decode (cdr c))
				     '(inline)))))
		   (cond
		      ((and recursive (eq? (car ctype) 'multipart))
		       ;; a recursive multipart
		       (let ((nboundary (assq 'boundary (caddr ctype))))
			  (if (not nboundary)
			      (multiple-value-bind (last content)
				 (multipart-read-up-to-boundary buffer in boundary)
				 (let* ((b `((boundary . ,boundary)))
					(a (append (caddr ctype) b))
					(mtype (list (car ctype) 'error a))
					(entry (list mtype enc dispo content)))
				    (if last
					(reverse! (cons entry entries))
					(laap (cons entry entries)))))
			      (let* ((b (cdr nboundary))
				     (p (mime-multipart-decode-port in b))
				     (e (list ctype enc dispo p)))
				 (multiple-value-bind (last _)
				    (multipart-read-up-to-boundary buffer in boundary)
				    (if last
					(reverse! (cons e entries))
					(laap (cons e entries))))))))
		      (else
		       (multiple-value-bind (last content)
			  (multipart-read-up-to-boundary buffer in boundary)
			  (let ((entry (list ctype enc dispo content header)))
			     (if last
				 (reverse! (cons entry entries))
				 (laap (cons entry entries)))))))))
	     ;; for now ignore non boundary lines
	     (if eof
		 '()
		 (loop entries))))))
   
;*---------------------------------------------------------------------*/
;*    mime-multipart-decode-port ...                                   */
;*---------------------------------------------------------------------*/
(define (mime-multipart-decode-port in boundary #!optional recursive quiet)
   (let ((buffer (make-string (+fx (string-length boundary) 256))))
      (multipart-parse-entry buffer in boundary recursive quiet)))

;*---------------------------------------------------------------------*/
;*    mime-multipart-decode ...                                        */
;*---------------------------------------------------------------------*/
(define (mime-multipart-decode str boundary #!optional recursive quiet)
   (let ((in (open-input-string str)))
      (unwind-protect
	 (mime-multipart-decode-port in boundary recursive quiet)
	 (close-input-port in))))
