;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/cgi.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 16 11:17:40 2003                          */
;*    Last change :  Tue Jan  6 09:27:18 2015 (serrano)                */
;*    Copyright   :  2003-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    CGI scripts handling                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_cgi

   (export (cgi-args->list::pair-nil ::bstring)
	   (cgi-fetch-arg ::bstring ::bstring)
	   (cgi-multipart->list ::bstring ::input-port ::elong ::bstring)
	   (cgi-post-arg-field ::obj ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    unhex ...                                                        */
;*---------------------------------------------------------------------*/
(define (unhex hexadecimal-string)
   (string (integer->char (string->integer hexadecimal-string 16))))

;*---------------------------------------------------------------------*/
;*    hex-string-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (hex-string-ref str i)
   (let ((n (string-ref-ur str i)))
      (cond
	 ((and (char>=? n #\0) (char<=? n #\9))
	  (-fx (char->integer n) (char->integer #\0)))
	 ((and (char>=? n #\a) (char<=? n #\f))
	  (+fx 10 (-fx (char->integer n) (char->integer #\a))))
	 (else
	  (+fx 10 (-fx (char->integer n) (char->integer #\A)))))))

;*---------------------------------------------------------------------*/
;*    unhex+! ...                                                      */
;*---------------------------------------------------------------------*/
(define (unhex+! hexstr)
   (let ((len (string-length hexstr)))
      (let loop ((i 0)
		 (j 0))
	 (if (=fx i len)
	     (string-shrink! hexstr j)
	     (let ((c (string-ref hexstr i)))
		(cond
		   ((char=? c #\%)
		    (let* ((c1 (hex-string-ref hexstr (+fx i 1)))
			   (c2 (hex-string-ref hexstr (+fx i 2)))
			   (n (+fx (*fx c1 16) c2)))
		       (string-set! hexstr j (integer->char n))
		       (loop (+fx i 3) (+fx j 1))))
		   ((char=? c #\+)
		    (string-set! hexstr j #\space)
		    (loop (+fx i 1) (+fx j 1)))
		   (else
		    (string-set! hexstr j c)
		    (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    decode ...                                                       */
;*---------------------------------------------------------------------*/
(define (decode str)
   (let ((len (string-length str)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     str)
	    ((char=? (string-ref str i) #\+)
	     (string-set! str i #\space)
	     (loop (+fx i 1)))
	    (else
	     (loop (+fx i 1)))))))
	     
;*---------------------------------------------------------------------*/
;*    cgi-args->list ...                                               */
;*---------------------------------------------------------------------*/
(define (cgi-args->list query)
   (let ((gram (regular-grammar (fields-list field-name)
		  ((when (not (rgc-context? 'val))
		      (: (* (or (out "=%&") (: "%" xdigit xdigit))) "="))
		   (set! field-name
		      (unhex+! (the-substring 0 (-fx (the-length) 1))))
		   (rgc-context 'val)
		   (ignore))
		  ((when (rgc-context? 'val)
		      (+ (or (out "&%") (: #\% xdigit xdigit))))
		   (set! fields-list
		      (cons (cons field-name (unhex+! (the-string)))
			 fields-list))
		   (rgc-context)
		   (ignore))
		  (#\&
		   (ignore))
		  (else (reverse! fields-list)))))
      (let ((p (open-input-string query)))
	 (let ((res (read/rp gram p '() "")))
	    (close-input-port p)
	    res))))

;*---------------------------------------------------------------------*/
;*    cgi-fetch-arg ...                                                */
;*---------------------------------------------------------------------*/
(define (cgi-fetch-arg arg query)
   (let* ((fields-list '())
	  (field-name "")
	  (field-value "")
	  (gram (regular-grammar ()
		   ((when (not (rgc-context? 'val))
		       (+ (or (: (? #a013) #\newline) #\&)))
		    (ignore))
		   ((when (not (rgc-context? 'val))
		       (: (* (out "=%&")) "="))
		    (set! field-name
			  (string-append
			   field-name
			   (decode (the-substring 0 (-fx (the-length) 1)))))
		    (rgc-context 'val)
		    (ignore))
		   ((when (not (rgc-context? 'val))
		       (: (* (out "=%&")) "%" xdigit xdigit))
		    (set! field-name
			  (string-append
			   field-name
			   (decode (the-substring 0 (-fx (the-length) 3)))
			   (unhex (the-substring (-fx (the-length) 2)
						 (the-length)))))
		    (ignore))
		   ((when (rgc-context? 'val)
		       (+ (or (: (? #a013) #\newline) #\&)))
		    (if (string=? field-name arg)
			field-value
			(begin
			   (set! field-name "")
			   (set! field-value "")
			   (rgc-context)
			   (ignore))))
		   ((when (rgc-context? 'val)
		       (: (* (out "&%+")) #\% xdigit xdigit))
		    (set! field-value
			  (string-append
			   field-value
			   (the-substring 0 (-fx (the-length) 3))
			   (unhex (the-substring (-fx (the-length) 2)
						 (the-length)))))
		    (ignore))
		   ((when (rgc-context? 'val)
		       (: (* (out "&%+")) "+"))
		    (set! field-value (string-append
				       field-value
				       (the-substring 0 (-fx (the-length) 1))
				       " "))
		    (ignore))
		   ((when (rgc-context? 'val)
		       (* (out "&%+")))
		    (if (string=? field-name arg)
			(string-append field-value (the-string))
			(begin
			   (set! field-name "")
			   (set! field-value "")
			   (rgc-context)
			   (ignore))))
		   (else #f))))
      (let ((p (open-input-string query)))
	 (let ((res (read/rp gram p)))
	    (close-input-port p)
	    res))))

;*---------------------------------------------------------------------*/
;*    fill-line! ...                                                   */
;*---------------------------------------------------------------------*/
(define (fill-line! buffer port)
   (let ((len (string-length buffer)))
      (let loop ((i 0))
	 (if (>=fx i (-fx len 2))
	     (values i #f)
	     (let ((c (read-char port)))
		(string-set! buffer i c)
		(if (char=? c #\Return)
		    (let ((c2 (read-char port)))
		       (string-set! buffer (+fx i 1) c2)
		       (if (char=? c2 #\Newline)
			   (values i #t)
			   (loop (+fx i 2))))
		    (loop (+fx i 1))))))))
   
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
;*    cgi-parse-boundary ...                                           */
;*---------------------------------------------------------------------*/
(define (cgi-parse-boundary buffer port boundary)
   (multiple-value-bind (len crlf)
      (fill-line! buffer port)
      ;; according to RFC 2046, there may be additional characters
      ;; on line after the boundary
      (if (is-boundary? buffer boundary)
	  (begin
	     (unless crlf (flush-line port))
	     (last-boundary? buffer boundary))
	  (raise (instantiate::&io-parse-error
		    (proc "cgi-multipart->list")
		    (msg "Illegal boundary")
		    (obj (format "\n wanted:--~a\n  found:~a" boundary
			    (substring buffer 0 len))))))))

;*---------------------------------------------------------------------*/
;*    cgi-parse-content-disposition ...                                */
;*---------------------------------------------------------------------*/
(define (cgi-parse-content-disposition port)
   (let* ((str "Content-Disposition: form-data; name=")
	  (len (string-length str)))
      (let ((buf (read-chars len port)))
	 (if (string-ci=? str buf)
	     (let ((s (read port)))
		(if (string? s)
		    (let ((rest (flush-line port))
			  (pref "; filename=\""))
		       (if (substring-at? rest pref  0)
			   (let ((fname (substring
					   rest
					   (string-length pref)
					   (-fx (string-length rest) 1))))
			      (values s fname))
			   (values s #f)))
		    (raise (instantiate::&io-parse-error
			      (proc "cgi-multipart->list")
			      (msg "Illegal name")
			      (obj s)))))
	     (raise (instantiate::&io-parse-error
		       (proc "cgi-multipart->list")
		       (msg "Illegal Content-Disposition: ")
		       (obj buf)))))))

;*---------------------------------------------------------------------*/
;*    cgi-parse-header ...                                             */
;*---------------------------------------------------------------------*/
(define (cgi-parse-header port)
   (define value-grammar
      (regular-grammar ()
	 ((+ (in " \t"))
	  (ignore))
	 ((: (out " \t\r\n") (* (out "\r\n")) "\r\n")
	  (the-substring 0 (-fx (the-length) 2)))
	 ((: (out " \t\r\n") (* (out "\r\n")) "\n")
	  (the-substring 0 (-fx (the-length) 1)))
	 ((: (? #\Return) #\Newline)
	  "")
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 '()
		 c)))))
   (define blank-grammar
      (regular-grammar ()
	 ((+ (in " \t")) (ignore))))
   (define header-grammar
      (regular-grammar (header)
	 ((: (+ (or (out " :\r\n\t") (: #\space (out #\:)))) #\:)
	  (let ((k (the-downcase-keyword)))
	     (let ((v (read/rp value-grammar (the-port))))
		(set! header (cons (cons k v) header))
		(ignore))))
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  header)
	 (else
	  ;; accumultate all the characters to EOL for a better error message
	  (let ((c (the-failure)))
	     (let ((s (if (char? c)
			  (string-for-read
			     (string-append "<" (string c) ">"
				(read-line (the-port))))
			  c)))
		(raise (instantiate::&io-parse-error
			  (proc "cgi-multipart->list")
			  (msg "Illegal characters")
			  (obj s))))))))
   (read/rp header-grammar port '()))

;*---------------------------------------------------------------------*/
;*    cgi-read-file ...                                                */
;*---------------------------------------------------------------------*/
(define (cgi-read-file name header buffer port file tmp boundary)
   (let* ((path (make-file-name tmp file))
	  (dir (dirname (file-name-canonicalize path))))
      (when (substring-at? dir tmp 0) (make-directory dir))
      (let ((op (open-output-file path)))
	 (if (not (output-port? op))
	     (raise (instantiate::&io-file-not-found-error
		       (proc "cgi-multipart->list")
		       (msg "Can't open file for output")
		       (obj path)))
	     (unwind-protect
		(let loop ((ocrlf #f))
		   (multiple-value-bind (len crlf)
		      (fill-line! buffer port)
		      (if (is-boundary? buffer boundary)
			  (begin
			     (unless crlf (flush-line port))
			     (values (last-boundary? buffer boundary)
				(list name
				   :file path
				   :header header)))
			  (begin
			     (when ocrlf (display "\r\n" op))
			     (display-substring buffer 0 len op)
			     (loop crlf)))))
		(close-output-port op))))))

;*---------------------------------------------------------------------*/
;*    cgi-read-data ...                                                */
;*---------------------------------------------------------------------*/
(define (cgi-read-data name header buffer port boundary)
   (let loop ((lines '())
	      (first #t))
      (multiple-value-bind (len crlf)
	 (fill-line! buffer port)
	 (if (is-boundary? buffer boundary)
	     (begin
		(unless crlf (flush-line port))
		(tprint "cgi-read-data len="
		   (apply + (map string-length lines)))
		(values (last-boundary? buffer boundary)
		   (list name
		      :data (apply string-append (reverse! lines))
		      :header header)))
	     (if (or first (not crlf))
		 (loop (cons (substring buffer 0 len) lines) #f)
		 (loop (cons* (substring buffer 0 len) "\r\n" lines) #f))))))

;*---------------------------------------------------------------------*/
;*    cgi-parse-entry ...                                              */
;*---------------------------------------------------------------------*/
(define (cgi-parse-entry buffer port tmp boundary)
   (multiple-value-bind (name file)
      (cgi-parse-content-disposition port)
      (let ((header (cgi-parse-header port)))
	 (if (string? file)
	     (cgi-read-file name header buffer port file tmp boundary)
	     (cgi-read-data name header buffer port boundary)))))

;*---------------------------------------------------------------------*/
;*    cgi-multipart->list ...                                          */
;*---------------------------------------------------------------------*/
(define (cgi-multipart->list tmp port content-length boundary)
   (let ((buffer (make-string (+fx (string-length boundary) 256))))
      (if (cgi-parse-boundary buffer port boundary)
	  '()
	  (let loop ((res '()))
	     (multiple-value-bind (last entry)
		(cgi-parse-entry buffer port tmp boundary)
		(if last
		    (reverse! (cons entry res))
		    (loop (cons entry res))))))))

;*---------------------------------------------------------------------*/
;*    cgi-post-arg-field ...                                           */
;*---------------------------------------------------------------------*/
(define (cgi-post-arg-field key field)
   (let ((l (memq key field)))
      (and (pair? l) (pair? (cdr l)) (cadr l))))
