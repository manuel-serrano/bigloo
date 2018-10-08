;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/cgi.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 16 11:17:40 2003                          */
;*    Last change :  Fri Nov 27 08:15:54 2015 (serrano)                */
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
	   (cgi-multipart->list ::obj ::input-port ::elong ::bstring)
	   (cgi-post-arg-field ::obj ::pair-nil)))

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
;*    cgi-args->list ...                                               */
;*---------------------------------------------------------------------*/
(define (cgi-args->list query)
   (let ((gram (regular-grammar (fields-list field-name)
		  ((when (not (rgc-context? 'val))
		      (: (* (or (out "=%&") (: "%" xdigit xdigit))) "="))
		   (set! field-name (unhex+! (the-substring 0 (-fx (the-length) 1))))
		   (rgc-context 'val)
		   (ignore))
		  ((when (rgc-context? 'val)
		      (+ (or (out "&%") (: #\% xdigit xdigit))))
		   (set! fields-list (cons (cons field-name (unhex+! (the-string))) fields-list))
		   (rgc-context)
		   (ignore))
		  (#\&
		   (when (rgc-context? 'val)
                     (set! fields-list (cons (cons field-name "") fields-list))
                     (rgc-context))
		   (ignore))
		  (else
		    (when (rgc-context? 'val)
                      (set! fields-list (cons (cons field-name "") fields-list)))
                    (reverse! fields-list)))))
      (let ((p (open-input-string query)))
	 (let ((res (read/rp gram p '() "")))
	    (close-input-port p)
	    res))))

;*---------------------------------------------------------------------*/
;*    cgi-fetch-arg ...                                                */
;*---------------------------------------------------------------------*/
(define (cgi-fetch-arg arg query)
   (let* ((args (cgi-args->list query))
	  (c (assoc arg args)))
      (when (pair? c) (cdr c))))
   
;*---------------------------------------------------------------------*/
;*    fill-line! ...                                                   */
;*---------------------------------------------------------------------*/
(define (fill-line! buffer port)
   (let ((len (string-length buffer)))
      (let loop ((i 0))
	 (if (>=fx i (-fx len 2))
	     (values i #f)
	     (let ((c (read-char port)))
		(string-set! buffer i c)p
		(if (char=? c #\Return)
		    (let ((c2 (read-char port)))
		       (string-set! buffer (+fx i 1) c2)
		       (if (char=? c2 #\Newline)
			   (values i #t)
			   (loop (+fx i 2))))
		    (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    read-name ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-name port)
   (let ((g (regular-grammar ()
	       ((: #\" (* (or (out #\" #\\) (: #\\ #\"))) #\")
		(the-substring 1 -1)))))
      (read/rp g port)))

;*---------------------------------------------------------------------*/
;*    cgi-content-disposition ...                                      */
;*---------------------------------------------------------------------*/
(define cgi-content-disposition
   "Content-Disposition: form-data; name=")
;*---------------------------------------------------------------------*/
;*    cgi-parse-content-disposition ...                                */
;*---------------------------------------------------------------------*/
(define (cgi-parse-content-disposition port)
   (let* ((str cgi-content-disposition)
	  (len (string-length str)))
      (let ((buf (read-chars len port)))
	 (if (string-ci=? str buf)
	     (let ((s (read-name port)))
		(if (string? s)
		    (let ((rest (read-line port))
			  (pref "; filename=\""))
		       (if (substring-at? rest pref 0)
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
		(set! header (cons* k v header))
		(ignore))))
	 ((: (* (in #\space #\tab)) (? #\Return) #\Newline)
	  header)
	 (else
	  ;; accumulate all the characters to EOL for a better error message
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
;*    tmp-file-mutex ...                                               */
;*---------------------------------------------------------------------*/
(define tmp-file-mutex #f)

;*---------------------------------------------------------------------*/
;*    make-tmp-file ...                                                */
;*---------------------------------------------------------------------*/
(define (make-tmp-file dir file)
   (unless tmp-file-mutex
      (set! tmp-file-mutex (make-mutex)))
   (let ((path (make-file-name dir file)))
      (synchronize tmp-file-mutex
	 (if (file-exists? path)
	     (let ((suffix (suffix file))
		   (pref (prefix path)))
		(let loop ((cnt 0))
		   (let ((path (string-append
				  pref "(" (fixnum->string cnt) ")." suffix)))
		      (if (file-exists? path)
			  (loop (+fx cnt 1))
			  path))))
	     path))))

;*---------------------------------------------------------------------*/
;*    cgi-read-file ...                                                */
;*---------------------------------------------------------------------*/
(define (cgi-read-file name header port file tmp boundary)
   (let* ((path (make-tmp-file tmp file))
	  (dir (dirname (file-name-canonicalize path))))
      (when (substring-at? dir tmp 0) (make-directory dir))
      (let ((op (open-output-file path)))
	 (if (not (output-port? op))
	     (raise (instantiate::&io-file-not-found-error
		       (proc "cgi-multipart->list")
		       (msg "Can't open file for output")
		       (obj path)))
	     (unwind-protect
		(multiple-value-bind (last data)
		   (read/rp cgi-multipart-data-grammar port boundary '())
		   (display data op)
		   (values last (list name :file path :header header)))
		(close-output-port op))))))

;*---------------------------------------------------------------------*/
;*    cgi-multipart-data-grammar ...                                   */
;*---------------------------------------------------------------------*/
(define cgi-multipart-data-grammar
   (regular-grammar (boundary lines)
      ((+ (out "\r\n"))
       (set! lines (cons (the-string) lines))
       (ignore))
      ((or (: (? #\Return) "\n") #\Return)
       (set! lines (cons (the-string) lines))
       (ignore))
      ((: (? #\Return) "\n--")
       ;; a beginning of boundary
       (let* ((str (read-chars (string-length boundary) (the-port)))
	      (c (read-char (the-port))))
	  (cond
	     ((and (char=? c #\Return) (string=? str boundary))
	      (let ((c2 (read-char (the-port))))
		 (if (or (char=? c2 #\Newline) (eof-object? c2))
		     (values #f (apply string-append (reverse! lines)))
		     (begin
			(set! lines
			   (cons* (string c c2) str (the-string) lines))
			(ignore)))))
	     ((and (char=? c #\-) (string=? str boundary))
	      (let* ((c2 (read-char (the-port)))
		     (c3 (read-char (the-port)))
		     (c4 (read-char (the-port))))
		 (if (and (char=? c2 #\-) (char=? c3 #\Return)
			  (or (char=? c4 #\Newline) (eof-object? c4)))
		     ;; last boundary
		     (values #t (apply string-append (reverse! lines)))
		     ;; a regular line
		     (begin
			(set! lines
			   (cons* (string c c2 c3 c4) str (the-string) lines))
			(ignore)))))
	     (else
	      (set! lines (cons (the-string) lines))
	      (ignore)))))
      (else
       (let* ((c (the-failure))
	      (s (if (char? c)
		     (string-for-read
			(string-append "<" (string c) ">"
			   (read-line (the-port))))
		     c)))
	  (raise
	     (instantiate::&io-parse-error
		(proc "cgi-multipart->list")
		(msg "Illegal data character")
		(obj s)))))))

;*---------------------------------------------------------------------*/
;*    cgi-multipart-boundary-grammar ...                               */
;*---------------------------------------------------------------------*/
(define cgi-multipart-boundary-grammar
   (regular-grammar (boundary)
      ((: "--" (* #\-) (+ (out "-\n\r")) #\Newline)
       (if (string=? (the-substring 2 -1) boundary)
	   'start
	   (raise
	      (instantiate::&io-parse-error
		 (proc "cgi-multipart->list")
		 (msg "Illegal start boundary character")
		 (obj (the-string))))))
      ((: "--" (* #\-) (+ (out "\n\r-")) #\Return #\Newline)
       (if (string=? (the-substring 2 -2) boundary)
	   'start
	   (raise
	      (instantiate::&io-parse-error
		 (proc "cgi-multipart->list")
		 (msg "Illegal start boundary character")
		 (obj (the-string))))))
      ((: "--" (* #\-) (+ (out "\r\n-")) "--" #\Newline)
       (if (string=? (the-substring 2 -3) boundary)
	   'end
	   (raise
	      (instantiate::&io-parse-error
		 (proc "cgi-multipart->list")
		 (msg "Illegal end boundary character")
		 (obj (the-string))))))
      ((: "--" (* #\-) (+ (out "\n-")) "--" #\return #\Newline)
       (if (string=? (the-substring 2 -4) boundary)
	   'end
	   (raise
	      (instantiate::&io-parse-error
		 (proc "cgi-multipart->list")
		 (msg "Illegal end boundary character")
		 (obj (the-string))))))
      (else
       (or (eof-object? (the-failure))
	   (let* ((c (the-failure))
		  (s (if (char? c)
			 (string-for-read
			    (string-append "<" (string c) ">"
			       (read-line (the-port))))
			 c)))
	      (raise
		 (instantiate::&io-parse-error
		    (proc "cgi-multipart->list")
		    (msg "Illegal boundary character")
		    (obj s))))))))
      
;*---------------------------------------------------------------------*/
;*    cgi-read-data ...                                                */
;*---------------------------------------------------------------------*/
(define (cgi-read-data name header port boundary)
   (multiple-value-bind (last data)
      (read/rp cgi-multipart-data-grammar port boundary '())
      (values last (list name :data data :header header))))

;*---------------------------------------------------------------------*/
;*    cgi-parse-entry ...                                              */
;*---------------------------------------------------------------------*/
(define (cgi-parse-entry port tmp boundary)
   (multiple-value-bind (name file)
      (cgi-parse-content-disposition port)
      (let ((header (cgi-parse-header port)))
	 (if (string? file)
	     (let ((dir (if (string? tmp) tmp (tmp))))
		(cgi-read-file name header port file dir boundary))
	     (cgi-read-data name header port boundary)))))

;*---------------------------------------------------------------------*/
;*    cgi-multipart->list ...                                          */
;*---------------------------------------------------------------------*/
(define (cgi-multipart->list tmp port content-length boundary)
   ;; check for an empty payload
   (if (or (not (number? content-length))
	   (< content-length 0)
	   (> content-length
	      (+ (* 2 (string-length boundary))
		 2
		 (string-length cgi-content-disposition))))
       (if (read/rp cgi-multipart-boundary-grammar port boundary)
	   (let loop ((res '()))
	      (multiple-value-bind (last entry)
		 (cgi-parse-entry port tmp boundary)
		 (if last
		     (reverse! (cons entry res))
		     (loop (cons entry res)))))
	   '())
       (raise
	  (instantiate::&io-parse-error
	     (proc "cgi-multipart->list")
	     (msg (format "empty body [~a]" boundary))
	     (obj port)))))

;*---------------------------------------------------------------------*/
;*    cgi-post-arg-field ...                                           */
;*---------------------------------------------------------------------*/
(define (cgi-post-arg-field key field)
   (let ((l (memq key field)))
      (and (pair? l) (pair? (cdr l)) (cadr l))))
