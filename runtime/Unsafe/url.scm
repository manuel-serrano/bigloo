;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/url.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May 28 13:32:00 2005                          */
;*    Last change :  Thu Dec 14 16:09:24 2017 (serrano)                */
;*    Copyright   :  2005-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    URL parsing                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __url

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
	   __error
	   __evenv
	   __os
	   __param)

   (import __rgc)

   (export (url-parse ::obj)
	   (url-sans-protocol-parse ::obj ::bstring)
	   (http-url-parse ::obj)
	   (url?::bool ::bstring)
	   (url-path-encode::bstring ::bstring)
	   (uri-encode::bstring ::bstring)
	   (uri-encode-component::bstring ::bstring)
	   (url-encode::bstring ::bstring)
	   (url-decode::bstring ::bstring)
	   (url-decode!::bstring ::bstring)
	   (uri-decode::bstring ::bstring)
	   (uri-decode::bstring ::bstring)
	   (uri-decode-component::bstring ::bstring)
	   (uri-decode-component!::bstring ::bstring)
	   (www-form-urlencode::bstring ::pair-nil)
	   (x-www-form-urlencode::bstring ::pair-nil)
	   (www-form-urldecode::pair-nil ::bstring)))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error port msg obj)
   (let ((line (read-line port)))
      (raise (instantiate::&io-parse-error
		(obj (if (string? line)
			 (format "{~a}~a" obj line)
			 obj))
		(proc 'url-parse)
		(msg msg)))))

;*---------------------------------------------------------------------*/
;*    uri-grammar ...                                                  */
;*---------------------------------------------------------------------*/
(define uri-grammar
   (regular-grammar ((CRLF "\r\n"))
      ("*"
       (values "*" #f #f #f #f))
      ((: "/" (* (out " \r\n")))
       (values "file" #f #f #f (the-string)))
      ((: (out #\/) (* (out #\:)) "://")
       (read/rp absolute-uri-grammar (the-port) (the-substring 0 -3) #f))
      (else
       (rgc-buffer-unget-char (the-port) (the-byte))
       (values "*" #f #f #f (read-line (the-port))))))

;*---------------------------------------------------------------------*/
;*    abspath-grammar ...                                              */
;*---------------------------------------------------------------------*/
(define abspath-grammar
   (regular-grammar ()
      ((: (in "?/") (* (out "\r\n")))
       (the-string))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      "/"
	      (parse-error (the-port) "Illegal character" (the-failure)))))))

;*---------------------------------------------------------------------*/
;*    absolute-uri-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define absolute-uri-grammar
   (regular-grammar ((CRLF "\r\n")
		     (unreserved (or alpha digit #\| #\- #\. #\_ #\~ #\/))
		     (pct-encoded (: #\% xdigit xdigit))
		     (sub-delims (in "!$&'()*+,;= "))
		     protocol
		     userinfo)
      ((: (* (or "<> " unreserved pct-encoded sub-delims #\: #\@ )) #\@)
       (set! userinfo (the-substring 0 -1))
       (ignore))
      ((: (+ (out "@:/")) ":")
       (let* ((host (the-substring 0 (-fx (the-length) 1)))
	      (port (read/rp http-port-grammar (the-port)))
	      (abspath (read/rp abspath-grammar (the-port))))
	  (values protocol userinfo host port abspath)))
      ((: (+ (out "@:/")))
       (let* ((host (the-substring 0 (the-length)))
	      (port (if (string=? protocol "https") 443 80))
	      (abspath (read/rp abspath-grammar (the-port))))
	  (values protocol userinfo host port abspath)))
      ((: "/" (* (out ":\r\n")))
       (values protocol #f #f #f (the-string)))
      (CRLF
       #f)
      (else
       (parse-error (the-port) "Illegal character" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    http-uri-grammar ...                                             */
;*---------------------------------------------------------------------*/
(define http-uri-grammar
   (regular-grammar ()
      ("*"
       (values "*" #f #f #f "*"))
      ((: "/" (* (out " \r\n")))
       (values "*" #f #f #f (the-string)))
      ((: (out #\/) (* (out #\:)) "://")
       (read/rp absolute-http-uri-grammar (the-port) (the-substring 0 -3) #f))
      (else
       (rgc-buffer-unget-char (the-port) (the-byte))
       (read/rp absolute-http-uri-grammar (the-port) "http" #f))))
      
;*---------------------------------------------------------------------*/
;*    absolute-http-uri-grammar ...                                    */
;*---------------------------------------------------------------------*/
(define absolute-http-uri-grammar
   (regular-grammar ((CRLF "\r\n")
		     (unreserved (or alpha digit #\| #\- #\. #\_ #\~))
		     (pct-encoded (: #\% xdigit xdigit))
		     (sub-delims (in "!$&'()*+,;="))
		     protocol
		     userinfo)
      ((: (* (or unreserved pct-encoded sub-delims #\:)) #\@)
       (set! userinfo (the-substring 0 -1))
       (ignore))
      ((: (+ (out "@:/")) ":")
       (let ((host (the-substring 0 (-fx (the-length) 1))))
	  (let* ((port (read/rp http-port-grammar (the-port)))
		 (abspath (read/rp http-abspath-grammar (the-port))))
	     (values protocol userinfo host port abspath))))
      ((: (+ (out "@:/")))
       (let* ((host (the-substring 0 (the-length)))
	      (port (if (string=? protocol "https") 443 80))
	      (abspath (read/rp http-abspath-grammar (the-port))))
	  (values protocol userinfo host port abspath)))
      ((: "/" (* (out ": \r\n")))
       (values protocol #f #f #f (the-string)))
      (CRLF
       (values protocol #f #f #f (the-string)))
      (else
       (parse-error (the-port) "Illegal character" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    http-abspath-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define http-abspath-grammar
   (regular-grammar ()
      ((: "/" (* (out " \r\n")))
       (the-string))
      (else
       (let ((c (the-failure)))
	  (if (or (eof-object? c) (memq c '(#\space #\tab #\return)))
	      "/"
	      (parse-error (the-port) "Illegal character" (the-failure)))))))
      
;*---------------------------------------------------------------------*/
;*    http-port-grammar ...                                            */
;*---------------------------------------------------------------------*/
(define http-port-grammar
   (regular-grammar ()
      ((+ digit)
       (the-fixnum))
      (else
       (parse-error (the-port) "Illegal character" (the-failure)))))
      
;*---------------------------------------------------------------------*/
;*    url-parse ...                                                    */
;*---------------------------------------------------------------------*/
(define (url-parse url)
   (cond
      ((input-port? url)
       (read/rp uri-grammar url))
      ((string? url)
       (let ((p (open-input-string url)))
	  (unwind-protect
	     (read/rp uri-grammar p)
	     (close-input-port p))))
      (else
       (bigloo-type-error "url-parse" "input-port or string" url))))

;*---------------------------------------------------------------------*/
;*    url-sans-protocol-parse ...                                      */
;*---------------------------------------------------------------------*/
(define (url-sans-protocol-parse url protocol)
   (cond
      ((input-port? url)
       (read/rp absolute-uri-grammar url protocol #f))
      ((string? url)
       (let ((p (open-input-string url)))
	  (unwind-protect
	     (read/rp absolute-uri-grammar p protocol #f)
	     (close-input-port p))))
      (else
       (bigloo-type-error "url-sans-protocol-parse" "input-port or string" url))))

;*---------------------------------------------------------------------*/
;*    http-url-parse ...                                               */
;*---------------------------------------------------------------------*/
(define (http-url-parse url)
   (cond
      ((input-port? url)
       (read/rp http-uri-grammar url))
      ((string? url)
       (let ((p (open-input-string url)))
	  (unwind-protect
	     (read/rp http-uri-grammar p)
	     (close-input-port p))))
      (else
       (bigloo-type-error "url-parse" "input-port or string" url))))

;*---------------------------------------------------------------------*/
;*    encode-char ...                                                  */
;*---------------------------------------------------------------------*/
(define (encode-char res j c)
   
   (define (int->char c)
      (cond
	 ((<fx c 10)
	  (integer->char (+fx c (char->integer #\0))))
	 ((<fx c 16)
	  (integer->char (+fx (-fx c 10) (char->integer #\A))))))
   
   (let ((n (char->integer c)))
      (string-set! res j #\%)
      (cond
	 ((<fx n 16)
	  (string-set! res (+fx j 1) #\0)
	  (string-set! res (+fx j 2) (int->char n)))
	 (else
	  (let ((n1 (/fx n 16))
		(n2 (remainderfx n 16)))
	     (string-set! res (+fx j 1) (int->char n1))
	     (string-set! res (+fx j 2) (int->char n2)))))))

;*---------------------------------------------------------------------*/
;*    url-path-encode/set ...                                          */
;*---------------------------------------------------------------------*/
(define (url-path-encode/set str set)
   
   (define (count str ol)
      (let loop ((i 0)
		 (n 0))
	 (if (=fx i ol)
	     n
	     (let ((c (string-ref str i)))
		(cond
		   ((string-index set c)
		    (loop (+fx i 1) (+fx n 3)))
		   ((or (char<? c #a032) (char>=? c #a127))
		    (loop (+fx i 1) (+fx n 3)))
		   (else
		    (loop (+fx i 1) (+fx n 1))))))))
   
   (define (encode str ol nl)
      (if (=fx nl ol)
	  str
	  (let ((res (make-string nl)))
	     (let loop ((i 0)
			(j 0))
		(if (=fx j nl)
		    res
		    (let ((c (string-ref str i)))
		       (cond
			  ((string-index set c)
			   (encode-char res j c)
			   (loop (+fx i 1) (+fx j 3)))
			  ((or (char<? c #a032) (char>=? c #a127))
			   (encode-char res j c)
			   (loop (+fx i 1) (+fx j 3)))
			  (else
			   (string-set! res j c)
			   (loop (+fx i 1) (+fx j 1))))))))))
   
   (let ((ol (string-length str)))
      (encode str ol (count str ol))))

;*---------------------------------------------------------------------*/
;*    url-path-encode ...                                              */
;*---------------------------------------------------------------------*/
(define (url-path-encode str)
   (url-path-encode/set str "# \"'`&=%?:\n^[]\\<>;,{|}()~$!+"))

;*---------------------------------------------------------------------*/
;*    uri-encode ...                                                   */
;*---------------------------------------------------------------------*/
(define (uri-encode str)
   (url-path-encode/set str " \"`%\n^[]\\<>{|}"))

;*---------------------------------------------------------------------*/
;*    uri-encode-component ...                                         */
;*---------------------------------------------------------------------*/
(define (uri-encode-component str)
   (url-path-encode/set str "# \"`+=%?:\n^[]\\<>;/@&$,{|}"))

;*---------------------------------------------------------------------*/
;*    url-encode ...                                                   */
;*---------------------------------------------------------------------*/
(define (url-encode str)
   (multiple-value-bind (scheme uinfo host port abspath)
      (url-parse str)
      (if (string=? scheme "file")
	  str
	  (let ((epath (url-path-encode abspath)))
	     (if uinfo
		 (format "~a://~a@~a:~a~a" scheme uinfo host port epath)
		 (format "~a://~a:~a~a" scheme host port epath))))))

;*---------------------------------------------------------------------*/
;*    url? ...                                                         */
;*    -------------------------------------------------------------    */
;*    Is a string a valid URL?                                         */
;*---------------------------------------------------------------------*/
(define (url? str)
   (let ((len (string-length str)))
      (let loop ((i 0))
	 (if (=fx i len)
	     #t
	     (let ((c (string-ref str i)))
		(cond
		   ((char=? c #\%)
		    (if (and (<=fx i (-fx len 3))
			     (char-hexnumeric? (string-ref str (+fx i 1)))
			     (char-hexnumeric? (string-ref str (+fx i 2))))
			(loop (+fx i 3))
			#f))
		   ((char<=? c #a127)
		    (loop (+fx i 1)))
		   (else
		    (loop (+fx i 1)))))))))

;*---------------------------------------------------------------------*/
;*    char-value ...                                                   */
;*---------------------------------------------------------------------*/
(define (char-value c)
   (cond
      ((char-numeric? c)
       (-fx (char->integer c) (char->integer #\0)))
      ((char<=? c #\F)
       (+fx 10 (-fx (char->integer c) (char->integer #\A))))
      (else
       (+fx 10 (-fx (char->integer c) (char->integer #\a))))))

;*---------------------------------------------------------------------*/
;*    url-string-decode-inner! ...                                     */
;*---------------------------------------------------------------------*/
(define (url-string-decode-inner! str ol nl res set)
   (let ((ol-2 (-fx ol 2)))
      (let loop ((i 0)
		 (j 0))
	 (if (=fx j nl)
	     res
	     (let ((c (string-ref str i)))
		(if (and (char=? c #\%) (<fx i ol-2))
		    (let ((c1 (string-ref str (+fx i 1)))
			  (c2 (string-ref str (+fx i 2))))
		       (if (and (char-hexnumeric? c1) (char-hexnumeric? c2))
			   (let* ((v1 (char-value c1))
				  (v2 (char-value c2))
				  (d (+fx (*fx v1 16) v2)))
			      (if (string-index set (integer->char d))
				  (begin
				     (string-set! res j c)
				     (string-set! res (+fx j 1) c1)
				     (string-set! res (+fx j 2) c2)
				     (loop (+fx i 3) (+fx j 3)))
				  (begin
				     (string-set! res j (integer->char d))
				     (loop (+fx i 3) (+fx j 1)))))
			   (begin
			      (string-set! res j c)
			      (loop (+fx i 1) (+fx j 1)))))
		    (begin
		       (string-set! res j c)
		       (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    url-decode-count ...                                             */
;*---------------------------------------------------------------------*/
(define (url-decode-count str ol set)
   (let loop ((i (-fx ol 3))
	      (c 0))
      (cond
	 ((=fx i -1)
	  c)
	 ((char=? (string-ref str i) #\%)
	  (let ((c1 (string-ref str (+fx i 1)))
		(c2 (string-ref str (+fx i 2))))
	     (if (and (char-hexnumeric? c1)
		      (char-hexnumeric? c2))
		 (let* ((v1 (char-value c1))
			(v2 (char-value c2))
			(d (+fx (*fx v1 16) v2)))
		    (if (string-index set (integer->char d))
			(loop (-fx i 1) c)
			(loop (-fx i 1) (+fx c 1))))
		 (loop (-fx i 1) c))))
	 (else
	  (loop (-fx i 1) c)))))

;*---------------------------------------------------------------------*/
;*    url-decode/set ...                                               */
;*---------------------------------------------------------------------*/
(define (url-decode/set str set)
   (let ((ol (string-length str)))
      (if (>=fx ol 3)
	  (let ((count (url-decode-count str ol set)))
	     (if (=fx count 0)
		 (string-copy str)
		 (let* ((nl (-fx ol (*fx count 2)))
			(res (make-string nl)))
		    (url-string-decode-inner! str ol nl res set))))
	  (string-copy str))))

;*---------------------------------------------------------------------*/
;*    url-decode/set! ...                                              */
;*---------------------------------------------------------------------*/
(define (url-decode/set! str set)
   (let ((ol (string-length str)))
      (if (>=fx ol 3)
	  (let ((count (url-decode-count str ol set)))
	     (if (=fx count 0)
		 str
		 (let* ((nl (-fx ol (*fx count 2)))
			(res (make-string nl)))
		    (url-string-decode-inner! str ol nl res set))))
	  str)))

;*---------------------------------------------------------------------*/
;*    url-decode ...                                                   */
;*---------------------------------------------------------------------*/
(define (url-decode str)
   (url-decode/set str ""))

;*---------------------------------------------------------------------*/
;*    uri-decode ...                                                   */
;*---------------------------------------------------------------------*/
(define (uri-decode str)
   (url-decode/set str "#$&+,/:;=?@"))

;*---------------------------------------------------------------------*/
;*    uri-decode-component ...                                         */
;*---------------------------------------------------------------------*/
(define (uri-decode-component str)
   (url-decode/set str ""))

;*---------------------------------------------------------------------*/
;*    url-decode! ...                                                  */
;*---------------------------------------------------------------------*/
(define (url-decode! str)
   (url-decode/set! str ""))

;*---------------------------------------------------------------------*/
;*    uri-decode! ...                                                  */
;*---------------------------------------------------------------------*/
(define (uri-decode! str)
   (url-decode/set! str "#$&+,/:;=?@"))

;*---------------------------------------------------------------------*/
;*    uri-decode-component! ...                                        */
;*---------------------------------------------------------------------*/
(define (uri-decode-component! str)
   (url-decode/set! str ""))

;*---------------------------------------------------------------------*/
;*    form-urlencode ...                                               */
;*---------------------------------------------------------------------*/
(define (form-urlencode args sep)
   
   (define (count-string str)
      (let ((len (string-length str)))
	 (let loop ((i 0)
		    (n 0))
	    (if (=fx i len)
		n
		(let ((c (string-ref str i)))
		   (case c
		      ((#\; #\& #\= #\# #\" #\' #\+ #\% #\? #\: #\|)
		       (loop (+fx i 1) (+fx n 3)))
		      (else
		       (if (or (char>=? c #a128) (char<? c #a032))
			   (loop (+fx i 1) (+fx n 3))
			   (loop (+fx i 1) (+fx n 1))))))))))
   
   
   (define (encode-string res o str)
      (let ((len (string-length str)))
	 (let loop ((i 0)
		    (j o))
	    (if (=fx i len)
		j
		(let ((c (string-ref str i)))
		   (case c
		      ((#\; #\& #\= #\# #\" #\' #\+ #\% #\? #\: #\|)
		       (encode-char res j c)
		       (loop (+fx i 1) (+fx j 3)))
		      ((#\space)
		       (string-set! res j #\+)
		       (loop (+fx i 1) (+fx j 1)))
		      (else
		       (if (or (char>=? c #a128) (char<? c #a032))
			   (begin
			      (encode-char res j c)
			      (loop (+fx i 1) (+fx j 3)))
			   (begin
			      (string-set! res j c)
			      (loop (+fx i 1) (+fx j 1)))))))))))
   
   (define (count-arg arg)
      (let ((cname (count-string (car arg))))
	 (if (eq? (cadr arg) #unspecified)
	     cname
	     (+fx (+fx cname 1) (count-string (cadr arg))))))
   
   (define (encode-arg string i arg)
      (let ((ni (encode-string string i (car arg))))
	 (if (eq? (cadr arg) #unspecified)
	     ni
	     (begin
		(string-set! string ni #\=)
		(encode-string string (+fx ni 1) (cadr arg))))))
   
   (if (null? args)
       ""
       (let loop ((a args)
		  (len 0))
	  (if (null? (cdr a))
	      (let* ((len (+fx len (count-arg (car a))))
		     (res (make-string len)))
		 (let loop ((a args)
			    (i 0))
		    (if (null? (cdr a))
			(begin
			   (encode-arg res i (car a))
			   res)
			(let ((ni (encode-arg res i (car a))))
			   (string-set! res ni sep)
			   (loop (cdr a) (+fx ni 1))))))
	      (loop (cdr a) (+fx (+fx 1 len) (count-arg (car a))))))))

;*---------------------------------------------------------------------*/
;*    www-form-urlencode ...                                           */
;*---------------------------------------------------------------------*/
(define (www-form-urlencode args)
   (form-urlencode args #\;))

;*---------------------------------------------------------------------*/
;*    x-www-form-urlencode ...                                         */
;*---------------------------------------------------------------------*/
(define (x-www-form-urlencode args)
   (form-urlencode args #\&))

;*---------------------------------------------------------------------*/
;*    char-hexnumeric? ...                                             */
;*---------------------------------------------------------------------*/
(define (char-hexnumeric? c)
   (or (char-numeric? c)
       (and (char>=? c #\A) (char<=? c #\F))
       (and (char>=? c #\a) (char<=? c #\f))))

;*---------------------------------------------------------------------*/
;*    www-form-urldecode ...                                           */
;*---------------------------------------------------------------------*/
(define (www-form-urldecode string)

   (define (string-count str ol)
      (let loop ((i (-fx ol 3))
		 (c 0))
	 (cond
	    ((=fx i -1)
	     c)
	    ((char=? (string-ref str i) #\%)
	     (if (and (char-hexnumeric? (string-ref str (+fx i 1)))
		      (char-hexnumeric? (string-ref str (+fx i 2))))
		 (loop (-fx i 1) (+fx c 1))
		 (loop (-fx i 1) c)))
	    (else
	     (loop (-fx i 1) c)))))

   (define (string-decode-inner! str ol nl res)
      (define (char-value c)
	 (cond
	    ((char-numeric? c)
	     (-fx (char->integer c) (char->integer #\0)))
	    ((char<=? c #\F)
	     (+fx 10 (-fx (char->integer c) (char->integer #\A))))
	    (else
	     (+fx 10 (-fx (char->integer c) (char->integer #\a))))))
      (let ((ol-2 (-fx ol 2)))
	 (let loop ((i 0)
		    (j 0))
	    (if (=fx j nl)
		res
		(let ((c (string-ref str i)))
		   (cond
		      ((and (char=? c #\%) (<fx i ol-2))
		       (let ((c1 (string-ref str (+fx i 1)))
			     (c2 (string-ref str (+fx i 2))))
			  (if (and (char-hexnumeric? c1) (char-hexnumeric? c2))
			      (let* ((v1 (char-value c1))
				     (v2 (char-value c2))
				     (d (integer->char (+fx (*fx v1 16) v2))))
				 (string-set! res j d)
				 (loop (+fx i 3) (+fx j 1)))
			      (begin
				 (string-set! res j c)
				 (loop (+fx i 1) (+fx j 1))))))
		      ((char=? c #\+)
		       (string-set! res j #\space)
		       (loop (+fx i 1) (+fx j 1)))
		      (else
		       (string-set! res j c)
		       (loop (+fx i 1) (+fx j 1)))))))))
   
   (define (string-decode str)
      (let ((ol (string-length str)))
	 (if (>=fx ol 3)
	     (let ((count (string-count str ol)))
		(if (=fx count 0)
		    (string-replace! str #\+ #\space)
		    (let* ((nl (-fx ol (*fx count 2)))
			   (res (make-string nl)))
		       (string-decode-inner! str ol nl res))))
	     (string-copy str))))
	    
   (let ((len (string-length string)))
      (if (=fx len 0)
	  '()
	  (map! (lambda (s)
		   (let ((arg (string-split s "=")))
		      (set-car! arg (string-decode (car arg)))
		      (if (null? (cdr arg))
			  (set-cdr! arg (list #unspecified))
			  (set-car! (cdr arg) (string-decode (cadr arg))))
		      arg))
		(string-split string ";&")))))
