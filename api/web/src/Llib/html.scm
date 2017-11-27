;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/html.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 17 08:16:28 2005                          */
;*    Last change :  Mon Nov 27 08:42:47 2017 (serrano)                */
;*    Copyright   :  2005-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    HTML helpers                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_html
   
   (import __web_xml)
   
   (export (html-parse::pair-nil ::input-port
				 #!key
				 (content-length 0)
				 (procedure list)
				 (encoding 'UTF-8)
				 (eoi #f))
	   (html-string-decode::bstring ::bstring)
	   (html-string-encode::bstring ::bstring)
	   
	   (unhtml-port ::input-port ::output-port #!optional table)
	   (unhtml::bstring ::bstring #!optional table)))

;*---------------------------------------------------------------------*/
;*    *html-special-elements* ...                                      */
;*---------------------------------------------------------------------*/
(define *html-special-elements*
   '((meta)
     (link)
     (br) (hr) (img) (input)
     (p . (a abbr acronym address big button caption del em
	     i img kbd label legend 
	     q s samp small span strike strong sub sup u var))
     (colgroup . (col))
     (script . ,html-parse-script)))

;*---------------------------------------------------------------------*/
;*    html-parse ...                                                   */
;*---------------------------------------------------------------------*/
(define (html-parse port
		    #!key
		    (content-length 0)
		    (procedure list)
		    (encoding 'UTF-8)
		    (eoi #f))
   (xml-parse port
	      :specials *html-special-elements*
	      :strict #f
	      :eoi eoi
	      :content-length content-length
	      :procedure procedure
	      :encoding encoding))

;*---------------------------------------------------------------------*/
;*    html-parse-script ...                                            */
;*---------------------------------------------------------------------*/
(define (html-parse-script iport)
   (let* ((sp (input-port-position iport))
	  (g (regular-grammar ()
		((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
		 (let ((s (the-substring 1 (-fx (the-length) 1))))
		    (cons (string-append "\"" (string-for-read s) "\"")
			  (ignore))))
		((: "\'" (* (or (out #a000 #\\ #\') (: #\\ all))) "\'")
		 (let ((s (the-substring 1 (-fx (the-length) 1))))
		    (cons (string-append "\'" (string-for-read s) "\'")
			  (ignore))))
		((: "//" (* all))
		 (ignore))
		((: "/*" (* (or (out #\*) (: #\* (out "/")))) "*/")
		 (ignore))
		((+ (out "<"))
		 (let ((s (the-string)))
		    (cons s (ignore))))
		(#\<
		 (let ((s (the-string)))
		    (cons s (ignore))))
		((uncase "</script>")
		 '())  
		(else
		 (let ((char (the-failure)))
		    (raise
		     (instantiate::&io-parse-error
			(proc 'xml-parse)
			(msg (if (eof-object? char)
				 "Premature end of file"
				 "Unclosed list"))
			(obj (if (eof-object? char)
				 char
				 (string-append "{" (string char) "}")))
			(fname (input-port-name iport))
			(location (input-port-position iport)))))))))
      (let ((exp (read/rp g iport)))
	 (cond
	    ((null? exp)
	     '())
	    ((null? (cdr exp))
	     exp)
	    (else
	     (list (apply string-append exp)))))))

;*---------------------------------------------------------------------*/
;*    html-string-decode ...                                           */
;*---------------------------------------------------------------------*/
(define (html-string-decode str)
   (xml-string-decode str))

;*---------------------------------------------------------------------*/
;*    html-string-encode ...                                           */
;*---------------------------------------------------------------------*/
(define (html-string-encode str)
   (define (count str ol)
      (let loop ((i 0)
		 (n 0))
	 (if (=fx i ol)
	     n
	     (let ((c (string-ref str i)))
		(case c
		   ((#\")
		    (loop (+fx i 1) (+fx n 6)))
		   ((#\&  #\' #\;)
		    (loop (+fx i 1) (+fx n 5)))
		   ((#\< #\>)
		    (loop (+fx i 1) (+fx n 4)))
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
		       (case c
			  ((#\<)
			   (blit-string! "&lt;" 0 res j 4)
			   (loop (+fx i 1) (+fx j 4)))
			  ((#\>)
			   (blit-string! "&gt;" 0 res j 4)
			   (loop (+fx i 1) (+fx j 4)))
			  ((#\&)
			   (blit-string! "&amp;" 0 res j 5)
			   (loop (+fx i 1) (+fx j 5)))
			  ((#\')
			   (blit-string! "&#39;" 0 res j 5)
			   (loop (+fx i 1) (+fx j 5)))
			  ((#\;)
			   (blit-string! "&#59;" 0 res j 5)
			   (loop (+fx i 1) (+fx j 5)))
			  ((#\")
			   (blit-string! "&quot;" 0 res j 6)
			   (loop (+fx i 1) (+fx j 6)))
			  (else
			   (string-set! res j c)
			   (loop (+fx i 1) (+fx j 1))))))))))
   (let ((ol (string-length str)))
      (encode str ol (count str ol))))

;*---------------------------------------------------------------------*/
;*    unhtml-table ...                                                 */
;*---------------------------------------------------------------------*/
(define unhtml-table #f)

;*---------------------------------------------------------------------*/
;*    make-unhtml-table ...                                            */
;*---------------------------------------------------------------------*/
(define (make-unhtml-table)
   (let ((t (make-hashtable 64)))
      (for-each (lambda (e) (hashtable-put! t (car e) (cdr e)))
		'(("&lt;" . "<")
		  ("&gt;" . ">")
		  ("&amp;" . "&")
		  ("&quot;" . "\"")
		  ("&apos;" . "'")
		  ("&nbsp;" . " ")))
      t))

;*---------------------------------------------------------------------*/
;*    unhtml-default-table ...                                         */
;*---------------------------------------------------------------------*/
(define (unhtml-default-table)
   (unless (hashtable? unhtml-table) (set! unhtml-table (make-unhtml-table)))
   unhtml-table)

;*---------------------------------------------------------------------*/
;*    *unhtml-comment-grammar* ...                                     */
;*---------------------------------------------------------------------*/
(define *unhtml-comment-grammar*
   (regular-grammar ()
      ((+ (out #\-))
       (ignore))
      ((: #\- (+ (out #\-)))
       (ignore))
      ((: #\- #\- (+ (out #\>)) (* #\Newline))
       (ignore))
      (else
       'done)))

;*---------------------------------------------------------------------*/
;*    *unhtml-grammar* ...                                             */
;*---------------------------------------------------------------------*/
(define *unhtml-grammar*
   (regular-grammar (out table)
      ((+ (out "<>&\n%"))
       (display-string (the-string) out)
       (ignore))
      ((: (or (: "<!" (out #\-) (* (out #\>)) ">")
	      (: "<!-" (out #\-) (* (out #\>)) ">"))
	  (? #\Newline))
       (ignore))
      ((: (: "<" (out #\b #\! #\>) (* (out #\>)) ">") (? #\Newline))
       (ignore))
      ((: (: "<b" (* (out #\>)) ">") (? #\Newline))
       (let ((s (the-string)))
	  (when (or (string-ci=? s "<br>") (substring-ci-at? s "<br " 0))
	     (newline out))
	  (ignore)))
      ("<!--"
       (read/rp *unhtml-comment-grammar* (the-port))
       (ignore))
      ((: #\& (+ alpha) #\;)
       (let* ((s (the-string))
	      (e (hashtable-get table s)))
	  (if e
	      (display-string e out)
	      (display-string s out)))
       (ignore))
      ((: #\% (= 2 xdigit))
       (let ((i (string->integer (the-substring 1 (the-length)))))
	  (write-char (integer->char i) out)
	  (ignore)))
      ((+ (in "\n%"))
       (display-string (the-string) out)
       (ignore))
      ((+ #\&)
       (display-string (the-string) out)
       (ignore))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      'done
	      (ignore))))))

;*---------------------------------------------------------------------*/
;*    unhtml-port ...                                                  */
;*---------------------------------------------------------------------*/
(define (unhtml-port in out #!optional table)
   (read/rp *unhtml-grammar* in out (or table (unhtml-default-table))))

;*---------------------------------------------------------------------*/
;*    unhtml ...                                                       */
;*---------------------------------------------------------------------*/
(define (unhtml str #!optional table)
   (let ((in (open-input-string str))
	 (out (open-output-string)))
      (unhtml-port in out table)
      (close-input-port in)
      (close-output-port out)))

