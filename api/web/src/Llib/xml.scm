;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/web/src/Llib/xml.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 11 16:23:53 2005                          */
;*    Last change :  Sun Oct  7 08:46:03 2018 (serrano)                */
;*    Copyright   :  2005-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    XML parsing                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_xml

   (option (set! *dlopen-init-gc* #t))
   
   (export (xml-parse::pair-nil port::input-port
				#!key
				(content-length 0)
				(procedure list)
				(specials '())
				(strict #t)
				(encoding 'UTF-8)
				(eoi #f))
	   (xml-string-decode::bstring ::bstring)
	   (xml-string-decode!::bstring ::bstring)
	   (xml-string-encode::bstring ::bstring)
	   (read-xml #!optional (port::input-port (current-input-port)))
	   (xml-metadata xml-tree::pair-nil)))

;*---------------------------------------------------------------------*/
;*    xml-parse ...                                                    */
;*---------------------------------------------------------------------*/
(define (xml-parse::pair-nil port::input-port
			     #!key
			     (content-length 0)
			     (procedure list)
			     (specials '())
			     (strict #t)
			     (encoding 'UTF-8)
			     (eoi #f))
   (when (elong? content-length)
      (set! content-length (elong->fixnum content-length)))
   (when (and (fixnum? content-length) (>fx content-length 0))
      (input-port-fill-barrier-set! port content-length))
   (when (>fx content-length 0)
      (set! content-length (+fx content-length (input-port-position port))))
   (let loop ((decoder (lambda (x) x)))
      (let ((obj (read/rp xml-grammar port procedure procedure specials strict decoder encoding)))
	 (when (and (fixnum? content-length) (>fx content-length 0))
	    (input-port-fill-barrier-set! port -1))
	 (cond
	    ((eof-object? obj)
	     '())
	    ((or (and (procedure? eoi) (eoi obj))
		 (and (>fx content-length 0)
		      (>=fx (input-port-position port) content-length)))
	     (list obj))
	    ((and (pair? obj) (eq? 'xml-decl (car obj)))
	     (let ((enc (assq 'encoding (cdr obj))))
		(if enc
		    (cons obj (loop (get-decoder (cdr enc) encoding)))
		    (cons obj (loop decoder)))))
	    (else
	     (cons obj (loop decoder)))))))

;*---------------------------------------------------------------------*/
;*    xml-parse-error ...                                              */
;*---------------------------------------------------------------------*/
(define (xml-parse-error msg obj name pos)
   (raise
    (instantiate::&io-parse-error
       (proc 'xml-parse)
       (msg msg)
       (obj obj)
       (fname name)
       (location pos))))

;*---------------------------------------------------------------------*/
;*    error-line ...                                                   */
;*---------------------------------------------------------------------*/
(define (error-line c port)
   (let ((line (read-line port)))
      (string-append "{" (string c) "}" (if (string? line) line ""))))

;*---------------------------------------------------------------------*/
;*    special ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct special tag attributes body owner)

;*---------------------------------------------------------------------*/
;*    collect-up-to ...                                                */
;*---------------------------------------------------------------------*/
(define (collect-up-to ignore tag attributes port make specials strict decoder encoding)
   
   (define (collect ignore tags)
      (let ((name (input-port-name port))
	    (po (input-port-position port)))
	 (let loop ((acc '())
		    (item (ignore)))
	    (cond
	       ((symbol? item)
		(cond
		   ((eq? item tag)
		    (make tag attributes (reverse! acc)))
		   (strict
		    (xml-parse-error "Illegal closing tag"
		       (format "`~a' expected, `~a' provided"
			  tag item)
		       name po))
		   (else
		    (make tag attributes (reverse! acc)))))
	       ((special? item)
		(let ((nitem (make (special-tag item)
				(special-attributes item)
				(special-body item))))
		   (if (memq (special-tag item) tags)
		       (loop acc nitem)
		       (begin
			  (list (make tag attributes (reverse! acc)) nitem)))))
	       ((eof-object? item)
		(if strict
		    (xml-parse-error
		       (format "Premature end of line, expecting tag `~a'"
			  tag)
		       item name po)
		    (make tag attributes (reverse! acc))))
	       (else
		(let ((po (input-port-last-token-position port)))
		   (loop (econs item acc (list 'at name po)) (ignore))))))))

   (let ((spec (assq tag specials)))
      (cond
	 ((not spec)
	  (collect ignore '()))
	 ((null? (cdr spec))
	  (make tag attributes '()))
	 ((procedure? (cdr spec))
	  (make tag attributes ((cdr spec) port)))
	 ((pair? (cdr spec))
	  (let ((ignore (lambda ()
			   (read/rp xml-grammar port
				    (lambda (t a b) (special t a b tag))
				    make
				    specials strict decoder encoding)))) 
	     (collect ignore (cdr spec))))
	 (else
	  (error "xml-parse" "Illegal special handler" spec)))))

;*---------------------------------------------------------------------*/
;*    attribute-value-grammar ...                                      */
;*---------------------------------------------------------------------*/
(define attribute-value-grammar
   (regular-grammar (strict tag)
      ((+ (in " \t\n\r"))
       (ignore))
      ((: #\" (* (or (out #\\ #\") (: #\\ all))) #\")
       (the-substring 1 (-fx (the-length) 1)))
      ((: #\' (* (or (out #\\ #\') (: #\\ all))) #\')
       (the-substring 1 (-fx (the-length) 1)))
      ((: (+ digit) (? (or "%" "px" "cm" "em" "mm" "inch")))
       (if strict
	   (xml-parse-error (format "Illegal `~a' attribute value" tag)
			    (the-string)
			    (input-port-name (the-port))
			    (input-port-position (the-port)))
	   (the-string)))
      ((+ (out " \t\n\r<>(){}[]@!\"'/"))
       (if strict
	   (xml-parse-error (format "Illegal `~a' attribute character" tag)
			    (the-string)
			    (input-port-name (the-port))
			    (input-port-position (the-port)))
	   (the-string)))
      (else
       (let ((c (the-failure)))
	  (if (not (eof-object? c))
	      (if (or strict
		      (not (or (char=? c #\space)
			       (char=? c #\Newline)
			       (char=? c #\>))))
		  (xml-parse-error
		   (format "Illegal `~a' attribute character" tag)
		   (error-line c (the-port))
		   (input-port-name (the-port))
		   (input-port-position (the-port)))
		  " ")
	      (xml-parse-error
	       (format "Premature end of line for tag `~a' attribute" tag)
	       c
	       (input-port-name (the-port))
	       (-fx (input-port-position (the-port)) 1)))))))
      
;*---------------------------------------------------------------------*/
;*    attribute-grammar ...                                            */
;*---------------------------------------------------------------------*/
(define attribute-grammar
   (regular-grammar ((id (: (in ("azAZ") "_") (* (in ("azAZ09") ":_-"))))
		     tag
		     strict
		     decoder)
      ((+ (in " \t\n\r"))
       (ignore))
      ((: id "=")
       (let* ((key (the-substring 0 (-fx (the-length) 1)))
	      (val (read/rp attribute-value-grammar (the-port) strict tag)))
	  (cons (string->symbol (decoder key)) (decoder val))))
      ((: id (+ blank) "=")
       (let* ((key (the-substring 0 (-fx (the-length) 2)))
	      (val (read/rp attribute-value-grammar (the-port) strict tag)))
	  (let loop ((i (-fx (string-length key) 1)))
	     (case (string-ref key i)
		((#\space #\tab #\Newline)
		 (loop (-fx i 1)))
		(else
		 (set! key (substring key 0 (+ i 1))))))
	  (cons (string->symbol (decoder key)) (decoder val))))
      ((: id)
       (let* ((key (decoder (the-string))))
	  (cons (string->symbol key) key)))
      ((or "/>" ">")
       (the-symbol))
      (else
       (let ((c (the-failure)))
	  (if (not (eof-object? c))
	      (xml-parse-error "Illegal attribute character"
			       (error-line c (the-port))
			       (input-port-name (the-port))
			       (input-port-position (the-port)))
	      (xml-parse-error
	       (format "Premature end of line, expecting tag `~a'" tag)
	       c
	       (input-port-name (the-port))
	       (-fx (input-port-position (the-port)) 1)))))))

;*---------------------------------------------------------------------*/
;*    cdata-grammar ...                                                */
;*---------------------------------------------------------------------*/
(define cdata-grammar
   (regular-grammar (decoder)
      ((* (out "]"))
       (let* ((res (decoder (the-string)))
	      (rest (ignore)))
	  (string-append res rest)))
      ("]"
       (string-append "]" (ignore)))
      ((: "]]>" (? "\n"))
       "")
      (else
       (let* ((c (the-failure))
	      (msg (if (not (eof-object? c))
		       "Illegal <![CDATA[ character"
		       "Premature end of line, expecting tag `]]>'")))
	  (xml-parse-error msg
			   c
			   (input-port-name (the-port))
			   (input-port-position (the-port)))))))

;*---------------------------------------------------------------------*/
;*    get-decoder ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-decoder::procedure enc::bstring dst-enc)
   (let ((src-enc (string->symbol (string-upcase enc))))
      (cond
	 ((or (not src-enc) (eq? src-enc dst-enc))
	  (lambda (x) x))
	 ((eq? src-enc 'UTF-8)
	  (cond
	     ((memq dst-enc '(ISO-8859-1 ISO-8859-2 ISO-8859-15))
	      utf8->iso-latin)
	     ((eq? dst-enc 'UCS-2)
	      utf8-string->ucs2-string)
	     (else
	      (lambda (x) x))))
	 ((memq src-enc '(ISO-8859-1 ISO-8859-2 ISO-8859-15))
	  (cond
	     ((eq? dst-enc 'UTF-8)
	      iso-latin->utf8)
	     ((eq? dst-enc 'UCS-2)
	      (lambda (x)
		 (utf8-string->ucs2-string (iso-latin->utf8 x))))
	     (else
	      (lambda (x) x))))
	 (else
	  (lambda (x) x)))))
      
;*---------------------------------------------------------------------*/
;*    xml-grammar ...                                                  */
;*---------------------------------------------------------------------*/
(define xml-grammar
   (regular-grammar ((id (: (in ("azAZ") "!?") (* (in ("azAZ09") ":_-"))))
		     next
		     make
		     specials
		     strict
		     decoder
		     encoding)
      
      ((+ (in " \t\n\r"))
       (the-string))
      ((: "<!--"
	  (* (or (out "-") (: "-" (out "-")) (: "--" (out ">"))))
	  "-->")
       (cons 'comment (the-string)))
      ((: "<!" (: (or (out "[-") (: "-" (out "-")))
		  (* (out ">]"))
		  (? (: "[" (* (out "]")) "]"))
		  (* (out ">"))) ">")
       (cons 'declaration (the-string)))
      ("<![CDATA["
       (cons 'cdata (read/rp cdata-grammar (the-port) decoder)))
      ((: "<?xml " (* (out "?>")) "?>")
       (let ((s (the-substring 6 (the-length))))
	  (string-set! s (-fx (string-length s) 2) #\space)
	  (let ((p (open-input-string s)))
	     (let loop ((attr '()))
		(let ((obj (read/rp attribute-grammar p 'xml #t decoder)))
		   (cond
		      ((pair? obj)
		       (loop (cons obj attr)))
		      ((eq? obj '>)
		       (cons 'xml-decl attr))))))))
      ((: "<?" (* (out ">")) ">")
       (cons 'instruction (the-string)))
      ((: "<" id ">")
       (let* ((t (the-substring 1 (-fx (the-length) 1)))
	      (ts (string->symbol t))
	      (p (the-port)))
	  (collect-up-to ignore ts '() p make specials strict decoder encoding)))
      ((: "<" id "/>")
       (let ((t (the-substring 1 (-fx (the-length) 2))))
	  (make (string->symbol t) '() '())))
      ((: "<" id (in " \n\t\r"))
       (let* ((t (the-substring 1 (-fx (the-length) 1)))
	      (ts (string->symbol t))
	      (p (the-port)))
	  (let loop ((attr '()))
	     (let ((obj (read/rp attribute-grammar p t strict decoder)))
		(cond
		   ((pair? obj)
		    (loop (cons obj attr)))
		   ((eq? obj '>)
		    (collect-up-to ignore ts (reverse! attr) p make specials strict decoder encoding))
		   ((eq? obj '/>)
		    (make ts (reverse! attr) '())))))))
      ((: "</" id ">")
       (string->symbol (the-substring 2 (-fx (the-length) 1))))
      ((+ (out "<"))
       (decoder (the-string)))
      (else
       (let ((c (the-failure)))
	  (cond
	     ((not (eof-object? c))
	      (xml-parse-error "Illegal character"
			       (error-line c (the-port))
			       (input-port-name (the-port))
			       (input-port-position (the-port))))
	     (else
	      c))))))

;*---------------------------------------------------------------------*/
;*    char-hexnumeric? ...                                             */
;*---------------------------------------------------------------------*/
(define (char-hexnumeric? c)
   (or (char-numeric? c)
       (and (char>=? c #\A) (char<=? c #\F))
       (and (char>=? c #\a) (char<=? c #\f))))

;*---------------------------------------------------------------------*/
;*    xml-string-decode-inner! ...                                     */
;*---------------------------------------------------------------------*/
(define (xml-string-decode-inner! str ol nl res)
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
		(if (and (char=? c #\%) (<fx i ol-2))
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
			      (loop (+fx i 1) (+fx j 1)))))
		    (begin
		       (string-set! res j c)
		       (loop (+fx i 1) (+fx j 1)))))))))

;*---------------------------------------------------------------------*/
;*    xml-count ...                                                    */
;*---------------------------------------------------------------------*/
(define (xml-count str ol)
   (let loop ((i 0)
	      (c 0))
      (cond
	 ((=fx i ol)
	  c)
	 ((char=? (string-ref str i) #\&)
	  (cond
	     ((substring-at? str "&lt;" i)
	      (loop (+fx i 4) (+fx c 1)))
	     ((substring-at? str "&gt;" i)
	      (loop (+fx i 4) (+fx c 1)))
	     ((substring-at? str "&amp;" i)
	      (loop (+fx i 5) (+fx c 1)))
	     ((substring-at? str "&quot;" i)
	      (loop (+fx i 6) (+fx c 1)))
	     ((substring-at? str "&nbsp;" i)
	      (loop (+fx i 6) (+fx c 1)))
	     ((substring-at? str "&#" i)
	      (let liip ((i (+fx i 2)))
		 (cond
		    ((=fx i ol)
		     c)
		    ((char-numeric? (string-ref str i))
		     (liip (+fx i 1)))
		    (else
		     (loop (+fx i 1) (+fx c 1))))))
	     (else
	      (loop (+fx i 1) (+fx c 1)))))
	 (else
	  (loop (+fx i 1) (+fx c 1))))))

;*---------------------------------------------------------------------*/
;*    xml-decode ...                                                   */
;*---------------------------------------------------------------------*/
(define (xml-decode! str res ol nl)
   (let loop ((i 0)
	      (j 0))
      (cond
	 ((=fx i ol)
	  res)
	 ((char=? (string-ref str i) #\&)
	  (cond
	     ((substring-at? str "&lt;" i)
	      (string-set! res j #\<)
	      (loop (+fx i 4) (+fx j 1)))
	     ((substring-at? str "&gt;" i)
	      (string-set! res j #\>)
	      (loop (+fx i 4) (+fx j 1)))
	     ((substring-at? str "&amp;" i)
	      (string-set! res j #\&)
	      (loop (+fx i 5) (+fx j 1)))
	     ((substring-at? str "&quot;" i)
	      (string-set! res j #\")
	      (loop (+fx i 6) (+fx j 1)))
	     ((substring-at? str "&nbsp;" i)
	      (string-set! res j #\space)
	      (loop (+fx i 6) (+fx j 1)))
	     ((substring-at? str "&#" i)
	      (let liip ((i (+fx i 2))
			 (n 0))
		 (if (=fx i ol)
		     res
		     (let ((c (string-ref str i)))
			(if (char-numeric? c)
			    (liip (+fx i 1)
			       (+fx (*fx n 10)
				  (-fx (char->integer c)
				     (char->integer #\0))))
			    (begin
			       (string-set! res j (integer->char n))
			       (loop (+fx i 1) (+fx j 1))))))))
	     (else
	      (string-set! res j (string-ref str i))
	      (loop (+fx i 1) (+fx j 1)))))
	 (else
	  (string-set! res j (string-ref str i))
	  (loop (+fx i 1) (+fx j 1))))))

;*---------------------------------------------------------------------*/
;*    xml-string-decode ...                                            */
;*---------------------------------------------------------------------*/
(define (xml-string-decode str)
   (let ((ol (string-length str)))
      (if (>=fx ol 3)
	  (let ((nl (xml-count str ol)))
	     (if (=fx nl ol)
		 (string-copy str)
		 (let ((res (make-string nl)))
		    (xml-decode! str res ol nl)
		    res)))
	  (string-copy str))))
   
;*---------------------------------------------------------------------*/
;*    xml-string-decode! ...                                           */
;*---------------------------------------------------------------------*/
(define (xml-string-decode! str)
   (let ((ol (string-length str)))
      (if (>=fx ol 3)
	  (let ((nl (xml-count str ol)))
	     (if (=fx nl ol)
		 str
		 (begin
		    (xml-decode! str str ol nl)
		    (string-shrink! str nl))))
	  str)))

;*---------------------------------------------------------------------*/
;*    xml-string-encode ...                                            */
;*---------------------------------------------------------------------*/
(define (xml-string-encode str)
   (define (count str ol)
      (let loop ((i 0)
		 (n 0))
	 (if (=fx i ol)
	     n
	     (let ((c (string-ref str i)))
		(case c
		   ((#\")
		    (loop (+fx i 1) (+fx n 6)))
		   ((#\&)
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
			  ((#\")
			   (blit-string! "&quot;" 0 res j 6)
			   (loop (+fx i 1) (+fx j 6)))
			  (else
			   (string-set! res j c)
			   (loop (+fx i 1) (+fx j 1))))))))))
   (let ((ol (string-length str)))
      (encode str ol (count str ol))))
	 
;*---------------------------------------------------------------------*/
;*    read-xml ...                                                     */
;*---------------------------------------------------------------------*/
(define (read-xml #!optional (port::input-port (current-input-port)))
   (xml-parse port))

;*---------------------------------------------------------------------*/
;*    xml-metadata ...                                                 */
;*---------------------------------------------------------------------*/
(define (xml-metadata xml)
   (let ((xml-ver #f)
	 (xml-enc #f)
	 (xml-lang #f)
	 (root-ver 0.0)
	 (xml-root #f)
	 (xml-ns '()))
      (let loop1 ((l xml))
	 (when (pair? l)
	    (match-case (car l)
	       ((xml-decl . (and ?attr (?- . ?-)))
		(for-each (lambda (at)
			     (case (car at)
				((version) (set! xml-ver (cdr attr)))
				((encoding) (set! xml-enc (cdr attr)))))
			  attr))
	       ((?mark ?lattr . ?-)
		(let loop3 ((lattr lattr))
		   (unless xml-root (set! xml-root mark))
		   (when (pair? lattr)
		      (let ((attr (car lattr)))
			 (case (car attr)
			    ((xml:lang)
			     (set! xml-lang (cdr attr)))
			    ((xmlns)
			     (set! xml-root (cons (cdr attr) xml-root)))
			    ((version)
			     (set! root-ver (string->number (cdr attr))))
			    (else
			     (let ((str (symbol->string (car attr))))
				(when (substring=? str "xmlns:" 6)
				   (let* ((l (string-length str))
					  (s (substring str 6 l))
					  (si (string->symbol s)))
				      (set! xml-ns
					    (cons (cons (cdr attr) si)
						  xml-ns)))))))
			 (loop3 (cdr lattr)))))))
	    (loop1 (cdr l))))
      (unless xml-root
	 (error "xml-metadata" "Empty XML document !" xml))
      ;; Values are :
      ;; - XML Version (1.0 or 1.1) or #f
      ;; - XML Encoding (#f if unknown)
      ;; - xml:lang value
      ;; - Pair, which car is the first data markup, and
      ;;   cdr the default namespace
      ;; - xml first data markup version attribute (0 if unspecified)
      ;; - list of prefixed namespaces (prefix . path)
      (values xml-ver xml-enc xml-lang xml-root root-ver xml-ns)))
