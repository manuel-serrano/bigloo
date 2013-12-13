;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/text/src/Llib/bibtex.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 12 14:57:58 2001                          */
;*    Last change :  Fri Dec 13 12:50:31 2013 (serrano)                */
;*    Copyright   :  2001-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A BibTeX parser, produces a list of UTF-8 entries.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __text_bibtex
   (option (set! *dlopen-init-gc* #t))
   (export (bibtex::pair-nil obj)
	   (bibtex-string::pair-nil ::bstring)
	   (bibtex-port::pair-nil ::input-port)
	   (bibtex-file::pair-nil ::bstring)
	   (bibtex-parse-authors::pair-nil ::bstring)))

;*---------------------------------------------------------------------*/
;*    bibtex ...                                                       */
;*---------------------------------------------------------------------*/
(define (bibtex obj)
   (cond
      ((input-port? obj)
       (bibtex-port obj))
      ((string? obj)
       (bibtex-file obj))
      (else
       (error "bibtex" "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    bibtex-port ...                                                  */
;*---------------------------------------------------------------------*/
(define (bibtex-port port::input-port)
   (parse-bibtex port))

;*---------------------------------------------------------------------*/
;*    bibtex-file ...                                                  */
;*---------------------------------------------------------------------*/
(define (bibtex-file file::bstring)
   (call-with-input-file file bibtex-port))

;*---------------------------------------------------------------------*/
;*    bibtex-string ...                                                */
;*---------------------------------------------------------------------*/
(define (bibtex-string str::bstring)
   (call-with-input-string str bibtex-port))
      
;*---------------------------------------------------------------------*/
;*    *bibtex-string-table* ...                                        */
;*---------------------------------------------------------------------*/
(define *bibtex-string-table* #unspecified)

;*---------------------------------------------------------------------*/
;*    make-bibtex-hashtable ...                                        */
;*---------------------------------------------------------------------*/
(define (make-bibtex-hashtable)
   (let ((table (make-hashtable)))
      (for-each (lambda (k)
		   (let ((cp (string-capitalize k)))
		      (hashtable-put! table k cp)
		      (hashtable-put! table cp cp)))
		'("jan" "feb" "mar" "apr" "may" "jun" "jul"
			"aug" "sep" "oct" "nov" "dec"))
      table))

;*---------------------------------------------------------------------*/
;*    parse-bibtex ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-bibtex port::input-port)
   (unless (hashtable? *bibtex-string-table*)
      (set! *bibtex-string-table* (make-bibtex-hashtable)))
   (with-handler
      (lambda (e)
	 (if (isa? e &io-parse-error)
	     (with-access::&error e (obj proc)
		(match-case obj
		   ((?token (?fname . ?pos) . ?val)
		    (error/location proc
		       "bibtex parse error"
		       token
		       fname
		       pos))
		   (else
		    (raise e))))
	     (raise e)))
      (read/lalrp bibtex-parser bibtex-lexer port)))

;*---------------------------------------------------------------------*/
;*    the-coord ...                                                    */
;*---------------------------------------------------------------------*/
(define (the-coord port)
   (cons (input-port-name port) (input-port-position port)))

;*---------------------------------------------------------------------*/
;*    bibtex-lexer ...                                                 */
;*---------------------------------------------------------------------*/
(define bibtex-lexer
   (regular-grammar ((blank (in " \t\n\r")))
      ;; separators
      ((+ blank)
       (list 'BLANK (the-coord (the-port))))
      ;; comments
      ((: "%" (* all))
       (ignore))
      ;; egal sign
      (#\=
       (list 'EGAL (the-coord (the-port))))
      ;; sharp sign
      ((: (* blank) #\# (* blank))
       (list 'SHARP (the-coord (the-port))))
      ;; open bracket
      (#\{
       (list 'BRA-OPEN (the-coord (the-port))))
      ;; close bracket
      (#\}
       (list 'BRA-CLO (the-coord (the-port))))
      ;; comma
      (#\,
       (list 'COMMA (the-coord (the-port))))
      ;; double quote
      ((: #\\ (in "\"\\_"))
       (list 'CHAR (the-coord (the-port)) (the-character)))
      ;; optional linebreak
      ((: #\\ #\-)
       (ignore))
      ;; special latin characters
      ((or "{\\'e}" "\\'e" "{\\`{e}}")
       (list 'CHAR (the-coord (the-port)) "\303\251"))
      ((or "{\\'E}" "\\'E" "{\\`{E}}")
       (list 'CHAR (the-coord (the-port)) "\303\211"))
      ((or "{\\o}" "\\o")
       (list 'CHAR (the-coord (the-port)) "\303\270"))
      ((or "{\\O}" "\\O")
       (list 'CHAR (the-coord (the-port)) "\303\230"))
      ((or "{\\~{n}}" "\\~{n}")
       (list 'CHAR (the-coord (the-port)) "\303\261"))
      ((or "{\\~{N}}" "\\~{N}")
       (list 'CHAR (the-coord (the-port)) "\303\221"))
      ((or "{\\^{o}}" "\\^{o}")
       (list 'CHAR (the-coord (the-port)) "\303\264"))
      ((or "{\\^{O}}" "\\^{O}")
       (list 'CHAR (the-coord (the-port)) "\303\224"))
      ((or "{\\\"{o}}" "\\\"{o}")
       (list 'CHAR (the-coord (the-port)) "\303\266"))
      ((or "{\\\"{O}}" "\\\"{O}")
       (list 'CHAR (the-coord (the-port)) "\303\226"))
      ((or "{\\`e}" "\\`e")
       (list 'CHAR (the-coord (the-port)) "\303\250"))
      ((or "{\\`E}" "\\`E")
       (list 'CHAR (the-coord (the-port)) "\303\210"))
      ((or "{\\^e}" "\\^e")
       (list 'CHAR (the-coord (the-port)) "\303\252"))
      ((or "{\\^E}" "\\^E")
       (list 'CHAR (the-coord (the-port)) "\303\212"))
      ((or "{\\`a}" "\\`a")
       (list 'CHAR (the-coord (the-port)) "\303\240"))
      ((or "{\\'a}" "\\'a")
       (list 'CHAR (the-coord (the-port)) "\303\241"))
      ((or "{\\`A}" "\\`A")
       (list 'CHAR (the-coord (the-port)) "\303\200"))
      ((or "{\\'A}" "\\'A")
       (list 'CHAR (the-coord (the-port)) "\303\201"))
      ((or "{\\\"i}" "{\\\"{i}}" "\\\"i" "\\\"{i}" "{\\\"\\i}" "\\\"{\\i}")
       (list 'CHAR (the-coord (the-port)) "\303\257"))
      ((or "{\\\"I}" "{\\\"{I}}" "\\\"I" "\\\"{I}" "{\\\"\\I}" "\\\"{\\I}")
       (list 'CHAR (the-coord (the-port)) "\303\217"))
      ((or "{\\^i}" "{\\^{i}}" "\\^i" "\\^{i}" "{\\^\\i}" "\\^{\\i}")
       (list 'CHAR (the-coord (the-port)) "\303\256"))
      ((or "{\\^I}" "{\\^{I}}" "\\^I" "\\^{I}" "{\\^\\I}" "\\^{\\I}")
       (list 'CHAR (the-coord (the-port)) "\303\216"))
      ((or "{\\'i}" "{\\'{i}}" "\\'i" "\\'{i}" "{\\'\\i}" "\\'{\\i}")
       (list 'CHAR (the-coord (the-port)) "\303\255"))
      ((or "{\\'I}" "{\\'{I}}" "\\'I" "\\'{I}" "{\\'\\I}" "\\'{\\I}")
       (list 'CHAR (the-coord (the-port)) "\303\215"))
      ((or "{\\\"u}" "\\\"u")
       (list 'CHAR (the-coord (the-port)) "\303\274"))
      ((or "{\\\"U}" "\\\"U")
       (list 'CHAR (the-coord (the-port)) "\303\234"))
      ((or "{\\`u}" "\\`u")
       (list 'CHAR (the-coord (the-port)) "\303\271"))
      ((or "{\\`U}" "\\`U")
       (list 'CHAR (the-coord (the-port)) "\303\231"))
      ;; latex commands
      ((: #\\ (or alpha (in "$&%#_{}")) (* (or alpha digit)))
       (let ((s (the-substring 1 (the-length))))
	  (cond
	     ((member s '("pi" "Pi" "lambda" "Lambda"))
	      (list 'IDENT (the-coord (the-port)) s))
	     ((and (substring-at? s "char" 0)
		   (pregexp-match "char[0-9]+" s))
	      (list 'SPECIAL
		    (the-coord (the-port))
		    (string-append "\\" s)))
	     ((and (=fx (string-length s) 1)
		   (member (string-ref s 0) '(#\$ #\& #\% #\# #\_ #\{ #\})))
	      (list 'CHAR (the-coord (the-port)) (string-ref s 0)))
	     (else
	      (ignore)))))
      ;; strings
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (list 'STRING
	     (the-coord (the-port))
	     (the-substring 1 (-fx (the-length) 1))))
      ;; commands
      ((: "@" (+ alpha))
       (let* ((str (string-upcase (the-substring 1 (the-length))))
	      (sym (string->symbol str)))
	  (case sym
	     ((STRING)
	      (list 'BIBSTRING (the-coord (the-port))))
	     (else
	      (list 'BIBITEM (the-coord (the-port)) sym)))))
      ;; digit
      ((+ digit)
       (list 'NUMBER (the-coord (the-port)) (the-string)))
      ;; ident
      ((+ (or alpha digit (in ".:-&/?+*_")))
       (let ((s (the-string)))
	  (if (string-ci=? s "author")
	      (list 'AUTHOR (the-coord (the-port)) (the-string))
	      (list 'IDENT (the-coord (the-port)) (the-string)))))
      ;; default
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (list 'CHAR (the-coord (the-port)) c))))))

;*---------------------------------------------------------------------*/
;*    bibtex-parser ...                                                */
;*---------------------------------------------------------------------*/
(define bibtex-parser
   (lalr-grammar
      ;; tokens
      (CHAR SPECIAL IDENT AUTHOR STRING COMMA BRA-OPEN BRA-CLO
	    SHARP BLANK NUMBER EGAL
	    BIBSTRING BIBITEM)
      
      ;; bibtex
      (bibtex
       (()
	'())
       ((bibtex string-def)
	bibtex)
       ((bibtex bibtex-entry)
	(cons bibtex-entry bibtex))
       ((bibtex BLANK)
	bibtex))
      
      ;; blank*
      (blank*
       (() '())
       ((blank* BLANK) '()))
      
      ;; string-def
      (string-def
       ((BIBSTRING BRA-OPEN blank* IDENT blank* EGAL blank* bibtex-entry-value blank* BRA-CLO)
	(bibtex-string-def! (cadr IDENT) bibtex-entry-value)))
      
      ;; bibtex-entry
      (bibtex-entry
       ((BIBITEM blank* BRA-OPEN blank* IDENT blank* COMMA
		 bibtex-entry-item* BRA-CLO)
	(make-bibtex-entry (cadr BIBITEM)
			   (cadr IDENT)
			   bibtex-entry-item*)))
      
      ;; bibtex-entry-item*
      (bibtex-entry-item*
       ((blank*)
	'())
       ((bibtex-entry-item)
	(list bibtex-entry-item))
       ((bibtex-entry-item COMMA bibtex-entry-item*)
	(cons bibtex-entry-item bibtex-entry-item*)))
      
      ;; bibtex-entry-item
      (bibtex-entry-item
       ((blank* IDENT blank* EGAL blank* bibtex-entry-value blank*)
	(cons (string->symbol (string-downcase (cadr IDENT)))
	      bibtex-entry-value))
       ((blank* AUTHOR blank* EGAL blank* bibtex-entry-value blank*)
	(cons (string->symbol (string-downcase (cadr AUTHOR)))
	      bibtex-entry-value)))
      
      ;; bibtex-entry-value
      (bibtex-entry-value
       ((NUMBER)
	(list (cadr NUMBER)))
       ((bibtex-entry-value-string)
	bibtex-entry-value-string)
       ((BRA-OPEN bibtex-entry-value-block* BRA-CLO)
	bibtex-entry-value-block*))
      
      ;; bibtex-entry-value-string
      (bibtex-entry-value-string
       ((bibtex-entry-value-string-simple)
	(list bibtex-entry-value-string-simple))
       ((bibtex-entry-value-string SHARP bibtex-entry-value-string-simple)
	`(,@bibtex-entry-value-string ,bibtex-entry-value-string-simple)))
      
      ;; bibtex-entry-value-string-simple
      (bibtex-entry-value-string-simple
       ((STRING)
	(cadr STRING))
       ((IDENT)
	`(ref ,(cadr IDENT))))
      
      ;; bibtex-entry-value-block*
      (bibtex-entry-value-block*
       (()
	'())
       ((bibtex-entry-value-block* bibtex-entry-value-block)
	(append bibtex-entry-value-block* bibtex-entry-value-block)))
      
      ;; bibtex-entry-value-block
      (bibtex-entry-value-block
       ((BRA-OPEN bibtex-entry-value-block* BRA-CLO)
	bibtex-entry-value-block*)
       ((COMMA)
	(list ","))
       ((SHARP)
	(list "#"))
       ((IDENT)
	(list (cadr IDENT)))
       ((BLANK)
	(list " "))
       ((EGAL)
	(list "="))
       ((CHAR)
	(list (cadr CHAR)))
       ((SPECIAL)
	(list (cadr SPECIAL)))
       ((NUMBER)
	(list (cadr NUMBER)))
       ((STRING)
	(list (string-append "\"" (cadr STRING) "\""))))))

;*---------------------------------------------------------------------*/
;*    bibtex-parse-authors ...                                         */
;*---------------------------------------------------------------------*/
(define (bibtex-parse-authors val)
   (define (parse-author auth)
      (let ((m (pregexp-match "^({[^}]+}) ({[^}]+})" auth)))
	 (if m
	     (list (string-capitalize! (caddr m)) (cadr m))
	     (let ((m (pregexp-match "^([^,]+),+[ \t\n](.+)$" auth)))
		(if m
		    (list (string-capitalize! (cadr m)) (caddr m))
		    (let ((m (pregexp-match "^((?:[A-Z][^A-Z]|[^A-Z ]+| [A-Z][^ A-Z])+)[ \t\n]([A-Z \n\t]+)$" auth)))
		       (if m
			   (list (string-capitalize! (caddr m)) (cadr m))
			   (list (string-capitalize! auth) ""))))))))
   (let ((stop (-fx (string-length val) (string-length " et al."))))
      (if (substring-at? val " et al." stop)
	  (let ((as (pregexp-split "[ \t\n]and[ \t\n]" (substring val 0 stop))))
	     (if (pair? as)
		 (append (map parse-author as) (list 'et-al))
		 '()))
	  (let ((as (pregexp-split "[ \t\n]and[ \t\n]" val)))
	     (if (pair? as)
		 (map parse-author as)
		 '())))))

;*---------------------------------------------------------------------*/
;*    bibtex-string-def! ...                                           */
;*---------------------------------------------------------------------*/
(define (bibtex-string-def! ident value)
   (define (->string value)
      (if (string? value)
	  value
	  (match-case value
	     (((and ?s (? string?)))
	      s)
	     (((and ?n (? number?)))
	      (number->string n))
	     (else
	      (apply string-append (map ->string value))))))
   (hashtable-put! *bibtex-string-table* ident (->string value)))

;*---------------------------------------------------------------------*/
;*    parse-entry-value ...                                            */
;*---------------------------------------------------------------------*/
(define (parse-entry-value val)
   (let loop ((val (reverse val))
	      (res ""))
      (cond
	 ((null? val)
	  (untexify res))
	 ((char? (car val))
	  (loop (cdr val) (string-append (string (car val)) res)))
	 ((string? (car val))
	  (loop (cdr val) (string-append (car val) res)))
	 (else
	  (match-case (car val)
	     ((ref ?ref)
	      (let ((h (hashtable-get *bibtex-string-table* ref)))
		 (loop (cdr val)
		       (if (string? h)
			   (string-append h res)
			   res))))
	     (else
	      (loop (cdr val) res)))))))

;*---------------------------------------------------------------------*/
;*    make-bibtex-entry ...                                            */
;*---------------------------------------------------------------------*/
(define (make-bibtex-entry kind ident value)
   (let ((fields (map (lambda (line)
			 (let ((key (car line))
			       (val (parse-entry-value (cdr line))))
			    (cons key
				  (if (eq? key 'author)
				      (bibtex-parse-authors val)
				      val))))
		      value)))
      `(,ident ,kind ,@fields)))

;*---------------------------------------------------------------------*/
;*    untexify ...                                                     */
;*---------------------------------------------------------------------*/
(define (untexify val)
   
   (define (untexify-math-string str)
      (string-case str
	 ((+ (out #\_ #\^ #\space #\Newline #\tab))
	  (let ((s (the-string)))
	     (string-append s (ignore))))
	 ((+ (in "^_"))
	  (ignore))
	 ((+ (in " \n\t"))
	  (string-append " " (ignore)))
	 (else
	  "")))
   
   (define (untexify-string str)
      (let ((s (pregexp-replace* "C[$]\\^[$]_[+][+][$][$]" str "C++")))
	 (string-case (pregexp-replace* "[{}]" s "")
	    ((+ (out #\\ #\$ #\space #\Newline #\tab #\~))
	     (let ((s (the-string)))
		(string-append s (ignore))))
	    ((: #\\
		(or (: "c" (+ (out #\h #\space)))
		    (: "ch" (+ (out #\a #\space)))
		    (: "cha" (+ (out #\r #\space)))
		    (: "char" (+ (out digit #\space)))
		    (: (out #\c) (+ (out #\space))))
		(? #\space))
	     (ignore))
	    ((: #\\ "char" (+ digit))
	     (string-append
	      (string
	       (integer->char
		(string->integer
		 (the-substring 5 (the-length)))))
	      (ignore)))
	    ((: #\$ (* (out #\$)) #\$)
	     (let ((s (the-substring 1 (-fx (the-length) 1))))
		(string-append (untexify-math-string s) (ignore))))
	    ((+ (in " \n\t~"))
	     (string-append " " (ignore)))
	    (else
	     ""))))

   (if (string? val)
       (untexify-string val)
       (map untexify val)))
