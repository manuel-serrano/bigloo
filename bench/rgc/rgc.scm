;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bench/rgc/rgc.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 30 13:36:49 1998                          */
;*    Last change :  Tue Mar  1 11:50:20 2016 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A benchmark to test the reader performances.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read
   (main main))

;*---------------------------------------------------------------------*/
;*    *bigloo-case-senstive* ...                                       */
;*---------------------------------------------------------------------*/
(define *bigloo-case-sensitive* *bigloo-case-sensitive*)
   
;*---------------------------------------------------------------------*/
;*    *bigloo-interpreter* ...                                         */
;*---------------------------------------------------------------------*/
(define *bigloo-interpreter* #f)

;*---------------------------------------------------------------------*/
;*    Les variables de control du lecteur                              */
;*---------------------------------------------------------------------*/
(define *par-open*  0)
(define *bra-open*  0)

;*---------------------------------------------------------------------*/
;*    Parenthesis mismatch (or unclosing) errors.                      */
;*---------------------------------------------------------------------*/
(define *list-error-level* 20)
(define *list-errors*      (make-vector *list-error-level* #unspecified))
(define *vector-errors*    (make-vector *list-error-level* #unspecified))

;*---------------------------------------------------------------------*/
;*    Control variables.                                               */
;*---------------------------------------------------------------------*/
(define *position?*   #f)
(define *line-number* 1)
(define *dot-symbol*  (string->symbol ";"))

;*---------------------------------------------------------------------*/
;*    reader-reset! ...                                                */
;*---------------------------------------------------------------------*/
(define (reader-reset!)
   (set! *line-number* 1)
   (set! *par-open* 0)
   (set! *bra-open* 0))

;*---------------------------------------------------------------------*/
;*    *grammar* ...                                                    */
;*    -------------------------------------------------------------    */
;*    This grammar includes several extension to a regular scheme      */
;*    grammar:                                                         */
;*       - #unspecified                                                */
;*       - #<????>                                                     */
;*       - tagged vectors                                              */
;*       - structures                                                  */
;*       - dsssl extensions: keyword, #!key, #!optional, #!rest        */
;*       - #a??? ascii character specifications.                       */
;*       - #"..." C escaped strings.                                   */
;*       - #u???? UCS-2 characters.                                    */
;*---------------------------------------------------------------------*/
(define *grammar*
   (regular-grammar ((chiffre (in ("09")))
		     (hexa    (or chiffre (in "af" "AF")))
		     (float   (or (: (* chiffre) #\. (+ chiffre))
				  (: (+ chiffre) #\. (* chiffre))))
		     (lettre  (in (#\a #\z) (#\A #\Z)))
		     (special (in #\! #\@ #\~ #\$ #\%
				  #\^ #\& #\* #\> #\<
				  #\/ #\- #\_ #\+   
				  #\\ #\| #\= #\? #\: #\.))
		     (quote   (in #\" #\, #\' #\`))
		     (paren   (in #\( #\) #\[ #\] #\{ #\}))
		     (id      (: (* chiffre)
				 (or lettre special)
				 (* (or lettre special chiffre #\, #\' #\`))))
		     (blank   (in #\space #\tab)))

      ;; Newlines
      ((+ #\Newline)
       (set! *line-number* (+fx *line-number* (the-length)))
       (ignore))
      
      ;; blank lines
      ((+ blank)
       (ignore))
      
      ;; comments
      ((: ";" (* all))
       (ignore))

      ;; characters
      ((: #\# #\\ (or lettre chiffre special #\# quote paren ";" #\space))
       (string-ref (the-string) 2))
      ((: #\# #\\ (: lettre (+ lettre)))
       (let ((string    (the-string))
	     (char-name (string->symbol
			 (string-upcase
			  (substring (the-string) 2 (the-length))))))
	  (case char-name
	     ((newline NEWLINE)
	      #\Newline)
	     ((tab TAB)
	      #\tab)
	     ((space SPACE)
	      #\space)
	     ((return RETURN)
	      (integer->char 13))
	     (else
	      (error/location "read"
			      "Illegal character"
			      string
			      (input-port-name    (the-port))
			      (input-port-position (the-port)))))))

      ;; string
      ((: #\" (* (or (out #\\ #\") (: #\\ all))) #\")
       ;; string of chars
       (escape-scheme-string (the-substring 1 (-fx (the-length) 1))))
      ((: #\# #\" (* (or (out #\\ #\") (: #\\ all))) #\")
       ;; foreign strings of char
       (escape-C-string (the-substring 1 (-fx (the-length) 1))))

      ;; fixnum
      ((or (+ chiffre) (: #\- (+ chiffre)) (: #\+ (+ chiffre)))
       (the-fixnum))
      ((: "#o" (or (+ (in (#\0 #\7))) (: (in #\+ #\-) (in (#\0 #\7)))))
       (string->integer (substring (the-string) 2 (the-length)) 8))
      ((: "#d" (or (+ chiffre) (: (in #\+ #\-) chiffre)))
       (string->integer (substring (the-string) 2 (the-length)) 10))
      ((: "#x" (or (+ (or chiffre (in (#\a #\f)) (in (#\A #\F))))
		   (: (in #\+ #\-) (+ (or chiffre (in (#\a #\f))
					  (in (#\A #\F)))))))
       (string->integer (substring (the-string) 2 (the-length)) 16))

      ((context pair (: #\. (* blank) #\)))
       (error/location "read"
		       "Illegal pair"
		       (the-string)
		       (input-port-name    (the-port))
		       (input-port-position (the-port))))

      ;; doted pairs
      ((context pair #\.)
       *dot-symbol*)
      (#\.
       (error/location "read"
		       "Illegal token"
		       #\.
		       (input-port-name    (the-port))
		       (input-port-position (the-port))))

      ;; unspecified and eof-object
      ((: #\# (or #\u #\e) (+ lettre))
       (let ((symbol (string->symbol
		      (string-upcase
		       (substring (the-string) 1 (the-length))))))
	  (case symbol
	     ((unspecified UNSPECIFIED)
	      unspec)
	     ((eof-object EOF-OBJECT)
	      beof)
	     (else
	      (error/location "read"
			      "Illegal identifier"
			      symbol
			      (input-port-name    (the-port))
			      (input-port-position (the-port)))))))

      ;; booleans
      ((: #\# (in #\t #\T))
       #t)
      ((: #\# (in #\f #\F))
       #f)

      ;; identifier
      (id
       ;; this rule has to be placed after the rule matching the `.' char
       (the-symbol))

      ;; quotation
      (#\'
       (cons 'quote (cons (ignore) '())))
      (#\`
       (cons 'quasiquote (cons (ignore) '())))
      (#\,
       (cons 'unquote (cons (ignore) '())))
      ((: #\, #\@)
       (cons 'unquote-splicing (cons (ignore) '())))

      ;; lists
      ((in #\( #\[)
       (let ((open-key *par-open*)
	     (pos      (input-port-position (the-port)))
	     (line     *line-number*))
	  ;; if possible, we store the opening parenthesis.
	  (if (and (vector? *list-errors*)
		   (<fx open-key (vector-length *list-errors*)))
	      (vector-set-ur! *list-errors* open-key pos))
	  ;; and then, we compute the result list...
	  (set! *par-open* (+fx 1 *par-open*))
	  (rgc-context 'pair)
	  (let loop-pair ((walk (ignore))
			  (pos  pos)
			  (line line))
	     (cond
		((eq? walk *dot-symbol*)
		 ;; une pair pointee
		 (rgc-context)
		 (let ((cdr (ignore)))
		    (ignore)
		    (if (=fx open-key *par-open*)
			(begin
			   (rgc-context 'pair)
			   cdr)
			(error/location "read"
					"Illegal pair"
					cdr
					(input-port-name (the-port))
					(input-port-position (the-port))))))
		((=fx open-key *par-open*)
		 (if (=fx open-key 0)
		     (rgc-context))
		 '())
		(else
		 (let ((new-pos  (input-port-position (the-port)))
		       (new-line *line-number*))
		    (if *position?*
			;; we put position only on pairs.
			(econs walk
			       (loop-pair (ignore) new-pos new-line)
			       (list 'at
				     (input-port-name (the-port))
				     pos
				     line))
			(cons walk
			      (loop-pair (ignore) new-pos new-line)))))))))
      ((in #\) #\])
       (set! *par-open* (-fx *par-open* 1))
       (if (<fx *par-open* 0)
	   (begin
	      (set! *par-open* 0)
	      (ignore))
	   #f))

      ;; vectors
      ((: #\# #\() 
       (let ((open-key *par-open*))
	  ;; if possible, we store the opening parenthesis.
	  (if (and (vector? *vector-errors*)
		   (<fx open-key (vector-length *vector-errors*)))
	      (let ((pos (input-port-position (the-port))))
		 (vector-set-ur! *vector-errors* open-key pos)))
	  ;; and then, we compute the result list...
	  (set! *par-open* (+fx 1 *par-open*))
	  (let loop-vector ((walk  (ignore))
			    (res  '())
			    (len   0))
	     (cond
		((=fx open-key *par-open*)
		 (let ((vect ($create-vector len)))
		    (let loop-vector-inner ((i (-fx len 1))
					    (l res))
		       (if (=fx i -1)
			   vect
			   (begin
			      (vector-set! vect i (car l))
			      (loop-vector-inner (-fx i 1)
						 (cdr l)))))))
		(else
		 (loop-vector (ignore)
			      (cons walk res)
			      (+fx 1 len)))))))
      ;; error
      (else
       (let ((char (the-failure)))
	  (if (eof-object? char)
	      (cond
		 ((>fx *par-open* 0)
		  (let ((open-key (-fx *par-open* 1)))
		     (reader-reset!)
		     (if (and (<fx open-key (vector-length *list-errors*))
			      (fixnum? (vector-ref-ur *list-errors* open-key)))
			 (error/location "read"
					 "Unclosed list"
					 char
					 (input-port-name (the-port))
					 (vector-ref-ur *list-errors* open-key))
			 (error "read"
				"Unexpected end-of-file"
				"Unclosed list"))))
		 ((>fx *bra-open* 0)
		  (let ((open-key (-fx *bra-open* 1)))
		     (reader-reset!)
		     (if (and (<fx open-key (vector-length *vector-errors*))
			      (fixnum? (vector-ref-ur *vector-errors*
						     open-key)))
			 (error/location "read"
					 "Unclosed vector or structure"
					 char
					 (input-port-name (the-port))
					 (vector-ref-ur *vector-errors*
						       open-key))
			 (error "read"
				"Unexpected end-of-file"
				"Unclosed vector or structure"))))
		 (else
		  (reset-eof (the-port))
		  char))
	      (error/location "read"
			      "Illegal char"
			      (illegal-char-rep char)
			      (input-port-name    (the-port))
			      (input-port-position (the-port))))))))

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test)
   (let ((port (open-input-file "rgc.scm")))
      (let loop ((exp (read/rp *grammar* port))
		 (num 0))
	 (if (eof-object? exp)
	     (begin
		(close-input-port port)
		num)
	     (loop (read/rp *grammar* port)
		   (+fx num 1))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let loop ((i (if (pair? (cdr argv))
		     (string->number (cadr argv))
		     500)))
      (if (= i 1)
	  (print "rgc: " (test))
	  (begin
	     (test)
	     (loop (- i 1))))))
