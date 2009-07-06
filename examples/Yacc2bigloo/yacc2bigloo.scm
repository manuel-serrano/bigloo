;<font size="-3"><pre>
;*=====================================================================*/
;*    .../prgm/project/bigloo/examples/Yacc2bigloo/yacc2bigloo.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 27 09:54:26 1995                          */
;*    Last change :  Wed Jan  8 05:56:34 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `Yacc grammar -> Bigloo grammar' translator                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module yacc->bigloo
   (main main))

;*---------------------------------------------------------------------*/
;*    Control variables                                                */
;*---------------------------------------------------------------------*/
(define *dest*    #f)
(define *src*     #f)
(define *oport*   (current-output-port))
(define *iport*   (current-input-port))
(define *verbose* 1)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (parse-args! (cdr args))
   (yacc->bigloo))

;*---------------------------------------------------------------------*/
;*    yacc->bigloo ...                                                 */
;*---------------------------------------------------------------------*/
(define (yacc->bigloo)
   (if (string? *dest*)
       (if (not (file-exists? *dest*))
	   (yacc->bigloo/oport)
	   (begin
	      (set! *oport* (open-output-file *dest*))
	      (if (not (output-port? *oport*))
		  (error "yacc->bigloo" "Can't open file for output" *dest*)
		  (unwind-protect (yacc->bigloo/oport)
				  (close-output-port *oport*)))))
       (begin
	  (set! *verbose* 0)
	  (yacc->bigloo/oport))))

;*---------------------------------------------------------------------*/
;*    yacc->bigloo/oport ...                                           */
;*---------------------------------------------------------------------*/
(define (yacc->bigloo/oport)
   (hello-world)
   (if (not *src*)
       (translate)
       (if (not (file-exists? *src*))
	   (error "yacc->bigloo" "Can't find file" *src*)
	   (begin
	      (set! *iport* (open-input-file *src*))
	      (if (not (input-port? *iport*))
		  (error "yacc->bigloo"
			 "Can't open file for input"
			 *src*)
		  (unwind-protect (translate)
				  (close-input-port *iport*)))))))

;*---------------------------------------------------------------------*/
;*    verbose ...                                                      */
;*---------------------------------------------------------------------*/
(define (verbose level . mes)
   (if (> *verbose* level)
       (apply display* mes)))

;*---------------------------------------------------------------------*/
;*    hello-world ...                                                  */
;*---------------------------------------------------------------------*/
(define (hello-world)
   ;; let's say hello
   (if (string? *src*)
       (verbose 0 (string-append *src* ":") #\Newline)))

;*---------------------------------------------------------------------*/
;*    parse-args! ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-args! args)
   (let loop ((args args))
      (cond
	 ((not (pair? args))
	  #t)
	 (else
	  (let ((pr (car args)))
	     (cond
;*--- -o --------------------------------------------------------------*/
		((string=? pr "-o")
		 (if (null? (cdr args))
		     (error "parse-args"
			    "one argument require for -o option" "")
		     (begin
			(set! *dest* (cadr args))
			(loop (cddr args)))))
;*--- --to-stdout -----------------------------------------------------*/
		((string=? pr "--to-stdout")
		 (set! *verbose* -1)
		 (set! *dest* '--to-stdout)
		 (loop (cdr args)))
;*--- -v --------------------------------------------------------------*/
		((string=? pr "-s")
		 ;; Le silence
		 (set! *verbose* -1)
		 (loop (cdr args)))
		((string=? pr "-v")
		 ;; La verbage
		 (set! *verbose* 1)
		 (loop (cdr args)))
		((string=? pr "-v2")
		 ;; La verbage
		 (set! *verbose* 2)
		 (loop (cdr args)))
		((string=? pr "-v3")
		 ;; La verbage
		 (set! *verbose* 3)
		 (loop (cdr args)))
;*--- else ------------------------------------------------------------*/
		(else
		 (if (string? *src*)
		     (error "yacc->bigloo" "don't know what to do with" pr)
		     (begin
			(set! *src* pr)
			(loop (cdr args)))))))))))
				     
;*---------------------------------------------------------------------*/
;*    help ...                                                         */
;*---------------------------------------------------------------------*/
(define (help)
   (print "usage: yacc->bigloo [options] [src_name]*")
   (newline)
   (print "   src_name         --  The name of the source files.")
   (print "   -o <name>        --  Name the output file <name>.")
   (print "   --to-stdout      --  Write C code on current output channel.")
   (exit 0))

;*---------------------------------------------------------------------*/
;*    token-reader ...                                                 */
;*---------------------------------------------------------------------*/
(define token-reader
   (regular-grammar ((letter (or #\_ (uncase (in ("az")))))
		     (digit  (in ("09"))))
      ((+ (in #\space #\newline))
	  (ignore))

      ;; comment
      ((: "/*" (* (or (out #\*) (: #\* (out #\/)))) #\* #\/)
       (ignore))

      ;; identifier
      ((: (or #\_ letter) (* (or #\_ letter #\_ digit)))
       (add-token! (the-string))
       (ignore))

      (else
       'done)))

;*---------------------------------------------------------------------*/
;*    prelude-lexer                                                    */
;*---------------------------------------------------------------------*/
(define prelude-lexer
   (let ((start-rule #f))
      (regular-grammar ((letter (or #\_ (uncase (in ("az")))))
			(digit  (in ("09"))))

	 ;; blank
	 ((+ (in #\space #\newline))
	  (ignore))

	 ;; comment
	 ((: "/*" (* (or (out #\*) (: #\* (out #\/)))) #\* #\/)
	  (ignore))

	 ;; token
	 ((: "%token " (* all))
	  (let* ((string (substring (the-string) 7 (the-length)))
		 (port   (open-input-string string)))
	     (read/rp token-reader port)
	     (ignore)))

	 ;; start
	 ((: "%start " (* all))
	  (let ((string (the-string))
		(len    (the-length)))
	     (set! start-rule (substring string 7 len))
	     (ignore)))
      
	 ;; %%
	 ("%%"
	  start-rule)

	 ;; else
	 (else
	  (error/location "yacc->bigloo"
			  "Illegal char"
			  (the-failure)
			  (input-port-name input-port)
			  (input-port-position input-port))))))

;*---------------------------------------------------------------------*/
;*    skip-semantics-action ...                                        */
;*---------------------------------------------------------------------*/
(define skip-semantics-action
   (let ((nested 1))
      (regular-grammar ()
	 
	 ;; comment
	 ((: "/*" (* (or (out #\*) (: #\* (out #\/)))) #\* #\/)
	  (ignore))

	 ;; string
	 ((: #\" (* (out #\")) #\")
	  (ignore))

	 (#\{
	  (set! nested (+fx nested 1))
	  (ignore))
	 
	 (#\}
	  (set! nested (-fx nested 1))
	  (if (>fx nested 0)
	      (ignore)
	      (begin
		 (set! nested 1)
		 'done)))
	 
	 ((+ (out #\} #\/ #\" #\{))
	  (ignore))

	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 (error/location "yacc->bigloo"
				 "Illegal semantic action"
				 c
				 (input-port-name input-port)
				 (input-port-position input-port))
		 (ignore)))))))
	 
;*---------------------------------------------------------------------*/
;*    lexer ...                                                        */
;*---------------------------------------------------------------------*/
(define lexer
   (regular-grammar ((letter (or #\_ (uncase (in ("az")))))
		     (digit  (in ("09"))))

      ;; blank
      ((+ (in #\space #\newline #\tab))
       (ignore))

      ;; comment
      ((: "/*" (* (or (out #\*) (: #\* (out #\/)))) #\* #\/)
       (ignore))

      ;; semantics actions
      (#\{
       (read/rp skip-semantics-action input-port)
       (ignore))
      
      ;; stop
      ("%%"
       'stop)

      ;; id
      ((: letter (* (or digit letter)))
       (list 'ID (the-string)))

      ;; :
      (#\:
       (list ':))

      ;; ;
      (#\;
       (list 'SEMI-COMMA))

      ;; |
      (#\|
       (list ''or))

      ;; "???"
      ((: #\" (* (or #\" (out #\" #\Newline ))) #\")
       (let* ((res   (the-substring 1 (-fx (the-length) 1)))
	      (token (cond
			((string=? res "")
			 'EMPTY-STRING)
			(else
			 res))))
	  (add-token! token)
	  (list 'ID token)))
      
      ;; '???'
      ((: #\' (or "\\'" (+ (out #\' #\Newline ))) #\')
       (let* ((res   (the-substring 1 (-fx (the-length) 1)))
	      (token (cond
			((string=? res "(")
			 'PAR-OPEN)
			((string=? res ")")
			 'PAR-CLO)
			((string=? res "{")
			 'BRA-OPEN)
			((string=? res "}")
			 'BRA-CLO)
			((string=? res "[")
			 'ANGLE-OPEN)
			((string=? res "]")
			 'ANGLE-CLO)
			((string=? res ";")
			 'SEMI-COMMA)
			((string=? res ",")
			 'COMMA)
			((string=? res ".")
			 'DOT)
			(else
			 res))))
	  (add-token! token)
	  (list 'ID token)))

      (else
       (error/location "yacc->bigloo"
		       "Illegal char"
		       (the-failure)
		       (input-port-name input-port)
		       (input-port-position input-port)))))

;*---------------------------------------------------------------------*/
;*    parser ...                                                       */
;*---------------------------------------------------------------------*/
(define parser
   (lalr-grammar
      ;; tokens
      (ID : OR SEMI-COMMA)
      ;; rules
      (rules
       (()
	`())
       ((rule rules)
	`(,rule ,@rules)))

      (rule
       ((ID : matches)
	`(,(car ID) ,matches)))

      (matches
       ((match OR matches)
	`(,match ,@matches))
       ((match SEMI-COMMA)
	`(,match)))

      (match
       (()
	'())
       ((ID match)
	`(,(car ID) ,@match)))))

;*---------------------------------------------------------------------*/
;*    id->b-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (id->b-string id)
   (define (c-string->b-string string)
      (let ((new (make-string (string-length string))))
	 (let loop ((i (-fx (string-length string) 1)))
	    (if (=fx i -1)
		new
		(begin
		   (case (string-ref string i)
		      ((#\_)
		       (string-set! new i #\-))
		      (else
		       (string-set! new i (string-ref string i))))
		   (loop (-fx i 1)))))))
   (match-case id
      ((ID (and (? string?) ?string))
       (c-string->b-string string))
      ((and (? string?) ?string)
       (c-string->b-string string))
      ((and (? symbol?) ?symbol)
       (c-string->b-string (symbol->string symbol)))
      (else
       id)))

;*---------------------------------------------------------------------*/
;*    make-match ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-match match)
   (match-case match
      ((?symbol)
       (let ((res (id->b-string symbol)))
	  (list (list res) res)))
      (else
       (let ((res (map id->b-string match)))
	  (list res (list 'quasiquote
			  (map (lambda (x) (list 'unquote x)) res)))))))

;*---------------------------------------------------------------------*/
;*    make-brule ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-brule name matches)
   `(,(id->b-string name) ,@(map make-match matches)))
     
;*---------------------------------------------------------------------*/
;*    make-rule ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-rule rule)
   (match-case rule
      ((?name ?matches)
       (add-rule! (make-brule name matches)))
      (else
       (error "yacc->bigloo" "Illegal rule shape" rule))))

;*---------------------------------------------------------------------*/
;*    translate ...                                                    */
;*---------------------------------------------------------------------*/
(define (translate)
   (let ((start-rule (read/rp prelude-lexer *iport*)))
      (if (not (string? start-rule))
	  (error "yacc->bigloo"
		 "Can't find starting rule"
		 #f)
	  (let ((start (gensym)))
	     (display "(lalr-grammar" *oport*)
	     (add-rule! `(,start ((,start-rule))))
	     (for-each make-rule
		       (try (read/lalrp parser
					lexer
					*iport*
					(lambda (x) (eq? x 'stop)))
			    (lambda (escape proc mes obj)
			       (match-case obj
				  ((?token (?fname . ?pos) . ?-)
				   (error/location proc
						   "Unexpected"
						   "token"
						   fname
						   pos))
				  (else
				   (error/location
				    proc
				    mes
				    obj
				    (input-port-name *iport*)
				    (input-port-position *iport*)))))))
	     (output-tokens)
	     (output-rules)
	     (display ")" *oport*)))))

;*---------------------------------------------------------------------*/
;*    *token* ...                                                      */
;*---------------------------------------------------------------------*/
(define *token* '())

;*---------------------------------------------------------------------*/
;*    add-token! ...                                                   */
;*---------------------------------------------------------------------*/
(define (add-token! token)
   (let ((key (cond
		 ((string? token)
		  (string->symbol token))
		 ((symbol? token)
		  token)
		 (else
		  #f))))
      (cond
	 ((not (symbol? key))
	  'done)
	 ((getprop key 'token)
	  'done)
	 (else
	  (putprop! key 'token #t)
	  (set! *token* (cons token *token*))))))

;*---------------------------------------------------------------------*/
;*    output-tokens ...                                                */
;*---------------------------------------------------------------------*/
(define (output-tokens)
   (fprint *oport* ";; tokens" #\Newline *token*))

;*---------------------------------------------------------------------*/
;*    *rule* ...                                                       */
;*---------------------------------------------------------------------*/
(define *rule* '())

;*---------------------------------------------------------------------*/
;*    add-rule! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-rule! rule)
   (set! *rule* (cons rule *rule*)))

;*---------------------------------------------------------------------*/
;*    output-rules ...                                                 */
;*---------------------------------------------------------------------*/
(define (output-rules)
   (fprint *oport* ";; rules")
   (for-each (lambda (rule)
		(fprint *oport* rule))
	     (reverse! *rule*)))
;</pre></font>
