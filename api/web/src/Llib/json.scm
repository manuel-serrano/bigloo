;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/api/web/src/Llib/json.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan  4 06:12:28 2014                          */
;*    Last change :  Tue Sep 10 16:46:58 2019 (serrano)                */
;*    Copyright   :  2014-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    JSON support                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_json

   (option (set! *unsafe-type* #t)
	   (set! *unsafe-arity* #t))

   (export (json-parse o::input-port #!key
	      array-alloc array-set array-return
	      object-alloc object-set object-return
	      parse-error (undefined #t) reviver expr
	      constant-alloc string-alloc)))

;*---------------------------------------------------------------------*/
;*    return ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (return key . val)
   `(list ,key
       ,(if (pair? val) (car val) '(the-string))
       (input-port-name (the-port))
       (input-port-position (the-port))))

;*---------------------------------------------------------------------*/
;*    *json-lexer* ...                                                 */
;*    -------------------------------------------------------------    */
;*    http://json.org/ for the exact grammar                           */
;*---------------------------------------------------------------------*/
(define *json-lexer*
   
   (regular-grammar (undefined constant-alloc string-alloc)
      
      ;; blank
      ((+ (in #\space #\newline #\tab #\return))
       (ignore))
      
      ;; commas
      (#\,
       (return 'COMMA))
      (#\:
       (return 'COLON))
      
      ;; angles
      (#\[
       (return 'ANGLE-OPEN))
      (#\]
       (return 'ANGLE-CLO))
      
      ;; parenthesis
      (#\(
       (return 'PAR-OPEN))
      (#\)
       (return 'PAR-CLO))
      
      ;; brackets
      (#\{
       (return 'BRA-OPEN))
      (#\}
       (return 'BRA-CLO))
      
      ;; integer constant
      ((: (? (in "+-")) (+ digit))
       (return 'CONSTANT (constant-alloc (the-integer))))
      
      ;; floating-point constant
      ((or (: (? #\-) (+ digit)
		      (: (in #\e #\E) (? (in #\- #\+)) (+ digit))
		      (? (in #\f #\F #\l #\L)))
	   (: (? #\-) (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
		      (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
		      (? (in #\f #\F #\l #\L))))
       (return 'CONSTANT (constant-alloc (the-flonum))))
      
      ;; string constant
      ((: "\"" (* (or (out #a000 #a001 #a002 #a003 #a004 #a005
			 #a006 #a007 #a008 #a009 #a010 #a011
			 #a012 #a013 #a014 #a015 #a016 #a017 #a018
			 #a019 #a020 #a021 #a022 #a023 #a024 #a025
			 #a026 #a027 #a028 #a029 #a030 #a031
			 #\\ #\")
		      (: #\\ (in "\"/\\bfnrt"))
		      (: #\\ #\u (= 4 (in ("09afAF"))))))
	  "\"")
       (let ((str (ucs2->utf8 (the-substring 1 (-fx (the-length) 1)) 0)))
	  (return 'STRING (string-alloc (string-as-read str)))))
      
      ;; identifier
      ((: (or #\_ alpha) (* (or #\_ alpha digit)))
       (case (the-symbol)
	  ((null) (return 'CONSTANT (constant-alloc '())))
	  ((undefined) (if undefined
			   (return 'CONSTANT (constant-alloc #unspecified))
			   (return 'ERROR #unspecified)))
	  ((true) (return 'CONSTANT (constant-alloc #t)))
	  ((false) (return 'CONSTANT (constant-alloc #f)))
	  (else (return 'ERROR (the-symbol)))))
      
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      (return 'EOS c)
	      (return 'ERROR (format "<~a>~a" c (read-chars 10 (the-port)))))))))

;*---------------------------------------------------------------------*/
;*    ucs2->utf8 ...                                                   */
;*---------------------------------------------------------------------*/
(define (ucs2->utf8 str start)
   
   (define (hex n)
      (cond
	 ((and (char>=? n #\0) (char<=? n #\9))
	  (-fx (char->integer n) (char->integer #\0)))
	 ((and (char>=? n #\a) (char<=? n #\f))
	  (+fx 10 (-fx (char->integer n) (char->integer #\a))))
	 ((and (char>=? n #\A) (char<=? n #\F))
	  (+fx 10 (-fx (char->integer n) (char->integer #\A))))
	 (else
	  0)))
   
   (define (utf8 str i)
      (let* ((c0 (hex (string-ref str i)))
	     (c1 (hex (string-ref str (+fx i 1))))
	     (c2 (hex (string-ref str (+fx i 2))))
	     (c3 (hex (string-ref str (+fx i 3))))
	     (n (+fx (bit-lsh (+fx (*fx c0 16) c1) 8) (+fx (*fx c2 16) c3)))
	     (u (integer->ucs2 n)))
	 (ucs2-string->utf8-string (make-ucs2-string 1 u))))
   
   (let ((len (string-length str)))
      (let loop ((i start))
	 (cond
	    ((=fx i len)
	     (if (=fx start 0)
		 str
		 (substring str start len)))
	    ((and (char=? (string-ref str i) #\\)
		  (<=fx (+fx i 6) len)
		  (char=? (string-ref str (+fx i 1)) #\u))
	     (if (>fx i start)
		 (string-append (substring str start i)
		    (utf8 str (+fx i 2))
		    (ucs2->utf8 str (+fx i 6)))
		 (string-append (substring str start i)
		    (utf8 str (+fx i 2))
		    (ucs2->utf8 str (+fx i 6)))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    *eot* ...                                                        */
;*---------------------------------------------------------------------*/
(define *eot* (cons 1 2))

;*---------------------------------------------------------------------*/
;*    json-parse ...                                                   */
;*---------------------------------------------------------------------*/
(define (json-parse o::input-port #!key
	   array-alloc array-set array-return
	   object-alloc object-set object-return
	   parse-error (undefined #t) reviver expr
	   constant-alloc string-alloc)
   
   (define (check-procedure proc arity name)
      (unless (and (procedure? proc) (correct-arity? proc arity))
	 (raise
	    (instantiate::&error
	       (proc "json-parse")
	       (msg (format "wrong argument \"~s\"" name))
	       (obj proc)))))
   
   (define last-token #f)

   (define cnst-alloc
      (if (and (procedure? constant-alloc) (correct-arity? constant-alloc 1))
	  constant-alloc
	  (lambda (x) x)))

   (define str-alloc
      (if (and (procedure? string-alloc) (correct-arity? string-alloc 1))
	  string-alloc
	  (lambda (x) x)))
      
   (define (read-token)
      (let ((t (read/rp *json-lexer* o undefined cnst-alloc str-alloc)))
	 (set! last-token t)
	 t))
   
   (define (parse-token type)
      (let ((token (read-token)))
	 (if (eq? (car token) type)
	     token
	     (parse-error (format "token \"~a\" expected" type)
		(caddr token)
		(cadddr token)))))

   (define (parse-token-error token)
      (if (eq? (car token) 'ERROR)
	  (parse-error
	     (format "wrong token: \"~a\"" (cadr token))
	     (caddr token) (cadddr token))
	  (parse-error
	     (format "wrong ~a token: \"~a\"" (car token) (cadr token))
	     (caddr token) (cadddr token))))
   
   (define (parse-array array)
      (let ((val (parse-text 'ANGLE-CLO)))
	 (if (eq? val *eot*)
	     (array-return array 0)
	     (begin
		(array-set array 0 val)
		(let loop ((i 1))
		   (let ((token (read-token)))
		      (case (car token)
			 ((ANGLE-CLO)
			  (array-return array i))
			 ((COMMA)
			  (let ((val (parse-text #f)))
			     (array-set array i val)
			     (loop (+fx i 1))))
			 (else
			  (parse-error "syntax error"
			     (caddr token) (cadddr token))))))))))
   
   (define (parse-object object)
      (let loop ()
	 (let ((token (read-token)))
	    (case (car token)
	       ((STRING)
		(parse-token 'COLON)
		(let* ((key (cadr token))
		       (val (parse-text #f)))
		   (if reviver
		       (let ((res (reviver object key val)))
			  (when res
			     (object-set object key res)))
		       (object-set object key val))
		   (loop)))
	       ((COMMA)
		(loop))
	       ((BRA-CLO)
		(object-return object))
	       (else
		(parse-token-error token))))))
   
   (define (parse-text end)
      (let loop ()
	 (let ((token (read-token)))
	    (case (car token)
	       ((ANGLE-OPEN)
		(parse-array (array-alloc)))
	       ((BRA-OPEN)
		(parse-object (object-alloc)))
	       ((CONSTANT STRING)
		(cadr token))
	       ((ERROR)
		(parse-token-error token))
	       (else
		(unless (eq? (car token) end)
		   (parse-token-error token))
		*eot*)))))
   
   (check-procedure array-alloc 0 :array-alloc)
   (check-procedure array-set 3 :array-set)
   (check-procedure array-return 2 :array-return)
   (check-procedure object-alloc 0 :object-alloc)
   (check-procedure object-set 3 :object-set)
   (check-procedure object-return 1 :object-return)
   (check-procedure parse-error 3 :parse-error)
   (when reviver (check-procedure reviver 3 :reviver))

   (let ((val (parse-text #f)))
      (unless expr
	 (let ((token (parse-text 'EOS)))
	    (unless (eq? token *eot*)
	       (if (pair? token)
		   (parse-error (format "Illegal JSON trailing ~a token: \"~a\""
				   (car token) (cadr token))
		      (cadr last-token) (caddr last-token))
		   (parse-error (format "Illegal JSON trailing token: \"~a\""
				   token)
		      #f #f)))))
      val))


