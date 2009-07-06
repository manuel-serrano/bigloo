;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/src/Llib/lexer.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 18 08:28:42 2007                          */
;*    Last change :  Tue May  5 12:35:49 2009 (serrano)                */
;*    Copyright   :  2007-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Portable sqltiny lexer                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_lexer
   (export sqltiny-lexer))

;*---------------------------------------------------------------------*/
;*    loc ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct loc fname pos)

;*---------------------------------------------------------------------*/
;*    *keyword* ...                                                    */
;*---------------------------------------------------------------------*/
(define *keyword*
   (let* ((l '("CREATE" "DROP" "ALTER"
			"TABLE" "INDEX" "TRIGGER" "VIEW"
			"CONSTRAINT" "NULL" "ISNULL" "NOTNULL" "IN"
			"RENAME" "TO" "ADD" "COLUMN"
			"ON" "CONFLICT" "PRIMARY" "KEY" "AUTOINCREMENT"
			"UNIQUE" "CHECK" "DEFAULT" "COLLATE" "ASC" "DESC"
			"BEGIN" "END" "TRANSACTION"
			"INSERT" "INTO" "VALUES"
			"SELECT" "ALL" "DISTINCT" "FROM" "WHERE" "GROUP" "BY"
			"HAVING" "ORDER" "LIMIT" "OFFSET" "AS"
			"UNION" "INTERSECT" "EXCEPT" "OR" "IF" "EXISTS"
			"USING" "DELETE" "REPLACE" "SET" "UPDATE" "VACUUM"
			"PRAGMA"))
	  (t (make-hashtable (* 3 (length l)))))
      (for-each (lambda (v) (hashtable-put! t v #t)) l)
      t))

(define *like*
   (let ((l '("LIKE" "GLOB" "REGEXP" "MATCH"))
	 (t (make-hashtable 12)))
      (for-each (lambda (v) (hashtable-put! t v #t)) l)
      t))

(define *join*
   (let* ((l '("NATURAL" "LEFT" "RIGHT" "FULL" "OUTER" "INNER" "CROSS" "JOIN"))
	  (t (make-hashtable (* 3 (length l)))))
      (for-each (lambda (v) (hashtable-put! t v #t)) l)
      t))

;*---------------------------------------------------------------------*/
;*    the-loc ...                                                      */
;*---------------------------------------------------------------------*/
(define (the-loc ip)
   (loc (input-port-name ip) (input-port-position ip)))

;*---------------------------------------------------------------------*/
;*    strip-string! ...                                                */
;*    -------------------------------------------------------------    */
;*    Check for double " and '.                                        */
;*---------------------------------------------------------------------*/
(define (strip-string! str)
   (let* ((len (string-length str))
	  (len1 (-fx len 1)))
      (let loop ((i 0))
	 (if (>=fx i len1)
	     str
	     (let ((c (string-ref str i)))
		(if (or (char=? c #\') (char=? c #\"))
		    (if (char=? (string-ref str (+fx i 1)) c)
			(let liip ((i (+fx i 2))
				   (j (+fx i 1)))
			   (cond
			      ((>=fx i len)
			       (string-shrink! str j))
			      ((=fx i len1)
			       (string-set! str j (string-ref str i))
			       (string-shrink! str (+fx j 1)))
			      (else
			       (let ((c (string-ref str i)))
				  (if (or (char=? c #\') (char=? c #\"))
				      (if (char=? (string-ref str (+fx i 1)) c)
					  (begin
					     (string-set! str j c)
					     (liip (+fx i 2) (+fx j 1)))
					  (begin
					     (string-set! str j c)
					     (liip (+fx i 1) (+fx j 1))))
				      (begin
					 (string-set! str j c)
					 (liip (+fx i 1) (+fx j 1))))))))
			(loop (+fx i 1)))
		    (loop (+fx i 1))))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-lexer ...                                                */
;*---------------------------------------------------------------------*/
(define sqltiny-lexer
   (regular-grammar ()
      ;; multi-line comments
      ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
       (ignore))
      ;; single-line comments
      ((: "--" (* all))
       (ignore))
      ;; separators
      ((+ (in " \n\t"))
       (ignore))
      ;; parenthesis
      ("("
       (list 'PAR-OPEN (the-loc (the-port))))
      (")"
       (list 'PAR-CLO (the-loc (the-port))))
      ;; delimiters
      (";"
       (list 'SEMI-COMMA (the-loc (the-port))))
      (","
       (list 'COMMA (the-loc (the-port))))
      ("*"
       (list 'STAR (the-loc (the-port))))
      ("."
       (list 'DOT (the-loc (the-port))))
      ("="
       (list 'EGAL (the-loc (the-port))))
      ;; binary-op
      ((or "||" (uncase "or"))
       (list 'BINARY-OP 'or (the-loc (the-port))))
      ((uncase "AND")
       (list 'BINARY-OP 'and (the-loc (the-port))))
      ((or "/" "%" "+" "-" "<<" ">>" "&" "|" "<" "<=" ">" ">=" "==" "!=" "<>")
       (list 'BINARY-OP (the-symbol) (the-loc (the-port))))
      ;; unary-op
      ((or "!" "~")
       (list 'UNARY-OP (the-string) (the-loc (the-port))))
      ((uncase "NOT")
       (list 'NOT (the-loc (the-port))))
      ;; integer
      ((: (? #\-) (+ digit))
       (list 'INTEGER (the-fixnum) (the-loc (the-port))))
      ;; strings
      ((or (: #\" (* (or (out #\") "\"\"")) #\")
	   (: #\' (* (or (out #\') "''")) #\'))
       (list 'STRING (strip-string! (the-substring 1 -1))
	     (the-loc (the-port))))
      ;; identifier
      ((: (or #\_ alpha) (* (or #\_ alpha digit)))
       (let* ((str (the-string))
	      (id (string-upcase str)))
	  (cond
	     ((hashtable-get *keyword* id)
	      (list (string->symbol id) (the-loc (the-port))))
	     ((hashtable-get *like* id)
	      (list 'LIKE-OP (string->symbol id) (the-loc (the-port))))
	     ((hashtable-get *join* id)
	      (list 'JOIN-OP (string->symbol id) (the-loc (the-port))))
	     (else
	      (list 'NAME str (the-loc (the-port)))))))
      ;; error or eof
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (raise
	       (instantiate::&io-parse-error
		  (fname (input-port-name (the-port)))
		  (location (input-port-position (the-port)))
		  (proc 'sqltiny)
		  (msg "Illegal character")
		  (obj (sqltiny-lexer-error-message c (the-port))))))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-lexer-error-message ...                                  */
;*---------------------------------------------------------------------*/
(define (sqltiny-lexer-error-message c port)
   (if (char? c)
       (let ((line (read-line port)))
	  (string-for-read
	   (string-append "{" (string c) "}" (if (string? line) line ""))))
       c))
