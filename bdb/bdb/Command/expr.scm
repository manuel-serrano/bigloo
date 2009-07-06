;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Command/expr.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Aug  3 19:05:19 1999                          */
;*    Last change :  Thu Feb  8 08:38:30 2001 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Gdb expression handling.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module command_expr
   (import tools_tools
	   tools_speek
	   engine_param
	   patient_mangling
	   (bdb-notify-error tools_error))
   (export (bdb-expr->gdb-expr::bstring ::bstring ::obj)
	   (gdb-expr->bdb-expr::bstring ::bstring)
	   (reset-expression-table!)))

;*---------------------------------------------------------------------*/
;*    gdb-expr->bdb-expr ...                                           */
;*---------------------------------------------------------------------*/
(define (gdb-expr->bdb-expr expr::bstring)
   (if (hashtable? *expression-table*)
       (let ((bdb-expr (hashtable-get *expression-table* expr)))
	  (if bdb-expr bdb-expr expr))
       expr))
      
;*---------------------------------------------------------------------*/
;*    bdb-expr->gdb-expr ...                                           */
;*    -------------------------------------------------------------    */
;*    This function convert a Bdb expression into a Gdb expression.    */
;*    For this conversion, the type of EXPR is examined. If EXPR       */
;*    stands for a Bigloo expression it is translated into a C         */
;*    expression. If EXPR is a simple Bigloo identifier it is          */
;*    mangled. Otherwise, EXPR is left unchanged.                      */
;*    -------------------------------------------------------------    */
;*    This function stores in a hash table the expression it converts. */
;*    This is used to avoid recompilation of a same expression and     */
;*    this is used to print to the user its expression instead of      */
;*    compiled expression.                                             */
;*---------------------------------------------------------------------*/
(define (bdb-expr->gdb-expr expr::bstring gdb-fun)
   (if (memq *bdb-mode* '(scheme mixte))
       (let ((res (if (and (string? gdb-fun)
			   (bigloo-expr? expr))
		      (let ((res (bigloo-expr->gdb-expr expr gdb-fun)))
			 (if (string? res)
			     res
			     (c-expr->gdb-expr expr gdb-fun)))
		      (c-expr->gdb-expr expr gdb-fun))))
	  (init-expression-table!)
	  (hashtable-put! *expression-table* res expr)
	  (verbose 4 "bdb-expr->gdb-exp [" expr "] -> [" res #"]\n")
	  res)
       expr))

;*---------------------------------------------------------------------*/
;*    *expression-table* ...                                           */
;*---------------------------------------------------------------------*/
(define *expression-table* #unspecified)

;*---------------------------------------------------------------------*/
;*    reset-expression-table! ...                                      */
;*---------------------------------------------------------------------*/
(define (reset-expression-table!)
   (set! *expression-table* #unspecified))

;*---------------------------------------------------------------------*/
;*    init-expression-table! ...                                       */
;*---------------------------------------------------------------------*/
(define (init-expression-table!)
   (if (not (hashtable? *expression-table*))
       (set! *expression-table* (make-hashtable))))
   
;*---------------------------------------------------------------------*/
;*    bigloo-expr? ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function returns #t iff EXPR is a Bigloo expression.        */
;*    -------------------------------------------------------------    */
;*    A Bigloo expression is denoted by a string that start with       */
;*    an opening parenthesis and that is ended with a closing          */
;*    parenthesis.                                                     */
;*---------------------------------------------------------------------*/
(define (bigloo-expr? expr::bstring)
   (let ((len (string-length expr)))
      (and (>fx len 0)
	   (char=? (string-ref expr 0) #\()
	   (char=? (string-ref expr (-fx len 1)) #\)))))

;*---------------------------------------------------------------------*/
;*    bdb-mangle-expr ...                                              */
;*    -------------------------------------------------------------    */
;*    We don't know if the expression to be demangled is local or      */
;*    global, thus we try both.                                        */
;*---------------------------------------------------------------------*/
(define (bdb-mangle-expr expr::bstring gdb-fun)
   (let ((c (and (string? gdb-fun) (bdb-mangle2 gdb-fun expr))))
      (if (string? c)
	  c
	  (let ((c (bdb-mangle expr)))
	     (if (string? c)
		 c
		 #f)))))
   
;*---------------------------------------------------------------------*/
;*    c-expr->gdb-expr ...                                             */
;*    -------------------------------------------------------------    */
;*    Translating a C expression consists in a simple mangling         */
;*    stage.                                                           */
;*---------------------------------------------------------------------*/
(define (c-expr->gdb-expr expr gdb-fun)
   (if (bigloo-ident? expr)
       (let ((c (bdb-mangle-expr expr gdb-fun)))
	  (if (string? c)
	      c
	      expr))
       expr))

;*---------------------------------------------------------------------*/
;*    bigloo-expr->gdb-expr ...                                        */
;*    -------------------------------------------------------------    */
;*    If EXPR denotes a Bigloo expression we have to translate it      */
;*    into a form suitable for GDB. For instance, we have to turn      */
;*    Bigloo funcall into C funcall:                                   */
;*       (FOO X)  ->  BDB_FUNCALL(FOO_FOO_ENV, MAKE_PAIR(X_12, NIL))   */
;*    -------------------------------------------------------------    */
;*    BDB_FUNCALL is just an alias for APPLY.                          */
;*---------------------------------------------------------------------*/
(define (bigloo-expr->gdb-expr expr::bstring gdb-fun)
   ;; We read in the expression and we evaluate it. If any error is
   ;; raised, this function returns #f otherwise, it returns a string
   (try (let ((port (open-input-string expr)))
	   (unwind-protect
	      (let* ((exp (read-case-sensitive port))
		     (eof (read port)))
		 (if (not (eof-object? eof))
		     (error "eval" "Junk at end of expression" eof)
		     (compile-expr exp gdb-fun)))
	      (close-input-port port)))
	(lambda (escape obj proc msg)
	   (bdb-notify-error obj proc msg)
	   (escape #f))))

;*---------------------------------------------------------------------*/
;*    compile-expr ...                                                 */
;*    -------------------------------------------------------------    */
;*    BDB_FUNCALL is just an alias for APPLY.                          */
;*---------------------------------------------------------------------*/
(define (compile-expr exp gdb-fun)
   (define (compile exp site)
      (match-case exp
	 ((? symbol?)
	  (let* ((str (symbol->string exp))
		 (c   (bdb-mangle2 gdb-fun str)))
	     (if (string? c)
		 c
		 (let ((c (if (eq? site 'app)
			      (bdb-mangle-for-funcall str)
			      (bdb-mangle str))))
		    (if (string? c)
			c
			(error "eval" "Unknown function" str))))))
	 ((? number?)
	  (string-append "bdb_bint( " (number->string exp) " )"))
	 ((? boolean?)
	  (if exp "bdb_true()" "bdb_false()"))
	 ((? char?)
	  (string-append "bdb_bchar( " (number->string (char->integer exp)) " )"))
	 (()
	  "bdb_nil()")
	 (#unspecified
	  "BUNSPEC")
	 ((if ?test ?then)
	  (string-append "if( " (compile test 'no) " ) " (compile then 'no)))
	 ((if ?test ?then ?else)
	  (string-append "if( "
			 (compile test 'no) " ) "
			 (compile then 'no)
			 " else "
			 (compile else 'no)))
	 ((begin . ?body)
	  (if (null? body)
	      "()"
	      (begin
		 (string-append "(")
		 (let loop ((body body))
		    (if (null? (cdr body))
			(string-append (compile (car body) 'no) ")")
			(string-append (compile (car body) 'no) ", "
				       (loop (cdr body))))))))
	 ((?fun . ?args)
	  (let* ((fun  (compile fun 'app))
		 (args (let loop ((args args))
			  (if (null? args)
			      "bdb_nil()"
			      (let ((arg (compile (car args) 'no)))
				 (string-append "make_pair(" arg
						", "
						(loop (cdr args))
						")"))))))
	     (string-append "bdb_funcall( " fun ", " args " )")))))
   (compile exp 'no))

			      
	   
       
	   
       
	  
       


   
