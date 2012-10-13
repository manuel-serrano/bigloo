;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pkgcomp/src/Llib/interface.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 12 05:22:11 2006                          */
;*    Last change :  Sat Oct 13 07:56:18 2012 (serrano)                */
;*    Copyright   :  2006-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of the INTERFACE expander                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    @interface-source-directory ...                                  */
;*---------------------------------------------------------------------*/
(define (@interface-source-directory x name)
   (let ((access ((bigloo-module-resolver) name '*)))
      (if (pair? access)
	  (dirname (car access))
	  ".")))
   
;*---------------------------------------------------------------------*/
;*    @interface-expander ...                                          */
;*---------------------------------------------------------------------*/
(define (@interface-expander x e)
   (match-case x
      ((interface (and ?name (? symbol?)) . ?body)
       (cond
	  ((not (interface-valid-name? name))
	   (error 'interface "Illegal interface name" x))
	  ((or (not (list? body))
	       (not (every (lambda (x)
			      (and (pair? x)
				   (symbol? (car x))
				   (list? (cdr x))))
		       body)))
	   (error 'interface "Illegal interface body" x))
	  (else
	   (multiple-value-bind (lang source suffix export from import rest)
	      (@interface-body-compiler x body)
	      (let* ((dir (@interface-source-directory x name))
		     (src (cond
			     ((string? source)
			      source)
			     ((string? suffix)
			      (string-append (symbol->string name) "." suffix))
			     (else
			      (string-append
			       (symbol->string name)
			       "."
			       (pkgcomp-default-suffix)))))
		     (bigloo (apply append
				    (filter-map (lambda (c)
						   (when (eq? (car c) 'bigloo)
						      (cdr c)))
						rest)))
		     (mo (assq 'module-override bigloo))
		     (eo (assq 'export-override bigloo))
		     (er (assq 'export-replace bigloo)))
		 (if (pair? mo)
		     ;; use the embedded module clause
		     ;; instead of the interface
		     (@interface-source-module (make-file-name dir src))
		     (let* ((expo (if (pair? eo)
				      (@exports-override export (cdr eo))
				      export))
			    (exp (if (pair? er)
				     (@exports-replace expo (cdr er))
				     expo))
			    (bglo (if (pair? eo)
				      (remq! eo bigloo)
				      bigloo))
			    (bgl (if (pair? er)
				     (remq! er bglo)
				     bglo))
			    (mod `(module ,name
				     (export ,@exp)
				     ,@from
				     (import ,@import)
				     ,@(@define-macro-module)
				     (include ,(make-file-name dir src))
				     ,@bgl)))
			(evepairify mod x))))))))
      (else
       (error 'interface "Illegal interface syntax" x))))

;*---------------------------------------------------------------------*/
;*    @interface-source-module ...                                     */
;*---------------------------------------------------------------------*/
(define (@interface-source-module src)
   (with-input-from-file src
      (lambda ()
	 (let ((mod (read (current-input-port) #t)))
	    (set-cdr! (last-pair mod)
		      `(,@(@define-macro-module)
			(include ,src)))
	    mod))))

;*---------------------------------------------------------------------*/
;*    @define-macro-module ...                                         */
;*---------------------------------------------------------------------*/
(define (@define-macro-module)
   '((option (define @scmpkg-warning (bigloo-warning))
	     (bigloo-warning-set! 0))
     (option (define-macro (module . _) ''()))
     (option (bigloo-warning-set! @scmpkg-warning))))

;*---------------------------------------------------------------------*/
;*    @untype-ident ...                                                */
;*---------------------------------------------------------------------*/
(define (@untype-ident id)
   (if (not (symbol? id))
       id
       (let* ((string (symbol->string id))
	      (len    (string-length string)))
	  (let loop ((walker  0))
	     (cond
		((=fx walker len)
		 id)
		((and (char=? (string-ref string walker) #\:)
		      (<fx walker (-fx len 1))
		      (char=? (string-ref string (+fx walker 1)) #\:))
		 (string->symbol (substring string 0 walker)))
		(else
		 (loop (+fx walker 1))))))))

;*---------------------------------------------------------------------*/
;*    @exports-override ...                                            */
;*---------------------------------------------------------------------*/
(define (@exports-override exports new-exports)
   (define reptable (make-hashtable))
   (define (replace id old) (or (hashtable-get reptable id) old))
   ;; fill the hash table
   (for-each (lambda (e)
		(match-case e
		   ((? symbol?)
		    (hashtable-put! reptable e e))
		   ((macro ?id . ?-)
		    (hashtable-put! reptable id e))
		   ((expander ?id)
		    (hashtable-put! reptable id e))
		   (((or inline generic) ?id . ?-)
		    (hashtable-put! reptable (@untype-ident id) e))
		   ((class ?id . ?-)
		    (hashtable-put! reptable (@untype-ident id) e))
		   (((and (? symbol?) ?id) . ?-)
		    (hashtable-put! reptable (@untype-ident id) e))
		   (else
		    (error '@export-override "Illegal new export clause" e))))
	     new-exports)
   (map! (lambda (c)
	    (match-case c
	       ((? var-name?)
		(replace c c))
	       ((class ?id . ?-)
		(replace (@untype-ident id) c))
	       ((macro ?id . ?-)
		(replace id c))
	       ((?id . ?-)
		(replace (@untype-ident id) c))
	       (else
		c)))
	 exports))

;*---------------------------------------------------------------------*/
;*    @exports-replace ...                                             */
;*---------------------------------------------------------------------*/
(define (@exports-replace exports new-exports)
   (define (replace id old)
      (let ((cell (assq id new-exports)))
	 (if (pair? cell)
	     (cadr cell)
	     old)))
   (filter-map (lambda (c)
		  (match-case c
		     ((? var-name?)
		      (replace c c))
		     ((macro ?id . ?-)
		      (replace id c))
		     ((class ?id . ?-)
		      (replace (@untype-ident id) c))
		     ((?id . ?-)
		      (replace (@untype-ident id) c))
		     (else
		      c)))
	       exports))

;*---------------------------------------------------------------------*/
;*    @interface-body-compiler ...                                     */
;*    -------------------------------------------------------------    */
;*    The argument body is a well formed list.                         */
;*    -------------------------------------------------------------    */
;*    This function does not coalesces all the clause information      */
;*    so that it can be re-used by the package repository manager.     */
;*---------------------------------------------------------------------*/
(define (@interface-body-compiler x body::pair-nil)
   (let loop ((b body)
	      (language #f)
	      (source #f)
	      (suffix #f)
	      (exp '())
	      (frm '())
	      (imp '())
	      (rest '()))
      (if (null? b)
	  (values language source suffix exp frm imp rest)
	  (case (caar b)
	     ((language)
	      ;; language
	      (if language
		  (error 'interface "Language specified twice" (car b))
		  (loop (append (@interface-language-compiler (car b))
				(cdr b))
			(cadr (car b))
			source suffix exp frm imp rest)))
	     ((import)
	      ;; import
	      (loop (cdr b) language source suffix exp frm
		    (append! imp (@interface-import-compiler (car b)))
		    rest))
	     ((export)
	      ;; export
	      (multiple-value-bind (nexp nfrm)
		 (@interface-export-compiler (car b))
		 (loop (cdr b) language source suffix
		       (append! exp nexp)
		       (append! frm nfrm)
		       imp rest)))
	     ((suffix)
	      ;; suffix
	      (loop (cdr b) language source (cadr (car b))
		    exp frm imp rest))
	     ((source)
	      ;; source
	      (loop (cdr b) language (cadr (car b)) suffix
		    exp frm imp rest))
	     (else
	      (unless (memq (caar b) (pkgcomp-interface-keywords))
		 (warning 'interface
			  (format "Unknown interface keyword ~a: " (caar b))
			  (car b)))
	      (loop (cdr b) language source suffix exp frm imp
		    (cons (car b) rest)))))))

;*---------------------------------------------------------------------*/
;*    @interface-language-compiler ...                                 */
;*---------------------------------------------------------------------*/
(define (@interface-language-compiler clause)
   (let* ((l (cadr clause))
	  (lang (assq l (pkgcomp-languages)))
	  (rest (map (lambda (s)
			(symbol-append (pkgcomp-language-mark) l '- s))
		     (cddr clause))))
      (cond
	 ((not lang)
	  (list `(import ,(symbol-append (pkgcomp-language-mark) l) ,@rest)))
	 ((memq l (pkgcomp-native-languages))
	  (cdr lang))
	 (else
	  (cons `(import ,@rest) (cdr lang))))))

;*---------------------------------------------------------------------*/
;*    @interface-import-compiler ...                                   */
;*---------------------------------------------------------------------*/
(define (@interface-import-compiler clause)
   (if (every (lambda (e)
		 (or (symbol? e)
		     (and (list? e) (every symbol? e))))
	  (cdr clause))
       (cdr clause)
       (error 'interface "Illegal `import' clause" clause)))

;*---------------------------------------------------------------------*/
;*    @interface-export-compiler ...                                   */
;*---------------------------------------------------------------------*/
(define (@interface-export-compiler clause)
   (values (filter-map (lambda (c)
			  (@interface-export-clause-compiler c clause))
		       (cdr clause))
	   (append-map (lambda (c)
			  (@interface-from-clause-compiler c clause))
		       (cdr clause))))

;*---------------------------------------------------------------------*/
;*    @interface-export-clause-compiler ...                            */
;*    -------------------------------------------------------------    */
;*    c is a proper list.                                              */
;*---------------------------------------------------------------------*/
(define (@interface-export-clause-compiler c clause)
   (match-case c
      ((class . ?-)
       (@interface-export-class-compiler c))
      ((macro . ?-)
       (@interface-export-macro-compiler c))
      ((from . ?-)
       #f)
      ((? pair?)
       (@interface-export-function-compiler c))
      ((? var-name?)
       c)
      (else
       (error 'interface "Illegal `export' clause" (if (pair? c) c clause)))))

;*---------------------------------------------------------------------*/
;*    @interface-from-clause-compiler ...                              */
;*    -------------------------------------------------------------    */
;*    c is a proper list.                                              */
;*---------------------------------------------------------------------*/
(define (@interface-from-clause-compiler c clause)
   (match-case c
      ((class . ?-)
       '())
      ((macro . ?-)
       '())
      ((from . ?-)
       (@interface-export-from-compiler c))
      ((? pair?)
       '())
      ((? var-name?)
       '())
      (else
       (error 'interface "Illegal `export' clause" (if (pair? c) c clause)))))

;*---------------------------------------------------------------------*/
;*    @interface-export-exception-compiler ...                         */
;*---------------------------------------------------------------------*/
(define (@interface-export-exception-compiler c)
   (define (exc name parent body)
      (if (not (every symbol? body))
	  (error 'interface "Illegal exception clause" body)
	  (let ((cname (symbol-append name '|::| parent)))
	     (evepairify `(class ,cname ,@body) c))))
   (define (exc/parent name parent body)
      (let ((p (assq parent (pkgcomp-native-exceptions))))
	 (if (pair? p)
	     (exc name (cadr p) body)
	     (exc name parent body))))
   (match-case c
      ((?- (and ?name (? var-name?)) . ?body)
       (exc/parent name (pkgcomp-root-exception) body))
      ((?- ((and ?name (? var-name?)) (and ?parent (? var-name?))) . ?body)
       (exc/parent name parent body))
      (else
       (error 'interface "Illegal `exception' clause" c))))

;*---------------------------------------------------------------------*/
;*    @interface-export-class-compiler ...                             */
;*---------------------------------------------------------------------*/
(define (@interface-export-class-compiler c)
   (@class->class c))

;*---------------------------------------------------------------------*/
;*    @interface-export-macro-compiler ...                             */
;*---------------------------------------------------------------------*/
(define (@interface-export-macro-compiler c)
   (match-case c
      ((?- (and ?proto ((or (? symbol?) (? keyword?)) . ?-)))
       (evepairify (cons 'macro proto) c))
      (else
       (error 'interface "Illegal `macro' clause" c))))

;*---------------------------------------------------------------------*/
;*    @interface-export-from-compiler ...                              */
;*---------------------------------------------------------------------*/
(define (@interface-export-from-compiler c)
   (match-case c
      ((?- (? symbol?))
       (list (evepairify (cons 'from (cdr c)) c)))
      ((?- (and ?module (? symbol?)) . ?rest)
       (map (lambda (e)
	       (evepairify `(from (,e ,module)) c))
	    rest))
      (else
       (error 'interface "Illegal `from' clause" c))))

;*---------------------------------------------------------------------*/
;*    @interface-export-function-compiler ...                          */
;*---------------------------------------------------------------------*/
(define (@interface-export-function-compiler c)
   (cond
      ((not (symbol? (car c)))
       (error 'interface "Illegal `function' clause" c))
      ((srfi89-args? (cdr c))
       (@srfi89->dsssl-proto c))
      (else
       c)))

;*---------------------------------------------------------------------*/
;*    var-name? ...                                                    */
;*---------------------------------------------------------------------*/
(define (var-name? n)
   (symbol? n))

;*---------------------------------------------------------------------*/
;*    interface-valid-name? ...                                        */
;*---------------------------------------------------------------------*/
(define (interface-valid-name? n::symbol)
   (let* ((s (symbol->string n))
	  (l (string-length s)))
      (let loop ((i 0))
	 (cond
	    ((=fx i l)
	     #t)
	    ((char=? (string-ref s i) #\_)
	     (loop (+fx i 1)))
	    (else
	     (not (string-index s #\_ i)))))))
