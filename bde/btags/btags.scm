;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/btags/btags.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 28 07:24:34 1998                          */
;*    Last change :  Wed Jul 24 07:42:08 2013 (serrano)                */
;*    Copyright   :  2000-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo tag generator.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module btags
   (main main))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *btags-version*         "0.3")
(define *btags-append*          #f)
(define *btags-language*        #f)
(define *btags-table-name*      "TAGS")
(define *btags-etags*           "etags")
(define *btags-files*           '())
(define *btags-modules*         #f)
(define *btags-extern?*         #f)
(define *oport*                 #f)
(define *btags-define-var-list* '(define define-struct define-parameter))
(define *btags-define-fun-list* '(define define-inline define-generic
				    define-macro define-expander))
(define *btags-module-list*     '(module))
(define *btags-suffixes*        '("scm" "sch" "bgl"))
(define *btags-ignore-errors*   #f)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; we parse command line arguments
   (parse-args argv)
   ;; we start erasing the old table if not in append mode
   (if (not *btags-append*)
       (delete-file *btags-table-name*))
   ;; we emit the argument option for other bee tools (such as kbrowse)
   (btags-emit-configuration)
   ;; we produce tags entrie regarding the source language
   (for-each btags-file (reverse! *btags-files*))
   ;; we close the output port if currently opened
   (if (output-port? *oport*)
       (begin
	  (close-output-port *oport*)
	  (set! *oport* #f))))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (define (usage args-parse-usage level)
      (print "usage: bgltags [options] file ...")
      (newline)
      (args-parse-usage #f)
      (newline))
   (args-parse (cdr cmd-args)
      (("?")
       (usage args-parse-usage 1)
       (exit 1))
      ((("-help" "--help") (help "This help message"))
       (usage args-parse-usage 1)
       (exit 0))
      (("-a" (help "-a, --append" "Append to existing tag file."))
       (set! *btags-append* #t))
      ((("-s" "-suffix" "--suffix") ?suffix (help "Add source suffix"))
       (set! *btags-suffixes* (cons suffix *btags-suffixes*)))
      (("--append")
       (set! *btags-append* #t))
      (("-e" (help "-e, --extern" "Parse Bigloo extern clauses"))
       (set! *btags-extern?* #t))
      (("--extern")
       (set! *btags-extern?* #t))
      (("-l"
	?language
	(help "-l language, --language-language"
		  "Parse the following files according to the given language."))
       (if (string=? language "none")
	   (set! *btags-language* #f)
	   (set! *btags-language* language)))
      (("-l=?language")
       (if (string=? language "none")
	   (set! *btags-language* #f)
	   (set! *btags-language* language)))
      (("-o"
	?tagfile
	(help "-o tagfile, --output=tagfile"
		  "Explict name of file for tag table (default `TAGS')."))
       (set! *btags-table-name* tagfile))
      (("--output=?tagfile")
       (set! *btags-table-name* tagfile))
      (("-module" (help "Generate tag entry for module identifier"))
       (set! *btags-modules* #t))
      (("-v" (help "-v, --version"
		       "Print the current-version of the program"))
       (print *btags-version*)
       (exit 0))
      (("--version")
       (print *btags-version*)
       (exit 0))
      (("--define-var" ?k (synopsis "Add define keyword `(KWD VAR VAL)'"))
       (set! *btags-define-var-list*
	     (cons (string->symbol k) *btags-define-var-list*)))
      (("--define-fun" ?k (synopsis "Add define keyword `(KWD (ID ...) VAL)'"))
       (set! *btags-define-fun-list*
	     (cons (string->symbol k) *btags-define-fun-list*)))
      (("--module" ?k (synopsis "Add module keyword `(KWD ID ...)'"))
       (set! *btags-module-list*
	     (cons (string->symbol k) *btags-module-list*)))
      (("--ignore-error" (synopsis "Ignore errors"))
       (set! *btags-ignore-errors* #t))
      (else
       (if (string? *btags-language*)
	   (set! *btags-files* (cons (cons else *btags-language*)
				     *btags-files*))
	   (set! *btags-files* (cons else *btags-files*))))))

;*---------------------------------------------------------------------*/
;*    btags-file ...                                                   */
;*---------------------------------------------------------------------*/
(define (btags-file file)
   (if (pair? file)
       (let ((fname (car file))
	     (language (cdr file)))
	  (if (string=? language "bigloo")
	      (bigloo-btags fname)
	      (other-btags fname language)))
       (let ((suffix (suffix file)))
	  (cond
	     ((member suffix *btags-suffixes*)
	      (bigloo-btags file))
	     (else
	      (other-btags file #f))))))

;*---------------------------------------------------------------------*/
;*    other-btags ...                                                  */
;*    -------------------------------------------------------------    */
;*    We produce tags entries for other languages. This is quiet       */
;*    easy because we just call `etags'                                */
;*---------------------------------------------------------------------*/
(define (other-btags file language)
   (if (output-port? *oport*)
       (begin
	  (close-output-port *oport*)
	  (set! *oport* #f)))
   (let ((command (string-append *btags-etags*
				 (if language
				     (string-append " --language=" language)
				     "")
				 " --append"
				 " --output=" *btags-table-name* " "
				 file)))
      (system command)))
      
;*---------------------------------------------------------------------*/
;*    btags-emit-configuration ...                                     */
;*---------------------------------------------------------------------*/
(define (btags-emit-configuration)
   (define (btags-list l)
      (for-each (lambda (x)
		   (display "(meta-define define " *oport*)
		   (display (string-downcase (symbol->string x)) *oport*)
		   (fprint *oport* (string #a127 #\0 #\, #\0)))
		l))
   ;; first, we open the output file
   (if (not (output-port? *oport*))
       (begin
	  (set! *oport* (append-output-file *btags-table-name*))
	  (if (not (output-port? *oport*))
	      (error 'bgltags
		     "Can't open file for output"
		     *btags-table-name*))))
   ;; then we do the real job
   (if (or (pair? *btags-define-var-list*)
	   (pair? *btags-define-fun-list*)
	   (pair? *btags-module-list*))
       (begin
	  (fprint *oport* #a012)
	  (fprint *oport* "-,0")
	  (if (pair? *btags-define-var-list*)
	      (btags-list *btags-define-var-list*))
	  (if (pair? *btags-define-fun-list*)
	      (btags-list *btags-define-fun-list*))
	  (if (pair? *btags-module-list*)
	      (btags-list *btags-module-list*)))))

;*---------------------------------------------------------------------*/
;*    bigloo-btags ...                                                 */
;*    -------------------------------------------------------------    */
;*    The true jobs, the Bigloo tags generator...                      */
;*---------------------------------------------------------------------*/
(define (bigloo-btags file)
   ;; first, we open the output file
   (if (not (output-port? *oport*))
       (begin
	  (set! *oport* (append-output-file *btags-table-name*))
	  (if (not (output-port? *oport*))
	      (error 'bgltags
		     "Can't open file for output"
		     *btags-table-name*))))
   ;; the real btags jobs
   (if *btags-ignore-errors*
       (with-handler
	  (lambda (e)
	     #f)
	  (bigloo-btags-file *oport* file))
       (bigloo-btags-file *oport* file)))

;*---------------------------------------------------------------------*/
;*    bigloo-btags-file ...                                            */
;*    -------------------------------------------------------------    */
;*    Because we want to avoid parsing source file without reading     */
;*    (with the regular Bigloo reader), we process two readings of     */
;*    a source file. The first one, we locate declarations and         */
;*    in the second one, we emit tags declarations.                    */
;*---------------------------------------------------------------------*/
(define (bigloo-btags-file oport file)
   (let ((decl-lines (find-declaration-lines file)))
      (let ((iport (and (file-exists? file)
			(open-input-file file))))
	 (if (not (input-port? iport))
	     (error 'bgltags "Can't open file for input" file)
	     (unwind-protect
		;; we have to reset the reader other line numbering is
		;; erroneous
		(reader-reset!)
		;; we now scan the file in order to emit the TAGs decls
		(let loop ((line  (read-line iport))
			   (lnum  1)
			   (pos   1)
			   (lines decl-lines)
			   (tags  ""))
		   (if (and (pair? lines) (not (eof-object? line)))
		       (if (=fx lnum (car (car lines)))
			   ;; this is a declaration line
			   (let ((tag (tags-substring line (cdr (car lines)))))
			      (loop line
				    lnum
				    pos
				    (cdr lines)
				    (if (string? tag)
					(string-append
					 tags
					 tag
					 (make-string 1 #a127)
					 (number->string lnum)
					 ","
					 ;; we have to remove 1 to the position
					 ;; otherwise etags (at least the one
					 ;; comes with gnu-emacs 21.2) gets
					 ;; wrong and search the next tag
					 ;; match
					 (number->string (-fx pos 1))
					 #"\n")
					tags)))
			   (loop (read-line iport)
				 (+fx lnum 1)
				 (+fx pos (+fx 1 (string-length line)))
				 lines
				 tags))
		       (begin
			  ;; we emit the file header
			  (fprint oport #a012)
			  (let ((len (string-length tags)))
			     (fprint oport file "," len)
			     (display tags oport)))))
		(close-input-port iport))))))

;*---------------------------------------------------------------------*/
;*    tags-substring ...                                               */
;*---------------------------------------------------------------------*/
(define (tags-substring str1 str2)
   (define (substring-at? start str1 str2)
      (let ((len2 (string-length str2)))
	 (let loop ((i1 start)
		    (i2 0))
	    (cond
	       ((=fx i2 len2)
		#t)
	       ((char-ci=? (string-ref str1 i1) (string-ref str2 i2))
		(loop (+fx i1 1) (+fx i2 1)))
	       (else
		#f)))))
   (let ((len1 (string-length str1)))
      (cond
	 ((pair? str2)
	  ;; if the argument is a pair instead of a string, it means that
	  ;; it is a typed identifier (such as a class or a method) that
	  ;; must be written with its type information. Nevertheless we
	  ;; have to take care that we must strip off all the extra
	  ;; information found at the line. For instead for a class declaration
	  ;; such as: (class point-3d::point (z (default 0)))
	  ;; we have to write:
	  ;; (class point-3d::point^?...)
	  (let loop ((r 0))
	     (cond
		((=fx r len1)
		 str1)
		((or (char=? (string-ref str1 r) #\))
		     (char=? (string-ref str1 r) #\;))
		 (substring str1 0 r))
		(else
		 (loop (+fx r 1))))))
	 ((vector? str2)
	  (string-append "(define (" (vector-ref str2 1)))
	 ((cell? str2)
	  (string-append "(define-parameter " (cell-ref str2) "-set!"))
	 (else
	  ;; this is neither a class nor a method...
	  (let* ((len2 (string-length str2))
		 (stop (-fx len1 len2))
		 (start (let loop ((j 0))
			   (cond
			      ((=fx j len1)
			       #f)
			      ((char=? (string-ref str1 j) #\space)
			       (+fx j 1))
			      (else
			       (loop (+fx j 1)))))))
	     (if (fixnum? start)
		 (let loop ((i start))
		    (cond
		       ((>fx i stop)
			#f)
		       ((substring-at? i str1 str2)
			(substring str1 0 (+fx i len2)))
		       (else
			(loop (+fx i 1)))))
		 #f))))))
 
;*---------------------------------------------------------------------*/
;*    find-declaration-lines ...                                       */
;*    -------------------------------------------------------------    */
;*    During this stage, we walk thru the code in order to find        */
;*    all the source lines that contains a declaration. For each       */
;*    of these lines, we allocate a pair containing the line number    */
;*    and a delimiter sentinel string.                                 */
;*---------------------------------------------------------------------*/
(define (find-declaration-lines file)
   (let ((iport (and (file-exists? file) (open-input-file file))))
      (if (not (input-port? iport))
	  (error 'bgltags "Can't open file for input" file)
	  (unwind-protect
	     (let ((exp (read iport #t)))
		(if (not (eof-object? exp))
		    (let ((lines (if (module? exp)
				     (append (btags-module exp)
					     (btags-expression exp))
				     (btags-expression exp))))
		       (let loop ((exp   (read iport #t))
				  (lines lines))
			  (if (eof-object? exp)
			      (reverse! lines)
			      (begin
				 (loop (read iport #t)
				       (append (btags-expression exp)
					       lines))))))))
	     (close-input-port iport)))))
	 
;*---------------------------------------------------------------------*/
;*    module? ...                                                      */
;*    -------------------------------------------------------------    */
;*    Is an expression a module expression.                            */
;*---------------------------------------------------------------------*/
(define (module? expr)
   (match-case expr
      (((? (lambda (x) (memq x *btags-module-list*))) (? symbol?) . ?-)
       #t)
      ((directives . ?-)
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    line-number ...                                                  */
;*---------------------------------------------------------------------*/
(define (line-number expr)
   (and (epair? expr)
	(match-case (cer expr)
	   ((at ?fname ?pos)
	    (file-position->line (+fx pos 1) fname))
	   (else
	    #f))))

;*---------------------------------------------------------------------*/
;*    id->string ...                                                   */
;*---------------------------------------------------------------------*/
(define (id->string id decl)
   (cond
      ((and (pair? id) (symbol? (car id)))
       (symbol->string (car id)))
      ((string? id)
       id)
      ((not (symbol? id))
       (error 'bgltags "Illegal identifier" decl))
      (else
       (let* ((string (symbol->string id))
	      (len    (string-length string)))
	  (let loop ((walker  0))
	     (cond
		((=fx walker len)
		 (symbol->string id))
		((and (char=? (string-ref string walker) #\:)
		      (<fx walker (-fx len 1))
		      (char=? (string-ref string (+fx walker 1)) #\:))
		 (substring string 0 walker))
		(else
		 (loop (+fx walker 1)))))))))

;*---------------------------------------------------------------------*/
;*    btags-module ...                                                 */
;*---------------------------------------------------------------------*/
(define (btags-module expr)
   (define (tags-module-clause lines clauses)
      (let loop ((clauses clauses)
		 (lines   lines))
	 (if (pair? clauses)
	     (let ((clause (car clauses)))
		(loop (cdr clauses)
		      (if (pair? clause)
			  (case (car clause)
			     ((eval)
			      (append (btags-eval-clause clause) lines))
			     ((static export)
			      (append (btags-static+export-clause clause)
				      lines))
			     ((extern)
			      (if *btags-extern?*
				  (append (btags-extern-clause clause) lines)
				  lines))
			     (else
			      lines))
			  lines)))
	     lines)))
   (match-case expr
      ((module (and ?name (? symbol?)) . ?clauses)
       (tags-module-clause
	(if (and (epair? expr) *btags-modules*)
	    (let ((lnum (line-number expr)))
	       (list (cons lnum (symbol->string name))))
	    '())
	clauses))
      ((directives . ?clauses)
       (tags-module-clause '() clauses))))

;*---------------------------------------------------------------------*/
;*    btags-clause-walker ...                                          */
;*---------------------------------------------------------------------*/
(define (btags-clause-walker parser clause)
   (let loop ((decls clause)
	      (lines '()))
      (if (pair? decls)
	  (let ((nlines (parser (car decls))))
	     (loop (cdr decls)
		   (if (list? nlines)
		       (append nlines lines)
		       lines)))
	  lines)))
   
;*---------------------------------------------------------------------*/
;*    btags-eval-clause ...                                            */
;*---------------------------------------------------------------------*/
(define (btags-eval-clause clause)
   (define (eval-parse decl)
      (match-case decl
	 ((import (and (? symbol?) ?var))
	  (let ((lnum (line-number decl)))
	     (if (number? lnum)
		 (list (cons lnum (symbol->string var))))))))
   (btags-clause-walker eval-parse clause))

;*---------------------------------------------------------------------*/
;*    btags-static+export-clause ...                                   */
;*---------------------------------------------------------------------*/
(define (btags-static+export-clause clause)
   (define (static+export-parse decl)
      (match-case decl
	 (((and ?class (or class final-class wide-class)) ?name . ?clauses)
	  (btags-class-clause decl class name clauses))))
   (btags-clause-walker static+export-parse clause))

;*---------------------------------------------------------------------*/
;*    btags-class-clause ...                                           */
;*---------------------------------------------------------------------*/
(define (btags-class-clause decl class id slots)
   (let ((name (id->string id decl)))
      (define (clines l cid ro cs)
	 (let ((sid (symbol->string cid)))
	    (if (number? l)
		(if ro
		    (cons (cons l (vector cid
					  (string-append name "-" sid)))
			  cs)
		    (cons* (cons l (vector cid
					   (string-append name "-" sid)))
			   (cons l (vector cid
					   (string-append name "-" sid "-set!")))
			   cs))
		cs)))
      (let loop ((slots slots)
		 (lines '()))
	 (if (null? slots)
	     (let ((l (line-number decl)))
		(if (number? l)
		    (reverse!
		     (cons* (cons l (vector name
					    (string-append name "?")))
			    (cons l (vector name
					    (string-append "make-" name)))
			    (cons l (vector name
					    (string-append "fill-" name "!")))
			    (cons l name)
			    (reverse! lines)))
		    (reverse! lines)))
	     (match-case (car slots)
		((? symbol?)
		 (let ((l (line-number slots)))
		    (loop (cdr slots)
			  (clines l (car slots) #f lines))))
		((* (and ?id (? symbol?)) . ?att)
		 (let ((l (or (line-number (cdr (car slots)))
			      (line-number (car slots))
			      (line-number slots)))
		       (ro (and (list? att) (memq 'read-only att))))
		    (loop (cdr slots)
			  (clines l id ro lines))))
		(((and ?id (? symbol?)) . ?att)
		 (let ((l (or (line-number (cdr (car slots)))
			      (line-number (car slots))
			      (line-number slots)))
		       (ro (and (list? att) (memq 'read-only att))))
		    (loop (cdr slots)
			  (clines l id ro lines))))
		(else
		 (loop (cdr slots) lines)))))))

;*---------------------------------------------------------------------*/
;*    btags-extern-clause ...                                          */
;*---------------------------------------------------------------------*/
(define (btags-extern-clause clause)
   (define (extern-parse decl)
      (if (epair? decl)
	  (match-case decl
	     ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
	      (let ((lnum (line-number decl)))
		 (if (number? lnum)
		     (list (cons lnum (string-append "\"" cname "\""))))))
	     ((or (macro ?l-name ?proto ?c-name)
		  (infix macro ?l-name ?proto ?c-name))
	      (let ((lnum (line-number decl)))
		 (if (number? lnum)
		     (list (cons lnum (id->string l-name decl))))))
	     ((macro ?l-name ?c-name)
	      (let ((lnum (line-number decl)))
		 (if (number? lnum)
		     (list (cons lnum (id->string l-name decl))))))
	     ((?l-name ?proto ?c-name)
	      (let ((lnum (line-number decl)))
		 (if (number? lnum)
		     (list (cons lnum (id->string l-name decl))))))
	     ((?l-name ?c-name)
	      (let ((lnum (line-number decl)))
		 (if (number? lnum)
		     (list (cons lnum (id->string l-name decl)))))))))
   (btags-clause-walker extern-parse (cdr clause)))

;*---------------------------------------------------------------------*/
;*    btags-expression ...                                             */
;*---------------------------------------------------------------------*/
(define (btags-expression expr)
   (define (var-keyword? id)
      (and (symbol? id) (memq id *btags-define-var-list*)))
   (define (fun-keyword? id)
      (and (symbol? id) (memq id *btags-define-fun-list*)))
   (define (module-keyword? id)
      (and (symbol? id) (memq id *btags-module-list*)))
   (match-case expr
      (((kwote cond-expand) . ?clauses)
       (letrec ((match-cond (lambda (requirement)
                              (match-case requirement
                                (((kwote and) . ?rest)
                                 (every match-cond rest))
                                (((kwote or) . ?rest)
                                 (any match-cond rest))
                                (((kwote not) ?req)
                                 (not (match-cond req)))
                                ((or srfi-0 srfi-1 srfi-2 srfi-6 srfi-8 srfi-9
                                     srfi-22 srfi-28 srfi-30 srfi-33
                                     bigloo bigloo-c bigloo-compile bigloo-debug
                                     bigloo3 bigloo38 debug)
                                 #t)
                                ((kwote else)
				 ;; cond-expand's else-clause
                                 #t)
                                (else
				 ;; no feature match
                                 #f)))))
         (let ((first-match (find (lambda (clause) (match-cond (car clause)))
			       clauses)))
           (if (and (pair? first-match) (pair? (cdr first-match)))
	       ;; definition might be any expression
	       (btags-expression (cadr first-match))
	       ;; requirement, but empty definition
	       '()))))
      ((begin . ?rest)
       (apply append (map btags-expression rest)))
      ((define-method (?fun . ?-) . ?-)
       ;; method are special because we are not seeking an exact match
       ;; for method because we want to write the type for which the
       ;; method overrides
       (let ((lnum (line-number expr)))
	  (if (number? lnum)
	      (list (cons lnum (list (id->string fun expr))))
	      '())))
      (((? fun-keyword?) (?fun . ?-) . ?-)
       (let ((lnum (line-number expr)))
	  (if (number? lnum)
	      (list (cons lnum (id->string fun expr)))
	      '())))
      (((and ?def (? var-keyword?)) (and (? symbol?) ?var) . ?-)
       (let ((lnum (line-number expr)))
	  (if (number? lnum)
	      (if (eq? def 'define-parameter)
		  (let ((s (id->string var expr)))
		     (list (cons lnum s)
			   (cons lnum (make-cell s))))
		  (list (cons lnum (id->string var expr))))
	      '())))
      (((? module-keyword?) (and (? symbol?) ?var) . ?-)
       (let ((lnum (line-number expr)))
	  (if (number? lnum)
	      (list (cons lnum (id->string var expr)))
	      '())))
      (else
       '())))

	  
       

