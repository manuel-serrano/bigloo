;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/action.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 17 07:44:53 2006                          */
;*    Last change :  Thu May 10 07:45:38 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bglpkg actions                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_action
   
   (library sqlite
	    pkglib)
   
   (import  bglpkg_param
	    bglpkg_extract
	    bglpkg_utils)
   
   (export  (bglpkg-db-list ::%sqlite)
	    (bglpkg-db-sync-list ::%sqlite)
	    (bglpkg-add-packages ::%sqlite ::pair-nil)
	    (bglpkg-remove-packages ::%sqlite ::pair-nil)
	    (bglpkg-inject-descriptions ::%sqlite ::pair-nil)
	    (bglpkg-sync ::%sqlite)
	    (bglpkg-wizard ::bstring ::pair-nil)
	    (bglpkg-query ::%sqlite ::pair-nil)
	    (bglpkg-with-package ::%sqlite ::bstring ::procedure)
	    (bglpkg-with-tuning ::%sqlite ::bstring ::bstring ::procedure)
	    (bglpkg-cleanup ::%sqlite ::obj)
	    (bglpkg-clear ::%sqlite ::obj)))

;*---------------------------------------------------------------------*/
;*    bglpkg-db-list ...                                               */
;*---------------------------------------------------------------------*/
(define (bglpkg-db-list db)
   (sqlite-map db
      (lambda (id name version language description url authors reldate)
	 (display (pkglib-color 'arg0 name))
	 (sqlite-map db
	    (lambda (version)
	       (sqlite-map db
		  (lambda (release path)
		     (display " ")
		     (cond
			((not (file-exists? path))
			 (display version)
			 (display "-")
			 (display release))
			((or (string=? url "") (string=? release "0"))
			 (display (pkglib-color 'warning version))
			 (display (pkglib-color 'warning "-"))
			 (display (pkglib-color 'warning release))
			 (display (pkglib-color 'warning "!")))
			(else
			 (display (pkglib-color 'arg1 version))
			 (display (pkglib-color 'arg1 "-"))
			 (display (pkglib-color 'arg1 release))
			 (display "*"))))
		  "SELECT release, path
                   FROM package
                   WHERE (name=~q) and (version=~q)
                   ORDER BY release DESC"
		  name version))
	    "SELECT version
             FROM package
             WHERE (name=~q)
             ORDER BY version DESC"
	    name)
	 (sqlite-eval db
	    (lambda (host)
	       (display* " " (pkglib-color 'error "fail")))
	    "SELECT host
              FROM port
              WHERE (package=~q) and (status='failure') and (host='bigloo')"
	    id)
	 (newline)
	 (when (>=fx (pkglib-verbose) 1)
	    (unless (string=? description "")
	       (display "  ")
	       (print (pkglib-color 'arg2 description))))
	 (when (>=fx (pkglib-verbose) 2)
	    (unless (string=? url "")
	       (print "  url: " url))
	    (unless (string=? authors "")
	       (print "  authors: " authors))
	    (let ((l language))
	       (print "  language: " (substring l 1 (- (string-length l) 1))))
	    (let ((tunings (sqlite-map db
			      (lambda (t) t)
			      "SELECT host
                                FROM tuning
                                WHERE (package=~q)
                                ORDER BY host"
			      id)))
	       (when (pair? tunings)
		  (print "  tunings: " tunings)))
	    (let ((dep (sqlite-map db
			  (lambda (n) n)
			  "SELECT name
                            FROM depend
                            WHERE (package=~q)
                            ORDER BY name"
			  id)))
	       (when (pair? dep)
		  (print "  dependencies: " dep))
	       (print "  sync date: " (seconds->date (string->elong reldate))))))
      "SELECT id, name, version, language, description, url, authors, reldate
        FROM package p
        ORDER BY name"))

;*---------------------------------------------------------------------*/
;*    bglpkg-db-sync-list ...                                          */
;*---------------------------------------------------------------------*/
(define (bglpkg-db-sync-list db)
   (print "(")
   (for-each (lambda (l)
		(display " (")
		(print (car l) " " (cadr l))
		(let loop ((l (cddr l)))
		   (when (pair? l)
		      (display "   ")
		      (write (car l))
		      (display " ")
		      (write (cadr l))
		      (newline)
		      (loop (cddr l))))
		(print "   )"))
	     (db-sync-list db))
   (print ")"))

;*---------------------------------------------------------------------*/
;*    bglpkg-add-package ...                                           */
;*---------------------------------------------------------------------*/
(define (bglpkg-add-package db p::pair-nil)
   (with-trace 3 'bglpkg-add-package
      (trace-item "p=" p)
      (multiple-value-bind (name version release tuning)
	 (package-name-parse (car p))
	 (if (string? tuning)
	     (for-each (lambda (p) (bglpkg-add-tuning db p)) p)
	     (let ((base (make-file-path (bglpkg-pkg-directory) name version)))
		(if (not (directory? base))
		    (if tuning
			(error 'bglpkg-add-packages
			       "Cannot install tuning without package"
			       name)
			(make-directories base)))
		(let* ((f (car p))
		       (dest (make-file-name base (basename f))))
		   (cond
		      ((not (file-exists? dest))
		       (copy-file f dest)
		       (when (>fx (pkglib-verbose) 0)
			  (print " "
				 (pkglib-color 'message name)))
		       (repo-add-package! db dest))
		      ((bglpkg-force-action)
		       (copy-file f dest))
		      (else
		       (error 'bglpkg-add-packages
			      "File already exists"
			      dest))))
		(for-each (lambda (t) (bglpkg-add-tuning db t)) (cdr p)))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-add-tuning ...                                            */
;*---------------------------------------------------------------------*/
(define (bglpkg-add-tuning db p)
   (multiple-value-bind (name version release tuning)
      (package-name-parse p)
      (let ((base (make-file-path (bglpkg-pkg-directory) name version)))
	 (if (not (directory? base))
	     (error 'bglpkg-add-tuning
		    "Cannot install tuning without package"
		    base)
	     (let ((dest (make-file-name base (basename p))))
		(cond
		   ((not (file-exists? dest))
		    (copy-file p dest)
		    (when (>fx (pkglib-verbose) 0)
		       (print " "
			      (pkglib-color 'message name)
			      " " (pkglib-color 'arg1 tuning)))
		    (repo-add-tuning! db dest))
		   ((bglpkg-force-action)
		    (copy-file p dest))
		   (else
		    (error 'bglpkg-add-tuning
			   "File already exists"
			   dest))))))))

;*---------------------------------------------------------------------*/
;*    tarball->packages ...                                            */
;*    -------------------------------------------------------------    */
;*    From a list of tarball builds a list of packages and their       */
;*    tunings. Example:                                                */
;*       (tarball->packages                                            */
;*         '("foo.tar.gz" "foo_bigloo.tar.gz" "bar.tar.gz"))           */
;*    =>                                                               */
;*       (("foo.tar.gz" "foo_bigloo.tar.gz") ("bar.tar.gz"))           */
;*---------------------------------------------------------------------*/
(define (tarball->packages lst)
   (let ((lst (filter package-filename? lst)))
      (if (null? lst)
	  '()
	  (multiple-value-bind (base version _ tuning)
	     (package-name-parse (car lst))
	     (let loop ((lst (cdr lst))
			(cur (list (car lst)))
			(pref base)
			(pkgs '()))
		(if (null? lst)
		    (reverse! (cons (reverse! cur) pkgs))
		    (multiple-value-bind (base version _ tuning)
		       (package-name-parse (car lst))
		       (if (string=? base pref)
			   (loop (cdr lst)
				 (cons (car lst) cur)
				 pref
				 pkgs)
			   (loop (cdr lst)
				 (list (car lst))
				 base
				 (cons (reverse! cur) pkgs))))))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-add-packages ...                                          */
;*---------------------------------------------------------------------*/
(define (bglpkg-add-packages db sources)
   (let ((packages (tarball->packages sources)))
      (for-each (lambda (p) (bglpkg-add-package db p)) packages)))

;*---------------------------------------------------------------------*/
;*    bglpkg-remove-packages ...                                       */
;*---------------------------------------------------------------------*/
(define (bglpkg-remove-packages db options)
   (define (bglpkg-remove-package db pkg)
      (repo-remove-package! db (bglpkg-pkg-directory) pkg))
   (define (bglpkg-remove-package-version db pkg version)
      (repo-remove-package-version! db (bglpkg-pkg-directory) pkg version))
   (let loop ((options options))
      (when (pair? options)
	 (let ((package (car options)))
	    (if (and (pair? (cdr options)) (package-version? (cadr options)))
		(begin
		   (bglpkg-remove-package-version db package (cadr options))
		   (loop (cddr options)))
		(begin
		   (bglpkg-remove-package db package)
		   (loop (cdr options))))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-inject-descriptions ...                                   */
;*---------------------------------------------------------------------*/
(define (bglpkg-inject-descriptions db files)
   (for-each (lambda (f) (bglpkg-inject-description db f)) files))

;*---------------------------------------------------------------------*/
;*    bglpkg-inject-description ...                                    */
;*---------------------------------------------------------------------*/
(define (bglpkg-inject-description db file)
   (unless (file-exists? file)
      (raise (instantiate::&io-error
		(proc 'bglpkg-inect-description)
		(msg "Cannot find file")
		(obj file))))
   (let ((p (open-input-file file)))
      (if (input-port? p)
	  (unwind-protect
	     (bglpkg-inject-description-list db (read p))
	     (close-input-port p))
	  (raise (instantiate::&io-read-error
		    (proc 'bglpkg-inect-description)
		    (msg "Cannot open file for input")
		    (obj file))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-inject-description-list ...                               */
;*---------------------------------------------------------------------*/
(define (bglpkg-inject-description-list db lst)
   (define (inject! x)
      (match-case x
	 ((interface ?name . ?rest)
	  (let* ((c (memq :path rest))
		 (v (memq :version rest))
		 (r (memq :release rest))
		 (version (if (pair? v) (cadr v) "0.0.1"))
		 (release (if (pair? r) (integer->string (cadr r)) "1"))
		 (sb (make-repo-package-path name version release))
		 (sp (make-file-name (bglpkg-pkg-directory) sb)))
	     ;; patch the path
	     (if (and (pair? c) (pair? (cdr c)))
		 (set-car! (cdr c) sp)
		 (set! rest (cons* :path sp rest)))
	     ;; remove the obsolete packages from the db (but not the repo)
	     (let ((pv (sqlite-map db
			  (lambda (name version)
			     (list name version))
			  "SELECT name, version
                           FROM package
                           WHERE (name=~q) AND
                                 ((version <= ~q) OR (release <= '~a'))"
			  name version release)))
		(for-each (lambda (pv)
			     (apply db-remove-package-version! db pv))
			  pv))
	     ;; register the package
	     (apply db-add-package! db name rest)))
	 ((tuning ?name . ?rest)
	  (let* ((c (memq :path rest))
		 (v (memq :version rest))
		 (r (memq :release rest))
		 (version (if (pair? v) (cadr v) "0.0.1"))
		 (release (if (pair? r) (integer->string (cadr r)) "1"))
		 (h (memq :host rest))
		 (host (if (pair? v)
			   (cadr v)
			   (error 'bglpkg-inject-description
				  "Illegal host"
				  h)))
		 (sb (make-repo-tuning-path name host version release))
		 (sp (make-file-name (bglpkg-pkg-directory) sb)))
	     ;; patch the path
	     (if (and (pair? c) (pair? (cdr c)))
		 (set-car! (cdr c) sp)
		 (set! rest (cons* :path sp rest)))
	     ;; remove the former tuning
	     (let ((pv (sqlite-map db
			  (lambda (name version host)
			     (list name version host))
			  "SELECT p.name, p.version, host
                           FROM tuning t, package p
                           WHERE (t.release <= '~a') AND
                                 (p.name = ~q) AND
                                 (p.version <= ~q) AND
                                 (p.id = t.package)"
			  release name version)))
		(for-each (lambda (pv)
			     (apply db-remove-tuning! db pv))
			  pv))
	     ;; register the tuning
	     (apply db-add-tuning! db name rest)))
	 (else
	  (warning 'bglpkg-inject-description "Illegal description -- " x))))
   (for-each inject! lst))

;*---------------------------------------------------------------------*/
;*    sync-url ...                                                     */
;*---------------------------------------------------------------------*/
(define (sync-url db url)
   (let ((p (open-input-file url)))
      (unless (input-port? p)
	 (raise (instantiate::&io-port-error
		   (proc 'sync-url)
		   (msg "Cannot open url")
		   (obj url))))
      (unwind-protect
	 (let loop ()
	    (let ((e (read-line p)))
	       (cond
		  ((eof-object? e)
		   (raise (instantiate::&io-read-error
			     (proc 'sync-url)
			     (msg "Cannot find sync list")
			     (obj url))))
		  ((string=? e "")
		   (let ((e (read p)))
		      (unless (list? e)
			 (let ((rest (read-line p)))
			    (raise (instantiate::&io-parse-error
				      (proc 'sync-url)
				      (msg (format "Illegal sync list: ~a~a"
						   e rest))
				      (obj url)))))
		      (bglpkg-inject-description-list db e)))
		  (else
		   (loop)))))
	 (close-input-port p))))

;*---------------------------------------------------------------------*/
;*    bglpkg-sync ...                                                  */
;*    -------------------------------------------------------------    */
;*    Sync with the description of a remote server                     */
;*---------------------------------------------------------------------*/
(define (bglpkg-sync db)
   (for-each (lambda (u) (sync-url db u)) (bglpkg-sync-urls)))

;*---------------------------------------------------------------------*/
;*    bglpkg-wizard ...                                                */
;*---------------------------------------------------------------------*/
(define (bglpkg-wizard name arguments)
   (let* ((version "0.0.1")
	  (destdir (cond
		      ((and (string=? (bglpkg-destdir) ".")
			    (string=? (basename (pwd)) name))
		       (bglpkg-destdir))
		      ((string=? (basename (bglpkg-destdir)) name)
		       (bglpkg-destdir))
		      (else
		       (make-file-name (bglpkg-destdir) name)))))
      (when (pair? arguments)
	 (if (package-version? (car arguments))
	     (begin
		(set! version (car arguments))
		(when (pair? (cdr arguments))
		   (warning 'bglpkg-wizard
			    "Ignored arguments -- "
			    (cdr arguments))))
	     (warning 'bglpkg-wizard
		      "Ignored arguments -- "
		      arguments)))
      (unless (or (directory? destdir) (make-directories destdir))
	 (error 'bglpkg-wizard "Cannot create directory" destdir))
      ;; etc
      (let* ((etc (make-file-name destdir "etc"))
	     (meta (make-file-name etc "meta")))
	 (unless (or (directory? etc) (make-directory etc))
	    (error 'bglpkg-wizard "Cannot create directory" etc))
	 (when (or (not (file-exists? meta)) (bglpkg-force-action))
	    (with-output-to-file meta
	       (lambda ()
		  (display "(")
		  (print '(maintainer "\"\""))
		  (display " ") (write `(version ,version)) (newline)
		  (print " " '(author "\"\""))
		  (print " " '(description "\"\""))
		  (print " " '(keywords misc))
		  (print " " '(license lgpl-v2.1) ")")))))
      ;; package
      (let* ((src (string-append name ".scm"))
	     (file (find-file/path src (bglpkg-search-path)))
	     (imports '())
	     (exports '()))
	 (when (string? file)
	    (let ((dest (make-file-name destdir src)))
	       (unless (string=? (file-name-canonicalize file)
				 (file-name-canonicalize dest))
		  (copy-file file dest)))
	    (multiple-value-bind (imps exps)
	       (bigloo-parse-module file)
	       (set! exports exps)
	       (set! imports imps)))
	 ;; interface
	 (generate-spi destdir name imports exports)
	 ;; test
	 (generate-test destdir name)
	 ;; doc
	 (generate-doc destdir name exports))))

;*---------------------------------------------------------------------*/
;*    dsssl->srfi89 ...                                                */
;*---------------------------------------------------------------------*/
(define (dsssl->srfi89 fun args)
   (let loop ((args args)
	      (state 'plain))
      (cond
	 ((not (pair? args))
	  args)
	 ((symbol? (car args))
	  (let ((narg (case state
			 ((plain)
			  (untype-ident (car args)))
			 ((optional)
			  (list (untype-ident (car args)) #f))
			 (else
			  (let ((a (untype-ident (car args))))
			     (list (symbol->keyword a) a #f))))))
	     (cons narg (loop (cdr args) state))))
	 ((eq? (car args) #!optional)
	  (loop (cdr args) 'optional))
	 ((eq? (car args) #!key)
	  (loop (cdr args) 'key))
	 ((eq? (car args) #!rest)
	  (loop (cdr args) 'rest))
	 ((eq? state 'optional)
	  (let ((a (untype-ident (caar args)))
		(v (cadar args)))
	     (cons (list a v)
		   (loop (cdr args) state))))
	 ((eq? state 'key)
	  (let ((a (untype-ident (caar args)))
		(v (cadar args)))
	     (cons (list (symbol->keyword a) a v)
		   (loop (cdr args) state))))
	 (else
	  (error 'dsssl->srfi89 "Illegal formal parameter" fun)))))
	 
;*---------------------------------------------------------------------*/
;*    untype-ident ...                                                 */
;*---------------------------------------------------------------------*/
(define (untype-ident id)
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
		 (if (=fx walker 0)
		     'o
		     (string->symbol (substring string 0 walker))))
		(else
		 (loop (+fx walker 1))))))))

;*---------------------------------------------------------------------*/
;*    generate-spi ...                                                 */
;*---------------------------------------------------------------------*/
(define (generate-spi destdir name imports exports)

   (define (export-proto export)
      (match-case export
	 ((macro ?fun . ?args)
	  `(macro (,(untype-ident fun) ,@args)))
	 ((syntax ?fun)
	  `(syntax ,fun))
	 ((class ?id . ?fields)
	  `(class ,id ,@(map (lambda (f)
				(if (symbol? f)
				    (untype-ident f)
				    (car f)))
			     fields)))
	 ((inline ?fun . ?args)
	  `(,(untype-ident fun) ,@(dsssl->srfi89 fun args)))
	 ((?fun . ?args)
	  `(,(untype-ident fun) ,@(dsssl->srfi89 fun args)))
	 (else
	  export)))
   
   (define header-line ";; generated file (bglpkg), don't edit!")
   
   (define (generated-spi-file? pkg)
      (with-input-from-file pkg
	 (lambda ()
	    (equal? (read-line) header-line))))
   
   (let* ((spi (string-append name ".spi"))
	  (pkg (make-file-name destdir spi)))
      (when (or (not (file-exists? pkg))
		(bglpkg-force-action)
		(generated-spi-file? pkg))
	 (with-output-to-file pkg
	    (lambda ()
	       (print header-line)
	       (print ";; " (current-date))
	       (print "(interface " name " ")
	       (print "   (language bigloo)")
	       (print "   (bigloo (module-override))")
	       (when (pair? imports)
		  (print "   " `(import ,@imports)))
	       (if (pair? exports)
		   (begin
		      (display "   ")
		      (write `(export ,@(map export-proto exports)))
		      (print ")"))
		   (print "   )")))))))

;*---------------------------------------------------------------------*/
;*    generate-test ...                                                */
;*---------------------------------------------------------------------*/
(define (generate-test destdir name)
   (let* ((test (make-file-name destdir "test"))
	  (spi (make-file-name test (string-append name "-test.spi")))
	  (src (make-file-name test (string-append name "-test.scm"))))
      (unless (or (directory? test) (make-directory test))
	 (error 'bglpkg-wizard "Cannot create directory" test))
      (when (or (not (file-exists? spi)) (bglpkg-force-action))
	 (with-output-to-file spi
	    (lambda ()
	       (print "(interface " name "-test")
	       (print "   (import recette " name ")")
	       (print "   (export (run tests)))"))))
      (when (or (not (file-exists? src)) (bglpkg-force-action))
	 (with-output-to-file src
	    (lambda ()
	       (print "(define (run names)")
	       (print "   (apply run-tests names))"))))))

;*---------------------------------------------------------------------*/
;*    generate-doc ...                                                 */
;*---------------------------------------------------------------------*/
(define (generate-doc destdir name exports)
   (define (proto args)
      (with-output-to-string
	 (lambda ()
	    (let loop ((args args))
	       (cond
		  ((symbol? args)
		   (display* " . " (untype-ident args)))
		  ((pair? args)
		   (cond
		      ((symbol? (car args))
		       (display* " " (untype-ident (car args))))
		      ((pair? (car args))
		       (display " ")
		       (write (cons (untype-ident (caar args)) (cdar args))))
		      (else
		       (display* " " (car args))))
		   (loop (cdr args))))))))
   (let* ((doc (make-file-name destdir "doc"))
	  (src (make-file-name doc (string-append name ".wiki"))))
      (unless (or (directory? doc) (make-directory doc))
	 (error 'bglpkg-wizard "Cannot create directory" doc))
      (when (or (not (file-exists? src)) (bglpkg-force-action))
	 (with-output-to-file src
	    (lambda ()
	       (print "<doc> " name)
	       (newline)
	       (print "== Description ==")
	       (newline)
	       (print "== Index ==")
	       (print ",(<WIKI-INDEX>)")
	       (newline)
	       (print "== Definitions ==")
	       (for-each (lambda (export)
			    (match-case export
			       ((macro ?fun . ?args)
				(print "<syntax> " fun (proto args))
				(print "</syntax>"))
			       ((syntax ?fun)
				(print "<syntax> " fun "\n</syntax>\n"))
			       ((inline ?fun . ?args)
				(print "<procedure> "
				       (untype-ident fun)
				       (proto args))
				(print "</procedure>\n"))
			       ((class ?id . ?fields)
				(print "<class> " id)
				(print "</class>\n"))
			       ((?fun . ?args)
				(print "<procedure> "
				       (untype-ident fun)
				       (proto args))
				(print "</procedure>\n"))
			       (else
				(print "<variable> " export)
				(print "</variable>\n"))))
			 exports)
	       (print "</doc>"))))))

;*---------------------------------------------------------------------*/
;*    bigloo-parse-module ...                                          */
;*---------------------------------------------------------------------*/
(define (bigloo-parse-module file)
   (with-input-from-file file
      (lambda ()
	 (match-case (read)
	    ((module ?- . ?args)
	     (values (assq* 'import args '())
		     (assq* 'export args '())))
	    (else
	     (values '() '()))))))
	     
;*---------------------------------------------------------------------*/
;*    bglpkg-query ...                                                 */
;*---------------------------------------------------------------------*/
(define (bglpkg-query db arguments)
   (define (print-proto pkg proto color)
      (display* pkg ": ")
      (match-case (with-input-from-string proto read)
	 ((macro (?fun . ?rest))
	  (display "(macro ")
	  (display (cons (pkglib-color color fun) rest))
	  (display ")"))
	 ((?fun . ?rest)
	  (display (cons (pkglib-color color fun) rest)))
	 (else
	  (display (pkglib-color color proto))))
      (newline))
   (if (null? arguments)
       (for-each (lambda (c)
		    (sqlite-map db
		       (lambda (pkg proto)
			  (print-proto pkg proto c))
		       "SELECT package, proto
                        FROM ~a"
		       c))
		 '(function variable macro record exception))
       (for-each (lambda (a)
		    (for-each (lambda (c)
				 (sqlite-map db
				    (lambda (pkg proto)
				       (print-proto pkg proto c))
				    "SELECT package, proto
                                     FROM ~a
                                     WHERE (name regexp ~q)" c a))
			      '(function variable macro record exception)))
		 arguments)))

;*---------------------------------------------------------------------*/
;*    bglpkg-with-package ...                                          */
;*---------------------------------------------------------------------*/
(define (bglpkg-with-package db package proc)
   (when (>fx (pkglib-verbose) 0) (print "with packages:"))
   (multiple-value-bind (name version release _)
      (package-name-parse package)
      (let ((old (sqlite-eval db
		    (lambda (x) x)
		    "SELECT id FROM package
                              WHERE (name=~q) and (version=~q)"
		    name version)))
	 (if (and old (not (bglpkg-force-action)))
	     (error 'bglpkg "Cannot override existing package" version)
	     (unwind-protect
		(begin
		   (when old (bglpkg-remove-packages db (list package)))
		   (bglpkg-add-packages db (list package))
		   (proc db name version))
		(bglpkg-remove-packages db (list name version)))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-with-tuning ...                                           */
;*    -------------------------------------------------------------    */
;*    Similar to BGLPKG-WITH-PACKAGE but in addition use a tunnig.     */
;*---------------------------------------------------------------------*/
(define (bglpkg-with-tuning db package tuning proc)
   (when (>fx (pkglib-verbose) 0) (print "with tunings:"))
   (multiple-value-bind (name version release _)
      (package-name-parse package)
      (let ((old (sqlite-eval db
		    (lambda (x) x)
		    "SELECT id FROM package
                              WHERE (name=~q) and (version=~q)"
		    name version)))
	 (if old
	     (error 'bglpkg "Cannot override existing package" version)
	     (unwind-protect
		(begin
		   (bglpkg-add-packages db (list package tuning))
		   (proc db name version))
		(bglpkg-remove-packages db (list name version)))))))

;*---------------------------------------------------------------------*/
;*    cleanup-directories! ...                                         */
;*    -------------------------------------------------------------    */
;*    Remove the empty directories                                     */
;*---------------------------------------------------------------------*/
(define (cleanup-directories!)
   (let loop ((dir (bglpkg-pkg-directory))
	      (lst (directory->list (bglpkg-pkg-directory))))
      (for-each (lambda (f)
		   (let ((p (make-file-name dir f)))
		      (when (directory? p)
			 (let ((lst (directory->list p)))
			    (if (null? lst)
				(delete-directory p)
				(loop p lst))))))
		lst)))

;*---------------------------------------------------------------------*/
;*    collect-cleanup-pkg-files ...                                    */
;*---------------------------------------------------------------------*/
(define (collect-cleanup-pkg-files db)
   (let ((table (make-hashtable)))
      (sqlite-map db
	 (lambda (path) (hashtable-put! table path #t))
	 "SELECT path FROM package, tuning")
      (let loop ((dir (bglpkg-pkg-directory))
		 (res '()))
	 (let ((files (directory->list dir)))
	    (let liip ((lst (map! (lambda (f) (make-file-name dir f)) files))
		       (res res))
	       (cond
		  ((null? lst)
		   res)
		  ((directory? (car lst))
		   (loop (car lst) res))
		  ((not (package-filename? (car lst)))
		   (liip (cdr lst) (cons (car lst) res)))
		  ((not (hashtable-get table (car lst)))
		   (liip (cdr lst) (cons (car lst) res)))
		  (else
		   (liip (cdr lst) res))))))))
      
;*---------------------------------------------------------------------*/
;*    collect-cleanup-files ...                                        */
;*---------------------------------------------------------------------*/
(define (collect-cleanup-files db)
   (append
    ;; tmp files
    (map (lambda (f)
	    (make-file-name (bglpkg-tmp-directory) f))
	 (directory->list (bglpkg-tmp-directory)))
    ;; cache files
    (map (lambda (f)
	    (make-file-name (bglpkg-cache-directory) f))
	 (directory->list (bglpkg-cache-directory)))
    ;; pkg files
    (collect-cleanup-pkg-files db)))

;*---------------------------------------------------------------------*/
;*    bglpkg-cleanup ...                                               */
;*---------------------------------------------------------------------*/
(define (bglpkg-cleanup db arguments)
   (let ((files (collect-cleanup-files db)))
      (when (yes-or-no? "Delete files? [y/n] " files)
	 (for-each delete-file files)))
   (cleanup-directories!))

;*---------------------------------------------------------------------*/
;*    bglpkg-clear ...                                                 */
;*---------------------------------------------------------------------*/
(define (bglpkg-clear db argument)
   (when (yes-or-no? "Clear all cache? [y/n] " #f)
      (rm-rf (bglpkg-pkg-directory))
      (rm-rf (bglpkg-cache-directory))
      #t))
