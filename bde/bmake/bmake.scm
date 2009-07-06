;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmake/bmake.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 29 07:25:21 1998                          */
;*    Last change :  Thu Aug  3 11:41:41 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The tools that update (or build) Makefiles.                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmake
   (import  (bmake_template "bmake/template.scm"))
   (eval    (export *source-suffixes*)
	    (export *target-name*)
	    (export *project-name*)
	    (export *template-name*)
	    (export *makefile-name*)
	    (export *mco?*)
	    (export *library?*)
	    (export *verbose*)
	    (export *make*))
   (main    main))

;*---------------------------------------------------------------------*/
;*    Global user parameters ...                                       */
;*---------------------------------------------------------------------*/
(define *bmake-version*          "0.3")
(define *search-path*            '("."))
(define *exclude-path*           '())

(define *object-entry*           '())
(define *source-entry*           '())
(define *heap-entry*             #f)
(define *heap-source?*           #f)
(define *main-entry*             #f)

(define *source-suffixes*        '("scm" "sch" "bgl"))
(define *gui-suffix*             "bld")

(define *mode*                   #f)
(define *project*                #f)

(define *load-rc?*               #t)

;; rc parameters
(define *makefile-name*          #f)
(define *template-name*          'application)
(define *project-name*           (prefix (basename (pwd))))
(define *target-name*            #f)
(define *mco?*                   #t)
(define *library?*               #f)
(define *verbose*                #f)
(define *make*                   "make")

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (define (usage args-parse-usage level)
      (print "usage: bglmake [options] [file]")
      (newline)
      (args-parse-usage #f)
      (newline)
      (exit 0))
   
   (args-parse (cdr cmd-args)
      
      ;; miscellaneous
      (section "Misc")
      (("?")
       (usage args-parse-usage 1))
      (("-help" (help "This help message"))
       (usage args-parse-usage 1))
      (("-v" (help "Be verbose"))
       (set! *verbose* #t))
      (("-q" (help "Do not load the ~/.bmakerc file"))
       (set! *load-rc?* #f))
      (("-suffix" ?suf (help "Add source suffixes [default: scm sch bgl]"))
       (set! *source-suffixes* (cons suf *source-suffixes*)))
      (("-gui-suffix" ?suf (help "Set GUI suffixe [default: bld]"))
       (set! *gui-suffix* suf))
      (("-mco" (help "-[no_]mco" "Enable [disable] mco rules production"))
       (set! *mco?* #t))
      (("-no_mco")
       (set! *mco?* #f))
      
      ;; source search path
      (section "Search path")
      (("-I?dir" (help "Add DIR to the search path [default: .]"))
       (if (directory? dir)
	   (set! *search-path* (cons dir *search-path*))
	   (warning "bglmake" "Can't find directory -- " dir)))
      (("-I" ?dir)
       (if (directory? dir)
	   (set! *search-path* (cons dir *search-path*))
	   (warning "bglmake" "Can't find directory -- " dir)))
      (("-X?dir" (help "Prevent the source search process to enter DIR"))
       (set! *exclude-path* (cons dir *exclude-path*)))
      (("-X" ?dir)
       (set! *exclude-path* (cons dir *exclude-path*)))
      
      ;; target file and project name
      (section "Makefile, Target and Project name")
      (("-o" ?name (help "Set makefile target name"))
       (set! *makefile-name* name))
      (("-p" ?name (help "Set the project name"))
       (set! *project-name* name))
      (("-t" ?name (help "Set the target name"))
       (set! *target-name* name))
      
      ;; project mode
      (section "Development mode")
      (("-debug" (help "Set the Makefile in debugging (high debug) mode"))
       (set! *mode* "debug"))
      (("-devel" (help "Set the Makefile in development (debug) mode"))
       (set! *mode* "devel"))
      (("-final" (help "Set the Makefile in final (optim) mode"))
       (set! *mode* "final"))

      ;; template Makefile
      (section "Template")
      (("-f?tpl" (help "-f<template>" "The template for Makefile creation"))
       (cond
	  ((string=? tpl "application")
	   (set! *template-name* 'application))
	  ((string=? tpl "library")
	   (set! *library?* #t)
	   (set! *mco?* #f)
	   (set! *template-name* 'library))
	  (else
	   (if (not (file-exists? tpl))
	       (error "bglmake" "Can't find Makefile template" tpl)
	       (set! *template-name* tpl)))))
      (("-library" (help "Generates a library makefile"))
       (set! *library?* #t))
      
      ;; sources
      (section "Project, Sources and Objects")
      (("-project" ?name (help "Set project name"))
       (set! *project* name))
      (("-object" ?obj (help "Add an entry to the object list"))
       (set! *object-entry* (cons obj *object-entry*)))
      (("-source" ?src (help "Add an entry to the source list"))
       (set! *source-entry* (cons src *source-entry*)))
      (("-main" ?entry (help "Set the main entry point"))
       (if (not (string? *target-name*))
	   (set! *target-name* (prefix (basename entry))))
       (if (string? *main-entry*)
	   (error "bglmake"
		  "Only one source file (the main) must be provided"
		  entry)
	   (set! *main-entry* entry)))
      (("-heap" ?entry (help "Set the heap entry point"))
       (if (string? *heap-entry*)
	   (error "bglmake"
		  "Only one source file (the heap) must be provided"
		  entry)
	   (set! *heap-entry* entry)))
      (("-heap-source" (help "Insert the heap file in the source file list."))
       (set! *heap-source?* #t))

      (else
       (if (not (string? *target-name*))
	   (set! *target-name* (prefix (basename else))))
       (if (string? *main-entry*)
	   (error "bglmake"
		  "Only one source file (the main) must be provided"
		  else)
	   (set! *main-entry* else))))

   ;; we are done with argument parsing we just perform some extra
   ;; configuration setup
   ;; we load the .bmakerc file (unless disable with -q option)
   (if *load-rc?*
       (let ((home (getenv "HOME")))
	  (if (string? home)
	      (let ((fname (string-append home "/.bglmakerc")))
		 (if (file-exists? fname)
		     (loadq fname)
		     (let ((fname (string-append home "/.bmakerc")))
			(if (file-exists? fname)
			    (loadq fname))))))))

   (if (not (string? *target-name*))
       (set! *target-name* (prefix (basename (pwd))))))
   
;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; we parse command line arguments
   (parse-args argv)
   ;; we now may enter the true processing
   (engine))

;*---------------------------------------------------------------------*/
;*    with-makefile-output ...                                         */
;*---------------------------------------------------------------------*/
(define-macro (with-makefile-output expr)
   `(if (string? *makefile-name*)
	(with-output-to-file *makefile-name*
	   (lambda () ,expr))
	(with-output-to-port (current-output-port)
	   (lambda () ,expr))))

;*---------------------------------------------------------------------*/
;*    engine ...                                                       */
;*---------------------------------------------------------------------*/
(define (engine) 
   (cond
      ((and (string? *main-entry*) (string? *heap-entry*))
       (error "bglmake" "You can't set both main and heap entry" *main-entry*))
      ((string? *mode*)
       ;; this is just a Makefile mode changes
       (if (not (string? *makefile-name*))
	   (error "bglmake" "You must specify a Makefile name (see -o opt)" #f)
	   (update-makefile-mode *mode*)))
      ((string? *project*)
       ;; this is just a Makefile project name changes
       (if (not (string? *makefile-name*))
	   (error "bglmake" "You must specify a Makefile name (see -o opt)" #f)
	   (update-makefile-project-name *project*)))
      ((and (not (string? *main-entry*))
	    (not (string? *heap-entry*))
	    (null? *source-entry*)
	    (null? *object-entry*))
       ;; this is just a simple template dump
       (let ((port (open-input-template)))
	  (if (not (input-port? port))
	      (error "bglmake" "Can't open template" *template-name*)
	      (unwind-protect
		 (with-makefile-output (dump-template port))
		 (close-input-port port)))))
      ((and (not (string? *main-entry*)) (not (string? *heap-entry*)))
       ;; this is just an addition of some entries
       (let* ((old-objects (get-makefile-list "objects"))
	      (old-sources (get-makefile-list "sources")))
	  (update-makefile
	   (object-sort
	    (uniquify! (append *object-entry* old-objects)))
	   (uniquify! (append *source-entry* old-sources)))))
      ((and (string? *makefile-name*) (file-exists? *makefile-name*))
       ;; this is an udpate because the Makefile already exists
       ;; we start searching all the Bigloo source files
       (find-directory-sources)
       ;; we fetch old Makefile fields
       (let* ((old-objects (get-makefile-list "objects"))
	      (old-source (get-makefile-list "sources"))
	      (bigloo-sources (find-bigloo-entries)))
	  (update-makefile
	   (object-sort
	    (uniquify! (append *object-entry*
			       (bigloo-entries->bigloo-object bigloo-sources)
			       old-objects)))
	   (uniquify! (append *source-entry* bigloo-sources old-source)))))
      (else
       ;; this is a pure Makefile creation
       ;; we start searching all the Bigloo source files
       (find-directory-sources)
       (let ((bigloo-sources (find-bigloo-entries)))
	  (with-makefile-output
	   (create-makefile
	    *project-name*
	    *target-name*
	    (object-sort
	     (uniquify!
	      (append *object-entry*
		      (bigloo-entries->bigloo-object bigloo-sources))))
	    (uniquify! (append *source-entry* bigloo-sources))))))))

;*---------------------------------------------------------------------*/
;*    open-input-template ...                                          */
;*---------------------------------------------------------------------*/
(define (open-input-template)
   (case *template-name*
      ((application)
       (open-input-string *application-template*))
      ((library)
       (open-input-string *library-template*))
      (else
       (open-input-file *template-name*))))

;*---------------------------------------------------------------------*/
;*    dump-template ...                                                */
;*---------------------------------------------------------------------*/
(define (dump-template port)
   (let loop ((line (read-line port)))
      (if (not (eof-object? line))
	  (begin
	     (print line)
	     (loop (read-line port))))))

;*---------------------------------------------------------------------*/
;*    get-makefile-list ...                                            */
;*---------------------------------------------------------------------*/
(define (get-makefile-list field)
   (let* ((cmd  (string-append "| make -s get" field))
	  (port (open-input-file cmd)))
      (let loop ((exp (read-of-strings port))
		 (lst '()))
	 (if (eof-object? exp)
	     (begin
		(close-input-port port)
		lst)
	     (loop (read-of-strings port)
		   (cons exp lst))))))
      
;*---------------------------------------------------------------------*/
;*    bigloo-source? ...                                               */
;*---------------------------------------------------------------------*/
(define (bigloo-source? fname)
   (member (suffix fname) *source-suffixes*))

;*---------------------------------------------------------------------*/
;*    gui-source? ...                                                  */
;*---------------------------------------------------------------------*/
(define (gui-source? fname)
   (string=? (suffix fname) *gui-suffix*))

;*---------------------------------------------------------------------*/
;*    read-module-name ...                                             */
;*    -------------------------------------------------------------    */
;*    Read from a Bigloo source file the module name.                  */
;*---------------------------------------------------------------------*/
(define (read-module-name fname) 
   (let ((iport (open-input-file fname)))
      (if (input-port? iport)
	  (unwind-protect
	     (try (let ((module (read iport)))
		     (match-case module
			((module ?name . ?-)
			 name)
			(else
			 #f)))
		  (lambda (escape obj proc msg)
		     (warning "bglmake:Can't read file" fname)
		     (escape #f)))
	     (close-input-port iport))
	  (error "bglmake" "Can't open file for input" fname))))

;*---------------------------------------------------------------------*/
;*    read-gui-module-name ...                                         */
;*---------------------------------------------------------------------*/
(define (read-gui-module-name fname)
   (with-input-from-file fname
      (lambda ()
	 (read))))
	     
;*---------------------------------------------------------------------*/
;*    find-directory-sources ...                                       */
;*    -------------------------------------------------------------    */
;*    This function walks trought the directory structure in order     */
;*    to find all Bigloo sources.                                      */
;*---------------------------------------------------------------------*/
(define (find-directory-sources)
   (if *verbose*
       (fprint (current-error-port) "Scanning for source files..."))
   (define (find-all-bigloo-sources/basename basename files)
      (for-each (lambda (file)
		   (let ((fname (string-append basename file)))
		      (if (file-exists? fname)
			  (cond
			     ((directory? fname)
			      (if (not (member fname *exclude-path*))
				  (find-all-bigloo-sources/basename
				   (string-append fname "/")
				   (directory->list fname))))
			     ((gui-source? fname)
			      (let ((module (read-gui-module-name fname)))
				 (if (symbol? module)
				     (putprop! module 'source fname))))
			     ((bigloo-source? fname)
			      (let ((module (read-module-name fname)))
				 (if (symbol? module)
				     (putprop! module 'source fname))))))))
		files))
   (for-each (lambda (dir)
		(find-all-bigloo-sources/basename (string-append dir "/")
						  (directory->list dir)))
	     *search-path*))

;*---------------------------------------------------------------------*/
;*    find-bigloo-entries ...                                          */
;*    -------------------------------------------------------------    */
;*    Make a transitive closure for Bigloo source files. this function */
;*    returns the list of Bigloo source files.                         */
;*---------------------------------------------------------------------*/
(define (find-bigloo-entries)
   (if *verbose*
       (fprint (current-error-port) "Searching for Bigloo source files..."))
   ;; we put each source file inside an hash table. this function makes
   ;; the transitive closure of Bigloo importations. that is, this function
   ;; open bigloo source files to fetch the importation clauses.
   (if (string? *main-entry*)
       ;; when producing an application makefile we add the main to the
       ;; object list
       (add-one-source! *main-entry*)
       ;; when producing a library makefile we do not add the heap file
       ;; to the object list unless one module imports it.
       (add-imported-modules! *heap-entry*))
   (let ((sources '()))
      (hashtable-for-each *source-env*
			  (lambda (key src)
			     (set! sources (cons key sources))))
      (cond
	 ((and *heap-source?*
	       (string? *heap-entry*)
	       (not (member *heap-entry* sources)))
	  (cons *heap-entry* sources))
	 ((and (not *heap-source?*)
	       (member *heap-entry* sources))
	  (delete *heap-entry* sources))
	 (else
	  sources))))
   
;*---------------------------------------------------------------------*/
;*    *source-env* ...                                                 */
;*---------------------------------------------------------------------*/
(define *source-env* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    add-one-source! ...                                              */
;*---------------------------------------------------------------------*/
(define (add-one-source! source)
   ;; we have to removing the head `./' otherwise etags.el will be
   ;; confused when searching for source files.
   (let ((source (if (substring=? source "./" 2)
		     (substring source 2 (string-length source))
		     source)))
      (if (not (hashtable-get *source-env* source))
	  (begin
	     (hashtable-put! *source-env* source #t)
	     (if *verbose*
		 (fprint (current-error-port) source ":"))
	     (add-imported-modules! source)))))

;*---------------------------------------------------------------------*/
;*    add-imported-modules! ...                                        */
;*---------------------------------------------------------------------*/
(define (add-imported-modules! fname)
   (if (file-exists? fname)
       (let ((port (open-input-file fname)))
	  (if (not (input-port? port))
	      (error "bglmake" "Can't open file for input" fname)
	      (unwind-protect
		 (try (if (not (gui-source? fname))
			  (let ((module (read port)))
			     (match-case module
				((module (? symbol?) . ?clauses)
				 (add-imported-modules/clauses! clauses))
				((directives . ?clauses)
				 (add-imported-modules/clauses! clauses)
				 clauses)))
			  (let* ((name (read port))
				 (simports (read port))
				 (imports (map (lambda (x)
						  (if (string? x)
						      (string->symbol
						       (string-upcase! x))
						      x))
					       simports)))
			     (add-imported-modules/clauses!
			      (list `(import ,@imports)))))
		      (lambda (escape obj proc msg)
			 (escape #f)))
		 (close-input-port port))))
       (warning "bglmake" "Can't find file -- " fname)))

;*---------------------------------------------------------------------*/
;*    add-imported-modules/clauses! ...                                */
;*---------------------------------------------------------------------*/
(define (add-imported-modules/clauses! clauses)
   (define (add-imported-modules/import! import)
      (match-case import
	 (((and ?module (? symbol?)) (and ?fname (? string?)) . ?rest)
	  ;; (module-name "file-name" ...)
	  (add-one-source! fname))
	 (((and ?var (? symbol?)) (and ?module (? symbol?)))
	  ;; (variable module-name)
	  (let ((source (getprop module 'source)))
	     (if (string? source)
		 (add-one-source! source))))
	 (((? symbol?) (? symbol?) (and ?fname (? string?)) . ?rest)
	  (add-one-source! fname))
	 ((and ?module (? symbol?))
	  ;; module-name
	  (let ((source (getprop module 'source)))
	     (if (string? source)
		 (add-one-source! source))))))
   (define (add-imported-modules/clause! clause)
      (match-case clause
	 (((or use import) . ?imports)
	  (for-each add-imported-modules/import! imports))
	 ((include . ?fnames)
	  (for-each add-imported-modules! fnames))))
   (for-each add-imported-modules/clause! clauses))

;*---------------------------------------------------------------------*/
;*    uniquify! ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function removes multiple occurrences of the string         */
;*    members of LST.                                                  */
;*---------------------------------------------------------------------*/
(define (uniquify! lst)
   (let ((mark (gensym)))
      ;; we mark the strings
      (for-each (lambda (str) (putprop! (string->symbol str) mark #t)) lst)
      ;; we collect them
      (let loop ((lst lst)
		 (res '()))
	 (if (null? lst)
	     res
	     (let ((sym (string->symbol (car lst))))
		(if (getprop sym mark)
		    (begin
		       (remprop! sym mark)
		       (loop (cdr lst)
			     (cons (car lst) res)))
		    (loop (cdr lst) res)))))))

;*---------------------------------------------------------------------*/
;*    bigloo-entries->bigloo-object ...                                */
;*    -------------------------------------------------------------    */
;*    From a source file list this function builds an object file      */
;*    list.                                                            */
;*    -------------------------------------------------------------    */
;*    For each file that is a GUI source file, we add the Bigloo       */
;*    module as a target. This target are inserted in the head of      */
;*    the object list in order to enforce GUI file compilation         */
;*    before Bigloo compilation.                                       */
;*---------------------------------------------------------------------*/
(define (bigloo-entries->bigloo-object sources)
   (if (not *mco?*)
       (map (lambda (src)
	       (string-append (prefix src) ".o"))
	    sources)
       (let loop ((sources sources)
		  (res     '()))
	  (if (null? sources)
	      res
	      (let* ((src  (car sources))
		     (base (prefix src)))
		 (loop (cdr sources)
		       (cons (string-append base ".mco")
			     (cons (string-append base ".o")
				   res))))))))

;*---------------------------------------------------------------------*/
;*    sources->gui-objects ...                                         */
;*    -------------------------------------------------------------    */
;*    This function scans the sources in order to find which one       */
;*    are GUI sources.                                                 */
;*---------------------------------------------------------------------*/
(define (sources->gui-objects sources)
   (let loop ((src sources)
	      (res '()))
      (cond
	 ((null? src)
	  res)
	 ((source->gui (car src))
	  (loop (cdr src)
		(cons (string-append (prefix (car src)) ".scm") res)))
	 (else
	  (loop (cdr src)
		res)))))
   
;*---------------------------------------------------------------------*/
;*    object-sort ...                                                  */
;*    -------------------------------------------------------------    */
;*    If mco is enabled we sort the object list for mco object to      */
;*    be first.                                                        */
;*---------------------------------------------------------------------*/
(define (object-sort objects) 
   (if (not *mco?*)
       objects
       (let loop ((objects objects)
		  (mco     '())
		  (o       '()))
	  (if (null? objects)
	      (append mco o)
	      (let ((suf (suffix (car objects))))
		 (cond
		    ((string=? suf "mco")
		     (loop (cdr objects)
			   (cons (car objects) mco)
			   o))
		    (else
		     (loop (cdr objects)
			   mco
			   (cons (car objects) o)))))))))

;*---------------------------------------------------------------------*/
;*    object->objects_p ...                                            */
;*    -------------------------------------------------------------    */
;*    Builds the list of the profile objects.                          */
;*---------------------------------------------------------------------*/
(define (objects->objects_p objects)
   (map (lambda (obj)
	   (if (string=? (suffix obj) "mco")
	       (string-append (prefix obj) "_p.mco")
	       (string-append (prefix obj) "_p.o")))
	objects))

;*---------------------------------------------------------------------*/
;*    objects->classes ...                                             */
;*    -------------------------------------------------------------    */
;*    Builds the list of the JVM objects.                              */
;*---------------------------------------------------------------------*/
(define (objects->classes objects)
   (map (lambda (obj)
	   (if (string=? (suffix obj) "mco")
	       obj
	       (string-append (prefix obj) ".class")))
	objects))

;*---------------------------------------------------------------------*/
;*    sources->classes ...                                             */
;*---------------------------------------------------------------------*/
(define (sources->classes sources)
   (define (untype-ident id)
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
		(loop (+fx walker 1)))))))
   (define (find-classes::pair-nil clauses)
      (let loop ((clauses clauses)
		 (classes '()))
	 (if (null? clauses)
	     classes
	     (match-case (car clauses)
		(((or export static) . ?statexp)
		 (let liip ((statexp statexp)
			    (classes classes))
		    (if (null? statexp)
			(loop (cdr clauses)
			      classes)
			(match-case (car statexp)
			   (((or class abstract-class final-class wide-class)
			     ?ident . ?-)
			    (let* ((id (untype-ident ident))
				   (sid (symbol->string id)))
			       (if (bigloo-need-mangling? sid)
				   (liip (cdr statexp)
					 (cons (string-append
						(bigloo-mangle sid)
						".class")
					       classes))
				   (liip (cdr statexp)
					 (cons (string-append
						sid
						".class")
					       classes)))))
			   (else
			    (liip (cdr statexp)
				  classes))))))
		(else
		 (loop (cdr clauses) classes))))))
   (define (source->classes::pair-nil source)
      (if (and (not (string=? (suffix source) "mco"))
	       (file-exists? source))
	  (with-input-from-file source
	     (lambda ()
		(try (match-case (read)
			((module ?- . ?clauses)
			 (find-classes clauses))
			(else
			 '()))
		     (lambda (escape obj proc msg)
			(escape '())))))
	  '()))
   (let loop ((sources sources)
	      (classes '()))
      (if (null? sources)
	  classes
	  (loop (cdr sources)
		(append (source->classes (car sources)) classes)))))

;*---------------------------------------------------------------------*/
;*    sources->sources_c ...                                           */
;*    -------------------------------------------------------------    */
;*    Builds the list of C source files.                               */
;*---------------------------------------------------------------------*/
(define (sources->sources_c sources)
   (let loop ((sources sources)
	      (res     '()))
      (cond
	 ((null? sources)
	  (reverse! res))
	 ((let ((suf (suffix (car sources))))
	     (or (member suf *source-suffixes*)
		 (string=? suf *gui-suffix*)))
	  (loop (cdr sources)
		(cons (string-append (prefix (car sources)) ".c") res)))
	 (else
	  (loop (cdr sources)
		res)))))

;*---------------------------------------------------------------------*/
;*    skip-until-eol ...                                               */
;*---------------------------------------------------------------------*/
(define (skip-until-eol port)
   (let ((grammar (regular-grammar ()
		     (#\\
		      (ignore))
		     ((: #\\ #\Newline)
		      (ignore))
		     (#\Newline
		      'done) 
		     ((+ (out #\Newline #\\))
		      (ignore)))))
      (read/rp grammar port)))

;*---------------------------------------------------------------------*/
;*    update-makefile ...                                              */
;*---------------------------------------------------------------------*/
(define (update-makefile objects sources)
   ;; first we have to rename the old Makefile
   (let* ((old-name (string-append *makefile-name* "~"))
	  (sources  (make-sources-list sources))
	  (objects-jvm (objects->classes objects))
	  (class-jvm (sources->classes sources))
	  (objects_e '())
	  (gui-objects (sources->gui-objects sources))
	  (update-grammar (regular-grammar ((blank (out #\tab #\space #\Newline)))
			     ((: "EOBJECTS" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) objects_e)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "OBJECTS" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) objects)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "OBJECTS_JVM" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) objects-jvm)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "CLASS_OBJECTS_JVM" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) class-jvm)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "GUI_OBJECTS" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) gui-objects)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "OBJECTS_P" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string)
						   (objects->objects_p objects))
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "SOURCES" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) sources)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "SOURCES_C" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string)
						   (sources->sources_c sources))
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: (+ #\Newline) "# Rules")
			      'done)
			     ((+ blank)
			      (display (the-string))
			      (ignore))
			     ((+ (in #\Newline #\tab #\space))
			      (display (the-string))
			      (ignore))
			     (else
			      (let ((c (the-failure)))
			 	 (if (eof-object? c)
				     c
				     (error "bglmake"
					    "Illegal char"
					    (string #\[ c #\]))))))))
      (rename-file *makefile-name* old-name)
      (with-makefile-output
       (with-input-from-file old-name
	  (lambda ()
	     (read/rp update-grammar (current-input-port))
	     (emit-dependences sources objects gui-objects))))))

;*---------------------------------------------------------------------*/
;*    print-makefile-list ...                                          */
;*---------------------------------------------------------------------*/
(define (print-makefile-list field lst)
   (wprin field #\space)
   (for-each (lambda (el)
		(let ((len (+fx 1 (string-length el))))
		   (if (>fx len 73)
		       (wprint el #" \\\n      ")
		       (begin
			  (if (> (+fx *wcol* len) 73)
			      (begin
				 (wfill-to-column 73 #\space)
				 (wprin #"\\\n      ")))
			  (wprin el " ")))))
	     lst)
   (wnewline))
  
;*---------------------------------------------------------------------*/
;*    create-makefile ...                                              */
;*---------------------------------------------------------------------*/
(define (create-makefile project target objects sources)
   (let* ((port (open-input-template))
 	  (sources (make-sources-list sources))
	  (objects-jvm (objects->classes objects))
	  (class-jvm (sources->classes sources))
	  (objects_e '())
	  (objects_p (objects->objects_p objects))
	  (gui-objects (sources->gui-objects sources))
	  (create-grammar (regular-grammar ((blank (out #\$ #\tab #\space #\Newline)))
			     ((: "EOBJECTS" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) objects_e)
			      (skip-until-eol port)
			      (ignore))
			     ((: "OBJECTS" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) objects)
			      (skip-until-eol port)
			      (ignore))
			     ((: "OBJECTS_JVM" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) objects-jvm)
			      (skip-until-eol port)
			      (ignore))
			     ((: "CLASS_OBJECTS_JVM" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) class-jvm)
			      (skip-until-eol port)
			      (ignore))
			     ((: "GUI_OBJECTS" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string) gui-objects)
			      (skip-until-eol port)
			      (ignore))
			     ((: "OBJECTS_P" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list
			       (the-string)
			       objects_p)
			      (skip-until-eol port)
			      (ignore))
			     ((: "SOURCES" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list
			       (the-string)
			       sources)
			      (skip-until-eol port)
			      (ignore))
			     ((: "SOURCES_C" (+ (in #\tab #\space)) #\=)
			      (print-makefile-list (the-string)
						   (sources->sources_c sources))
			      (skip-until-eol port)
			      (ignore))
			     ((+ blank)
			      (display (the-string))
			      (ignore))
			     ((: #\$ #\[ (+ (in (#\A #\Z) #\-)) #\])
			      (let ((str (the-string)))
				 (cond
				    ((string=? str "$[PROJECT]")
				     (display *project-name*))
				    ((string=? str "$[TARGET-NAME]")
				     (display *target-name*))
				    ((string=? str "$[MAKEFILE]")
				     (if (string? *makefile-name*)
					 (display *makefile-name*)
					 (display "Makefile")))
				    ((string=? str "$[ENTRY]")
				     (if (string? *heap-entry*)
					 (display *heap-entry*)
					 (display *main-entry*)))
				    (else
				     (display (the-string)))))
			      (ignore))
			     ((+ (in #\Newline #\tab #\space))
			      (display (the-string))
			      (ignore))
			     (#\$
			      (display (the-string))
			      (ignore))
			     (else
			      (let ((c (the-failure)))
				 (if (eof-object? c)
				     c
				     (error "bglmake"
					    "Illegal char"
					    (string #\[ c #\]))))))))
      (if (not (input-port? port))
	  (error "bglmake" "Can't open template" *template-name*)
	  (unwind-protect
	     (begin
		(emit-header)
		(read/rp create-grammar port)
		(emit-dependences sources objects gui-objects))
	     (close-input-port port)))))

;*---------------------------------------------------------------------*/
;*    update-makefile-mode ...                                         */
;*---------------------------------------------------------------------*/
(define (update-makefile-mode mode::bstring)
   (let* ((old-name (string-append *makefile-name* "~"))
	  (update-grammar (regular-grammar ((blank (out #\tab #\space #\Newline)))
			     ((: "MODE" (+ (in #\tab #\space)) #\=)
			      (print (the-string) " " mode)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((+ blank)
			      (display (the-string))
			      (ignore))
	 		     ((+ (in #\Newline #\tab #\space))
			      (display (the-string))
			      (ignore))
			     ((+ blank)
			      (display (the-string))
			      (ignore))
			     (else
			      (let ((c (the-failure)))
				 (if (eof-object? c)
				     c
				     (error "bglmake"
					    "Illegal char"
					    (string #\[ c #\]))))))))
      (rename-file *makefile-name* old-name)
      (with-makefile-output
       (with-input-from-file old-name
	  (lambda ()
	     (read/rp update-grammar (current-input-port)))))))

;*---------------------------------------------------------------------*/
;*    update-makefile-project-name ...                                 */
;*---------------------------------------------------------------------*/
(define (update-makefile-project-name name::bstring)
   (let* ((old-name (string-append *makefile-name* "~"))
	  (update-grammar (regular-grammar ((blank (out #\tab #\space #\Newline)))
			     ((: "PROJECT" (+ (in #\tab #\space)) #\=)
			      (print (the-string) " " name)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((: "TARGET_NAME" (+ (in #\tab #\space)) #\=)
			      (print (the-string) " " name)
			      (skip-until-eol (current-input-port))
			      (ignore))
			     ((+ blank)
			      (display (the-string))
			      (ignore))
	 		     ((+ (in #\Newline #\tab #\space))
			      (display (the-string))
			      (ignore))
			     ((+ blank)
			      (display (the-string))
			      (ignore))
			     (else
			      (let ((c (the-failure)))
				 (if (eof-object? c)
				     c
				     (error "bglmake"
					    "Illegal char"
					    (string #\[ c #\]))))))))
      (rename-file *makefile-name* old-name)
      (with-makefile-output
       (with-input-from-file old-name
	  (lambda ()
	     (read/rp update-grammar (current-input-port)))))))

;*---------------------------------------------------------------------*/
;*    source->gui ...                                                  */
;*---------------------------------------------------------------------*/
(define (source->gui fname)
   (let ((name (string-append (prefix fname) "." *gui-suffix*)))
      (if (file-exists? name)
	  name
	  #f)))

;*---------------------------------------------------------------------*/
;*    make-sources-list ...                                            */
;*    -------------------------------------------------------------    */
;*    This function takes as input a list of Bigloo source files       */
;*    and it builds a new source list where some Bigloo source files   */
;*    may have been turned to GUI source files.                        */
;*---------------------------------------------------------------------*/
(define (make-sources-list sources)
   (map (lambda (f)
	   (let ((gui (source->gui f)))
	      (if (string? gui) gui f)))
	sources))

;*---------------------------------------------------------------------*/
;*    Writer variables ...                                             */
;*---------------------------------------------------------------------*/
(define *wcol*  1)

;*---------------------------------------------------------------------*/
;*    wnewline ...                                                     */
;*---------------------------------------------------------------------*/
(define (wnewline)
   (set! *wcol* 1)
   (newline))

;*---------------------------------------------------------------------*/
;*    wdisplay ...                                                     */
;*---------------------------------------------------------------------*/
(define (wdisplay obj)
   (cond
      ((string? obj)
       (let ((len (string-length obj)))
	  (let loop ((i 0))
	     (cond
		((=fx i len)
		 (display obj))
		((char=? (string-ref obj i) #\Newline)
		 (set! *wcol* 1)
		 (loop (+fx i 1)))
		((char=? (string-ref obj i) #\tab)
		 (set! *wcol* (+fx *wcol* (-fx 8 (modulofx *wcol* 8))))
		 (loop (+fx i 1)))
		(else
		 (set! *wcol* (+fx *wcol* 1))
		 (loop (+fx i 1)))))))
      ((char? obj)
       (if (char=? obj #\Newline)
	   (set! *wcol* 1)
	   (set! *wcol* (+fx 1 *wcol*)))
       (display obj))
      (else
       (let ((port (open-output-string)))
	  (display obj port)
	  (wdisplay (close-output-port port))))))

;*---------------------------------------------------------------------*/
;*    wprin ...                                                        */
;*---------------------------------------------------------------------*/
(define (wprin . obj)
   (for-each wdisplay obj))
   
;*---------------------------------------------------------------------*/
;*    wprint ...                                                       */
;*---------------------------------------------------------------------*/
(define (wprint . obj)
   (for-each wdisplay obj)
   (wnewline))

;*---------------------------------------------------------------------*/
;*    wfill-to-column ...                                              */
;*---------------------------------------------------------------------*/
(define (wfill-to-column column motif)
   (let loop ()
      (if (<fx *wcol* column)
	  (begin
	     (wprin motif)
	     (loop)))))

;*---------------------------------------------------------------------*/
;*    wcomment ...                                                     */
;*---------------------------------------------------------------------*/
(define (wcomment string)
   (if (<fx (string-length string) 65)
       (begin
	  (wprin "#*    ")
	  (wprin string)
	  (wfill-to-column 72 #\space)
	  (wprint "*/"))
       (begin
	  (wcomment (substring string 0 64))
	  (wcomment (substring string 65 (string-length string))))))
    
;*---------------------------------------------------------------------*/
;*    emit-header ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-header)
   ;; we emit the Makefile header
   (wprin "#*") (wfill-to-column 72 #\=) (wprint "*/")
   (wcomment (if (string? *makefile-name*) *makefile-name* "stdout"))
   (wcomment "-------------------------------------------------------------")
   (let ((author (getenv "LOGNAME")))
      (wcomment (string-append "Author      :  "
			       (if (string? author)
				   author
				   ""))))
   (let ((date::bstring (date)))
      (wcomment (string-append "Creation    :  " date)))
   (wcomment "Last change :  ")
   (wcomment "-------------------------------------------------------------")
   (wcomment (string-append "Automatically generated file (bglmake v"
			    *bmake-version* "):"))
   (wcomment (string-append "   " (car (command-line))))
   (for-each (lambda (arg)
		(wcomment (string-append "      " arg)))
	     (cdr (command-line)))
   (wprin "#*") (wfill-to-column 72 #\=) (wprint "*/"))

;*---------------------------------------------------------------------*/
;*    emit-dependences ...                                             */
;*    -------------------------------------------------------------    */
;*    In order to avoid non portable implicit rules we emit an         */
;*    explicit rule for each of the object files.                      */
;*---------------------------------------------------------------------*/
(define (emit-dependences sources objects gui-objects)
   (if *library?*
       (emit-library-dependences sources objects gui-objects)
       (emit-plain-dependences sources objects gui-objects)))

;*---------------------------------------------------------------------*/
;*    emit-plain-dependences ...                                       */
;*---------------------------------------------------------------------*/
(define (emit-plain-dependences sources objects gui-objects)
   (define (bigloo-object? pre)
      (let loop ((suf *source-suffixes*))
	 (cond
	    ((null? suf)
	     #f)
	    ((member (string-append pre "." (car suf)) sources)
	     #t)
	    (else
	     (loop (cdr suf))))))
   (define (gui-object? pre)
      (member (string-append pre "." *gui-suffix*) sources))
   (define (c-object? pre)
      (member (string-append pre ".c") sources))
   (wprint #"\n\n# Rules")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# GUI objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj)))
		   (wprint obj ": " pre ".bld")
		   (wprint #"\t@ $(BLD2SCM) "
			   pre ".bld -o " obj)
		   (wprint "")))
	     gui-objects)
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# Bigloo objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj))
		      (suf (suffix obj)))
		   (cond
		      ((and (string=? suf "o") (bigloo-object? pre))
		       (wprint obj ": " pre ".scm")
		       (wprint #"\t@ $(BIGLOO) $(BFLAGS) -c "
			       pre ".scm -o " pre ".o")
		       (wprint pre "_p.o: " pre ".scm")
		       (wprint #"\t@ $(BIGLOO) $(BFLAGS_P) -c "
			       pre ".scm -o " pre "_p.o")
		       (if *mco?*
			   (begin
			      (wprint pre ".mco: " pre ".scm")
			      (wprint #"\t@ $(MCO_EXE) $(MCOFLAGS) "
				      pre ".scm -o " pre ".mco")
			      (wprint pre "_p.mco: " pre ".scm")
			      (wprint #"\t@ $(MCO_EXE) $(MCOFLAGS) "
				      pre ".scm -o " pre "_p.mco")))
		       (wprint ""))
		      ((and (string=? suf "o") (gui-object? pre))
		       (wprint obj ": " pre ".bld")
;* 		       (wprint #"\t@ $(BLD2SCM) "          */
;* 			       pre ".bld -o " pre ".scm")              */
		       (wprint #"\t@ $(BIGLOO) $(BFLAGS) -c "
			       pre ".scm -o " pre ".o")
		       (wprint pre "_p.o: " pre ".bld")
;* 		       (wprint #"\t@ $(BLD2SCM) "          */
;* 			       pre ".bld -o " pre ".scm")              */
		       (wprint #"\t@ $(BIGLOO) $(BFLAGS_P) -c "
			       pre ".scm -o " pre "_p.o")
		       (if *mco?*
			   (begin
			      (wprint pre ".mco: " pre ".scm")
			      (wprint #"\t@ $(MCO_EXE) $(MCOFLAGS) "
				      pre ".scm -o " pre ".mco")
			      (wprint pre "_p.mco: " pre ".scm")
			      (wprint #"\t@ $(MCO_EXE) $(MCOFLAGS) "
				      pre ".scm -o " pre "_p.mco")))
		       (wprint "")))))
	     objects)
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# Bigloo JVM objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj))
		      (suf (suffix obj)))
		   (cond
		      ((and (string=? suf "class") (bigloo-object? pre))
		       (wprint obj ": " pre ".scm")
		       (wprint #"\t@ $(BIGLOO) $(BJVMFLAGS) $(BFLAGS) -c "
			       pre ".scm -o " pre ".class")
		       (wprint ""))
		      ((and (string=? suf "class") (gui-object? pre))
		       (wprint obj ": " pre ".bld")
;* 		       (wprint #"\t@ $(BLD2SCM) "          */
;* 			       pre ".bld -o " pre ".scm")              */
		       (wprint #"\t@ $(BIGLOO) $(BJVMFLAGS) $(BFLAGS) -c "
			       pre ".scm -o " pre ".class")
		       (wprint pre "_p.o: " pre ".bld")
;* 		       (wprint #"\t@ $(BLD2SCM) "          */
;* 			       pre ".bld -o " pre ".scm")              */
		       (wprint "")))))
	     (objects->classes objects))
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# C objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj))
		      (suf (suffix obj)))
		   (if (and (string=? suf "o") (c-object? pre))
		       (begin
			  (wprint obj ": " pre ".c")
			  (wprint #"\t@ echo " pre ".c:")
			  (wprint #"\t@ $(CC) $(CFLAGS) -c "
				  pre ".c -o " obj)
			  (wprint pre "_p.o: " pre ".c")
			  (wprint #"\t@ echo " pre ".c:")
			  (wprint #"\t@ $(CC) $(CFLAGS_P) -c "
				  pre ".c -o " pre ".o; "
				  "mv " obj " " pre "_p.o")
			  (wprint "")))))
	     objects))
   
;*---------------------------------------------------------------------*/
;*    emit-library-dependences ...                                     */
;*---------------------------------------------------------------------*/
(define (emit-library-dependences sources objects gui-objects)
   (define (bigloo-object? pre)
      (let loop ((suf *source-suffixes*))
	 (cond
	    ((null? suf)
	     #f)
	    ((member (string-append pre "." (car suf)) sources)
	     #t)
	    (else
	     (loop (cdr suf))))))
   (define (c-object? pre)
      (member (string-append pre ".c") sources))
   (wprint #"\n\n# Rules")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# GUI objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj)))
		   (wprint obj ": " pre ".bld")
		   (wprint #"\t@ $(BLD2SCM) "
			   obj "-o " pre ".scm")
		   (wprint "")))
	     gui-objects)
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# Bigloo objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj))
		      (suf (suffix obj)))
		   (if (and (string=? suf "o") (bigloo-object? pre))
		       (begin
			  (wprint ".Olib_u/" obj ": " pre ".scm")
			  (wprint #"\t@ $(BIGLOO) $(BFLAGS) -c "
				  pre ".scm -o .Olib_u/" obj)
			  (wprint ".Olib/" obj ": " pre ".scm")
			  (wprint #"\t@ $(BIGLOO) $(BFLAGS) -c "
				  pre ".scm -o .Olib/" obj)
			  (wprint ".Olib_p/" obj ": " pre ".scm")
 			  (wprint #"\t@ $(BIGLOO) $(BFLAGS) -c "
				  pre ".scm -o .Olib_p/" obj)
			  (wprint "")))))
	     objects)
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# Bigloo JVM objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj))
		      (suf (suffix obj)))
		   (if (and (string=? suf "class") (bigloo-object? pre))
		       (begin
			  (wprint ".Olib_u/" obj ": " pre ".scm")
			  (wprint #"\t@ $(BIGLOO) $(BJVMFLAGS) $(BFLAGS) -c "
				  pre ".scm -o .Olib_u/" obj)
			  (wprint ".Olib/" obj ": " pre ".scm")
			  (wprint #"\t@ $(BIGLOO) $(BJVMFLAGS) $(BFLAGS) -c "
				  pre ".scm -o .Olib/" obj)
			  (wprint ".Olib_p/" obj ": " pre ".scm")
 			  (wprint #"\t@ $(BIGLOO) $(BJVMFLAGS) $(BFLAGS) -c "
				  pre ".scm -o .Olib_p/" obj)
			  (wprint "")))))
	     (objects->classes objects))
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "# C objects")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (for-each (lambda (obj)
		(let ((pre (prefix obj))
		      (suf (suffix obj)))
		   (if (and (string=? suf "o") (c-object? pre))
		       (begin
			  (wprint ".Olib_u/" obj ": " pre ".c")
			  (wprint #"\t@echo " pre ".c:")
			  (wprint #"\t@ $(CC) $(CFLAGS) -c "
				  pre ".c -o .Olib_u/" obj)
			  (wprint ".Olib/" obj ": " pre ".c")
			  (wprint #"\t@echo " pre ".c:")
			  (wprint #"\t@ $(CC) $(CFLAGS) -c "
				  pre ".c -o .Olib/" obj)
			  (wprint ".Olib_p/" obj ": " pre ".c")
			  (wprint #"\t@echo " pre ".c:")
			  (wprint #"\t@ $(CC) $(CFLAGS) -c "
				  pre ".c -o .Olib_p/" obj)
			  (wprint "")))))
	     objects))
