;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bdepend/bdepend.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 31 07:37:29 1998                          */
;*    Last change :  Thu Dec 15 18:31:04 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bigloo depend utility.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bdepend
   (main main))

;*---------------------------------------------------------------------*/
;*    Global parameters                                                */
;*---------------------------------------------------------------------*/
(define *bdepend-version*          "0.4")
(define *bdepend-path*             '("."))
(define *bdepend-exclude-path*     '())
(define *bdepend-source-files*     '())
(define *bdepend-makefile*         #f)
(define *bdepend-suffixes*         '("scm" "sch" "bgl"))
(define *bdepend-gui-suffix*       "bld")
(define *bdepend-initial-suffixes* *bdepend-suffixes*)
(define *bdepend-verbose*          #f)

(define *bdepend-start-sentinel*   "#bdepend start (don't edit)")
(define *bdepend-stop-sentinel*    "#bdepend stop")
(define *bdepend-iport*            #f)
(define *bdepend-mco?*             #t)
(define *bdepend-jvm?*             #t)
(define *bdepend-obj/mco-dir*      "")
(define *bdepend-obj-suffix*       ".o")
(define *bdepend-strict-obj-dir*   #f)
(define *bdepend-strict-mco-dir*   #f)
(define *bdepend-strict-class-dir* #f)
(define *bdepend-include-path*     '("."))
(define *bdepend-append*           #f)
(define *bdepend-sentinel-found*   #f)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; we parse command line arguments
   (parse-args argv)
   ;; we setup default value
   (default-setup)
   (unwind-protect
      (if (not (and (string? *bdepend-makefile*)
		    (file-exists? *bdepend-makefile*)))
	  (begin
	     (start-writer! #f)
	     (generate-depends))
	  (begin
	     (duplicate-makefile-prolog)
	     (generate-depends)
	     (duplicate-makefile-epilogue)))
      (stop-writer!)))

;*---------------------------------------------------------------------*/
;*    default-setup ...                                                */
;*---------------------------------------------------------------------*/
(define (default-setup)
   #f)

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args cmd-args)
   (define (usage args-parse-usage level)
      (print "usage: bgldepend [options] file ...")
      (newline)
      (args-parse-usage #f)
      (newline))
   (args-parse (cdr cmd-args)
      (("?")
       (usage args-parse-usage 1)
       (exit 0))
      ((("-help" "--help") (help "This help message"))
       (usage args-parse-usage 1)
       (exit 0))
      (("-search-path" ?path (help "Add search path"))
       (if (directory? path)
	   (set! *bdepend-path* (cons path *bdepend-path*))
	   (warning "bdepend" "Can't find search directory -- " path)))
      (("-exclude-path" ?path (help "Exclude search path"))
       (if (directory? path)
	   (set! *bdepend-exclude-path* (cons path *bdepend-exclude-path*))
	   (warning "bdepend" "Can't find search directory -- " path)))
      (("-I" ?path (help "Add <name> to the include path"))
       (set! *bdepend-include-path* (cons path *bdepend-include-path*)))
      (("-suffix" ?suf (help "Add Bigloo source suffixes"))
       (set! *bdepend-suffixes* (cons suf *bdepend-suffixes*)))
      (("-gui-suffix" ?suf (help "Set GUI suffix"))
       (set! *bdepend-gui-suffix* suf))
      (("-o" ?name (help "Set makefile name"))
       (set! *bdepend-makefile* name))
      (("-v" (help "Be verbose"))
       (set! *bdepend-verbose* #t))
      (("-fno-mco" (help "Don't produce mco dependences"))
       (set! *bdepend-mco?* #f))
      (("-fmco" (help "Do produce mco dependences"))
       (set! *bdepend-mco?* #t))
      (("-fno-jvm" (help "Don't produce dependences for .class"))
       (set! *bdepend-jvm?* #f))
      (("-fjvm" (help "Do produce dependences for .class"))
       (set! *bdepend-jvm?* #t))
      (("-obj-dir" ?pref (help "directory prefix for .o/.mco files"))
       (set! *bdepend-obj/mco-dir* (string-append pref "/")))
      (("-obj-suffix" ?suf (help "object file suffix (defaults to .o)"))
       (set! *bdepend-obj-suffix* suf))
      (("-strict-obj-dir" ?pref (help "directory for .o"))
       (set! *bdepend-strict-obj-dir* (string-append pref "/")))
      (("-strict-mco-dir" ?pref (help "directory for .mco"))
       (set! *bdepend-strict-mco-dir* (string-append pref "/")))
      (("-strict-class-dir" ?pref (help "directory for .class"))
       (set! *bdepend-strict-class-dir* (string-append pref "/")))
      (("-append" (help "append depencies instead of replacing existing ones"))
       (set! *bdepend-append* #t))
      (else
       (if (source? else)
	   (set! *bdepend-source-files* (cons else *bdepend-source-files*))))))

;*---------------------------------------------------------------------*/
;*    duplicate-makefile-prolog ...                                    */
;*---------------------------------------------------------------------*/
(define (duplicate-makefile-prolog)
   (let ((svg-name (string-append *bdepend-makefile* "~")))
      (rename-file *bdepend-makefile* svg-name)
      (start-writer! *bdepend-makefile*)
      (let ((iport (open-input-file svg-name)))
	 (if (not (input-port? iport))
	     (error "bgldepend" "Can't open file for input" svg-name)
	     (begin
		(let loop ((line (read-line iport)))
		   (cond
		      ((eof-object? line)
		       (close-input-port iport))
		      ((string=? line *bdepend-start-sentinel*)
		       (set! *bdepend-sentinel-found* #t)
		       (when *bdepend-append* (wprint line))
		       (let loop ((line (read-line iport)))
			  (cond
			     ((eof-object? line)
			      (close-input-port iport))
			     ((string=? line *bdepend-stop-sentinel*)
			      (set! *bdepend-iport* iport))
			     (else
			      (when *bdepend-append* (wprint line))
			      (loop (read-line iport))))))
		      (else
		       (wprint line)
		       (loop (read-line iport))))))))))

;*---------------------------------------------------------------------*/
;*    duplicate-makefile-epilogue ...                                  */
;*---------------------------------------------------------------------*/
(define (duplicate-makefile-epilogue)
   (if (input-port? *bdepend-iport*)
       (let loop ((line (read-line *bdepend-iport*)))
	  (if (eof-object? line)
	      (close-input-port *bdepend-iport*)
	      (begin
		 (wprint line)
		 (loop (read-line *bdepend-iport*)))))))

;*---------------------------------------------------------------------*/
;*    Writer variables ...                                             */
;*---------------------------------------------------------------------*/
(define *wport* #f)
(define *wcol*  1)

;*---------------------------------------------------------------------*/
;*    stop-writer! ...                                                 */
;*---------------------------------------------------------------------*/
(define (stop-writer!)
   (if (output-port? *wport*)
       (begin
	  (if (>fx *wcol* 1)
	      (wnewline))
	  (flush-output-port *wport*)
	  (if (not (eq? *wport* (current-output-port)))
	      (close-output-port *wport*)))))

;*---------------------------------------------------------------------*/
;*    wnewline ...                                                     */
;*---------------------------------------------------------------------*/
(define (wnewline)
   (set! *wcol* 1)
   (newline *wport*))

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
		 (display obj *wport*))
		((char=? (string-ref obj i) #\Newline)
		 (set! *wcol* 1)
		 (loop (+fx i 1)))
		(else
		 (set! *wcol* (+fx *wcol* 1))
		 (loop (+fx i 1)))))))
      ((char? obj)
       (if (char=? obj #\Newline)
	   (set! *wcol* 1)
	   (set! *wcol* (+fx 1 *wcol*)))
       (display obj *wport*))
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
	  (wcomment (substring string 0 69))
	  (wcomment (substring string 70 (string-length string))))))
   
;*---------------------------------------------------------------------*/
;*    start-writer! ...                                                */
;*---------------------------------------------------------------------*/
(define (start-writer! file)
   (if (string? file)
       (begin
	  (set! *wport* (open-output-file file))
	  (if (not (output-port? *wport*))
	      (error "start-writer!" "Can't open file for output" file)))
       (set! *wport* (current-output-port))))
   
;*---------------------------------------------------------------------*/
;*    generate-depends ...                                             */
;*---------------------------------------------------------------------*/
(define (generate-depends)
   (define (generate-include-dependency inc)
      (wprin inc ": "))
   (define (generate-module-dependency mod)
      (wprin (if (source? mod) (source->object mod) mod))
      (if (and (source? mod) *bdepend-jvm?*)
	  (wprin " " (source->class mod)))
      (wprin ": "))
   (define (generate-dependency objects)
      (for-each (lambda (object)
		   (let ((len (+fx 1 (string-length object))))
		      (if (>fx len 74)
			  (wprint object " \\")
			  (begin
			     (if (> (+fx *wcol* len) 74)
				 (begin
				    (wfill-to-column 74 #\space)
				    (if (=fx *wcol* 1)
					(wprin "      "))
				    (wprin #"\\\n      ")))
			     (wprin object " ")))))
		objects)
      (wnewline))
   ;; The object files
   (unless (and *bdepend-append* *bdepend-sentinel-found*)
      (wprint *bdepend-start-sentinel*))
   ;; the dependencies entry
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   (wcomment "Dependencies ...")
   (wprin "#*") (wfill-to-column 72 #\-) (wprint "*/")
   ;; we load all source files
   (find-all-bigloo-sources)
   ;; and we start emitting objects
   (multiple-value-bind (objects includes)
      (find-all-dependences *bdepend-source-files*)
      (for-each (lambda (include)
		   (generate-include-dependency (car include))
		   (generate-dependency (cdr include)))
		includes)
      (for-each (lambda (module)
		   (generate-module-dependency (car module))
		   (generate-dependency (cdr module)))
		objects)
      (wnewline))
   ;; we are done
   (wprint *bdepend-stop-sentinel*))

;*---------------------------------------------------------------------*/
;*    source? ...                                                      */
;*---------------------------------------------------------------------*/
(define (source? fname)
   (member (suffix fname) *bdepend-suffixes*))

;*---------------------------------------------------------------------*/
;*    gui? ...                                                         */
;*---------------------------------------------------------------------*/
(define (gui? fname)
   (string=? (suffix fname) *bdepend-gui-suffix*))

;*---------------------------------------------------------------------*/
;*    gui->source ...                                                  */
;*---------------------------------------------------------------------*/
(define (gui->source fname)
   (string-append (prefix fname) ".scm"))

;*---------------------------------------------------------------------*/
;*    source->module ...                                               */
;*---------------------------------------------------------------------*/
(define (source->module fname)
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
		     (escape '())))
	     (close-input-port iport))
	  (error "bgldepend" "Can't open file for input" fname))))

;*---------------------------------------------------------------------*/
;*    find-all-bigloo-sources ...                                      */
;*---------------------------------------------------------------------*/
(define (find-all-bigloo-sources)
   (if *bdepend-verbose* (print "Scanning for source files..."))
   (define (dir->list dir)
      (sort (directory->list dir) string<?))
   (define (find-all-bigloo-sources/basename basename files)
      (for-each (lambda (file)
		   (let ((fname (string-append basename file)))
		      (if (file-exists? fname)
			  (cond
			     ((directory? fname)
			      (if (not (memq fname *bdepend-exclude-path*))
				  (find-all-bigloo-sources/basename
				   (string-append fname "/")
				   (dir->list fname))))
			     ((source? fname)
			      (let ((module (source->module fname)))
				 (if (symbol? module)
				     (putprop! module 'source fname))))))))
		files))
   (find-all-bigloo-sources/basename "" (dir->list "."))
   (for-each (lambda (dir)
		(find-all-bigloo-sources/basename (string-append dir "/")
						  (dir->list dir)))
	     *bdepend-path*))

;*---------------------------------------------------------------------*/
;*    *object-env* ...                                                 */
;*---------------------------------------------------------------------*/
(define *object-env* (make-hashtable))
(define *include-env* (make-hashtable))
(define *extern-include-env* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    find-all-dependences ...                                         */
;*---------------------------------------------------------------------*/
(define (find-all-dependences source-files)
   (if *bdepend-verbose*
       (print "Generating object list...: " source-files))
   (for-each add-one-source! source-files)
   (let ((reso '())
	 (resi '()))
      (hashtable-for-each *object-env*
			  (lambda (key src)
			     (if (pair? (cdr src))
				 (set! reso (cons src reso)))))
      (hashtable-for-each *include-env*
			  (lambda (key src)
			     (if (pair? (cdr src))
				 (set! resi (cons src resi)))))
      (values reso resi)))

;*---------------------------------------------------------------------*/
;*    add-one-file! ...                                                */
;*---------------------------------------------------------------------*/
(define (add-one-file! source env)
   (if (not (hashtable-get env source))
       (let ((object (list source)))
	  (if *bdepend-verbose* (print source ":"))
	  (hashtable-put! env source object)
	  (set-cdr! object (find-dependences source))
	  (let ((include-files (hashtable-get *extern-include-env* source)))
	     (when (and include-files *bdepend-mco?*)
		(let ((key (source->mco source)))
		   (hashtable-put! env
				   key
				   (cons key (cdr include-files))))))
	  source)
       source))

;*---------------------------------------------------------------------*/
;*    add-one-source! ...                                              */
;*---------------------------------------------------------------------*/
(define (add-one-source! source)
   (add-one-file! source *object-env*))

;*---------------------------------------------------------------------*/
;*    add-one-include! ...                                             */
;*---------------------------------------------------------------------*/
(define (add-one-include! source)
   (add-one-file! source *include-env*))

;*---------------------------------------------------------------------*/
;*    find-dependences ...                                             */
;*---------------------------------------------------------------------*/
(define (find-dependences source)
   (if (gui? source)
       (list (gui->source source))
       (find-imported-modules source)))
			  
;*---------------------------------------------------------------------*/
;*    find-imported-modules ...                                        */
;*---------------------------------------------------------------------*/
(define (find-imported-modules fname)
   (if (file-exists? fname)
       (let ((port (open-input-file fname)))
	  (if (not (input-port? port))
	      (error "bgldepend" "Can't open file for input" fname)
	      (unwind-protect
		 (try (let ((module (read port)))
			 (match-case module
			    ((module (? symbol?) . ?clauses)
			     (find-imported-modules/clauses fname clauses))
			    ((directives . ?clauses)
			     (find-imported-modules/clauses fname clauses))
			    (else
			     '())))
		      (lambda (escape obj proc msg)
			 (escape '())))
		 (close-input-port port))))
       '()))

;*---------------------------------------------------------------------*/
;*    source->object ...                                               */
;*---------------------------------------------------------------------*/
(define (source->object src)
   (if (string? *bdepend-strict-obj-dir*)
       (string-append *bdepend-strict-obj-dir* (basename (prefix src))
                      *bdepend-obj-suffix*)
       (string-append *bdepend-obj/mco-dir* (prefix src)
                      *bdepend-obj-suffix*)))

;*---------------------------------------------------------------------*/
;*    source->mco ...                                                  */
;*---------------------------------------------------------------------*/
(define (source->mco src)
   (if (string? *bdepend-strict-mco-dir*)
       (string-append *bdepend-strict-mco-dir* (basename (prefix src)) ".mco")
       (string-append *bdepend-obj/mco-dir* (prefix src) ".mco")))

;*---------------------------------------------------------------------*/
;*    source->class ...                                                */
;*---------------------------------------------------------------------*/
(define (source->class src)
   (if (string? *bdepend-strict-class-dir*)
       (string-append *bdepend-strict-class-dir* (basename (prefix src)) ".class")
       (string-append *bdepend-obj/mco-dir* (prefix src) ".class")))

;*---------------------------------------------------------------------*/
;*    find-imported-modules/clauses ...                                */
;*---------------------------------------------------------------------*/
(define (find-imported-modules/clauses source clauses)
   (define (find-imported-file file)
      (let find ((search-dirs *bdepend-path*))
	 (if (null? search-dirs)
	     file
	     (let ((find-file (make-file-name (car search-dirs) file)))
		(if (file-exists? find-file)
		    find-file
		    (find (cdr search-dirs)))))))
   (define (find-imported-modules/import import)
      (match-case import
	 (((and ?module (? symbol?)) (and ?fname (? string?)) . ?rest)
	  ;; (module-name "file-name" ...)
	  (let ((source (add-one-source! fname)))
	     (if *bdepend-mco?*
		 (list (source->mco source))
		 '())))
	 (((and ?var (? symbol?)) (and ?module (? symbol?)))
	  ;; (variable module-name)
	  (let ((source (getprop module 'source)))
	     (if (string? source)
		 (begin
		    (add-one-source! source)
		    (if *bdepend-mco?*
			(list (source->mco source))
			'())))))
	 (((? symbol?) (? symbol?) (and ?fname (? string?)) . ?rest)
	  (add-one-source! fname)
	  (if *bdepend-mco?*
	      (list (source->object fname) (source->mco fname))
	      (list (source->object fname))))
	 ((and ?module (? symbol?))
	  ;; module-name
	  (let ((source (getprop module 'source)))
	     (if (string? source)
		 (begin
		    (add-one-source! source)
		    (if *bdepend-mco?*
			(list (source->mco source))
			'())))))
	 (else
	  '())))
   (define (find-imported-modules/clause clause)
      (match-case clause
	 (((or use import) . ?imports)
	  (let loop ((imports imports)
		     (res     '()))
	     (if (null? imports)
		 res
		 (let ((aux (find-imported-modules/import (car imports))))
		    (loop (cdr imports)
			  (if (pair? aux)
			      (append aux res)
			      res))))))
	 ((cond-expand . ?eclauses)
	  (append-map
	     (lambda (c)
		(find-imported-modules/clause (cadr c)))
	     eclauses))
 	 ((extern . ?eclauses)
	  ;; we only fetch external include clauses
 	  (let loop ((clauses eclauses)
		     (all     '())
 		     (res     '()))
	     (cond
		((null? clauses)
		 (if (pair? all)
		     (hashtable-put! *extern-include-env*
				     source (cons source all)))
		 res)
		(else
		 (match-case (car clauses)
		    ((include ?fname)
		     (let ((f (find-file/path fname *bdepend-include-path*)))
			;; we don't have to include a file if it does not
			;; exists otherwise we could have dependencies
			;; such as:
			;;   foo.scm: signal.h
			(if (string? f)
			    (loop (cdr clauses)
				  (cons f all)
				  (cons fname res))
			    (loop (cdr clauses)
				  all
				  res))))
		    (else
		     (loop (cdr clauses) all res)))))))
 	 ((load . ?tags)
 	  (let loop ((tag tags)
 		     (fnames '()))
	     (cond
		((null? tag)
		 (let ((actual-includes (map find-imported-file fnames)))
		    (for-each add-one-source! actual-includes)
		    actual-includes)
		 fnames)
		((pair? (car tag))
		 (loop (cdr tag) (append fnames (cdar tag))))
		(else
		 (loop (cdr tag) fnames)))))
	 ((include . ?fnames)
 	  (let ((actual-includes (map find-imported-file fnames)))
 	     (for-each add-one-include! actual-includes)
 	     actual-includes))
	 (else
	  '())))
   (let loop ((clauses clauses)
	      (res     '()))
      (if (null? clauses)
	  res
	  (loop (cdr clauses)
		(append (find-imported-modules/clause (car clauses)) res)))))

   
