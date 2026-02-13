;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/bde/jfile/jfile.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 10:49:15 1993                          */
;*    Last change :  Fri Feb 13 11:47:52 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Package access file generator.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module jfile (main main))

;*---------------------------------------------------------------------*/
;*    Global variables                                                 */
;*---------------------------------------------------------------------*/
(define *verbose* #f)
(define *suffixes* '("scm" "sch" "bgl"))
(define *gui-suffix* "bld")
(define *package-base* #unspecified)
(define *module-keywords* '(module))
(define *strip* 0)
(define *strip-prefix* #f)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((access-list '())
	 (add-list '())
	 (output-file '())
	 (search-path '(".")))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This message"))
	  (usage args-parse-usage))
	 ((("-v" "--verbose") (help "Verbose"))
	  (set! *verbose* #t))
	 ((("-o" "--output-file") ?file (help "Set output file"))
	  (set! output-file file))
	 ((("-I" "--search-path") ?path (help "Add search path"))
	  (set! search-path (cons path search-path)))
	 ((("-padd" "--padd") ?module ?file (help "Add module access"))
	  (set! add-list (cons (list module file) add-list)))
	 ((("-pbase" "--pbase") ?dir (help "Set base package"))
	  (set! *package-base* dir))
	 ((("-s" "-suffix" "--suffix") ?suf (help "Add source suffix"))
	  (set! *suffixes* (cons suf *suffixes*)))
	 ((("-gui-suffix" "--gui-suffix") ?suf (help "Set gui suffix"))
	  (set! *gui-suffix* suf))
	 ((("-m" "--module-keyword") ?k (help "Add module keyword"))
	  (set! *module-keywords* (cons (string->symbol k) *module-keywords*)))
	 (("--strip" ?int (help "Strip directories (default 0)"))
	  (set! *strip* (string->integer int)))
	 (("--strip-prefix" ?prefix (help "Strip prefix directory (default \"\")"))
	  (set! *strip-prefix* prefix))
	 (else
	  (set! access-list (cons else access-list))))
      (output access-list add-list output-file search-path)))

;*---------------------------------------------------------------------*/
;*    my-open-input-file ...                                           */
;*---------------------------------------------------------------------*/
(define (my-open-input-file file-name)
   (if *verbose*
       (print file-name ":"))
   (open-input-file file-name))

;*---------------------------------------------------------------------*/
;*    make-package-name ...                                            */
;*---------------------------------------------------------------------*/
(define (make-package-name name)
   (let* ((base-name-sans-ext (prefix (basename name)))
          ;; we are assuming that the directory portion does not contain
          ;; components requiring name mangling. If this is not so, the
          ;; jvm will complain about invalid class names. Should we
          ;; name mangle directory components?
          (dir-name  (dirname name))
          (name (string-append (if (string=? dir-name ".")
                                   ""
                                   (string-append dir-name "/")) 
                   (if (bigloo-need-mangling? base-name-sans-ext)
                       (bigloo-mangle base-name-sans-ext)
                       base-name-sans-ext))))
      (let loop ((i (-fx (string-length name) 1)))
	 (cond
	    ((=fx i -1)
	     (let ((name (cond
			    ((>fx *strip* 0)
			     (let ((l (string-split name ".")))
				(format "~(.)" (list-tail l *strip*))))
			    ((and (string? *strip-prefix*)
				  (string-prefix? *strip-prefix* name))
			     (substring name (+fx 1 (string-length *strip-prefix*))))
			    (else
			     name))))
		(if (string? *package-base*)
		    (string-append *package-base* "." name)
		    name)))
	    ((char=? (string-ref name i) #\/)
	     (string-set! name i #\.)
	     (loop (-fx i 1)))
	    (else
	     (loop (-fx i 1)))))))
	      
;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output access-list add-list output-file path)
   (let ((port (if (string? output-file)
		   (begin
		      (if (file-exists? output-file)
			  (rename-file output-file
				       (string-append output-file "~")))
		      (open-output-file output-file))
		   (current-output-port))))
      (fprint port
	      ";; " (pwd) #\Newline
	      ";; " (date) #\Newline #\Newline
	      #\()
      (for-each (lambda (cell)
		   (fprint port "  (" (car cell) " " "\"" (cadr cell) "\")"))
		add-list)
      (let loop ((access-list access-list))
	 (if (null? access-list)
	     (begin
		(fprint port #\) #\Newline)
		(unless (eq? port (current-output-port))
		   (close-output-port port)))
	     (let ((suf (suffix (car access-list))))
		(cond
		   ((member suf *suffixes*)
		    (let ((n (find-module-name (car access-list) path)))
		       (when (symbol? n)
			  (fprint port
			     "  ("
			     n
			     " "
			     #\" (make-package-name (car access-list)) #\" #\)))))
		   ((string=? suf *gui-suffix*)
		    (fprint port
			    "  ("
			    (find-gui-module-name (car access-list) path)
			    " "
			    #\" (make-package-name
				 (string-append (prefix (car access-list))))
			    #\" #\))))
		(loop (cdr access-list)))))))
	  
;*---------------------------------------------------------------------*/
;*    module? ...                                                      */
;*---------------------------------------------------------------------*/
(define (module? s)
   (and (symbol? s) (memq s *module-keywords*)))

;*---------------------------------------------------------------------*/
;*    find-module-name ...                                             */
;*---------------------------------------------------------------------*/
(define (find-module-name fname path)
   (let ((file (find-file/path fname path)))
      (if (or (not (string? file)) (not (file-exists? file)))
	  (begin
	     (fprint (current-error-port) "*** ERROR:bgljfile:"
		     #\Newline
		     "Can't find file -- " fname)
	     'no-such-module)
	  (let ((port (my-open-input-file file)))
	     (if (not (input-port? port))
		 (begin
		    (fprint (current-error-port) "*** ERROR:bgljfile:"
			    #\Newline
			    "Can't open file -- " fname)
		    'no-such-module)
		 (let ((exp (read port)))
		    (match-case exp
		       (((? module?) ?module-name . ?-)
			(close-input-port port)
			module-name)
		       ((directives . ?-)
			(close-input-port port)
			#f)
		       (else
			(close-input-port port)
			(fprint (current-error-port) "*** ERROR:bglfile:"
				#\Newline
				"Illegal file format -- " file)
			#f))))))))

;*---------------------------------------------------------------------*/
;*    find-gui-module-name ...                                         */
;*---------------------------------------------------------------------*/
(define (find-gui-module-name file path)
   (let ((file (find-file/path file path)))
      (if (or (not (string? file)) (not (file-exists? file)))
	  (begin
	     (fprint (current-error-port) "*** ERROR:bglfile:"
		     #\Newline
		     "Can't find file -- " file)
	     'no-such-module)
	  (with-input-from-file file
	     (lambda ()
		(read))))))
   
;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "Bgljfile v"
      (bigloo-config 'release-number)
      (bigloo-config 'specific-version))
   (print "usage: bgljfile [options]")
   (args-parse-usage #f)
   (exit 0))
