;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/afile/afile.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 10:49:15 1993                          */
;*    Last change :  Tue Mar 20 05:34:23 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Module access file generator.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module afile (main main))

;*---------------------------------------------------------------------*/
;*    Global variables                                                 */
;*---------------------------------------------------------------------*/
(define *verbose*    #f)
(define *suffixes*   '("scm" "sch" "bgl" "snow" "spi" "hop"))
(define *gui-suffix* "bld")
(define *module-keywords* '(module))
(define *add-accesses* '())

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((output-file '())
	 (paths '("."))
	 (access-list '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This message"))
	  (usage args-parse-usage))
	 ((("-v" "--verbose") (help "Verbose"))
	  (set! *verbose* #t))
	 ((("-o" "--output-file") ?file (help "Set output file"))
	  (set! output-file file))
	 ((("-I" "--search-path") ?path (help "Add search path"))
	  (set! paths (cons path paths)))
	 ((("-s" "-suffix" "--suffix") ?suf (help "Add source suffix"))
	  (set! *suffixes* (cons suf *suffixes*)))
	 ((("-gui-suffix" "--gui-suffix") ?suf (help "Set gui suffix"))
	  (set! *gui-suffix* suf))
	 ((("-m" "--module-keyword") ?k (help "Add module keyword"))
	  (set! *module-keywords* (cons (string->symbol k) *module-keywords*)))
	 ((("-a" "--access") ?module ?file (help "Manually add access"))
	  (set! *add-accesses* (cons (list (string->symbol module) file) *add-accesses*)))
	 ((("-w" "--no-warning") (help "Inhibit warning messages"))
	  (bigloo-warning-set! 0))
	 (else
	  (set! access-list (cons else access-list))))
      (output access-list output-file paths)))

;*---------------------------------------------------------------------*/
;*    output ...                                                       */
;*---------------------------------------------------------------------*/
(define (output access-list output-file path)
   (define (file->module file)
      (let ((suf (suffix file)))
	 (cond
	    ((member suf *suffixes*)
	     (let* ((f (find-file/path file path))
		    (mod (find-module-name f file)))
		(when mod (list mod f))))
	    ((string=? suf *gui-suffix*)
	     (let* ((f (find-file/path file path))
		    (mod (find-gui-module-name f)))
		(when mod
		   (list mod (string-append (prefix f) ".scm")))))
	    (else
	     #f))))
   (let ((new (sort (append *add-accesses*
			    (filter-map file->module access-list))
		    (lambda (s1 s2)
		       (string<=? (cadr s1) (cadr s2)))))
	 (old (and (string? output-file)
		   (file-exists? output-file)
		   (with-input-from-file output-file (lambda () (read))))))
      (unless (equal? new old)
	 (let ((po (let ((p (if (string? output-file)
				(begin
				   (if (file-exists? output-file)
				       (rename-file output-file
						    (string-append output-file
								   "~")))
				   (open-output-file output-file))
				#f)))
		      (if (not (output-port? p))
			  (current-output-port)
			  p))))
	    (fprint po
		    ";; automatically generated, don't edit" #\Newline
		    ";; " (pwd) #\Newline
		    ";; " (date) #\Newline #\Newline
		    #\()
	    (for-each (lambda (a)
			 (display "  " po)
			 (write a po)
			 (newline po))
		      new)
	    (fprint po ")\n")
	    (unless (eq? po (current-output-port))
	       (close-output-port po))))))
	  
;*---------------------------------------------------------------------*/
;*    find-module-name ...                                             */
;*---------------------------------------------------------------------*/
(define (find-module-name file name)
   (cond
      ((not (string? file))
       (warning 'bglafile "Cannot find file for input: " file)
       #f)
      ((not (file-exists? file))
       (warning 'bglafile "file does not exist" file)
       #f)
      (else
       (when *verbose* (print file ":"))
       (let ((port (open-input-file file)))
	  (if (not (input-port? port))
	      (begin
		 (warning 'bglafile "Cannot open file for input: " file)
		 #f)
	      (unwind-protect
		 (let ((exp (read port)))
		    (match-case exp
		       (((? module?) ?module-name . ?-)
			module-name)
		       ((directives . ?-)
			#f)
		       (else
			(warning 'bglafile "Illegal file format: " file)
			#f)))
		 (close-input-port port)))))))

;*---------------------------------------------------------------------*/
;*    module? ...                                                      */
;*---------------------------------------------------------------------*/
(define (module? s)
   (and (symbol? s) (memq s *module-keywords*)))

;*---------------------------------------------------------------------*/
;*    find-gui-module-name ...                                         */
;*---------------------------------------------------------------------*/
(define (find-gui-module-name file)
   (if (not (file-exists? file))
       (begin
	  (fprint (current-error-port) "*** ERROR:bglafile:" #\Newline
		  "Can't find file -- " file)
	  'no-such-module)
       (with-input-from-file file
	  (lambda ()
	     (read)))))
   
;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage)
   (print "Bglafile v"
	  (bigloo-config 'release-number)
	  (bigloo-config 'specific-version))
   (print "usage: bglafile [options]")
   (args-parse-usage #f)
   (exit 0))
