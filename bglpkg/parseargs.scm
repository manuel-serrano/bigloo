;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/parseargs.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 12 13:32:52 2004                          */
;*    Last change :  Fri Nov 29 21:11:58 2013 (serrano)                */
;*    Copyright   :  2004-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bglpkg command line parsing                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_parseargs

   (library pkglib)
   
   (import  bglpkg_configure
	    bglpkg_param
	    bglpkg_chicken
	    bglpkg_mzscheme
	    bglpkg_snow)
   
   (export  (parse-args ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    parse-args ...                                                   */
;*---------------------------------------------------------------------*/
(define (parse-args args)
   (let ((loadp #t)
	 (rc-file #unspecified)
	 (exprs '())
	 (opts '())
	 (action '())
	 (urls '()))
      (args-parse (cdr args)
	 ;; Misc
	 (section "Misc")
         ((("-h" "--help") (help "This message (use -v -h for long help)"))
          (usage args-parse-usage)
          (exit 0))
         (("--options" (help "Display the Bglpkg options and exit"))
          (usage args-parse-usage)
          (exit 0))
         (("--version" (help "Print the version and exit"))
          (print (bglpkg-name) (bglpkg-version))
          (exit 0))
	 
	 ;; Actions
	 (section "Actions")
	 ((("-i" "--install") (help "Download, extract, and install the packages (default action)"))
	  (set! action (cons 'install action)))
	 ((("-x" "--extract") (help "Extract the packages"))
	  (set! action (cons 'extract action)))
	 ((("-d" "--download") (help "Download the packages"))
	  (set! action (cons 'download action)))
	 ((("-t" "--test") (help "Test the packages"))
	  (set! action (cons 'test action)))
	 ((("-w" "--wizard") ?name (help "Create a new package"))
	  (set! action (cons* name 'wizard action)))

	 ;; Query
	 (section "Help and search")
	 ((("-m" "--man") (help "Display a package man page"))
	  (set! action (cons* 'man action)))
	 ((("-l" "--list") (help "List available packages"))
	  (set! action (cons 'list action)))
	 ((("-q" "--query") (help "Look for a binding"))
	  (set! action (cons* 'query action)))
	 ((("-e" "--regexp") (help "Look for a binding (allow regexp)"))
	  (set! action (cons* 'query-regexp action)))

	 ;; Admins
	 (section "Admins")
	 ((("-c" "--create-db") (help "Create db from local repository"))
	  (set! action (cons 'create-db action)))
	 ((("-u" "--update-db") (help "Update db with a local repository"))
	  (set! action (cons 'update-db action)))
	 ((("-a" "--add") (help "Add a tarball to the local repository"))
	  (set! action (cons 'add action)))
	 ((("-r" "--remove")
	   (help "Remove packages (e.g., --remove foo 0.0.0 bar gee)"))
	  (set! action (cons 'remove action)))
	 ((("-s" "--sync") (help "Sync with a remote pkglib server"))
	  (set! action (cons 'sync action)))
	 ((("-p" "--sync-list") (help "Display the local sync list"))
	  (set! action (cons 'sync-list action)))
	 ((("-j" "--inject") (help "Inject package descriptions"))
	  (set! action (cons 'inject action)))
	 (("--setup" (help "Setup a default bglpkg repository and exit"))
	  (set! action (cons 'setup action)))
	 (("--cleanup" (help "Cleanup the package repository"))
	  (set! action (cons 'cleanup action)))
	 (("--clear" (help "Clear the package repository"))
	  (set! action (cons 'clear action)))
	 (("--reset" (help "Reset the database"))
	  (set! action (cons 'reset action)))

	 ;; Options
	 (section "Options")
	 ((("-F" "--force") (help "Force action"))
	  (bglpkg-force-action-set! #t))
	 ((("-Y" "--force-download") (help "Download a fresh copy"))
	  (bglpkg-force-download-set! #t))
	 ((("-T" "--tuning") ?tuning
			     (help (format "Use TUNING (default ~s)"
					   (bglpkg-tunings))))
	  (bglpkg-tunings-set! (list tuning)))
	 ((("-Z" "--no-tuning") (help "Disable tuning"))
	  (bglpkg-tunings-set! '()))
	 ((("-P" "--search-path") ?dir (help "Add source search directory"))
	  (bglpkg-search-path-set! (cons dir (bglpkg-search-path))))
	 ((("-C" "--directory") ?dir (help "Change to directory DIR"))
	  (bglpkg-destdir-set! dir))
	 ((("-D" "--no-deps") (help "Don't install recursively"))
	  (bglpkg-recursive-set! #f))
	 ((("-S" "--sync-url") ?url
			       (help (format "Set sync url (default ~a)"
					     (bglpkg-sync-urls))))
	  (set! urls (cons url urls)))
	 ((("-N" "--no-remove") (help "Don't remove tmp files"))
	  (bglpkg-delete-tmp-files-set! #f))
	 ((("-L" "--library") ?name ?version (help "Set library name and version"))
	  (bglpkg-library-set! (cons name version)))
	 ((("-X" "--exclude") ?pkg (help "Exclude package PKG"))
	  (bglpkg-excludes-set! (cons pkg (bglpkg-excludes))))
	 (("--noconfirm" (help "bypass \"are you sure?\" questions"))
	  (bglpkg-noconfirm-set! #t))
	 (("--flat" (help "Extract the source codes in the same directory"))
	  (bglpkg-flat-set! #t))
	 
	 ;; RC
	 (section "RC")
	 (("--no-rc" (help "Do not load an init file"))
	  (set! loadp #f))
	 (("--rc-file" ?file (help "Load alternate rc file"))
	  (set! rc-file file))
	 (("--rc-dir" ?dir (help "Set rc directory"))
	  (bglpkg-rc-directory-set! dir))
	 (("--show-defaults" (help "Query default settings and exit"))
	  (params-setting)
	  (exit 0))
	 
	 ;; Verbosity and logs
	 (section "Verbosity")
         (("-v?<level>" (help "Increase/set verbosity level (-v0 crystal silence)"))
          (if (string=? <level> "")
	      (pkglib-verbose-set! (+fx 1 (pkglib-verbose)))
	      (pkglib-verbose-set! (string->integer <level>))))
         (("-g?<level>" (help "Increase/set debug level"))
          (if (string=? <level> "")
	      (begin
		 (bigloo-debug-module-set! (+fx 1 (bigloo-debug-module)))
		 (bigloo-debug-set! (+fx 1 (bigloo-debug))))
	      (begin
		 (bigloo-debug-module-set! (string->integer <level>))
		 (bigloo-debug-set! (string->integer <level>)))))
	 (("--no-color" (help "Disable colored traces"))
	  (bigloo-trace-color-set! #f))

	 ;; Misc
	 (section "Miscellaneous")
	 (("--eval" ?string (help "Evaluate STRING"))
	  (set! exprs (cons string exprs)))
	 
	 ;; else
	 (("-?dummy")
	  (usage args-parse-usage
		 :exitval 1
		 :msg (format "*** ERROR: Unknown argument `-~a'" dummy)))
	 (else
	  (set! opts (cons else opts))))

      ;; sync urls
      (when (pair? urls)
	 (bglpkg-sync-urls-set! (reverse! urls)))

      ;; plugins
      (bglpkg-plugins-set! (cons* (cons 'egg egg-plugin)
				  (cons 'planet planet-plugin)
				  (cons 'snow snow-plugin)
				  (bglpkg-plugins)))

      ;; language preprocessors
      (bglpkg-language-preprocessor-set!
       (cons* (cons 'chicken chicken-preprocessor)
	      (cons 'mzscheme mzscheme-preprocessor)
	      (cons 'snow snow-preprocessor)
	      (bglpkg-language-preprocessor)))
      
      ;; load rc
      (when loadp (load-rc rc-file))
      
      ;; evaluate command line expressions
      (when (pair? exprs) (evaluate exprs))
      
      ;; what to do
      (values (if (pair? action)
		  (reverse! action)
		  '(install))
	      (reverse! opts))))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage args-parse-usage #!key (exitval 0) (msg #f))
   (when (string? msg)
      (print msg)
      (newline))
   (print (string-capitalize (bglpkg-name)) " v" (bglpkg-version))
   (print "usage: " (bglpkg-name) " [options] arg0 arg1 ...")
   (when (procedure? args-parse-usage)
      (args-parse-usage #f))
   (when (>fx (pkglib-verbose) 0)
      (newline)
      (print "Examples:")
      (print "  initiate the system:")
      (print "    bglpkg --setup\n")
      (print "  synchronize with a remote host:")
      (print "    bglpkg -s -S http://hop.inria.fr/hop/scmpkg/sync\n")
      (print "  install a package in the current directory:")
      (print "    bglpkg kishi\n")
      (print "  install a package in another directory:")
      (print "    bglpkg kishi -C /tmp/KISHI\n")
      (print "  install a tarball in another directory:")
      (print "    bglpkg kishi-1.0.0.tar.gz -C /tmp/KISHI\n")
      (print "  install and tuning a tarball:")
      (print "    bglpkg kishi-1.0.0.tar.gz kishi_bigloo-1.0.0.tar.gz\n")
      (print "  install and tuning a tarball:")
      (print "    bglpkg 'http://hop.inria.fr/hop/scmpkg/download?tarball=calend-1.0.0.tar.gz&dir=quarantine'\n")
      (print "  test a package in another directory:")
      (print "    bglpkg -t modeline -C /tmp/FOO\n")
      (print "  test a package:")
      (print "    bglpkg -t 'http://hop.inria.fr/hop/scmpkg/download?tarball=calend-1.0.0.tar.gz&dir=quarantine'\n")
      (print "  create a package:")
      (print "    bglpkg -w newpkg"))
   (exit exitval))

;*---------------------------------------------------------------------*/
;*    %bglpkg-load-rc ...                                              */
;*---------------------------------------------------------------------*/
(define (%bglpkg-load-rc path)
   (when (and (string? path) (file-exists? path))
      (pkglib-verb 2 "Loading `" path "'...\n")
      (loadq path)))

;*---------------------------------------------------------------------*/
;*    load-rc ...                                                      */
;*---------------------------------------------------------------------*/
(define (load-rc rc-file)
   (if (string? rc-file)
       (%bglpkg-load-rc rc-file)
       (let* ((rc (bglpkg-rc-file))
	      (path (make-file-name (bglpkg-rc-directory) rc)))
	  (if (file-exists? path)
	      (%bglpkg-load-rc path)
	      (let ((path (make-file-name (bglpkg-etc-directory) rc)))
		 (%bglpkg-load-rc path))))))

;*---------------------------------------------------------------------*/
;*    evaluate ...                                                     */
;*---------------------------------------------------------------------*/
(define (evaluate exprs)
   (for-each (lambda (expr)
		(with-input-from-string expr
		   (lambda ()
		      (let ((sexp (read (current-input-port))))
			 (with-handler
			    (lambda (e)
			       (if (isa? e &eval-warning)
				   (begin
				      (warning-notify e)
				      #unspecified)
				   (raise e)))
			    (eval sexp))))))
	     exprs))
