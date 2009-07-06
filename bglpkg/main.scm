;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/main.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 09:28:38 2006                          */
;*    Last change :  Mon Oct 22 13:08:35 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The bglpkg entry point.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg
   
   (library sqlite
	    pkglib)
   
   (import  bglpkg_configure
	    bglpkg_param
	    bglpkg_parseargs
	    bglpkg_package
	    bglpkg_action
	    bglpkg_extract
	    bglpkg_install
	    bglpkg_testing
	    bglpkg_man
	    bglpkg_utils)
   
   (main    main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (multiple-value-bind (actions arguments)
      (parse-args args)
      (when (and (pair? actions) (eq? (car actions) 'setup))
	 (sqlite-close (bglpkg-setup!))
	 (exit 0))
      (when (and (pair? actions) (eq? (car actions) 'reset))
	 (when (yes-or-no? "Reset the whole database? [y/n] " #f)
	    (let ((p (make-file-name (bglpkg-etc-directory) (bglpkg-db-file))))
	       (delete-file p)
	       (rm-rf (bglpkg-pkg-directory))
	       (rm-rf (bglpkg-cache-directory))
	       (sqlite-close (bglpkg-setup!))
	       (exit 0))))
      (let ((dpath (make-file-name (bglpkg-etc-directory) (bglpkg-db-file))))
	 (if (file-exists? dpath)
	     (let ((db (instantiate::sqltiny (path dpath))))
		(let loop ((actions actions))
		   (when (pair? actions)
		      (case (car actions)
			 ((create-db)
			  ;; create the database from the repository
			  (when (file-exists? dpath) (delete-file dpath))
			  (repo-create-database (bglpkg-pkg-directory) dpath))
			 ((update-db)
			  (repo-create-database (bglpkg-pkg-directory) dpath))
			 ((list)
			  ;; list the database
			  (bglpkg-db-list db))
			 ((add)
			  ;; add a package to the repository and database
			  (bglpkg-add-packages db arguments))
			 ((remove)
			  ;; remove a package to the repository and database
			  (bglpkg-remove-packages db arguments))
			 ((inject)
			  ;; inject the package descriptions found in the files
			  (bglpkg-inject-descriptions db arguments))
			 ((sync)
			  (bglpkg-sync db))
			 ((sync-list)
			  (bglpkg-db-sync-list db))
			 ((download)
			  ;; donwload the packages (and their deps)
			  (let ((packages (package-list db arguments)))
			     (bglpkg-download packages)))
			 ((extract)
			  ;; extract the packages (and their deps)
			  (bglpkg-extract db arguments))
			 ((install)
			  ;; extract and install the packages
			  (bglpkg-install db arguments))
			 ((test)
			  ;; test a package
			  (bglpkg-test db arguments))
			 ((wizard)
			  ;; create a new directory package
			  (bglpkg-wizard (cadr actions) arguments)
			  (set! actions (cdr actions)))
			 ((man)
			  ;; display the man pages
			  (bglpkg-man arguments))
			 ((query)
			  ;; query a binding definition
			  (bglpkg-query db (map (lambda (a)
						   (string-append ".*" a ".*"))
						arguments)))
			 ((query-regexp)
			  ;; query a binding definition
			  (bglpkg-query db arguments))
			 ((cleanup)
			  ;; cleanup the local repository
			  (bglpkg-cleanup db arguments))
			 ((clear)
			  ;; clear the local repository
			  (when (bglpkg-clear db arguments)
			     (bglpkg-setup!)))
			 ((exit)
			  ;; we are done
			  (sqlite-close db)
			  (exit 0))
			 (else
			  (error 'main "Unknown action" (car actions))))
		      (loop (cdr actions))))
		(sqlite-close db))
	     (with-output-to-port (current-error-port)
		(lambda ()
		   (print "*** ERROR: " (bglpkg-name))
		   (print "Database does not exist: " dpath)
		   (print "To set a bglpkg account up: "
			  (bglpkg-name) " --setup -v2")
		   (exit 1)))))))

;*---------------------------------------------------------------------*/
;*    bglpkg-setup! ...                                                */
;*    -------------------------------------------------------------    */
;*    Setup a default bglpkg repository                                */
;*---------------------------------------------------------------------*/
(define (bglpkg-setup!)
   
   (define (make-directories/check dir)
      (make-directories dir)
      (unless (directory? dir)
	 (error 'bglpkg-setup! "Cannot create directory" dir)))
   
   (define (make-directory/check dir)
      (make-directories dir)
      (unless (directory? dir)
	 (error 'bglpkg-setup! "Cannot create directory" dir)))
   
   (let* ((rcdir (bglpkg-default-rc-directory))
	  (etcdir (make-file-name rcdir "etc"))
	  (pkgdir (make-file-name rcdir "pkg"))
	  (tmpdir (make-file-name rcdir "tmp")))
      (unless (directory? rcdir)
	 (when (>fx (pkglib-verbose) 0)
	    (print (pkglib-color 'message rcdir) " created"))
	 (make-directories/check rcdir))
      (unless (directory? etcdir)
	 (when (>fx (pkglib-verbose) 0)
	    (print (pkglib-color 'message etcdir) " created"))
	 (make-directory/check etcdir))
      (unless (directory? pkgdir)
	 (when (>fx (pkglib-verbose) 0)
	    (print (pkglib-color 'message pkgdir) " created"))
	 (make-directory/check pkgdir))
      (unless (directory? tmpdir)
	 (when (>fx (pkglib-verbose) 0)
	    (print (pkglib-color 'message tmpdir) " created"))
	 (make-directory/check tmpdir))
      (let ((dpath (make-file-name etcdir (bglpkg-db-file))))
	 (repo-create-database pkgdir dpath))))

