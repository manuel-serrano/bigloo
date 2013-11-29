;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/install.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 31 09:12:56 2006                          */
;*    Last change :  Fri Nov 29 21:15:18 2013 (serrano)                */
;*    Copyright   :  2006-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Install (once extracted) a ScmPkg package for Bigloo.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_install

   (include "makefile.sch")
   
   (library sqlite
	    pkglib)

   (import  bglpkg_param
	    bglpkg_package
	    bglpkg_action
	    bglpkg_extract
	    bglpkg_utils)

   (export  (bglpkg-install ::%sqlite ::pair-nil)
	    (bglpkg-install-package ::bstring  ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    pkginfo-name ...                                                 */
;*---------------------------------------------------------------------*/
(define (pkginfo-name p)
   (with-access::pkginfo p (name)
      name))

;*---------------------------------------------------------------------*/
;*    pkginfo-fail ...                                                 */
;*---------------------------------------------------------------------*/
(define (pkginfo-fail p)
   (with-access::pkginfo p (fail)
      fail))

;*---------------------------------------------------------------------*/
;*    pkginfo-source ...                                               */
;*---------------------------------------------------------------------*/
(define (pkginfo-source p)
   (with-access::pkginfo p (source)
      source))

;*---------------------------------------------------------------------*/
;*    pkginfo-exceptions ...                                           */
;*---------------------------------------------------------------------*/
(define (pkginfo-exceptions p)
   (with-access::pkginfo p (exceptions)
      exceptions))

;*---------------------------------------------------------------------*/
;*    pkginfo-macros ...                                               */
;*---------------------------------------------------------------------*/
(define (pkginfo-macros p)
   (with-access::pkginfo p (macros)
      macros))

;*---------------------------------------------------------------------*/
;*    bglpkg-install ...                                               */
;*---------------------------------------------------------------------*/
(define (bglpkg-install db arguments::pair-nil)
   (when (string=? (bglpkg-destdir) ".")
      (unless (yes-or-no? "Install in current directory? [y/n] " arguments)
	 (print "aborting...")
	 (exit 0)))
   (when (pair? arguments)
      (cond
	 ((url? (car arguments))
	  (let ((pkg (download-package (car arguments))))
	     (unwind-protect
		(if (and (pair? (cdr arguments))
			 (url? (cadr arguments)))
		    (let ((tuning (download-package (cadr arguments))))
		       (bglpkg-with-tuning db pkg tuning
	                  ;;; action to execute with the fake package
			  (lambda (db n v)
			     (install-inner db (cons* n v (cddr arguments))))))
		    (bglpkg-with-package db pkg
	               ;;; action to execute with the fake package
		       (lambda (db n v)
			  (install-inner db (cons* n v (cdr arguments))))))
		(when (bglpkg-delete-tmp-files)
		   (delete-file pkg)))))
	 ((package-filename? (car arguments))
	  (if (and (pair? (cdr arguments))
		   (package-tuning? (cadr arguments)))
	      (bglpkg-with-tuning db (car arguments) (cadr arguments)
	         ;;; action to execute with the fake package
		 (lambda (db n v)
		    (install-inner db (cons* n v (cddr arguments)))))
	      (bglpkg-with-package db (car arguments)
	         ;;; action to execute with the fake package
		 (lambda (db n v)
		    (install-inner db (cons* n v (cdr arguments)))))))
	 (else
	  (install-inner db arguments)))))

;*---------------------------------------------------------------------*/
;*    install-inner ...                                                */
;*---------------------------------------------------------------------*/
(define (install-inner db arguments)
   (let ((packages (package-list db arguments)))
      (when (and (any pkginfo-fail packages)
		 (not (bglpkg-force-action)))
	 (error 'main
		"Some packages are known to fail, use -F to force action"
		(filter-map (lambda (p)
			       (when (pkginfo-fail p)
				  (pkginfo-name p)))
			    packages)))
      ;; extract and install the packages
      (bglpkg-extract-packages packages)
      (bglpkg-install-package (car arguments) packages)))

;*---------------------------------------------------------------------*/
;*    bglpkg-install-package ...                                       */
;*---------------------------------------------------------------------*/
(define (bglpkg-install-package master::bstring packages::pair-nil)
   (with-trace 3 'bglpkg-install
      (trace-item "master=" master)
      (trace-item "packages=" (map pkginfo-name packages))
      (let* ((dirs (map pkginfo-name packages))
	     (exn (append-map pkginfo-exceptions packages))
	     (sources (append-map package-sources dirs))
	     (macros-id (append-map pkginfo-macros packages))
	     (msources (filter-map (lambda (pkg)
				      (when (string? (pkginfo-source pkg))
					 (let ((path (make-file-path
						      (bglpkg-destdir)
						      (pkginfo-name pkg)
						      (pkginfo-source pkg))))
					    (when (file-exists? path)
					       path))))
				   packages))
	     (macros (append-map (lambda (s)
				    (source-macros s macros-id))
				 msources))
	     (mko (append-map tuning-makefile-objects dirs))
	     (mkr (append-map tuning-makefile-rules dirs)))
	 (bglpkg-install-directories master dirs sources exn macros mko mkr))))

;*---------------------------------------------------------------------*/
;*    Templates ...                                                    */
;*---------------------------------------------------------------------*/
(define *Makefile* (makefile "etc/Makefile"))
(define *make_lib.scm* (makefile "etc/make_lib.scm"))
(define *init* (makefile "etc/init"))

;*---------------------------------------------------------------------*/
;*    bglpkg-install-directories ...                                   */
;*---------------------------------------------------------------------*/
(define (bglpkg-install-directories master packages sources exn macros mko mkr)
   (with-trace 2 'bglpkg-install-directories
      (trace-item "master=" master)
      (trace-item "packages=" packages)
      (trace-item "exceptions=" exn)
      (trace-item "sources=" sources)
      (let* ((modules (filter-map source-package sources))
	     (libname (if (pair? (bglpkg-library))
			  (car (bglpkg-library))
			  master))
	     (version (if (and (pair? (bglpkg-library))
			       (cdr (bglpkg-library)))
			  (cdr (bglpkg-library))
			  (bigloo-config 'release-number))))
	 (install-make_lib libname modules exn macros)
	 (install-init libname version macros)
	 (install-makefile libname version sources modules mko mkr))))
   
;*---------------------------------------------------------------------*/
;*    package-sources ...                                              */
;*---------------------------------------------------------------------*/
(define (package-sources pkg)
   (let ((pkgdir (make-file-name (bglpkg-destdir) pkg)))
      (directory->sources pkgdir)))

;*---------------------------------------------------------------------*/
;*    directory->sources ...                                           */
;*---------------------------------------------------------------------*/
(define (directory->sources dir)
   (filter-map (lambda (f)
		  (when (string-suffix-ci? (pkglib-interface-suffix) f)
		     (make-file-name dir f)))
	       (directory->list dir)))

;*---------------------------------------------------------------------*/
;*    tuning-makefile-objects ...                                      */
;*---------------------------------------------------------------------*/
(define (tuning-makefile-objects dir)
   (tuning-makefiles dir "Makefile.objects"))

;*---------------------------------------------------------------------*/
;*    tuning-makefile-rules ...                                        */
;*---------------------------------------------------------------------*/
(define (tuning-makefile-rules dir)
   (tuning-makefiles dir "Makefile.rules"))

;*---------------------------------------------------------------------*/
;*    tuning-makefiles ...                                             */
;*---------------------------------------------------------------------*/
(define (tuning-makefiles dir makefile)
   (filter-map (lambda (t)
		  (let ((tdir (make-file-path (bglpkg-destdir) dir t)))
		     (when (and (file-exists? tdir) (directory? tdir))
			(let ((mko (make-file-name tdir makefile)))
			   (when (file-exists? mko)
			      mko)))))
	       (bglpkg-tunings)))
   
;*---------------------------------------------------------------------*/
;*    source-package ...                                               */
;*---------------------------------------------------------------------*/
(define (source-package src::bstring)
   (with-trace 4 'source-package
      (trace-item "src=" src)
      (with-input-from-file src
	 (lambda ()
	    (let ((intf (interface-read-interface (current-input-port))))
	       (when intf
		  (multiple-value-bind (name version)
		     (interface-name-version intf)
		     (trace-item "name=" name)
		     name)))))))

;*---------------------------------------------------------------------*/
;*    install-make_lib ...                                             */
;*---------------------------------------------------------------------*/
(define (install-make_lib name modules exceptions macros)
   (with-trace 3 'install-make_lib
      (trace-item "name=" name)
      (trace-item "exceptions=" exceptions)
      (let* ((destdir (bglpkg-destdir))
	     (path (make-file-name destdir "make_lib.scm")))
	 (when (or (not (file-exists? path)) (bglpkg-force-action))
	    (unless (directory? destdir) (make-directories destdir))
	    (with-output-to-file path
	       (lambda ()
		  (sed *make_lib.scm*
		       `((@LIBNAME@ ,name)
			 (@DLOADSYM@ ,name)
			 (@MODULES@ ,(list-to-string modules))
			 (@CLASSES@ ,(list-to-string exceptions))
			 (@MACROS@ ,(list-to-string macros))))))))))

;*---------------------------------------------------------------------*/
;*    install-init ...                                                 */
;*---------------------------------------------------------------------*/
(define (install-init libname version macros)
   (with-trace 3 'install-init
      (trace-item "libname=" libname)
      (trace-item "version=" version)
      (let* ((destdir (bglpkg-destdir))
	     (path (make-file-name destdir (string-append libname ".init")))
	     (before (make-file-path destdir
				     libname
				     "bigloo"
				     (string-append libname "-before.init")))
	     (after (make-file-path destdir
				    libname
				    "bigloo"
				    (string-append libname "-after.init"))))
	 (when (or (not (file-exists? path)) (bglpkg-force-action))
	    (unless (directory? destdir) (make-directories destdir))
	    (with-output-to-file path
	       (lambda ()
		  ;; tune the init file
		  (when (file-exists? before)
		     (with-input-from-file before
			(lambda ()
			   (display (read-string)))))
		  (sed *init*
		       `((@LIBNAME@ ,libname)
			 (@DLOADSYM@ ,libname)
			 (@VERSION@ ,version)
			 (@MACROS@ ,(list-to-string macros))))
		  (when (file-exists? after)
		     (with-input-from-file after
			(lambda ()
			   (display (read-string)))))))))))

;*---------------------------------------------------------------------*/
;*    install-makefile ...                                             */
;*---------------------------------------------------------------------*/
(define (install-makefile libname version sources modules mko mkr)
   (with-trace 3 'install-makefile
      (trace-item "libname=" libname)
      (trace-item "version=" version)
      (trace-item "sources=" sources)
      (let* ((destdir (bglpkg-destdir))
	     (path (make-file-name destdir "Makefile"))
	     (libdir (bigloo-config 'library-directory))
	     (files (map (lambda (f) (relative-file-name f destdir)) sources))
	     (inco (apply string-append 
			  (map (lambda (m)
				  (string-append "include " m "\n"))
			       mko)))
	     (incr (apply string-append 
			  (map (lambda (m)
				  (string-append "include " m "\n"))
			       mkr))))
	 (when (or (not (file-exists? path)) (bglpkg-force-action))
	    (unless (directory? destdir) (make-directories destdir))
	    (with-output-to-file path
	       (lambda ()
		  (sed *Makefile*
		       `((@LIBNAME@ ,libname)
			 (@DLOADSYM@  ,libname)
			 (@VERSION@ ,version)
			 (@SOURCES@ ,(list-to-string files))
			 (@PACKAGES@ ,(list-to-string modules))
			 (@BUILDDIR@ ,(bglpkg-destdir))
			 (@TUNING_MAKEFILE_OBJECTS@ ,inco)
			 (@TUNING_MAKEFILE_RULES@ ,incr)
			 (@LIBDIR@ ,libdir)))))))))

;*---------------------------------------------------------------------*/
;*    list-to-string ...                                               */
;*---------------------------------------------------------------------*/
(define (list-to-string lst)
   (with-output-to-string
      (lambda ()
	 (for-each (lambda (l)
		      (display l)
		      (display " "))
		   lst))))

;*---------------------------------------------------------------------*/
;*    *sed-grammar* ...                                                */
;*---------------------------------------------------------------------*/
(define *sed-grammar*
   (regular-grammar (replacements)
      ((+ (out #\@))
       (display (the-string))
       (ignore))
      ((: #\@ (+ (out #\@ #\Space #\Newline #\Tab)) #\@)
       (let ((k (the-symbol)))
	  (let ((r (assq k replacements)))
	     (if (pair? r)
		 (display (cadr r))
		 (display k)))
	  (ignore)))
      (#\@
       (display #\@)
       (ignore))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    sed ...                                                          */
;*---------------------------------------------------------------------*/
(define (sed string replacements)
   (with-input-from-string string
      (lambda ()
	 (read/rp *sed-grammar* (current-input-port) replacements))))
					  
;*---------------------------------------------------------------------*/
;*    source-macros ...                                                */
;*---------------------------------------------------------------------*/
(define (source-macros::pair-nil source::bstring macros-id::pair-nil)
   (with-trace 4 'source-macros
      (trace-item "source=" source)
      ;; get the interface 
      (with-input-from-file source
	 (lambda ()
	    (let ((exp (port->sexp-list (current-input-port))))
	       (filter-map (lambda (x)
			      (match-case x
				 ((define-macro (?name . ?-) .?)
				  (and (memq name macros-id) x))
				 ((define-expander ?name . ?-)
				  (and (memq name macros-id) x))
				 ((define-syntax ?name . ?-)
				  (and (memq name macros-id) x))
				 (else
				  #f)))
			   exp))))))
