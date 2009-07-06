;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/extract.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 12:52:06 2006                          */
;*    Last change :  Tue Jun  5 13:21:16 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Extract the tarballs and packages                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_extract

   (library sqlite
	    pkglib
	    web)
   
   (import  bglpkg_param
	    bglpkg_tuning
	    bglpkg_package
	    bglpkg_utils)
   
   (export  (bglpkg-extract ::%sqlite ::pair-nil)
	    (bglpkg-extract-packages ::pair-nil)
	    (bglpkg-download ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    bglpkg-extract ...                                               */
;*---------------------------------------------------------------------*/
(define (bglpkg-extract db arguments)
   (let ((packages (package-list db arguments)))
      (bglpkg-extract-packages packages)))
   
;*---------------------------------------------------------------------*/
;*    extract ...                                                      */
;*---------------------------------------------------------------------*/
(define (bglpkg-extract-packages packages)
   (with-trace 2 'extract
      (trace-item "packages=" (map pkginfo-id packages))
      (for-each extract-package packages)))

;*---------------------------------------------------------------------*/
;*    bglpkg-download ...                                              */
;*---------------------------------------------------------------------*/
(define (bglpkg-download packages)
   (with-trace 2 'bglpkg-download
      (trace-item "packages=" (map pkginfo-id packages))
      (for-each download-pkginfo packages)))

;*---------------------------------------------------------------------*/
;*    extract-package ...                                              */
;*    -------------------------------------------------------------    */
;*    Find all the tarballs associated with a package, download them   */
;*    if needed, and  then install them. Proceed recursively for its   */
;*    dependencies.                                                    */
;*---------------------------------------------------------------------*/
(define (extract-package pkg::pkginfo)
   (with-trace 2 'extract-package
      (trace-item "pkg=" pkg)
      (with-access::pkginfo pkg (name version path url md5 tunings language)
	 ;; verbose
	 (when (>=fx (pkglib-verbose) 1)
	    (print (pkglib-color 'message name)
		   (pkglib-color 'message " v")
		   (pkglib-color 'message version)
		   ":"))
	 (when (or (not (directory? (make-file-name (bglpkg-destdir) name)))
		   (bglpkg-force-download)
		   (bglpkg-force-action))
	    ;; install the plain version
	    (download path url md5)
	    (let ((tmpdir (make-file-name (bglpkg-tmp-directory)
					  (format "~a-~a"
						  name (current-seconds)))))
	       (unwind-protect
		  (begin
		     (make-directories tmpdir)
		     (extract-tarball tmpdir name version #f path)
		     ;; extract the documentation
		     (extract-doc tmpdir name version)
		     ;; extract the plugin
		     (extract-plugin tmpdir name version path)
		     ;; preprocess the source code
		     (preprocess-package tmpdir name version language)
		     ;; extract the tunings
		     (trace-item "tunings=" tunings)
		     (for-each (lambda (tuning)
				  (with-access::tuninfo tuning (host path url md5)
				     (download path url md5)
				     (extract-tarball tmpdir name version #t
						      path)
				     (tune-package tmpdir name version host language)))
			       tunings)
		     (if (bglpkg-flat)
			 (cp* (make-file-name tmpdir name) (bglpkg-destdir))
			 (cp-r (make-file-name tmpdir name) (bglpkg-destdir))))
		  (when (directory? tmpdir) (rm-rf tmpdir))))))))

;*---------------------------------------------------------------------*/
;*    download-pkinfo ...                                              */
;*---------------------------------------------------------------------*/
(define (download-pkginfo pkg::pkginfo)
   (with-trace 2 'download-package
      (trace-item "pkg=" pkg)
      (with-access::pkginfo pkg (name version path url md5 tunings language)
	 ;; verbose
	 (when (>=fx (pkglib-verbose) 1)
	    (print (pkglib-color 'message name)
		   (pkglib-color 'message " v")
		   (pkglib-color 'message version)
		   ":"))
	 ;; install the plain version
	 (download path url md5)
	 (let ((tmpdir (make-file-name (bglpkg-tmp-directory)
				       (format "~a-~a"
					       name (current-seconds)))))
	    (unwind-protect
	       (begin
		  (make-directories tmpdir)
		  (extract-tarball tmpdir name version #f path)
		  ;; extract the documentation
		  (extract-doc tmpdir name version)
		  ;; extract the tunings
		  (trace-item "tunings=" tunings)
		  (for-each (lambda (tuning)
			       (with-access::tuninfo tuning (host path url md5)
				  (download path url md5)))
			    tunings))
	       (when (directory? tmpdir) (rm-rf tmpdir)))))))

;*---------------------------------------------------------------------*/
;*    extract-tarball ...                                              */
;*---------------------------------------------------------------------*/
(define (extract-tarball destdir pkg version istuning ball)
   (let ((path (make-file-name destdir pkg)))
      (if (and (file-exists? path) (not istuning))
	  (if (bglpkg-force-action)
	      (begin
		 (rm-rf path)
		 (tarball-untar-gz ball destdir))
	      (warning 'extract-tarballs
		       "Directory exists, skipping -- "
		       path))
	  (tarball-untar-gz ball destdir))
      (if (directory? path)
	  (with-output-to-file (make-file-name path ".bglpkg")
	     (lambda ()
		(print `(,pkg ,version :tunings ,(bglpkg-tunings)))))
	  (error 'extract-tarball "Fail to create directory" path))))

;*---------------------------------------------------------------------*/
;*    extract-plugin ...                                               */
;*---------------------------------------------------------------------*/
(define (extract-plugin tmpdir name version path)
   (let ((meta (extract-meta tmpdir name)))
      ;; try in sequence all the plugins
      (let loop ((plugins (bglpkg-plugins)))
	 (when (pair? plugins)
	    (let* ((plugin (car plugins))
		   (cell (assq (car plugin) meta)))
	       (if (pair? cell)
		   ((cdr plugin) tmpdir name version path (cdr cell))
		   (loop (cdr plugins))))))))

;*---------------------------------------------------------------------*/
;*    extract-meta ...                                                 */
;*---------------------------------------------------------------------*/
(define (extract-meta tmpdir name)
   (append (read-interface-meta tmpdir name)
	   (read-etc-meta tmpdir name)))

;*---------------------------------------------------------------------*/
;*    read-etc-meta ...                                                */
;*---------------------------------------------------------------------*/
(define (read-etc-meta tmpdir name)
   (let ((info (make-file-path tmpdir name "etc" "meta")))
      (if (file-exists? info)
	  (with-input-from-file info read)
	  '())))

;*---------------------------------------------------------------------*/
;*    read-interface-meta ...                                          */
;*---------------------------------------------------------------------*/
(define (read-interface-meta tmpdir name)      
   (let ((spi (make-file-path tmpdir name (string-append name ".spi"))))
      (unless (file-exists? spi)
	 (error 'interface-meta "Cannot find .spi file" spi))
      (with-input-from-file spi
	 (lambda ()
	    (let ((intf (interface-read-interface (current-input-port))))
	       (interface-meta intf))))))
   
;*---------------------------------------------------------------------*/
;*    preprocess-package ...                                           */
;*---------------------------------------------------------------------*/
(define (preprocess-package tmpdir name version lang)
   (let ((preprocessor (assq (car lang) (bglpkg-language-preprocessor))))
      (when (pair? preprocessor)
	 (let* ((proc (cdr preprocessor))
		(suf (assq/default (car lang) (bglpkg-language-suffix) ".scm"))
		(scmname (string-append name suf))
		(spiname (string-append name ".spi"))
		(dir (make-file-name tmpdir name))
		(scmfile (make-file-name dir scmname))
		(spifile (make-file-name dir spiname)))
	    (for-each (lambda (f)
			 (if (string-suffix-ci? suf f)
			     (let* ((fq (make-file-name dir f)))
				(unless (directory? fq)
				   (let ((str (read-file fq)))
				      (with-output-to-file fq
					 (lambda ()
					    (display (proc str)))))))))
		      (directory->list dir))
	    (when (file-exists? spifile)
	       (let ((spistr (read-file spifile)))
		  (with-output-to-file spifile
		     (lambda ()
			(display (proc spistr))))))))))

;*---------------------------------------------------------------------*/
;*    extract-doc ...                                                  */
;*---------------------------------------------------------------------*/
(define (extract-doc tmpdir name version)
   (let ((docdir (make-file-path tmpdir name "doc")))
      (when (directory? docdir)
	 (let ((base (make-file-path (bglpkg-doc-directory) 
				     name
				     (string-append "v" version))))
	    (make-directories base)
	    (cp-r docdir base)))))
