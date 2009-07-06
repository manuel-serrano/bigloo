;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/testing.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  3 15:30:35 2007                          */
;*    Last change :  Sun Jun 17 18:26:29 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Package testing                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_testing
   
   (library sqlite
	    pkglib)
   
   (import  bglpkg_param
	    bglpkg_utils
	    bglpkg_install
	    bglpkg_extract
	    bglpkg_package
	    bglpkg_action)
   
   (export  (bglpkg-test ::%sqlite ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    bglpkg-test ...                                                  */
;*---------------------------------------------------------------------*/
(define (bglpkg-test db arguments)
   (when (string=? (bglpkg-destdir) ".")
      (unless (yes-or-no? "Test in current directory? [y/n] " arguments)
	 (print "aborting...")
	 (exit 0)))
   (when (directory? (bglpkg-destdir))
      (bglpkg-delete-tmp-files-set! #f))
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
                             (inner-test db (cons* n v (cddr arguments))))))
                    (bglpkg-with-package db pkg
                       ;;; action to execute with the fake package
                       (lambda (db n v)
                          (inner-test db (cons* n v (cdr arguments))))))
                (when (bglpkg-delete-tmp-files)
                   (delete-file pkg)))))
         ((package-filename? (car arguments))
          (if (file-exists? (car arguments))
              (if (and (pair? (cdr arguments))
                       (package-tuning? (cadr arguments)))
                  (bglpkg-with-tuning db (car arguments) (cadr arguments)
                     ;;; action to execute with the fake package
                     (lambda (db n v)
                        (inner-test db (cons* n v (cddr arguments)))))
                  (bglpkg-with-package db (car arguments)
                     ;;; action to execute with the fake package
                     (lambda (db n v)
                        (inner-test db (cons* n v (cdr arguments))))))
              (error 'bglpkg "File not found" (car arguments))))
         (else
          (inner-test db arguments)))))

;*---------------------------------------------------------------------*/
;*    inner-test ...                                                   */
;*---------------------------------------------------------------------*/
(define (inner-test db arguments)
   (let* ((master (car arguments))
	  (args (cons* "recette" arguments))
	  (packages (package-list db args)))
      (bglpkg-extract-packages packages)
      (let* ((dir (make-file-path (bglpkg-destdir) master "test"))
	     (tpath (make-file-name dir (string-append
					 master "-test."
					 (pkglib-interface-suffix)))))
	 (if (file-exists? tpath)
	     (begin
		(cp-r dir (bglpkg-destdir))
		(let* ((pkg (directory->package
			     (string-append master "-test") "test"))
		       (packages (cons pkg packages)))
		   (bglpkg-install-package master packages)
		   (unwind-protect
		      (test master)
		      (when (bglpkg-delete-tmp-files)
			 (rm-rf (bglpkg-destdir))))))
	     (warning 'bglpkg-test
		      "No tests defined for package -- "
		      master)))))

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test package)
   (let* ((path (make-file-name (bglpkg-destdir) "main.scm"))
	  (make (bglpkg-test-make-command))
	  (mkfs (make-file-path (bglpkg-destdir)
				package
				"bigloo"
				"Makefile.test"))
	  (cmp (if (file-exists? mkfs)
		   (format "make -f ~a" mkfs)
		   (format "~a main.scm" (bglpkg-test-compile-command))))
	 (exec "a.out"))
      (generate-test-main package path)
      ;; compile the library
      (let ((c (format "(cd ~a && ~a && ~a && ./~a)"
		       (bglpkg-destdir)
		       make cmp exec)))
	 (when (>fx (pkglib-verbose) 0)
	    (print (pkglib-color 'command c)))
	 (system c))))

;*---------------------------------------------------------------------*/
;*    generate-test-main ...                                           */
;*---------------------------------------------------------------------*/
(define (generate-test-main package path)
   (when (or (not (file-exists? path)) (bglpkg-force-action))
      (with-output-to-file path
	 (lambda ()
	    (print "(module main (library pkgcomp " package ") (main main))")
	    (print "(define (main x) (run (cdr x)))")))))
      
   
