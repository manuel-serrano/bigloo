;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/param.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 13 09:36:33 2006                          */
;*    Last change :  Fri Nov 29 21:11:07 2013 (serrano)                */
;*    Copyright   :  2006-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bglpkg-bigloo params                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_param
   
   (include "param.sch")
   
   (import  bglpkg_configure)
   
   (export  (bglpkg-rc-directory::bstring)
	    (bglpkg-rc-directory-set! ::bstring)
	    
	    (bglpkg-default-rc-directory::bstring)
	    
	    (bglpkg-rc-file::bstring)
	    (bglpkg-rc-file-set! ::bstring)
	    
	    (bglpkg-db-file::bstring)
	    (bglpkg-db-file-set! ::bstring)
	    
	    (bglpkg-etc-directory::bstring)
	    (bglpkg-etc-directory-set! ::bstring)
	    
	    (bglpkg-tmp-directory::bstring)
	    (bglpkg-tmp-directory-set! ::bstring)
	    
	    (bglpkg-cache-directory::bstring)
	    (bglpkg-cache-directory-set! ::bstring)
	    
	    (bglpkg-pkg-directory::bstring)
	    (bglpkg-pkg-directory-set! ::bstring)
	    
	    (bglpkg-doc-directory::bstring)
	    (bglpkg-doc-directory-set! ::bstring)
	    
	    (bglpkg-sync-urls::pair-nil)
	    (bglpkg-sync-urls-set! ::pair-nil)
	    
	    (bglpkg-force-action::bool)
	    (bglpkg-force-action-set! ::bool)
	    
	    (bglpkg-force-download::bool)
	    (bglpkg-force-download-set! ::bool)
	    
	    (bglpkg-destdir::bstring)
	    (bglpkg-destdir-set! ::bstring)
	    
	    (bglpkg-tunings::pair-nil)
	    (bglpkg-tunings-set! ::pair-nil)
	    
	    (bglpkg-recursive::bool)
	    (bglpkg-recursive-set! ::bool)
	    
	    (bglpkg-delete-tmp-files::bool)
	    (bglpkg-delete-tmp-files-set! ::bool)
	    
	    (bglpkg-installation::symbol)
	    (bglpkg-installation-set! ::symbol)
	    
	    (bglpkg-excludes::pair-nil)
	    (bglpkg-excludes-set! ::pair-nil)
	    
	    (bglpkg-library::obj)
	    (bglpkg-library-set! ::obj)
	    
	    (bglpkg-test-make-command::bstring)
	    (bglpkg-test-make-command-set! ::bstring)
	    
	    (bglpkg-test-compile-command::bstring)
	    (bglpkg-test-compile-command-set! ::bstring)

	    (bglpkg-builtin-languages::pair-nil)
	    (bglpkg-builtin-languages-set! ::pair-nil)

	    (bglpkg-language-dependencies::pair-nil)
	    (bglpkg-language-dependencies-set! ::pair-nil)

	    (bglpkg-language-preprocessor::pair-nil)
	    (bglpkg-language-preprocessor-set! ::pair-nil)

	    (bglpkg-language-suffix::pair-nil)
	    (bglpkg-language-suffix-set! ::pair-nil)

	    (bglpkg-plugins::pair-nil)
	    (bglpkg-plugins-set! ::pair-nil)

	    (bglpkg-search-path::pair-nil)
	    (bglpkg-search-path-set! ::pair-nil)

	    (bglpkg-shell::bstring)
	    (bglpkg-shell-set! ::bstring)

	    (bglpkg-noconfirm::bool)
	    (bglpkg-noconfirm-set! ::bool)
	    
	    (bglpkg-flat::bool)
	    (bglpkg-flat-set! ::bool)

	    (params-setting))
   
   (import bglpkg_configure))

;*---------------------------------------------------------------------*/
;*    bglpkg-suffix ...                                                */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-suffixes
   '("snow" "bgl" "scm")
   (lambda (v)
      (unless (and (list? v) (every string? v))
	 (error 'bglpkg-suffixes-set! "Illegal suffixes" v))
      v))

;*---------------------------------------------------------------------*/
;*    user-tmp ...                                                     */
;*---------------------------------------------------------------------*/
(define user-etc 0)
(define user-tmp 0)
(define user-cache 0)
(define user-pkg 0)
(define user-doc 0)
(define user-afile 0)

;*---------------------------------------------------------------------*/
;*    bglpkg-rc-directory ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-rc-directory
   (let ((home (or (getenv "HOME") (string (file-separator))))
	 (host (hostname))
	 (base "bglpkg"))
      (let loop ((host (if (not (string? host)) (getenv "HOST") host)))
	 (if (string? host)
	     (let ((home/host (make-file-path home ".config"
					      (string-append base "." host))))
		(if (and (file-exists? home/host) (directory? home/host))
		    home/host
		    (if (string=? (suffix host) "")
			(let ((home/def (make-file-path home ".config" base)))
			   (cond
			      ((and (file-exists? home/def)
				    (directory? home/def))
			       home/def)
			      (else
			       home)))
			(loop (prefix host))))))))
   (lambda (v)
      (unless (<=fx user-etc 1)
	 (bglpkg-etc-directory-set! (make-file-name v "etc")))
      (unless (<=fx user-tmp 1)
	 (bglpkg-tmp-directory-set! (make-file-name v "tmp")))
      (unless (<=fx user-cache 1)
	 (bglpkg-cache-directory-set! (make-file-name v "cache")))
      (unless (<=fx user-pkg 1)
	 (bglpkg-pkg-directory-set! (make-file-name v "pkg")))
      (unless (<=fx user-doc 1)
	 (bglpkg-doc-directory-set! (make-file-name v "doc")))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-default-rc-directory ...                                  */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-default-rc-directory
   (let ((home (or (getenv "HOME") (string (file-separator)))))
      (make-file-path  home ".config" "bglpkg")))
   
;*---------------------------------------------------------------------*/
;*    bglpkg-rc-file ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-rc-file
   "bglpkgrc.scm")

;*---------------------------------------------------------------------*/
;*    bglpkg-db-file ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-db-file
   "bglpkg.db")

;*---------------------------------------------------------------------*/
;*    bglpkg-etc-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-etc-directory
   (make-file-name (bglpkg-rc-directory) "etc")
   (lambda (v)
      (set! user-etc (+fx user-etc 1))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-tmp-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-tmp-directory
   (make-file-name (bglpkg-rc-directory) "tmp")
   (lambda (v)
      (set! user-tmp (+fx user-tmp 1))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-cache-directory ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-cache-directory
   (make-file-name (bglpkg-rc-directory) "cache")
   (lambda (v)
      (set! user-cache (+fx user-cache 1))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-pkg-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-pkg-directory
   (make-file-name (bglpkg-rc-directory) "pkg")
   (lambda (v)
      (set! user-pkg (+fx user-pkg 1))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-doc-directory ...                                         */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-doc-directory
   (make-file-name (bglpkg-rc-directory) "doc")
   (lambda (v)
      (set! user-doc (+fx user-doc 1))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-sync-urls ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-sync-urls
   '("http://hop.inria.fr/hop/scmpkg/sync")
   (lambda (v)
      (unless (every string? v)
	 (error 'bglpkg-sync-url "Illegal string list" v))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-verbose ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-verbose
   0)

;*---------------------------------------------------------------------*/
;*    bglpkg-force-action ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-force-action
   #f)

;*---------------------------------------------------------------------*/
;*    bglpkg-force-download ...                                        */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-force-download
   #f)

;*---------------------------------------------------------------------*/
;*    bglpkg-destdir ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-destdir
   "."
   (lambda (v)
      (unless (string? v)
	 (error 'bglpkg-destdir-set! "Illegal directory" v))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-tunings ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-tunings
   '("bigloo")
   (lambda (v)
      (unless (every string? v)
	 (error 'bglpkg-tuning-set! "Illegal tuning list" v))
      v))

;*---------------------------------------------------------------------*/
;*    bglpkg-recursive ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-recursive
   #t)

;*---------------------------------------------------------------------*/
;*    bglpkg-delete-tmp-files ...                                      */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-delete-tmp-files
   #t)

;*---------------------------------------------------------------------*/
;*    bglpkg-installation ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-installation
   'none)

;*---------------------------------------------------------------------*/
;*    bglpkg-excludes ...                                              */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-excludes
   '())

;*---------------------------------------------------------------------*/
;*    bglpkg-library ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-library
   #unspecified)

;*---------------------------------------------------------------------*/
;*    bglpkg-test-make-command ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-test-make-command
   "make EFLAGS=-g")

;*---------------------------------------------------------------------*/
;*    bglpkg-test-compile-command ...                                  */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-test-compile-command
   "bigloo -static-all-bigloo -g -scmpkg")

;*---------------------------------------------------------------------*/
;*    bglpkg-builtin-languages ...                                     */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-builtin-languages
   '(r5rs bigloo))

;*---------------------------------------------------------------------*/
;*    bglpkg-language-dependencies ...                                 */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-language-dependencies
   '())

;*---------------------------------------------------------------------*/
;*    bglpkg-language-preprocessor ...                                 */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-language-preprocessor
   '()
   (lambda (v)
      (if (and (list? v)
	       (every (lambda (e)
			 (and (pair? e)
			      (symbol? (car e))
			      (procedure? (cdr e))))
		       v))
	  v
	  (error 'bglpkg-language-preprocessor "Illegal preprocessors" v))))

;*---------------------------------------------------------------------*/
;*    bglpkg-language-suffix ...                                       */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-language-suffix
   '((mzscheme . ".ss")
     (stklos . ".stk")
     (hop . ".hop"))
   (lambda (v)
      (if (and (list? v)
	       (every (lambda (e)
			 (and (pair? e)
			      (symbol? (car e))
			      (string? (cdr e))))
		  v))
	  v
	  (error 'bglpkg-language-suffix "Illegal suffix" v))))

;*---------------------------------------------------------------------*/
;*    bglpkg-plugins ...                                               */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-plugins
   '())

;*---------------------------------------------------------------------*/
;*    bglpkg-search-path ...                                           */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-search-path
   '(".")
   (lambda (v)
      (if (not (and (list? v) (every string? v)))
	  (error 'bglpkg-search-path-set! "Illegal path" v)
	  v)))

;*---------------------------------------------------------------------*/
;*    bglpkg-shell ...                                                 */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-shell
   "sh")

;*---------------------------------------------------------------------*/
;*    bglpkg-noconfirm ...                                             */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-noconfirm
   #f)

;*---------------------------------------------------------------------*/
;*    bglpkg-flat ...                                                  */
;*---------------------------------------------------------------------*/
(define-parameter bglpkg-flat
   #f)

;*---------------------------------------------------------------------*/
;*    params-setting ...                                               */
;*---------------------------------------------------------------------*/
(define (params-setting)
   (print-parameters bglpkg-sync-urls
		     bglpkg-rc-directory
		     bglpkg-rc-file
		     bglpkg-pkg-directory
		     bglpkg-tunings
		     bglpkg-search-path))


