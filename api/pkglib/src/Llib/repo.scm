;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkglib/src/Llib/repo.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 14 14:18:12 2006                          */
;*    Last change :  Tue May 13 15:47:12 2008 (serrano)                */
;*    Copyright   :  2006-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Facilities for handling a pkglib repository                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_repo
   
   (library sqlite)
   
   (import  __pkglib_param
	    __pkglib_misc
	    __pkglib_database
	    __pkglib_package
	    __pkglib_interface)
   
   (export  (repo-create-database::%sqlite ::bstring ::bstring)
	    (repo-populate-database! ::%sqlite ::bstring)

	    (repo-add-package! ::%sqlite ::bstring)
	    (repo-add-tuning! ::%sqlite ::bstring)
	    (repo-remove-package-version! ::%sqlite ::bstring ::bstring ::bstring)
	    (repo-remove-tuning! ::%sqlite ::bstring ::bstring ::bstring ::bstring)
	    (repo-remove-package! ::%sqlite ::bstring ::bstring)

	    (make-repo-package-path::bstring ::bstring ::bstring ::obj)
	    (make-repo-tuning-path::bstring ::bstring ::bstring ::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    repo-create-database ...                                         */
;*    -------------------------------------------------------------    */
;*    Create a database from a repository on the disk.                 */
;*---------------------------------------------------------------------*/
(define (repo-create-database repodir::bstring dbpath::bstring)
   (when (>fx (pkglib-verbose) 0)
      (print "Creating database:")
      (print " repo: " (pkglib-color 'arg0 repodir))
      (print " db  : " (pkglib-color 'arg1 dbpath)))
   (with-trace 2 'repo-create-database
      (trace-item "dbpath=" dbpath)
      (repo-populate-database! (make-repo-db dbpath) repodir)))

;*---------------------------------------------------------------------*/
;*    repo-populate-database! ...                                      */
;*---------------------------------------------------------------------*/
(define (repo-populate-database! db repodir::bstring)
   (with-trace 2 'repo-populate-database!
      (trace-item "repodir=" repodir)
      (let ((pkgdirs (directory->list repodir)))
	 (for-each (lambda (f)
		      (let ((pdir (make-file-name repodir f)))
			 (when (directory? pdir)
			    (populate-package-directory! db pdir))))
		   pkgdirs)
	 db)))

;*---------------------------------------------------------------------*/
;*    populate-package-directory! ...                                  */
;*    -------------------------------------------------------------    */
;*    Populate from a package directory. This directory contains       */
;*    subversion directories.                                          */
;*---------------------------------------------------------------------*/
(define (populate-package-directory! db pkgdir)
   (for-each (lambda (version)
		(let ((vdir (make-file-name pkgdir version)))
		   (when (directory? vdir)
		      (populate-package-version-directory! db vdir))))
	     (directory->list pkgdir)))

;*---------------------------------------------------------------------*/
;*    populate-package-version-directory! ...                          */
;*---------------------------------------------------------------------*/
(define (populate-package-version-directory! db vdir)
   (for-each (lambda (file)
		(when (package-filename? file)
		   (if (package-tuning? file)
		       (populate-tuning! db vdir file)
		       (populate-package! db vdir file))))
	     (directory->list vdir)))

;*---------------------------------------------------------------------*/
;*    populate-tuning! ...                                             */
;*---------------------------------------------------------------------*/
(define (populate-tuning! db dir file)
   (multiple-value-bind (name version release host)
      (package-name-parse file)
      (let ((path (make-file-name dir file)))
	 (db-add-tuning! db name
			 :version version
			 :release release
			 :host host
			 :path path
			 :md5 (md5sum-file path)))))

;*---------------------------------------------------------------------*/
;*    populate-package! ...                                            */
;*    -------------------------------------------------------------    */
;*    Read a local package and populate the DB with the extracted      */
;*    information.                                                     */
;*---------------------------------------------------------------------*/
(define (populate-package! db dir file)
   (multiple-value-bind (name version release _)
      (package-name-parse file)
      (let* ((path (make-file-name dir file))
	     (intf (package-extract-interface path name))
	     (info (package-extract-meta path name))
	     (provide (interface-export intf))
	     (language (interface-language intf))
	     (meta (if (list? info)
		       (append info (interface-meta intf))
		       (interface-meta intf)))
	     (descr (car (assq/default 'description meta '(""))))
	     (date (car (assq/default 'date meta '(""))))
	     (homepage (car (assq/default 'homepage meta '(""))))
	     (authors (car (assq/default 'author meta '(""))))
	     (categories (assq/default 'keywords meta '(misc)))
	     (note (car (assq/default 'note meta '(""))))
	     (failures (assq* 'failures meta '()))
	     (suf (car (assq/default 'suffix meta '("scm"))))
	     (custom (assq/default 'source meta #f))
	     (source (if (pair? custom)
			 (car custom)
			 (string-append name "." suf)))
	     (imports (append (interface-import intf) (interface-from intf)))
	     (companions (package-companions path name))
	     (deps (filter (lambda (i) (not (memq i companions))) imports)))
	 (db-add-package! db name
			  :version version
			  :release release
			  :language language
			  :source source
			  :path path
			  :md5 (md5sum-file path)
			  :category (car categories)
			  :description descr
			  :note note
			  :author authors
			  :date date
			  :homepage homepage
			  :keywords categories
			  :dependencies deps
			  :provides provide
			  :failures failures))))

;*---------------------------------------------------------------------*/
;*    repo-add-package! ...                                            */
;*---------------------------------------------------------------------*/
(define (repo-add-package! db path)
   (populate-package! db (dirname path) (basename path)))

;*---------------------------------------------------------------------*/
;*    repo-add-tuning! ...                                             */
;*---------------------------------------------------------------------*/
(define (repo-add-tuning! db path)
   (populate-tuning! db (dirname path) (basename path)))

;*---------------------------------------------------------------------*/
;*    repo-remove-tuning! ...                                          */
;*---------------------------------------------------------------------*/
(define (repo-remove-tuning! db repodir package version tuning)
   (let* ((release (db-package-release db package version))
	  (file (make-package-name package version release tuning))
	  (path (make-file-path repodir package version file)))
      (db-remove-tuning! db package version tuning)
      (when (file-exists? path)
	 (delete-file path))))

;*---------------------------------------------------------------------*/
;*    repo-remove-package-version! ...                                 */
;*---------------------------------------------------------------------*/
(define (repo-remove-package-version! db repodir package version)
   (let ((dir (make-file-path repodir package version)))
      (db-remove-package-version! db package version)
      (when (directory? dir)
	 (rm-rf dir))))

;*---------------------------------------------------------------------*/
;*    repo-remove-package! ...                                         */
;*---------------------------------------------------------------------*/
(define (repo-remove-package! db repodir package)
   (let ((dir (make-file-name repodir package)))
      (db-remove-package! db package)
      (when (directory? dir)
	 (rm-rf dir))))

;*---------------------------------------------------------------------*/
;*    rm-rf ...                                                        */
;*---------------------------------------------------------------------*/
(define (rm-rf path)
   (when (file-exists? path)
      (if (directory? path)
	  (let ((files (directory->list path)))
	     (for-each (lambda (f) (rm-rf (make-file-name path f))) files)
	     (delete-directory path))
	  (delete-file path))))

;*---------------------------------------------------------------------*/
;*    make-repo-package-path ...                                       */
;*---------------------------------------------------------------------*/
(define (make-repo-package-path package version rel)
   (let ((pname (make-package-name package version rel #f)))
      (make-file-path package version pname)))

;*---------------------------------------------------------------------*/
;*    make-repo-tuning-path ...                                        */
;*---------------------------------------------------------------------*/
(define (make-repo-tuning-path package host version rel)
   (let ((pname (make-package-name package version rel host)))
      (make-file-path package version pname)))

