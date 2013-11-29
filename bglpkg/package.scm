;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/package.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  3 10:52:01 2007                          */
;*    Last change :  Fri Nov 29 21:13:30 2013 (serrano)                */
;*    Copyright   :  2007-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Dealing with packages                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_package
   
   (library sqlite
	    pkglib
	    pkgcomp)
   
   (import  bglpkg_param
	    bglpkg_utils)
   
   (export  (class pkginfo
	       (id::bstring read-only)
	       (name::bstring read-only)
	       (source::obj read-only)
	       (version::bstring read-only)
	       (language::pair (default '(r5rs)))
	       (dependencies::pair-nil (default '()))
	       (path::bstring read-only)
	       (url::bstring read-only)
	       (md5::bstring read-only)
	       (tunings::pair-nil (default '()))
	       (macros::pair-nil (default '()))
	       (exceptions::pair-nil (default '()))
	       (fail::bool (default #f)))
	    
	    (class tuninfo
	       (host::bstring read-only)
	       (path::bstring read-only)
	       (url::bstring read-only)
	       (md5::bstring read-only))
	    
	    (package-list::pair-nil ::%sqlite ::pair-nil)
	    (directory->package::pkginfo ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    *packages* ...                                                   */
;*---------------------------------------------------------------------*/
(define *packages* (make-hashtable))

;*---------------------------------------------------------------------*/
;*    package-list ...                                                 */
;*    -------------------------------------------------------------    */
;*    Return the list of packages (including the recursive             */
;*    dependencies).                                                   */
;*---------------------------------------------------------------------*/
(define (package-list db arguments)
   (with-trace 2 'package-list
      (trace-item "arguments=" arguments)
      (let loop ((args arguments))
	 (if (null? args)
	     (begin
		;; remove the unwanted packages
		(for-each (lambda (x)
			     (hashtable-remove! *packages* x))
			  (bglpkg-excludes))
		(trace-item "packages="
			    (map pkginfo-id (hashtable->list *packages*)))
		;; return the whole list and the master package
		(hashtable->list *packages*))
	     (let* ((name (car args))
		    (version (and (pair? (cdr args))
				  (package-version? (cadr args))
				  (cadr args)))
		    (rest (if version (cddr args) (cdr args))))
		(db-store-package! db name version)
		(loop rest))))))

;*---------------------------------------------------------------------*/
;*    directory->package ...                                           */
;*---------------------------------------------------------------------*/
(define (directory->package name dir)
   (instantiate::pkginfo
      (id name)
      (name dir)
      (version "v2.0.0")
      (source #f)
      (path "")
      (url "")
      (md5 "")))

;*---------------------------------------------------------------------*/
;*    db-store-package! ...                                            */
;*---------------------------------------------------------------------*/
(define (db-store-package! db name version)
   (with-trace 2 'db-store-package!
      (trace-item "name=" name)
      (trace-item "version=" version)
      (let ((old (hashtable-get *packages* name)))
	 (trace-item "old=" old)
	 (if old
	     (with-access::pkginfo old ((vold version))
		(if (or (not version) (string=? version vold))
		    old
		    (error 'db-store-package!
			   "A different version has already been selected"
			   (format "~a v~a" name vold))))
	     (let* ((mkpkg (lambda (id version path url md5 language source)
			      (instantiate::pkginfo
				 (id id)
				 (name name)
				 (version version)
				 (source source)
				 (language (if (string=? language "")
					       '(r5rs)
					       (with-input-from-string language
						  read)))
				 (path path)
				 (url url)
				 (md5 md5))))
		    (pkg (if version
			     (sqlite-eval db
				mkpkg
				"SELECT id, version, path, url, md5, language, source
                                   FROM package, port
                                   WHERE (name=~q) and (version=~q)"
				name version)
			     (sqlite-eval db
				mkpkg
				"SELECT id, version, path, url, md5, language, source
                                  FROM package p
                                  WHERE (p.name=~q)
                                    AND (version IN (SELECT MAX(version)
                                                      FROM package
                                                      WHERE (p.name=name)))"
				name))))
		(unless (isa? pkg pkginfo)
		   (error 'get-package
			  "Cannot find package"
			  (if version (format "~a v~a" name version) name)))
		(hashtable-put! *packages* name pkg)
		(with-access::pkginfo pkg (id dependencies tunings
					      exceptions macros
					      fail language)
		   (trace-item "pkg=" id)
		   ;; dependencies
		   (set! dependencies
			 (append (language-dependencies db language)
				 (package-dependencies db id)))
		   ;; tunings
		   (when (pair? (bglpkg-tunings))
		      (set! tunings
			    (filter-map (lambda (tuning)
					   (sqlite-eval db
					      (lambda (path url md5)
						 (instantiate::tuninfo
						    (host tuning)
						    (path path)
						    (url url)
						    (md5 md5)))
					      "SELECT path, url, md5
                                                 FROM tuning
                                                 WHERE (package=~q)
                                                       AND (host=~q)"
					      id tuning))
					(bglpkg-tunings))))
		   ;; fail
		   (sqlite-eval db
		      (lambda (host) (set! fail #t))
		      "SELECT host
                        FROM port
                        WHERE (package=~q)
                          AND (status='failure') AND (host='bigloo')"
		      id)
		   ;; exceptions
		   (set! exceptions (package-exceptions db id))
		   ;; exceptions
		   (set! macros (package-macros db id)))
		pkg)))))
		 
;*---------------------------------------------------------------------*/
;*    package-dependencies ...                                         */
;*---------------------------------------------------------------------*/
(define (package-dependencies db id)
   (with-trace 3 'package-dependencies
      (trace-item "id=" id)
      (if (bglpkg-recursive)
	  (sqlite-map db
	     (lambda (name version)
		(db-store-package! db
				   name
				   (if (string=? version "*") #f version)))
	     "SELECT name, version FROM depend WHERE (package=~q)" id)
	  '())))

;*---------------------------------------------------------------------*/
;*    language-dependencies ...                                        */
;*---------------------------------------------------------------------*/
(define (language-dependencies db language)
   (with-trace 3 'package-dependencies
      (trace-item "language=" language)
      (let ((l (car language)))
	 (if (and (bglpkg-recursive)
		  (not (memq l (bglpkg-builtin-languages))))
	     (let ((dep (assq l (bglpkg-language-dependencies)))
		   (r (map (lambda (s)
			      (symbol-append (pkgcomp-language-mark) l '- s))
			   (cdr language))))
		(map (lambda (name)
			(sqlite-map db
			   (lambda (name version)
			      (db-store-package! db
						 name
						 (if (string=? version "*")
						     #f
						     version)))
			   "SELECT name, version
                            FROM package WHERE (name='~s')" name))
		     (if (pair? dep)
			 (append (cadr dep) r)
			 (cons (symbol-append (pkgcomp-language-mark) l) r))))
	     '()))))
   
;*---------------------------------------------------------------------*/
;*    package-exceptions ...                                           */
;*---------------------------------------------------------------------*/
(define (package-exceptions db id)
   (sqlite-map db
      (lambda (x)
	 (let ((proto (with-input-from-string x read)))
	    (match-case proto
	       ((exception (and (? symbol?) ?exc) . ?-)
		`(class ,exc))
	       ((exception (?exc ?parent) . ?-)
		`(class ,exc))
	       (else
		(error 'package-exceptions "Illegal exception" proto)))))
      "SELECT proto FROM exception WHERE package=~q" id))
   
;*---------------------------------------------------------------------*/
;*    package-macros ...                                               */
;*---------------------------------------------------------------------*/
(define (package-macros db id)
   (sqlite-map db
      string->symbol
      "SELECT name FROM macro WHERE package=~q" id))
   
