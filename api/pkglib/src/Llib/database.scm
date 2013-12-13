;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkglib/src/Llib/database.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 14 14:18:12 2006                          */
;*    Last change :  Fri Dec 13 12:14:28 2013 (serrano)                */
;*    Copyright   :  2006-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Facilities for handling a the database                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_database
   
   (option (set! *dlopen-init-gc* #t))
   
   (library sqlite)
   
   (import  __pkglib_param)
   
   (export  (initialize-repo-db! ::%sqlite)
	    (reset-repo-db! ::%sqlite)
	    (make-repo-db::%sqlite ::bstring)
	    
	    (db-add-port! ::%sqlite ::bstring ::bstring
			  #!key
			  (status 'unknown)
			  (message ""))
	    (db-report-failure! ::%sqlite ::bstring ::bstring ::bstring)
	    
	    (db-add-package! ::%sqlite ::bstring 
			     #!key
			     (version "")
			     (release 0)
			     (language '())
			     (source "")
			     (path "")
			     (url "")
			     (md5 "")
			     (category 'misc)
			     (description "")
			     (note "")
			     (author "")
			     (license "")
			     (date "")
			     (homepage "")
			     (keywords '())
			     (dependencies '())
			     (provides '())
			     (failures '()))
	    (db-add-tuning! ::%sqlite ::bstring
			    #!key
			    (version "")
			    (host "")
			    (release 0)
			    (path "")
			    (url "")
			    (md5 ""))

	    (db-package-release ::%sqlite ::bstring ::bstring)
	    
	    (db-remove-package-version! ::%sqlite ::bstring ::bstring)
	    (db-remove-tuning! ::%sqlite ::bstring ::bstring ::bstring)
	    (db-remove-package! ::%sqlite ::bstring)
	    
	    (db-sync-list ::%sqlite
			  #!key
			  (mkpath (lambda (x) x))
			  (mkurl (lambda (s u) u)))))

;*---------------------------------------------------------------------*/
;*    make-db-package-key ...                                          */
;*---------------------------------------------------------------------*/
(define (make-db-package-key pkg version)
   (string-append pkg " " version))

;*---------------------------------------------------------------------*/
;*    initialize-repo-db! ...                                          */
;*---------------------------------------------------------------------*/
(define (initialize-repo-db! db)
   (sqlite-exec db "CREATE TABLE meta
                           (name STRING PRIMARY KEY, value STRING DEFAULT '0');
                    INSERT INTO meta (name, value)
                           VALUES ('counter', '0');
                    INSERT INTO meta (name, value)
                           VALUES ('version', '~a')" (pkglib-version))
   (sqlite-exec db "CREATE TABLE package
                              (id STRING,
                               name STRING,
                               version STRING,
                               release INTEGER,
                               language STRING,
                               source STRING,
                               category STRING DEFAULT 'misc',
                               path STRING DEFAULT '',
                               url STRING DEFAULT '',
                               md5 STRING DEFAULT '',
                               description STRING DEFAULT '',
                               note STRING DEFAULT '',
                               authors STRING DEFAULT '',
                               license STRING DEFAULT '',
                               doc STRING DEFAULT '',
                               date STRING DEFAULT '',
                               homepage STRING DEFAULT '',
                               number INTEGER DEFAULT 0,
                               reldate STRING DEFAULT '#e0',
                               PRIMARY KEY(id, release))")
   (sqlite-exec db "CREATE TABLE keyword
                              (package STRING,
                               value STRING,
                               PRIMARY KEY(package, value))")
   (sqlite-exec db "CREATE TABLE tuning
                              (package STRING,
                               host STRING,
                               release INTEGER,
                               path STRING,
                               url STRING DEFAULT '',
                               md5 STRING DEFAULT '',
                               number INTEGER DEFAULT 0,
                               reldate STRING DEFAULT '#e0',
                               PRIMARY KEY(package, host, release))")
   (sqlite-exec db "CREATE TABLE port
                              (package STRING,
                               host STRING,
                               status STRING DEFAULT '',
                               message STRING DEFAULT '',
                               PRIMARY KEY(package, host))")
   (sqlite-exec db "CREATE TABLE function
                              (package STRING,
                               name STRING,
                               proto STRING,
                               PRIMARY KEY(package, name, proto))")
   (sqlite-exec db "CREATE TABLE variable
                              (package STRING,
                               name STRING,
                               proto STRING,
                               PRIMARY KEY(package, name, proto))")
   (sqlite-exec db "CREATE TABLE macro
                              (package STRING,
                               name STRING,
                               proto STRING,
                               PRIMARY KEY(package, name, proto))")
   (sqlite-exec db "CREATE TABLE record
                              (package STRING,
                               name STRING,
                               proto STRING,
                               PRIMARY KEY(package, name, proto))")
   (sqlite-exec db "CREATE TABLE exception
                              (package STRING,
                               name STRING,
                               proto STRING,
                               PRIMARY KEY(package, name, proto))")
   (sqlite-exec db "CREATE TABLE depend
                              (package STRING,
                               name STRING,
                               version STRING DEFAULT '')")
   db)

;*---------------------------------------------------------------------*/
;*    reset-repo-db! ...                                               */
;*---------------------------------------------------------------------*/
(define (reset-repo-db! db)
   (sqlite-exec db "BEGIN TRANSACTION;
                    DROP TABLE package;
                    DROP TABLE meta;
                    DROP TABLE keyword;
                    DROP TABLE tuning;
                    DROP TABLE port;
                    DROP TABLE function;
                    DROP TABLE variable;
                    DROP TABLE macro;
                    DROP TABLE record;
                    DROP TABLE exception;
                    DROP TABLE depend;
                    END TRANSACTION;")
   (initialize-repo-db! db)
   db)   

;*---------------------------------------------------------------------*/
;*    make-repo-db ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-repo-db path)
   (unless (directory? (dirname path)) (make-directories (dirname path)))
   (when (file-exists? path) (delete-file path))
   (let ((db (instantiate::sqltiny (path path))))
      (initialize-repo-db! db)
      db))

;*---------------------------------------------------------------------*/
;*    db-inc-counter! ...                                              */
;*---------------------------------------------------------------------*/
(define (db-inc-counter! db)
   (sqlite-exec db "BEGIN TRANSACTION")
   (let ((c (string->integer
	     (sqlite-exec db
		"SELECT value FROM meta WHERE (name='counter')"))))
      (set! c (+fx 1 c))
      (sqlite-exec db
	 "REPLACE INTO meta (name, value) VALUES ('counter','~a');
	  END TRANSACTION"
	 c)
      c))
   
;*---------------------------------------------------------------------*/
;*    db-add-port! ...                                                 */
;*---------------------------------------------------------------------*/
(define (db-add-port! db id host #!key (status 'unknown) (message ""))
   (sqlite-exec db
      "REPLACE INTO
          port (package, host, status, message)
          VALUES (~q, '~a', '~a', '~a')"
      id host status message))
   
;*---------------------------------------------------------------------*/
;*    db-report-failure! ...                                           */
;*---------------------------------------------------------------------*/
(define (db-report-failure! db id host message)
   (sqlite-exec db
      "BEGIN TRANSACTION;
       DELETE FROM port WHERE (package = ~q) AND (host = '~a')"
      id host)
   (db-add-port! db id host :status 'failure :message message)
   (sqlite-exec db "END TRANSACTION"))

;*---------------------------------------------------------------------*/
;*    db-add-package! ...                                              */
;*---------------------------------------------------------------------*/
(define (db-add-package! db name
			 #!key 
			 (version "")
			 (release 0)
			 (language '())
			 (source "")
			 (path "")
			 (url "")
			 (md5 "")
			 (category 'misc)
			 (description "")
			 (note "")
			 (author "")
			 (license "")
			 (date "")
			 (homepage "")
			 (keywords '())
			 (dependencies '())
			 (provides '())
			 (failures '()))
   (let ((id (make-db-package-key name version)))
      ;; remove existing versions
      (sqlite-eval db
	 (lambda (name version rel)
	    (when (integer? release)
	       (set! release (+fx 1 (string->integer rel))))
	    (sqlite-exec db "BEGIN TRANSACTION")
	    (db-remove-package-version-sans-tuning! db name version)
	    (sqlite-exec db "END TRANSACTION"))
	 "SELECT name, version, release FROM package WHERE (id=~q)" id)
      ;; add the package in the db
      (let ((counter (db-inc-counter! db)))
	 (sqlite-exec db
	    "REPLACE INTO
               package (id, name, version, release,
                        language, source, category,
                        path, url, md5,
                        date, homepage, 
                        description, note, authors, number, reldate)
               VALUES (~q, ~q, ~q, ~q, ~q, '~a', '~a', ~q, ~q, ~q, ~q, ~q, ~q, ~q, ~q, ~q, '~a')"
	    id name version release
	    (with-output-to-string (lambda () (display language)))
	    source
	    category (or path "") url md5
	    date homepage
	    description note author counter (current-seconds)))
      ;; add the functions, variables, macros, records and exceptions
      (for-each (lambda (form)
		   (match-case form
		      ((exception ?exc . ?-)
		       (sqlite-exec db
			  "REPLACE INTO exception (package, name, proto)
                              VALUES (~q, '~q', '~q')"
			  id exc form))
		      ((record (and ?proto ((and (? symbol?) ?fun) . ?-)))
		       (sqlite-exec db
			  "REPLACE INTO function (package, name, proto)
                              VALUES (~q, '~q', '~q')"
			  id fun form))
		      ((macro (and ?proto ((and (? symbol?) ?mac) . ?-)))
		       (sqlite-exec db
			  "REPLACE INTO macro (package, name, proto)
                              VALUES (~q, '~q', '~q')"
			  id mac form))
		      (((and (? symbol?) ?fun) . ?-)
		       (sqlite-exec db
			  "REPLACE INTO function (package, name, proto)
                              VALUES (~q, '~q', '~q')"
			  id fun form))
		      ((? symbol?)
		       (sqlite-exec db
			  "REPLACE INTO variable (package, name, proto)
                              VALUES (~q, '~q', '~q')"
			  id form form))))
		provides)
      ;; add the category
      (for-each (lambda (keyword)
		   (sqlite-exec db
		      "REPLACE INTO keyword (package, value)
                          VALUES (~q, '~a')"
		      id keyword))
		keywords)
      ;; add the dependencies
      (for-each (lambda (depend)
		   ;; DEPEND is a pair when a package depends on a
		   ;; specific package version. Otherwise, it is a simple
		   ;; symbol denoting the package name.
		   (if (pair? depend)
		       (sqlite-exec db
			  "REPLACE INTO depend (package, name, version)
                              VALUES (~q, '~a', '~a')"
			  id (car depend) (cadr depend))
		       (sqlite-exec db
			  "REPLACE INTO depend (package, name, version)
                              VALUES (~q, '~a', '*')"
			  id depend)))
		dependencies)
      ;; add the failures
      (for-each (lambda (t)
		   (let ((name (if (symbol? (car t))
				   (symbol->string (car t))
				   (car t))))
		      (db-report-failure! db id name (cadr t))))
		failures)
      ;; add the tuning
      (sqlite-map db
	 (lambda (host)
	    (db-add-port! db id host :status 'tuning))
	 "SELECT host FROM tuning
          WHERE (package=~q)" id)
      ;; add the transitive failures
      '(for-each (lambda (depend)
		   (for-each (lambda (host)
				(if (pair? depend)
				    (let ((m (format
					      "depends on failing `~a ~a'"
					      (car depend)
					      (cadr depend))))
				       (db-report-failure! db id host m))
				    (let ((m (format
					      "depends on failing `~a'"
					      depend
					      depend depend)))
				       (db-report-failure! db id host m))))
			     (if (pair? depend)
				 (sqlite-map db
				    (lambda (host) host)
				    "SELECT host FROM port, package
                                         WHERE (status='failure')
                                           AND (package.name='~a')
                                           AND (version='~a')
                                           and (port.package=package.id)"
				    (car depend) (cadr depend))
				 (sqlite-map db
				    (lambda (host) host)
				    "SELECT host FROM port, package
                                         WHERE (status='failure')
                                           AND (package.name='~a')
                                           and (port.package=package.id)"
				    depend))))
		dependencies)))

;*---------------------------------------------------------------------*/
;*    db-add-tuning! ...                                               */
;*---------------------------------------------------------------------*/
(define (db-add-tuning! db name
			#!key
			(version "")
			(host "")
			(release 0)
			(path "") (url "") (md5 ""))
   (let ((c (db-inc-counter! db))
	 (id (make-db-package-key name version)))
      (sqlite-exec db
	 "REPLACE INTO
            tuning (package, host, release, path, url, md5, number, reldate)
            VALUES (~q, '~a', ~q, ~q, ~q, ~q, ~a, '~a')"
	 id host release path url md5 c (current-seconds))
      (db-add-port! db id host :status 'tuning)))

;*---------------------------------------------------------------------*/
;*    db-package-release ...                                           */
;*---------------------------------------------------------------------*/
(define (db-package-release db pkg ver)
   (sqlite-eval db
      (lambda (x) x)
      "SELECT release FROM package where (name=~q) and (version=~q)" pkg ver))

;*---------------------------------------------------------------------*/
;*    db-remove-package-version! ...                                   */
;*---------------------------------------------------------------------*/
(define (db-remove-package-version! db pkg version)
   (let ((key (make-db-package-key pkg version)))
      (sqlite-exec db "BEGIN TRANSACTION")
      (db-remove-package-version-sans-tuning! db pkg version)
      (sqlite-exec db "DELETE FROM tuning WHERE (package=~q)" key)
      (sqlite-exec db "END TRANSACTION; VACUUM")))

;*---------------------------------------------------------------------*/
;*    db-remove-package-version-sans-tuning! ...                       */
;*---------------------------------------------------------------------*/
(define (db-remove-package-version-sans-tuning! db pkg version)
   (let ((key (make-db-package-key pkg version)))
      (sqlite-exec db "DELETE FROM function WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM variable WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM keyword WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM macro WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM record WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM exception WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM depend WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM port WHERE (package=~q)" key)
      (sqlite-exec db "DELETE FROM package WHERE (id=~q)" key)))

;*---------------------------------------------------------------------*/
;*    db-remove-tuning! ...                                            */
;*---------------------------------------------------------------------*/
(define (db-remove-tuning! db pkg version tuning)
   (let ((key (make-db-package-key pkg version)))
      (sqlite-exec db "BEGIN TRANSACTION")
      (sqlite-exec db "DELETE FROM tuning WHERE (package=~q) and (host=~q)"
		   key tuning)
      (sqlite-exec db "DELETE FROM port WHERE (package=~q) and (host=~q)"
		   key tuning)
      (sqlite-exec db "END TRANSACTION; VACUUM")))

;*---------------------------------------------------------------------*/
;*    db-remove-package! ...                                           */
;*---------------------------------------------------------------------*/
(define (db-remove-package! db pkg)
   (for-each (lambda (version)
		(db-remove-package-version! db pkg version))
	     (sqlite-map db
		(lambda (x) x)
		"SELECT version FROM package WHERE (name=~q)" pkg)))

;*---------------------------------------------------------------------*/
;*    db-sync-list ...                                                 */
;*---------------------------------------------------------------------*/
(define (db-sync-list db
		      #!key
		      (mkpath (lambda (x) x))
		      (mkurl (lambda (s u) u)))

   (define (sync-interf id name ver rel src lang path url md5 descr auth cat)
      (let ((d (sqlite-map db
		  (lambda (name version)
		     (list name version))
		  "SELECT name, version
                     FROM depend WHERE (package=~q)" id))
	    (t (sqlite-map db
		  (lambda (host path url md5)
		     (list host
			   :path (mkpath path)
			   :url (mkurl path url)
			   :md5 md5))
		  "SELECT host, path, url, md5
                     FROM tuning t
                    WHERE (package=~q)
                      AND release IN (SELECT MAX(release)
                                        FROM tuning
                                       WHERE t.package=package
                                       GROUP BY package)"
		      id))
	    (f (sqlite-map db
		  (lambda (host message)
		     (list host message))
		  "SELECT host, message
                     FROM port
                     WHERE (package=~q) AND (status='failure')" id))
	    (p (append-map (lambda (c)
			      (sqlite-map db
				 (lambda (p)
				    (with-input-from-string p read))
				 "SELECT proto
                                    FROM ~q
                                   WHERE (package=~q)" c id))
			   '(record exception macro function variable))))
	 (list 'interface name
	       :version ver
	       :release (string->integer rel)
	       :language (with-input-from-string lang read)
	       :source src
	       :category cat
	       :path (mkpath path)
	       :url (mkurl path url)
	       :md5 md5
	       :description descr
	       :author auth
	       :failures f
	       :dependencies d
	       :provides p)))
   
   (define (sync-tunings id name version)
      (sqlite-map db
	 (lambda (host path url md5 release)
	    (list 'tuning name
		  :host host
		  :version version
		  :release (string->integer release)
		  :path (mkpath path)
		  :url (mkurl path url)
		  :md5 md5))
	 "SELECT host, path, url, md5, release
            FROM tuning t
           WHERE (package=~q)
             AND release IN (SELECT MAX(release)
                               FROM tuning
                              WHERE t.package=package
                              GROUP BY package)"
	 id))
   
   (apply append
	  (sqlite-map db
	     (lambda (id name version rel src lang path url md5 descr auth cat)
		(cons (sync-interf id name version rel src lang path url md5 descr auth cat)
		      (sync-tunings id name version)))
	     "SELECT id, name, version, release, source, language, path, url, md5, description, authors, category
              FROM package p
              WHERE version IN (SELECT MAX(version)
                                FROM package
                                WHERE p.name=name
                                GROUP BY name)
              GROUP BY name")))
