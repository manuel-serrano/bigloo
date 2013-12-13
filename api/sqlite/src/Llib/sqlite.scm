;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/src/Llib/sqlite.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Erick Gallesio                                    */
;*    Creation    :  Thu Nov 10 13:55:46 2005                          */
;*    Last change :  Fri Dec 13 12:15:43 2013 (serrano)                */
;*    Copyright   :  2005-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SQLITE Scheme binding                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_sqlite
   
   (option (set! *dlopen-init-gc* #t))
   
   (cond-expand
      ((and bigloo-c (not sqltiny))
       (include "sqlite.sch")))

   (cond-expand
      ((and bigloo-c (not sqltiny))
       (export (class sqlite::%sqlite
		  (%setup-sqlite!)
		  ($builtin::$sqlite (default ($sqlite-nil))))
	       ($sqlite-nil)))
      (else
       (export (class sqlite::sqltiny))))
   
   (import __sqlite_sqltiny)
   
   (export  (abstract-class %sqlite
	       (path::bstring read-only (default ":memory:")))

	    (class sqltiny::%sqlite
	       (%setup-sqltiny!)
	       (sync::symbol read-only (default 'automatic))
	       ($builtin::$sqltiny (default (class-nil $sqltiny))))
	    
	    (%setup-sqltiny! ::sqltiny)
	    (%setup-sqlite! ::sqlite)
	    
	    (generic sqlite-close ::%sqlite)
	    
	    (generic sqlite-exec ::%sqlite ::bstring . o)
	    (generic sqlite-eval ::%sqlite ::procedure ::bstring . o)
	    (generic sqlite-map::pair-nil ::%sqlite ::procedure ::bstring . o)
	    
	    (sqlite-table-informations ::%sqlite ::bstring)
	    (sqlite-table-number-of-rows ::%sqlite ::bstring)
	    (sqlite-table-name-of-columns ::%sqlite ::bstring)
	    (sqlite-name-of-tables ::%sqlite)
	    (sqlite-dump ::%sqlite ::output-port)
	    (generic sqlite-dump-table ::%sqlite ::bstring ::output-port)
	    
	    (sqlite-last-insert-rowid ::%sqlite)

	    (sqlite-format ::bstring . ::obj)))

;*---------------------------------------------------------------------*/
;*    $sqlite-nil ...                                                  */
;*---------------------------------------------------------------------*/
(cond-expand
   ((and bigloo-c (not sqltiny))
    (define ($sqlite-nil)
       ($_sqlite-nil))))

;*---------------------------------------------------------------------*/
;*    object-display ::sqlite ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-display o::sqlite . port)
   (with-output-to-port (if (pair? port) (car port) (current-output-port))
      (lambda ()
         (with-access::sqlite o (path)
            (display* "#<sqlite:" path ">")))))

;*---------------------------------------------------------------------*/
;*    object-write ::sqlite ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-write o::sqlite . port)
   (with-output-to-port (if (pair? port) (car port) (current-output-port))
      (lambda ()
         (with-access::sqlite o (path)
            (display* "#<sqlite:" path ">")))))

;*---------------------------------------------------------------------*/
;*    object-print ::sqlite ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-print o::sqlite port print-slot)
   (object-write o port))

;*---------------------------------------------------------------------*/
;*    %setup-sqlite! ...                                               */
;*---------------------------------------------------------------------*/
(define (%setup-sqlite! o::sqlite)
   (cond-expand
      ((and bigloo-c (not sqltiny))
       (with-access::sqlite o ($builtin path)
	  (set! $builtin ($sqlite-open path))))
      (else
       (with-access::sqlite o ($builtin path sync)
	  (set! $builtin ($sqltiny-open path sync))))))

;*---------------------------------------------------------------------*/
;*    %setup-sqltiny! ...                                              */
;*---------------------------------------------------------------------*/
(define (%setup-sqltiny! o::sqltiny)
   (with-access::sqltiny o ($builtin path sync)
      (set! $builtin ($sqltiny-open path sync))))

;*---------------------------------------------------------------------*/
;*    sqlite-close ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (sqlite-close o::%sqlite)
   (with-access::sqltiny o ($builtin)
      ($sqltiny-close $builtin o)
      (set! $builtin (class-nil $sqltiny))))

(cond-expand
   ((and bigloo-c (not sqltiny))
    (define-method (sqlite-close o::sqlite)
       (with-access::sqlite o ($builtin)
	  ($sqlite-close $builtin o)))))

;*---------------------------------------------------------------------*/
;*    sqlite-exec ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (sqlite-exec o::%sqlite fmt::bstring . args)
   (with-access::sqltiny o ($builtin)
      (if (null? args)
	  ($sqltiny-exec $builtin fmt o)
	  ($sqltiny-exec $builtin (apply sqlite-format fmt args) o))))

(cond-expand
   ((and bigloo-c (not sqltiny))
    (define-method (sqlite-exec o::sqlite fmt::bstring . args)
       (with-access::sqlite o ($builtin)
	  (if (null? args)
	      ($sqlite-exec $builtin fmt o)
	      ($sqlite-exec $builtin (apply sqlite-format fmt args) o))))))

;*---------------------------------------------------------------------*/
;*    sqlite-callback ...                                              */
;*---------------------------------------------------------------------*/
(define-macro (sqlite-callback proc exc)
   `(case (procedure-arity ,proc)
       ((1)
	(lambda (x)
	   (with-handler (lambda (e) (set! ,exc e)) (,proc x))))
       ((2)
	(lambda (x y)
	   (with-handler (lambda (e) (set! ,exc e)) (,proc x y))))
       ((3)
	(lambda (x y z)
	   (with-handler (lambda (e) (set! ,exc e)) (,proc x y z))))
       ((4)
	(lambda (x y z t)
	   (with-handler (lambda (e) (set! ,exc e)) (,proc x y z t))))
       (else
	(lambda l
	   (with-handler (lambda (e) (set! ,exc e)) (apply ,proc l))))))
       
;*---------------------------------------------------------------------*/
;*    sqlite-eval ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (sqlite-eval o::%sqlite p::procedure fmt::bstring . args)
   (with-access::sqltiny o ($builtin)
      (if (null? args)
	  ($sqltiny-eval $builtin p fmt o)
	  ($sqltiny-eval $builtin p (apply sqlite-format fmt args) o))))

(cond-expand
   ((and bigloo-c (not sqltiny))
    (define-method (sqlite-eval o::sqlite p::procedure fmt::bstring . args)
       (let* ((exc #f)
	      (p (sqlite-callback p exc)))
	  (unwind-protect
	     (with-access::sqlite o ($builtin)
		(if (null? args)
		    ($sqlite-eval $builtin p fmt o)
		    ($sqlite-eval $builtin p (apply sqlite-format fmt args) o)))
	     (when exc (raise exc)))))))

;*---------------------------------------------------------------------*/
;*    sqlite-map ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (sqlite-map o::%sqlite p::procedure fmt::bstring . args)
   (with-access::sqltiny o ($builtin)
      (if (null? args)
	  ($sqltiny-map $builtin p fmt o)
	  ($sqltiny-map $builtin p (apply sqlite-format fmt args) o))))

(cond-expand
   ((and bigloo-c (not sqltiny))
    (define-method (sqlite-map o::sqlite p::procedure fmt::bstring . args)
       (let* ((exc #f)
	      (p (sqlite-callback p exc)))
	  (unwind-protect
	     (with-access::sqlite o ($builtin)
		(if (null? args)
		    ($sqlite-map $builtin p fmt o)
		    ($sqlite-map $builtin p (apply sqlite-format fmt args) o)))
	     (when exc (raise exc)))))))

;*---------------------------------------------------------------------*/
;*    sqlite-table-informations ...                                    */
;*---------------------------------------------------------------------*/
(define (sqlite-table-informations db name)
   (sqlite-map db
      (lambda (x y) y)
      (format "PRAGMA table_info(~a)" name)))

;*---------------------------------------------------------------------*/
;*    sqlite-table-number-of-rows ...                                  */
;*---------------------------------------------------------------------*/
(define (sqlite-table-number-of-rows db name)
   (sqlite-eval db
      (lambda (x) (string->integer x))
      (format "SELECT MAX(rowid) FROM ~A" name)))

;*---------------------------------------------------------------------*/
;*    sqlite-table-name-of-columns ...                                 */
;*---------------------------------------------------------------------*/
(define (sqlite-table-name-of-columns db name)
   (sqlite-table-informations db name))

;*---------------------------------------------------------------------*/
;*    sqlite-name-of-tables ...                                        */
;*---------------------------------------------------------------------*/
(define (sqlite-name-of-tables db)
   (sqlite-map db
      (lambda (x) x)
      "SELECT name FROM sqlite_master WHERE type='table'"))

;*---------------------------------------------------------------------*/
;*    sqlite-dump ...                                                  */
;*---------------------------------------------------------------------*/
(define (sqlite-dump db::%sqlite out::output-port)
   (for-each (lambda (table)
		(sqlite-dump-table db table out))
	     (sqlite-name-of-tables db)))

;*---------------------------------------------------------------------*/
;*    sqlite-dump-table ...                                            */
;*---------------------------------------------------------------------*/
(define-generic (sqlite-dump-table db::%sqlite table::bstring out)
   (with-access::sqltiny db ($builtin)
      ($sqltiny-dump-table db $builtin table out))
   #unspecified)

(cond-expand
   ((and bigloo-c (not sqltiny))
    (define-method (sqlite-dump-table db::sqlite table out)
       (write (sqlite-exec db (string-append ".dump " table)) out))))

;*---------------------------------------------------------------------*/
;*    sqlite-format ...                                                */
;*---------------------------------------------------------------------*/
(define (sqlite-format fmt . objs)
   (let ((p (open-output-string))
	 (len (string-length fmt)))
      (let loop ((i 0)
		 (os objs))
	 (define (next os fmt)
	    (if (null? os)
		(error 'sqlite-format "Insufficient number of arguments" fmt)
		(car os)))
	 (define (print-radix radix num)
	    (if (not (number? num))
		(error 'sqlite-format "Illegal number" num)
		(display (number->string num radix) p)))
	 (define (display-sqlite obj p l)
	    (cond
	       ((string? obj)
		(if (=fx l 0)
		    (write-char #\' p)
		    (display "\"\"" p))
		(let ((len (string-length obj)))
		   (do ((i 0 (+fx i 1)))
		       ((=fx i len))
		       (let ((c (string-ref obj i)))
			  (write-char c p)
			  (when (char=? #\' c) (write-char #\' p)))))
		(if (=fx l 0)
		    (write-char #\' p)
		    (display "\"\"" p)))
	       ((date? obj)
		(display (date->seconds obj) p))
	       ((or (elong? obj) (llong? obj))
		(write obj p))
	       ((or (eq? obj #unspecified) (eq? obj #f))
		(display "NULL" p))
	       ((pair? obj)
		(display-sqlite-pair obj p))
	       ((vector? obj)
		(display-sqlite-vector obj p))
	       ((struct? obj)
		(display-sqlite-struct obj p))
	       (else
		(display obj p))))
	 (define (display-sqlite-pair obj p)
	    (display "(" p)
	    (let loop ((obj obj))
	       (display-sqlite (car obj) p 1)
	       (cond
		  ((null? (cdr obj))
		   (display ")" p))
		  ((not (pair? (cdr obj)))
		   (display " . " p)
		   (display-sqlite (cdr obj) p 1)
		   (display ")" p))
		  (else
		   (display " " p)
		   (loop (cdr obj))))))
	 (define (display-sqlite-vector obj p)
	    (display "#(" p)
	    (let ((len (vector-length obj)))
	       (let loop ((i 0))
		  (if (=fx i len)
		      (display ")" p)
		      (begin
			 (display " " p)
			 (display-sqlite (vector-ref obj i) p 1)
			 (loop (+fx i 1)))))))
	 (define (display-sqlite-struct obj p)
	    (display "#{" p)
	    (display (struct-key obj) p)
	    (let ((len (struct-length obj)))
	       (let loop ((i 0))
		  (if (=fx i len)
		      (display "}" p)
		      (begin
			 (display " " p)
			 (display-sqlite (struct-ref obj i) p 1)
			 (loop (+fx i 1)))))))
	 (define (display-sqlite-list obj p)
	    (cond
	       ((null? obj)
		#unspecified)
	       ((not (pair? obj))
		(error 'sqlite-format "Illegal list" obj))
	       ((null? (cdr obj))
		(display (car obj) p))
	       (else
		(let loop ((o obj))
		   (cond
		      ((pair? (cdr o))
		       (display (car o) p)
		       (display "," p)
		       (loop (cdr o)))
		      ((null? (cdr o))
		       (display (car o) p))
		      (else
		       (error 'sqlite-form "Illegal list" obj)))))))
	 (define (display-sqlite-quote-list obj p)
	    (cond
	       ((not (pair? obj))
		(error 'sqlite-format "Illegal list" obj))
	       ((null? obj)
		#unspecified)
	       ((null? (cdr obj))
		(display-sqlite (car obj) p 0))
	       (else
		(let loop ((o obj))
		   (cond
		      ((pair? (cdr o))
		       (display-sqlite (car o) p 0)
		       (display "," p)
		       (loop (cdr o)))
		      ((null? (cdr o))
		       (display-sqlite (car o) p 0))
		      (else
		       (error 'sqlite-form "Illegal list" obj)))))))
	 (if (<fx i len)
	     (let ((c (string-ref fmt i)))
		(if (char=? c #\~)
		    (if (=fx i (-fx len 1))
			(error 'sqlite-format
			       "Tag not allowd here"
			       (substring fmt i len))
			(let ((f (string-ref fmt (+fx i 1))))
			   (case f
			      ((#\a #\A)
			       (display (next os f) p)
			       (loop (+fx i 2) (cdr os)))
			      ((#\q #\Q)
			       (display-sqlite (next os f) p 0)
			       (loop (+fx i 2) (cdr os)))
			      ((#\l #\L)
			       (display-sqlite-list (next os f) p)
			       (loop (+fx i 2) (cdr os)))
			      ((#\k #\K)
			       (display-sqlite-quote-list (next os f) p)
			       (loop (+fx i 2) (cdr os)))
			      ((#\s #\S)
			       (write (next os f) p)
			       (loop (+fx i 2) (cdr os)))
			      ((#\v #\V)
			       (display (next os f) p)
			       (newline p)
			       (loop (+fx i 2) (cdr os)))
			      ((#\c #\C)
			       (let ((o (next os f)))
				  (if (not (char? o))
				      (error sqlite-format "Illegal char" o)
				      (begin
					 (write-char o p)
					 (loop (+fx i 2) (cdr os))))))
			      ((#\x #\X)
			       (print-radix 16 (next os f))
			       (loop (+fx i 2) (cdr os)))
			      ((#\o #\O)
			       (print-radix 8 (next os f))
			       (loop (+fx i 2) (cdr os)))
			      ((#\b #\B)
			       (print-radix 2 (next os f))
			       (loop (+fx i 2) (cdr os)))
			      ((#\% #\n)
			       (newline p)
			       (loop (+fx i 2) os))
			      ((#\r)
			       (write-char #\return p)
			       (loop (+fx i 2) os))
			      ((#\~)
			       (write-char #\~ p)
			       (loop (+fx i 2) os))
			      (else
			       (error 'sqlite-format "Illegal tag" f)))))
		    (begin
		       (write-char c p)
		       (loop (+fx i 1) os))))
	     (close-output-port p)))))
			 
;*---------------------------------------------------------------------*/
;*    sqlite-last-insert-rowid ...                                     */
;*---------------------------------------------------------------------*/
(define (sqlite-last-insert-rowid db)
   (sqlite-exec db "SELECT last_insert_rowid()"))

