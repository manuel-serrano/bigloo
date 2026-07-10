;*=====================================================================*/
;*    .../project/bigloo/5.0.x/api/sqlite/src/Llib/sqltiny.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 16 17:44:47 2007                          */
;*    Last change :  Thu Jul  9 08:09:56 2026 (serrano)                */
;*    Copyright   :  2007-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The portable replacement for sqlite                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_sqltiny
   
   (import __sqlite_types
	   __sqlite_engine
	   __sqlite_parser)
   
   (export ($sqltiny-open::$sqltiny ::bstring ::symbol)
	   ($sqltiny-close ::$sqltiny ::obj)
	   ($sqltiny-exec::obj ::$sqltiny ::bstring ::obj)
	   ($sqltiny-eval::obj ::$sqltiny ::procedure ::bstring ::obj)
	   ($sqltiny-get::obj ::$sqltiny ::procedure ::bstring ::obj)
	   ($sqltiny-map::pair-nil ::$sqltiny ::procedure ::bstring ::obj)
	   ($sqltiny-for-each ::$sqltiny ::procedure ::bstring ::obj)
	   ($sqltiny-dump-table ::obj ::$sqltiny ::bstring ::output-port)))

;*---------------------------------------------------------------------*/
;*    display-object ::$sqltiny-table ...                              */
;*---------------------------------------------------------------------*/
(define-method (display-object o::$sqltiny-table #!optional (port::output-port (current-output-port)))
   (with-access::$sqltiny-table o (name)
      (fprintf port "#<$sqltiny-table:~a>")))

;*---------------------------------------------------------------------*/
;*    write-object ::$sqltiny-table ...                                */
;*---------------------------------------------------------------------*/
(define-method (write-object o::$sqltiny-table #!optional (port::output-port (current-output-port)))
   (with-access::$sqltiny-table o (name columns)
      (display "#<sqltiny:" port)
      (display name port)
      (display ":" port)
      (for-each (lambda (c)
		   (with-access::$sqltiny-column c (name)
		      (display name port)
		      (display "," port)))
	 columns)
      (fprint port ">")))

;*---------------------------------------------------------------------*/
;*    $sqltiny-open ...                                                */
;*---------------------------------------------------------------------*/
(define ($sqltiny-open path sync)
   (if (and (not (string=? path ":memory:")) (file-exists? path))
       (let ((p (open-input-binary-file path)))
	  (unwind-protect
	     (let ((o (input-obj p)))
		(with-access::$sqltiny o ((spath path))
		   (set! spath path))
		o)
	     (close-binary-port p)))
       (let* ((crowid (instantiate::$sqltiny-column
			 (name "rowid")
			 (type 'integer)
			 (default 0)
			 (index 0)))
	      (cname (instantiate::$sqltiny-column
			(name "name")
			(type 'string)
			(default "")
			(index 1)))
	      (ctype (instantiate::$sqltiny-column
			(name "type")
			(type 'string)
			(default "")
			(index 2)))
	      (master (instantiate::$sqltiny-table
			 (name "sqlite_master")
			 (removable #f)
			 (columns (list crowid cname ctype)))))
	  (instantiate::$sqltiny
	     (path (string-copy path))
	     (sync sync)
	     (tables (list master))))))

;*---------------------------------------------------------------------*/
;*    $sqltiny-close ...                                               */
;*---------------------------------------------------------------------*/
(define ($sqltiny-close builtin obj)
   (with-access::$sqltiny builtin (path)
      (unless (string=? path ":memory:")
	 (let ((p (open-output-binary-file path)))
	    (unwind-protect
	       (output-obj p builtin)
	       (close-binary-port p))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-do ...                                                   */
;*---------------------------------------------------------------------*/
(define (sqltiny-do builtin cmd obj proc)
   (with-input-from-string cmd
      (lambda ()
	 (let loop ((actions (sqltiny-parse (current-input-port)))
		    (res #f)
		    (cols #f))
	    (if (null? actions)
		(proc res cols)
		(let ((act (car actions)))
		   (multiple-value-bind (r c)
		      (act obj builtin)
		      (loop (cdr actions) (or r res) (or c cols)))))))))

;*---------------------------------------------------------------------*/
;*    $sqltiny-exec ...                                                */
;*---------------------------------------------------------------------*/
(define ($sqltiny-exec builtin cmd obj)
   (let ((p (lambda (res cols) (when (pair? res) (car res)))))
      (let ((l (sqltiny-do builtin cmd obj p)))
	 (when (pair? l)
	    (car l)))))

;*---------------------------------------------------------------------*/
;*    $sqltiny-eval ...                                                */
;*---------------------------------------------------------------------*/
(define ($sqltiny-eval builtin proc cmd obj)
   (let ((p (lambda (res cols) (when (pair? res) (apply proc (car res))))))
      (sqltiny-do builtin cmd obj p)))

;*---------------------------------------------------------------------*/
;*    $sqltiny-get ...                                                 */
;*---------------------------------------------------------------------*/
(define ($sqltiny-get builtin proc cmd obj)
   (let ((p (lambda (res cols)
	       (when (pair? res)
		  (proc
		     (if (pair? cols) (list->vector (car cols)) '#())
		     (list->vector (car res)))))))
      (sqltiny-do builtin cmd obj p)))

;*---------------------------------------------------------------------*/
;*    $sqltiny-map ...                                                 */
;*---------------------------------------------------------------------*/
(define ($sqltiny-map builtin proc cmd obj)
   (let ((p (lambda (res cols)
	       (if (pair? res)
		   (map (lambda (r) (apply proc r)) res)
		   '()))))
      (sqltiny-do builtin cmd obj p)))

;*---------------------------------------------------------------------*/
;*    $sqltiny-for-each ...                                            */
;*---------------------------------------------------------------------*/
(define ($sqltiny-for-each builtin proc cmd obj)
   (let ((p (lambda (res cols)
	       (when (pair? res)
		  (let ((cols (if (pair? cols) (list->vector (car cols)) '#())))
		     (for-each (lambda (r)
				  (proc cols (list->vector r)))
			res))))))
      (sqltiny-do builtin cmd obj p)))

;*---------------------------------------------------------------------*/
;*    for-list ...                                                     */
;*---------------------------------------------------------------------*/
(define (for-list proc out lst)
   (when (pair? lst)
      (let loop ((lst lst))
	 (proc (car lst) out)
	 (unless (null? (cdr lst))
	    (display "," out)
	    (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    dump-column ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-column col out)
   (with-access::$sqltiny-column col (name type primkey default)
      (display name out)
      (unless (eq? type 'OBJ)
	 (display " " out)
	 (display type out))
      (unless (eq? default #unspecified)
	 (display " DEFAULT " out)
	 (write default out))
      (when primkey
	 (display " " out)
	 (display "PRIMARY KEY" out))))

;*---------------------------------------------------------------------*/
;*    dump-row ...                                                     */
;*---------------------------------------------------------------------*/
(define (dump-row name columns row out)
   (display "INSERT INTO " out)
   (display name out)
   (display "(" out)
   (for-list display out columns)
   (display ") VALUES(" out)
   (for-list (lambda (x out)
		(if (eq? x #unspecified)
		    (display "NULL" out)
		    (write x out)))
	     out (cdr (vector->list row)))
   (display ");\n" out))
   
;*---------------------------------------------------------------------*/
;*    $sqltiny-dump-table ...                                          */
;*---------------------------------------------------------------------*/
(define ($sqltiny-dump-table obj builtin table out)
   (let ((t (sqltiny-get-table builtin table)))
      (if (isa? t $sqltiny-table)
	  (with-access::$sqltiny-table t (name *columns constraints rows)
	     (display "BEGIN TRANSACTION;\n" out)
	     (display "CREATE TABLE " out)
	     (display name out)
	     (display " (" out)
	     (for-list (lambda (c out) (dump-column c out))
		       out
		       *columns)
	     (when (pair? constraints)
		(display ", " out)
		(for-list (lambda (c out)
			     (match-case c
				((primary-key . ?rest)
				 (display "PRIMARY KEY(")
				 (for-list display out rest)
				 (display ")"))))
			  out
			  constraints))
	     (display ");\n" out)
	     (with-access::$sqltiny-table t (columns)
		(let ((cols (map (lambda (c)
				    (with-access::$sqltiny-column c (name)
				       name))
			       columns)))
		   (for-each (lambda (r) (dump-row name (cdr cols) r out))
		      rows)))
	     (display "END TRANSACTION;\n"))
	  (raise
	   (instantiate::&error
	      (proc 'sqltiny-drop-table)
	      (msg (format "SQL error: no such table: ~a" table))
	      (obj obj))))))

;*---------------------------------------------------------------------*/
;*    serialization ...                                                */
;*---------------------------------------------------------------------*/
(register-class-serialization!
 $sqltiny
 (lambda (t)
    (with-access::$sqltiny t (path tables sync)
       (vector path tables sync)))
 (lambda (v)
    (let ((o (instantiate::$sqltiny
		(path (vector-ref v 0))
		(tables (vector-ref v 1))
		(sync (vector-ref v 2))
		(transaction #f))))
       ;; patch all the tables for adding the constraint
       (for-each (lambda (t)
		    (with-access::$sqltiny-table t ((cols columns)
						    (cts constraints)
						    keycheck)
		       (let ((kcheck (sqltiny-compile-key-check o t cols cts)))
			  (set! keycheck kcheck))))
		 (vector-ref v 1))
       o)))
    
				  
(register-class-serialization!
 $sqltiny-table
 (lambda (t)
    (with-access::$sqltiny-table t (name
				    removable columns *columns
				    rows rowid
				    last-row-pair constraints)
       (vector name removable
	       columns *columns
	       rows rowid last-row-pair constraints)))
 (lambda (v)
    (instantiate::$sqltiny-table 
       (name (vector-ref v 0))
       (removable (vector-ref v 1))
       (columns (vector-ref v 2))
       (*columns (vector-ref v 3))
       (rows (vector-ref v 4))
       (rowid (vector-ref v 5))
       (last-row-pair (vector-ref v 6))
       (constraints (vector-ref v 7)))))
