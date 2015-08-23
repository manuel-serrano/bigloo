;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/src/Llib/engine.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 16 18:35:10 2007                          */
;*    Last change :  Fri Aug  7 19:49:50 2015 (serrano)                */
;*    Copyright   :  2007-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Simple SQLTINY evaluator                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_engine
   (import __sqlite_sqltiny)
   (export (sqltiny-create-table o ::$sqltiny ::bstring ::pair-nil ::pair-nil)
	   (sqltiny-compile-key-check o ::$sqltiny-table ::pair-nil ::obj)
	   (sqltiny-drop-table ::obj ::$sqltiny ::bstring ::bool)
	   (sqltiny-get-table ::$sqltiny ::bstring)
	   (sqltiny-insert ::obj ::$sqltiny ::bstring ::obj ::pair-nil ::bool)
	   (sqltiny-begin-transaction! ::obj ::$sqltiny)
	   (sqltiny-end-transaction! ::obj ::$sqltiny)
	   (sqltiny-select ::obj ::$sqltiny ::pair)
	   (sqltiny-update ::obj ::$sqltiny ::bstring ::pair ::pair-nil)
	   (sqltiny-delete ::obj ::$sqltiny ::bstring ::obj)
	   (sqltiny-table-info ::obj ::$sqltiny ::bstring)
	   (sqltiny-vacuum ::obj ::$sqltiny)
	   (sqltiny-alter ::obj ::$sqltiny ::bstring ::procedure)
	   (sqltiny-add-column! ::obj ::$sqltiny ::$sqltiny-table ::obj)))

;*---------------------------------------------------------------------*/
;*    *sqltiny-mutex* ...                                              */
;*---------------------------------------------------------------------*/
(define *sqltiny-mutex* (make-mutex "sqltiny-portable"))

;*---------------------------------------------------------------------*/
;*    sqltiny-create-table ...                                         */
;*    -------------------------------------------------------------    */
;*    There is no need to explictly flushes the database in this       */
;*    function since the insertion in the master table will            */
;*    force the sync.                                                  */
;*---------------------------------------------------------------------*/
(define (sqltiny-create-table obj builtin table-name columns constraints)
   (with-access::$sqltiny builtin (mutex)
      (synchronize mutex
	 (when (sqltiny-get-table builtin table-name)
	    (raise
	       (instantiate::&error
		  (proc "create-table")
		  (msg (format "SQL error: table ~s already exists" table-name))
		  (obj obj))))
	 (let ((tbl (instantiate::$sqltiny-table
		       (name table-name)
		       (constraints constraints)))
	       (cols (cons (instantiate::$sqltiny-column
			      (name "rowid")
			      (type 'INTEGER)
			      (default -1))
			(list-copy (sort-columns columns)))))
	    (table-set-columns! obj tbl cols columns)
	    (with-access::$sqltiny builtin (tables)
	       (set! tables (cons tbl tables)))))
      (sqltiny-insert obj builtin
	 "sqlite_master"
	 '("name" "type")
	 (list table-name "table")
	 #f)
      #f))

;*---------------------------------------------------------------------*/
;*    table-set-columns! ...                                           */
;*---------------------------------------------------------------------*/
(define (table-set-columns! obj tbl cols *cols)
   (with-access::$sqltiny-table tbl (columns *columns constraints keycheck)
      (let ((colis (index-columns! cols)))
	 (set! columns colis)
	 (set! *columns *cols)
	 (let ((kcheck (sqltiny-compile-key-check obj tbl colis constraints)))
	    (set! keycheck kcheck)))))
	 
;*---------------------------------------------------------------------*/
;*    sqltiny-compile-key-check ...                                    */
;*---------------------------------------------------------------------*/
(define (sqltiny-compile-key-check obj table columns contraints)
   (let ((skc (compile-simple-key-check obj table columns contraints))
	 (mkc (compile-multiple-key-check obj table columns contraints)))
      (if skc
	  (if (not mkc)
	      skc
	      (raise
	       (instantiate::&error
		  (proc "create-table")
		  (msg (format
			"SQL error: table ~s has more than one primary key"
			(with-access::$sqltiny-table table (name) name)))
		  (obj obj))))
	  (or mkc (lambda (obj r rs replacep) #t)))))

;*---------------------------------------------------------------------*/
;*    compile-simple-key-check ...                                     */
;*---------------------------------------------------------------------*/
(define (compile-simple-key-check obj table columns contraints)
   (let ((primkeys (filter (lambda (c)
			      (with-access::$sqltiny-column c (primkey)
				 primkey))
		      columns)))
      (with-access::$sqltiny-table table (name)
	 (cond
	    ((null? primkeys)
	     #f)
	    ((pair? (cdr primkeys))
	     (raise
		(instantiate::&error
		   (proc "create-table")
		   (msg (format "SQL error: table ~s has more than one primary key"
			   name))
		   (obj obj))))
	    (else
	     ;; simple key check
	     (with-access::$sqltiny-column (car primkeys) (index (n name))
		(lambda (obj r rs replacep)
		   (let* ((v (vector-ref r index))
			  (f (filter (lambda (r)
					(equal? (vector-ref r index) v))
				rs)))
		      
		      (cond
			 ((null? f)
			  #t)
			 (replacep
			    (vector-copy! (car f) 1 r 1)
			    #f)
			 (else
			  (raise
			     (instantiate::&error
				(proc "insert")
				(msg (format
					"SQL error: column ~a for table ~a is not unique: ~a" n name r))
				(obj obj)))))))))))))

;*---------------------------------------------------------------------*/
;*    compile-multiple-key-check ...                                   */
;*---------------------------------------------------------------------*/
(define (compile-multiple-key-check obj table columns constraints)
   (let ((f (filter (lambda (c) (eq? (car c) 'primary-key)) constraints)))
      (with-access::$sqltiny-table table (name)
	 (when (pair? f)
	    (when (pair? (cdr f))
	       (raise
		  (instantiate::&error
		     (proc "create-table")
		     (msg (format
			     "SQL error: table ~s has more than one primary key"
			     name))
		     (obj obj))))
	    (let* ((pk (car f))
		   (of (map (lambda (c)
			       (let ((o (column-offset obj table c)))
				  (or o
				      (raise
					 (instantiate::&error
					    (proc "create-table")
					    (msg (format
						    "SQL error: table ~a has no column named ~a"
						    name
						    c))
					    (obj obj))))))
			  (cdr pk))))
	       (lambda (obj r rs replacep)
		  (let* ((vs (map (lambda (i) (vector-ref r i)) of))
			 (f (filter (lambda (r)
				       (let ((v (map (lambda (i)
							(vector-ref r i))
						   of)))
					  (equal? vs v)))
			       rs)))
		     (cond
			((null? f)
			 #t)
			(replacep
			   (vector-copy! (car f) 1 r 1)
			   #f)
			(else
			 (raise
			    (instantiate::&error
			       (proc "insert")
			       (msg (format
				       "SQL error: column ~a for table ~a is not unique: ~a"
				       (cdr pk) name r))
			       (obj obj)))
			 #t)))))))))

;*---------------------------------------------------------------------*/
;*    sort-columns ...                                                 */
;*---------------------------------------------------------------------*/
(define (sort-columns columns)
   (sort columns (lambda (c1 c2)
		    (with-access::$sqltiny-column c1 ((name1 name))
		       (with-access::$sqltiny-column c2 ((name2 name))
			  (string<? name1 name2))))))

;*---------------------------------------------------------------------*/
;*    index-columns! ...                                               */
;*---------------------------------------------------------------------*/
(define (index-columns! columns)
   (let loop ((columns columns)
	      (i 0))
      (when (pair? columns)
	 (with-access::$sqltiny-column (car columns) (index)
	    (set! index i)
	    (loop (cdr columns) (+fx i 1)))))
   columns)

;*---------------------------------------------------------------------*/
;*    sqltiny-drop-table ...                                           */
;*---------------------------------------------------------------------*/
(define (sqltiny-drop-table obj builtin table-name if-exists)
   (with-access::$sqltiny builtin (mutex)
      (synchronize mutex
	 (let ((tbl (sqltiny-get-table builtin table-name)))
	    (if (not (isa? tbl $sqltiny-table))
		(unless if-exists
		   (raise
		      (instantiate::&error
			 (proc "sqltiny-drop-table")
			 (msg (format "SQL error: no such table: ~a" table-name))
			 (obj obj))))
		(begin
		   (with-access::$sqltiny builtin (tables sync)
		      (set! tables (remq! tbl tables))
		      (unless (eq? sync 'ondemand)
			 (%sqltiny-flush obj builtin)))))))
      (sqltiny-delete obj builtin
	 "sqlite_master"
	 `(binary = ,table-name (colref "*" "name")))))

;*---------------------------------------------------------------------*/
;*    sqltiny-get-table ...                                            */
;*---------------------------------------------------------------------*/
(define (sqltiny-get-table builtin table-name)
   (with-access::$sqltiny builtin (tables)
      (let loop ((tables tables))
	 (cond
	    ((null? tables)
	     #f)
	    ((string=? (with-access::$sqltiny-table (car tables) (name) name)
		table-name)
	     (car tables))
	    (else
	     (loop (cdr tables)))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-insert ...                                               */
;*---------------------------------------------------------------------*/
(define (sqltiny-insert obj builtin table-name column-names vals replacep)
   (let ((tbl (sqltiny-get-table builtin table-name)))
      (unless tbl
	 (raise
	    (instantiate::&error
	       (proc "sqltiny-insert")
	       (msg (format "SQL error: no such table: ~a" table-name))
	       (obj obj))))
      (let* ((lv (length vals))
	     (cols (cond
		      ((pair? column-names)
		       (unless (=fx (length column-names) lv)
			  (raise
			     (instantiate::&error
				(proc "sqltiny-insert")
				(msg (format "SQL error: ~a values for ~a columns"
					lv (length column-names)))
				(obj obj))))
		       column-names)
		      ((>fx lv (length (with-access::$sqltiny-table tbl (*columns)
					  *columns)))
		       (raise
			  (instantiate::&error
			     (proc "sqltiny-insert")
			     (msg (format "SQL error: table ~a has ~a columns but ~a values were supplied"
				     table-name
				     (length (with-access::$sqltiny-table tbl
						   (*columns) *columns))
				     lv))
			     (obj obj))))
		      (else
		       (map (lambda (c)
			       (with-access::$sqltiny-column c (name) name))
			  (take (with-access::$sqltiny-table tbl (*columns)
				   *columns)
			     lv))))))
	 (insert obj builtin tbl cols vals replacep)))
   #f)

;*---------------------------------------------------------------------*/
;*    insert ...                                                       */
;*---------------------------------------------------------------------*/
(define (insert obj builtin table column-names vals replacep)
   (with-access::$sqltiny-table table (name rowid columns rows last-row-pair
					    mutex keycheck)
      (let* ((len (length columns))
	     (row (apply vector
		     (map (lambda (c)
			     (with-access::$sqltiny-column c (default)
				default))
			columns)))
	     (cols (sort (map cons column-names vals)
			 (lambda (d1 d2)
			    (string<? (car d1) (car d2))))))
	 (let loop ((cols cols)
		    (columns (cdr columns)))
	    (cond
	       ((null? cols)
		(synchronize mutex
		   (when (keycheck obj row rows replacep)
		      (set! rowid (+fx 1 rowid))
		      (vector-set! row 0 rowid)
		      (let ((last (cons row '())))
			 (if (pair? last-row-pair)
			     (set-cdr! last-row-pair last)
			     (set! rows last))
			 (set! last-row-pair last)))
		   (unless (eq? (with-access::$sqltiny builtin (sync) sync)
			      'ondemand)
		      (%sqltiny-flush obj builtin))))
	       ((null? columns)
		(when (pair? cols)
		   (raise
		    (instantiate::&error
		       (proc "sqltiny-insert")
		       (msg (format "SQL error: table ~a has no column named ~a"
				    name (caar cols)))
		       (obj obj)))))
	       (else
		(let ((c (string-compare3
			  (caar cols)
			  (with-access::$sqltiny-column (car columns) (name)
			     name))))
		   (cond
		      ((=fx c 0)
		       (with-access::$sqltiny-column (car columns) (index)
			  (vector-set! row index (cdar cols))
			  (loop (cdr cols) (cdr columns))))
		      ((>fx c 0)
		       (loop cols (cdr columns)))
		      (else
		       (raise
			(instantiate::&error
			   (proc "sqltiny-insert")
			   (msg (format "SQL error: table ~a has no column named ~a"
					name (caar cols)))
			   (obj obj))))))))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-begin-transaction! ...                                   */
;*    -------------------------------------------------------------    */
;*    Transaction are not implemented yet. No db lock is aquired yet.  */
;*---------------------------------------------------------------------*/
(define (sqltiny-begin-transaction! obj builtin)
   (synchronize *sqltiny-mutex*
      (with-access::$sqltiny builtin (transaction mutex)
	 (if transaction
	     (raise
		(instantiate::&error
		   (proc "sqltiny-begin-transaction!")
		   (msg (format "SQL error: cannot start a transaction within a transaction"))
		   (obj obj)))
	     (set! transaction #t))))
   #f)
		     
;*---------------------------------------------------------------------*/
;*    sqltiny-end-transaction! ...                                     */
;*---------------------------------------------------------------------*/
(define (sqltiny-end-transaction! obj builtin)
   (synchronize *sqltiny-mutex*
      (with-access::$sqltiny builtin (transaction mutex)
	 (if (not transaction)
	     (raise
		(instantiate::&error
		   (proc "sqltiny-end-transaction!")
		   (msg (format "SQL error: cannot commit - no transaction is active"))
		   (obj obj)))
	     (set! transaction #f))))
   #f)

;*---------------------------------------------------------------------*/
;*    sqltiny-select ...                                               */
;*---------------------------------------------------------------------*/
(define (sqltiny-select obj builtin stmt)
   (let* ((select-c (compile-expr stmt '() obj builtin))
	  (rows (select-c '())))
      (map (lambda (row)
	      (map! (lambda (v) (if (integer? v) (integer->string v) v)) row))
	   rows)))

;*---------------------------------------------------------------------*/
;*    sqltiny-update ...                                               */
;*---------------------------------------------------------------------*/
(define (sqltiny-update obj builtin table-name stmt assignments)
   (let* ((select-c (compile-expr stmt '() obj builtin))
	  (rows (select-c '()))
	  (tbl (sqltiny-get-table builtin table-name)))
      (unless tbl
	 (raise
	    (instantiate::&error
	       (proc "sqltiny-update")
	       (msg (format "SQL error: no such table: ~a" table-name))
	       (obj obj))))
      (for-each (lambda (row)
		   (for-each (lambda (a)
				(let ((o (column-offset obj tbl (car a))))
				   (vector-set! row o (cdr a))))
		      assignments))
	 rows)
      (with-access::$sqltiny builtin (sync mutex)
	 (unless (eq? sync 'ondemand)
	    (synchronize mutex
	       (%sqltiny-flush obj builtin))))
      '()))

;*---------------------------------------------------------------------*/
;*    compile-select ...                                               */
;*---------------------------------------------------------------------*/
(define (compile-select tnames result where group orderby distinct
			limit env obj builtin)
   (let* ((frame (map (lambda (t) (make-select-frame t obj builtin)) tnames))
	  (nenv (append frame env))
	  (table (prod (map (lambda (b)
			       (with-access::$sqltiny-table (cdr b) (rows)
				  rows))
			  frame)))
	  (where-c (compile-expr where nenv obj builtin))
	  (group-c (map (lambda (g) (compile-expr g nenv obj builtin)) group))
	  (order-c (compile-order-by orderby nenv obj builtin))
	  (rows->groups (cond
			   ((and (null? group) (null? orderby))
			    (lambda (rows)
			       (list rows)))
			   ((null? group)
			    (lambda (rows)
			       (list (sort rows order-c))))
			   ((null? orderby)
			    (lambda (rows)
			       (select-group-by rows group-c)))
			   (else
			    (lambda (rows)
			       (let ((gs (select-group-by rows group-c)))
				  (map (lambda (g) (sort g order-c)) gs)))))))
      (multiple-value-bind (print-c aggregation)
	 (compile-select-results result nenv obj builtin)
	 (let ((grps->res (cond
			     (aggregation
			      (lambda (groups)
				 (filter-map (lambda (g)
						(when (pair? g)
						   (make-select-result-row
						    (car g) g print-c)))
					     groups)))
			     ((pair? group)
			      (lambda (groups)
				 (filter-map (lambda (g)
						(when (pair? g)
						   (make-select-result-row
						    (car g) group print-c)))
					     groups)))
			     (else
			      (lambda (groups)
				 (map (lambda (r)
					 (make-select-result-row
					  r group print-c))
				      (car groups))))))
	       (limit (match-case limit
			 (#f
			  (lambda (rows)
			     rows))
			 ((and (? fixnum?) ?num)
			  (lambda (rows)
			     (let ((len (length rows)))
				(if (>fx len num)
				    (take rows num)
				    rows))))
			 ((?num . ?offset)
			  (lambda (rows)
			     (let ((len (length rows)))
				(cond
				   ((>fx offset len)
				    '())
				   ((> (- num offset) len)
				    (drop rows offset))
				   (else
				    (take (drop rows offset) num)))))))))
	    (lambda (rows)
	       (let* ((rows (filter-iter (lambda (r)
					    (where-c (append r rows)))
					 table))
		      (groups (rows->groups rows))
		      (res0 (grps->res groups))
		      (res1 (if distinct
				(select-distinguish res0)
				res0)))
		  (limit res1)))))))

;*---------------------------------------------------------------------*/
;*    compile-update ...                                               */
;*---------------------------------------------------------------------*/
(define (compile-update tname where env obj builtin)
   (let* ((frame (list (make-select-frame (cons tname tname) obj builtin)))
	  (nenv (append frame env))
	  (table (prod (map (lambda (b)
			       (with-access::$sqltiny-table (cdr b) (rows)
				  rows))
			  frame)))
	  (where-c (compile-expr where nenv obj builtin)))
      (lambda (rows)
	 (apply append
		(filter-iter (lambda (r) (where-c (append r rows))) table)))))

;*---------------------------------------------------------------------*/
;*    filter-iter ...                                                  */
;*---------------------------------------------------------------------*/
(define (filter-iter pred lst)
   (let loop ((lst lst)
	      (res '()))
      (cond
	 ((null? lst)
	  (reverse! res))
	 ((pred (car lst))
	  (loop (cdr lst) (cons (car lst) res)))
	 (else
	  (loop (cdr lst) res)))))

;*---------------------------------------------------------------------*/
;*    make-select-result-row ...                                       */
;*---------------------------------------------------------------------*/
(define (make-select-result-row row rows print-c)
   (append-map (lambda (p) (p row rows)) print-c))

;*---------------------------------------------------------------------*/
;*    make-select-frame ...                                            */
;*---------------------------------------------------------------------*/
(define (make-select-frame table-name obj builtin)
   (let* ((n (car table-name))
	  (t (sqltiny-get-table builtin n)))
      (if (not (isa? t $sqltiny-table))
	  (raise
	   (instantiate::&error
	      (proc "sqltiny-select")
	      (msg (format "SQL error: no such table: ~a" n))
	      (obj obj)))
	  (cons (cdr table-name) t))))

;*---------------------------------------------------------------------*/
;*    compile-order-by ...                                             */
;*---------------------------------------------------------------------*/
(define (compile-order-by order env obj builtin)
   (cond
      ((not (pair? order))
       #f)
      ((null? (cdr order))
       (let ((t (compile-expr (caar order) env obj builtin))
	     (c (if (eq? (cdar order) 'desc) sqltiny> sqltiny<)))
	  (lambda (v1 v2)
	     (c (t v1) (t v2)))))
      (else
       (let ((tests (map (lambda (o)
			    (list
			     (compile-expr (car o) env obj builtin)
			     (if (eq? (cdr o) 'desc) sqltiny> sqltiny<)
			     (if (eq? (cdr o) 'desc) sqltiny< sqltiny>)))
			 order)))
	  (lambda (v1 v2)
	     (let loop ((tests tests))
		(if (null? tests)
		    #f
		    (let* ((o (car tests))
			   (e (car o))
			   (c1 (cadr o))
			   (c2 (caddr o))
			   (e1 (e v1))
			   (e2 (e v2)))
		       (or (c1 e1 e2)
			   (if (c2 e1 e2)
			       #f
			       (loop (cdr tests))))))))))))

;*---------------------------------------------------------------------*/
;*    select-distinguish ...                                           */
;*---------------------------------------------------------------------*/
(define (select-distinguish rows)
   (let loop ((els rows)
	      (res '())
	      (rem #f))
      (cond
	 ((null? els)
	  (if rem (reverse! res) rows))
	 ((member (car els) res)
	  (loop (cdr els) res #t))
	 (else
	  (loop (cdr els) (cons (car els) res) rem)))))
   
;*---------------------------------------------------------------------*/
;*    select-group-by ...                                              */
;*    -------------------------------------------------------------    */
;*    This function assumes that group-c is not null.                  */
;*---------------------------------------------------------------------*/
(define (select-group-by rows group-c)
   (define (row-key row)
      (map (lambda (g) (g row)) group-c))
   (define (make-group key row)
      (cons key (list row)))
   (define (group-rows group)
      (reverse! (cdr group)))
   (define (group-add-row! group row)
      (set-cdr! group (cons row (cdr group))))
   (define (find-group groups key)
      (assoc key groups))
   (if (null? rows)
       (list rows)
       (let loop ((rows rows)
		  (groups '()))
	  (if (null? rows)
	      (map group-rows (reverse! groups))
	      (let* ((row (car rows))
		     (key (row-key row))
		     (grp (find-group groups key)))
		 (if grp
		     (begin
			(group-add-row! grp row)
			(loop (cdr rows) groups))
		     (loop (cdr rows) (cons (make-group key row) groups))))))))

;*---------------------------------------------------------------------*/
;*    prod ...                                                         */
;*---------------------------------------------------------------------*/
(define (prod ls)
   (cond
      ((null? ls)
       '())
      ((null? (cdr ls))
       (map list (car ls)))
      (else
       (let ((l0 (car ls))
	     (lr (prod (cdr ls))))
	  (append-map (lambda (c)
			 (map (lambda (l) (cons c l)) lr))
		      l0)))))

;*---------------------------------------------------------------------*/
;*    compile-select-results ...                                       */
;*---------------------------------------------------------------------*/
(define (compile-select-results results env obj builtin)
   (let loop ((results results)
	      (compilers '())
	      (aggreg #f))
      (if (null? results)
	  (values (reverse! compilers) aggreg)
	  (multiple-value-bind (c a)
	     (compile-select-result (car results) env obj builtin)
	     (loop (cdr results) (cons c compilers) (or a aggreg))))))

;*---------------------------------------------------------------------*/
;*    compile-select-result ...                                        */
;*---------------------------------------------------------------------*/
(define (compile-select-result expr env obj builtin)
   (match-case expr
      (*
       (let ((tbl (cdr (car env))))
	  (values 
	   (lambda (row rows)
	      (append-map (lambda (v)
			     (map (lambda (col)
				     (with-access::$sqltiny-column col (index)
					(vector-ref v index)))
				(with-access::$sqltiny-table tbl (*columns)
				   *columns)))
			  row))
	   #f)))
      ((?name . *)
       (let ((i (table-offset-in-list obj env name)))
	  (values 
	   (lambda (row rows)
	      (cdr (vector->list (list-ref row i))))
	   #f)))
      ((colref ?table-name ?name)
       (multiple-value-bind (i j)
	  (find-column-offset obj env table-name name)
	  (values 
	   (lambda (row rows)
	      (list (vector-ref (list-ref row i) j)))
	   #f)))
      ((funcall ?aggregator (colref ?table-name ?name))
       (multiple-value-bind (i j)
	  (find-column-offset obj env table-name name)
	  (let ((op (case aggregator
		       ((max)
			sqltinymax)
		       ((min)
			sqltinymin)
		       ((sum)
			(lambda (l) (apply + l)))
		       (else
			(raise
			 (instantiate::&error
			    (proc "sqltiny-select-result")
			    (msg (format "SQL error: not implemented ~a" expr))
			    (obj obj)))))))
	     (values 
	      (lambda (row rows)
		 (let ((as (map (lambda (r)
				   (vector-ref (list-ref r i) j))
				rows)))
		    (list (op as))))
	      #t))))
      (else
       (raise
	(instantiate::&error
	   (proc "sqltiny-select-result")
	   (msg (format "SQL error: not implemented ~a" expr))
	   (obj obj))))))

;*---------------------------------------------------------------------*/
;*    compile-expr ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-expr expr env obj builtin)
   (match-case expr
      ((or #t (? integer?) (? string?))
       (lambda (rows) expr))
      ((colref ?table-name ?col-name)
       (multiple-value-bind (i j)
	  (find-column-offset obj env table-name col-name)
	  (lambda (rows)
	     (vector-ref (list-ref rows i) j))))
      ((isnull ?expr)
       (let ((c1 (compile-expr expr env obj builtin)))
	  (lambda (rows)
	     (eq? (c1 rows) #unspecified))))
      ((notnull ?expr)
       (let ((c1 (compile-expr expr env obj builtin)))
	  (lambda (rows)
	     (not (eq? (c1 rows) #unspecified)))))
      ((binary ?op ?expr1 ?expr2)
       (let ((c1 (compile-expr expr1 env obj builtin))
	     (c2 (compile-expr expr2 env obj builtin)))
	  (case op
	     ((=)
	      (lambda (rows)
		 (equal? (c1 rows) (c2 rows))))
	     ((>=)
	      (lambda (rows)
		 (sqltiny>= (c1 rows) (c2 rows))))
	     ((>)
	      (lambda (rows)
		 (sqltiny> (c1 rows) (c2 rows))))
	     ((<=)
	      (lambda (rows)
		 (sqltiny<= (c1 rows) (c2 rows))))
	     ((<)
	      (lambda (rows)
		 (sqltiny< (c1 rows) (c2 rows))))
	     ((<> !=)
	      (lambda (rows)
		 (not (equal? (c1 rows) (c2 rows)))))
	     ((or)
	      (lambda (rows)
		 (or (c1 rows) (c2 rows))))
	     ((and)
	      (lambda (rows)
		 (and (c1 rows) (c2 rows))))
	     (else
	      (raise
	       (instantiate::&error
		  (proc "compile-expr")
		  (msg (format "SQL error: not implemented ~a" expr))
		  (obj obj)))))))
      ((is-select ?select-statement)
       (let ((select-c (compile-expr select-statement env obj builtin)))
	  (lambda (rows)
	     (let ((val (select-c rows)))
		(if (or (null? val) (null? (car val)))
		    #unspecified
		    (caar val))))))
      ((in-select ?op ?expr ?select-statement)
       (let ((expr-c (compile-expr expr env obj builtin))
	     (select-c (compile-expr select-statement env obj builtin)))
	  (lambda (rows)
	     (let* ((vals (select-c rows))
		    (val (expr-c rows))
		    (ret (pair? (assoc val vals))))
		(if (eq? op 'in) ret (not ret))))))
      ((select ?distinct ?result ?tnames ?where ?group ?order ?lim)
       (compile-select tnames result where group order distinct lim
		       env obj builtin))
      ((update ?tname ?where)
       (compile-update tname where env obj builtin))
      ((in-value ?expr ?value-list)
       (let ((c (compile-expr expr env obj builtin)))
	  (lambda (rows)
	     (let ((val (c rows)))
		(member val value-list)))))
      ((like ?op ?no ?string ?pattern)
       (let ((cs (compile-expr string env obj builtin))
	     (cp (compile-expr pattern env obj builtin)))
	  (case op
	     ((LIKE)
	      (if (eq? no 'not)
		  (lambda (rows)
		     (let* ((vstr (cs rows))
			    (vpat (cp rows)))
			(not (pregexp-match (like->regexp vpat) vstr))))
		  (lambda (rows)
		     (let* ((vstr (cs rows))
			    (vpat (cp rows)))
			(pregexp-match (like->regexp vpat) vstr)))))
	     ((GLOB)
	      (raise
	       (instantiate::&error
		  (proc "compile-expr")
		  (msg (format "SQL error: not implemented ~a" expr))
		  (obj obj))))
	     ((REGEXP)
	      (if (eq? no 'not)
		  (lambda (rows)
		     (let* ((vstr (cs rows))
			    (vpat (cp rows)))
			(not (pregexp-match vpat vstr))))
		  (lambda (rows)
		     (let* ((vstr (cs rows))
			    (vpat (cp rows)))
			(pregexp-match vpat vstr)))))
	     ((MATCH)
	      (raise
	       (instantiate::&error
		  (proc "compile-expr")
		  (msg (format "SQL error: not implemented ~a" expr))
		  (obj obj)))))))
      (else
       (raise
	(instantiate::&error
	   (proc "compile-expr")
	   (msg (format "SQL error: not implemented ~a" expr))
	   (obj obj))))))

;*---------------------------------------------------------------------*/
;*    like->regexp ...                                                 */
;*---------------------------------------------------------------------*/
(define (like->regexp str)
   (let ((len (string-length str)))
      (let loop ((i 0)
		 (nlen len))
	 (cond
	    ((=fx i len)
	     (let ((res (make-string nlen)))
		(let loop ((r 0)
			   (w 0))
		   (if (=fx r len)
		       res
		       (let ((c (string-ref str r)))
			  (cond
			     ((char=? c #\%)
			      (string-set! res w #\.)
			      (string-set! res (+ w 1) #\*)
			      (loop (+fx r 1) (+fx w 2)))
			     ((char=? c #\_)
			      (string-set! res w #\.)
			      (loop (+fx r 1) (+fx w 1)))
			     (else
			      (string-set! res w c)
			      (loop (+fx r 1) (+fx w 1)))))))))
	    ((char=? (string-ref str i) #\%)
	     (loop (+fx i 1) (+fx nlen 1)))
	    (else
	     (loop (+fx i 1) nlen))))))
	     
;*---------------------------------------------------------------------*/
;*    binop ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (define-sqltiny-binop op)
   (let ((id (symbol-append 'sqltiny op))
	 (fxid (symbol-append op 'fx))
	 (stringid (symbol-append 'string op '?)))
      `(define (,id v1 v2)
	  (cond
	     ((fixnum? v1) (and (fixnum? v2) (,fxid v1 v2)))
	     ((string? v1) (and (string? v2) (,stringid v1 v2)))
	     (else #f)))))

(define-sqltiny-binop >=)
(define-sqltiny-binop >)
(define-sqltiny-binop <)
(define-sqltiny-binop <=)

;*---------------------------------------------------------------------*/
;*    sqltinyminmax ...                                                */
;*---------------------------------------------------------------------*/
(define (sqltinyminmax op lst)
   (if (null? lst)
       #f
       (let loop ((lst (cdr lst))
		  (m (car lst)))
	  (cond
	     ((null? lst)
	      m)
	     ((op (car lst) m)
	      (loop (cdr lst) (car lst)))
	     (else
	      (loop (cdr lst) m))))))

(define (sqltinymax lst)
   (sqltinyminmax sqltiny> lst))

(define (sqltinymin lst)
   (sqltinyminmax sqltiny< lst))

;*---------------------------------------------------------------------*/
;*    table-offset-in-list ...                                         */
;*---------------------------------------------------------------------*/
(define (table-offset-in-list obj env name)
   (if (string=? name "*")
       0
       (let loop ((i 0)
		  (env env))
	  (cond
	     ((or (null? env) (null? (car env)))
	      (raise
	       (instantiate::&error
		  (proc "sqltiny-insert")
		  (msg (format "SQL error: no such table: ~a" name))
		  (obj obj))))
	     ((string=? name (caar env))
	      i)
	     (else
	      (loop (+fx i 1) (cdr env)))))))

;*---------------------------------------------------------------------*/
;*    find-column-offset ...                                           */
;*---------------------------------------------------------------------*/
(define (find-column-offset obj env table-name column-name)
   (if (string=? table-name "*")
       (let loop ((i 0)
		  (env env))
	  (if (null? env)
	      (raise
	       (instantiate::&error
		  (proc "find-column-offset")
		  (msg (format "SQL error: no such column: ~a" column-name))
		  (obj obj)))
	      (let* ((t (cdr (car env)))
		     (j (column-offset obj t column-name)))
		 (if j
		     (values i j)
		     (loop (+fx i 1) (cdr env))))))
       (let* ((i (table-offset-in-list obj env table-name))
	      (t (cdr (list-ref env i)))
	      (j (column-offset obj t column-name)))
	  (unless j
	     (raise
	      (instantiate::&error
		 (proc "find-column-offset")
		 (msg (format "SQL error: no such column `~a' in table: ~a"
			      column-name
			      table-name))
		 (obj obj))))
	  (values i j))))

;*---------------------------------------------------------------------*/
;*    column-offset ...                                                */
;*---------------------------------------------------------------------*/
(define (column-offset obj table name)
   (with-access::$sqltiny-table table (columns (table-name name))
      (let loop ((columns columns)
		 (i 0))
	 (cond
	    ((null? columns)
	     #f)
	    ((string=? name
		(with-access::$sqltiny-column (car columns) (name) name))
	     i)
	    (else
	     (loop (cdr columns) (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-delete ...                                               */
;*---------------------------------------------------------------------*/
(define (sqltiny-delete obj builtin table-name where-expr)
   (let ((tbl (sqltiny-get-table builtin table-name))
	 (glock (with-access::$sqltiny builtin (mutex) mutex)))
      (unless tbl
	 (raise
	    (instantiate::&error
	       (proc "sqltiny-delete")
	       (msg (format "SQL error: no such table: ~a" table-name))
	       (obj obj))))
      
      (let* ((env (list (cons table-name tbl)))
	     (pred (compile-expr where-expr env obj builtin)))
	 (synchronize glock
	    (let* ((r (prod (map (lambda (b)
				    (with-access::$sqltiny-table (cdr b) (rows)
				       rows))
			       env)))
		   (rows (filter! pred r)))
	       (with-access::$sqltiny-table tbl ((table-rows rows) last-row-pair)
		  (let loop ((trows table-rows)
			     (drows rows)
			     (prev '()))
		     (cond
			((pair? drows)
			 (let ((rowid (vector-ref (caar drows) 0)))
			    (if (=fx rowid (vector-ref (car trows) 0))
				(if (null? prev)
				    (begin
				       (set! table-rows (cdr trows))
				       (loop (cdr trows) (cdr drows) '()))
				    (begin
				       (set-cdr! prev (cdr trows))
				       (loop (cdr trows) (cdr drows) prev)))
				(loop (cdr trows)
				   drows
				   trows))))
			((pair? prev)
			 (when (null? (cdr prev)) (set! last-row-pair prev)))
			((null? table-rows)
			 (set! last-row-pair '()))
			(else
			 (assert () (eq? last-row-pair
				       (last-pair table-rows)))))))
	       (with-access::$sqltiny builtin (sync)
		  (unless (eq? sync 'ondemand)
		     (%sqltiny-flush obj builtin)))))
	 #f)))

;*---------------------------------------------------------------------*/
;*    sqltiny-table-info ...                                           */
;*---------------------------------------------------------------------*/
(define (sqltiny-table-info obj builtin table-name)
   (let ((table (sqltiny-get-table builtin table-name)))
      (unless table
	 (raise
	  (instantiate::&error
	     (proc "sqltiny-insert")
	     (msg (format "SQL error: no such table: ~a" table-name))
	     (obj obj))))
      (map (lambda (c)
	      (with-access::$sqltiny-column c (name)
		 (list 0 name)))
	 (with-access::$sqltiny-table  table (*columns) *columns))))

;*---------------------------------------------------------------------*/
;*    %sqltiny-flush ...                                               */
;*    -------------------------------------------------------------    */
;*    This function assumes that the builtin lock is already acquired. */
;*---------------------------------------------------------------------*/
(define (%sqltiny-flush obj builtin)
   (with-access::$sqltiny builtin (path)
      (unless (string=? path ":memory:")
	 (let ((p (open-output-binary-file path)))
	    (if (binary-port? p)
		(unwind-protect
		   (output-obj p builtin)
		   (close-binary-port p))
		(raise (instantiate::&io-port-error
			  (proc 'sqltiny-flush)
			  (msg "Cannot open file for output")
			  (obj path))))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-vacuum ...                                               */
;*---------------------------------------------------------------------*/
(define (sqltiny-vacuum obj builtin)
   (with-access::$sqltiny builtin (path mutex)
      (synchronize mutex
	 (%sqltiny-flush obj builtin))))

;*---------------------------------------------------------------------*/
;*    sqltiny-alter ...                                                */
;*---------------------------------------------------------------------*/
(define (sqltiny-alter obj builtin table-name proc)
   (with-access::$sqltiny builtin (mutex sync)
      (synchronize mutex
	 (let ((tbl (sqltiny-get-table builtin table-name)))
	    (unless tbl
	       (raise
		  (instantiate::&error
		     (proc "create-table")
		     (msg (format "SQL error: no such table: ~s" table-name))
		     (obj obj))))
	    (proc obj builtin tbl)
	    (unless (eq? sync 'ondemand) (%sqltiny-flush obj builtin))))))

;*---------------------------------------------------------------------*/
;*    sqltiny-add-column! ...                                          */
;*---------------------------------------------------------------------*/
(define (sqltiny-add-column! obj builtin tbl col)
   (with-access::$sqltiny-table tbl (columns *columns rows last-row-pair)
      (table-set-columns! obj tbl
	 (append! columns (list col))
	 (append! *columns (list col)))
      (with-access::$sqltiny-column col (default)
	 (let ((len (length columns)))
	    (set! rows (map (lambda (row)
			       (let ((v (make-vector len default)))
				  (vector-copy! v 0 row)
				  v))
			  rows))
	    (set! last-row-pair (last-pair rows))))))
