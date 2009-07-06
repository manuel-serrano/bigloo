;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/src/Llib/parser.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 18 08:30:50 2007                          */
;*    Last change :  Wed Nov 28 08:54:46 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Portable sqltiny parser                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_parser
   
   (option (set! *optim-O-macro?* #t)
	   (set! *unsafe-type* #t)
	   (set! *unsafe-arity* #t)
	   (set! *unsafe-range* #t))
   
   (import __sqlite_sqltiny
	   __sqlite_engine)
   
   (export sqltiny-parser))

;*---------------------------------------------------------------------*/
;*    type-default ...                                                 */
;*---------------------------------------------------------------------*/
(define type-default
   '((INTEGER . 0)
     (STRING . "")))

;*---------------------------------------------------------------------*/
;*    not-implemented ...                                              */
;*---------------------------------------------------------------------*/
(define (not-implemented msg)
   (lambda (obj builtin)
      (raise (instantiate::&error
		(proc 'sqltiny-portable)
		(msg (string-append "Not implemented: " msg))
		(obj obj)))))

;*---------------------------------------------------------------------*/
;*    sqltiny-parser ...                                               */
;*---------------------------------------------------------------------*/
(define sqltiny-parser
   (lalr-grammar
      
      ;; tokens
      (CREATE DROP ALTER TABLE
	      INDEX TRIGGER VIEW
	      SEMI-COMMA COMMA PAR-OPEN PAR-CLO NAME RENAME TO ADD COLUMN
	      CONSTRAINT NULL ISNULL NOTNULL
	      ON CONFLICT PRIMARY KEY UNIQUE CHECK
	      DEFAULT COLLATE AUTOINCREMENT
	      DOT ASC DESC
	      BEGIN END TRANSACTION
	      INSERT INTO VALUES AS OR IF EXISTS
	      DELETE SET PRAGMA REPLACE UPDATE VACUUM
	      SELECT ALL DISTINCT FROM WHERE GROUP BY HAVING ORDER LIMIT OFFSET
	      UNION INTERSECT EXCEPT
	      JOIN-OP USING
	      INTEGER STRING
	      (left: IN)
	      (left: STAR)
	      (left: EGAL)
	      (left: BINARY-OP)
	      (left: LIKE-OP)
	      (right: UNARY-OP)
	      (right: NOT))
      
      ;; main rule
      (start
       (()
	'())
       ((command)
	(list command))
       ((command SEMI-COMMA start)
	(cons command start)))
      
      ;; commands
      (command
       ((alter-table)
	alter-table)
       ((begin-transaction)
	(lambda (obj builtin)
	   (sqltiny-begin-transaction! obj builtin)))
       ((create-table)
	create-table)
       ((delete)
	delete)
       ((drop)
	drop)
       ((end-transaction)
	(lambda (obj builtin)
	   (sqltiny-end-transaction! obj builtin)))
       ((insert)
	insert)
       ((pragma)
	pragma)
       ((replace)
	replace)
       ((select-statement)
	(lambda (obj builtin)
	   (sqltiny-select obj builtin select-statement)))
       ((update)
	update)
       ((vacuum)
	sqltiny-vacuum))
      
      ;; alter-table
      (alter-table
       ((ALTER TABLE NAME alteration)
	(lambda (obj builtin)
	   (sqltiny-alter obj builtin (car NAME) alteration))))
      
      ;; alteration
      (alteration
       ((RENAME TO NAME)
	#unspecified)
       ((ADD column-def)
	(lambda (obj builtin tbl)
	   (sqltiny-add-column! obj builtin tbl column-def)))
       ((ADD COLUMN column-def)
	(lambda (obj builtin tbl)
	   (sqltiny-add-column! obj builtin tbl column-def))))
      
      ;; begin-transaction
      (begin-transaction
       ((BEGIN)
	#unspecified)
       ((BEGIN TRANSACTION)
	#unspecified)
       ((BEGIN TRANSACTION NAME)
	#unspecified))
      
      ;; create-table
      (create-table
       ((CREATE TABLE NAME PAR-OPEN column-defs PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-create-table obj builtin
				 (car NAME) column-defs '())))
       ((CREATE TABLE NAME PAR-OPEN column-defs COMMA constraints PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-create-table obj builtin
				 (car NAME) column-defs constraints)))
       ((CREATE TABLE NAME IF NOT EXISTS PAR-OPEN column-defs PAR-CLO)
	(lambda (obj builtin)
	   (unless (sqltiny-get-table builtin (car NAME))
	      (sqltiny-create-table obj builtin
				    (car NAME) column-defs '()))))
       ((CREATE TABLE NAME IF NOT EXISTS PAR-OPEN column-defs
		COMMA constraints PAR-CLO)
	(lambda (obj builtin)
	   (unless (sqltiny-get-table builtin (car NAME))
	      (sqltiny-create-table obj builtin
				    (car NAME) column-defs constraints)))))
      
      ;; delete
      (delete
       ((DELETE FROM NAME WHERE expr)
	(lambda (obj builtin)
	   (sqltiny-delete obj builtin (car NAME) expr))))
      
      ;; drop
      (drop
       ((DROP TABLE IF EXISTS NAME)
	(lambda (obj builtin)
	   (sqltiny-drop-table obj builtin (car NAME) #t)))
       ((DROP TABLE NAME)
	(lambda (obj builtin)
	   (sqltiny-drop-table obj builtin (car NAME) #f)))
       ((DROP INDEX IF EXISTS NAME)
	(not-implemented "DROP INDEX ..."))
       ((DROP INDEX NAME)
	(not-implemented "DROP INDEX ..."))
       ((DROP TRIGGER IF EXISTS NAME)
	(not-implemented "DROP TRIGGER ..."))
       ((DROP TRIGGER NAME)
	(not-implemented "DROP TRIGGER ..."))
       ((DROP VIEW IF EXISTS NAME)
	(not-implemented "DROP VIEW ..."))
       ((DROP VIEW NAME)
	(not-implemented "DROP VIEW ...")))
      
      ;; end-transaction
      (end-transaction
       ((END)
	#unspecified)
       ((END TRANSACTION)
	#unspecified)
       ((END TRANSACTION NAME)
	#unspecified))
      
      ;; insert
      (insert
       ((INSERT INTO NAME PAR-OPEN column-list PAR-CLO VALUES
		PAR-OPEN value-list PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-insert obj builtin (car NAME) column-list value-list #f)))
       ((INSERT INTO NAME VALUES PAR-OPEN value-list PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-insert obj builtin (car NAME) #f value-list #f)))
       ((INSERT OR REPLACE INTO NAME PAR-OPEN column-list PAR-CLO VALUES
		PAR-OPEN value-list PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-insert obj builtin (car NAME) column-list value-list #t)))
       ((INSERT OR REPLACE INTO NAME VALUES PAR-OPEN value-list PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-insert obj builtin (car NAME) #f value-list #t)))
       ((INSERT INTO NAME select-statement)
	(not-implemented  "INSERT INTO ... SELECT ..."))
       ((INSERT INTO NAME PAR-OPEN column-list PAR-CLO select-statement)
	(not-implemented  "INSERT INTO ... SELECT ...")))
      
      ;; pragma
      (pragma
       ((PRAGMA NAME PAR-OPEN NAME@table-name PAR-CLO)
	(if (string=? (car NAME) "table_info")
	    (lambda (obj builtin)
	       (sqltiny-table-info obj builtin (car table-name)))
	    (not-implemented (string-append "PRAGMA " (car NAME) " ...")))))
      
      ;; replace
      (replace
       ((REPLACE INTO NAME PAR-OPEN column-list PAR-CLO
		 VALUES PAR-OPEN value-list PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-insert obj builtin (car NAME) column-list value-list #t)))
       ((REPLACE INTO NAME VALUES PAR-OPEN value-list PAR-CLO)
	(lambda (obj builtin)
	   (sqltiny-insert obj builtin (car NAME) #f value-list #t)))
       ((REPLACE INTO NAME PAR-OPEN column-list PAR-CLO select-statement)
	(not-implemented "REPLACE INTO ... SELECT ..."))
       ((REPLACE INTO NAME VALUES PAR-OPEN value-list PAR-CLO select-statement)
	(not-implemented "REPLACE INTO ... SELECT ...")))
      
      ;; update
      (update
       ((UPDATE BINARY-OP NAME NAME@table-name SET assignment-list WHERE expr)
	(not-implemented "UPDATE BINARY-OP ... WHERE ..."))
       ((UPDATE NAME@table-name SET assignment-list WHERE expr)
	(lambda (obj builtin)
	   (let ((update `(update ,(car table-name) ,expr)))
	      (sqltiny-update obj builtin
			      (car table-name)
			      update assignment-list)))))
      
      ;; vacuum
      (vacuum
       ((VACUUM)
	#unspecified)
       ((VACUUM NAME)
	#unspecified))
      
      ;; column-list
      (column-list
       ((NAME)
	(list (car NAME)))
       ((NAME COMMA column-list)
	(cons (car NAME) column-list)))
      
      ;; columns-defs
      (column-defs
       ((column-def)
	(list column-def))
       ((column-defs COMMA column-def)
	`(,@column-defs ,column-def)))
      
      ;; column-def
      (column-def
       ((NAME)
	(instantiate::$sqltiny-column
	   (name (car NAME))))
       ((NAME column-constraints)
	(let ((default (cond ((assq 'default column-constraints) => cdr)
			     ((assq 'string type-default) => cdr)
			     (else #unspecified)))
	      (primary (pair? (assq 'primary-key column-constraints))))
	   (instantiate::$sqltiny-column
	      (name (car NAME))
	      (primkey primary)
	      (default default))))
       ((NAME@name NAME@type)
	(instantiate::$sqltiny-column
	   (name (car name))
	   (type (string->symbol (car type)))))
       ((NAME@name NAME@type column-constraints)
	(let* ((ty (string->symbol (car type)))
	       (default (cond ((assq 'default column-constraints) => cdr)
			      ((assq ty type-default) => cdr)
			      (else #unspecified)))
	       (primary (pair? (assq 'primary-key column-constraints))))
	   (instantiate::$sqltiny-column
	      (name (car name))
	      (type ty)
	      (primkey primary)
	      (default default)))))
      
      ;; column-constraints
      (column-constraints
       ((column-constraint)
	(list column-constraint))
       ((column-constraint column-constraints)
	(cons column-constraint column-constraints))
       ((named-column-constraint)
	(list named-column-constraint))
       ((named-column-constraint column-constraints)
	(cons named-column-constraint column-constraints)))
      
      ;; named-column-constraint
      (named-column-constraint
       ((CONSTRAINT NAME column-constraint)
	(tprint 'todo)))
      ;; the SQLITE grammar is ambiguous so I enforce naming constraints
      ;; ((column-constraint)
      ;; (tprint 'todo)))
      
      ;; column-constraint
      (column-constraint
       ((column-constraint-conflict-clause)
	column-constraint-conflict-clause)
       ((column-constraint-primary-key)
	column-constraint-primary-key)
       ((column-constraint-unique)
	column-constraint-unique)
       ((column-constraint-check)
	column-constraint-check)
       ((column-constraint-default)
	column-constraint-default))
      
      ;; column-constraint-conflict-clause
      (column-constraint-conflict-clause
       ((NOT NULL conflict-clause)
	#unspecified))
      
      ;; column-constraint-primary-key
      (column-constraint-primary-key
       ((PRIMARY KEY)
	(cons 'primary-key #t))
       ((PRIMARY KEY sort-order)
	#unspecified)
       ((PRIMARY KEY sort-order AUTOINCREMENT)
	#unspecified)
       ((PRIMARY KEY conflict-clause)
	#unspecified)
       ((PRIMARY KEY conflict-clause AUTOINCREMENT)
	#unspecified)
       ((PRIMARY KEY sort-order conflict-clause)
	#unspecified)
       ((PRIMARY KEY sort-order conflict-clause AUTOINCREMENT)
	#unspecified))
      
      ;; column-constraint-unique
      (column-constraint-unique
       ((UNIQUE conflict-clause)))
      
      ;; column-constraint-check
      (column-constraint-check
       ((CHECK value)
	(cons 'check value)))
      
      ;; column-constraint-default
      (column-constraint-default
       ((DEFAULT value)
	(cons 'default value)))
      
      ;; column-constraint-collate
      (column-constraint-collate
       ((COLLATE NAME)))
      
      ;; conflict-clause
      (conflict-clause
       ((ON CONFLICT NAME)
	(tprint "todo: on-conflict")
	#unspecified))
      
      ;; conflict-algorith
      (conflict-algorithm
       ((NAME)
	(tprint "todo: conflict-algorithm")
	#unspecified))
      
      ;; constraints
      (constraints
       ((constraint)
	(list constraint))
       ((constraint COMMA constraints)
	(cons constraint constraints)))
      
      ;; constraint
      (constraint
       ((PRIMARY KEY PAR-OPEN name-list PAR-CLO)
	`(primary-key ,@name-list))
       ((PRIMARY KEY PAR-OPEN name-list PAR-CLO conflict-clause)
	`(primary-key ,@name-list))
       ((UNIQUE PAR-OPEN name-list PAR-CLO)
	`(primary-key ,@name-list))
       ((UNIQUE PAR-OPEN name-list PAR-CLO conflict-clause)
	`(primary-key ,@name-list))
       ((CHECK expr)
	(tprint "todo: check")
	#unspecified))
      
      ;; name-list
      (name-list 
       ((NAME)
	(list (car NAME)))
       ((NAME COMMA name-list)
	(cons (car NAME) name-list)))
      
      ;; sort-order
      (sort-order
       ((ASC)
	'asc)
       ((column-constraint-collate ASC)
	'asc)
       ((DESC)
	'desc)
       ((column-constraint-collate DESC)
	'desc))
      
      ;; expr-list
      (expr-list
       ((expr)
	(list expr))
       ((expr COMMA expr-list)
	(cons expr expr-list)))
      
      ;; expr
      (expr
       ((binary-expr)
	binary-expr)
       ((PAR-OPEN expr PAR-CLO)
	expr)
       ((value)
	value)
       ((expr ISNULL)
	`(isnull ,expr))
       ((expr NOTNULL)
	`(notnull ,expr))
       ((NAME)
	`(colref "*" ,(car NAME)))
       ((NAME@table-name DOT NAME@column-name)
	`(colref ,(car table-name) ,(car column-name)))
       ((NAME PAR-OPEN expr-list PAR-CLO)
	`(funcall ,(string->symbol (string-downcase (car NAME))) ,@expr-list))
       ((expr IN PAR-OPEN value-list PAR-CLO)
	`(in-value ,expr ,value-list))
       ((PAR-OPEN select-statement PAR-CLO)
	`(is-select ,select-statement))
       ((expr IN PAR-OPEN select-statement PAR-CLO)
	`(in-select in ,expr ,select-statement))
       ((expr NOT IN PAR-OPEN select-statement PAR-CLO)
	`(in-select not-in ,expr ,select-statement))
       ((expr IN NAME)
	`(in ,expr)))
      
      ;; binary-expr
      (binary-expr
       ((expr@expr1 BINARY-OP expr@expr2)
	`(binary ,(car BINARY-OP) ,expr1 ,expr2))
       ((expr@expr1 LIKE-OP expr@expr2)
	`(like ,(car LIKE-OP) - ,expr1 ,expr2))
       ((expr@expr1 NOT LIKE-OP expr@expr2)
	`(like ,(car LIKE-OP) not ,expr1 ,expr2))
       ((expr@expr1 EGAL expr@expr2)
	`(binary = ,expr1 ,expr2))
       ((expr@expr1 STAR expr@expr2)
	`(binary * ,expr1 ,expr2)))
      
      ;; select-statement
      (select-statement
       ((SELECT all-or-distinct result FROM table-list
		where group-by having compounds order-by limit)
	`(select ,(eq? all-or-distinct 'distinct)
		 ,result
		 ,table-list
		 ,where
		 ,group-by
		 ,order-by
		 ,limit)))
      
      ;; all-or-distinct
      (all-or-distinct
       (()
	'all)
       ((ALL)
	'all)
       ((DISTINCT)
	'distinct))
      
      ;; result
      (result
       ((result-column)
	(list result-column))
       ((result-column COMMA result)
	(cons result-column result)))
      
      ;; result-column
      (result-column
       ((STAR)
	'*)
       ((NAME DOT STAR)
	(cons (car NAME) '*))
       ((expr)
	expr)
       ((expr AS NAME)
	((not-implemented "expr AS NAME") #f #f)))
      
      ;; where
      (where
       (()
	#t)
       ((WHERE expr)
	expr))
      
      ;; group-by
      (group-by
       (()
	'())
       ((GROUP BY expr-list)
	expr-list))
      
      ;; having
      (having
       (()
	#unspecified)
       ((HAVING expr)
	((not-implemented "HAVING expr") #f #f)))

      ;; compounds
      (compounds
       (()
	'())
       ((compound-op select-statement)
	((not-implemented "COMPOUND-OP select ...") #f #f)))
      
      ;; compound-op
      (compound-op
       ((UNION)
	'union)
       ((UNION ALL)
	'union-all)
       ((INTERSECT)
	'intersect)
       ((EXCEPT)
	'except))
      
      ;; order-by
      (order-by
       (()
	'())
       ((ORDER BY sort-expr-list)
	sort-expr-list))
      
      ;; sort-expr-list
      (sort-expr-list
       ((expr)
	(list (cons expr 'asc)))
       ((expr sort-order)
	(list (cons expr sort-order)))
       ((expr COMMA sort-expr-list)
	(cons (cons expr 'asc) sort-expr-list))
       ((expr sort-order COMMA sort-expr-list)
	(cons (cons expr sort-order) sort-expr-list)))
      
      ;; limit
      (limit
       (()
	#f)
       ((LIMIT INTEGER)
	(car INTEGER))
       ((LIMIT INTEGER@offset COMMA INTEGER@len)
	(cons (car len) (car offset)))
       ((LIMIT INTEGER@len OFFSET INTEGER@offset)
	(cons (car len) (car offset))))
      
      ;; table-list
      (table-list
       ((table)
	(list table))
       ((table join-tables)
	(cons table join-tables)))
      
      ;; table
      (table
       ((NAME)
	(cons (car NAME) (car NAME)))
       ((NAME AS NAME@alias)
	(cons (car NAME) (car alias)))
       ((NAME NAME@alias)
	(cons (car NAME) (car alias))))
      
      ;; join-tables
      (join-tables
       ((join-op table join-args)
	(list table))
       ((join-op table join-args join-tables)
	(cons table join-tables)))
      
      ;; join-op
      (join-op
       ((COMMA)
	#unspecified)
       ((JOIN-OP)
	((not-implemented "JOIN-OP") #f #f)))
      
      ;; join-args
      (join-args
       (()
	'())
       ((ON expr)
	((not-implemented "ON expr") #f #f))
       ((ON expr USING PAR-OPEN name-list PAR-CLO)
	((not-implemented "ON expr USING ...") #f #f))
       ((USING PAR-OPEN name-list PAR-CLO)
	((not-implemented "USING ...") #f #f)))
      
      ;; value-list
      (value-list
       ((value)
	(list value))
       ((value-list COMMA value)
	`(,@value-list ,value)))
      
      ;; value
      (value
       ((NULL)
	#unspecified)
       ((INTEGER)
	(car INTEGER))
       ((STRING)
	(car STRING)))
      
      ;; assignment-list
      (assignment-list
       ((assignment)
	(list assignment))
       ((assignment COMMA assignment-list)
	(cons assignment assignment-list)))
      
      ;; assignment
      (assignment
       ((NAME EGAL expr)
	(cons (car NAME) expr)))))
