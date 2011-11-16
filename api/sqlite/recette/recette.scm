;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/recette/recette.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Wed Nov 16 10:22:44 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of Sqlite.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library sqlite)
   (main    main))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err . msg)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (for-each write msg)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    do-something-else ...                                            */
;*---------------------------------------------------------------------*/
(define (do-something-else)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let ((provided (with-handler
		      (lambda (e)
			 (error-notify e)
			 (vector res))
		      (prgm))))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (begin
	     (set! *success* (+fx 1 *success*))
	     (print "ok."))
	  (begin
	     (set! *failure* (cons name *failure*))
	     (print "error.")
	     (print "   ==> provided: [" (with-output-to-string
					    (lambda () (write provided)))
		    "]\n       expected: ["
		    (let ((r (if (procedure? res) (res 'result) res)))
		       (with-output-to-string
			  (lambda () (write r))))
		    "]")))))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))

;*---------------------------------------------------------------------*/
;*    cond-expand ...                                                  */
;*---------------------------------------------------------------------*/
(define-test cond-expand
   (cond-expand
      (sqlite #t)
      (sqltiny #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    *db* ...                                                         */
;*---------------------------------------------------------------------*/
(define *db* #unspecified)

;*---------------------------------------------------------------------*/
;*    create-db ...                                                    */
;*---------------------------------------------------------------------*/
(define-test create-db
   (begin
      (when (file-exists? "test.db") (delete-file "test.db"))
      (set! *db* (instantiate::sqlite (path "test.db")))
      *db*)
   :result (lambda (v)
	      (isa? *db* sqlite)))

;*---------------------------------------------------------------------*/
;*    create-table ...                                                 */
;*---------------------------------------------------------------------*/
(define-test create-table
   (sqlite-exec *db* "CREATE TABLE gee (x INTEGER, y INTEGER)"))

;*---------------------------------------------------------------------*/
;*    insert ...                                                       */
;*---------------------------------------------------------------------*/
(define-test insert
   (for-each (lambda (x)
		(sqlite-exec *db*  "INSERT INTO gee VALUES(~A, ~A)" x (* x x)))
	     (iota 10)))

;*---------------------------------------------------------------------*/
;*    select ...                                                       */
;*---------------------------------------------------------------------*/
(define-test select
   (sqlite-map  *db* vector "SELECT * FROM gee")
   :result '(#("0" "0")
	     #("1" "1")
	     #("2" "4")
	     #("3" "9")
	     #("4" "16")
	     #("5" "25")
	     #("6" "36")
	     #("7" "49")
	     #("8" "64")
	     #("9" "81")))

;*---------------------------------------------------------------------*/
;*    select.2 ...                                                     */
;*---------------------------------------------------------------------*/
(define-test select.2
   (sqlite-map *db* vector "SELECT y,x FROM gee WHERE y > 50")
   :result '(#("64" "8")
	     #("81" "9")))

;*---------------------------------------------------------------------*/
;*    create.2                                                         */
;*---------------------------------------------------------------------*/
(define-test create.2
   (sqlite-exec *db* "create table foo (f2, f1 PRIMARY KEY);
                      create table bar (b1,b2,b3);
                      create table hux (h1,h2);
                      create table lee (l1,l2,l3);
                      create table mee (m1,m2);"))

;*---------------------------------------------------------------------*/
;*    insert                                                           */
;*---------------------------------------------------------------------*/
(define-test insert.2
   (sqlite-exec *db* "insert into foo (f1,f2) values ('foo1','foo2');
                      insert into foo (f1,f2) values ('foo4','foo3');

                      insert into bar values ('bar1','bar2','bar3');
                      insert into bar values ('bar4','bar6','bar5');

                      insert into hux values ('hux10', 10);
                      insert into hux values ('hux11', 11);
                      insert into hux values ('hux12', 12);
                      insert into hux values ('hux13', 12);

                      insert into lee values (1,2,0);
                      insert into lee values (1,2,1);
                      insert into lee values (2,3,0);
                      insert into lee values (2,4,0);
                      insert into lee values (3,4,0);
                      insert into lee values (4,5,8);
                      insert into lee values (4,6,7);

                      insert into mee (m1) values (1);
                      insert into mee (m1,m2) values (2,2);"))

(define-test insert.3
   (sqlite-exec *db* "insert into hux (h1,h2) values ('hux14', 1);
                      insert into hux (h1,h2) values ('hux15', 2)"))

;*---------------------------------------------------------------------*/
;*    selects ...                                                      */
;*---------------------------------------------------------------------*/
(define-test select.3
   (sqlite-map *db* list "select * from foo")
   :result '(("foo2" "foo1") ("foo3" "foo4")))

(define-test select.4
   (sqlite-map *db* list "select * from foo where (f1=f2)"
   :result '()))

(define-test select.5
   (sqlite-map *db* list "select l1,l2 from lee where (l1=1) || (l1=2)")
   :result '(("1" "2") ("1" "2") ("2" "3") ("2" "4")))

(define-test select.6
   (sqlite-map *db* list "select l1,l2 from lee where (l1=3) or (l2>=5)")
   :result '(("3" "4") ("4" "5") ("4" "6")))

(define-test select.7
   (sqlite-map *db* list "select l1,l2 from lee where (l1=4) and (l2>=5)")
   :result '(("4" "5") ("4" "6")))

(define-test select.8
   (sqlite-map *db* list "select l1,l2 from lee where (l1=4) and (l2<2)")
   :result '())

(define-test select.9
   (sqlite-map *db* list "select * from foo where (f1<f2)"
   :result '(("foo2" "foo1"))))

(define-test select.10
   (sqlite-map *db* list "select * from foo where (foo.f1<foo.f2)"
   :result '(("foo2" "foo1"))))

(define-test select.11
   (sqlite-map *db* list "select f1 from foo"
   :result '(("foo1") ("foo4"))))

(define-test select.12
   (sqlite-map *db* list "select f2 from foo"
   :result '(("foo2") ("foo3"))))

(define-test select.13
   (sqlite-map *db* list "select f1,f2 from foo"
   :result '(("foo1" "foo2") ("foo4" "foo3"))))

(define-test select.14
   (sqlite-map *db* list "select * from foo,bar"
   :result '(("foo2" "foo1" "bar1" "bar2" "bar3")
	     ("foo2" "foo1" "bar4" "bar6" "bar5")
	     ("foo3" "foo4" "bar1" "bar2" "bar3")
	     ("foo3" "foo4" "bar4" "bar6" "bar5"))))

(define-test select.15
   (sqlite-map *db* list "select f1 from foo,bar ORDER BY f1"
   :result '(("foo1")
	     ("foo1")
	     ("foo4")
	     ("foo4"))))

(define-test select.16
   (sqlite-map *db* list "select b1 from foo,bar ORDER BY b1")
   :result '(("bar1")
	     ("bar1")
	     ("bar4")
	     ("bar4")))

(define-test select.17
   (sqlite-map *db* list "select h1 from hux where h2 in (10,11)")
   :result '(("hux10")
	     ("hux11")))

;*---------------------------------------------------------------------*/
;*    distinct                                                         */
;*---------------------------------------------------------------------*/
(define-test distinct.1
   (sqlite-map *db* list "select DISTINCT b1 from foo,bar")
   :result '(("bar1")
	     ("bar4")))

(define-test distinct.3
   (sqlite-map *db* list "select DISTINCT l1 from lee")
   :result '(("1") ("2") ("3") ("4")))

;*---------------------------------------------------------------------*/
;*    join                                                             */
;*---------------------------------------------------------------------*/
(define-test join.1
   (sqlite-map *db* list "select l1.*
                            from lee l1, lee l2
                           where (l1.l1=l2.l2) and (l1.l2=3)")
   :result '(("2" "3" "0") ("2" "3" "0")))

(define-test join.2
   (sqlite-map *db* list "select DISTINCT l1.*
                            from lee l1, lee l2
                           where (l1.l1=l2.l2) and (l1.l2=3)")
   :result '(("2" "3" "0")))

(define-test join.3
   (sqlite-map *db* list "select h1, l2 from hux, lee where (h2=l1)")
   :result '(("hux14" "2") ("hux14" "2") ("hux15" "3") ("hux15" "4")))

;*---------------------------------------------------------------------*/
;*    like                                                             */
;*---------------------------------------------------------------------*/
(define-test like.1
   (sqlite-map *db* list "select DISTINCT h2 from hux where h1 like 'hux%' ORDER BY h2")
   :result '(("1") ("2") ("10") ("11") ("12")))

(define-test like.2
   (sqlite-map *db* list "select DISTINCT h2 from hux where h1 like 'hux1_' ORDER BY h2")
   :result '(("1") ("2") ("10") ("11") ("12")))

;* (define-test regexp.1                                               */
;*    (sqlite-map *db* list "select DISTINCT h2 from hux where h1 regexp 'hux.[45]'") */
;*    :result '(("1") ("2")))                                          */
;*                                                                     */
;* (define-test regexp.2                                               */
;*    (sqlite-map *db* list "select DISTINCT h2 from hux where h1 not regexp 'hux.[45]'") */
;*    :result '(("10") ("11") ("12")))                                 */
;*                                                                     */
;* (define-test regexp.3                                               */
;*    (sqlite-map *db* list "select DISTINCT h2 from hux where h1 regexp 'hux.[0-3]'") */
;*    :result '(("10") ("11") ("12")))                                 */

;*---------------------------------------------------------------------*/
;*    subqueries                                                       */
;*---------------------------------------------------------------------*/
(define-test subquery.0
   (sqlite-map *db* list "create table ta (ta1,ta2);
                          create table tb (tb1,tb2);
                          insert into ta values(1,2);
                          insert into ta values(3,5);
                          insert into tb values(4,2)"))

(define-test subquery.1
   (sqlite-map *db* list "select *
                           from ta
                           where ta2=(select tb2
                                      from tb
                                      where tb1>=ta2);")
   :result '(("1" "2")))

(define-test subquery.2
   (sqlite-map *db* list "select h1
                           from hux
                           where h2 = (select l2
                                        from lee l2
                                        where l2.l2=h2)")
   :result '(("hux15")))

(define-test subquery.3
   (sqlite-map *db* list "select *
                           from ta
                           where ta2 in (select tb2
                                         from tb
                                         where tb1>3);")
   :result '(("1" "2")))

(define-test subquery.4
   (sqlite-map *db* list "select *
                           from ta
                           where ta2 not in (select tb2
                                             from tb
                                             where ta2<tb1);")
   :result '(("3" "5")))

(define-test subquery.5
   (sqlite-map *db* list "select *
                           from ta ta1
                           where 4 > (select ta2
                                       from ta
                                       where ta1.ta2 = ta2)")
   :result '(("1" "2")))

(define-test subquery.6
   (let ((db (instantiate::sqlite)))
      (sqlite-exec db "create table assoc1 (k1,v1);
                       create table assoc2 (k2,v2);")
      (let loop ((i 10))
	 (when (>fx i 0)
	    (sqlite-exec db "insert into assoc1 values (~a, ~a);
                             insert into assoc2 values (~a, ~a)"
			 i (+fx i 1) i (+fx i 1))
	    (loop (-fx i 1))))
      (sqlite-eval db
	 (lambda (x) x)
	 "select MAX(k1)
           from assoc1
           where k1 < (select MIN(v2) from assoc2 where k2 > k1)"))
   :result "9")

;*---------------------------------------------------------------------*/
;*    null                                                             */
;*---------------------------------------------------------------------*/
(define-test isnull  
   (sqlite-map *db* list "select m1 from mee where m2 ISNULL")
   :result '(("1")))
	       
(define-test notnull
   (sqlite-map *db* list "select m1 from mee where m2 NOTNULL")
   :result '(("2")))
	       
;*---------------------------------------------------------------------*/
;*    order-by                                                         */
;*---------------------------------------------------------------------*/
(define-test order-by.1
   (sqlite-map *db* list "select b2 from foo,bar ORDER BY b2 ASC")
   :result '(("bar2")
	     ("bar2")
	     ("bar6")
	     ("bar6")))

(define-test order-by.2
   (sqlite-map *db* list "select b2 from foo,bar ORDER BY b2 DESC")
   :result '(("bar6")
	     ("bar6")
	     ("bar2")
	     ("bar2")))

(define-test order-by.3
   (sqlite-map *db* list "select b2 from foo,bar ORDER BY b2 DESC LIMIT 2")
   :result '(("bar6")
	     ("bar6")))

(define-test order-by.4
   (sqlite-map *db* list "select b2 from foo,bar ORDER BY b2 DESC LIMIT 2,2")
   :result '(("bar2")
	     ("bar2")))

(define-test order-by.5
   (sqlite-map *db* list "select l3 from lee WHERE l1=1 ORDER BY l1,l2,l3 DESC")
   :result '(("1") ("0")))

(define-test order-by.6
   (sqlite-map *db* list "select l3 from lee WHERE l1=1 ORDER BY l1,l2,l3 ASC")
   :result '(("0") ("1")))

(define-test order-by.7
   (sqlite-map *db* list "select l1, l3 from lee WHERE l1<3 ORDER BY l1 ASC,l2,l3 DESC")
   :result '(("1" "1") ("1" "0") ("2" "0") ("2" "0")))

(define-test order-by.8
   (sqlite-map *db* list "select l1,l2,l3 from lee WHERE l1<3 ORDER BY l1 DESC,l2,l3 ASC")
   :result '(("2" "3" "0") ("2" "4" "0") ("1" "2" "0") ("1" "2" "1")))

(define-test order-by.9
   (sqlite-map *db* list "select l1,l2,l3 from lee WHERE l1=4 ORDER BY l1 ASC,l2 DESC,l3 ASC")
   :result '(("4" "6" "7") ("4" "5" "8")))

(define-test order-by.10
   (sqlite-map *db* list "select ALL b1 from foo,bar order BY b1")
   :result '(("bar1")
	     ("bar1")
	     ("bar4")
	     ("bar4")))

;*---------------------------------------------------------------------*/
;*    group-by                                                         */
;*---------------------------------------------------------------------*/
(define-test group-by.1
   (sqlite-map *db* list "select MIN(h1) from hux GROUP BY h2 ORDER BY h1")
   :result '(("hux10") ("hux11") ("hux12") ("hux14") ("hux15")))
   
(define-test group-by.2
   (sqlite-map *db* list "select MIN(l2) from lee group by l1")
   :result '(("2") ("3") ("4") ("5")))

(define-test group-by.3
   (sqlite-map *db* list "select MAX(l2) from lee group by l1")
   :result '(("2") ("4") ("4") ("6")))
   
(define-test group-by.4
   (sqlite-map *db* list "select MAX(l3) from lee group by l1,l2")
   :result '(("1") ("0") ("0") ("0") ("8") ("7")))
   
(define-test group-by.5
   (sqlite-map *db* list "select MIN(l3) from lee group by l1,l2")
   :result '(("0") ("0") ("0") ("0") ("8") ("7")))
   
(define-test group-by.6
   (sqlite-map *db* list "select l1 from lee group by l1")
   :result '(("1") ("2") ("3") ("4")))
   
;*---------------------------------------------------------------------*/
;*    replace ...                                                      */
;*---------------------------------------------------------------------*/
(define-test replace.1
   (sqlite-map *db*
      (lambda (x) x)
      "INSERT INTO foo VALUES ('foo4','foo5');
       REPLACE INTO foo (f1,f2) values ('foo1', 'foo1-2');
       SELECT f2 from foo WHERE (f1='foo1')")
   :result '("foo1-2"))

(define-test replace.2
   (sqlite-map *db*
      (lambda (x) x)
      "REPLACE INTO foo (f1,f2) values ('foo4', 'foo4-3');
       SELECT f2 from foo WHERE (f1='foo4')")
   :result '("foo4-3"))

(define-test replace.3
   (sqlite-map *db*
      (lambda (x) x)
      "SELECT f2 from foo WHERE (f1='foo1')")
   :result '("foo1-2"))
      
(define-test replace.4
   (sqlite-map *db*
      (lambda (x) x)
      "REPLACE INTO foo (f1,f2) values ('foo5', 'foo5-4');
       VACUUM;
       SELECT f2 from foo WHERE (f1='foo5')")
   :result '("foo5-4"))

;*---------------------------------------------------------------------*/
;*    update                                                           */
;*---------------------------------------------------------------------*/
(define-test update.1
   (sqlite-map *db*
      (lambda (x) x)
      "UPDATE foo SET f2 ='new-foo5-4' WHERE (f1='foo5');
       SELECT f2 from foo WHERE (f1='foo5')")
   :result '("new-foo5-4"))

;*---------------------------------------------------------------------*/
;*    number-of-rows                                                   */
;*---------------------------------------------------------------------*/
(define-test sqlite-table-number-of-rows.1
   (sqlite-table-number-of-rows *db* "bar")
   :result 2)

(define-test sqlite-table-number-of-rows.2
   (sqlite-eval *db* (lambda (x) x) "select MAX(rowid) from bar")
   :result "2")

;*---------------------------------------------------------------------*/
;*    sqlite-table-name-of-columns                                     */
;*---------------------------------------------------------------------*/
(define-test sqlite-table-name-of-columns
   (sqlite-table-name-of-columns *db* "bar")
   :result '("b1" "b2" "b3"))

;*---------------------------------------------------------------------*/
;*    sqlite-name-of-tables ...                                        */
;*---------------------------------------------------------------------*/
(define-test sqlite-name-of-tables
   (sqlite-name-of-tables *db*)
   :result '("gee" "foo" "bar" "hux" "lee" "mee" "ta" "tb"))

(define-test drop-table
   (begin
      (sqlite-exec *db* "DROP TABLE mee")
      ;(sqlite-exec *db* "DROP TABLE IF EXISTS mee")
      (sqlite-name-of-tables *db*))
   :result '("gee" "foo" "bar" "hux" "lee" "ta" "tb"))

;*---------------------------------------------------------------------*/
;*    transaction                                                      */
;*---------------------------------------------------------------------*/
(define-test begin
   (sqlite-exec *db*
      "BEGIN TRANSACTION; insert into foo (f1, f2) values ('foo6',10);"))

(define-test end
   (sqlite-exec *db*
      "END TRANSACTION; select f2 from foo where f1='foo6'")
   :result "10")

;*---------------------------------------------------------------------*/
;*    delete ...                                                       */
;*---------------------------------------------------------------------*/
(define-test delete.1
   (sqlite-exec *db* "DELETE FROM foo where (f1='foo1');
                      SELECT * FROM  foo where (f1='foo1')")
   :result #f)

(define-test delete.1b
   (sqlite-exec *db* "SELECT f2 FROM  foo where (f1='foo4')")
   :result "foo4-3")

(define-test delete.2
   (sqlite-map *db* (lambda (x) x)
	       "DELETE FROM hux where (h2 >= 12);
                SELECT h1 FROM  hux where (h2 >= 10)")
   :result '("hux10" "hux11"))

;*---------------------------------------------------------------------*/
;*    transactions                                                     */
;*---------------------------------------------------------------------*/
(define-test roolback.1
   (cond-expand
      ((not sqltiny)
       (let ((db (instantiate::sqlite)))
	  (sqlite-exec db "CREATE TABLE test (field TEXT, value TEXT)")
	  (sqlite-exec db "INSERT INTO test VALUES (~q,~q)" "a-field" "a-value")
	  (sqlite-exec db "BEGIN")
	  (with-handler
	     (lambda (exc) #f)
	     (sqlite-eval db
		   (lambda (field value)
		      (raise 'error))
		"SELECT * FROM test"))
	  (sqlite-exec db "ROLLBACK")
	  'ok)
       :result 'ok)))
    
;*---------------------------------------------------------------------*/
;*    close ...                                                        */
;*---------------------------------------------------------------------*/
(define-test close
   (sqlite-close *db*))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (unwind-protect
	 (for-each (lambda (pvn)
		      (apply test pvn))
		   (if (null? tests)
		       (reverse *tests*)
		       (reverse (filter (lambda (t) (memq (car t) tests))
					*tests*))))
	 (when (file-exists? "test.db") (delete-file "test.db")))
      ;; if we reach that point, we are done
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))
