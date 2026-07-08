<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/sqlite.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    SQLITE                                                        -->
<!--==================================================================-->

,(implementation-path "../api/sqlite/src/Llib/sqlite.scm")
,(example-path "../test/src/sqlite0.bgl")

SQLITE
======

The C and wasm back-ends support SQL queries. It relies on the SQLite
library (@url{http://www.sqlite.org/}). The SQLite binding is
accessible to Bigloo via the @code{sqlite} library. Here is an example
of module that uses this library.

The jvm backend only supports the limited `sqlitiny` implementation
that is compatible with sqlite but that is slower.

> [!IMPORTANT] A module that uses Sqlite features must include in its
> declaration the clause `(library sqlite)`. Example:

```bigloo
(module sqlite-ex
  (library sqlite)
```

Classes
-------
### sqltiny ###

The instances of the class `sqlite` hold SQLite databases. A database
may be permanently stored on a disk or loaded in memory. The class
attribute `path` is the location on the disk where the database is
stored. The special path `:memory:` denotes in-memory databases. If no
`path` is specified, the database is opened in memory. In memory
databases are not preserved from one execution to the other. When an
instance is created a SQLite database is _opened_.

> [!WARNING] Sqltiny databases rely exclusively on a Bigloo
> implementation. They expose acceptable performance for small
> databases but as much as possible, for larger databases and when
> portability is not a major issue, the native implementation
> supported by Sqlite should be prefered.

### sqlite ###

Similar to `sqltiny` but uses the SQLite library to implement the database.

### sqlite-close ###
This function closes a database previously opened by creating an instance
of the class `sqltiny` or one of its subclasses, e.g., `sqlite`.

### sqlite-format ###
Constructs a string of characters representing an SQLite
commands. This function acts as `format` (see chapter [i/o](io.html)).
It is augmented with two additional escape sequence:
`~q`, `~k`. The first one build a string of
characters where the characters denoting SQL strings (i.e., the
character `'`) is automatically escaped. The escape character
`~k` introduces a list of SQL strings.

Summary of all escape codes:

  * `~a` The corresponding value is inserted into the string 
  as if printed with display.
  * `~s` The corresponding value is inserted into the string 
  as if printed with write.
  * `~%` A newline is inserted.
  * `~~` A tilde `~` is inserted.
  * `~q` An SQL escaped string.
  * `~l` Introduces a list (comma separated).
  * `~k` Introduces a list of SQL strings.


### sqlite-exec ###
The function `sqlite-exec` _executes_ an SQLite command. The command
is the built by implicitly invoking `sqlite-format` on `string` and
the optional `arg` arguments. This function returns a single element,
the first one returned by the SQL engine.


### sqlite-eval ###
The function `sqlite-eval` invokes a SQLite command built by
implicitly invoking `sqlite-format` on `string` and the optional
`arg` arguments. The result of the function is built by applying 
`procedure` to the first value returned by the SQLite call.

> [!NOTE] user callback (`procedure`) _must not_ exit. That is they must
> not invoke a function create by `bind-exit`. Exiting from a callback will
> leave the database in a inconsistent state that prevent transactions to
> be rolled back.

### sqlite-get ###
Similar to `sqlite-eval` but the callback is invoked with two
arguments: an array of column names and an array of values.

@deffn {bigloo sqlite function} sqlite-for-each @var{sqlite} @var{procedure} @var{string} @var{arg} @dots{}
The function @code{sqlite-for-each} invokes a SQLite command built by
implicitly invoking @code{sqlite-format} on @var{string} and the optional
@var{arg} arguments. The function @var{procedure} is applied to all
the elements statisfying the request. It accepts two vectors. The
first one is the name of the table column, the second the values.
The function @var{sqlite-for-each} does not return any value.

Note: user callback (@var{procedure}) @b{must not} exit. That is they must
not invoke a function create by @code{bind-exit}. Exiting from a callback will
leave the database in a inconsistent state that prevent transactions to
be rolled back.

Example:

@smalllisp
(module example
   (library sqlite))

(define *db* (instantiate::sqlite))

(sqlite-exec *db* "CREATE TABLE foo (x INTEGER, y INTEGER)")
(for-each (lambda (x)
		(sqlite-exec *db*  "INSERT INTO foo VALUES(~A, ~A)" x (* x x)))
	     (iota 10))
(sqlite-map *db* 
  (lambda (keys vals) (print keys vals))
  "SELECT * FROM foo")
   @print{} #("x" "y") #(0 0)
            #("x" "y") #(1 1)
            ...
@end smalllisp
@end deffn

@deffn {bigloo sqlite function} sqlite-map @var{sqlite} @var{procedure} @var{string} @var{arg} @dots{}
The function @code{sqlite-map} invokes a SQLite command built by
implicitly invoking @code{sqlite-format} on @var{string} and the optional
@var{arg} arguments. The result is a list whose elements are built by applying 
@var{procedure} to all the values returned by the SQLite call.

Note: user callback (@var{procedure}) @b{must not} exit. That is they must
not invoke a function create by @code{bind-exit}. Exiting from a callback will
leave the database in a inconsistent state that prevent transactions to
be rolled back.
Example:

@smalllisp
(module example
   (library sqlite))

(define *db* (instantiate::sqlite))

(sqlite-exec *db* "CREATE TABLE foo (x INTEGER, y INTEGER)")
(for-each (lambda (x)
		(sqlite-exec *db*  "INSERT INTO foo VALUES(~A, ~A)" x (* x x)))
	     (iota 10))
(sqlite-map *db* 
  (lambda (s1 s2) (+ (string->integer s1) (string->integer s2))) 
  "SELECT * FROM foo")
   @result{} (0 2 6 12 20 30 42 56 72 90)
@end smalllisp
@end deffn

Example2:
@smalllisp
(module example
   (library sqlite))

(define *db* (instantiate::sqlite))

(sqlite-exec *db* "CREATE TABLE foo (x INTEGER, y INTEGER)")
(for-each (lambda (x)
		(sqlite-exec *db*  "INSERT INTO foo VALUES(~A, ~A)" x (* x x)))
	     (iota 10))
(sqlite-map *db* vector "SELECT * FROM foo")
   @result{} '(#("0" "0")
	#("1" "1")
	#("2" "4")
	#("3" "9")
	#("4" "16")
	#("5" "25")
	#("6" "36")
	#("7" "49")
	#("8" "64")
	#("9" "81"))
@end smalllisp

@deffn {bigloo sqlite function} sqlite-name-of-tables @var{sqlite}
Returns the name of tables in the database. This list can also be
obtained with

@smalllisp
(sqlite-map db
   (lambda (x) x)
   "SELECT name FROM sqlite_master WHERE type='table'")
@end smalllisp
@end deffn

@deffn {bigloo sqlite function} sqlite-table-name-of-columns @var{sqlite} @var{table}
Returns the name of columns in the table.
@end deffn

@deffn {bigloo sqlite function} sqlite-last-insert-rowid @var{sqlite}
Returns the SQLite @emph{rowid} of the last inserted row.
@end deffn







