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

The threw backend supports SQL queries but only the C backend relies
on the native SQLite library (@url{http://www.sqlite.org/}). The jvm
and wasm backends only supports the limited `sqlitiny` implementation
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

> [!NOTE] user callbacks (`procedure`) _must not_ exit. That is they must
> not invoke a function create by `bind-exit`. Exiting from a callback will
> leave the database in a inconsistent state that prevent transactions to
> be rolled back.

### sqlite-get ###
Similar to `sqlite-eval` but the callback is invoked with two
arguments: an array of column names and an array of values.

> [!NOTE] user callbacks (`procedure`) _must not_ exit. That is they must
> not invoke a function create by `bind-exit`. Exiting from a callback will
> leave the database in a inconsistent state that prevent transactions to
> be rolled back.

### sqlite-for-each ###
The function `sqlite-for-each` invokes a SQLite command built by
implicitly invoking `sqlite-format` on `string` and the optional
`arg` arguments. The function `procedure` is applied to all
the elements statisfying the request. It accepts two vectors. The
first one is the name of the table column, the second the values.
The function `sqlite-for-each` does not return any value.

> [!NOTE] user callbacks (`procedure`) _must not_ exit. That is they must
> not invoke a function create by `bind-exit`. Exiting from a callback will
> leave the database in a inconsistent state that prevent transactions to
> be rolled back.

### sqlite-map ###
The function `sqlite-map` invokes a SQLite command built by
implicitly invoking `sqlite-format` on `string` and the optional
`arg` arguments. The result is a list whose elements are built by applying 
`procedure` to all the values returned by the SQLite call.

> [!NOTE] user callbacks (`procedure`) _must not_ exit. That is they must
> not invoke a function create by `bind-exit`. Exiting from a callback will
> leave the database in a inconsistent state that prevent transactions to
> be rolled back.

### sqlite-name-of-tables ###
Returns the name of tables in the database. This list can also be
obtained with:

```bigloo
(sqlite-map db
   (lambda (x) x)
   "SELECT name FROM sqlite_master WHERE type='table'")
```

<span></span>

### sqlite-table-name-of-columns ###
Returns the name of columns in the table.

### sqlite-last-insert-rowid ###
Returns the SQLite _rowid_ of the last inserted row. This can also be
obtained with:

```bigloo
(sqlite-exec db
   "SELECT last_insert_rowid()")
```

<span></span>







