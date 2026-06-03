<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    I/O                                                           -->
<!--==================================================================-->

,(include "head.html")

,(implementation-path "../runtime/Ieee/port.scm")
,(implementation-path "../runtime/Ieee/output.scm")
,(implementation-path "../runtime/Ieee/input.scm")
,(implementation-path "../runtime/Read/reader.scm")
,(implementation-path "../runtime/Pp/circle.scm")
,(implementation-path "../runtime/Pp/pp.scm")
,(example-path "../test/src/io.bgl")

Input/Output
============


Predicates
----------

### eof-object? ###
Returns `#t` is `obj` is the object denoting end-of-file. Returns `#f`
otherwise.

### char-ready? ###
As specified in the R5Rs, `char-ready?
returns `#t` if a character is ready on the input `ip` and
returns `#f` otherwise.  If `char-ready` returns `#t` then
the next `read-char` operation on the given `ip` is guaranteed
not to hang.  If the port `ip` is at end of file then `char-ready?`
returns `#t`. The argument `ip` may be omitted, in which case it defaults to
the value returned by `current-input-port`.

When using `char-ready?` consider the latency that may exists
before characters are available. For instance, executing the
following source code:

For a discussion of Bigloo processes, [Process](process).


Library Functions
-----------------

### read-char ###
Reads a character from `ip`. Returns either a character or the end-of-file
object.

### read-byte ###
Reads a byte from `ip`. Returns either a fixnum or the end-of-file object.

### peek-char ###
Reads a character from `ip` without consuming it. Returns either a
character or the end-of-file object.

### peek-byte ###
Reads a byte from `ip` without consuming it. Returns either a
byte or the end-of-file object.

### read-string ###
Reads all the characters of `input-port` into a string.

### read-chars ###
The function `read-chars` returns a newly allocated strings made
of `size` characters read from `input-port` (or from
`(current-input-port)` if `input-port` is not provided). If
less than `size` characters are available on the input port, the
returned string is smaller than `size`. Its size is the number of
available characters.

### read-chars! ###
The `read-chars!` fills the buffer `buf` with at most
`size` characters, read from `input-port`.

### read-fill-string! ###
Fills the string `s` starting at offset `o` with at
most `len` characters read from the input port `input-port`
(or from `(current-input-port)` if `input-port` is not provided).
This function returns the number of read characters (which may be smaller
than `len` if less characters are available) or the end of file object.
The argument `len` is a small integer.

The function `read-fill-string!` is similar to `read-chars!`
except that it returns the `end-of-file` object on termination while
`read-chars!` returns 0.

### unread-char! ###
Pushes the given `char`, into the input-port.
The next read character will be the pushed one. The `input-port` must
be buffered and not be closed.

### unread-string! ###
Pushes the given string into the input-port.
The next read character(s) will be the pushed ones. The `input-port` must
be buffered and not be closed.

### unread-substring! ###
Pushes the given substring into the input-port.
The next read character(s) will be the pushed ones. The `input-port` must
be buffered and not be closed.

### read ###
Reads a lisp expression from `ip`. If the argument `location` is `#t`, read
list are composed of epairs instead of plain pairs, whose `cer` denotes
the position in `ip` where the list was read from.

### read-line ###
Reads characters from `input-port` until a `#\Newline`, a `#\Return`
or an `end of file` condition is encountered.  The function
`read-line` returns a newly allocated string composed of the
characters read.

The strings returned by `read-line` do not contain the newline delimiters.

### read-line-newline ###
Reads characters from `input-port` until a `#\Newline`, a `#\Return`
or an `end of file` condition is encountered.  The function
`read-line` returns a newly allocated string composed of the
characters read.

The strings returned by `read-line-newline` do contain the newline delimiters.

### read-lines ###
Accumulates all the line of an `input-port` into a list.

### read-of-strings ###
Reads a sequence of non-space characters on `input-port`, makes a
string of them and returns the string.

### password ###
Reads a password from the current input port. The reading stops when the user
hits the `Enter` key.

### port->string-list ###
Returns a list of strings composed of the elements of `input-port`.

### port->list ###
The function `port->list` applies reader to port repeatedly until it 
returns EOF, then returns a list of results. 

### port->sexp-list ###
The function `port->sexp-list` is equivalent to `(port->list read port)`.

### file->string ###
This function builds a new string out of all the characters of the file 
`path`. If the file cannot be open or read, an `&io-exception`
is raised.

### write ###
Writes a readable representation of `obj` to the output-port `op`. 

### write-circle ###
Displays recursive object `obj` on `output-port`. Each component
of the object is displayed using the `write`.

### display ###
Displays `obj` to the output-port `op`. 

### display-circle ###
Display recursive object `obj` on `output-port`. Each component
of the object is displayed using `display`.

### display* ###
Displays all its arguments to the current output port.

### display-string ###
Displays a string to `output-port`.

### display-substring ###
Displays a substring formed from the characters
of string beginning with index `start` (inclusive) and ending with index
`end` (exclusive). The arguments `start` and `end` must be exact integers 
satisfying
0 &le; `start` &le; `end` &le; `(string-length string)`.

### display-object ###
This generic function is invoked by `display` to display objects.

### write-object ###
This generic function is invoked by `write` to display objects.

### print ###
As `display*` but also print a newline.

### fprint ###
As `print` but writes values to `op`.

### write-char ###
Displays a character to `output-port`.

### write-byte ###
Displays a byte to `output-port`.

### newline ###
Displays a newline to `output-port`.

### send-chars ###
Transfer the characters from `input-port` to `output-port`. This
procedure is sometimes mapped to a system call (such as `sendfile` under
Linux) and might thus be more efficient than copying the ports by hand. The
optional argument `offset` specifies an offset from which characters of
`input-port` are sent. The function `send-chars` returns the number
of characters sent.

### send-file ###
The function `send-file` opens the file `filename` in order to
get its input port. On some backends, `send-file` might be more efficient
than `send-chars` because it may avoid creating a full-fledged Bigloo
`input-port`.

> [!NOTE]
> Note that the type of `len` and `offset` is
> `elong` (i.e., exact long), which is also returned by `file-size`.

### format ###
Accepts a message template (a Scheme String), and processes it,
replacing any escape sequences in order with one or more characters,
the characters themselves dependent on the semantics of the escape
sequence encountered.

An escape sequence is a two character sequence in the string where the
first character is a tilde `~`. Each escape code's meaning is as
follows:

  * `~a` The corresponding value is inserted into the string 
  as if printed with display.
  * `~s` The corresponding value is inserted into the string 
  as if printed with write.
  * `~%` or `~n` A newline is inserted A newline is inserted.
  * `~~` A tilde `~` is inserted.
  * `~r` A return (`#\Return`) is inserted.
  * `~v` The corresponding value is inserted into the string 
  as if printed with display followed by a newline. This tag is hence
  equivalent to the sequence `~a~n`.
  * `~c` The corresponding value must be a character and is
  inserted into the string as if printed with write-char.
  * `~d`, `~x`, `~o`, `~b`  The corresponding value must
  must be a number and is printed with radix 16, 8 or 2.
  * `~l` If the corresponding value is a proper list, its items 
  are inserted into the string, separated by whitespaces, without the 
  surrounding parenthesis. If the corresponding value is not a list, it 
  behaves as `~s`.
  * `~(SEP)` If the corresponding value is a proper list, its items 
  are inserted into the string, separated from each other by `SEP`, 
  without the surrounding parenthesis. If the corresponding value is not a 
  list, it behaves as `~s`.
  * `~Ndxob` Print a number in `N` columns with space padding.
  * `~N,PADDINGdxob` Print a number in `num` columns 
  with `PADDING` padding.

When encountered, `~a` and `~s`, require a corresponding Scheme value
to be present after the format string. The values provided as operands
are used by the escape sequences in order. It is an error if fewer
values are provided than escape sequences that require them.

The options `~%` and `~~` require no corresponding value.

### printf ###
Formats the sequence of `obj` according to `fmt` and outputs the result
to the current output port.

### fprintf ###
Formats the sequence of `obj` according to `fmt` and outputs the result
to `port`.

### pp ###
Pretty prints `obj` on `output-port`.

### define-reader-ctor ###

The present [SRFI-10](http://srfi.schemers.org/srfi-10/srfi-10.html) proposes an
extensible external representation of Scheme values, a notational
convention for future SRFIs. This SRFI adds `#\x2c(` as a new token and
extends production rules of the grammar for a Scheme reader. The `#\x2c()`
form can be used for example to denote values that do not have a
convenient printed representation, as well for conditional code
compilation. It is proposed that future SRFIs that contain new read
syntax for values use the `#\x2c()` notation with an appropriate tag
symbol.

As a particular example and the reference implementation for the `#\x2c()`
convention, this SRFI describes an interpretation of the `#\x2c()` external
form as a read-time application.


