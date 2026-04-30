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
,(example-path "../test/src/port.bgl")

Input/Output
============

#### Buffers ####
<!-- [:buffers@section] --> 

Many ports functions accept an optional argument named `bufinfo`. It is
used to specify the buffer associated with the input/output ports. The
value can either be:

  * `#t`: the port will use a default buffer (generally of size 1024);
  * `#f`: no buffer is used;
  * a string: it is used as read buffer;
  * a fixnum: the size of the buffer to be used;

Constructors
------------

### open-input-file ###
If `file-name` is a regular file name, `open-input-file` opens a
regular input port on that file, if it exists. It returns `#f`
otherwise. If `file-name` starts with special prefixes it behaves
differently. Here are the recognized prefixes:

  * `| ` (a string made of the characters `#\|` and `#\space`)
   Instead of opening a regular file, Bigloo opens an input pipe. 
   The same syntax is used for output file. 

  * `pipe:`, same as `| `.

  * `file`: opens a regular file.

  * `fd`: opens a file descriptor.

  * `gzip:` opens a port on a gzipped filed. This is equivalent to 
`open-input-gzip-file`. 
  * `string:` opens a port on a string. This is equivalent to 
    `open-input-string`. 
    
  * `http:` opens an `http` connection. More precisely:
  
    * `http://server/path` opens an `http` connection on `server` and open an 
      input file on file `path`.
    * `http://server:port-number/path`.
    * `http://user:password@@server:port-number/path`.

  * `ftp:` Opens an `ftp` connection on `server` and open an input file
     on file `path`. Log in as anonymous.

    * `ftp://server/path`
    * `ftp://user:password@@server/path`

  * `ressource:` opens a JVM `ressource` file. Opening a `ressource:` file in 
    non JVM backend always return `#f`. On the JVM backend it returns
    a input port if the ressource exists. Otherwise, it returns `#f`.


Example of a pipe port:

```bigloo
(define pin (open-input-file "| cat /etc/passwd"))
(define pout (open-output-file "| wc -l"))

(display (read pin) pout)
(close-input-port pin)
(newline pout)
(close-output-port pout)
```


Example of a file descriptor port:
```bigloo
(with-input-from-file "fd:0"
   (lambda ()
      (read)))
```

Example of a string port:
```bigloo
(with-input-from-file "string:foo bar Gee"
   (lambda ()
      (print (read))
      (print (read))
      (print (read))))
   &rarr; foo
   &rarr; bar
   &rarr; Gee
```


Example of a gzip port:

```bigloo
(with-input-from-file "gzip:bigloo.tar.gz"
   (lambda ()
      (send-chars (current-input-port) (current-output-port))))
```


The optional argument `buffer` can either be:

  * A positive fixnum, this gives the size of the buffer.
  * The boolean `#t`, a buffer is allocated.
  * The boolean `#f`, the socket is unbufferized.
  * A string, it is used as buffer.

The optional argument `timeout`, an integer represents a microseconds 
timeout for the open operation.

### input-port-reopen! ###
Re-open the input port `obj`. That is, re-start reading from the first
character of the input port.


### open-input-descriptor ###
Open a file descriptor (as C `fdopen`).

For the the optional argument `bufinfo` see [buffers](#buffers).

> [!WARNING]
> The jvm backend has a very limited support for `open-input-descritpor`. It
> can only opens the descriptor 0. For all other values, it will return `#f`.

### open-input-string ###
Returns an `input-port` able to deliver characters from `string`.

The arguments `start` and `end` must be exact integers satisfying 0
&le; `start` &le; `end` &le; `(string-length string)`.

### open-input-string! ###
The function `open-input-string!` acts as `open-input-string`
but it might modify the string it receives as parameter.

### open-input-procedure ###
Returns an `input-port` able to deliver characters from
`procedure`. Each time a character has to be read, the `procedure`
is called. This procedure may returns a string of characters, or
the boolean `#f`. This last value stands for the end of file. 

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-input-mmap ###

The arguments `start` and `end` must be exact integers satisfying:
0 &le; `start` &le; `end` &le; `(mmap-length string)`.

Returns an `input-port` able to deliver characters from `mmap`.

See the [mmap](mmap.html) documentation.

### open-input-gzip-file ###

Open a gzipped file for input and a port on a gzipped stream.

### open-input-gzip-port ###

Open a gzipped port for input and a port on a gzipped stream.
Note that closing a gzip port opened from a port `pi` does not close
the `pi` port.

### open-output-file ###
The same syntax as `open-input-file` for file names applies here.
When a file name starts with `| `, Bigloo opens an output pipe
instead of a regular file.

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-output-string ###
Returns an _output string port_. This object has almost the same
purpose as `output-port`. It can be used with all the printer
functions which accept `output-port`. An output on a _output
string port_ memorizes all the characters written. An invocation of
`flush-output-port` or `close-output-port` on an
_output string port_ returns a new string which contains all the
characters accumulated in the port.

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-output-procedure ###
This function returns an _output procedure port_. This object has almost
the same purpose as `output-port`. It can be used with all
the printer functions which accept `output-port`. An output
on a _output procedure port_ invokes the `proc` procedure
each time it is used for writing. That is, `proc` is invoked with a
string denoting the displayed characters. When the function
`flush-output-port` is called on such a port, the optional
`flush` procedure is invoked. When the function `close-output-port`
is called on such a port, the optional `close` procedure is invoked.

For the the optional argument `bufinfo` see [buffers](#buffers).

Default ports
-------------

### current-input-port ###
Returns the current input-port which default to the standard input
port of the application.

### current-output-port ###
Returns the current output-port which default to the standard output
port of the application.

### current-error-port ###
Returns the current error output-port which default to the standard error
port of the application.

Predicates
----------

### port? ###
Returns `#t` if `obj` is any kind of ports. Returns `#f` otherwise.

### input-port? ###
Returns `#t` iff `obj` is an `input-port`. Returns `#f` otherwise.

### input-string-port? ###
Returns `#t iff `obj` is an `input-port` opened on a string. Returns
`#f` otherwise.

### output-port? ###
Returns `#t` iff `obj` is an `output-port`. Returns `#f` otherwise.

### output-string-port? ###
Returns `#t` iff `obj` is an `output-port` opened on a string. Returns
`#f` otherwise.

### closed-input-port? ###
Predicates that returns `#t` if and if their associated port is closed.
It return `#f` otherwise.


Properties
----------

### input-port-name ###
Returns the name of the file used to open the `input-port`.

### input-port-name-set! ###
Sets a different name for the input-port.

### input-port-position ###
The character position in the input-port.

### input-port-length ###
Returns the number of bytes contained in the input-port of -` if
that number is unknown (typically for a pipe).

### input-port-timeout ###
Returns the timeout of this `port`. Returns 0 is the `port` has
no timeout. The timeout is expressed in microseconds 
(1 second = 1,000,000 microseconds).

Reading on an input port which exceeds the timeout triggers an exception.

> [!WARNING]
> Not all backends offers the same level of support for timeouts. In 
> particular the wasm backend does not support them at all. A negative 
> value returned by `input-port-timeout` means that timeouts for this
> input port are not supported.


### input-port-timeout-set! ###
Sets a timeout of this `port`. The timeout is expressed in microseconds 
(1 second = 1,000,000 microseconds).

### output-port-name ###
Returns the name of the file used to open the `output-port`.

### output-port-name-set! ###
Sets a different name for the output-port.

### output-port-position ###
Returns the current position (a character number), in the `port`.

### output-port-timeout ###
Returns the timeout of this `port`. Returns 0 is the `port` has
no timeout. The timeout is expressed in microseconds 
(1 second = 1,000,000 microseconds).

Reading on an output port which exceeds the timeout triggers an exception.

> [!WARNING]
> Not all backends offers the same level of support for timeouts. In 
> particular the wasm backend does not support them at all. A negative 
> value returned by `output-port-timeout` means that timeouts for this
> output port are not supported.

### output-port-timeout-set! ###
If the `timeout` limit (expressed in microseconds) exceededs, an exception
of time `&io-timeout-error` is raised.

Setting a timeout equal to 0, restore the socket in blocking mode. Setting
a timeout with a value lesser than 0 is ignored.

> [!NOTE]
> Ports created from sockets share their internal file descriptor. Hence
> it is erroneous to set a timeout for only one of the two ports. Both
> must be set.

Library Functions
-----------------

### close-input-port ###
Closes an `input-port`.

### close-output-port ###
Closes an `output-port`. If it was created using
`open-output-string`, the value returned is the string consisting
of all characters sent to the port.

### call-with-input-file ###
Invokes `proc` with an input port opened on `file`. Returns the result
of the call and closes the port. Triggers an error is `file` cannot
be opened.

### call-with-input-string ###
Invokes `proc` on an input port opened on `string`.

### call-with-output-file ###
Invokes `proc` on an output port opened on `string`.

### call-with-append-file ###
Invokes `proc` on an output port opened on `string` for appending new
characters.

### call-with-output-string ###
As `call-with-output-file` but opens a string port instead of a file
port. Returns the string formed by all the written characters.

### with-input-from-file ###
A port is opened from file `string`. This `port` is made the
current input port and `thunk` is called. 

### with-input-from-string ###
A port is opened from the string `string`. This `port` is made the
current input port and `thunk` is called. 

### with-input-from-procedure ###
A port is opened from the `procedure`. This `port` is made the
current input port and `thunk` is called. 

### with-input-from-port ###
Invokes `thunk` with `port` being made the current input port.

### with-output-to-file ###
A port is opened from file `string`. This port is made the
current output port and `thunk` is called. 

### with-append-to-file ###
A port is opened from file `string` for append. This port is made the
current output port and `thunk` is called. 

### with-output-to-string ###
A string port is opened. This port is made the
current output port and `thunk` is called. 
Returns the string formed by all the written characters.

### with-output-to-procedure ###
A procedure port is opened. This port is made the
current iutput port and `thunk` is called. 

### with-output-to-port ###
Invokes `thunk` with `port` being made the current output port.

### with-error-to-file ###
A port is opened from file `string`. This port is made the
current error port and `thunk` is called. 

### with-error-to-string ###
A string port is opened. This port is made the
current error port and `thunk` is called. 

### with-error-to-procedure ###
An error port is opened. This port is made the
current error port and `thunk` is called. 

### with-error-to-port ###
Invokes `thunk` with `port` being made the current error port.

### get-output-string ###
Given an output port created by `open-output-string`, 
returns a string consisting of the characters that have been 
output to the port so far. 

@deffn {bigloo procedure} output-port-flush-hook
@deffnx {bigloo procedure} output-port-flush-hook-set
Returns (resp. sets) the @emph{flush hook} of the output
@var{port}. The flush hook is a procedure of two arguments, the output
port and the number of characters that are to be actually written out
during the flush. It is unspecified when the hook is invoked, however,
one may expect the C back-end to invoke the hook only when output
buffers are full. The other back-ends (JVM and DOTNET) are likely to
invoke the hook as soon as a character is to be written.

A flush hook can return two types of values:

@itemize @bullet
@item A string, which is then directly displayed to the system stream
associated with the output port.

@item An integer, which denotes the number of characters of the output port
flush buffer (see @code{output-port-flush-buffer}) that have to be
displayed on the system stream.
@end itemize



@deffn {bigloo procedure} output-port-flush-buffer
@deffnx {bigloo procedure} output-port-flush-buffer-set
These functions gets and sets a buffer that can be used by program by the
flush hooks. The runtime system makes no provision for automatically allocated
these buffers that hence must be manually allocated by programs. The motivation
for flush buffer is to allow programs to write flush hooks that don't have
to allocate a new string each time invoked.


@deffn {bigloo procedure} output-port-close-hook
@deffnx {bigloo procedure} output-port-close-hook-set
Returns (resp. sets) the @emph{close hook} of the output @var{port}. The
close hook is a procedure of one argument, the closed port. The hook 
is invoked @emph{after} the @var{port} is closed.


@deffn {bigloo procedure} input-port-close-hook
@deffnx {bigloo procedure} input-port-close-hook-set
Returns (resp. sets) the @emph{close hook} of the input @var{port}. The
close hook is a procedure of one argument, the closed port.

Example:
@smalllisp
(let ((p (open-input-string "/etc/passwd")))
  (input-port-close-hook-set! p (lambda () (display 'done)))
  ...
  (close-input-port p))
@end smalllisp

@deffn {bigloo procedure} open-input-zlib-file@deffnx {bigloo procedure} open-input-zlib-port
@cindex zip
@cindex gzip

Open respectively a zlib file for input and a port on a zlib stream.
Note that closing a zlib port opened from a port @var{pi} does not close
the @var{pi} port.



@deffn {bigloo procedure} open-input-c-string
Returns an @code{input-port} able to deliver characters from
C @var{string}. The buffer used by the input port is the exact
same string as the argument. That is, no buffer is allocated.


@deffn {bigloo procedure} open-input-ftp-file
Returns an @code{input-port} able to deliver characters from a
remote file located on a FTP server.

Example:

@smalllisp
(let ((p (open-input-ftp-file "ftp-sop.inria.fr/ls-lR.gz'')))
  (unwind-protect
     (read-string p)
     (close-input-port p)))
@end smalllisp
  
The file name may contain user authentication such as:

@smalllisp
(let ((p (open-input-ftp-file "anonymous:foo@@ftp-sop.inria.fr/ls-lR.gz'')))
  (unwind-protect
     (read-string p)
     (close-input-port p)))
@end smalllisp




@deffn {bigloo procedure} unread-char
@deffnx {bigloo procedure} unread-string
@deffnx {bigloo procedure} unread-substring
Pushes the given @var{char}, @var{string} or substring into the input-port.
The next read character(s) will be the pushed ones. The @var{input-port} must
be buffered and not be closed.

Example:

@smalllisp
(define p (open-input-string "a ymbol c"))
(read p)                       @result{} a
(read-char p)                  @result{} #\space
(unread-char! #\s p)
(read p)                       @result{} symbol
(read-char p)                  @result{} #\space
(read p)                       @result{} c
(char-ready? p)                @result{} #f
(unread-string! "sym1 sym2" p)
(char-ready? p)                @result{} #t
(read p)                       @result{} sym1
(read p)                       @result{} sym2
@end smalllisp




@deffn {bigloo procedure} set-input-port-position
@deffnx {bigloo procedure} set-output-port-position
These functions set the file position indicator for @var{port}. The new 
position, measured in bytes, is specified by @var{pos}. It is an error 
to seek a port that cannot be changed (for instance, a procedure or a 
console port). The result of these functions is unspecified. An error
is raised if the position cannot be changed.


@deffn {procedure} read
@deffnx {bigloo procedure} read
@deffnx {bigloo procedure} read-case-sensitive
@deffnx {bigloo procedure} read-case-insensitive
Read a lisp expression. The case sensitivity of @code{read} is unspecified. 
If have to to enforce a special behavior regarding the case, use 
@code{read/case}, @code{read-case-sensitive} or @code{read-case-insensitive}. 
Let us consider the following source code: The value of the @code{read/case}'s
@var{case} argument may either be @code{upcase}, @code{downcase} or 
@code{sensitive}. Using any other value is an error.

```
(define (main argv)
   (let loop ((exp (read-case-sensitive)))
      (if (not (eof-object? exp))
          (begin
             (display "exp: ")
             (write exp)
             (display " [")
             (display exp)
             (display "]")
             (print " eq?: " (eq? exp 'FOO) " " (eq? exp 'foo))
             (loop (read-case-sensitive))))))
```

Thus:

```
> a.out
foo
  &rarr; exp: foo [foo] eq?: #f #t
FOO
  &rarr; exp: FOO [FOO] eq?: #t #f
```

@deffn {bigloo procedure} read
@deffnx {bigloo procedure} read
These functions are fully explained in @ref{Regular Parsing},
and @ref{Lalr Parsing}.


@deffn {bigloo procedure} define-reader-ctor
@cindex SRFI-10

Note: This feature is experimental and might be removed in feature versions.

The present SRFI-10
(@url{http://srfi.schemers.org/srfi-10/srfi-10.html}) proposes an
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

Examples:

```
(define-reader-ctor 'list list) 
(with-input-from-string "#,(list 1 2 #f \"4 5\")" read) @result{} (1 2 #f "4 5")

(define-reader-ctor '+ +)
(with-input-from-string "#,(+ 1 2)" read) @result{} 3
```

@deffn {bigloo procedure} set-read-syntax

Note: This feature is experimental and might be removed in feature versions.

Registers a function @var{procedure} to be invoked with one argument, an
input-port, that is invoked when the reader hits an unparsed character.

Example:

@smalllisp
(set-read-syntax! #\@{
   (lambda (port)
      (let loop ((c (peek-char port)) (exps '()))
	 (cond ((eof-object? c)
		(error "@{" "EOF encountered while parsing @{ ... @} clause" port))
	       ((char=? c #\@})
		(read-char port)   ; discard
		`(begin ,@@(reverse exps)))
	       ((char-whitespace? c)
		(read-char port)   ; discard whitespace
		(loop (peek-char port) exps))
	       (else
		(let ((exp (read port)))
		   (loop (peek-char port)
                      (cons exp exps))))))))
@end smalllisp



@deffn {procedure} read-char
@deffnx {procedure} read-byte
@deffnx {procedure} peek-char
@deffnx {procedure} peek-byte
@deffnx {procedure} eof-object


@deffn {procedure} char-ready
@cindex run-process and char-ready?
@cindex char-ready? and run-process
@cindex run-process and input/output
As specified in the R5Rs, @ref{Ports,,r5rs.info,R5RS}, @code{char-ready?}
returns @t{#t} if a character is ready on the input @var{port} and
returns @t{#f} otherwise.  If @samp{char-ready} returns @t{#t} then
the next @samp{read-char} operation on the given @var{port} is guaranteed
not to hang.  If the @var{port} is at end of file then @samp{char-ready?}
returns @t{#t}.  @var{Port} may be omitted, in which case it defaults to
the value returned by @samp{current-input-port}.

When using @code{char-ready?} consider the latency that may exists
before characters are available. For instance, executing the
following source code:

@smalllisp
(let* ((proc (run-process "/bin/ls" "-l" "/bin" output: pipe:))
       (port (process-output-port proc)))
   (let loop ((line (read-line port)))
      (print "char ready " (char-ready? port))
      (if (eof-object? line)
          (close-input-port port)
          (begin
             (print line)
             (loop (read-line port))))))
@end smalllisp

@noindent Produces outputs such as:

@display
char ready #f
total 7168
char ready #f
-rwxr-xr-x    1 root     root         2896 Sep  6  2001 arch
char ready #f
-rwxr-xr-x    1 root     root        66428 Aug 25  2001 ash
char ready #t
...
@end display

For a discussion of Bigloo processes, see @ref{Process}.

@emph{Note:} Thanks to Todd Dukes for the example and the suggestion
of including it this documentation.


@deffn {bigloo procedure} read-line
@deffnx {bigloo procedure} read-line-newline
Reads characters from @var{input-port} until a @code{#\Newline}, 
a @code{#\Return} or an @code{end of file} condition is encountered. 
@code{read-line} returns a newly allocated string composed of the characters 
read.

The strings returned by @code{read-line} do not contain the newline delimiters.
The strings returned by @code{read-line-newline} do contain them.


@deffn {bigloo procedure} read-lines
Accumulates all the line of an @var{input-port} into a list.


@deffn {bigloo procedure} read-of-strings
Reads a sequence of non-space characters on @var{input-port}, makes a
string of them and returns the string.


@deffn {bigloo procedure} read-string
Reads all the characters of @var{input-port} into a string.


@deffn {bigloo procedure} read-chars
@deffnx {bigloo procedure} read-chars

The function @code{read-chars} returns a newly allocated strings made
of @var{size} characters read from @var{input-port} (or from
@code{(current-input-port)} if @var{input-port} is not provided). If
less than @var{size} characters are available on the input port, the
returned string is smaller than @var{size}. Its size is the number of
available characters.

The function @code{read-chars!} fills the buffer @var{buf} with at most
@var{size} characters.


@deffn {bigloo procedure} read-fill-string
Fills the string @var{s} starting at offset @var{o} with at
most @var{len} characters read from the input port @var{input-port}
(or from @code{(current-input-port)} if @var{input-port} is not provided).
This function returns the number of read characters (which may be smaller
than @var{len} if less characters are available) or the end of file object.
The argument @code{len} is a small integer.

The function @code{read-fill-string!} is similar to @code{read-chars!}
except that it returns the @emph{end-of-file} object on termination while
@code{read-chars!} returns 0.

Example:
@smalllisp
(let ((s (make-string 10 #\-)))
   (with-input-from-string "abcdefghijlkmnops"
      (lambda ()
         (read-fill-string! s 3 5)
         s)))
   @result{} ---abcde--
@end smalllisp


@deffn {bigloo procedure} port-
Returns a list of strings composed of the elements of @var{input-port}.


@deffn {bigloo procedure} port-
@deffnx {bigloo procedure} port-
@code{Port->list} applies reader to port repeatedly until it returns EOF, 
then returns a list of results. 
@code{Port->list-sexp} is equivalent to @code{(port->list read port)}.


@deffn {bigloo procedure} file-
This function builds a new string out of all the characters of the file 
@var{path}. If the file cannot be open or read, an @code{IO_EXCEPTION}
is raised.


@deffn {bigloo procedure} send-chars
@deffnx {bigloo procedure} send-file
Transfer the characters from @var{input-port} to @var{output-port}. This
procedure is sometimes mapped to a system call (such as @code{sendfile} under
Linux) and might thus be more efficient than copying the ports by hand. The
optional argument @var{offset} specifies an offset from which characters of
@var{input-port} are sent. The function @code{send-chars} returns the number
of characters sent.

The function @code{send-file} opens the file @var{filename} in order to
get its input port. On some backends, @code{send-file} might be more efficient
than @code{send-chars} because it may avoid creating a full-fledged Bigloo
@code{input-port}.

Note that the type of @var{len} and @var{offset} is
@code{elong} (i.e., exact long), which is also returned by @code{file-size}.


@deffn {library procedure} write
@deffnx {library procedure} display
@deffnx {bigloo procedure} print
This procedure allows several objects to be displayed. When
all these objects have been printed, @code{print} adds a newline.


@deffn {bigloo procedure} display
This function is similar to @code{print} but does not add a newline.


@deffn {bigloo procedure} fprint
This function is the same as @code{print} except that a
port is provided.


@deffn {procedure} write-char
@deffnx {procedure} write-byte
These procedures write a char (respec. a byte, i.e., in integer in the range
0..255) to the @var{output-port}.


@deffn {procedure} newline
@deffnx {bigloo procedure} flush-output-port
This procedure flushes the output port @var{output-port}. This function
@emph{does not} reset characters accumulated in string port. For this
uses, @code{reset-output-port}.


@deffn {procedure} newline
@deffnx {bigloo procedure} reset-output-port
This function is equivalent to @code{flush-output-port} but in addition,
for string ports, it reset the internal buffer that accumulates the
displayed characters.



@deffn {bigloo procedure} format
@cindex SRFI-28

@emph{Note}: Many thanks to Scott G. Miller who is the author of
SRFI-28. Most of the documentation of this function is copied from the
SRFI documentation.

Accepts a message template (a Scheme String), and processes it,
replacing any escape sequences in order with one or more characters,
the characters themselves dependent on the semantics of the escape
sequence encountered.

An escape sequence is a two character sequence in the string where the
first character is a tilde @code{~}. Each escape code's meaning is as
follows:

@itemize @bullet
@item @code{~a} The corresponding value is inserted into the string 
as if printed with display.
@item @code{~s} The corresponding value is inserted into the string 
as if printed with write.
@item @code{~%} or @code{~n} A newline is inserted A newline is inserted.
@item @code{~~} A tilde @code{~} is inserted.
@item @code{~r} A return (@code{#\Return}) is inserted.
@item @code{~v} The corresponding value is inserted into the string 
as if printed with display followed by a newline. This tag is hence
equivalent to the sequence @code{~a~n}.
@item @code{~c} The corresponding value must be a character and is
inserted into the string as if printed with write-char.
@item @code{~d}, @code{~x}, @code{~o}, @code{~b}  The corresponding value must
must be a number and is printed with radix 16, 8 or 2.
@item @code{~l} If the corresponding value is a proper list, its items 
are inserted into the string, separated by whitespaces, without the 
surrounding parenthesis. If the corresponding value is not a list, it 
behaves as @code{~s}.
@item @code{~(<sep>)} If the corresponding value is a proper list, its items 
are inserted into the string, separated from each other by @var{sep}, 
without the surrounding parenthesis. If the corresponding value is not a list, 
it behaves as @code{~s}.
@item @code{~Ndxob} Print a number in @var{N} columns with space padding.
@item @code{~N,<padding>dxob} Print a number in @var{num} columns 
with @var{padding} padding.
@end itemize

@code{~a} and @code{~s}, when encountered, require a corresponding
Scheme value to be present after the format string. The values
provided as operands are used by the escape sequences in order. It is
an error if fewer values are provided than escape sequences that
require them.

@code{~%} and @code{~~} require no corresponding value.

@smalllisp
(format "Hello, ~a" "World!") 
   @print{} Hello, World!
(format "Error, list is too short: ~s~%" '(one "two" 3)) 
   @print{} Error, list is too short: (one "two" 3)
(format "a ~l: ~l" "list" '(1 2 3))
   @print{} a list: 1 2 3
(format "a ~l: ~(, )" "list" '(1 2 3))
   @print{} a list: 1, 2, 3
(format "~3d" 4)
   @print{}   4
(format "~3,-d" 4)
   @print{} --4
(format "~3x" 16)
   @print{}  10
(format "~3,0d" 5)
   @print{} 005
@end smalllisp


@deffn {bigloo procedure} printf
@deffnx {bigloo procedure} fprintf
Formats @var{objs} to the current output port or to the specified @var{port}.


@deffn {bigloo procedure} pp
Pretty print @var{obj} on @var{output-port}.


@deffn {bigloo variable} *pp-case
Sets the variable to @code{respect}, @code{lower} or @code{upper}
to change the case for pretty-printing.


@deffn {bigloo variable} *pp-width
The width of the pretty-print.


@deffn {bigloo procedure} write-circle
@cindex circular representation
Display recursive object @var{obj} on @var{output-port}. Each component
of the object is displayed using the @code{write} library function.


@deffn {bigloo procedure} display-circle
Display recursive object @var{obj} on @var{output-port}. Each component
of the object is displayed using the @code{display} library function.

For instance:
@smalllisp
(define l (list 1 2 3))
(set-car! (cdr l) l)
(set-car! (cddr l) l)
(display-circle l)  @print{} #0=(1 #0# #0#)
@end smalllisp


@deffn {bigloo procedure} display-string
@deffnx {bigloo procedure} display-substring

@var{String} must be a string, and @var{start} and @var{end} must be exact 
integers satisfying
  `0 &le; start &le; end &le; (string-length string)`.

@code{Display-substring} displays a string formed from the characters
of string beginning with index @var{start} (inclusive) and ending with index
@var{end} (exclusive).


@deffn {bigloo procedure} password
Reads a password from the current input port. The reading stops when the user
hits the ,(code "Enter") key.


@deffn {bigloo procedure} open-pipes
Opens a bi-directional pipes. Returns two values, an @code{input-port} and
an @code{output-port}. The optional argument @var{name} is only used for
debugging.

Example:
@smalllisp
(multiple-value-bind (in out)
  (open-pipes "my pipe")
  (write-char #\z out)
  (flush-output-port out))
@end smalllisp


@deffn {bigloo procedure} select
A wrapper of the Posix @code{select} function. Returns three values,
the three lists of objects that are ready for reading, respectively writing,
or that are in error.

Example:
@smalllisp
(define *inpipe* #f)
(define *outpipe* #f)
(define *watch-mutex* (make-mutex "watch"))
(define *sockets* '())

(define (watch socket onclose)
   (synchronize *watch-mutex*
      (set! *sockets* (cons socket *sockets*))
      (if *outpipe*
	  (begin
	     (write-char *outpipe*)
	     (flush-output-port *outpipe*))
	  (thread-start!
	     (instantiate::hopthread
		(body (watch-thread onclose)))))))

(define (watch-thread onclose)
   (let loop ()
      (synchronize *watch-mutex*
	 (unless *inpipe*
	    (multiple-value-bind (in out)
	       (open-pipes)
	       (set! *inpipe* in)
	       (set! *outpipe* out))))
      (multiple-value-bind (readfs _ _)
	 (select :read (cons *inpipe* *sockets*))
	 (let ((socks (filter socket? readfs)))
	    (for-each onclose socks)
	    (synchronize *watch-mutex*
	       (for-each (lambda (s)
			    (set! *sockets* (remq! s *sockets*)))
		  socks)
	       (unless (pair? *sockets*)
		  (close-input-port *inpipe*)
		  (close-output-port *outpipe*)
		  (set! *inpipe* #f)
		  (set! *outpipe* #f)))
	    (when *outpipe*
	       (loop))))))
@end smalllisp


@deffn {bigloo procedure} lockf
Lock a file descriptor or an output port. It is an error to call
@code{lockf} with an port which is not open on a plain file (i.e., a port open
with @code{open-output-file}, or its variants).

The @var{command} argument is one of:

@itemize @bullet
@item @code{lock}: locks the file, raises an error on failure.
@item @code{ulock}: unlocks the file, raises an error on failure.
@item @code{test}: tests whether a file is locked or not.
@item @code{tlock}: tries to lock a file, return @code{#t} upon success and
  @code{#f} otherwise.
@end itemize

The argument @var{len} is the portion of the file to be locked.


