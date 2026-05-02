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

@deffnx {procedure} peek-byte
Reads a byte from `ip` without consuming it. Returns either a
byte or the end-of-file object.

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


