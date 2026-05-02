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
,(implementation-path "../runtime/Unsafe/gunzip.scm")
,(implementation-path "../runtime/Unsafe/ftp.scm")
,(example-path "../test/src/port.bgl")

Ports
=====

#### Ports buffers ####
<!-- [:buffers@section] --> 

Many ports functions accept an optional argument named `bufinfo`. It is
used to specify the buffer associated with the input/output ports. The
value can either be:

  * `#t`: the port will use a default buffer (generally of size 1024);
  * `#f`: no buffer is used;
  * a string: it is used as read buffer;
  * a fixnum: the size of the buffer to be used;

In case of doubt, the `buffinfo` argument can be ignored, i.e., left to
its default value.

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

### open-input-c-string ###
Returns an `input-port` able to deliver characters from
C `string1. The buffer used by the input port is the exact
same string as the argument. That is, no buffer is allocated.

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

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-input-gzip-port ###
Open a gzipped port for input and a port on a gzipped stream.
Note that closing a gzip port opened from a port `pi` does not close
the `pi` port.

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-input-zlib-file ###
Open a zlib file for input. 

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-input-zlib-port ###
Open a port on a zlib stream for input.
Note that closing a zlib port opened from a port `pi` does not close
the `pi port.

For the the optional argument `bufinfo` see [buffers](#buffers).

### open-input-ftp-file ###
Returns an `input-port` able to deliver characters from a
remote file located on a FTP server.

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

### open-pipes ###
Opens a bi-directional pipes. Returns two values, an `input-port` and
an `output-port`. The optional argument `name` is only used for
debugging.

> [!WARNING]
> The wasm backend does not currently supports pipes.

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
Predicates that returns `#t` if and only if its argument is closed.
It return `#f` otherwise.

### closed-output-port? ###
Predicate that returns `#t` if and only if its arguments is is closed.
It return `#f` otherwise.


Properties
----------

### input-port-name ###
Returns the name of the file used to open the `input-port`.

### input-port-name-set! ###
Sets a different name for the input-port.

### input-port-position ###
The character position in the input-port.

### input-port-position-set! ###
Sets the file position indicator `port`. The new 
position, measured in bytes, is specified by `pos`. It is an error 
to seek a port that cannot be changed (for instance, a procedure or a 
console port). The result of these functions is unspecified. An error
is raised if the position cannot be changed.

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

Setting a port timeout limits the time an read or write operation may
last.  If the `time` limit exceededs, an exception of time
`&io-timeout-error1 is raised.

Setting a timeout equal to 0, restore the socket in blocking mode. Setting
a timeout with a value lesser than 0 is ignored.

Note: ports created from sockets share their internal file descriptor. Hence
it is erroneous to set a timeout for only one of the two ports. Both
must be set.

### output-port-name ###
Returns the name of the file used to open the `output-port`.

### output-port-name-set! ###
Sets a different name for the output-port.

### output-port-position ###
Returns the current position (a character number), in the `port`.

### output-port-position-set! ###
Sets the file position indicator `port`. The new 
position, measured in bytes, is specified by `pos`. It is an error 
to seek a port that cannot be changed (for instance, a procedure or a 
console port). The result of these functions is unspecified. An error
is raised if the position cannot be changed.

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

### output-port-flush-hook ###
Returns the _flush hook_ of the output `port`. 

### output-port-flush-hook-set! ###
Sets the _flush hook_ of the output `port`. The flush hook is a procedure 
of two arguments, the output port and the number of characters that are to 
be actually written out during the flush. It is unspecified when the hook 
is invoked, however, one may expect the C back-end to invoke the hook only 
when output buffers are full. The other back-ends are likely to
invoke the hook as soon as a character is to be written.

A flush hook can return two types of values:

  * A string, which is then directly displayed to the system stream
    associated with the output port.

  * An integer, which denotes the number of characters of the output port
    flush buffer (see `output-port-flush-buffer) that have to be
    displayed on the system stream.

### output-port-flush-buffer ###
Returns the flush buffer of the output port.

### output-port-flush-buffer-set! ###
Sets a buffer that can be used by program by the flush hooks. The
runtime system makes no provision for automatically allocated these
buffers that hence must be manually allocated by programs. The
motivation for flush buffer is to allow programs to write flush hooks
that don't have to allocate a new string each time invoked.

### output-port-close-hook ###
Returns the _close hook_ of the `port`

### output-port-close-hook-set! ###
Sets the _close hook_ of the output `port`. The
close hook is a procedure of one argument, the closed port. The hook 
is invoked _after_ the `port` is closed.

### input-port-close-hook ###
Returns the _close hook_ of the `port`

### input-port-close-hook-set! ###
Sets the _close hook_ of the input `port1. The
close hook is a procedure of one argument, the closed port.

### select ###
A wrapper of the Posix `select` function. Returns three values,
the three lists of objects that are ready for reading, respectively writing,
or that are in error.

> [!WARNING]
> Only supported by the C backend.

### lockf ###
Locks a file descriptor or an output port. It is an error to call
`lockf` with an port which is not open on a plain file (i.e., a port open
with `open-output-file`, or its variants).

The `command` argument is one of:

  * `lock`: locks the file, raises an error on failure.
  * `ulock`: unlocks the file, raises an error on failure.
  * `test: tests whether a file is locked or not.
  * `tlock`: tries to lock a file, return `#t` upon success and
  `#f` otherwise.

The argument `len` is the portion of the file to be locked.

> [!WARNING]
> Only supported by the C backend.


