<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/error.md                 -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Errors                                                        -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/error.scm")
,(implementation-path "../runtime/Llib/object.scm")
,(example-path "../test/src/error.bgl")

Errors and Warnings
===================
Bigloo permits to signal an error via the error functions. Errors are
implemented by the means of exceptions (see `try` and `raise` forms).
Assertions allow the checking of predicates at certain points in programs.  


Classes
-------

### &condition ###

### &exception ###

### &error ###

### &io-error ###

### &type-error ###

### &warning ###

Library Functions
-----------------

### raise ###
Raises an exception.

### error ###
Raises an `&error` built from its arguments.

Switching on the `-g` compilation switch enables stack dumping when
the `error` function is invoked. That is, when a program is compiled
with `-g` and when, at runtime, the shell variable `BIGLOOSTACKDEPTH`
is set and contains a number, an execution stack of depth
`BIGLOOSTACKDEPTH` is printed when an error is raised.

### error/location ###
Raises an `&error` built from its arguments. The error is prompted
in `fname`, at character position `loc`.

The `fname` designed the source file where the error occurred. The
`loc` argument is the character number in the source of the
error. These values are used by the default error handler to display
the source location along with the reason of the error when it is
raised.

### bigloo-type-error ###
Raises a `&type-error` object.

### bigloo-type-error/location ###
Raises a `&type-error` object located at `fname` and `loc`.

### warning ###
Raises a warning message.

### warning/location ###
Raises a warning message located at `fname` and `loc`.

### exception-notify ###
Displays an exception report on the current error port.

### error-notify ###
Displays an error report on the current error port.

### error-notify/location ###
Displays an error message located at `fname` and `loc` on the current
error port.

### warning-notify ###
Displays a warning message on the current error port.

### warning-notify/location ###
Displays a warning message located at `fname` and `loc` on the current
error port.

### get-trace-stack ###
Switching on the `-g` compilation switch enables stack dumping.  That
is, the list of the pending calls can be dumped by the
runtime-system. The function `get-trace-stack` builds such a
trace. The list built by `get-trace-stack` only contains the
`size` top most pending calls. 

### dump-trace-stack ###
The function `dump-trace-stack` displays a representation of a stack obtained
with `get-trace-stack` on the current output-port.
