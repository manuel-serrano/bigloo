<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/thread.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Threads                                                       -->
<!--==================================================================-->

,(include "head.html")

,(implementation-path "../runtime/Llib/thread.scm")
,(example-path "../test/src/thread.bgl")

Threads
=======

When the backend supports native multithreading, Bigloo supports
Posix multithreaded programming. It easily to take benefit of the
actual parallelism that is now available on stock hardware.

> [!WARNING]
> As of May 2026, hardware parallelism is not supported by the Wasm backend.

The multithreading support is organized in:

  * a base front-end API;
  * libraries that implements the common API and that extend it.
  
Base API
--------

The base front-end implements [SRFI-18 (Multithreading
support)](http://srfi.schemers.org/srfi-18/srfi-18.html). As provided
by the standard Bigloo runtime system, all the functions are of the
base API are implemented with empty functions that merely satisfy
their type constraints. The Bigloo distribution contains two
implementations that map the base API to the native host
multithreading: the _srfi-18_ impementation and the _posix_
implementations. The selection of the library is made in the module
claus using a `library clause (see [modules](./module.html). One
application can mixed the two library.

The posix library being a superset of the srfi-18 library, in doubt, it is
recommended to prefer it.

This section describes the functions that are available independently
of the multi-threading library.

Bigloo uses a set of _primitive_ functions and methods to create,
run and handle thread. For the sake of standardization the name and
semantic  of SRFI-18 has been used. This section presents only the
mandatory functions to program with threads in Bigloo.

The most important difference with SRFI-18, is the missing of the
function `make-thread`, which is not available for all libraries,
as it can be hard  to predict the type of thread which will be created
if several thread libraries are used simultaneously. As threads are
regular Bigloo objects, they can be created using the
`instantiate` syntax. See the [Posix Threads](./thread.html#posix)
specific sections for more details about thread creation and
examples.

The examples given in this section use a _generic_
syntax with `instantiate::thread`, to run the examples, you will
have to put them in a function in a module (see [modules](./module.html))
and import one of the libraries using `library` module
declaration.

### current-thread ###
Returns the current thread.

### thread? ###
Returns `#t` if and only if `obj` is a thread object. Returns `#f` otherwise.

### thread-name ###
Returns the name of a thread.

### thread-name-set! ###
Sets the name of a thread.

### thread-specific ###
Returns value in the specific field of the `thread`. If no
value has been set, returns `#unspecified`.

### thread-specific-set! ###
Sets the specific of a thread.

### thread-parameter ###
Returns value in the parameter field of the `thread`. If no
value has been set, returns `#f`.

A thread parameter is implemented by a chunk of memory specific to
each thread. All threads are created with an empty set of parameters.

### thread-parameter-set! ###
Sets the parameter of a thread.


Posix Library
-------------

                            
