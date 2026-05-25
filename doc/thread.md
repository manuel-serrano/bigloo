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

### thread? ###
Returns `#t` if and only if `obj` is a thread object. Returns `#f` otherwise.

### current-thread ###
Returns the current thread, or `#f` is called from outside any thread.

### make-thread ###
Creates a new thread. Its implementation depends on the library used
in the module. For instance, to create a posix thread the module has
to use the `pthread` library as in:

```bigloo
(module ex
   (library pthread))
   
(make-thread (lambda () (fib 40)))
```

> [!WARNING]
> If no thread library is specified, the function `make-thread` will create
> a _fake_ thread to do not executes its body. 

### thread-start! ###
Starts a thread.

### thread-start-joinable! ###
Starts a _joinable_ thread.

### thread-join! ###
Waits for `thread` to complete. It requires `thread` to be started
with `thread-start-joinable!`. If `thread` was started with `thread-start!`,
`thread-join!` returns immediately without waiting for `thread` to complete.

### thread-kill! ###
Sends `signum` to the thread.

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
<!-- [:@C-jvm] -->
Returns value in the parameter field of the `thread` of the current
thread. If no value has been set, returns `#f`.

A thread parameter is implemented by a chunk of memory specific to
each thread. All threads are created with an empty set of parameters.

### thread-parameter-set! ###
<!-- [:@C-jvm] -->
Sets the parameter of the current thread thread.

### thread-cleanup ###
Returns the cleanup function associated to the thread. The cleanup
function is called with the thread itself. The cleanup function is
executed in a context where `current-thread` is the thread owning the
cleanup function.

If no cleanup procedure is associated with the thread, returns `#unspecified`.

### thread-cleanup-set! ###
Associates a cleanup function to a thread.


Posix Library
-------------

                            
