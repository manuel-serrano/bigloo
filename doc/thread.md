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
,(implementation-path "../runtime/Llib/semaphore.scm")
,(implementation-path "../api/pthread/src/Llib/psemaphore.scm")
,(example-path "../test/src/thread.bgl")

Threads
=======

When the backend supports native multithreading, Bigloo supports
Posix multithreaded programming. It easily to take benefit of the
actual parallelism that is now available on stock hardware.

The multithreading support is organized in:

  * a base front-end API;
  * libraries that implements the common API and that extend it.

> [!WARNING]
> As of May 2026, multi-threading is not supported by the Wasm backend.


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

### (class thread ...) ###
<!-- [:synchronize@NoDef] --> 
This is an abstract class from which all thread implementations, e.g., 
posix threads or stfi-18 threads, inherit.

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

The `make-thread` function is a mere wrapper that instantiate a thread
class instance depending on the library used by the module.

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

Mutexes
-------

### mutex? ###
Returns `#t` if and only if `obj` is a mutex. Returns `#f` otherwise.

### make-mutex ###
Creates a mutex.

### make-spinlock ###
The function `make-spinlock` creates a spin lock on architectures
on support it, otherwise it creates a regular mutex as if `make-mutex`
was called. The support for spin lock can be checked with:

`bigloo-config 'have-spinlock`

On some hardware, a spinlock is more efficient than a full-fledged
mutex but a a spinlock cannot be used for synchronizing condition
variables.

### mutex-name ###
Returns the name of the mutex.

### mutex-state ###
Returns a description of the state of the mutex. The description of the
state depends of the thread backend.

### mutex-lock! ###
Locks the mutex.

### mutex-unlock! ###
Unlocks the mutex.

### (synchronize ...) ###
<!-- [:synchronize@NoDef] --> 
The form `synchronize` evaluates the expression `exp1`,
`exp2`, etc. The mutex `mutex` is acquired and released before
`exp1` gets evaluated.  Its value is the value of the evaluated
expression. The form `synchronize` ensures that however the form
returns, the mutex `mutex` is always unlocked.

Condition Variables
-------------------

### condition-variable? ###
Returns `#t` if and only if, `obj` is a condition variable.

### make-condition-variable ###
Creates a fresh condition variable.

### condition-variable-name ###
Returns the name of the condition variable.

### condition-variable-wait! ###
Blocks the thread on the condition variable `cv`. Requires `mutex` to
be lockes but the thread. On waiting for `cv`, it releases the `mutex`, which
is automatically re-locked on `cv`.

### condition-variable-signal! ###
Notifies one of the threads blocks on the condition variable `cv`.

### condition-variable-broadcast! ###
Notifies all of the threads blocks on the condition variable `cv`.

Posix Library
-------------

Posix threads are supported by the Bigloo 'pthread' library. The use of
the library must be specified in the module declaration:

```bigloo
(module ex
  (library pthread)
  ...)
 ...)
```

A pthread mutex state can either be:

  * `lock`: the mutex has been locked by a thread.
  * `unlock`: the mutex is currently unlocked.

### (class pthread::thread ...) ###
<!-- [:pthread@NoDef-C-jvm] --> 
Intantiating a `pthread` object creates new thread which is not
started yet. The body of the thread is the body of the procedure
`thunk`. The optional argument `name` can be use to identify the
thread. It can be any Bigloo value.

### open-semaphore ###
<!-- [:@C] -->
Creates a new semaphore or opens an existing semaphore. The semaphore is
identified by `name`.

### semaphore? ###
<!-- [:@C] -->
Returns `#t` if and only if `obj` is semaphone.

### close-semaphore ###
<!-- [:@C] -->
Closes a semaphore object.

### delete-semaphore ###
<!-- [:@C] -->
Deletes a semaphore object.

### semaphore-post ###
<!-- [:@C] -->
Posts on a semaphore.

### semaphore-wait ###
<!-- [:@NoTest-C] -->
Waits for a semaphore, see `semaphore-post` for an example.

### semaphore-trywait ###
<!-- [:@C] -->
Waits for a semaphore.

### semaphore-value ###
<!-- [:@C] -->
Returns the current value of the semaphore

SRFI-18
-------

[Srfi-18](http://srfi.schemers.org/srfi-18/srfi-18.html) are supported
by the Bigloo `srfi-18` library. The use of this library must be
specified in the module declaration:

```bigloo
(module ex
  (library stfi-18)
  ...)
 ...)
```

A srfi-18 mutex state can either be:

  * thread `t`: the mutex is in locked/owned state the thread `t` is the other 
    of the mutex;
  * `not-owned`: the mutex is in the lock/not-owned state;
  * `abandonned`: the mutex is in the unlocked/abandond state;
  * `not-abandonned`: the mutex is in the unlocked/not-abandoned state.





                            
