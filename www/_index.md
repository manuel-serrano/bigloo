${ var doc = require( "hopdoc" ) }

The Bigloo Programming Language
-------------------------------

The Bigloo programming language is a strict-parenthetical-function
programming language. It belongs to the Lisp programming language
family, and more specifically, it is a descendant of the Scheme
programming language.  Its design is governed by one rule: enabling
Scheme-based programming style where C(++) is usually required.

Bigloo aims at being a practical programming language and a practical
programming environment. For it offers features usually presented by
traditional programming languages but seldom supported by functional
programming languages. Bigloo compiles modules. It delivers small and
fast stand alone binary executables. Bigloo enables full connections
with C programs, or with Java programs, if you choose the JVM backend.

The Bigloo C backend is ported to:

  * Linux (x86, x86/64, and arm)
  * MacOS X
  * Android
  * Windows WSL
  
Some projects implemented using Bigloo:

  * [hop](http://hop.inria.fr)
  * [php tools](http://savannah.nongnu.org/projects/phptools)
  
  
${<div class="gallery-comment">It helps Bigloo to have projects listed here
so if you are willing to have you project mentioned on that page
please send a mail to manuel serrano at inria fr for inclusion.</div>}

### Example 1: Bigloo and C

Bigloo makes it easy to share code with C. In that example, the result
of the `fib` Bigloo function operating on Bigloo fixnum numbers will
be passed to the standard `printf` C function. This example also give
a flavor of Bigloo's module.

```scheme
${doc.include( "./fib.scm" )}
```

This Bigloo program can be turned into a binary executable with:

```shell
$ bigloo fib.scm -o fib
$ ./fib 30
```


### Example 2: Multi-Threading

This second example illustrate the use of libraries, multi-threading, and
objects. Pthreads are accessed via the Bigloo `pthread` library as declared
in the module declaration. Threads are instances of the `pthread` class.
Once instantiate they can be spawned and synchronized. Bigloo supports
the traditional smorgasbord of locking and synchronization primtives.


```scheme
${doc.include( "./fib-mt.scm" )}
```

To compile and run this program, simply use:

```shell
$ bigloo fib-mt.scm -o fib-mt
$ ./fib-mt 30
```


### Example 3: Using Libraries

The third example. hardly more complex, shows how to use builtin
libraries to build an efficient music player decoding `.flac` on
the fly.

```scheme
${doc.include( "./flac.scm" )}
```

To compile and run this program, simply use:

```shell
$ bigloo flac.scm -o flac
$ ./flac sample1.flac sample2.flac sample3.flac
```

This toy example rests on five builtin libraries to decode the flac
format, to control the audio card and to play the music. This player
is highly efficient. This player is highly efficient. It has been used
in production on small ARM 200Mhz using only 64MB. Today, installed on
a Raspberry PI, it will deliver more performance than needed to
implement of high quality music streaming system.
