<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Main page                                                     -->
<!--==================================================================-->

## Main Features ##

Bigloo (aka bgl), is a strict, typed, dynamic language. it aims at
being lightweight, fast, and portable.

It supports three backends:

  * Native (x86, x86/64, and arm), MacOS X, and Windows WSL;
  * Jvm (desktop, Android OS);
  * WebAssembly (desktop, web browsers).

The language supports:

  * modules for separate compilation;
  * preemptive multi-threading with shared memory;
  * an object system based on classes and generic functions;
  * exceptions;
  * rich set of builtin libraries (threads, multimedia, databases, ...).
  
The implementation provides:

  * an optimizing AoT compilation;
  * safe and unsafe execution modes.
  
## Examples ##

### Bigloo and C
<!-- [:@C] -->

Bigloo makes it easy to share code with C. In that example, the result
of the `fib` Bigloo function operating on Bigloo fixnum numbers will
be passed to the standard `printf` C function. This example also give
a flavor of Bigloo's module.

```bigloo
,(include "./examples/c/fib.bgl")
```

This Bigloo program can be turned into a binary executable with:

```shell
$ bigloo fib.bgl -o fib
$ ./fib 30
$ file ./fib
./fib: ELF 64-bit LSB pie executable, x86-64, version 1 (SYSV), dynamically linked, stripped
```

### Bigloo and Java
<!-- [:@jvm] -->

A similar example could be implemented for the Jvm using the java
`System.out.println` method. It simply requires the "C" ad-hoc clause with
one suitable for Java:

```bigloo
,(include "./examples/java/fib.bgl")
```

This Bigloo program can be turned into a Jvm class with:

```shell
$ bigloo fib.bgl -o fib -jvm
$ ./fib 30
$ file ./fib
./fib: POSIX shell script, ASCII text executable
$ file ./fib.class
./fib.class: compiled Java class data, version 45.3
```

### Bigloo and Wasm
<!-- [:@wasm] -->

A similar example could also be built for WebAssembly. It could run on 
any WebAssembly 3.0 implementation.

```bigloo
,(include "./examples/wasm/fib.bgl")
```

This Bigloo program can be turned into a wasm file with:

```shell
$ bigloo fib.bgl -o fib -wasm
$ ./fib 30
$ file ./fib
./fib: POSIX shell script, ASCII text executable
$ file ./fib.wasm
./fib.wasm: WebAssembly (wasm) binary module version 0x1 (MVP)
```

