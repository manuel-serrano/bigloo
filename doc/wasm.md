<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/wasm.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    WASM backend                                                  -->
<!--==================================================================-->

Wasm Backend
===========
<!-- [:@wasm] -->

We call all the pieces of program devoted to the interactions between
Bigloo and another language a _foreign interface_. This document
describes the Wasm interface that is available when using the Wasm
backend. The [C](./c.html) and [Jvm](./jvm.html) interfaces are
described in dedicated chapters.

The Bigloo wasm foreign interface allows import of Wasm and Javascript
functions, ad Wasm and JavaScript variables, which enables Bigloo code
to invoke these functions and to access these variables. It also
enables application to import Bigloo functions and variables for Wasm code.

To generate Wasm object files, the Bigloo compiler has to be invoked
with the `-wasm` command line option. See `bigloo -help` for all the
Jvm related option. 

When compiling to Wasm, Bigloo defines the property cond-expand's `wasm` 
property. Example

```shell
$ cat > foo.scm
(module foo (main))
(define (main argv)
   (print "Hello world: " argv))
$ bigloo -O3 -wasm foo.scm -o foo.wat
$ bigloo -O3 foo.wat
$ a.out
  &rarr; Hello world: (a.out)
```

> [!NOTE] The Jvm interface does not support Java class definitions. 
> Consequently, programming environments that requires new classes to be
> declared, need a mix of Java code and Bigloo. A complete example can
> be found in the Android section.

Introduction
------------
