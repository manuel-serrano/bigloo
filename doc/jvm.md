<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/jvm.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    JVM backend                                                   -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/foreign.scm")
,(implementation-path "../runtime/Llib/bigloo.scm")
,(example-path "../test/src/extern_jvm.bgl")

JVM Backend
===========
<!-- [:@jvm] -->

We call all the pieces of program devoted to the interactions between
Bigloo and another language a _foreign interface_. This document
describes the Jvm interface that is available when using the C
backend. The [C](./c.html) and [wasm](./wasm.html) interfaces are
described in dedicated chapters.

The Bigloo jvm foreign interface allows import of Java classes, which
enables Bigloo code to invoke Java methods (static or not) and to
access object fields and class static fields. It also enables application
to export functions for Java code.

> [!NOTE] The Jvm interface does not support Java class definitions. 
> Consequently, programming environments that requires new classes to be
> declared, need a mix of Java code and Bigloo. A complete example can
> be found in the Android section.

Introduction
------------

Connecting Bigloo code with Java is generally straightforward. To
illustrate this simplicity, let us consider a simple example involving
three source files. First a Java interface `Intf.java`:

```Java
,(include "../test/src/Intf.java" :line-start 11)
```

```Java
,(include "../test/src/Point.java" :line-start 11)
```

