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

The Bigloo Wasm backend produces `.wat` files. Bigloo can also invoke
transparently a wat-to-wasm compiler such as `wasm-as` to generate directly
binary files that can be loaded inside wasm machines.

The Bigloo linker, when linking, Wasm files, produces a shell script file
that can be used to execute the program with `nodejs`, `mozjs`, and `jsc`. 
Each of these systems uses its own implementation for primitive native
operations, e.g., sockets API. As of July 2026, only nodejs implement
all the Bigloo Wasm features and as such is highly recommended. The other
systems could be used to testing and performance measurements but probably
not for production code.

In addition to executing Wasm code in a server-side implementations,
the generated Wasm code can be executed by Web client in collaboration
to JavaScript code.

> [!NOTE] Whether it is a server-side implementation such as Nodejs or a web
> client such as Firefox or Chrome, Wasm code communicates with the host
> environment via JavaScript but the Bigloo Wasm generated code can only 
> communicate with Wasm. 

Extern "wasm" Module Clause
---------------------------

```bnf
<MWExtern> --> ( extern "wasm" <MWClause>* )

<MWClause> --> <MWImportClause>
  | <MWExportClause>
  
<MWImportClause> --> ( <WModuleName> <Ident> <Ident>* )

<MWExportClause> --> ( export <MWExport>+ )

<MWExport> --> <Ident>
  | ( <Ident> <String> )
  
<WModuleName> --> <String>
```

Web Example
-----------

Let us consider a minimal example showing how to use Bigloo wasm compiled code
in a web application. First, let us consider a minmal Bigloo module that 
implements function named `click` that increments a counter when called and
that retreive the element `console` of the current web page and that inserts
the counter value in that HTML element.

```bigloo
,(include "examples/wasm/click.bgl")
```

In order to get the HTML element and to modify it, the Bigloo code
uses the facilities of the [`browser`](./browser.html) library. To
make the `click` function visible from within Wasm code, the function
is exported by Bigloo under the same name.

The Bigloo source file can be compiled with:

```shell
$ bigloo -wasm click.bgl -c
```

The HTML page could be implemented as:

```html
,(include "examples/wasm/click.html")
```

The body of the page merely creates the `console` html element and the button.
The most interesting part is the script of the head part. In this code
`BIGLOOROOT` stands for the location where Bigloo is installed. 

The JavaScript file `bigloo-web.mjs` is the Bigloo runtime system
implementation for the web. The Bigloo code refers to the `browser`
library, which must then be loaded on the HTML page. Firt it is
declared (the `libs` variable) and then used to create the WebAssembly
machine (the call to `runDynamic`). This calls returns a JavaScript
object containing all the exports of the Wasm code. In our example, this
contains the `click` function. It is bound the the JavaScript global 
environment (the JavaScript `globalThis` variable) that can then use
it in the attribute of the HTML button element.



