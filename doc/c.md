<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/c.md                     -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    C backend                                                     -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/foreign.scm")
,(implementation-path "../runtime/Llib/bigloo.scm")
,(example-path "../test/src/extern_c.bgl")

C Backend
=========
<!-- [:@C] -->

We call all the pieces of program devoted to the interactions between
Scheme and another language a _foreign interface_. This document
describes the C interface that is available when using the C
backend. The [jvm](./jvm.html) and [wasm](./wasm.html) interfaces are
described in dedicated chapters.

In Bigloo, the foreign interface allows Bigloo's functions and
variables to be exported to a foreign language and foreign functions
and variables to be imported into the Bigloo code. Using the foreign
interface requires two kind of operations.

  * Declarations --- _type_ declarations, _import_ declarations or _export_ declarations.
  * Foreign reference in the Bigloo code.

> [!IMPORTANT] Foreign declarations take place in a special module clause, whose syntax
> varies depending on the module version (see [module 5](./module5.html)
> and [module 4](./module4.html)). As modules 4 are deprecated, only
> modules 5 are documented here and are assumed all along this chapter.

When compiling to C, Bigloo defines the property `cond-expand` `C` 
property that enables.


Introduction
------------

Connecting Bigloo code with C is generally straightforward. To
illustrate this simplicity, let us consider a simple example involving
two source files. First a simple C file `sum.c` containing a
single declaration:

```C
long sum(long x, long y) { return x + y; }
```

Then, let us assume a Bigloo source code `main.bgl` that makes
uses of that C function:

```bigloo
(module foo
   (extern "C" (sum::long ::long))
   (main))

(define (main x)
   (print (sum (length x) 10)))
```

With a Unix installation of Bigloo, this program can be compiled and
executed with the following commands:

```shell
  $ gcc sum.c -c
  $ bigloo main.bgl sum.o -o main
  $ ./main 1 2 3 &rarr; 13
```

The connection between Bigloo and C is made particularly easy 
because the programmer is free from inserting conversion between Bigloo values
and C values. When needed, these are automatically inserted by the compiler.


Extern Module Clause
--------------------

When compiling to C, Bigloo reads the `extern "C"` module clauses. There syntax
is:

```bnf
<MCExtern> --> ( extern "C" <MCClause>* )

<MCClause> --> <MCImport>
  | <MCType>
  | <MCExport>

<MCImport> --> <MCImportVariable>
  | <MCImportFunction>
  | <MCImportMacro>
  
<MCType> --> ( type <Ident> <String> )

<MCExport> --> ( export <Ident> <String> )
```

Embeeded C expressions
----------------------

Bigloo C backend has a special form which allows the inclusion of
foreign text into the produced code.

### (pragma::ident string arg ...) ###
<!-- [:pragma@NoDef-C] -->

This force Bigloo to include `string` in the produced C code as a
regular C fragment of code. This form must not be used without an in depth
understanding of Bigloo C code production; with unskilled use, the
produced C file may be unacceptable to the C compiler.

Values can be passed to a `pragma` form, being
referenced in `string` by expressions of the form `number`.
Such expression are replaced by the corresponding
values, the number of referenced values in `string`
being exactly the number of values provided. Here is an example
of `pragma` usage:

Arguments provided to a `pragma` form are not converted during compilation.
Hence, pragma arguments can be of any types, including, foreign types.

A pragma result type can be specified using the notation 
`pragma::ident` where the default type is `unspecified`. Then,
for instance, the expression `(pragma::bool "$1 == 0" x)` will
be considered to be returning a object of type `bool` (C boolean) while
the expression `(pragma "$1 == 0" x)` will be considered by
Bigloo to be returning the `unspecified` typed object.


### (free-pragma::ident string arg ...) ###
<!-- [:free-pragma@NoDef-C] -->

This form is equivalent to a previously described `pragma`
but it tells the compiler that the evaluation of the C
expression does not make any side effect.

### (pragma::ident ident) ###
<!-- [:pragma-id@NoDef-C] -->

This `pragma` enables _injecting_ a Bigloo mangled identifier into the
generated C code.


Name Mangling
-------------

In order to avoid name clashes, Bigloo uses name mangling when
compiling to C. The name mangling for a Bigloo identifier may be
overridden by the means of an extern `export` clause.

Four public functions may be used to mangle and to demangle Scheme
identifiers:

### bigloo-mangle ##
Mangle the identifier `string`.

### bigloo-module-mangle ###
Mangle the identifier `string1` that belongs to module `string2`.

### bigloo-mangled? ###
Returns `#t` if `string` has been computed by the `bigloo-mangle`
or `bigloo-module-mangle1 function.

### bigloo-need-mangling? ###
Returns `#t` if `string` requires name mangling because it
is not a C or Jvm valid identifier.

### bigloo-demangle ###
Demangle any type of previously mangled identifiers. it returns one
single value in the case of local identifiers.  In returns two values
when demangling a module idnetifier.

### bigloo-class-mangled? ##
Returns `#t` if `string` is a mangled name of a Bigloo class.

### bigloo-class-demangle ###
Demangles previously mangled class identifier.


Embedded Bigloo Applications
----------------------------

It is possible to design and realize embedded Bigloo
applications. This facility is useful for adding a new Scheme part to
an already existing C program. The C part of the program has only to
enter the Bigloo initialization, hence, it can call any Bigloo
function.

Normally, Bigloo creates an initialization function called 
`main` when it reads a `main` module clause. To use an embedded
Bigloo program, such an initialization function would have to be
created but with a different name. Changing the name can be be done
using the following Bigloo option: `-copt "-DBIGLOO_MAIN=new-name"`. 
To prevent exit from the program
after `new-name` is executed, the following Bigloo option must
be used: `-copt "-DBIGLOO_EXIT='BUNSPEC'"`.

