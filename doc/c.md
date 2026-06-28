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
Bigloo and another language a _foreign interface_. This document
describes the C interface that is available when using the C
backend. The [jvm](./jvm.html) and [wasm](./wasm.html) interfaces are
described in dedicated chapters.

In Bigloo, the foreign interface allows Bigloo's functions and
variables to be exported to a foreign language and foreign functions
and variables to be imported into the Bigloo code. Using the foreign
interface requires two kind of operations.

  * Declarations --- _type_ declarations, _import_ declarations or _export_ declarations.
  * Foreign reference in the Bigloo code.

C backend is the default Bigloo backend. As such, in the absence of
other directive, the compiler generates C code.

> [!IMPORTANT] Foreign declarations take place in a special module
> clause, whose syntax varies depending on the module version (see
> [module 5](./module5.html) and [module 4](./module4.html)). As
> modules 4 are deprecated, only modules 5 are documented here and are
> assumed all along this chapter.

When compiling to C, Bigloo defines the property cond-expand's `C` 
property.


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
   (extern "C" (sum::long ::long ::long))
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


Extern "C" Module Clause
------------------------

When compiling to C, Bigloo reads the `extern "C"` module clauses. There syntax
is:

```bnf
<MCExtern> --> ( extern "C" <MCClause>* )

<MCClause> --> <MCImport>
  | <MCType>
  | <MCExport>
  | <MCInclude>

<MCImport> --> <MCImportVariable>
  | <MCImportFunction>
  | <MCImportMacro>
  
<MCType> --> ( type <Ident> <String> )
  | ( type <Ident> :affinity <Ident> <String> )

<MCExport> --> ( export <Ident> <String> )

<MCInclude> --> ( include <String> )

<MCImportVariable> -->  ( variable <TypedIdent> <TypedIdent>* <String>? )

<MCImportFunction> -->  ( <TypedIdent> <TypedIdent>* <String>? )

<MCImportMacro> --> <MCImportMacroFunction>
  | <MCImportInfixMacroFunction>
  | <MCImportMacroConstant>

<MCImportMacroFunction> --> ( macro <TypedIdent> <TypedIdent>* <String>? )
  | ( macro <TypedIdent> <TypedIdent>+ ... <String>? )

<MCImportInfixMacroFunction> --> ( infix macro <TypedIdent> <TypedIdent>* <String>? )
  | ( infix macro <TypedIdent> <TypedIdent>+ ... <String>? )

<MCImportMacroConstant> --> ( cnst macro <TypedIdent> <String>? )
```

The &lt;MCType&gt; clause enables functions to use C types as result
or arguments. Local variables can also use these types.

The &lt;MCimport&gt; clause _imports_ variables, macros, and functions
in the Bigloo module.

The &lt;MCExport&gt; clause _exports_ a Bigloo function or variable to
C. The C types are thoses given in the Bigloo declaration. The &lt;String&gt;
element specifies the name under which the Bigloo function or variable
&lt;Ident&gt; will be known.

The &lt;MCInclue&gt; clause instructs the Bigloo compiler to emit
a C `#include` statement in the C generated code.

> [!IMPORTANT] C extern imports, i.e., types, functions, variables, 
> and macros, are automatically exported and are visible by all modules 
> that import the present one. Contrary to regular Bigloo definitions,
> C extern definitions cannot be aliased. All modules access them
> under the same unique Bigloo name. 

C types
-------

C types can be used unrestrictively but Bigloo imposes constraints
on those of these types are used by _escaping_ values, i.e., values
that _escape_ the tracking of the compiler that need to be converted
to the most general Bigloo type `obj`. For instance, if a function
is exported or used as a value, the compiler cannot know how the
values it returns will be used. Hence, the compiler must use a _generic
type_ capable of representing any value and whose dynamic can be
inspected. For that the compiler introduces conversions from and to
specific types. 

By default C declared types are not convertible to Bigloo values and
values of that C types cannot escape. When escaping C values are needed,
conversions from the C values to Bigloo values and vice versa must be
specified. This is the purpose of the `:affinity` attribute of the
C type declaration. The value of the `:affinity` property is a Bigloo
type name. Bigloo automatically creates conversions from and to
this Bigloo type.

A common pattern is to wrap escaping C values into Bigloo classes.
This gives more flexibility as common printers and readers can be
defined for classes and this also eliminates the risk of spurious
allocations required for converting the C values back and forth.

Here is an example of such a wrapping.

```bigloo
,(include "./examples/c/wrap.bgl")
```

In this example, the C type `FILE *` is bound to the Bigloo type
`$file*` (a general convention is to prefix identifiers denoting
foreign entities with the `$` sign). A Bigloo class `file*` is created
to _wrap_ these values.

Functions and Macros
--------------------

C functions and C macros are imported in a Bigloo module using the
&lt;MCImportFunction&gt; and &lt;MCImportMacroFunction&gt;. C functions
and C macros are called as any regular Bigloo functions but cannot be
used as first class values. They cannot be passed as
argument, returned as values, or stored into a data structure.

The function result type and Bigloo identifier are extracted from the
first &lt;TypedIdent&gt; of the declaration. The other denotes
the possibly empty parameters. The optional &lt;String&gt;, if given
is the C name of the function, which defaults to the Bigloo
identifier. 

Bigloo does not produce "C extern prototype" for macro functions
(those introduced by &lt;MCImportMacro&gt;. From the BIgloo point of
view, this is the only difference between regular C functions and
C macros. 

In macro import clauses, the symbol `...` is used to refer variadic
functions and macros. Bigloo restricts the optional arguments
to be all of the same type.

Here is an example a Bigloo module that import the C `printf` and
`putchar` functions and call them with Bigloo values. As `printf`
and `putchar` are already declared in the `stdio.h` Bigloo is
prevented from emitting C `extern` declarations for these two
functions by declaring them as _macro_ instead of regular
functions.

```bigloo
,(include "./examples/c/function.bgl")
```

A C macro can be declared `infix` which instructs Bigloo to treat it
as an operator, instead of generating a C function call. Example:

```bigloo
,(include "./examples/c/infix.bgl")
```

Variables and Constants
-----------------------

Variables and constants, i.e., C macros, are imported with the
&lt;MCImportVariable&gt; and &lt;MCImportMacroConstant&gt; extern
clauses. C variables can be assigned (provided there are not C
`const`) while macros cannot. Example:

```bigloo
,(include "./examples/c/var.bgl")
```

Embedded C expressions
----------------------

Bigloo C backend has a special form which allows the inclusion of
foreign text into the produced code.

### (pragma::ident ::bstring arg ...) ###
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


### (free-pragma::ident ::bstring arg ...) ###
<!-- [:free-pragma@NoDef-C] -->

This form is equivalent to a previously described `pragma`
but it tells the compiler that the evaluation of the C
expression does not make any side effect.

### (pragma::ident ::symbol) ###
<!-- [:pragma-id@NoDef-C] -->

This `pragma` enables _injecting_ a Bigloo mangled identifier into the
generated C code.


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

