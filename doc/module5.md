<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/boolean.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Modules                                                       -->
<!--==================================================================-->


Modules 5
=========

Revision 5 introduces a new module system, henceforth refereed to as
_module 5_. Old modules, _module 4_, are still maintained and will be to
ensure backward compatibility with existing code. Modules 5 can
import modules 4 but not it opposite. As much as possible, new code
should use modules 5 exclusively.

The main characteristics of modules 5 are:

  * Dependency graphs are acyclic, i.e., a module cannot import itself;
  * All imported bindings must be explicitly declared;
  * Modules can re-export imported bindings;
  * Imports and exports can renamed bindings;
  * Exported bindings are immutable from outside their module of definition;
  * A module is associated to one file, one file defines one module;
  * Modules are referenced by files path.
  

Syntax
------

```bnf
<Module> --> ( module <Ident> <MExpression>* ) <Expression>*

<Ident> --> any legal Bigloo identifier
<FilePath> --> <String>
<Alias> --> <Ident> | ( <Ident> <Ident> )
<Expression> --> any Bigloo expression

<MExpression> --> <MClause> | <Include> | <CondExpand>

<MClause> --> <MExport> 
  | <MReExport> 
  | <MImport> 
  | <MMain> 
  | <MLibrary>
  | <MExtern>

<MExport> --> ( export <Alias>+ )

<MReExport> --> <MReExportAll> | <MReExportSome>
<MReExportAll> --> ( export <FilePath> )
  | ( export :version 4 <FilePath>+ )
<MReExportSome> --> ( export <FilePath> <Alias> )


<MImport> --> <MImportInit> | <MImportAll> | <MImportSome>
<MImportInit> --> ( import <FilePath> )
<MImportAll> ( import <FilePath> . <Ident> )
  | ( import :version 5 <FilePath> . <Ident> )
  | ( import :version 4 <FilePath> )
  | ( import <FilePath> <Alias>+ )
  
<MMain> --> ( main ) | ( main <Ident> )
  
<MLibrary> --> ( library <Ident>* )

<MExtern> --> ( export <String> <EClause>+ )

<EClause> --> extern language depend clause

<Include> --> ( include <FilePath> )

<CondExpand> --> bigloo cond-expand form that expands into an <MExpression>
```

Export
------

A module exports some of the definitions it declared with the `export` clause.
Functions, generic functions, inline functions, variables, and classes can
all be exported. Variables are mutable inside the module that defines them
but immutable in modules that import them.

In this simple example, the module `ex8` exports the macro `ex8m` and the
function `ex8f`.

[simple export](../test/src/modules/module5_ex0.bgl)

Note that in this example, that although the variable `ex0a` is
mutated from within `ex0` each time the exported function `ex0b` is
called, it cannot be directly mutated from the modules that import it.

An export clause can alias an exported declaration, i.e., it can make
the declaration visible under another name from the module that import it.

This example illustrates the different manners of exporting definitions.
The definitions `ex1a` and `ex1c` are exported under their definition names,
and `ex1b` is exported under the name `EX1b`.

[aliasing](../test/src/modules/module5_ex1.bgl)

ReExport
--------

A module can _re-export_ definitions it imports from other modules. A re-export
clause might alias the re-exported definition.

In the following example, the module `ex4` export the definition of `ex3b`
exported by `module5_ex3.bgl` under its exported name and the definition
of `ex3a` under then name `EX4A`, i.e., the name that we will visible form
the that imports `ex4`. It all exports all the exported definitions of the
module `module5_ex5.bgl`.

[re-exporting from two modules](../test/src/modules/module5_ex4.bgl)

Import
------

In an `import` directives, the imported module is designed under its
path name _relative_ to the module path itself. These names are
independent from the directory from where the compiler is invoked.

> [!WARNING]
> Module imports are acyclic, meaning that a module cannot import
> itself, directly or indirectly. The compiler detects such cycles
> and raises errors.

An import can be selective, i.e., import only some exported variables
with or without aliasing. For instance, in the following example,
module `ex19` import `ex0a`, `ex0b`, and `ex0c` from module
`module5_ex0.bgl` without aliasing but it imports variables
`EX2a` from module `module5_ex2.bgl` aliasing it, i.e., making it
visible under the name, `EX2A`.

An import can be global, meaning that all the exported variables
are made accessible, under a _qualified name_. In the following
example, all variables of the module `module5_ex1.bgl` and accessible
as exported with the prefix `ex1`, and similarly for the modules
`module_ex4.bgl`, and module `module5_ex17.bgl`. For instance, assuming
that `module_ex4.bgl` exports the variables `ex3b` and `ex3a` aliased 
`EX4A`, the module `ex19` accesses it under the name `ex4.ex3b` and
`ex4.EX4A`.

Modules 5, can import modules 4. This is what `module5_ex19.bgl` does in the
following example. It imports `module4_ex12.scm`, which is a module 4.

At last, a module can import another module only its initialization, not
for importing any definition. In the example below, `module5_ex7.bgl` is
imported only for initialization.

[imports](../test/src/modules/module5_ex19.bgl)

Include
-------

Module clauses can be inlined in the module declaration as they can be
placed in separated _include_ files. As for import path references,
include path reference are relative to the file being compiled.

The following example illustrates this feature. The module
`module5_ex3.bgl` exports the variable `ex3b` and include the file
`module5_ex3.bgh`, which also contains an export clause that exports
the variable `ex3a`. The modules that import `module5_ex3.bgl`, will
have access to both `ex3a` and `ex3b`.

[including](../test/src/modules/module5_ex3.bgl)

Note that a module include file is a mere file containing a list of
directives that is appended to the module declaration.

[include](../test/src/modules/module5_ex3.bgh)


Extern
------

The extern module clause is used to imported variables, functions, and macros
from a foreign language. The syntax of the extern clauses depends on each
backend. The following example shows an example of a module importing the
C macro `$printf` when compiling to C and the Wasm function `$printf` when
compiling to Wasm. This module could not be compiled to Jvm as it provides
not declaration of `$printf` for that backend.

[extern](../test/src/modules/module5_ex7.bgl)

Library
-------

A library is a collection of modules grouped together and exposed as if
implemented in a single module. Using variables of a library differ
from importing variable from a module in that all the variables a library
exports can be used inside a module without explicitly mentioning them.
In the following example, several variables of the `text` library are
used from within `module5_ex20.bgl`.

[using libraries](../test/src/modules/module5_ex20.bgl)

> [!WARNING]
> Variable imported from a library cannot be directly re-exported in contrast
> to variables imported from regular modules.

Conditional Clauses
-------------------

The form `cond-expand` can be used inside a module declaration. In that
case, the `cond-expand` must expands into a legal module clause that
will take place inside the module declaration depending on the condition.

In the following example, the variable `get-element-by-id` from the 
library `browser` is imported only when compiling for Wasm.

[conditional clause](../test/src/modules/module5_ex15.bgl)


The module object
-----------------

Inside a module the variable `filename` qualified with the module identifier
is a constant bound to the actual path of the module.

In the following example, the module `ex2`, exports its file name via the
variable `ex2c`.

[Module path](../test/src/modules/module5_ex2.bgl).

