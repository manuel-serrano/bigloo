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


Modules
=======

Revision 5 introduces a new module system, namely called
_module 5_. Old modules, _module 4_ are still maintained and will be to
ensure backward compatibility with existing code. New modules 5 can
import modules 4 but not it opposite. As much as possible, new code
should use the new modules exclusiverly.

The main characteristics of modules 5 are:

  * Imports are acyclic;
  * All imported bindings must be declared;
  * Modules can re-export imported bindings;
  * Import and export can renamed bindings;
  * Exported bindings are immutable from outside their module of definition;
  * A module is associated to one file;
  * One file defines one module;
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

<MClause> --> <MReExport> | <MExport> | <MImport> | <MMain> | <MLibrary>

<MReExport> --> <MReExportAll> | <MReExportSome>

<MReExportAll> --> ( export <FilePath> )
  | ( export :version 4 <FilePath>+ )
<MReExportSome> --> ( export <FilePath> <Alias> )

<MExport> --> ( export <Alias>+ )

<MImport> --> <MImportInit> | <MImportAll> | <MImportSome>
<MImportInit> --> ( import <FilePath> )
<MImportAll> ( import <FilePath> . <Ident> )
  | ( import :version 5 <FilePath> . <Ident> )
  | ( import :version 4 <FilePath> )
  | ( import <FilePath> <Alias>+ )
  
<MMain> --> ( main ) | ( main <Ident> )
  
<MLibrary> --> ( library <Ident> )

<Include> --> ( include <FilePath> )

<CondExpand> --> bigloo cond-expand form
```
