
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
,(implementation-path "../runtime/Llib/bconfigure.scm")
,(example-path "../test/src/bigloo.bgl")

Bigloo Specifics
================

Configuration
-------------

### bigloo-config ###

This function enables programs to query properties of the current
installation. If `key` is not provided, its returns the whole alist of
properties. If `key` is provided and correspond to an existing
configuration property, this value is returned. Otherwise, the
`bigloo-config` returns `#unspecified`.

Name Mangling
-------------

In order to avoid name clashes, Bigloo uses name mangling when
generating native code, i.e., C, jvm byte code, or Wasm byte code. The
name mangling for a Bigloo identifier may be overridden by the means
of an extern `export` clause.

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



