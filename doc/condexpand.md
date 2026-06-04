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

,(implementation-path "../runtime/Eval/expdsrfi0.scm")
,(example-path "../test/src/condexpand.bgl")


Conditional Execution
=====================

### (cond-expand ...) ###
<!-- [:cond-expand@NoDef] -->

The `cond-expand` form (see
[SRFI-0 (Feature-based conditional expansion construct)](https://srfi.schemers.org/srfi-0)), 
tests for the existence
of features at macro-expansion time. It either expands into the body
of one of its clauses or signals and error during syntactic
processing. The `cond-expand` form expands into the body of the first
clause whose feature requirement is currently satisfied (the
`else` clause, if present, is selected if none of the previous
clauses is selected).

A feature requirement has an obvious interpretation as a logical
formula, where the variables have meaning _true_ is the feature
corresponding to the feature identifier, as specified in the _SRFI_
registry, is in effect at the location of the `cond-expand` form,
and _false_ otherwise. A feature requirement is satisfied it its
formula is true under this interpretation. The formula may make use of
identifier, `and`, `or` and `not` operators.

Bigloo extends the SRFI-0 formula syntax in several directions, for
testing the support of a library or a configuration. The syntax
of the extensions are:

```bnf
<CondExpandFormula> --> srfi-0 formulas
  | ( library <ident> )
  | ( config <ident> <expr> )
```

Examples: 

```bigloo
(cond-expand
   ((and (library pthread) (library multimedia)) #t))
    &rarr; #t ;; iff the pthread and multimedia libraries are avaiable

(cond-expand 
   ((config int-size 61) #t))
    &rarr; #t ;; iff host integers are 61 bits wide
   
(cond-expand 
   ((config endianeness little-endian) #t))
    &rarr; #t ;; iff host is little-endian
```

The form `cond-expand` can be used the body of a module and in its
declaration. The following example illustrates the use of `cond-expand`
in module clauses:

```bigloo
(module ex
   (cond-expand
      ((library pthread) (include "multithread.bgh"))
      (else (include "singlethread.bgh"))))
```

When writing portable code, the case used for the feature identifier
should match the one in the SRFI registry. This is to ensure that the
feature identifier will be correctly recognized whether or not the
Scheme system is case-sensitive. To support case-insensitive Scheme
systems, the feature identifiers in the SRFI registry are guaranteed to
be unique even when ignoring the case.

In order to distinguish Bigloo versions, the following symbols are
recognized in `cond-expand` forms.

  * `bigloo`
  * `bigloo&lt;branch-release&gt;`
  * `bigloo&lt;major-release&gt;`
  * `bigloo&lt;major-release&gt;&lt;minor-release&gt;`

When finalizers have been configured, the two following symbols are
recognized by `cond-expand`:

  * `bigloo-finalizer`
  * `bigloo-weakptr`

Bigloo implements differents SRFI for the compiler and the interpreter.
Thus, their are two Bigloo SRFI registers. One for the compiler and one
for the interpreter. Bigloo compiler SRFI register contains at least the 
following symbols: 

  * `srfi-0`
  * `srfi-1`
  * `srfi-2`
  * `srfi-6`
  * `srfi-8`
  * `srfi-9`
  * `srfi-22`
  * `srfi-28`
  * `srfi-30`

With respect to the used Bigloo back-end, one of these symbols is
registered:

  * `bigloo-c`, `C`
  * `bigloo-jvm`, `jvm`
  * `bigloo-wasm`, `wasm`
  
Bigloo compiler implements the following SRFI:


  * `bigloo-compile`
  * `bigloo-compile&lt;major-release&gt;`
  * `bigloo-compile&lt;major-release&gt;&lt;minor-release&gt;`

Then the `-g` compilation flag is set, the Bigloo compiler
additionally implements the SRFI:

  * `bigloo-debug`

Bigloo interpreter implements the following SRFI: 

  * `bigloo-eval` 
  * `bigloo-eval&lt;major-release&gt;`
  * `bigloo-eval&lt;major-release&gt;&lt;minor-release&gt;`

When a library is used, the name of the library is added to the compiler SRFI
register. For instance:

```bigloo
(module foo
   (library srfi1))

(print (cond-expand (srfi1 'with-srfi1) (else 'nothing)))
   &rarr; 'with-srfi1
(print (eval '(cond-expand (srfi1 'with-srfi1) (else 'nothing))))
   &rarr; 'with-srfi1
```

> [!WARNING] This should not be confused with the formula syntax 
> `(library &lt;ident&gt;)` that checks the existence of a library.


A property representing actual integers bit size is defined:

  * `bint&lt;integer-bit-size&gt;`
  * `elong&lt;exact-long-bit-size&gt;`

The frequently defined values are:

  * `bint30`: 32-bit architectures
  * `elong32`: 32-bit architectures
  * `bint32`: jvm 
  * `elong64`: jvm 
  * `bint61`: 64-bit architectures
  * `elong64`: 64-bit architectures

Other values could be observed in the future. Note that the actual
values of a particular setting can be obtained with:

```bigloo
(bigloo-config 'int-size)
(bigloo-config 'elong-size)
```
A configuration can be tested with:

  * `config key value`

For instance, the following formula will be true for C compilation, when
it supports stack allocation.

```bigloo
(cond-expand
  ((and C (config have-c99-stack-alloc #t)) ...)
  ...)
```

<span></span>

### register-srfi! ###

Registers both a compile- and eval- srfi symbol. This argument
`srfi-name` is a symbol. It registers `srfi-name` in the Bigloo
interpreter SRFI register. This function must only be used when
implementing a library.

### register-eval-srfi! ###
Registers a srfi symbol for the interpreter.
Calling `(register-eval-srfi! name)` makes `name` supported
by interpreted `cond-expand` forms. 

The code of that library must contain
one unique call to `register-eval-srfi!`. Let's suppose, for instance,
a `format` library. The implementation for that library must contain
an expression like:

```bigloo
(register-eval-srfi! 'format)
```

<span></span>

### register-compile-srfi! ###
Registers a srfi symbol for the compiler.

### unregister-srfi! ###
Unregisters a srfi symboll

### unregister-eval-srfi! ###
### unregister-compile-srfi! ###

### eval-srfi? ###
Returns `#t` if and only if `srfi` is a symbol registered with 
`register-eval-srfi!`.

### compile-srfi? ###
Returns `#t` if and only if `srfi` is a symbol registered with 
`register-compile-srfi!`.

> [!NOTE] The function `srfi-compile-list` returns the list of names
> defined for the compiler _at runtime_. During the execution of the
> compiled program, it will return the empty list. The list of compiler 
> srfi names is accessible at compile-time, for instance, during 
> macro-expansion.

### srfi-eval-list ###
Returns the list of all the srfi names registered for the interpreter.

### srfi-compile-list ###
Returns the list of all the srfi names registered for the compiler.

