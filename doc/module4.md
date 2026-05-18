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

Modules 4
=========

A modules is a compiler and interpreter entity. Modules have been
first designed for the compiler that compiles modules and then, links
them against libraries in order to produce executables. A module may
be split into several files but a file cannot contain more than
@emph{one} module. A module is made of a module clause that is a list
for which the @code{car} is the symbol @code{module} and followed by
any Bigloo expression (that is definitions or expressions). The module
clause names the module and defines the scope of the definitions. At
last, the module clause is also the place where foreign bindings are
defined and where classes are defined. Recent versions of Bigloo
(since 2.7b) fully supports modules from the interpreter.

> [!WARNING]
> Modules 4 are obsolete and should not be used for new code.
> Instead [modules 5](./module5.html) should be used. Modules 4
> are and will be maintained for backward compatibility with old code.

> [!WARNING] 
> Contrary to the other chapters, the examples used in this
> chapter are not directly extracted from test programs and should
> then be considered carefully and verified.

Program Structure
-----------------

A Bigloo program is composed of one or more Bigloo modules where a module is
defined by the following grammar:

```bnf
<module> --> <module-declaration> <module-body>
<module-declaration> --> the module declaration
<module-body> --> the module body
```

A module is not related to a specific file and can be spread over
several files if that is convenient. In particular, there is no
relationship between module names and file names. The module
declaration must be the first expression in the first of the files
containing the module; other expressions form the body of the
module. The module body contains global variables, function
definitions and _top level_ expressions.

Module 4 Declaration
--------------------

### (module name clause ...) ###

This form defines a module and must be the first in the file.  The
argument `name` is a symbol naming the module. If the same
module name is used more than once, Bigloo signals an error. The
runtime library is composed of modules that are read when a user module
is compiled and hence, if a user module has the same name as one of the
library modules, an error is signaled.

A simple module can be:

```bigloo
;; zz.scm
(module foo)

(display "this is a module")
```

The first line here is the complete module definition, the last line is
the complete module body and together they form a complete Bigloo program. If
these lines were stored in file `zz.scm`, invoking `bigloo zz.scm`
would create the executable `a.out` which, when obeyed, would
display `this is a module` on the terminal.

> [!NOTE]
> Some special identifiers are reserved and can't be used to name modules.
> If such an identifier is used, the compiler will trigger an error.
> The list of reserved identifiers may be enlarged for next release. For 
> the current release that list is made of: `eval`, `foreign` and `t`.

### (main name) ###

This clause defines the entry point for a stand alone application to be
procedure `name` of arity one. Bigloo invokes this procedure at the
beginning of execution providing the list, composed of the shell command
line arguments, as its single argument.

```bigloo
(module foo
   (main start))

(define (start argv)
   (display argv)
   (newline))
```

Then if this program is compiled into `foo` and invoked using the
command `foo -t bar`, the list which is the argument for the main 
procedure `start` would be `("foo" "-t" "bar")`. 

The special form `args-parse` helps main function argument parsing 

### (include file-name ...) ###

This is a list of `file-name`s to be included in the source file. Include
files are not modules and may have a special syntax. Thus, besides containing
Bigloo expressions, they can contain import and include clauses, which must
be written in a single list whose first element is the keyword 
`directives`. Includes files can be used to include implementation-neutral
Scheme expressions and definitions in a Bigloo module. Here is an example of 
an include file.

```bigloo
;; foo.sch
(define-struct point x y)
```

and the module that includes the `foo.sch` file:

```bigloo
;; foo.scm
(module foo
   (include "foo.sch"))

(print (point 1 2))
```

Include files, may contain module information. This is the role of the
include `directives` clause here illustrated with the `bar.sch`,
example:

```bigloo
;; bar.sch}
;; the directives
(directives (include "foobar.sch")
            (import  hux))

;; expressions
(define (gee x) (print x))
```

### (import import ...) ###

An `import` is a list of the form:

```bnf
<import> --> <iclause>+
<iclause> --> (<bind-name>+ <module-name> <file-name> ...)
  | (<bind-name>+  <module-name>)
  | <module-name>
  | (<module-name> <file-name>+)
<bind-name> --> <r5rs-ident>
  | <alias-name>
<alias-name> --> (<r5rs-ident> <r5rs-ident>)
<module-name> --> <r5rs-ident>
<file-name> --> <string>
```

The first alternative in `iclause` imports the variable named 
`bind-name` which is defined in the module `module-name`, 
located in the files `file-name` .... The second does the same 
but without specifying the name of the file where the module is located.  
The third and the fourth form import all the exported variables of the module
`module-name`.

> [!NOTE]
> The need for specifying in which files modules are located comes
> from the fact that there is no automatic mapping between module names
> and files names. Such a mapping can be defined in a "module access file"
> or in the import clause itself, as
> in the first and fourth alternatives in `iclause` above.

```bigloo
(module foo
   (import 
      ;; import all bar exported bindings:
      bar
      ;; import the hux binding exported by
      ;; the module hux
      (hux hux)       
      ;; import the fun1, fun2 and fun3 bindings exported by
      ;; the module mode
      (fun1 fun2 fun3 mod)       
      ;; import the fun4 bindings that will be known in this module
      ;; under the alias name f
      ((f fun4) mod)
      ;; import all gee bindings. the gee} module
      ;; is located in a file called gee.scm:
      (gee "gee.scm"))
```

### (use use ...) ###

The `use` clause has the same meaning as `import` except that modules
which are `used` are not initialized.
Used modules are read before imported modules.

### (with with ...) ###

This clause specifies a list of modules which have to be
initialized at runtime and is used to force the initialization
of modules which are never imported but which are required by an 
application.

### (export export ...) ###

In order to make a module's global bindings available to other modules, they
have to be _exported_. Export clauses are in charge of this task and an
`export` is a list of the form:

```bnf
<export> --> <eclause>+
<eclause> --> <varexport>
  | <funexport>
  | <macroexport>
  | <classexport>
<varexport> --> <ident>
<funexport> --> | (<ident>+)
  | (inline <ident> <ident>*)
  | (generic <ident> <ident>*)
<classexport> --> <class>
<macroexport> --> (macro <ident> <ident>*)
  | (expander <ident>)
  | (syntax <ident>)
```

> [!NOTE] 
> Only bindings defined in module `m` can be _exported_
> by `m` (i.e. bindings _imported_ by `m` cannot be 
> _exported_ by it).

The first form of `varexport` allows the variable `ident` be
exported.  Exported variables are mutable. That is, modules importing
a variable can change its value.

The form `funexport` exports functions. An exported function is
read-only. No module can modify its value. The prototype of exported
functions must be explicitly specified.

Type information, specified in any `ident` in an export clause, is
used by Bigloo. Where no type information is given, a default generic
type named `obj` is used.

> [!NOTE]
> The last formal argument of a multiple arity function can
> not be typed because this argument is bound to be a `pair`
> or `null`. This union cannot be denoted by any type.

Inline functions prototypes are prefixed by the `inline`
keyword. Pay attention that in order to export an inline function, all
the variables used in that function body must be exported too.  It is
an error to export a non-inline function using the `inline`
keyword. Example:

```bigloo
(module mod-exp
   (export 
      ;; export the bar mutable variable
      bar
      ;; export the hux function. this
      ;; function takes exactly two arguments
      (hux x y)       
      ;; export the inline function gee
      ;; that takes at least one argument.
      (inline gee x . z)))
```

Generic functions are exported by using the `generic` keyword.
Note that methods are not exported. Only the generic function they are
attached to are. It is an error to export a non-generic function using
the `inline` keyword.  Example:

```bigloo
(module mod-exp-object
   (export 
      ;; define two classes an export them
      (class point2d x y)
      (class point3d::point2d z)
      ;; export a generic function
      (generic show ::point2d ::output-port)))
```

It is an error to export an inline or a generic function without using
the proper keyword. 

The last form `macroexport` enables macro and expanders to be
exported. The prototype of the macro should be specified on the export
clause. Example:

```bigloo
(module mod-exp-macros
   (export 
      ;; exports the macro "add"
      (macro add x y)
      ;; exports the expander "+"
      (expander +)))

(define-macro (add x y)
   (if (and (number? x) (number? y))
       (+ x y)
       `(+ ,x ,y)))

(define-expander +
   (lambda (x e)
      (match-case x
         ((+ ?n ?m) (+ n m))
         (else (map (lambda (z e) (e z e)) x)))))
```

### (static static ...) ###

A `static` clause has exactly the same syntax as an export
clause. However, bindings declared static are local to the module. Since
the default scope of all bindings is static, `static` module clauses
are useful only for program documentation.

### (from from ...) ###

A `from` clauses has the syntax of `import`
clauses. The allow the re-exportation of imported bindings. That is, any 
module can export any bindings imported via a `from` clause.

As an example, suppose we have module `bar`:

```bigloo
(module bar
   (export (fun)))

(define (fun) "bar")
```

Now, suppose we have a module `foo` that imports `bar`, by the
means of a `from` clause. Module `foo` is able to re-export the
`bar` binding of module `bar`:

```bigloo
(module foo
   (from (fun bar "bar.scm")))
```

A third module, let's name it `gee`, importing module `foo`, can see
the binding for function `bar`:

```bigloo
(module gee
   (import (foo "foo.scm")))

(print (fun))
```

This feature is very useful when compiling modules exporting functions
with type annotations. In particular, one may write:

```bigloo
(module foo
  (export (class c1 x)))
```

Then,

```bigloo
(module bar
  (import foo)
  (from foo)
  (export (fun::c1)))

(define (fun)
   (instantiate::c1 (x 10)))
```

And,

```bigloo
(module gee
   (import bar)
   (main main))

(define (main x)
   (let ((o (fun)))
      (print o)
      (print (c1? o))))
```

### (load load ...) ###

A `load` is a list of the form:

```bnf
<load> --> <lclause>+
<lclause> --> (<module-name> <file-name>)
  | <module-name>
```

This clause forces Bigloo to load the module specified in the `lclause`
in the environment used by the macro expansion mechanism. This means that
the user's macros can use all the bindings of all the @code{load}ed modules
but the `load`ed bindings remains unknown to the compiler.

If the module `foo` is defined by:

```bigloo
(module foo
   (export (foo x)))

(define (foo x)
   `(cons ,x ,x))
```

then,

```bigloo
(module gee
   (load (foo "foo.scm")))

(define-macro (gee x)
   `(cons ,(-fx x 1) ,(foo x)))

(gee 5)   
  &rarr; (cons 4 (cons 5 5))
  &rarr; (4 5 . 5)
```

### (eval eval...) ###

This form allows interactions between compiled code and interpreted
code. Each `eval` has the following syntax:

```bnf
<eval> --> (export-all)
  | (export-module)
  | (export-exports)
  | (export <bind-name>)
  | (export (@ <bind-name> <module-name>))
  | (import <bind-name>)
  | (class <bind-name>)
  | (library lib1 ...)
```

The first clause, `(export-all)`, exports all the variables bound
in the module (i.e., the variables defined in the module and the
imported variables). The second clause, `(export-module)`, exports
the module to eval to so that it can be imported by other evaluated
modules; the third exports all the exports (i.e. the ones present
inside an `export` clause) variables to the interpreter; the fourth
and fifth clause each export one variable to the interpreter. The last
clause imports a variable from the interpreter and all such imported
variables are immutable (i.e. they cannot be the first argument of a
`set!`  expression with the compiled code). Variables that are
exported to the evaluators _must_ be exported.  If a variable is
exported to the evaluators but not exported within an `export`
clause, the compiler will produce an error message. The `library`
clause makes the variables and functions of a library accessible from
the interpreter.

```bigloo
(module foo
   (export (fib x))
   (eval (export fib)
         (import bar)))

(define (fib x) ...)
(print bar)
```

The clause `(class <bind-name>)` exports a class definition to
the interpreter. This makes the class constructor, the class predicate
and the slots access functions available from the interpreter. The
form 
   `(instantiate::class ...)` 
and 
   `(with-access::class ...)` 
are also available from the interpreter.

### (extern extern ...) ###

Extern (aka foreign) clauses will be explained in the foreign interface.

### (java java ...) ###

Java clauses will be explained in the Java interface

### (option option ...) ###

This clause enables variables which affect compilation to be set from inside
a module and since the expressions, @var{option} ..., are evaluated
_when compiling_, no code is compiled for them.  They are allowed to
make side effects and to change the values of the global variables which
describe how the compiler must compile. Usually they allow the control
variables, which are described when Bigloo is invoked with the `-help2`
option, to be set as in the following example:

```bigloo
(module examplar
   (option (set! *debug* 3)
           (set! *verbose* 2)))

(print 'dummy)
```

Whatever arguments are passed on the command line, Bigloo will compile this
module in both verbose mode and debug mode.
@end deffn

### (library library ...) ###

This clause enables libraries when compiling and linking Bigloo
modules. The expressions `library` ... are symbols naming the
libraries to be used.

Here is an example of a module declaration which makes use of a library
named `format`:

```bigloo
(module test
   (library format)
   (main    test-format)
   (import  (test2 "test2.scm")))
```

Using a library does not automatically binds its variables and functions
to the interpreter. In order to make these available to the interpreter
an explicit use of an eval `library` clause must be used.
@end deffn

### (type type ...) ###
This forms is used to define builtin Bigloo types. It is not recommended
to use it in user programs. So, it is left undocumented.


Module 4 initialization
-----------------------

_Initializing_ a module means evaluating, at runtime, its
top level forms (global bindings are top level forms).

When a module, `module1`, imports a module, `module2`,
`module2` is initialized before `module1`. Modules are
initialized only once, nothing being done if a module already met during 
initialization is met again. Library modules are initialized before user
modules and imported modules are initialized in the same order as they
appear in import clauses.

Here is a first example with two modules. First the module `foo`:

```bigloo
;; module foo
(module foo
   (main main)
   (import (bar "bar.scm")))

(define (main argv)
   (print "argv: " argv))
   
(print "foo")
```

Then the module `bar`:

```bigloo
;; module bar
(module bar)

(print "bar")
```

These can be compiled into the executable `a.out` with:

```shell
$ bigloo -c foo.scm
$ bigloo -c bar.scm
$ bigloo foo.o bar.o
```

Execution of `a.out` produces:

```shell
$ a.out
bar
foo
argv: (a.out)
```

The explanation is:

  1. module `foo` contains the program entry point so this is where
  initialization begins.
  2. because `foo` imports module `bar`, `bar` must be
  initialized @emph{before} `foo`. This explains why the word `bar`
  is printed before anything else.
  3. module initialization for `foo` is completed before `main`
  is called. This explains why word `foo` is printed before `main`
  is entered.

Let's consider another example with 3 modules:

```bigloo
;; module1
(module module1
   (main main)
   (import (module2 "module2.scm")))

(define (main argv)
   (print "argv: " argv))

(print "module1")
```

The second module:

```bigloo
;; module2
(module module2
   (import (module3 "module3.scm")))

(print "module2")
```

The third module:

```bigloo
;; module3
(module module3
   (import (module1 "module1.scm")))
 
(print "module3")
```

Compile with:

```shell
$ bigloo module1.scm -c
$ bigloo module2.scm -c
$ bigloo module3.scm -c
$ bigloo module1.o module2.o module3.o
```

Execution produces:

```shell
$ a.out
module3
module2
module1
argv: (a.out)
```

The order of module initialization can be explicitly specified using `with`
and `use` clauses.
   
Qualified notation
------------------

Global variables can be referenced using implicit notation or
using _qualified_ notation. Implicit notation is used when
variables are referenced just by their name whereas qualified notation
is used when variables are referenced by their name and 
the name of the module which defines them. Qualified notation has
the following syntax:

```bnf
<qualified-name> --> (@ <bind-name> <module-name>)
```

This is useful when several imported modules export a
variable with the same name. Using qualified notations instead of
short notation only affects compilation.

When several variables are defined under the same identifier, the
compiler uses the two following rules in order to decide which
variable is selected by an implicit reference: 1) the variable defined
in a module has a higher precedence than all imported variables, 2)
imported variables have a higher precedence than library variables.

Inline procedures
-----------------

Bigloo allows procedures called _inline_ and which differ from
normal ones only in the type of code planted.  An inline procedure is a
first class object which can be manipulated in the same way as any other
procedure but when Bigloo sees a reference to one, rather than
generating a C function call to the function, the body of the inline
procedure is open-coded.  The definition of an inline is given in the
following way:

### (define-inline (name args ...) body) ###

Apart from the initial word, this form has the same syntax as that used by
`define` for procedures. Inline procedures are exportable which means
that the compiler scans imported files to find the bodies of all inline
procedures. Here is a small example of a module which exports an inline and
a module which imports it.

```bigloo
(module exporter
        (export (inline make-list . objs)))

(define-inline (make-list . objs) objs)
```

The importer module: 

```bigloo
(module importer
        (import exporter))

(print (make-list 1 2 3 4 5))
```

Because of the open-coding of the exporter procedure, the above print
statement is equivalent to:

```bigloo
(print (let ((objs (list 1 2 3 4 5)))
          objs))
```

Any procedure can be an inline. Also any exported procedure can be an inline
provided all global variables and functions it uses are also exported.

> [!NOTE]
> Bigloo can decide to inline procedures declared with `define`
> but this can be achieved only with local procedures whereas procedures
> declared with the `define-inline` form are open-coded even through
> module importation.

> [!NOTE]
> Procedures declared _inline_ are macro expanded with 
> the macro defined in the module where they are invoked. That is, if 
> module `module1` declares an inline procedure `p` and module 
> `module2` imports it, `p` may have two different macro-expansions: 
> one for `module1` and one for `module2`.

Module access file
------------------

> [!NOTE]
> The relationship between module 5 and module 4 is one of the main
> difference between them. Modules 5 are referenced via path names, while
> modules 4 are referenced via symbol names, unrelated to path names.
> This section **only** applies to module 4.

Bigloo's module 4 are different from languages such as C where a
module is defined by a file. For Bigloo, the module 4 name is not
necessarily the name of the file where the text of the module is
written and modules can even be split across several files.

Since modules are defined independently of files, it is necessary to make a
link between a module and its files and there are two ways of doing this.
Choosing an import clause where the file-names are specified or creating a
"module access file". Such a file must contain only one @var{list}, each
element of the list being of the form:

```lisp
(module-name "file-name" ... "file-name")
```

Use the `-afile file` option to specify the "module access file" when 
compiling. By default Bigloo checks if a file named `.afile`
exists. If it exists it is loaded as a module access file.

> [!NOTE] 
> The Bigloo distribution contains a tool, `bglafile`,
> that can automatically build a "module access file".


Read path
---------

Imported, included or loaded files are sought first in the current
directory and then in the directories, sequentially from start to end,
of the list in the `*load-path*` This variable, initially set to the
empty list, can be reset by the `-I` option of the compiler.

