<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/boolean.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Traces                                                        -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/trace.scm")
,(example-path "../test/src/trace.bgl")

Traces
======

Bigloo provides a _trace_ facility which is intended for simple
debugging tasks. It is a replacement for user `display`s that
clutters the source code. Traces are enabled on compiled code when a
`-g` compiler option is specified and *not included* in the generated
code otherwise. As such, traces have no effect on production code.
Here is a typical example using it:

```bigloo
,(include "examples/c/trace.bgl")
```

When compiled with `-g` such as:

```shell
$ bigloo -g trace.bgl
```

It produces the following:

```shell
$ BIGLOOTRACE="foo loop liip" ./a.out
,(include "examples/c/trace-full.txt")
```

### (with-trace key::symbol label . body) ###
<!-- [:with-trace@NoDef] -->

Only when compiled in debug mode (see [compiler
options](./compiler.html)), it checks if the shell variable
`BIGLOOTRACE` contains `key`. If it does , it displays the `label` and
increases the margin side. It evaluates body, which will be the result
of the evaluation of the form. It then decreases the margin level.

When not compiled in debug mode or if `BIGLOOTRACE` does not contain
`key`, it only evaluates `body`.

The shell varibale `BIGLOOTRACE` is a list of whitespace separated
keyword. For instance: 

```shell
$ BIGLOOTRACE="loop liip" ./a.out
,(include "examples/c/trace-loop.txt")
```

<span></span>

### trace-item ###
When traces are actived, the function `trace-item` displays its arguments.

### trace-bold ###
Returns a bold-face string showing `s`.

### trace-color ###
The `color` argument is a positive integer. 
This function returns a string which is the representation of `args`
and that appears on the terminal in color `color`.

Colors can be enable or disabled using the @code{bigloo-trace-color}
parameter (see @ref{Parameters}).
@end deffn

### trace-margin ###
Returns the string used to fill margin.

### trace-margin-set! ###
Sets the string used to fill margin.

Usual applications should not use this feature. However, it may be
convenient to set the margin by hands in some context. For instance,
it can be used to distinguished threads in a multi-threaded
application such as:

```bigloo
(make-thread (lambda () 
                (trace-margin-set! (trace-color 1 "="))
                ...))
(make-thread (lambda () 
                (trace-margin-set! (trace-color 2 "="))
                ...))
```

<span></span>

### trace-port ###
Returns the output port used to display traces.

### trace-port-set! ###
Sets the output port used to display traces.

