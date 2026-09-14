<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/eval.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Eval                                                          -->
<!--==================================================================-->

,(implementation-path "../runtime/Eval/eval.scm")
,(implementation-path "../runtime/Eval/expand.scm")
,(example-path "../test/src/eval.bgl")

Eval
====

Bigloo includes an interpreter but it shows differences with compiled
code. The main differences are:

  * Type annotations are ignored in interpreted code.
  * No foreign objects can be handled by interpreter; 
  * The interpreter imposes a significant performance drop.

Compiled code and interpreted code can be mixed together. That
is, interpreted code is allowed to call compiled code and vice
versa. This connection can be use to circumvent the missing
features of the interpreter (see [Module Declaration](./module5.html)),
for a description of how to connect compiled and interpreted code).

By default the evaluator assumes that operators from the standard
library (e.g., `+`, `car`) are immutable. Hence, it optimizes
these operators's calls. 

Library Functions
-----------------

### eval ###

This form evaluates `exp`. The second argument is optional. It can be
the evaluation of one of these three function forms: 

  * `(scheme-report-environment 5)`
  * `(null-environment 5)`
  * `(interaction-environment)`

<span></span>

These three procedures have the definitions given in the 
[Scheme R5Rs standard](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS).


### expand ###
Returns the macro-expansion of `expr`.

### expand-once ###
Returns the macro-expansion of `expr` after _one_ macro-expansion.

Repl
----

### repl ###
This invokes the /read-eval-print/ loop. It reads all its commands
from the `current-input-port` and emits the results to the
`current-output-port`. It ends when it reads an end-of-file object. It
returns the value of the last evaluation.

Several `repl` can be embedded.

The `repl` function can be used to implement custom Bigloo interpreters.
For instance, one may write:

```bigloo
(module repl)
(repl)
```

When compiled, this will deliver an executable containing the sole
Bigloo interpreter. 

### quit ###
Exits from the currently running `repl`. If the current 
`repl` is the first one then this function ends the interpreter.

### get-repl-error-notifier ###
Get the procedure used in the REPLs to display errors or `#f` if none
defined.

### set-repl-error-notifier! ###
Sets the notifier, which is a procedure of one argument, the error or
exception that has been raised.

### get-prompter ###
Returns the current `repl` prompter.

### set-prompter! ###
Set the `repl` prompter, a procedure of one argument, which the
the nesting level of the current `repl`.

### set-repl-printer! ###

The argument `proc` has to be a procedure accepting one or two
arguments.  This function sets the `repl` display function. That is,
to display the result of its evaluations, `repl` invokes `proc` giving
it the evaluated expression as first argument and the current output
port as second argument. The function `set-repl-printer!`  returns the
former `repl` display function.

### native-repl-printer ###
Returns the default `repl` printer.

Loading Files
-------------

### load ###
Loads the Bigloo code contained in `filename`.  The file is searched in
the current directory and in all the directories mentioned in the
variable `*load-path*1.  The `load` procedure reads expressions and
definitions from the file, evaluating them sequentially. If the file
loaded is a module (i.e., if it begins with a regular module clause),
load behaves as module initialization. Otherwise, this function
returns the name of the loaded file.


### loadq ###
As `load`, it loads the Bigloo code contained in `filename` but
it does not printany intermediate evaluations.

### *load-path* ###
A list of search paths for the `load` functions.

Eval Command Line Options
-------------------------

Some Bigloo command line options control or configure or interpreted code.

  * `-i` Don't compile a module, interpret it!
  * `-export-all` Make all the bindings _defined_ by 
      the compiled module available from the interpreter.
  * `-export-export` Make all the bindings _exported_ by the 
      compiled module available from the interpreter.
  * `-export-mutable` Make all the bindings `exported` by the 
      compiled module mutable from outside the module. This option is
      _dangerous!_ Either all the modules composing the application 
      must be compiled with or without `-export-mutable`. It is impossible
      to mix `-export-mutable` enabled and disabled compilations.
