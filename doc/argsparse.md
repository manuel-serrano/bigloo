<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/argsparse.md             -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Command Line Parsing                                          -->
<!--==================================================================-->

,(include "head.html")
,(implementation-path "../runtime/Eval/expdargs.scm")
,(example-path "../test/src/argsparse.bgl")


Command Line Parsing
====================

Bigloo supports command line argument parsing. That is, when an
application is spawn from an Unix shell, the `main` function is called
and its argument is bound to the list of the command line arguments,
see [Modules](./module5.html). The `args-parse` form may be used to
parse it.

### (args-parse args::pair-nil ...) ###
<!-- [:args-parse@NoDef] -->

The argument `args` is a list of strings. The syntax of the body is defined
by the &lt;rule&gt; is defined by the following grammar:

```bnf
<rule> --> ( section <string> )
  | ((<option> <help>) <s-expression>)
  | ((<option>) <s-expression>)
  | ((<flag> <var> <var> ...) <s-expression>)
  | ((<flag> <var> <var> ... <help>) <s-expression>)
  
<null-rule> --> ( () <s-expression> )

<else-rule> --> ( else <s-expression> )

<option> --> <flag>
  | <string><var>
  
<flag> --> <string>
  | ( <string>+ )
  
<var> --> an identifier leaded by the `?` character
<help> --> ( help <s-expression> )
  | ( help <string> <s-expression> )
```

Each elements of `args` are match against the &lt;rule&gt;s. If one
of these matches, `args-parse` proceeds as follows:


  * The matched argument elements of `args`} are removed from the list.
  * The &lt;s-expression&gt; associated to the matching rule
   is evaluated in an environment where the rule variables are bound. 
  * The argument parsing is resumed with the rest of `args`.

In addition to parsing the command line arguments, `args-parse` enables
help message printing. 


### (args-parse-usage) ###
<!-- [:args-parse-usage@NoDef] -->

This is a procedure of one argument, an boolean. The function `args-parse-usage`
constructs an help message from all the option described in a `args-parse`
form. It is only defined in the &lt;s-expression&gt;
of an `args-parse` form.

At last, if no rule matches an argument and if the `args-parse`
form contains an `else` rule, this is evaluated. In the
&lt;s-expression&gt; part of that rule, the pseudo-variable
`else` is bound to the first unmatched argument and the pseudo-variable
`rest` is bound to all the unmatched arguments.
