<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/core.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Core language                                                 -->
<!--==================================================================-->

,(implementation-path "../runtime/Eval/expdsrfi0.scm")
,(example-path "../test/src/core.bgl")


Core Language
=============

This chapter presents the Bigloo basics. It presents the elements
that compose the body of a [module](./module5.html).

Syntax
------

The syntax of Bigloo is that of Scheme (a parenthesis based one) with
four exceptions: type information, multi-line comments, extended strings
escape characters, and Java-like syntax for large integers. Type
information is supplied when identifiers are introduced (via
`lambda`, `let`, `define`, ...) and those identifiers
holding type information are referred to as typed identifiers.
They are defined by the following grammar:

```bnf
<ident> --> <r5rs-ident> | <typed-ident>
<typed-ident> --> <r5rs-ident>::<r5rs-ident>
<r5rs-ident> --> the standard Scheme identifiers
```

For details of the standard Scheme identifiers, see 
[Scheme R5RS](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS).

[Multi-lines comments](http://srfi.schemers.org/srfi-30) are defined as:

```bnf
<ident> --> <r5rs-ident> | <typed-ident>
<comment> --> ;<all subsequent characters up to a line break>
   | #| <comment-text> (<comment> <comment-text>)* |#
<comment-text> --> <character sequence not containing #| or |#>
```

A Bigloo string literal is defined by:

```bnf
<string> --> <bgl-string> | <r5rs-string>
<r5rs-string> a normal R5RS string or a Bigloo string
<bgl-string> #"<character>*"
<character> --> any character but the character \
  | \\ | \n | \t | \b | \r | \f | \v | \"
  | \x<hex><hex>
  | \ux<hex><hex><hex><hex>
  | \u<hex>
  | \u<hex><hex>
  | \u<hex><hex><hex>
  | \u<hex><hex><hex><hex>
<hex> --> an hexa-decimal digit
```

Large integers can be written as:

```bnf
<integer> --> <r5rs-integer> | <large-integer>
<large-integer> --> (+|-?)?<digit><digit>?(_<digit><digit><digit>)+
<comment-text> --> <character sequence not containing #| or |#>
```

That is, large integer constants can use the `_` character to separate
sequences of 3 digits. Examples:

```bigloo
1_000_102
89_223
```

Comments and whitespaces are the same as in [Scheme R5RS](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS).

```biglloo
;;; The FACT procedure computes the factorial
;;; of a non-negative integer.
(define fact
  (lambda (n)
    (if (= n 0)
        1 ;; Base case: return 1
        (* n (fact (- n 1))))))
```

In addition, Bigloo supports _s-expressions_ comments. These
are introduced with the `#; syntax:

```bigloo
;;; The FACT procedure computes the factorial
;;; of a non-negative integer.
(define fact
  (lambda (n)
    #;(if (< n 2) 1 (* #;n (fact (- n 1))))
    (if (= n 0)
        1
        (* n (fact (- n 1))))))
```


Expressions
-----------

Bigloo does not distinguish statements from expressions as most languages do.
In Bigloo all expressions produce a value.

Bigloo expressions are the same as in 
[Scheme R5RS](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS)
with some extensions. The Bigloo syntactic
keywords are:

```
->                      =>                    and 
and-let*                args-parse            assert
begin                   bind-exit             case
cond                    cond-expand           define
define-expander         define-generic        define-inline
define-macro            define-method         define-record-type
define-struct           define-syntax         delay
do                      duplicate             else
failure                 if                    instantiate
labels                  lalr-grammar          lambda
let                     let*                  let-syntax
letrec                  letrec*               letrec-syntax
match-case              match-lambda          module
multiple-value-bind     or                    pragma
quasiquote              quote                 receive
regular-grammar         regular-search        set! 
shrink!                 try                   unquote
unquote-splicing        unwind-protect        widen! 
with-access             with-handler
```

All other non atomic Bigloo forms are evaluated as function
calls or macro calls.

Variables, literals, and quote:

```bigloo
(define x 28)                          &rarr;
x                                      &rarr; 28
(quote a)                              &rarr; A
(quote #(a b c))                       &rarr; #(A B C)
(quote (+ 1 2))                        &rarr; (+ 1 2)
'a                                     &rarr; A
'#(a b c)                              &rarr; #(A B C)
'()                                    &rarr; ()
'(+ 1 2)                               &rarr; (+ 1 2)
'(quote a)                             &rarr; (QUOTE A)
'"abc"                                 &rarr; "abc"
"abc"                                  &rarr; "abc"
'145932                                &rarr; 145932
145932                                 &rarr; 145932
'#t                                    &rarr; #t
#t                                     &rarr; #t
```

### (operator arg ...) ###
<!-- [:operator@NoDef] -->

Operators are implemented via functions. As such, they can be applied
to values to compute a result but they can also be used as values, i.e.,
passed as argument to other functions, returned from another function call,
or stored into variables and data structures.

### (lambda args body) ###
<!-- [:lambda@NoDef] -->

Functions are defined with the keyword `lambda`. The syntax

```bnf
<DefineLambda> --> (define (<Ident> <Arguments>) <Expression>
```

is a shorthand for:

```bnf
<Define> --> (define <Ident> (lambda <Arguments> <Expression>))
```

<span></span>

### (if test consequence alternate) ###
<!-- [:if@NoDef] -->

The simple condiditional forms are implemente by the `if` construct.

### (cond clause clause ...) ###
<!-- [:cond@NoDef] -->
Cascades of condiditionals can be expressed using the `cond` form.
Bigloo considers `else` as a keyword. It thus ignores clauses
following an `else`-clause.

### (case expr clause ...) ###
<!-- [:case@NoDef] -->
When all the tests of a cascade of `if` compare the same value to a string,
a symbol, or a number, the compact form `case` can be used instead.

> [!NOTE] A `case` is equivalent to a cascade of `if`. They have
> the same semantics but `case` gives opportunities to the compiler for
> more aggresive optimizations.

### (and expr ...) ###
<!-- [:and@NoDef] -->
Logical _and_ .

### (and-let* bindings expr ...) ###
<!-- [:and-let*@NoDef] -->

### (or expr ...) ###
<!-- [:or@NoDef] -->
Logical _or_.


### (set! variable value) ###
<!-- [:set!@NoDef] -->
Assigns a new value to a declared variable.

> [!NOTE] the form `(set! (-> obj field) value)` is the assignment of
> object fields. The form is described in the [Object](./object.html) chapter.


### (let bindings body) ###
<!-- [:let@NoDef] -->
Bindings are of the form

```bnf
<binding> --> ( <Ident> <Expression> )
  | <Ident>
```

The first syntax binds a variable to an expression. The second form
binds it to the `#unspecfied` value. The form

```bigloo
(let (x) x)
```

is equivalent to:

```bigloo
(let ((x #unspecfied)) x)
```

The identifiers introduced by `let` construct are bound in the `body`
of the `let`. 

<span></span>

### (let ident bindings body) ###
<!-- [:let@NoDef] -->
Binds a local function definition.

### (let* bindings body) ###
<!-- [:letn@NoDef] -->
Similar to `let` but the fresh variables are bound in the remaining bindings.
A `let*` form is equivalent to the cascade of `let` forms.

### (letrec bindings body) ###
<!-- [:letrec@NoDef] -->
Binds mutually recursive local variables.

### (letrec* bindings body) ###
<!-- [:letrec*@NoDef] -->

Each binding has the form:

```bnf
<Bindings> --> ( (<Ident> <Expression>)+ )
```

Each &lt;Expression&gt; is an expression. Any variable must not appear more
than once in the `&lt;variable&gt;}s.

The &lt;variable&gt;s are bound to fresh locations, each &lt;variable&gt;
is assigned in left-to-right order to the result of evaluating the
corresponding &lt;expression&gt;, the `body` is evaluated in the resulting
environment, and the values of the last expression in `body` are
returned. Despite the left-to-right evaluation and assignment order,
each binding of a &lt;variable&gt; has the entire letrec* expression as its
region, making it possible to define mutually recursive procedures.

It must be possible to evaluate each &lt;expression&gt; without assigning or
referring to the value of the corresponding &lt;variable&gt; or the
&lt;variable&gt; of any of the bindings that follow it in
`bindings`. Another restriction is that the continuation of each
&lt;expression&gt; should not be invoked more than once. 

### (begin expression ...) ###
<!-- [:begin@NoDef] -->
Sequence of expression. It returns the evaluation value of the last
expression.

### quasiquote template ###
<!-- [:quasiquote@NoDef] -->
Syntactic form for creating lists and vectors. Similar to `quote` except
that a `quasiquote` evaluates un `unquote` and `unquote-splicing` expression
it contains.

### define variable expression ###
<!-- [:define@NoDef] -->
Defines a variable.

### define (variable args) expression) ###
<!-- [:definefun@NoDef] -->
Defines a function.


