() <!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/boolean.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Lexers and Parsers                                            -->
<!--==================================================================-->

,(implementation-path "../runtime/Ieee/input.scm")
,(example-path "../test/src/rgc.bgl")
,(example-path "../test/src/lalr.bgl")


Lexers and Parsers
==================

Programming languages have poor reading libraries since the lexical
information that can be specified is directly tied to the structure of
the language. For example, in C it's hard to read a rational number
because there is no type rational.  Programs have been written to
circumvent this problem: Lex, for example, is one of them,
Yacc is another one. With Bigloo these facilities are built-in
and supported by two forms `regular-grammar` and `lalr-grammar`.

Regular Grammar
---------------

### read/rp ###

This function proceeds to the lexical analysis of the reads character
from `port`.

The first argument is a regular grammar (also known as regular
analyser) and the second a port. This way of reading is almost
the same as the Lex's one. The reader tries to match the longest
input, from the stream pointed to by `input-port`, with one of
several regular expressions contained in `regular-grammar`. If
many rules match, the reader takes the first one defined in the
grammar. When the regular rule has been found the corresponding Scheme
expression is evaluated.

### (regular-grammar::procedure bindings rules ...) ###
<!-- [:regular-grammar@NoDef] -->

Creates a _regular grammar_. The syntax of `bindings` and `rules` are:

```bnf
<binding>  --> (<variable> <re>)
  | <option>
  
<option> --> <variable>

<rule> --> <define>
  | (<cre> <s-expression> <s-expression> ... )
  | ( else <s-expression> <s-expression> ... )
  
<define> --> ( define <s-expression> )

<cre> --> <re>
  | ( context <symbol> <re> )
  | ( when <s-expr> <re> )
  | ( bol <re> )
  | ( eol <re> )
  | ( bof <re> )
  | ( eof <re> )
  
<re> --> <variable>
  | <char>
  | <string>
  | ( : <re> ... )
  | ( or <re> ... )
  | ( * <re> )
  | ( + <re> )
  | ( ? <re> )
  | ( = <integer> <re> )
  | ( >= <integer> <re> )
  | ( ** <integer> <integer> <re> )
  | ( ... <integer><re> )
  | ( uncase <re> )
  | ( in <cset> ... )
  | ( out <cset> ... )
  | ( and <cset> <cset> )
  | ( but <cset> <cset> )
  | ( posix <string> )
<variable> --> <symbol>
<cset> --> <string>
  | <char>
  | ( <string> )
  | ( <char> <char> )
```

Here is a description of the construction that can be used to build
&lt;re&gt;.

  * `( &lt;symbol&gt; &lt;re&gt; )`
Protects an expression. A `protected`
expression matches (or accepts) a word only if the grammar has been set to
the corresponding context.

  * ` ( when &lt;s-exprgt; &lt;re&gt; ) `
  Protects an expression. A `protected`
  expression matches (or accepts) a word only if the evaluation of 
  `&lt;s-expr&gt;` is true. 

  Example: ,(test rgc-when)

  * `( bol &lt;re&gt; )`
  Matches `&lt;re&gt;` at the beginning of line.

  Example: ,(test rgc-bol)

  * `( eol &lt;re&gt; )`
  Matches `&lt;re&gt;` at the end of line.

  * `( bof &lt;re&gt; )`
  Matches `&lt;re&gt;` at the beginning of file.

  * `( eof &lt;re&gt; )
  Matches `&lt;re&gt;` at the end of file.

  * `&lt;variable&gt;`
  This is the name of a variable bound by a &lt;binding&gt; construction. 
  In addition to user defined variables. Here is the list of predefined
  variables:

     * `all`: `(out #\Newline)`
     * `lower`: `(in ("az"))`
     * `upper`: `(in ("AZ"))`
     * `alpha`: `(or lower upper)`
     * `digit`: `(in ("09"))`
     * `xdigit`: `(uncase (in ("af09")))`
     * `alnum`: `(uncase (in ("az09")))`
     * `punct`: `(in ".,;!?")`
     * `blank`: `(in #" \t\n")`
     * `space`: `#\Space`

It is a error to reference a variable that it is not bound by a
&lt;binding&gt;.  Defining a variable that already exists is
acceptable and causes the former variable definition to be
erased. Here is an example of a grammar that binds two variables, one
called _ident_ and one called _number_. These two variables
are used within the grammar to match identifiers and numbers.

Example: ,(test rgc-var)

  * `&lt;char&gt;`
  The regular language described by one unique character. Here is an example of
  a grammar that accepts either the character `#\a` or the character
  `#\b`: ,(test rgc-char)

  * `&lt;string&gt;`
   This simple form of regular expression denotes the language represented
   by the string. For instance the regular expression `"Bigloo"` matches
   only the string composed of `B`, `i`, `g`, `l`, `o`, `o`. The regular 
   expression `".*["` matches the string `.`, `*`, and `[`.

  * `( : &lt;re&gt; ... )`
  This form constructs sequence of regular expression. That is a form
  `&lt;re1&gt; &lt;re2&gt; ... &lt;ren&gt;` matches the language construction 
  by concatenation of the language described by `&lt;re1&gt;`, `&lt;re2&gt;`, 
  `&lt;ren&gt;`. Thus, `(: "x" all "y")` matches all words of three
  letters, started by character the `#\x` and ended with the character
  `#\y`.

  * `( or &lt;re&gt; ... )`
  This construction denotes conditions. The language described by
  `(or re1 re2)` accepts words accepted by either `re1` or `re2`.
 
  * `( * &lt;re&gt; )`
   This is the Kleene operator, the language described by `(* &lt;re&gt;)` is
   the language containing, 0 or more occurrences of `&lt;re&gt;`. Thus, 
   the language described by `(* "abc")` accepts the empty word and
   any word composed by a repetition of the `abc` (`abc`,
   `abcabc`, `abcabcabc`, ...).

  * `( + &lt;re&gt; )`
  This expression described non empty repetitions. The form `(+ re)` is
  equivalent to `(: re (* re))`. Thus, `(+ "abc")` matches the
  words `abc`, `abcabc`, etc.

  * `( ? &lt;re&gt; )`
  This expression described one or zero occurrence. Thus, 
  `(? "abc")` matches the empty word or the words `abc`.

  * `( = &lt;integer&gt; &lt;re&gt; )`
  This expression described a fix number of repetitions. The form
  `(= num re)` is equivalent to `(: re re ... re)`. Thus,
  the expression `(= 3 "abc")` matches the only word `abcabcabc`.
  In order to avoid code size explosion when compiling, `&lt;integer&gt;` 
  must be smaller than an arbitrary constant. In the current version that 
  value is `81`.

  * `( &gt;= &lt;integer&gt; &lt;re&gt; )`
  The language described by the expression `(&gt;= int re)` accepts word
  that are, at least, `int` repetitions of `re`. For instance,
  `(&gt;= 10 #\a)`, accepts words compound of, at least, 10 times the
  character `#\a`. In order to avoid code size explosion when compiling, 
  `&lt;integer&gt;` must be smaller than an arbitrary constant. In the current 
  version that value is `81`. Example: ,(test rgc>=)
 
  * `( ** &lt;integer&gt; &lt;integer&gt; &lt;re&gt; )`
  The language described by the expression `(** min max re)` accepts
  word that are repetitions of `re`; the number of repetition is in
  the range `min`, `max`. For instance, `(** 10 20 #\a)`.
  In order to avoid code size explosion when compiling, 
  `&lt;integer&gt;` must be smaller than an arbitrary constant. In the current 
  version that value is `81`. Example ,(test rgc-**)

  * `( ... &lt;integer&gt; &lt;re&gt; )`
  The subexpression `&lt;re&gt;` has to be a sequence
  of characters. Sequences are build by the operator `:` or by string
  literals. The language described by `(... int re)`, denotes, the
  first letter of `re`, or the two first letters of `re`, or the
  three first letters of `re` or the `int` first letters of
  `re`. Thus, `(... 3 "begin")` is equivalent to 
  `(or "b" "be" "beg")`. Example ,(test rgc-...)

  * `( uncase &lt;re&gt; )`
  The subexpression `&lt;re&gt;` has to be a sequence
  construction. The language described by `(uncase re)` is the
  same as `re` where letters may be upper case or lower case. For
  instance, `(uncase "begin")`, accepts the words `"begin"`,
  `"beGin"`, `"BEGIN"`, `"BegiN"`, etc.

  * `( in &lt;cset&gt; ... )`
  Denotes union of characters. Characters may be described individually
  such as in `(in #\a #\b #\c #\d)`. They may be described by
  strings. The expression `(in "abcd")` is equivalent to `(in
  #\a #\b #\c #\d)`.  Characters may also be described using a range
  notation that is a list of two characters. The expression `(in (#\a
  #\d))` is equivalent to `(in #\a #\b #\c #\d)`. The Ranges may be
  expresses using lists of string. The expression `(in ("ad"))`
  is equivalent to `(in #\a #\b #\c #\d)`.

  * `( out &lt;cset&gt; ... )`
  The language described by `(out cset ...)` is opposite to
  the one described by `(in cset ...)`. For instance, 
  `(out ("azAZ") (#\0 #\9))` accepts all words of one character
  that are neither letters nor digits. One should not that if the character
  numbered zero may be used inside regular grammar, the `out` 
  construction never matches it. Thus to write a rule that, for instances,
  matches every character but `#\Newline` including the character
  zero, one should write: `(or (out #\Newline) #a000)`.

  * `( and &lt;cset&gt; &lt;cset&gt; )`
  The language described by `(and cset1 cset2)` accepts words 
  made of characters that are in both `cset1` and `cset2`.  
  Example: ,(test rgc-and)

  * `( but &lt;cset&gt; &lt;cset&gt; )`
  The language described by `(but cset1 cset2)` accepts words 
  made of characters of `cset1` that are not member of `cset2`. 
  Example: ,(test rgc-but)
 
  * `( posix &lt;string&gt; )`
  The expression `(posix string)` allows one to use Posix string
  notation for regular expressions. Example: ,(test rgc-posix).
  

### (string-case string rule ...) ###
<!-- [:string-case@NoDef] -->

This form dispatches on strings. it opens an input on `string`
a read into it according to the regular grammar defined by the
`binding` and `rule`.

Regular Grammar Semantic Rules
------------------------------

The semantics actions, i.e., the right-hand-side of rules are regular
Bigloo expressions. These expressions appear in an environment where
some "extra procedures" that are described below.

> [!NOTE] The functions described in this section are only available
> in right-hand-side of `regular-grammar` rules.

### (the-port::input-port) ###
<!-- [:the-port@NoDef] -->
Returns input port that is being used by the regular grammar.

### (the-length::long) ###
<!-- [:the-length@NoDef] -->
Gives the length of the biggest matching string.

### (the-string::bstring) ###
<!-- [:the-string@NoDef] -->
Gives a copy of the last matching string. The function `the-string`
returns a fresh copy of the matching each time it is called.

### (the-substring::bstring start::long end::long) ###
<!-- [:the-substring@NoDef] -->
Retuns a copy of a substring of the last matching string. If the `len`
is negative, it is subtracted to the whole match length.
Here is an example of a rule extracting a part of a match:

### (the-character::char) ####
<!-- [:the-character@NoDef] -->
Returns the first character of a match.

### (the-byte::long) ###
<!-- [:the-byte@NoDef] -->
Returns the first byte of a match.

### (the-byte-ref::long n::long) ###
<!-- [:the-byte-ref@NoDef] -->
Returns the `n`-th bytes of the matching string.

### (the-symbol::symbol) ###
<!-- [:the-symbol@NoDef] -->
Converts the last matching string into a symbol. 

### (the-downcase-symbol::symbol) ###
<!-- [:the-downcase-symbol@NoDef] -->
Converts the last matching string into a _downcased_ symbol. 

### (the-upcase-symbol::symbol) ###
<!-- [:the-upcase-symbol@NoDef] -->
Converts the last matching string into a _upcased_ symbol. 

### (the-subsymbol::symbol start::long end::long) ###
<!-- [:the-subsymbol@NoDef] -->
The function `the-subsymbol` obeys the same rules as `the-substring`.

### (the-keyword::keyword) ###
<!-- [:the-keyword@NoDef] -->
Converts the last matching string into a keyword. This function removes
the first character of the match, it it is the character \#:. Otherwise
it removes the last character of the match, whatever character it is.

### (the-downcase-keyword::keyword) ###
<!-- [:the-downcase-keyword@NoDef] -->
As `the-keyword` but downcase the match first.

### (the-upcase-keyword::kewword) ###
<!-- [:the-upcase-keyword@NoDef] -->
As `the-keyword` but upcase the match first.

### (the-fixnum::long) ###
<!-- [:the-fixnum@NoDef] -->
Converts the last matching string to fixnum.

### (the-flonum::double) ###
<!-- [:the-flonum@NoDef] -->
The conversion of the last matching string to flonum.
 
### (the-failure) ###
<!-- [:the-failure@NoDef] -->
Returns the first char that the grammar cannot match or the end of file
object.

### (ignore) ###
<!-- [:the-failure@NoDef] -->
Ignores the parsing, keep reading. It's better to use `(ignore)`
rather than an expression like `(read/rp @var{grammar} @var{port})`
in semantics actions since the `(ignore)` call will be done in a
tail recursive way.

### (rgc-context . context) ###
<!-- [:rgc-context@NoDef] -->
If no `context` is provide, this procedure reset the reader context
state. That is the reader is in no context. With one argument, `context`
set the reader in the context `context`. For instance,

> [!NOTE] RGC contexts are preserved across different uses of `read/rp`.

### (the-context::symbol) ###
<!-- [:the-context@NoDef] -->
Returns the value of the current Rgc context.


Lalr Parsing
------------

Regular grammar generators, like Lex, are often coupled with tools,
such as Yacc and Bison, that can generate parsers for more powerful
languages, namely (a subset of) context-free languages. These tools
take as input a description of the language to be recognized and
generate a parser for that language, written in some other language
(for example, Yacc and Bison generate parsers written in C). The user
must always be aware of the generated parser and that is a nuisance.
Bigloo provides such a tool that overcomes this annoyance. It
generates parsers for the class of Lalr(1) grammars in a more
opaque way.

### (lalr-grammar::procedure term-def::pair-nil rules::pair) ###
<!-- [:lalr-grammar@NoDef] -->
This form defines an lalr grammar suitable for parsing. The arguments
`term-def` is a list of terminal elements of the grammar.  Terminals can
grouped together to form precedence groups by including the related symbols
in a sub-lists of the `term-def` list.  Each precedence group must start
with one of the keywords `left:`, `right:` or `none:`-- this
indicates the associativity of the terminal symbol.  Here is a sample
`term-def` which declares eight terminals:

```bigloo
(terminal-1 terminal-2
 (left: terminal-3 terminal-4)
 terminal-5
 (right: terminal-6)
 (none: terminal-7)
 terminal-8)
```

In this case, `terminal-3` and `terminal-4` both have the same
precedence, which is greater than the precedence assigned to
`terminal-6`. No precedence was assigned to symbols `terminal-1`,
`terminal-2`, `terminal-5` or `terminal-8`.

Each `non-term-def` is a list whose first element is the
non-terminal being defined, i.e. a symbol. The remaining elements are
the production rules associated with this non-terminal. Each rule is a
list whose first element is the rule itself (a list of symbols) and
the other elements are the semantic actions associated with that
particular rule.  

For example, consider the following grammar:

```bnf
<E> --> <E> + id { <E>.val := <E1>.val + id.val }
  | id <E>.val := id.val
```

With Bigloo, it would be written:

```bigloo
(lalr-grammar
  (plus id)
  (e
   ((e plus id) (+ e id))
   ((id) id)))
```

The semantic value of a symbol in a rule can be accessed by simply
using the name of the symbol in the semantic action associated with
the rule. Because a rule can contain multiple occurrences of the same
symbol, Bigloo provides a way to access these occurrences
separately. To do so, the name of each occurrence must be suffixed by
`@``var` where `var` is the name of a variable that will be
bound to the semantic value of the occurrence. For example, if the
rule is

```bnf
<ifstmt> --> if <E> then <Stmt> else <Stmt>
```

then, in Bigloo, it would look like

```bigloo
(if-stmt
 ((if e then stmt@conseq else stmt@altern)
  (if (eval e) 
      (eval conseq) 
      (eval altern))))
```

@c -- Grammar definition --------------------------------------------- @c
@node Precedence and Associativity, The Parsing Function, Grammar Definition, Lalr Parsing
@section Precedence and associativity
@cindex Lalr precedence and associativity

The bigloo lalr(1) parser generator supports operator precedence and
associativity.  The method for specifying the precedence for terminal symbols
is described in @ref{Grammar Definition}.  Precedence is assigned to each
non-terminal production from the precedence of the last terminal symbol 
appearing in that production.

Typically, when the parser generator encounters a shift/reduce conflict, it
produces a warning message, then chooses to reduce.  When a parser generator
has precedence and associativity information, it can make a much more
sophisticated decision.

Let's use this simple calculator grammar as an example:
@smalllisp
(lalr-grammar
 ((left: op-mult op-div)
  (left: op-add op-sub)
  op-lparen op-rparen
  op-semicolon
  number)

 (file
   (())
   ((file stmt)))
 (stmt
   ((expr op-semicolon) (print expr)))
 (expr
   ((number) number)
   ((expr@@a op-add expr@@b) (+ a b))
   ((expr@@a op-sub expr@@b) (- a b))
   ((expr@@a op-mult expr@@b) (* a b))
   ((expr@@a op-div expr@@b) (/ a b))
   ((op-lparen expr op-rparen) expr))))
@end smalllisp

Let's start with this input:
@example
1 + 2 * 3;
@end example

At the point where the parser has read `1 + 2` and the lookahead symbol
is `*`, the parser encounters a shift/reduce conflict.  Should it first
reduce by the `(expr op-add expr)` production or shift the `*` in
the hopes of reducing the latter expression first?

The `(expr op-add expr)` production has gotten its precedence from the
`op-add` terminal symbol.  This is the precedence of the reduce.  The
precedence of the shift comes from the precedence assigned to the lookahead
terminal symbol, which is `op-mult`.  Since `op-mult` has higher
precedence, the parser generator in this state chooses to shift and does not
produce a warning.

Here's an example which we can use to demonstrate associativity:
@example
1 + 2 - 3;
@end example

The parser generator encounters a similar shift/reduce conflict this time,
except that when it tries to determine whether to shift or reduce, it finds
that both actions have the same precedence.  In this case, the parser
generator looks at the associativity of the precedence group containing the
`op-add` and `op-sub`.  Since these are declared to be
left-associative, the parser generator chooses to reduce from this state,
effectively calculating the `1 + 2`.  Had these symbols been 
right-associative, the parser would have chosen to shift, effectively
calculating `2 - 3` first.  If these symbols had been declared
non-associative with the `none:` keyword, the parser would generate an
error if it ever encountered this state.

@c -- The parsing function ------------------------------------------- @c
@node The Parsing Function, The Regular Grammar, Precedence and Associativity, Lalr Parsing
@comment  node-name,  next,  previous,  up
@section The parsing function
@cindex the lalr(1) parsing function

Once a grammar has been defined, it can be used to parse some input
using the following function:

@deffn {bigloo procedure} read/lalrp lg rg port 

This function takes three, possibly four, arguments. The first, `lg`, is
the Lalr(1) grammar. The second, `rg`, is the lexical analyzer that feeds
the grammar with tokens. The third argument, `port`, is the port that
contains the input to be parsed. The last argument, `emptyp`, if
provided, should be a function of one argument. It is called with each new
token read from the port and should return `#t` if the token denotes the
end of input. The result of the call is the value computed by the semantic
actions of the production rules.
@end deffn

@c -- The regular grammar -------------------------------------------- @c
@node  The Regular Grammar, Debugging Lalr Grammars, The Parsing Function, Lalr Parsing
@comment  node-name,  next,  previous,  up
@section The regular grammar
@cindex Lalr grammar and Regular grammar

In order to work properly, the regular grammar used with an
Lalr(1) grammar should follow some conventions:

@itemize @bullet

@item If a semantic value is to be associated with the token just
parsed, the regular grammar should return a pair whose `car` is the
token name (a symbol) and the `cdr` is the semantic value. 
@item If there is no value associated with the token, the regular
grammar can return just the token name. When used in conjunction with
an Lalr grammar, regular grammar should never return `#f` as a token
value. This is specially true when the regular grammar detects the end of
parsing. In that case, the regular grammar @emph{must not} return the 
`#f` value. A good way to handle end-of-file is illustrated in the 
following example:

@smalllisp
(let ((g (regular-grammar ()
             ...
             (else 
              (let ((c (the-failure)))
                 (if (eof-object? c)
                     c
                     (error 'rgc "Illegal character" c))))))
      (l (lalr-grammar ...)))
   (read/lalrp l g (current-input-port)))
@end smalllisp

This way, the Lalr grammar will automatically handles the end-of-file.
@end itemize

@c -- debugging ------------------------------------------------------ @c
@node Debugging Lalr Grammars, A Simple Example, The Regular Grammar, Lalr Parsing
@section Debugging Lalr Grammars
@cindex Debugging Lalr Grammars

Currently the debugging facility for debugging Lalr grammars is very
limited. When the parameter `bigloo-debug` is set to a value
greater or equal to 100, the Lalr engine outputs all of the state
changes the parser is going through.

@c -- A simple example ----------------------------------------------- @c
@node A Simple Example,  , Debugging Lalr Grammars, Lalr Parsing
@comment  node-name,  next,  previous,  up
@section A simple example
@cindex a simple example of Lalr(1) parsing
Here is the code for a simple calculator implemented by an Lalr(1)
grammar:

@smalllisp
(begin
  (read/lalrp
   (lalr-grammar
    (nl plus mult minus div const lpar rpar)
    (lines
     (())
     ((lines expression nl)    (display "--> ") 
                               (display expression) 
                               (newline))
     ((lines nl)))
    (expression
     ((expression plus term)   (+ expression term))
     ((expression minus term)  (- expression term))
     ((term)                   term))
    (term
     ((term mult factor)       (* term factor))
     ((term div factor)        (/ term factor))
     ((factor)                 factor))
    (factor
     ((lpar expression rpar)   expression)
     ((const)                  const)))

   (regular-grammar ()
    ((+ (or #\tab #\space)) (ignore))
    (#\newline              'nl)
    ((+ digit)              (cons 'const (string->number (the-string))))
    (#\+                    'plus)
    (#\-                    'minus)
    (#\*                    'mult)
    (#\/                    'div)
    (#\(                    'lpar)
    (#\)                    'rpar))

   (current-input-port))
  (reset-eof (current-input-port)))
@end smalllisp

