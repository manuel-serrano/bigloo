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

,(example-path "../test/src/match.bgl")


Pattern Matching
================

Pattern matching is a key feature of most modern functional
programming languages since it allows clean and secure code to be
written. Internally, _pattern-matching forms_ should be translated
(compiled) into cascades of _elementary tests_ where code is made as
efficient as possible, avoiding redundant tests; Bigloo's _pattern
matching compiler_ provides this.

The _pattern language_ allows the expression of a wide variety of
patterns, including:

  * Non-linear patterns: pattern variables can appear more than
    once, allowing comparison of subparts of the datum (through `eq?`);
  * Recursive patterns on lists: for example, checking that the
    datum is a list of zero or more `a`s followed by zero or more 
    `b`s.
  * Pattern matching on lists as well as on vectors and structures, 
    record types, and classes.


The Pattern Language
--------------------

The syntax for <pattern> is:

```bnf
<pattern> -->                          ;; Matches:
    <atom>                             ;; the <atom>
  | ( kwote <atom> )                   :: any expression `eq?` to <atom>.
  | ( and <pat1> ... <patn> )          ;; if all of <pati> match.`
  | ( or <pat1> ... ...<patn> )        ;; if any of <pat1> through <patn> matches.
  | ( not <pat> )                      ;; if <pat> doesn't match.
  | (`?` <predicate> )                 ;; if <predicate> is true.`
  | ( <pat1> ... <patn> )              ;; a list of n elements. Here, `...` is a
                                       ;; meta-character denoting a finite 
                                       ;; repetition of patterns.
  | <pat> ...                          ;; a (possibly empty) repetition
                                       ;; of <pat> in a list.`
  | #( <pat> ... <patn> )              ;; a vector of `n` elements.`
  | #{ <struct> <pat> ... }            ;; a structure.
  | ( isa <class> ( <id> <pat> ) ... ) ;; a class instance.
  | `?`<id>                            ;; anything, and binds <id> as a variable.
  | `?-`                               ;; anything.
  | `??-`                              ;; any (possibly empty) repetition of anything
                                       ;; in a list.
  | `???-`                             ;; any end of list.
```

@noindent
@emph{Remark: ` `and, or, not, check` and `kwote` must be
quoted in order to be treated as literals. This is the only justification
for having the `kwote` pattern since, by convention, any atom which is
not a keyword is quoted.

@itemize @bullet
@item `?-` matches any s-expr

@item `a` matches the atom `'a`.

@item `?a` matches any expression, and binds the variable `a` to
this expression.

@item `(? integer?)` matches any integer

@item `(a (a b))` matches the only list `'(a (a b))`.

@item `???-` can only appear at the end of a list, and always succeeds.
For instance, `(a ???-)` is equivalent to `(a . ?-)`.

@item when occurring in a list, `??-` matches any sequence of anything:
`(a ??- b)` matches any list whose `car` is `a` and last
`car` is `b`. 

@item `(a ...)` matches any list of `a`'s, possibly empty.

@item `(?x ?x)` matches any list of length 2 whose `car` is 
 @emph{eq` to its `cadr`

@item `((and (not a) ?x) ?x)` matches any list of length 2 whose 
`car` is not @emph{eq` to `'a` but is @emph{eq` to its `cadr`

@item `#(?- ?- ???-)` matches any vector whose length is at least 2.

@item `#@{foo (?- . ?-) (? integer?)@`` matches any structure or
record `foo` whose first and second fields are respectively a pair and an
integer. You can provide only the fields you want to test. The order is not
relevant.
@end itemize

@emph{Remark: ` `??-` and `...` patterns can not appear
inside a vector, where you should use `???-`: For example, 
`#(a ??- b)` or `#(a...)` are invalid patterns, whereas 
`#(a ???-)` is valid and matches any vector whose first element 
is the atom `a`.

@subsection Class Patterns
@cindex class patterns
@cindex isa pattern

The `isa` pattern matches instances of Bigloo classes. The syntax is:

@smallexample
(isa <class-name> (<field-name> <pattern>) ...)
@end smallexample

Each `(<field-name> <pattern>)` pair matches the named field against
the given pattern. Fields not mentioned are ignored, allowing partial
matching. If no fields are specified, only the type is checked.

The type check uses `isa?`, so a pattern `(isa point ...)` will
match instances of `point` and any subclass of `point`.

@smalllisp
(define-class point (x (default 0)) (y (default 0)))
(define-class point3d::point (z (default 0)))

;; Basic field binding
(match-case (instantiate::point (x 3) (y 4))
   ((isa point (x ?x) (y ?y)) (list x y)))
   @result{` (3 4)

;; Partial matching (only check some fields)
(match-case (instantiate::point3d (x 1) (y 2) (z 3))
   ((isa point3d (z ?z)) z))
   @result{` 3

;; Type-only check (no fields)
(match-case (instantiate::point (x 1) (y 2))
   ((isa point) 'yes)
   (else 'no))
   @result{` yes

;; Literal values in fields
(match-case (instantiate::point (x 0) (y 5))
   ((isa point (x 0) (y ?y)) y)
   (else 'fail))
   @result{` 5

;; Subclass matching via inheritance
(match-case (instantiate::point3d (x 1) (y 2) (z 3))
   ((isa point (x ?x) (y ?y)) (list x y)))
   @result{` (1 2)
@end smalllisp

Class patterns compose freely with other pattern combinators:

@smalllisp
;; or: match multiple class types
(match-case obj
   ((or (isa point3d (z ?v)) (isa point (x ?v))) v))

;; and: bind the whole object and destructure
(match-case obj
   ((and ?whole (isa point (x ?x) (y ?y)))
    (list whole x y)))

;; not: match non-instances
(match-case obj
   ((not (isa point)) 'not-a-point)
   (else 'is-a-point))

;; Predicates inside fields
(match-case obj
   ((isa point (x (and ?x (? positive?)))) x))

;; Nested class patterns
(define-class segment
   (start (default #unspecified))
   (end (default #unspecified)))

(match-case seg
   ((isa segment (start (isa point (x ?x1) (y ?y1)))
                 (end (isa point (x ?x2) (y ?y2))))
    (list x1 y1 x2 y2)))

;; Repeated variables (equality constraint across fields)
(define-class rect (width (default 0)) (height (default 0)))

(match-case (instantiate::rect (width 5) (height 5))
   ((isa rect (width ?s) (height ?s)) s)
   (else 'not-square))
   @result{` 5
@end smalllisp

Forms
-----
### (match-case key clauses ...) ###
<!-- [:cond-expand@NoDef] -->

The argument `key` may be any expression and each `clause` has the form:
(&lt;pattern&gt; &lt;expression&gt; ...)

** Semantics: ** A `match-case` expression is evaluated as
follows. `key` is evaluated and the result is compared with each
successive pattern. If the pattern in some `clause` yields a match, then
the expressions in that `clause` are evaluated from left to right in an
environment where the pattern variables are bound to the corresponding
subparts of the datum, and the result of the last expression in that
`clause` is returned as the result of the `match-case` expression.
If no `pattern` in any `clause` matches the datum, then, if there is an
`else` clause, its expressions are evaluated and the result of the last
is the result of the whole `match-case` expression; otherwise the result
of the `match-case` expression is unspecified.

The equality predicate used is `eq?`.

```bigloo
(match-case '(a b a)
   ((?x ?x) 'foo)
   ((?x ?- ?x) 'bar))
   &rarr; bar
```
The following syntax is also available:

### (match-lambda clauses ...) ###
<!-- [:cond-expand@NoDef] -->

It expands into a lambda-expression expecting an argument which, once
applied to an expression, behaves exactly like a `match-case`
expression.

```bigloo
((match-lambda
   ((?x ?x) 'foo)
   ((?x ?- ?x) 'bar))
 '(a b a))
   &rarr; bar
```

