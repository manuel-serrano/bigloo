<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/module5.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Macros                                                        -->
<!--==================================================================-->

Macro
=====

Bigloo makes use of two macro expansion systems. The one based on the
[expansion passing style][Dybvig et al. 86] and the one advocated by the 
[R5RS][Scheme R5RS].

Expansion passing style macros
------------------------------

### (define-expander name proc) ###
<!-- [:define-expander@NoDef] -->

This form defines an expander, `name`, where `proc`
is a procedure of two arguments: a form to macro-expand,
and an expander.

Example:

```bigloo
(define-expander when 
   (lambda (x e)
      (match-case x
         ((?- ?test . ?exps)
          (e `(if ,test (begin ,@exps)) e))
         (else
           (error "when" "illegal form" x)))))

(when (> a 0) (print a) a)
   &arr; (if (> a 0) (begin (print a) a))
```

### define-macro (name ...) body) ###
<!-- [:define-macro@NoDef] -->

This form is itself macro-expanded into a `define-expander` form.

Macro expanders cannot be exported or imported since there is no way
to specify expanders in a module declaration.

Macros defined with `define-expander` and `define-macro`
are used by both the compiler and the interpreter.

Example:

```bigloo
(define-macro (when test . exps)
   `(if ,test (begin ,@exps)))
```

Revised(5) macro expansion
--------------------------

Bigloo support the Revised(5) Report on the Scheme programming language.

### (let-syntax (...) body ###
<!-- [:let-syntax@NoDef] -->

### (letrec-syntax (...) body ###
<!-- [:letrec-syntax@NoDef] -->

### (define-syntax keyword transformer ###
<!-- [:define-syntax@NoDef] -->

### (syntax-rules literals rule...) ###
<!-- [:syntax-rules@NoDef] -->

These three forms are compatible with the description of the
Revised(5) Report on the Algorithmic Language Scheme.

> [!WARNING] Bigloo does not ensure hygiene for
> `let-syntax` and `letrec-syntax`. Hygienic expansion is
> only guaranteed for `define-syntax`.


