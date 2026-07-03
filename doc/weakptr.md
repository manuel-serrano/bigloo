<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/weakptr.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Weak Pointers                                                 -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/weakptr.scm")
,(example-path "../test/src/weakptr.bgl")

Weak Pointers
=============

> [!WARNING] Bigloo may support weak pointers. In order to activate this 
> support, Bigloo must be configured with the `finalization` enabled.
> That is, the `configure` script must be invoked with
> the option `--finalization=yes`. 

When the finalization and weak pointers support is enabled, Bigloo
defines the `cond-expand` properties `bigloo-finalizer` and
`bigloo-weakptr`. A program may test the support with expressions such
as:

```bigloo
(cond-expand
  (bigloo-weakptr <something>)
  (else <something-else>))
```

Weak pointers are pointers to objects which can be collected by the
garbage collector if they are weakly pointed to. An object is weakly
pointed to if the only pointers to it are weak pointers. Weakly
pointed objects can be collected by the garbage collector, and all the
weak pointers to such objects will cease to point to it and point to
`#unspecified` instead.

Predicate
---------

### weakptr? ###
Returns `#t` iff `obj` is a weak pointer, constructed by
`make-weakptr`.

### make-weakptr ###
Creates a weak pointer to `data` and `ref`.

### weakptr-data ###
Returns the `data` object pointed to by `ptr`. If the object has been
collected, it returns `#unspecified`.

### weakptr-data-set! ###
Set a new `data` to the weak pointer.

### weakptr-ref ###
Returns the `ref` object pointed to by `ptr`. If the object has been
collected, it returns `#unspecified`.

### weakptr-ref-set! ###
Set a new `ref` to the weak pointer.
