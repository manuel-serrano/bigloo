<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/boolean.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Booleans and predicates                                       -->
<!--==================================================================-->

,(implementation-path "../runtime/Ieee/boolean.scm")
,(implementation-path "../runtime/Ieee/equiv.scm")
,(example-path "../test/src/bool.bgl")

Booleans
========

Predicates
----------

The standard boolean objects are `#t` and `#f`. In a test, **all** values
but `#f` are considered as true.

### boolean? ###
Returns `#t` if and only if `obj` is a boolean. Returns `#f` otherwise.

### eq? ###
Returns `#t` if `obj1` and `obj2` are the same object in memory.

> [!WARNING]
> The C, jvm, and wasm may represent datatype differently. So it might be 
> that some `eq?` expressions evaluate to `#t` for some backends and `#f` 
> for the other. For instance, the C and wasm backends do not allocate fixnum
> integers that are then `eq?` when compared, while the jvm backend allocate
> them, and consequently `eq?` will be false. When it exists, use the
> datatype specific equalitiy predicate (for instance, for numbers prefer
> `=` and for fixnums prefer `=fx`), to the generic `eq?` predicate.

### eqv? ###
Returns `#t` if `obj1` and `obj2` are the same value.

### equal? ###
Returns `#t` if `obj1` and `obj2` are `eqv?` or if they are of the same
type and same length, and their subcomponents are recursively `equal?`.

> [!WARNING]
> The function `equal?` cannot be used on cyclic data structures.


Library functions
-----------------

### not ###
Returns `#t` is `obj` is false, otherwise returns `#f`.

