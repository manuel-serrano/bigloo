<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Pairs                                                         -->
<!--==================================================================-->

,(implementation-path "../runtime/Ieee/pairlist.scm")
,(example-path "../test/src/pair.bgl")

Pairs and Lists
===============

The form `'()` is _illegal_.

Constructors
------------

### cons ###

Creates a pair.

### econs ###

Creates an _extended pair_ from `obj``, `obj2`, and `obj3`. An extended
pair behaves as a regular pair and can be used wherever a pair is
expected. Contrary to plain pairs, extended pairs are the only objects
that satisfy the predicate `epair?` and they share a third object
that can be accessed with the `cer` getter

### cons* ###
Returns an object formed by consing all arguments together from right to left.
If only one `obj` is supplied, that `obj` is returned.

### list ###

Creates a fresh list from its arguments.

### make-list ###

Returns an `n`-element list, whose elements are all the value `o`. 
If the `o` argument is not given, the elements of the list may be
arbitrary values.


Predicates
----------

### pair-or-null? ###
Returns `#t` if @var{obj} is either a pair or the empty list. Otherwise
it returns `#f`.

### null? ###

Returns `#t` if and only if `obj` is the empty list. Returns `#f` otherwise.

### list? ###

Returns `#t`, if and only is `obj` is a pair whose last `cdr` is the
empty list.

Getters and Setters
-------------------

### car ###

Returns the first projection of `pair`.

### cdr ###

Returns the second projection of `pair`.

### cer ###

Returns the third projection of `epair`.

### set-car! ###

Sets the first projection of `pair` to `obj`.

### set-cdr! ###

Sets the second projection of `pair` to `obj`.

### set-cer! ###

Sets the third projection of `epair` to `obj`.

### caar ###
The expression `(caar x)` is equivalent to `(car (car x))`.

### cadr ###
### cadar ###
### caadr ###
### caaar ###
### caddr ###
### cadar ###
### cdddar ###
### cddddr ###

Library Functions
-----------------

### length ###

Returns the number of elements of `list`. It is an error to call `length`
with a value that does not satisfy the predicate `list?`.

### append ###

The function `append` appends all its list arguments.

### append! ###

A destructive `append`.

### eappend ###

The function `eappend` appends all its list arguments. If the argument
is an `epair`, it produces an `epair`. Otherwise, it returns a `pair`.

### reverse ###

Reverses the elements of the `l`.

### reverse! ###

A destructive `reverse`.


### list-ref ###

Returns the `k`th element of `list`.

### take ###

The function `take` returns a new list made of the first @var{k}
element of `list`.


### drop ###

The function `drop` returns the sublist of `list`
obtained by omitting the first `k` elements.

### list-tail ###

Behaves as `take`. 

### last-pair ###

Returns the last pair in the nonempty, possibly improper, @var{list}.


### memq ###
Returns the first sublist of `list` whose `car` is `eq?` to `obj`.
Returns `#f` if no element is found.

### memv ###
Returns the first sublist of `list` whose `car` is `eqv?` to `obj`.
Returns `#f` if no element is found.

### member ###
Returns the first sublist of `list` whose `car` is `equal?` to `obj`.
Returns `#f` if no element is found.

### assq ###
Returns the first element of `list` whose `car` is `eq?` to `obj`.
Returns `#f` if no element is found.

### assv ###
Returns the first element of `list` whose `car` is `eqv?` to `obj`.
Returns `#f` if no element is found.

### assoc ###
Returns the first element of `list` whose `car` is `equal?` to `obj`.
Returns `#f` if no element is found.

### remq ###
Returns a copy of `list` where all elements `eq?` to `obj` are removed.

### remv ###
Returns a copy of `list` where all elements `eqv?` to `obj` are removed.

### delete ###

Returns a copy of `list` where all elements `eq` to `obj` are removed.

### remq! ###
Destructive `remq`.

### remv! ###
Destructive `remv`.

### delete! ###

Destructive `delete`.

### every ###

Applies the function `pred` across the lists, returning the last 
non-false if the function returns non-false on every application. If 
non-false, the result of `every` is the last value returned by the
last application of `fun`.


### any ###

Applies the function `pred` across the lists, returning non-false if the
function returns non-false for at least one application. If non-false,
the result of `any` is the first non-false value returned by `pred`.


### find ###

Return the first element of `list that satisfies predicate
`pred`; false if no element does.

Note that `find` has an ambiguity in its lookup semantics -- if
find returns `#f`, you cannot tell (in general) if it found a
`#f` element that satisfied `pred`, or if it did not find any
element at all. In many situations, this ambiguity cannot arise --
either the list being searched is known not to contain any `#f`
elements, or the list is guaranteed to have an element satisfying
pred. However, in cases where this ambiguity can arise, you should use
`find-tail` instead of find -- `find-tail` has no such ambiguity:

```
(cond ((find-tail pred lis) => (lambda (pair) ...)) ; Handle (CAR PAIR)
      (else ...)) ; Search failed.
```

&nbsp;

### find-tail ###

Return the first pair of `list` whose `car` satisfies
`pred`. If no pair does, return `#f`.

The funtion `ind-tail` can be viewed as a general-predicate variant of
the `member` function. In the circular-list case, this procedure
"rotates" the list.


### reduce ###

If `list` is null returns `ridentity`, if `list` has
one element, returns that element. Otherwise, returns `f` applied to
the first element of the `list` and to `reduce` of the rest of
the list.


### list-tabulate ###

Returns an `n`-element list. Element i of the list, where 
`0 &ge; i &gt; n`, is produced by `(init-proc i)`. No guarantee is made 
about the dynamic order in which `init-proc` is applied to these indices.


### list-split ###
Splits the list `l into a list of lists of length `n`. Last smaller
list is filled with `fill`.

### iota ###

Returns a list containing incremented numbers. The
argument `count` is the size of the constructed list. 
The optional arguments `start` and `step` default to `0` and `1`, 
respectively. This procedure takes its name from the APL primitive.


### list-copy ###
The function `list-copy` copies the spine of the of the list.

### tree-copy ###

The function `tree-copy` recursively copies its arguments, descending
only into the list cells.

### delete-duplicates ###
### delete-duplicates! ###


The function `delete-duplicates` removes duplicate elements from the `list`
argument. If there are multiple equal elements in the argument list,
the result list only contains the first or leftmost of these elements
in the result. The order of these surviving elements is the same as in
the original list -- `delete-duplicates` does not disorder the list
(hence it is useful for "cleaning up" association lists).

The `equal` parameter is used to compare the elements of the list;
it defaults to `equal?`. If x comes before y in list, then the
comparison is performed `(= x y)`. The comparison procedure will be used
to compare each pair of elements in list no more than once; the order
in which it is applied to the various pairs is not specified.

The function `delete-duplicates` is allowed to share common tails
between argument and result lists -- for example, if the list argument
contains only unique elements, it may simply return exactly this
list. 

