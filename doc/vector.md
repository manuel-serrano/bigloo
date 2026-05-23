<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/vector.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Vectors                                                       -->
<!--==================================================================-->

,(include "head.html")

,(implementation-path "../runtime/Ieee/vector.scm")
,(example-path "../test/src/vector.bgl")

Vectors
=======

Vectors are not autoquoted objects.


Constructors
------------

### make-vector ###
Creates a vector of size `len`, initializes it with `fill`, which defaults
to `#unspecified`.

### vector ###

Creates a new vector of size `(length args)` initialized with all
the elements of `args`.


### copy-vector ###

Allocates a new vector of size `len` and fills it with the first `len`
element of `vector`. The new length `len` may be bigger than
the old vector length.

### vector-copy ###

Returns a newly allocated vector formed from the elements of `vector`
beginning with index `start` (inclusive) and ending with index `end`
(exclusive). The arguments `start` and `end` must be exact integers
satisfying: `0 &ge; start &ge; end &ge; (vector-length vector)`.

### vector-append ###

Returns a newly allocated vector that contains all elements in order
from the subsequent locations in `vector`.


Predicates
----------

### vector? ###
Returns `#t` if and only if `obj` is a vector. Returns `#f` otherwise.

Getters and setters
-------------------

### vector-length ###

Returns the size of the `vector`.

### vector-ref ###

Returns the character at position `k` in `vector`. If the argument 
`k` is not in the range `0 &le; k &lt; vector.length`, an exception is 
triggered.


### vector-set! ###

Sets the character at position `k` of `vector`. If the argument 
`k` is not in the range `0 &le; k &lt; vector.length`, an exception is 
triggered.


Library functions
-----------------

### vector->list ###

Converts the `vector` argument into a list.


### list->vector ###

Converts the `list` argument into a vector.

### vector-fill! ###

The function `vector-fill!` stores  `fill` in every element of vector.


### vector-copy! ###

Copies a block of elements from `source` to `target`, both of
which must be vectors, starting in target at `tstart` and starting in
source at `sstart`, ending when `send - sstart` elements have been
copied. It is an error for `target` to have a length less than 
`tstart + (send - sstart)`. The argument `sstart` defaults to `0` and 
`send` defaults to the length of `source`.

### vector-for-each ###

Apply `proc` to all the elements of the `vectors`. The arity
of `proc` must be the number of passed vectors. All vectors
must have the same length. The procedure is applied from elements
of index `0` to `(vector-length vector) - 1`.

### vector-map ###

The function `vector-map` creates a new vector whose size the is
the size of its argument `vector`. Each elements of the new vector
is the result of apply `proc` to the corresponding elements of the 
initial vectors.

### vector-map! ###

The function `vector-map!` modifies its `vector` argument by
applying `proc` to all its elements.

### vector-shrink! ###

Shrink a vector. The resulting vector's len is the minimum value of
`(vector-length vec)` and `nlen`. The argument `end` must be an exact
integers satisfying: `0 &le; end &le; (vector-length vector)`.

The function `vector-shrink!` returns a new vector formed from the
values of `vector` beginning with index 0 (inclusive) and ending with
index `end` (exclusive). As much as possible `vector-shrink!`  changes
the argument `vector`. That is, as much as possible, and for the
back-ends that enable it, `vector-shrink!` operates a side effect on
its argument.
