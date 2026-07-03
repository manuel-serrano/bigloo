<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/real.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Numbers                                                       -->
<!--==================================================================-->

,(implementation-path "../runtime/Ieee/number.scm")
,(example-path "../test/src/number.bgl")

Numbers
=======

Bigloo supports a limited numerical tower. It supports 
[integers, bignum, int8, ...](./int.md) and [reals](./real.md). In
addition to the specific operators available for each particular
type of numbers, it also provides the standard Scheme generic number library,
which is described in this chapter.

> [!NOTE] To benefit from better error messages and faster generated code,
> it is recommended to use specific operators instead of generic operators
> (i.e., use `+fx` or `+fl` instead of `+` as much as possible).


Predicates
----------

### number? ###
Returns `#t` if and only if `obj` is any kind of number (e.g., `bint`, `real`,
`bignum`, `int8`, ...).

### exact? ###
Returns `#t` if and only if `obj` is an exact number.

### inexact? ###
Returns `#t` if and only if `obj` is an inexact number.

### complex? ###
Returns `#t` if and only if `x` is a number.

### rational? ###
Returns `#t` if and only if `x` is a real. 

### zero? ###
Returns `#t` if the number 0.

Comparison Operators
--------------------

### = ###
Returns `#t` if an only if the arguments are pair-wise equal.

### > ###
Returns `#t` if an only if the arguments are pair-wise greater than.

### >= ###
Returns `#t` if an only if the arguments are pair-wise greater or equal than.

### <= ###
Returns `#t` if an only if the arguments are pair-wise lesser or equal than.

### < ###
Returns `#t` if an only if the arguments are pair-wise lesser than.

Conversions
-----------

### number->flonum ###
Converts its number argument into a string.

### flonum->fixnum ###
### fixnum->flonum ###
### flonum->elong ###
### elong->flonum ###
### flonum->llong ###
### llong->flonum ###
### bignum->flonum ###
### flonum->bignum ###

### flonum->int32 ###
### int32->flonum ###
### flonum->uint32 ###
### uint32->flonum ###

### flonum->int64 ###
### int64->flonum ###
### flonum->uint64 ###
### uint64->flonum ###

### exact->inexact ###
### inexact->exact ###

### number->string ###
Converts a number into a string representation.

### string->number ###
Converts a string into a number.

> [!WARNING] Contrary to `string->integer` and `string->real` that
> convert the number part of the string they receive, the function 
> `string->number` returns `#f` if not the representation of a single number.


Basic Operators
---------------

### + ###
### - ###
### * ###
### / ###

Operators
---------

### abs ###
### floor ###
### ceiling ###
### truncate ###
### round ###
### exp ###
### log ###
### log2 ###
### log10 ###
### sin ###
### cos ###
### tan ###
### asin ###
### acos ###
### atan ###
### sqrt ###
### expt ###
