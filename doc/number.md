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
type of numbers, it also provides the standard Scheme number library.

Predicates
----------

### number? ###

### exact? ###

### inexact? ###

### complex? ###

### rational? ###

Comparison Operators
--------------------

### = ###
### > ###
### >= ###
### <= ###
### < ###


Conversions
-----------

### number->flonum ###
### fixnum->flonum ###
