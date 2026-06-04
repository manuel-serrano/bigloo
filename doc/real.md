<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/real.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Reals                                                         -->
<!--==================================================================-->

,(implementation-path "../runtime/Ieee/flonum.scm")
,(example-path "../test/src/real.bgl")

Reals
=====

Real numbers of supported by the `real` type. Reals are legal values
whose type can be checked and which can be used in any number
operation (e.g., `cos`, `+`, ...). Bigloo also supports the `fload`
and `double` types that represents the reals of the host platform. Reals
are automatically converted to doubles and vice-versa when needed.

Real literals are described by the following grammar:

```bnf
<real> --> -?[0-9]+.[0-9]*
  | -?.[0-9]+
  | -?[0-9]+e[0-9]+
  | -?[0-9].[0-9]*+e-?[0-9]+
  | -?.[0-9]+e-?[0-9]+
  | +inf.0
  | -inf.0
  | +nan.o
```

Examples: `1.2`, `34e3`, `-5.6`, `+inf.0`...


Predicates
----------

### flonum? ###
Returns `#t` if and only if `obj` is a `real`.

### real? ###
Returns `#t` if an only if `obj` is a real number. 

> [!NOTE] Integers are also real so `flonum?` and `real?` will not
> always returns the same result.

### integerfl? ###
Returns `#t` if and only if the real is an integer.

### zerofl? ###
Returns `#t` if and only if the real is 0.

### finitefl? ###
Returns `#t` if and only if the real is finite.

### infinitefl? ###
Returns `#t` if and only if the real is infinite.

### nanfl? ###
Returns `#t` if and only if the real is a NaN value.

### oddfl? ###
Returns `#t` if and only if the real is odd.

### evenfl? ###
Returns `#t` if and only if the real is even.


Comparison Operators
--------------------

### =fl ###
### >fl ###
### >=fl ###
### <=fl ###
### <fl ###


Conversions
-----------

### string->real ###
Converts a string into a real. Stops at the first character that is not
part of the real.

### ieee-string->real ###
Convert the big-endian IEEE representations to their numeric values. Returns
a real.

### ieee-string->double ###
As `ieee-string->real` but returns a `double` instead of a `real` value.

### ieee-string->float ###
As `ieee-string->real` but returns a `float` instead of a `real` value.

### real->ieee-string ###
Convert the big-endian IEEE representations to their numeric values. Returns
a real.

### double->ieee-string ###
As `real->ieee-string` but accepts a `double` instead of a `real` value.

### float->ieee-string ###
As `real->ieee-string->real` but returns a `float` instead of a `real` value.

### double->llong-bits ###
Converts the IEEE representation of the double into a `llong` value. Usually,
this operation is implemented as a mere register tranfer from a floating point
register to an integer register.

### llong-bits->double ###
Converts the IEEE representation of a `llong` into a double. Usually,
this operation is implemented as a mere register tranfer from a integer
register to a floating point register.

### float->int-bits ###
Converts the IEEE representation of the float into a `int` value. Usually,
this operation is implemented as a mere register tranfer from a floating point
register to an integer register.

### int-bits->float ###
Converts the IEEE representation of a `int` into a float. Usually,
this operation is implemented as a mere register tranfer from a integer
register to a floating point register.


Basic Operators
---------------

### +fl ###
### -fl ###
### *fl ###
### /fl ###


Operators
---------

### negfl ###
### absfl ###
### floorfl ###
### ceilingfl ###
### truncatefl ###
### roundfl ###
### remainderfl ###
### expfl ###
### exptfl ###
### logfl ###
### log2fl ###
### log10fl ###
### sinfl ###
### tanfl ###
### sqrtfl ###


Miscellaneous
-------------

### signbitfl ###
Returns the value of the float point sign bit of the IEEE representation.
Namely, negative floats have their most significant bit set to `1`. 
Positive floats have their bit set to `0`.

### randomfl ###
Generates a random number statisfying 0 &le; `n` &lt; `1`.
