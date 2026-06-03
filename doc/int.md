<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/int.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Integers                                                      -->
<!--==================================================================-->

,(implementation-path "../runtime/Ieee/fixnum.scm")
,(implementation-path "../runtime/Llib/bit.scm")
,(implementation-path "../runtime/Unsafe/bignumber.scm")
,(example-path "../test/src/int1.bgl")
,(example-path "../test/src/int2.bgl")
,(example-path "../test/src/bit.bgl")
,(example-path "../test/src/bignum.bgl")

Integers
========

Bigloo supports several different integers, each represented by their
own types:

  * `bint`: Bigloo integers. Their size depends on backend used. 
     Examples: `1`, `-5`, `2435`.
  * `belong`: A bigloo _exact long_. 
     Examples: `#e1`, `#e-5`, `#e2345`.
  * `bllong`: A bigloo _exact long long_. 
     Examples. `#l1`, `#l-5`, `#l2435`.
  * `int8`: An 8-bit integer.
     Examples. `#s8:1`, `#s8:-5`.
  * `uint8`: An 8-bit integer.
     Examples. `#u8:1`, `#u8:130`.
  * `int16`: An 16-bit integer.
     Examples. `#s16:1`, `#s16:-5`, `#s16:300`.
  * `uint16`: An 16-bit integer.
     Examples. `#u16:1`, `#u16:65535`, `#u16:300`.
  * `int32`: An 32-bit integer.
     Examples. `#s32:1`, `#s32:-5`, `#s32:300`.
  * `uint32`: An 32-bit integer.
     Examples. `#u32:1`, `#u32:65535`, `#u32:300`.
  * `int64`: An 64-bit integer.
     Examples. `#s64:1`, `#s64:-5`, `#s64:300`.
  * `uint64`: An 64-bit integer.
     Examples. `#u64:1`, `#u64:65535`, `#u64:300`.
  * `bignum`: Infinit precicision integers.
     Examples. `#z1`, `#z-5`, `#z2435`.
  
Each backends provides its own implementation of the following types:
`short`, `int`, `long`, `ulong`, `elong`, `uelong, `llong`, and `ullong`.

Ranges
------

### minvalfx ###
Mininum fixnum value.

### maxvalfx ###
Maxium fixnum value.

### minvalelong ###
Mininum elong value.

### maxvalelong ###
Maximum elong value.

### minvalllong ###
Maximum elong value.

### maxvalllong ###
Maximum llong value.

Predicates
----------

### integer? ###
Returns `#t` if and only if `obj` is an integer. It returns `#t` if `obj`'s
type is one of the integer types, or if it is a [real](./real.md) representing
an integer number.

### fixnum? ###
Returns `#t` if and only if `obj` is a `bint` value.

### elong? ###
Returns `#t` if and only if `obj` is an `elong` value.

### llong? ###
Returns `#t` if and only if `obj` is an `llong` value.

### int8? ###
Returns `#t` if and only if `obj` is an `int8` value.

### uint8? ###
Returns `#t` if and only if `obj` is an `nint8` value.

### int16? ###
Returns `#t` if and only if `obj` is an `int16` value.

### uint16? ###
Returns `#t` if and only if `obj` is an `nint16` value.

### int32? ###
Returns `#t` if and only if `obj` is an `int32` value.

### uint32? ###
Returns `#t` if and only if `obj` is an `nint32` value.

### int64? ###
Returns `#t` if and only if `obj` is an `int64` value.

### uint64? ###
Returns `#t` if and only if `obj` is an `nint64` value.

### zerofx? ###
Returns `#t` if and only if `n` is 0.

### zeroelong? ###
Returns `#t` if and only if `n` is 0.

### zerollong? ###
Returns `#t` if and only if `n` is 0.

### zeros8? ###
Returns `#t` if and only if `n` is 0.

### zerou8? ###
Returns `#t` if and only if `n` is 0.

### zeros16? ###
Returns `#t` if and only if `n` is 0.

### zerou16? ###
Returns `#t` if and only if `n` is 0.

### zeros32? ###
Returns `#t` if and only if `n` is 0.

### zerou32? ###
Returns `#t` if and only if `n` is 0.

### zeros64? ###
Returns `#t` if and only if `n` is 0.

### zerou64? ###
Returns `#t` if and only if `n` is 0.

### odd? ###
Returns `#t` if and only if `n` is an odd integer.

### oddfx? ###
Returns `#t` if and only if `n` is an odd number.

### oddelong? ###
Returns `#t` if and only if `n` is an odd number.

### oddllong? ###
Returns `#t` if and only if `n` is an odd number.

### odds8? ###
Returns `#t` if and only if `n` is an odd number.

### oddu8? ###
Returns `#t` if and only if `n` is an odd number.

### odds16? ###
Returns `#t` if and only if `n` is an odd number.

### oddu16? ###
Returns `#t` if and only if `n` is an odd number.

### odds32? ###
Returns `#t` if and only if `n` is an odd number.

### oddu32? ###
Returns `#t` if and only if `n` is an odd number.

### odds64? ###
Returns `#t` if and only if `n` is an odd number.

### oddu64? ###
Returns `#t` if and only if `n` is an odd number.

### even? ###
Returns `#t` if and only if `n` is an even integer.

### evenfx? ###
Returns `#t` if and only if `n` is an even number.

### evenelong? ###
Returns `#t` if and only if `n` is an even number.

### evenllong? ###
Returns `#t` if and only if `n` is an even number.

### evens8? ###
Returns `#t` if and only if `n` is an even number.

### evenu8? ###
Returns `#t` if and only if `n` is an even number.

### evens16? ###
Returns `#t` if and only if `n` is an even number.

### evenu16? ###
Returns `#t` if and only if `n` is an even number.

### evens32? ###
Returns `#t` if and only if `n` is an even number.

### evenu32? ###
Returns `#t` if and only if `n` is an even number.

### evens64? ###
Returns `#t` if and only if `n` is an even number.

### evenu64? ###
Returns `#t` if and only if `n` is an even number.

Comparison Operators
--------------------

### =fx ###
### =elong ###
### =llong ###
### =s8 ###
### =u8 ###
### =s16 ###
### =u16 ###
### =s32 ###
### =u32 ###
### =s64 ###
### =u64 ###
### =bx ###

### <fx ###
### <elong ###
### <llong ###
### <s8 ###
### <u8 ###
### <s16 ###
### <u16 ###
### <s32 ###
### <u32 ###
### <s64 ###
### <u64 ###
### <bx ###

### <=fx ###
### <=elong ###
### <=llong ###
### <=s8 ###
### <=u8 ###
### <=s16 ###
### <=u16 ###
### <=s32 ###
### <=u32 ###
### <=s64 ###
### <=u64 ###
### <=bx ###

### >fx ###
### >elong ###
### >llong ###
### >s8 ###
### >u8 ###
### >s16 ###
### >u16 ###
### >s32 ###
### >u32 ###
### >s64 ###
### >u64 ###

### >=fx ###
### >=elong ###
### >=llong ###
### >=s8 ###
### >=u8 ###
### >=s16 ###
### >=u16 ###
### >=s32 ###
### >=u32 ###
### >=s64 ###
### >=u64 ###
### >=bx ###

Conversions
-----------

### fixnum->int8 ###
### fixnum->uint8 ###

### fixnum->int16 ###
### fixnum->uint16 ###

### fixnum->int32 ###
### fixnum->uint32 ###

### fixnum->int64 ###
### fixnum->uint64 ###

### fixnum->bignum ###
### elong->bignum ###
### llong->bignum ###

### integer->string ###
Converts an integer into a string representation.

### integer->string/padding ###
The function `integer->string/padding` converts its arguments into
a string with a left padding filled of characters `0`.

### fixnum->string ###
Converts an fixnum integer into a string representation.

### elong->string ###
Converts an elong integer into a string representation.

### llong->string ###
Converts an llong integer into a string representation.

### bignum->string ###
Converts an exact integer into a string representation.

### unsigned->string ###
The function `unsigned->string` only accepts the following radixes:
`2`, `8`, and `16`. It converts its argument into an
_unsigned_ representation. The size of the generated string depends on
the size of integers on the hostplaform. On 64-bit platforms, they
will contain 64 characters, on 32-bit platforms only 32 characters.

### string->integer ###
Converts a string into a `long` value. Stops at the first character
that does not belong to the radix representation of numbers.

### string->elong ###
Converts a string into an `elong` value. Stops at the first character
that does not belong to the radix representation of numbers.

### string->llong ###
Converts a string into an `llong` value. Stops at the first character
that does not belong to the radix representation of numbers.

Basic Operators
---------------

### +fx/ov ###
Sums two longs, if an operation overflows, returns a `bignum`, otherwise,
returns a `long`.

### +fx ###
### +elong ###
### +llong ###
### +s8 ###
### +u8 ###
### +s16 ###
### +u16 ###
### +s32 ###
### +u32 ###
### +s64 ###
### +u64 ###
### +bx ###

### -fx/ov ###
Substracts two longs, if an operation overflows, returns a `bignum`, otherwise,
returns a `long`.

### -fx ###
### -elong ###
### -llong ###
### -s8 ###
### -u8 ###
### -s16 ###
### -u16 ###
### -s32 ###
### -u32 ###
### -s64 ###
### -u64 ###
### -bx ###

### *fx/ov ###
Multiplies two longs, if an operation overflows, returns a `bignum`, otherwise,
returns a `long`.

### *fx ###
### *elong ###
### *llong ###
### *s8 ###
### *u8 ###
### *s16 ###
### *u16 ###
### *s32 ###
### *u32 ###
### *s64 ###
### *u64 ###
### *bx ###

### /fx ###
### /elong ###
### /llong ###
### /s8 ###
### /u8 ###
### /s16 ###
### /u16 ###
### /s32 ###
### /u32 ###
### /s64 ###
### /u64 ###
### /bx ###

Operators
---------

### maxfx ###
### maxelong ###
### maxllong ###
### maxs8 ###
### maxu8 ###
### maxs16 ###
### maxu16 ###
### maxs32 ###
### maxu32 ###
### maxs64 ###
### maxu64 ###
### maxbx ###

### minfx ###
### minelong ###
### minllong ###
### mins8 ###
### minu8 ###
### mins16 ###
### minu16 ###
### mins32 ###
### minu32 ###
### mins64 ###
### minu64 ###
### minbx ###

### absfx ###
### abselong ###
### absllong ###
### abss8 ###
### absu8 ###
### abss16 ###
### absu16 ###
### abss32 ###
### absu32 ###
### abss64 ###
### absu64 ###
### absbx ###

### exptfx ###
### exptfx/ov ###
### expts32 ###
### exptu32 ###
### expts64 ###
### exptu64 ###
### exptbx ###

Division Operators
------------------

### remainder ###
Computes the remainder of the integer division.
The remainder is what one get from truncated division:
`n1=n2.b + remainder`.

### remainderfx ###
### remainderelong ###
### remainderllong ###
### remainders8 ###
### remainderu8 ###
### remainders16 ###
### remainderu16 ###
### remainders32 ###
### remainderu32 ###
### remainders64 ###
### remainderu64 ###
### remainderbx ###

### quotient ###
Computes the quotient of the integer division. 

### quotientfx ###
### quotientelong ###
### quotientllong ###
### quotients8 ###
### quotientu8 ###
### quotients16 ###
### quotientu16 ###
### quotients32 ###
### quotientu32 ###
### quotients64 ###
### quotientu64 ###
### quotientbx ###

### modulo ###
The modulo retains the type of the divisor `n2`.

### modulofx ###
### moduloelong ###
### modulollong ###
### modulos8 ###
### modulou8 ###
### modulos16 ###
### modulou16 ###
### modulos32 ###
### modulou32 ###
### modulos64 ###
### modulou64 ###
### modulobx ###

Gcd, Lcm
--------

### gcd ###
### gcdfx ###
### gcdelong ###
### gcdllong ###
### gcds8 ###
### gcdu8 ###
### gcds16 ###
### gcdu16 ###
### gcds32 ###
### gcdu32 ###
### gcds64 ###
### gcdu64 ###
### gcdbx ###

### lcm ###
### lcmfx ###
### lcmelong ###
### lcmllong ###
### lcms8 ###
### lcmu8 ###
### lcms16 ###
### lcmu16 ###
### lcms32 ###
### lcmu32 ###
### lcms64 ###
### lcmu64 ###
### lcmbx ###

Bit Manipulations
-----------------

### bit-or ###
The bit-or of two `long` integers.

### bit-ors8 ###
The bit-or of two `int8` integers.

### bit-oru8 ###
The bit-or of two `uint8` integers.

### bit-ors16 ###
The bit-or of two `int16` integers.

### bit-oru16 ###
The bit-or of two `uint16` integers.

### bit-ors32 ###
The bit-or of two `int32` integers.

### bit-oru32 ###
The bit-or of two `uint32` integers.

### bit-ors64 ###
The bit-or of two `int64` integers.

### bit-oru64 ###
The bit-or of two `uint64` integers.

### bit-orbx ###
The bit-or of two `bignum` integers.

### bit-orelong ###
The bit-or of two `elong` integers.

### bit-orllong ###
The bit-or of two `llong` integers.

### bit-xor ###
The bit-xor of two `long` integers.

### bit-xors8 ###
The bit-xor of two `int8` integers.

### bit-xoru8 ###
The bit-xor of two `uint8` integers.

### bit-xors16 ###
The bit-xor of two `int16` integers.

### bit-xoru16 ###
The bit-xor of two `uint16` integers.

### bit-xors32 ###
The bit-xor of two `int32` integers.

### bit-xoru32 ###
The bit-xor of two `uint32` integers.

### bit-xors64 ###
The bit-xor of two `int64` integers.

### bit-xoru64 ###
The bit-xor of two `uint64` integers.

### bit-xorbx ###
The bit-xor of two `bignum` integers.

### bit-xorelong ###
The bit-xor of two `elong` integers.

### bit-xorllong ###
The bit-xor of two `llong` integers.

### bit-and ###
The bit-and of two `long` integers.

### bit-ands8 ###
The bit-and of two `int8` integers.

### bit-andu8 ###
The bit-and of two `uint8` integers.

### bit-ands16 ###
The bit-and of two `int16` integers.

### bit-andu16 ###
The bit-and of two `uint16` integers.

### bit-ands32 ###
The bit-and of two `int32` integers.

### bit-andu32 ###
The bit-and of two `uint32` integers.

### bit-ands64 ###
The bit-and of two `int64` integers.

### bit-andu64 ###
The bit-and of two `uint64` integers.

### bit-andbx ###
The bit-and of two `bignum` integers.

### bit-andelong ###
The bit-and of two `elong` integers.

### bit-andllong ###
The bit-and of two `llong` integers.

### bit-not ###
The bit-not of a `long` integer.

### bit-nots8 ###
The bit-not of a `int8` integer.

### bit-notu8 ###
The bit-not of a `uint8` integer.

### bit-nots8 ###
The bit-not of a `int8` integer.

### bit-notu8 ###
The bit-not of a `uint8` integer.

### bit-nots16 ###
The bit-not of a `int16` integer.

### bit-notu16 ###
The bit-not of a `uint16` integer.

### bit-nots32 ###
The bit-not of a `int32` integer.

### bit-notu32 ###
The bit-not of a `uint32` integer.

### bit-nots64 ###
The bit-not of a `int64` integer.

### bit-notu64 ###
The bit-not of a `uint64` integer.

### bit-notbx ###
The bit-not of a `bignum` integer.

### bit-notelong ###
The bit-not of a `elong` integer.

### bit-notllong ###
The bit-not of a `llong` integer.

### bit-lsh ###
The bit left shift of a `long` integer.

### bit-lshs8 ###
The bit left shift of a `int8` integer.

### bit-lshu8 ###
The bit left shift of a `uint8` integer.

### bit-lshs16 ###
The bit left shift of a `int16` integer.

### bit-lshu16 ###
The bit left shift of a `uint16` integer.

### bit-lshs32 ###
The bit left shift of a `int32` integer.

### bit-lshu32 ###
The bit left shift of a `uint32` integer.

### bit-lshs64 ###
The bit left shift of a `int64` integer.

### bit-lshu64 ###
The bit left shift of a `uint64` integer.

### bit-lshbx ###
The bit left shift of a `bignum` integer.

### bit-lshelong ###
The bit left shift of a `elong` integer.

### bit-lshllong ###
The bit left shift of a `llong` integer.

### bit-rsh ###
The bit right shift of a `long` integer.

### bit-rshs8 ###
The bit right shift of a `int8` integer.

### bit-rshu8 ###
The bit right shift of a `uint8` integer.

### bit-rshs16 ###
The bit right shift of a `int16` integer.

### bit-rshu16 ###
The bit right shift of a `uint16` integer.

### bit-rshs32 ###
The bit right shift of a `int32` integer.

### bit-rshu32 ###
The bit right shift of a `uint32` integer.

### bit-rshs64 ###
The bit right shift of a `int64` integer.

### bit-rshu64 ###
The bit right shift of a `uint64` integer.

### bit-rshbx ###
The bit right shift of a `bignum` integer.

### bit-rshelong ###
The bit right shift of a `elong` integer.

### bit-rshllong ###
The bit right shift of a `llong` integer.

### bit-ursh ###
The _unsigned_ bit right shift of a `long` integer.

### bit-urshs8 ###
The _unsigned_ bit right shift of a `int8` integer.

### bit-urshu8 ###
The _unsigned_ bit right shift of a `uint8` integer.

### bit-urshs16 ###
The _unsigned_ bit right shift of a `int16` integer.

### bit-urshu16 ###
The _unsigned_ bit right shift of a `uint16` integer.

### bit-urshs32 ###
The _unsigned_ bit right shift of a `int32` integer.

### bit-urshu32 ###
The _unsigned_ bit right shift of a `uint32` integer.

### bit-urshs64 ###
The _unsigned_ bit right shift of a `int64` integer.

### bit-urshu64 ###
The _unsigned_ bit right shift of a `uint64` integer.

### bit-urshelong ###
The _unsigned_ bit right shift of a `elong` integer.

### bit-urshllong ###
The _unsigned_ bit right shift of a `llong` integer.


Miscellaneous
-------------

### random ###
Generates a random number 0 &le; `n` &lt; `max`.

### randombx ###
Generates a random number 0 &le; `n` &lt; `max`.

### seed-random! ###
<!-- [:seed-random!@NoTest-C-jvm] -->
