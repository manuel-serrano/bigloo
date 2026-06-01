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
,(example-path "../test/src/int.bgl")
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
  * `bignum`: Infinit precicision integers.
     Examples. `#z1`, `#z-5`, `#z2435`.
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
  
Each backends provides its own implementation of the following types:
`short`, `int`, `long`, `ulong`, `elong`, `uelong, `llong`, and `ullong`.

Predicates
----------

### integer? ###
Returns `#t` if and only if `obj` is an integer. It returns `#t` if `obj`'s
type is one of the integer types, or if it is a [real](./real.md) representing
an integer number.

### fixnum? ###
Return `#t` if and only if `obj` is a `bint` value.

### elong? ###
Return `#t` if and only if `obj` is an `elong` value.

### llong? ###
Return `#t` if and only if `obj` is an `llong` value.

### int8? ###
Return `#t` if and only if `obj` is an `int8` value.

### uint8? ###
Return `#t` if and only if `obj` is an `nint8` value.

### int16? ###
Return `#t` if and only if `obj` is an `int16` value.

### uint16? ###
Return `#t` if and only if `obj` is an `nint16` value.

### int32? ###
Return `#t` if and only if `obj` is an `int32` value.

### uint32? ###
Return `#t` if and only if `obj` is an `nint32` value.

### int64? ###
Return `#t` if and only if `obj` is an `int64` value.

### uint64? ###
Return `#t` if and only if `obj` is an `nint64` value.

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
