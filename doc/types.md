<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/boolean.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Types                                                         -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/error.scm")
,(example-path "../test/src/types.bgl")

Types
=====

Bigloo is an optionally typed dynamic language. This means that any
variable and any function argument and any function return value can
be annotated with type. Type annotations take the form of the suffix
`::type` added to identifiers. For instance `(let ((num::double 3.4)) ...)`
declares a local variable `num` whose type is `double`.

Type annotations verifications combine compile-time analysis and
runtime-time checks. If the compiler can prove that the values
that flows in a variable or function are of the proper type, no dynamic
check is involed. If the compiler cannot prove it insert a dynamic type
check *if compiling in safe mode*.

> [!NOTE] When compiling in unsafe mode (`-unsafe` command line option)
> the compiler trust the type annotations without inserting any dynamic
> type checks. Consequently, unsafe mode can yield erroneously typed programs 
> to crash.

Bigloo uses two hierarchies of types: the _Bigloo_ types and the
_host_ types.  Bigloo types and host types are related together by a
mapping that enables the compiler to implicitly convert Bigloo values
into host values and vice-versa.

In addition to the set of pre-defined types Bigloo supports various
means for declaring new types, including [class definition](./object.html)
and [extern types](./module.html).


Predefined Bigloo Types
-----------------------

Bigloo types:

  * `obj`: the generic type denoting all Bigloo values. All Bigloo types 
     are subtypes of `obj`.
  * `bbool`: the [boolean](./bool.html) `#t` or `#f`.
  * `unspecified`: the [unspecified](./bigloo.html) value `#unspecified`.

Lists:

  * `pair`: the type of `cons` [lists](./pair.html).
  * `epair`: a subtype of `pair`, the type of `econs` [lists](./pair.html).
  * `nil`: the empty [list](./pair.html).
  * `pair-nil`: a `pair` or `nil`.
  
Numbers:

  * `bint`: Bigloo fixnum [integers](./int.html). The size of these 
    integers, i.e., the number of bits available for their representation, 
    depends on the backend used. See below section [Type Conversions](#Type Conversions).
  * `belong`: Bigloo boxed  _exact_ [integers](./int.html).
  * `bllong`: Bigloo boxed _long long_ [integers](./int.html).
  * `bint8`, `buint8`: Bigloo boxed [8-bit integers](./int.html).
  * `bint16`, `buint16`: Bigloo boxed [16-bit integers](./int.html).
  * `bint32`, `buint32`: Bigloo boxed [32-bit integers](./int.html).
  * `bint64`, `buint64`: Bigloo boxed [64-bit integers](./int.html).
  * `bignum`: infinite [integers](./int.html).
  * `real`: Bigloo floating point [numbers](./number.html).

Strings and characters:

  * `bchar`: Bigloo [characters](./string.html).
  * `bstring`: Bigloo [strings](./string.html).
  * `regexp`: [regular expressions](./regexp.html).

Symbols and Keywords:

  * `symbol`: [symbols](./symbol.html).
  * `keyword`: [keywords](./symbol.html).
  
Data Structures:

  * `cell`: [cells](./cell.html).
  * `struct`: [structures](./struct.html).
  * `vector`: [vectors](./vector.html).
  * `s8vector`, `u8vector`: [vector of 8-bit integers](./hvector.html).
  * `s16vector`, `u16vector`: [vector of 16-bit integers](./hvector.html).
  * `s32vector`, `u32vector`: [vector of 32-bit integers](./hvector.html).
  * `s64vector`, `u64vector`: [vector of 64-bit integers](./hvector.html).
  * `f32vector`, `f64vector`: [vector of 32 and 64-bit floats](./hvector.html).
  * `weakptr`: [weak pointers](./weakptr.html).
  
I/O:

  * `input-port`: Input [ports](./ports.html).
  * `output-port`: Output [ports](./ports.html).
  * `binary-port`: Binary [ports](./ports.html).
  * `mmap`: [Memory mapped areas](./mmap.html).

Sockets:

  * `socket`: [sockets](./socket.html).
  * `datagram-socket`: [udp sockets](./socket.html).
  
Misc:

  * `date`: [dates](./date.html).
  * `process`: [processes](./process.html).

Threads:

  * `thread`: [threads](./thread.html).
  * `condvar`: [condition variables](./thread.html).
  * `mutex`: [mutexes](./thread.html).
  * `semaphore`: [semaphores](./thread.html).
  

Predefined Host Types
---------------------

  * `bool`: host boolean values.
  * `byte`, `ubyte`, `short`, `ushort`, `int`, `uint`, `long`, `ulong`: host integers.
  * `elong`, `uelong`: host `long` values.
  * `llong`, `ullong`: host `long long` values.
  * `int8`, `uint8`, `int16`, `uint16`, `int32`, `uint32`, `int64`, `uint64`: 
     fixed size host integers.
  * `float`, `double`: host floating point numbers.
  * `string`: host strings.
  * `void`: no value.

> [!IMPORTANT] As much a possible host types should be preferred to Bigloo types
> when addition annotations, with one exception: `bstring` should be prefered to
> `string`. This is because converting from a `string` to a `bstring` involves
> recopying the string itself. 
  
Type Conversions
----------------

Bigloo enables automatic conversions from some Bigloo types to some
Host types. The nature of these conversions depends on the 
target backend (see [C](./c.html), [jvm](./jvm.html), and [wasm](./wasm.html).
In addition supports type conversions from Bigloo values to another types:

  * _any value_ &rarr; `obj`: always permitted.
  * `obj` &rarr; _any value_: involves a dynamic check that may succeeds or trigger an exception.
  * _any value_ &rarr; `bbool`: always permitted.
  * `epair` &rarr; `pair`: always permitted.
  * `pair` &rarr;` `epair`: permitted after a dynamic type check.
  
Library Functions
-----------------

### typeof ###
Returns the dynamic type name of its argument.

> [!NOTE] As `typeof` argument is `obj`, a conversion to a Bigloo 
> value is required when invoking it. In consequence, the type names
> it reports are Bigloo type names resulting of this conversion.
