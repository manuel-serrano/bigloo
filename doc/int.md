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
,(example-path "../test/src/bit.bgl")

Integers
========

Bigloo supports several different integers, each represented by their
own types:

  * `bint`: Bigloo integers. Their size depends on the host C integer
  size. On 32-bit platforms, `bint` are 30-bit wide. On 64-bit platforms,
  they are 61-bit wide.


  
Bit Manipulation
----------------
