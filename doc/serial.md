<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Serialization                                                 -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/intext.scm")
,(implementation-path "../runtime/Llib/binary.scm")
,(example-path "../test/src/serial.bgl")

Serialization
=============

Standard Serialization
----------------------

### obj->string ###
This function converts into a string `any` Bigloo object
which does not contain a procedure. 

### string->obj ###

This function converts a `string` which has been produced by
`obj->string` into a Bigloo object.

Custom Serialization
--------------------

### register-procedure-serialization! ###
There is no existing portable method to dump and restore a
procedure. Thus, if `obj->string` is passed a procedure, it emits an
error message.  However, it may, sometimes, be convenient to use an
ad-hoc framework to serialize and unserialize procedures. User may
specify there own procedure serializer and unserializer. This is the
role of `register-procedure-serialization!`. The argument `serializer`
is a procedure of one argument, converting a procedure into a
characters strings. The argument `unserializer` is a procedure of one
argument, converting a characters string into a procedure. It belongs
to the user to provide correct serializer and unserializer.

### get-procedure-serialization ###
Returns two values, the registered procedure serializer and unserializer.

### register-class-serialization! ###
Object (class instances) can be serialized and unserialized as any other
values but sometimes it is convenient or required to use specialized
serializers/unserializers for some classes. This is made possible
by the `register-class-serialization!` and `get-class-serialization`
functions.

The function `register-class-serialization!` registers a serializer
and an unserializer for a class. Subclasses of `class` inherit this
serializer.

### get-class-serialization ###
Returns two values, the registered class serializer and unserializer.

I/O
---

### input-obj ###
Deserializes a Bigloo value (possibly cyclic) from a binary `port`.
See chapter [Ports](./port.html).


### output-obj ###
Serializes a Bigloo value (possibly cyclic) to a binary `port`.
See chapter [Ports](./port.html).
