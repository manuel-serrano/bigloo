<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/struct.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Structs                                                       -->
<!--==================================================================-->

,(implementation-path "../runtime/Llib/struct.scm")
,(example-path "../test/src/struct.bgl")

Structures
==========

Bigloo structures are algebraic data types that serve the same role
a C struct.

Struct Declaration
------------------

### (define-struct name field...) ###
<!-- [:define-struct@NoDef] -->

This form defines a structure with name `name`, which is a symbol,
having fields `field`` which are symbols or lists, each
list being composed of a symbol and a default value. This form creates
several functions: creator, predicate, accessor and assigner functions. The
name of each function is built in the following way:


  * Creator: `make-`naem
  * Predicate: name`?`
  * Accessor: name`-`field
  * Assigner: name`-`field`-set!`


Function `make-`name accepts optional arguments. If a
single argument is provided, all the slots of the created structures
are filled with it. If more than one argument is passed, the various
values are used to initialize the corresponding structure slots. The
creator named `name` accepts as many arguments as the number of
slots of the structure. This function allocates a structure and fills
each of its slots with its corresponding argument.

If a structure is created using `make-`name and no initialization
value is provided, the slot default values (when provided) are used
to initialize the new structure. For instance, the execution of the program:

```bigloo
(define-struct pt1 a b)
(define-struct pt2 (h 4) (g 6))

(make-pt1)
   &rarr; #{PT1 () ()}
(make-pt1 5)
   &rarr; #{PT1 5 5}
(make-pt2)
   &rarr; #{PT2 4 6}
(make-pt2 5)
   &rarr; #{PT2 5 5}
```

<span></span>

Library Functions
-----------------

### struct? ###
Returns `#t` if and only if `obj` is a structure. Returns `#f` otherwise.

### make-struct ###
Creates a new `struct` object with `key` of lenght `len` and whose fields
are initialized with `init`.

### struct-key ###
Returns the `key` of the struct.

### struct-ref ###
Returns the `i` property of the structure.

### struct-set! ###
Sets the `i` property of the structure.

### struct-update! ###
Updates the key and all the property of `dst` with that of `src`. The
two structs are expected to be of the same size.

### struct->list ###
Converts a struct ito a list.

### list->struct ###
Converts a list into a struct. The first element of the list is the key
of the newly created struct and the other elements are the fields.
