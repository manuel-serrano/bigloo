<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Hashtables                                                    -->
<!--==================================================================-->

,(include "head.html")

,(implementation-path "../runtime/Llib/hash.scm")
,(example-path "../test/src/hash.bgl")


Hash Tables
===========

Bigloo offers various hash tables, some supporting weak pointers. Here
are described functions which define and use them.

Constructors
------------

### create-hashtable ###

Creates a nea hash table. The arguments are as follows:

  * `size`: the initial bucket size;
  * `max-bucket-len`: specifies when the table should be
     resized. If provided, these two values have to be exact integers greater or
     equal to 1. Normally you could ignore `size` and `max-bucket-len`
     arguments. 
  * `eqtest: when provided specifies of a comparison function. The first
     argument of this function is the keys contained in the table. The second
     argument is the searched key. By default,
     hash tables rely on `hashtable-equal?`, which is defined as:

```(define (hashtable-equal? obj1 obj2)
   (or (eq? obj1 obj2)
       (and (string? obj1)
            (string? obj2)
            (string=? obj1 obj2))))
```

  * `hash`: specifies an hashing function. It defaults to `get-hashnumber`.    
  * `weak`: might either be: `weak-keys`, `weak-data`, `weak-both`, or `open-string`. It
    specifies respectively whether the hash table should
    use weak pointers to store the keys and/or the data or if it should be an
    open ended string table (see below).  By default a
    hash table uses strong pointers for both keys and data.  
  * `max-length`: specifies a maximum length (in number of
    buckets) for this hashtable. It defaults to `16384`. If during the
    execution, the hashtable tries to expand itself more than
    `max-length`, an exception is raised. This feature helps debugging
    incorrect hashtable uses because excessive expansion is generally the
    signs of an incorrect behavior. Excessive expansions, cause the
    garbage collector to crash at some point. This debugging feature can
    be disabled by specifying a negative max length, in which case, no check
    is performed at runtime.
  * `bucket-expansion`: controls how `max-bucket-len` is
    expanded each time the table grows. This is a floating point number that
    is a multiplicative coefficient. It defaults to `1.2`.
    
Each optional arguments `size`, `max-bucket-len`, `eqtest`, `hash`,
`weak-keys`, and `weak-data` can be bound to the Bigloo value
`#unspecified` which forces its default.

> [!NOTE]
> Open-ended hashtables are significantly more efficient than any other sort of
> hashtables but they can be used only for keys that are strings. As much as 
> possible, they should be prefered to all the other sort of hashtables.

Predicates
----------

### hashtable? ###

Returns `#t` if `obj` is an hash table, constructed by
`make-hashtable`.


### hashtable-weak-keys? ###
Returns `#t` if `table` is a hash table with weakly pointed keys.


### hashtable-weak-data? ###
Returns `#t` if `table` is a hash table with weakly pointed data.


### hashtable-open-string? ###
Returns `#t` if `table` is a hash table is an open-string hashtale. 
Retiurn `#f` otherwis.


Library functions
-----------------

### hashtable-size ###

Returns the number of entries contained in `table`.
Note that for a weak hash table the size does not guarantee the real size,
since keys and/or data can dissapear before the next call to the hash table.


@deffn {bigloo procedure} hashtable-contains? table key
Returns the boolean `#t` if it exists at least one entry whose key 
is `key` in `table`. If not entry is found `#f` is returned.
Note that for a weak hash table, the fact this procedure returns `#t` 
does not guarantee that the key (or its associated data) will not dissapear
before the next call to the hash table.


@deffn {bigloo procedure} hashtable-get table key
Returns the entry whose key is `key` in `table`. If no entry
is found, or if the key and/or value is weakly pointed to and has dissapeard, 
`#f` is returned.


@deffn {bigloo procedure} hashtable-put! table key obj
Puts `obj` in `table` under the key `key`. This function 
returns the object bound in the table. If there was an object 
`obj-old` already in the table with the same key as `obj`, 
this function returns `obj-old`; otherwise it returns `obj`.


@deffn {bigloo procedure} hashtable-remove! table key
Removes the object associated to `key` from `table`, 
returning `#t` if such object
was bound in table and `#f` otherwise.


@deffn {bigloo procedure} hashtable-add! table key update-fun obj init-value
If key is already in table, the new value is calculated by
`(update-fun obj current-value)`. Otherwise the `table` is extended
by an entry linking key and `(update-fun obj init-value)`.


@deffn {deprecated bigloo procedure} hashtable-update! table key update-fun init-value
If key is already in table, the new value is calculated by
`(update-fun current-value)`. Otherwise the `table` is extended
by an entry linking key and `init-value`.


@deffn {bigloo procedure} hashtable->vector table
@deffnx {bigloo procedure} hashtable->list table
Returns the hash table `table`'s data as a vector (respectively a list). 
If the hash table is weak, the result will consist only of the data which 
haven't dissapeared yet and whose keys haven't dissapeared either.


@deffn {bigloo procedure} hashtable-key-list table
Returns the list of keys used in the `table`.
If the hash table is weak, the result will consist only of the keys which 
haven't dissapeared yet and whose data haven't dissapeared either.


@deffn {bigloo procedure} hashtable-map table fun
Returns a list whose elements are the result of applying `fun` to 
each of the keys and elements of `table` (no order is specified). In 
consequence, `fun` must be a procedure of two arguments. The first 
one is a key and the second one, an associated object.
If the hash table is weak, `fun` will only be mapped on sets of key/datum
which haven't dissapeared yet.


@deffn {bigloo procedure} hashtable-for-each table fun
Applies `fun` to each of the keys and elements of `table` 
(no order is specified). In consequence, `fun` must be a procedure
of two arguments. The first one is a key and the second one, an
associated object.
If the hash table is weak, `fun` will only be called on sets of key/datum
which haven't dissapeared yet.


@deffn {bigloo procedure} hashtable-map filter fun
Applies `fun` to each of the keys and elements of `table` 
(no order is specified). In consequence, `fun` must be a procedure
of two arguments. Returns a list of elements for which the predicate
`fun` evaluated to `#t`.


@deffn {bigloo procedure} hashtable-filter-map filter fun
Applies `fun` to each of the keys and elements of `table` 
(no order is specified). In consequence, `fun` must be a procedure
of two arguments. Returns a list of values produced by the call to `fun`
that did not return the value `#f`.


@deffn {bigloo procedure} hashtable-filter! table fun
Filter out elements from `table` according to predicate `fun`.
If the hash table is weak, `fun` will only be called on sets of key/datum
which haven't dissapeared yet.


@deffn {bigloo procedure} hashtable-clear! table
Remove all the elements from `table`.


Here is an example of hash table.

@smalllisp
(define *table* (make-hashtable))

(hashtable-put! *table* "toto" "tutu")
(hashtable-put! *table* "tata" "titi")
(hashtable-put! *table* "titi" 5)
(hashtable-put! *table* "tutu" 'tutu)
(hashtable-put! *table* 'foo 'foo)

(print (hashtable-get *table* "toto"))
   @print{} "tutu"
(print (hashtable-get *table* 'foo))
   @print{} 'foo
(print (hashtable-get *table* 'bar))
   @print{} #f

(hashtable-for-each *table* (lambda (key obj) (print (cons key obj))))
   @print{} ("toto" . "tutu")
      ("tata" . "titi")
      ("titi" . 5)
      ("tutu" . TUTU)
      (foo . foo)
@end smalllisp

@deffn {bigloo procedure} hashtable-collisions table
Returns a list of collisions for the keys from `table`.
A collision is represented by the number of extra steps (comparisons)
needed for a key. The length of the result gives the number of keys with
collisions, and the sum of all list elements is the sum of all extra
steps needed. This function can help to test different hash functions and
other hash table parameters.


@deffn {bigloo procedure} get-hashnumber obj
@deffnx {bigloo procedure} get-hashnumber-persistent obj

Computes a hash number of the value @var{obj}, which can be of any type.
The function `get-hashnumber-persistent` returns a hash number
that is persistent accross program executions and execution platforms.


@deffn {bigloo generic} object-hashnumber object
This generic function computes a hash number of the instance @var{object}.

Example:
@smalllisp
(define-method (object-hashnumber pt::point)
   (with-access::point pt (x y)
      (+fx (*fx x 10) y)))
@end smalllisp


@deffn {bigloo procedure} string-hash string [start 0] [len (string-length string)]
Compute a hash value for @var{string}, starting at index @var{start}, ending
at length @var{len}.



