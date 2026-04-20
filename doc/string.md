<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Strings                                                       -->
<!--==================================================================-->

,(include "head.html")

,(implementation-path "../runtime/Ieee/string.scm")
,(example-path "../test/string.bgl")

Strings
=======

The syntax of string literals is:

```scheme
(regular-grammar ()
   ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
    ;; regular Scheme strings
    (the-encoded-substring 1 (-fx (the-length) 1) (bigloo-string-encoding)))
   ((: "#\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
    ;; c strings
    (the-encoded-substring 1 (-fx (the-length) 1) 'bigloo))
   ((: "#w\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
    ;; wasm strings
    (the-encoded-substring 1 (-fx (the-length) 1) 'wasm))
   ((: "#u\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
    ;; utf8 strings
    (let ((str (the-encoded-substring 2 (-fx (the-length) 1) 'bigloo)))
      (utf8-string->ucs2-string str))))
```

The global parameter `bigloo-string-encoding` is documented in
the [param](param.md) module. The form `regular-grammar` and the
function`the-encoded-string` are documented in [regular grammar](rgc.md).
The function `utf8-string->ucs2-string` is documented in
[unicode](unicode.md).


Constructors
------------

### make-string ###

Creates a fresh string of size `k` initialized with `char`, which defaults
to `#\space`.


### string ###

Creates a fresh string of size `(length chars)` and initilized with the
optional arguments, that must all be characters.

### substring ###

Returns a substring of `string`, which must be a string, and `start` and `end` must be exact integers satisfying: `0 &le; `start` &le; `end` &le; `(string-length string)`.

The optional argument `end` defaults to `(string-length string)`.

The function `substring` returns a newly allocated string formed from the
characters of `string` beginning with index `start` (inclusive) 
and ending with index `end` (exclusive).

### string-append ###

Creates a new string by appending all its arguments that must all be
strings.

### string-copy ###

Creates a fresh copy of `string`.

Predicates
----------

### string? ###

Returns `#t` if and only if `obj` is a `string`. Returns `#f` otherwise.


### string-null? ###

Returns `#t` if and only if `obj` is a empty string. Returns `#f` otherwise.


### empty-string? ###

Return `#t' if and only if `string' is the empty string. Returns `#f' otherwise.


### string=? ###

Returns `#t` if `string1` and `string2` are of the the same characters.
It returns `#f` otherwise.

### string-ci=? ###

Behaves as `string=?` but it is case insenstive.

### substring=? ###

This function returns `#t` if `string1` and `string2` have a
common prefix of size `len`.

### substring-ci=? ###

Behaves as `substring=?` but it is case insenstivie.

### substring-at? ###

Returns `#t` if the string `string2` is at position `off` in the string
`string1`, considering at most `len` character. It returns `#f` otherwise.

### substring-ci-at? ###

Behaves as `substring-at?` but it is case insensitive.

### string<? ###

Returns `#t` is `string1` is smaller than `string2`. Returns `#f` otherwise.

### string<=? ###

Returns `#t` is `string1` is smaller than or equal to `string2`. Returns
`#f` otherwise.

### string>? ###

Returns `#t` is `string1` is bigger than `string2`. Returns `#f` otherwise.

### string>=? ###

Returns `#t` is `string1` is bigger than or equal to `string2`. Returns
`#f` otherwise.

### string-ci<? ###

Behaves as `string<?` but it is case insensitive.

### string-ci<=? ###

Behaves as `string<=?` but it is case insensitive.

### string-ci>? ###

Behaves as `string>?` but it is case insensitive.

### string-ci>=? ###

Behaves as `string>=?` but it is case insensitive.


Getters and setters
-------------------

### string-length ###

Returns the size of the `string`.


### string-ref ###

Returns the character at position `k` in `string`. If the argument 
`k` is not in the range `0 &le; k &lt; string.length`, an exception is 
triggered.


### string-set! ###

Sets the character at position `k` of `string`. If the argument 
`k` is not in the range `0 &le; k &lt; string.length`, an exception is 
triggered.


Library functions
-----------------

All the functions that uses case-insenstive comparison implement 
the tests with the operation:

```
(char-downcase (char-upcase c))
```

### string-index ###

Returns the first occurrence of a character `rs` in
`string`. The argument `rs` is either a character or a string.
If no character is found, it returns `#f`.
The optional argument `start`, is the start position of the scan.


### string-index-right ###

As `string-index` but search from right to left.


### string-char-index ###

Returns the first occurrence of the character `char` in `string`. If
no character is found, it returns `#f` The optional
argument `start`, is the start position of the scan. The optional
argument `count`, is the number of characters to be scanned in `string`.


### string-skip ###

Searches through the `string from the left  and 
returns the index of the first occurrence of a character rs which:

   * is not equal to `rs` (if `rs` is a character);
   * is not in `rs` (if `rs` is a character set);
   * does not satisfy the predicate `rs` (if `rs` is a procedure). 

If no such index exists, the functions return `#f`.

The start and end parameters specify the beginning and end indices of
the search; the search includes the start index, but not the end
index. Be careful of "fencepost" considerations: when searching
right-to-left, the first index considered is end-1 whereas when
searching left-to-right, the first index considered is start. That is,
the start/end indices describe a same half-open interval `start` &ge; i &gt; end.

### string-skip-right ###

Behaves as `string-skip` but searches from right to left.


### string-contains ###

Does string `s1` contain string `s2`? Return the index in `s1` where
`s2` occurs first as a substring, or `#f`.

### string-contains-ci ###

As `string-contains` but case-insenstive.

### string-compare3 ###

This function compares `a` and `b`. It returns a negative integer if
`a` < `b`. It returns zero if the `a` equal `b`. It returns a positive 
integer if `a` > `b`.


### string-compare3-ci ###

As `string-compare3` but uses case-insensitive comparisons.


### string-natural-compare3 ###

This function compares `a` and `b` according to a _natural string order_. 
It returns a negative integer if `a` < `b`. It returns zero if
the `a` equal `b`. It returns a positive integer if `a` > `b`.

### string-natural-compare3-ci ###

As `string-natural-compare3` but uses case-insensitive comparisons.

### string-shrink! ###

Returns a `string` whose length is at most `end`. The function may or may not
create a fresh string.

The argument `end` must be an exact integer satifying: 0 &ge; `end` &ge; `(string-length string).

The function `string-shrink!` returns a new string formed from the characters
of `string` beginning with index 0 (inclusive) and ending with
index `end` (exclusive). As much as possible `string-shrink!`
changes the argument `string`. That is, as much as possible, and
for the back-ends that enable it, `string-shrink!` operates a side
effect on its argument.

### string->list ###

Converts its arguments into a list of characters.


### list->string ###

Creates a fresh string from a list of characters.


### string-fill! ###

Stores `char` in every element of the given `string` and returns the
`#unspecified` value.


### string-downcase ###

Returns a newly allocated version of `string` where each upper case
letter is replaced by its lower case equivalent.

### string-upcase ###

Returns a newly allocated version of `string` where each lower case
letter is replaced by its upper case equivalent.

### string-capitalize ###

Returns a newly allocated capitalized version of `string`.


### string-downcase! ###

Physically downcases the `string` argument.

### string-upcase! ###

Physically upcases the `string` argument.

### string-capitalize! ###

Physically capitalized the `string` argument.

### string-for-read ###

Returns a copy of `string` with each special character
replaced by an escape sequence.

### blit-string! ###

Fills string `string2` starting at position `o2` with
`len` characters taken out of string `string1` from
position `o1`.

### string-replace ###

Replaces all the occurrences of `char1` by `char2` in `string`.
Returns a newly allocated string.

### string-replace! ###

Replaces all the occurrences of `char1` by `char2` in `string`.
Modifies its first argument.


### string-split ###

Parses `string` and returns a list of tokens ended by a character of the 
`delimiters` string. If `delimiters` is omitted, it defaults to a 
string containing a space, a tabulation and a newline characters.


### string-cut ###

The function `string-cut` behaves as `string-split` but it 
introduces empty strings for consecutive occurrences of delimiters.

### string-delete ###

Filters the string `string`, retaining only those characters that
are not equal to `obj`, not present in `obj`, or not
satisfying the predicate `obj`. This function returns a fresh string no larger
than `end` - `start.

### string-prefix-length ###

Returns the length of the longest common prefix of the two
strings `s1` and `s2`. For prefixes, this is equivalent to the
"mismatch index" for the strings (modulo the starti index offsets).

The optional `start`/`end` indices restrict the comparison to the
indicated substrings of `s1` and `s2`.

### string-prefix-length-ci ###

As `string-prefix-length` but uses case-insenstivie comparisons.


### string-suffix-length ###

Returns the length of the longest common suffix of the two
strings `s1` and `s2`. For suffixes, this is equivalent to the
"mismatch index" for the strings (modulo the starti index offsets).

The optional `start`/`end` indices restrict the comparison to the
indicated substrings of `s1` and `s2`.

### string-suffix-length-ci ###

As `string-suffix-length` but uses case-insenstivie comparisons.


### string-prefix? ###

Is `s1` a prefix of `s2`? The optional `start`/`end` indices restrict
the comparison to the indicated substrings of `s1` and `s2`.

### string-prefix-ci? ###

As `string-prefix` but case-insenstive.

### string-suffix? ###

Is `s1` a suffix of `s2`? The optional `start`/`end` indices restrict
the comparison to the indicated substrings of `s1` and `s2`.

### string-suffix-ci? ###

As `string-suffix` but case-insenstive.


### string-hex-intern ###

Converts an hexadecimal `string` of `n` characters into an actual 
string of `n/2` characters. 

### string-hex-intern! ###

As `string-hex-intern` but might returns its modified argument.

### string-hex-extern ###

Converts a `string` into a hexadecimal representation.

The arguments `start` and `end` must be exact integers satisfying:
  0 &le; `start` &le; `end` &lt; `(string-length str)`.
  

### string-trim-both ###

Trim `s` by skipping over all characters on the 
both sides that satisfy the second parameter char/char-set/pred:

   * if `opt` is a character char, characters equal to char are trimmed;
   * if `opt` is a char set cs, characters contained in cs are trimmed;
   * if `opt` is a predicate pred, it is a test predicate that is applied
     to the characters in s; a character causing it to return true is skipped.

The optional argument `opt` defaults to the character set
`char-set:whitespace` defined in SRFI 14.

If no trimming occurs, these functions may returns a copy of
`s`.

### string-trim ###

As `string-trim-both` but only trims characters at the left of `s`.

### string-trim-right ###

As `string-trim-both` but only trims characters at the right of `s`.

