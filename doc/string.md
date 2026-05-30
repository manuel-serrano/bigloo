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

,(implementation-path "../runtime/Ieee/string.scm")
,(implementation-path "../runtime/Ieee/char.scm")
,(implementation-path "../runtime/Llib/unicode.scm")
,(example-path "../test/src/string.bgl")
,(example-path "../test/src/char.bgl")
,(example-path "../test/src/unicode.bgl")

Strings
=======

Strings are sequences of 8-bit characters. They are mutable. They are
designated by the type `::bstring`. Native, backend depend strings,
are denoted by the type `::string`. The native implementation of the
native strings depends on the backend. 

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
    (the-encoded-substring 1 (-fx (the-length) 1) 'wasm)))
```

The global parameter `bigloo-string-encoding` is documented in
the [param](param.md) module. The form `regular-grammar` and the
function`the-encoded-string` are documented in [regular grammar](rgc.md).
The function `utf8-string->ucs2-string` is documented in
[unicode](unicode.md).

The type `::string` denotes host native strings of characters. Values
of types `::bstring` and `::string` are cast automatically. That is,
for instance, that is legal to invoke a function that accepts a `::string`
argument with a `::bstring` value.

The implementation of host `::string` depends on the backend, as does the
operation to cast a `::bstring` in a `::string` and vice-versa:

  * C: a `::string` is represented as a C `char *`. Casting from `::bstring`
  to `::string` is a mere pointer shift. Casting from `::string` to `::bstring`
  requires a whole copy of the characters. This is why the `::string` type
  should not be used in Bigloo code as it might implie many hidden copies
  of the sequence of characters.
   
  * Java: a `::string` is represented as a Java `byte []`. There is no 
  dynamic operation, i.e., no runtime code in a `::bstring` from and to
  a `::string`.
  
  * Wasm: a `::string` is an array of `i8` fixnums.  There is no 
  dynamic operation, i.e., no runtime code in a `::bstring` from and to
  a `::string`.
  
Summary:

|                            | C             | Java | Wasm |
| `::bstring` to `::string`  | pointer shift |  _   |   _  |
| `::string` to `::bstring`  | copy          |  _   |   _  |


String Constructors
-------------------

### make-string ###

Creates a fresh string of size `k` initialized with `char`, which defaults
to `#\space`.


### string ###

Creates a fresh string of size `(length chars)` and initilized with the
optional arguments, that must all be characters.

### substring ###

Returns a substring of `str`, which must be a string, and `start` and `end` must be exact integers satisfying: `0` &le; `start` &le; `end` &le; `(string-length str)`.
The optional argument `end` defaults to `(string-length str)`.

The function `substring` returns a newly allocated string formed from the
characters of `str` beginning with index `start` (inclusive) 
and ending with index `end` (exclusive).

### string-append ###

Creates a new string by appending all its arguments that must all be
strings.

### string-copy ###

Creates a fresh copy of `string`.

String Predicates
-----------------

### string? ###

Returns `#t` if and only if `obj` is a `string`. Returns `#f` otherwise.


### string-null? ###

Returns `#t` if and only if `obj` is a empty string. Returns `#f` otherwise.


### empty-string? ###

Return `#t` if and only if `string` is the empty string. Returns `#f` otherwise.


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

String Getters and Setters
--------------------------

### string-length ###

Returns the size of the `string`.


### string-ref ###

Returns the character at position `k` in `string`. If the argument 
`k` is not in the range `0` &le; `k` &lt; `string.length`, an exception is 
triggered.


### string-set! ###

Sets the character at position `k` of `string`. If the argument 
`k` is not in the range `0` &le; `k` &lt; `string.length`, an exception is 
triggered.


String Library Functions
------------------------

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

Replaces all the occurrences of `c1` by `c2` in `str`.
Returns a newly allocated string.

### string-replace! ###

Replaces all the occurrences of `c1` by `c2` in `str`.
Modifies its first argument.


### string-split ###

Parses `string` and returns a list of tokens ended by a character of the 
`delimiters` string. If `delimiters` is omitted, it defaults to a 
string containing a space, a tabulation and a newline characters.


### string-cut ###

The function `string-cut` behaves as `string-split` but it 
introduces empty strings for consecutive occurrences of delimiters.

### string-delete ###

Filters the string `str`, retaining only those characters that
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


### string-hex-intern ###

Converts an hexadecimal `string` of `n` characters into an actual 
string of `n/2` characters. 

### string-hex-intern! ###

As `string-hex-intern` but might returns its modified argument.

### string-hex-extern ###

Converts `str` into an hexadecimal representation.

The arguments `start` and `end` must be exact integers satisfying:
  0 &le; `start` &le; `end` &le; `(string-length str)`.
  

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

Unicode Strings
===============

Unicode String Predicates
-------------------------

### ascii-string? ###
Returns `#t` if and only if the argument `string` is only composed
of ascii characters. Otherwise returns `#f`.

### utf8-string? ###
Returns `#t` if and only if the argument `string` is a well formed
UTF-8 string. Otherwise returns `#f`.

If the optional argument `strict` is `#t`, half utf16-surrogates are
rejected otherwise they are accepted. The optional argument `strict`
defaults to `#f`.

Unicode String Standard Library
-------------------------------

### string-minimal-charset ###
Computes the minimal charset capable of representing that string.

### utf8-string-minimal-charset ###
Computes the minimal charset capable of representing that utf-8 string.

### utf8-char-size ###
Returns the utf8 encoding size of a 8-bit character.

### utf8-string-length ###
Returns the number of characters of an UTF-8 string. It raises an error
if the string is not a well formed UTF-8 string (i.e., it does satisfies
the `utf8-string?` predicate.

### utf8-string-encode ###
Returns a copy of `string` where all the illegal UTF-8 prefix are
replaced with the Unicode Replacement Character `EF BF BD`. The result
is a well formed UTF-8 string.

### utf8-string-ref ###
Returns the character (represented as a UTF-8 string) at the position
`i` in `string`.

### utf8-string-index->string-index ###
Return the index of the `i`-character of the UTF-8 string `str`.

### utf8-substring ###
The function `utf8-substring` returns a newly allocated string formed from the
characters of `string` beginning with index `start` (inclusive) 
and ending with index `end` (exclusive).

If the argument `string` is not a well formed UTF-8 string an error
is raised. Otherwise, the result is also a well formed UTF-8 string.

The arguments `start` and `end` must satisfy 
0 &le; `start` &le; `end` &le; `(utf8-string-length string)`.

### utf8-string-append ###
Appends two UTF-8 strings.

### utf8-string-append* ###
Appends many UTF-8 strings.

### utf8-string-append-fill! ###
Append the left `string` into the `buffer` at position
`index`. This function handles cases where the last char of the
concatanated char is a UNICODE remplacement char. The optional argument
`offset` is the byte offset into `string`.

### utf8->iso-latin ###
Converts the UTF-8 string `str` into a corresponding 8-bit ones, 
with the ISO-LATIN-1 encoding.

### utf8->iso-latin! ###
Converts the UTF-8 string `str` into a corresponding 8-bit ones, 
with the ISO-LATIN-1 encoding. If `str` requires no conversion, it returns it.

### utf8->iso-latin-15 ###
Converts the UTF-8 string `str` into a corresponding 8-bit ones, 
with the ISO-LATIN-15 encoding.

### utf8->iso-latin-15! ###
Converts the UTF-8 string `str` into a corresponding 8-bit ones, 
with the ISO-LATIN-15 encoding. If `str` requires no conversion, it returns it.

### utf8->cp1252 ###
Converts the UTF-8 string `str` into a corresponding 8-bit ones, 
with the CP1252 encoding.

### utf8->cp1252! ###
Converts the UTF-8 string `str` into a corresponding 8-bit ones, 
with the CP1215 encoding. If `str` requires no conversion, it returns it.

### iso-latin->utf8 ###
Converts the ISO-LATIN-1 8-bit string encoded into UTF-8 string.

### iso-latin->utf8! ###
Converts the ISO-LATIN-1 8-bit string encoded into UTF-8 string. If
`str` requires no conversion, it returns it.

### iso-latin-15->utf8 ###
Converts the ISO-LATIN-15 8-bit string encoded into UTF-8 string.

### iso-latin->utf8! ###
Converts the ISO-LATIN-15 8-bit string encoded into UTF-8 string.
If `str` requires no conversion, it returns it.

### cp1252->utf8 ###
Converts the CP1252 8-bit string encoded into UTF-8 string.

### cp1252->utf8! ###
Converts the CP1252 8-bit string encoded into UTF-8 string.
If `str` requires no conversion, it returns it.


Characters
==========

Bigloo knows named characters `#\alarm`, `#\backspace`,
`#\delete`, `#\escape`, `#\tab`, `#\return`, and
`#\null` in addition to the `#\space and `#\newline1
of Scheme [R5RS](https://r5rs.html).

A new alternate syntax exists for characters:
  `#a<ascii-code>`
  
where `&lt;ascii-code&gt;` is the three digit decimal ASCII number
of the character to be read. Thus, for instance, the character 
`#\space`
can be written `#a032`. Bigloo also supports the [R7Rs](https://r7rs.html)
syntax `#\x<hex-code>`.

Character Predicates
--------------------

### char? ###
Returns `#t` if and only if `obj` is a character. Returns `#f` otherwise.

### char=? ###
Returns `#t` if and only if `char1` and `char2` are the same character.
Returns `#f` otherwise.

### char<? ###
Returns `#t` if and only if `char1` is smaller than `char2` in the
lexicographique order. Returns `#f` otherwise.

### char>? ###
Returns `#t` if and only if `char1` is greater than `char2` in the
lexicographique order. Returns `#f` otherwise.

### char<=? ###
Returns `#t` if and only if `char1` is smaller than or equal to `char2` in the
lexicographique order. Returns `#f` otherwise.

### char>=? ###
Returns `#t` if and only if `char1` is greater than or equal to `char2` in the
lexicographique order. Returns `#f` otherwise.

### char-ci=? ###
As `char?` but case-insensitive.

### char-ci<? ###
As `char<?` but case-insensitive.

### char-ci>? ###
As `char>?` but case-insensitive.

### char-ci<=? ###
As `char<=?` but case-insensitive.

### char-ci>=? ###
As `char>-?` but case-insensitive.

### char-alphabetic? ###
Returns `#t` if and only if `char` is a letter is the roman alphabet. 
Returns `#f` otherwise.

### char-numeric? ###
Returns `#t` if and only if `char` is a letter is the arabic digit.
Returns `#f` otherwise.

### char-whitespace? ###
Returns `#t` if and only if `char` is a word separator.
Returns `#f` otherwise.

### char-upper-case? ###
Returns `#t` if and only if `char` is a roman upper case letter.
Return `#f` otherwise.

### char-lower-case? ###
Returns `#t` if and only if `char` is a roman lower case letter.
Return `#f` otherwise.

Character Library Functions
---------------------------

### char-upcase ###
Returns the corresponding upper case letter in the roman alphabet. 

### char-downcase ###
Returns the corresponding lower case letter in the roman alphabet. 

### char->integer ###
Returns the 8-bit integer interpretation of the character.

### integer->char ###
Returns the character associated to the 8-bit integer representation.


