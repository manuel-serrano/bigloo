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

,(implementation-path "../runtime/Ieee/symbol.scm")
,(implementation-path "../runtime/Unsafe/uuid.scm")
,(example-path "../test/src/symbol.bgl")
,(example-path "../test/src/char.bgl")

Symbols and Keywords
====================

Symbols and keywords are hashed literal that can be compare with the
`eq?` predicate (see the [booleans](bool.html) chapter). They are case
sensitive and the reader is case sensitive too. So:

```bigloo
(eq? 'foo 'FOO) &rarr; #f
(eq? foo: FOO:) &rarr; #f
(eq? foo: (string->keyword "foo")) &rarr; #t
(eq? foo: (string->keyword "Foo")) &rarr; #f
(eq? (string->symbol "foo") (string->symbol "FOO")) &rarr; #f
```

Symbols may contain special characters (such as `#\Newline` or
`#\Space`).  Such symbols that have to be read must be written:
`|[^]+|`. The function `write` uses that notation when it encounters
symbols containing special characters.

```bigloo
(write 'foo) &rarr; foo
(write 'Foo) &rarr;Foo
(write '|foo bar|) &rarr; |foo bar|
```

Keywords constitute an extension to Scheme introduced by the Dsssl
language. Keywords syntax is either `&lt;Ident&gt;:` or
`:&lt;Ident&gt;` or Keywords are auto-quoted, i.e., they don't need to
be prefixed with the quote character.

```bigloo
(eq? foo: :foo) &rarr; #t
```

Predicates
----------

### symbol? ###
Returns `#t` if and only if `obj` is a symbol.

### keyword? ###
Returns `#t` if and only if `obj` is a keyword.

### symbol-exists? ###
Returns `#t` if and only if a symbol named `name` exists.

Conversions
-----------

### symbol->string ###
Returns the name of the symbol as a fresh string. 

### symbol->string! ###
Returns the name of the symbol. Modifying the string result
of `symbol->string~` could yield incoherent programs.

### string->symbol ###
Creates or returns a symbol whose name is `string`.

### string->symbol-ci ###
Creates or returns a symbol whose name is the upcased `string`.

### keyword->string ###
Returns the name of the keyword.

### string->keyword ###
Creates or returns a keyword whose name is `string`.

### keyword->symbol ###
Converts a keyword into a symbol.

### symbol->keyword 
Converts a symbol into a keyword.

Library Functions
-----------------

### symbol-append ###
Returns a symbol whose name is concatenation of its argumets.

### gensym ###
Returns a new fresh symbol. If `obj` is provided and is a string or
a symbol, it is used as prefix for the new symbol.

### genuuid ###

Returns a string containing a new fresh 
[Universal Unique Identifier](http://fr.wikipedia.org/wiki/Universal_Unique_Identifier).

