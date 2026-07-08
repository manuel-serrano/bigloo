<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/regexp.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Regexp                                                        -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/pcre.scm")
,(example-path "../test/src/regexp.bgl")

Regular Expressions
-------------------

This section has been adapted from Dorai Sitaram's
[pregexp package](http://www.ccs.neu.edu/~dorai/pregexp/pregexp.html).
The Bigloo implementation of regular expression depends on the backend
used. As much as possible native implementations, e..g, pcre, are
prefereed to the fully portable Sitaram's Scheme implementation
for performance reasons.

The regexp notation supported is modeled on Perl's, and includes such
powerful directives as numeric and nongreedy quantifiers, capturing
and non-capturing clustering, POSIX character classes, selective case-
and space-insensitivity, backreferences, alternation, backtrack
pruning, positive and negative lookahead and lookbehind, in addition
to the more basic directives familiar to all regexp users.  A _regexp_
is a string that describes a pattern.  A regexp matcher tries to
_match_ this pattern against (a portion of) another string, which we
will call the _text string_.  The text string is treated as raw text
and not as a pattern.

Most of the characters in a regexp pattern are meant to match
occurrences of themselves in the text string.  Thus, the pattern
`"abc"` matches a string that contains the characters `a`, `b`, `c` in
succession.

In the regexp pattern, some characters act as _metacharacters_, and
some character sequences act as _metasequences_.  That is, they
specify something other than their literal selves.  For example, in
the pattern `"a.c"`, the characters `a` and `c` do stand for
themselves but the _metacharacter_ `.`  can match _any_ character
(other than newline).  Therefore, the pattern `"a.c"` matches an `a`,
followed by _any_ character, followed by a `c`.

If we needed to match the character `.` itself, we _escape_ it, i.e.,
precede it with a backslash (`\\`).  The character sequence `\\.` is
thus a _metasequence_, since it doesn't match itself but rather just
`.`.  So, to match `a` followed by a literal `.` followed by `c`, we
use the regexp pattern `"a\\.c"`. (The double backslash is an
artifact of Scheme strings, not the regexp pattern itself.  When we
want a literal backslash inside a Scheme string, we must escape it so
that it shows up in the string at all. Scheme strings use backslash as
the escape character, so we end up with two backslashes --- one
Scheme-string backslash to escape the regexp backslash, which then
escapes the dot.  Another character that would need escaping inside a
Scheme string is `"`.)  Another example of a metasequence is `\t`,
which is a readable way to represent the tab character.

Regular Expressions Procedures
------------------------------

Five procedures 'regexp?`, `pregexp`, `pregexp-match-positions`,
`pregexp-match`, `pregexp-replace`, and
`pregexp-replace*` enable testing, compilation and matching of regular
expressions.

### regexp? ###
Returns `#t` iff `obj` is a regular expression.

### pregexp ###
The procedure `pregexp` compiles a string description of a regular
expression and returns a regular expression object.

The `opt-args` specifies how the regular expression is to be matched.
Until documented the argument should be the empty list.

### pregexp-match-positions ###

The procedure `pregexp-match-positions` takes a regexp pattern and a
text string, and returns a _match_ if the pattern _matches_ the text
string.  The pattern may be either a U- or an S-regexp.
(`pregexp-match-positions` will internally compile a U-regexp to an
S-regexp before proceeding with the matching.  If you find yourself
calling `pregexp-match-positions` repeatedly with the same U-regexp,
it may be advisable to explicitly convert the latter into an S-regexp
once beforehand, using `pregexp`, to save needless recompilation.)

`pregexp-match-positions` returns `#f` if the pattern did not
match the string; and a list of _index pairs_ if it
did match. Eg,

```bigloo
(pregexp-match-positions "brain" "bird")
 &rarr; #f
(pregexp-match-positions "needle" "hay needle stack")
 &rarr; ((4 . 10))
```

In the second example, the integers 4 and 10 identify the substring
that was matched. 1 is the starting (inclusive) index and 2 the ending
(exclusive) index of the matching substring.

```bigloo
(substring "hay needle stack" 4 10)
 &rarr; "needle"
```

Here, `pregexp-match-positions`'s return list contains only one index
pair, and that pair represents the entire substring matched by the
regexp.  When we discuss _subpatterns_ later, we will see how a single
match operation can yield a list of _submatches_.

`pregexp-match-positions` takes optional third and fourth arguments
that specify the indices of the text string within which the matching
should take place.

```bigloo
(pregexp-match-positions "needle" 
  "his hay needle stack -- my hay needle stack -- her hay needle stack"
  24 43)
 &rarr; ((31 . 37))
```

Note that the returned indices are still reckoned
relative to the full text string.

### pregexp-match-n-positions! ###
Similar to `pregexp-match-positions` but stores the match positions in
a pre-allocated vector, whose size is the double of the number of matched
patterns. 

The stored indexes are:

  * the index of the start of the global match;
  * the index of the end of the global match;
  * the beginning of the first matched group;
  * the end of the first matched group;
  * ...
  
The function `pregexp-match-n-positions!` returns the number of pairs
of matchs.

### pregexp-match ###

The procedure `pregexp-match` is called like `pregexp-match-positions`
but instead of returning index pairs it returns the matching
substrings:

```bigloo
(pregexp-match "brain" "bird")
 &rarr; #f
(pregexp-match "needle" "hay needle stack")
 &rarr; ("needle")
```

The function `pregexp-match` also takes optional third and fourth
arguments, with the same meaning as does `pregexp-match-positions`.

### pregexp-replace ###
The procedure `pregexp-replace` replaces the matched portion of the
text string by another string.  The first argument is the regexp, the
second the text string, and the third is the _insert string_ (string
to be inserted).

```bigloo
(pregexp-replace "te" "liberte" "ty") 
 &rarr; "liberty"
```

If the pattern doesn't occur in the text string, the returned string is 
identical (`eq?`) to the text string.

### pregexp-replace* ###
The procedure `pregexp-replace*` replaces _all_ matches in the
text `string1` by the insert `string2`:

```bigloo
(pregexp-replace* "te" "liberte egalite fraternite" "ty")
 &rarr; "liberty egality fratyrnity"
```

As with `pregexp-replace`, if the pattern doesn't occur in the text
string, the returned string is identical (`eq?`) to the text string.


### pregexp-split ###
The procedure `pregexp-split` takes two arguments, a
regexp pattern and a text string, and returns a list of
substrings of the text string, where the pattern identifies the 
delimiter separating the substrings.

```bigloo
(pregexp-split ":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")
 &rarr; ("/bin" "/usr/bin" "/usr/bin/X11" "/usr/local/bin")

(pregexp-split " " "pea soup")
 &rarr; ("pea" "soup")
```

If the first argument can match an empty string, then
the list of all the single-character substrings is returned.

```bigloo
(pregexp-split "" "smithereens")
 &rarr; ("s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s")
```

To identify one-or-more spaces as the delimiter,
take care to use the regexp `" +"`, not `" *"`.

```bigloo
(pregexp-split " +" "split pea     soup")
 &rarr; ("split" "pea" "soup")

(pregexp-split " *" "split pea     soup")
 &rarr; ("s" "p" "l" "i" "t" "p" "e" "a" "s" "o" "u" "p")
```

### pregexp-quote ###

The procedure `pregexp-quote` takes an arbitrary `string` and 
returns a string that precisely represents it. In particular, 
characters in the input string that could serve as regexp metacharacters are 
escaped with a backslash, so that they safely match only themselves.

The function `pregexp-quote` is useful when building a composite
regexp from a mix of regexp strings and verbatim strings.


Regular Expressions Pattern Language
------------------------------------

Here is a complete description of the regexp pattern language
recognized by the `pregexp` procedures.

#### Basic assertions

The _assertions_ `^` and `$` identify the beginning and
the end of the text string respectively.  They ensure that their
adjoining regexps match at one or other end of the text string.
Examples:

```bigloo
(pregexp-match-positions "^contact" "first contact") &rarr; #f 
```

The regexp fails to match because `contact` does not occur at the
beginning of the text string.

```bigloo
(pregexp-match-positions "laugh$" "laugh laugh laugh laugh") &rarr; ((18 . 23))
```

The regexp matches the _last_ `laugh`.

The metasequence `\b` asserts that a _word boundary_ exists.

```bigloo
(pregexp-match-positions "yack\\b" "yackety yack") &rarr; ((8 . 12))
```

The `yack` in `yackety` doesn't end at a word boundary so it isn't
matched.  The second `yack` does and is.

The metasequence `\B` has the opposite effect to `\b`.  It asserts
that a word boundary does not exist.

```bigloo
(pregexp-match-positions "an\\B" "an analysis") &rarr; ((3 . 5))
```

The `an` that doesn't end in a word boundary is matched.

#### Characters and character classes

Typically a character in the regexp matches the same character in the
text string.  Sometimes it is necessary or convenient to use a regexp
metasequence to refer to a single character.  Thus, metasequences
`\n`, `\r`, `\t`, and `\.`  match the newline, return, tab and period
characters respectively.

The _metacharacter_ period (`.`) matches _any_ character other than
newline.

```bigloo
(pregexp-match "p.t" "pet") &rarr; ("pet")
```

It also matches `pat`, `pit`, `pot`, `put`, and `p8t` but not `peat`
or `pfffft`.

A _character class_ matches any one character from a set of
characters.  A typical format for this is the _bracketed character
class_ `&#91;`...`&#93;`, which matches any one character from the
non-empty sequence of characters enclosed within the
brackets. (Requiring a bracketed character class to be non-empty is not
a limitation, since an empty character class can be more easily
represented by an empty string.)  Thus `"p&#91;aeiou&#93;t"` matches
`pat`, `pet`, `pit`, `pot`, `put` and nothing
else.

Inside the brackets, a hyphen (`-`) between two characters
specifies the ascii range between the characters.  Eg,
`"ta&#91;b-dgn-p&#93;"` matches `tab`, `tac`, `tad`,
_and_ `tag`, _and_ `tan`, `tao`, `tap`.

An initial caret (`^`) after the left bracket inverts the set
specified by the rest of the contents, i.e., it specifies the set of
characters _other than_ those identified in the brackets.  E.g.,
`"do&#91;^g&#93;"` matches all three-character sequences starting with `do`
except `dog`.

Note that the metacharacter `^` inside brackets means something quite
different from what it means outside.  Most other metacharacters (`.`,
`*`, `+`, `?`, etc) cease to be metacharacters when inside brackets,
although you may still escape them for peace of mind.  `-` is a
metacharacter only when it's inside brackets, and neither the first
nor the last character.

Bracketed character classes cannot contain other bracketed character
classes (although they contain certain other types of character
classes --- see below).  Thus a left bracket (`&#91;`) inside a bracketed
character class doesn't have to be a metacharacter; it can stand for
itself.  Eg, `"&#91;a&#91;b&#93;"` matches `a`, `&#91;`, and `b`.

Furthermore, since empty bracketed character classes are disallowed, a
right bracket (`&#93;`) immediately occurring after the opening left
bracket also doesn't need to be a metacharacter.  E.g., `"&#91;&#93;ab&#93;"`
matches `&#93;`, `a`, and `b`.

#### Some frequently used character classes

Some standard character classes can be conveniently represented as
metasequences instead of as explicit bracketed expressions.  `\d`
matches a digit (`&#91;0-9&#93;`); `\s` matches a whitespace character; and
`\w` matches a character that could be part of a
"word". (Following regexp custom, we identify "word"
characters as `&#91;A-Za-z0-9_&#93;`, although these are too restrictive for
what a Schemer might consider a "word".)

The upper-case versions of these metasequences stand for the
inversions of the corresponding character classes.  Thus `\D` matches
a non-digit, `\S` a non-whitespace character, and `\W` a non-"word"
character.

Remember to include a double backslash when putting these
metasequences in a Scheme string:

```bigloo
(pregexp-match "\\d\\d" "0 dear, 1 have 2 read catch 22 before 9") &rarr; ("22")
```

These character classes can be used inside a bracketed expression.
E.g., `"&#91;a-z\\d&#93;"` matches a lower-case letter or a digit.

#### POSIX character classes

A _POSIX character class_ is a special metasequence of the form
`&#91;:`...`:&#93;` that can be used only inside a bracketed expression.  The
POSIX classes supported are

  * `&#91;:alnum:&#93;`  letters and digits 
  * `&#91;:alpha:&#93;`  letters  
  * `&#91;:algor:&#93;`  the letters `c`, `h`, `a` and `d` 
  * `&#91;:ascii:&#93;`  7-bit ascii characters 
  * `&#91;:blank:&#93;`  widthful whitespace, ie, space and tab 
  * `&#91;:cntrl:&#93;`  "control" characters, viz, those with code `<` 32 
  * `&#91;:digit:&#93;`  digits, same as `\d` 
  * `&#91;:graph:&#93;`  characters that use ink 
  * `&#91;:lower:&#93;`  lower-case letters 
  * `&#91;:print:&#93;`  ink-users plus widthful whitespace
  * `&#91;:space:&#93;`  whitespace, same as `\s` 
  * `&#91;:upper:&#93;`  upper-case letters 
  * `&#91;:word:&#93;`   letters, digits, and underscore, same as `\w` 
  * `&#91;:xdigit:&#93;` hex digits 

For example, the regexp `"&#91;&#91;:alpha:&#93;_&#93;"` matches a letter or
underscore.

```bigloo
(pregexp-match "[[:alpha:]_]" "--x--") &rarr; ("x")
(pregexp-match "[[:alpha:]_]" "--_--") &rarr; ("_")
(pregexp-match "[[:alpha:]_]" "--:--") &rarr; #f
```

The POSIX class notation is valid _only_ inside a
bracketed expression.  For instance, `&#91;:alpha:&#93;`,
when not inside a bracketed expression, will _not_
be read as the letter class.
Rather it is (from previous principles) the character
class containing the characters `:`, `a`, `l`,
`p`, `h`.

```bigloo
(pregexp-match "[[:alpha:]]" "--a--") &rarr; ("a")
(pregexp-match "[[:alpha:]]" "--_--") &rarr; #f
```

By placing a caret (`^`) immediately after `&#91;:`, you get the inversion
of that POSIX character class.  Thus, `&#91;:^alpha&#93;` is the class
containing all characters except the letters.

#### Quantifiers

The _quantifiers_ `*`, `+`, and `?` match respectively: zero or more,
one or more, and zero or one instances of the preceding subpattern.

```bigloo
(pregexp-match-positions "c[ad]*r" "cadaddadddr") &rarr; ((0 . 11))
(pregexp-match-positions "c[ad]*r" "cr")          &rarr; ((0 . 2))

(pregexp-match-positions "c[ad]+r" "cadaddadddr") &rarr; ((0 . 11))
(pregexp-match-positions "c[ad]+r" "cr")          &rarr; #f

(pregexp-match-positions "c[ad]?r" "cadaddadddr") &rarr; #f
(pregexp-match-positions "c[ad]?r" "cr")          &rarr; ((0 . 2))
(pregexp-match-positions "c[ad]?r" "car")         &rarr; ((0 . 3))
```

#### Numeric quantifiers

You can use braces to specify much finer-tuned quantification than is
possible with `*`, `+`, `?`.

The quantifier `{m`} matches _exactly_ `m` instances of the
preceding _subpattern_.  `m` must be a nonnegative integer.

The quantifier `{m,n`} matches at least `m` and at most `n`
instances.  `m` and `n` are nonnegative integers with `m <= n`.  You
may omit either or both numbers, in which case `m` defaults to 0 and
`n` to infinity.

It is evident that `+` and `?` are abbreviations for
`{1,`} and `{0,1`} respectively.  `*` abbreviates
`{,`}, which is the same as `{0,`}.

```bigloo
(pregexp-match "[aeiou]{3}" "vacuous")  &rarr; ("uou")
(pregexp-match "[aeiou]{3}" "evolve")   &rarr; #f
(pregexp-match "[aeiou]{2,3}" "evolve") &rarr; #f
(pregexp-match "[aeiou]{2,3}" "zeugma") &rarr; ("eu")
```

#### Non-greedy quantifiers

The quantifiers described above are _greedy_, ie, they match the
maximal number of instances that would still lead to an overall match
for the full pattern.

```bigloo
(pregexp-match "<.*>" "<tag1> <tag2> <tag3>")
 &rarr; ("<tag1> <tag2> <tag3>")
```

To make these quantifiers _non-greedy_, append a `?` to them.
Non-greedy quantifiers match the minimal number of instances needed to
ensure an overall match.

```bigloo
(pregexp-match "<.*?>" "<tag1> <tag2> <tag3>") &rarr; ("<tag1>")
```

The non-greedy quantifiers are respectively:
`*?`, `+?`, `??`, `{m`?}, `{m,n`?}.
Note the two uses of the metacharacter `?`.

#### Clusters

_Clustering_, ie, enclosure within parens `(`...`)`, identifies the
enclosed _subpattern_ as a single entity.  It causes the matcher to
_capture_ the _submatch_, or the portion of the string matching the
subpattern, in addition to the overall match.

```bigloo
(pregexp-match "([a-z]+) ([0-9]+), ([0-9]+)" "jan 1, 1970")
 &rarr; ("jan 1, 1970" "jan" "1" "1970")
```

Clustering also causes a following quantifier to treat
the entire enclosed subpattern as an entity.

```bigloo
(pregexp-match "(poo )*" "poo poo platter") &rarr; ("poo poo " "poo ")
```

The number of submatches returned is always equal to the number of
subpatterns specified in the regexp, even if a particular subpattern
happens to match more than one substring or no substring at all.

```bigloo
(pregexp-match "([a-z ]+;)*" "lather; rinse; repeat;")
 &rarr; ("lather; rinse; repeat;" " repeat;")
```

Here the `*`-quantified subpattern matches three times, but it is the
last submatch that is returned.

It is also possible for a quantified subpattern to fail to match, even
if the overall pattern matches.  In such cases, the failing submatch
is represented by `#f`.

```bigloo
(define date-re
  ;match `month year' or `month day, year'.
  ;subpattern matches day, if present 
  (pregexp "([a-z]+) +([0-9]+,)? *([0-9]+)"))

(pregexp-match date-re "jan 1, 1970")
 &rarr; ("jan 1, 1970" "jan" "1," "1970")

(pregexp-match date-re "jan 1970")
 &rarr; ("jan 1970" "jan" #f "1970")
```

#### Backreferences

Submatches can be used in the insert string argument of the procedures
`pregexp-replace` and `pregexp-replace*`.  The insert string
can use `\n` as a _backreference_ to refer back to the
_n_th submatch, ie, the substring that matched the _n_th
subpattern.  `\0` refers to the entire match, and it can also be
specified as `\&`.

```bigloo
(pregexp-replace "_(.+?)_" 
  "the _nina_, the _pinta_, and the _santa maria_"
  "*\\1*")
 &rarr; "the *nina*, the _pinta_, and the _santa maria_"

(pregexp-replace* "_(.+?)_" 
  "the _nina_, the _pinta_, and the _santa maria_"
  "*\\1*")
 &rarr; "the *nina*, the *pinta*, and the *santa maria*"

;recall: \S stands for non-whitespace character

(pregexp-replace "(\\S+) (\\S+) (\\S+)"
  "eat to live"
  "\\3 \\2 \\1")
 &rarr; "live to eat"
```

Use `\\` in the insert string to specify a literal
backslash.  Also, `\$` stands for an empty string,
and is useful for separating a backreference `\n`
from an immediately following number.

Backreferences can also be used within the regexp
pattern to refer back to an already matched subpattern
in the pattern.  `\n` stands for an exact repeat
of the _n_th submatch. (`\0`, which is useful in
an insert string, makes no  sense within the regexp
pattern, because the entire regexp has not matched yet
that you could refer back to it.)

```bigloo
(pregexp-match "([a-z]+) and \\1"
  "billions and billions")
 &rarr; ("billions and billions" "billions")
```

Note that the backreference is not simply a repeat of the previous
subpattern.  Rather it is a repeat of _the particular substring
already matched by the subpattern_.

In the above example, the backreference can only match
`billions`.  It will not match `millions`, even
though the subpattern it harks back to --- `(&#91;a-z&#93;+)`
---  would have had no problem doing so: 

```bigloo
(pregexp-match "([a-z]+) and \\1"
  "billions and millions")
 &rarr; #f 
```

The following corrects doubled words:

```bigloo
(pregexp-replace* "(\\S+) \\1"
  "now is the the time for all good men to to come to the aid of of the party"
  "\\1")
 &rarr; "now is the time for all good men to come to the aid of the party"
```

The following marks all immediately repeating patterns
in a number string:

```bigloo
(pregexp-replace* "(\\d+)\\1"
  "123340983242432420980980234"
  "@{\\1,\\1@}")
 &rarr; "12@{3,3@}40983@{24,24@}3242@{098,098@}0234"
```

#### Non-capturing clusters

It is often required to specify a cluster
(typically for quantification) but without triggering
the capture of submatch information.  Such
clusters are called _non-capturing_.  In such cases,
use `(?:` instead of `(` as the cluster opener.  In
the following example, the  non-capturing cluster 
eliminates the "directory" portion of a given
pathname, and the capturing cluster  identifies the
basename.

```bigloo
(pregexp-match "^(?:[a-z]*/)*([a-z]+)$" 
  "/usr/local/bin/mzscheme")
 &rarr; ("/usr/local/bin/mzscheme" "mzscheme")
```

#### Cloisters

The location between the `?` and the `:` of a non-capturing
cluster is called a _cloister_. (A useful, if terminally cute,
coinage from the abbots of Perl.)  You can put _modifiers_ there
that will cause the enclustered subpattern to be treated specially.  The
modifier `i` causes the subpattern to match
_case-insensitively_:

```bigloo
(pregexp-match "(?i:hearth)" "HeartH") &rarr; ("HeartH")
```

The modifier `x` causes the subpattern to match
_space-insensitively_, ie, spaces and
comments within the
subpattern are ignored.  Comments are introduced
as usual with a semicolon (`;`) and extend till
the end of the line.  If you need
to include a literal space or semicolon in
a space-insensitized subpattern, escape it
with a backslash.

```bigloo
(pregexp-match "(?x: a   lot)" "alot")
 &rarr; ("alot")

(pregexp-match "(?x: a  \\  lot)" "a lot")
 &rarr; ("a lot")

(pregexp-match "(?x:
   a \\ man  \\; \\   # ignore
   a \\ plan \\; \\   # me
   a \\ canal         # completely
   )" 
 "a man; a plan; a canal")
 &rarr; ("a man; a plan; a canal")
```

You can put more than one modifier in the cloister.

```bigloo
(pregexp-match "(?ix:
   a \\ man  \\; \\   # ignore
   a \\ plan \\; \\   # me
   a \\ canal         # completely
   )" 
 "A Man; a Plan; a Canal")
 &rarr; ("A Man; a Plan; a Canal")
```

A minus sign before a modifier inverts its meaning.
Thus, you can use `-i` and `-x` in a 
_subcluster_ to overturn the insensitivities caused by an
enclosing cluster.

```bigloo
(pregexp-match "(?i:the (?-i:TeX)book)"
  "The TeXbook")
 &rarr; ("The TeXbook")
```

This regexp will allow any casing for `the`
and `book` but insists that `TeX` not be 
differently cased.

#### Alternation

You can specify a list of _alternate_
subpatterns by separating them by `|`.   The `|`
separates subpatterns in the nearest enclosing cluster 
(or in the entire pattern string if there are no
enclosing parens).  

```bigloo
(pregexp-match "f(ee|i|o|um)" "a small, final fee")
 &rarr; ("fi" "i")

(pregexp-replace* "([yi])s(e[sdr]?|ing|ation)"
   "it is energising to analyse an organisation 
   pulsing with noisy organisms"
   "\\1z\\2")
 &rarr; "it is energizing to analyze an organization 
   pulsing with noisy organisms"
```
 
Note again that if you wish
to use clustering merely to specify a list of alternate
subpatterns but do not want the submatch, use `(?:`
instead of `(`. 

```bigloo
(pregexp-match "f(?:ee|i|o|um)" "fun for all")
 &rarr; ("fo")
```

An important thing to note about alternation is that
the leftmost matching alternate is picked regardless of
its length.  Thus, if one of the alternates is a prefix
of a later alternate, the latter may not have 
a chance to match.

```bigloo
(pregexp-match "call|call-with-current-continuation" 
  "call-with-current-continuation")
 &rarr; ("call")
```

To allow the longer alternate to have a shot at 
matching, place it before the shorter one:

```bigloo
(pregexp-match "call-with-current-continuation|call"
  "call-with-current-continuation")
 &rarr; ("call-with-current-continuation")
```

In any case, an overall match for the entire regexp is
always preferred to an overall nonmatch.  In the
following, the longer alternate still wins, because its
preferred shorter prefix fails to yield an overall
match.

```bigloo
(pregexp-match "(?:call|call-with-current-continuation) constrained"
  "call-with-current-continuation constrained")
 &rarr; ("call-with-current-continuation constrained")
```

Backtracking
------------

We've already seen that greedy quantifiers match
the maximal number of times, but the overriding priority
is that the overall match succeed.  Consider

```bigloo
(pregexp-match "a*a" "aaaa")
```

The regexp consists of two subregexps,
`a*` followed by `a`.
The subregexp `a*` cannot be allowed to match
all four `a`'s in the text string `"aaaa"`, even though
`*` is a greedy quantifier.  It may match only the first
three, leaving the last one for the second subregexp.
This ensures that the full regexp matches successfully.

The regexp matcher accomplishes this via a process
called _backtracking_.  The matcher
tentatively allows the greedy quantifier 
to match all four `a`'s, but then when it becomes
clear that the overall match is in jeopardy, it 
_backtracks_ to a less greedy match of 
_three_ `a`'s.  If even this fails, as in the
call

```bigloo
(pregexp-match "a*aa" "aaaa")
```

The matcher backtracks even further.  Overall
failure is conceded only when all possible backtracking
has been tried with no success. 

Backtracking is not restricted to greedy quantifiers.
Nongreedy quantifiers match as few instances as
possible, and progressively backtrack to more and more
instances in order to attain an overall match.  There
is backtracking in alternation too, as the more
rightward alternates are tried when locally successful
leftward ones fail to yield an overall match.

#### Disabling backtracking

Sometimes it is efficient to disable backtracking.  For
example, we may wish  to  _commit_ to a choice, or
we know that trying alternatives is fruitless.  A
nonbacktracking regexp is enclosed in `(?>`...`)`.

```bigloo
(pregexp-match "(?>a+)." "aaaa")
 &rarr; #f
```

In this call, the subregexp `?>a*` greedily matches
all four `a`'s, and is denied the opportunity to
backpedal.  So the overall match is denied.  The effect
of the regexp is therefore to match one or more `a`'s
followed by something that is definitely non-`a`.

Looking ahead and behind
------------------------

You can have assertions in your pattern that look 
_ahead_ or _behind_ to ensure that a subpattern does
or does not occur.   These _look around_ assertions are
specified by putting the subpattern checked for in a
cluster whose leading characters are: `?=` (for positive
lookahead), `?!` (negative lookahead), `?<=`
(positive lookbehind), `?<!` (negative lookbehind).
Note that the subpattern in the assertion  does not
generate a match in the final result.  It merely allows
or disallows the rest of the match.

#### Lookahead

Positive lookahead (`?=`) peeks ahead to ensure that
its subpattern _could_ match.  

```bigloo
(pregexp-match-positions "grey(?=hound)" 
  "i left my grey socks at the greyhound") 
 &rarr; ((28 . 32))
```

The regexp `"grey(?=hound)"` matches `grey`, but
_only_ if it is followed by `hound`.  Thus, the first
`grey` in the text string is not matched. 

Negative lookahead (`?!`) peeks ahead
to ensure that its subpattern could not possibly match.  

```bigloo
(pregexp-match-positions "grey(?!hound)"
  "the gray greyhound ate the grey socks") 
 &rarr; ((27 . 31))
```

The regexp `"grey(?!hound)"` matches `grey`, but
only if it is _not_ followed by `hound`.  Thus 
the `grey` just before `socks` is matched.

#### Lookbehind

Positive lookbehind (`?<=`) checks that its subpattern _could_ match
immediately to the left of the current position in
the text string.  

```bigloo
(pregexp-match-positions "(?<=grey)hound"
  "the hound in the picture is not a greyhound") 
 &rarr; ((38 . 43))
```

The regexp `(?<=grey)hound` matches `hound`, 
but only if it is preceded by `grey`.  

Negative lookbehind
(`?<!`) checks that its subpattern
could not possibly match immediately to the left.  

```bigloo
(pregexp-match-positions "(?<!grey)hound"
  "the greyhound in the picture is not a hound")
 &rarr; ((38 . 43))
```

The regexp `(?<!grey)hound` matches `hound`, but only if
it is _not_ preceded by `grey`.

Lookaheads and lookbehinds can be convenient when they
are not confusing.  

An Extended Example
-------------------

Here's an extended example from Friedl that covers many of the features
described above.  The problem is to fashion a regexp that will match any
and only IP addresses or _dotted quads_, ie, four numbers separated
by three dots, with each number between 0 and 255.  We will use the
commenting mechanism to build the final regexp with clarity.  First, a
subregexp `n0-255` that matches 0 through 255.

```bigloo
(define n0-255
  "(?x:
  \\d          ;  0 through   9
  | \\d\\d     ; 00 through  99
  | [01]\\d\\d ;000 through 199
  | 2[0-4]\\d  ;200 through 249
  | 25[0-5]    ;250 through 255
  )")
```

The first two alternates simply get all single- and
double-digit numbers.  Since 0-padding is allowed, we
need to match both 1 and 01.  We need to be careful
when getting 3-digit numbers, since numbers above 255
must be excluded.  So we fashion alternates to get 000
through 199, then 200 through 249, and finally 250
through 255. (Note that `n0-255` lists prefixes as
preferred alternates, something we cautioned against in
section @ref{Alternation}. However, since we intend
to anchor this subregexp explicitly to force an overall
match, the order of the alternates does not matter.)

An IP-address is a string that consists of
four `n0-255`s with three dots separating
them.

```bigloo
(define ip-re1
  (string-append
    "^"        ;nothing before
    n0-255     ;the first n0-255,
    "(?x:"     ;then the subpattern of
    "\\."      ;a dot followed by
    n0-255     ;an n0-255,
    ")"        ;which is
    "@{3@}"      ;repeated exactly 3 times
    "$"        ;with nothing following
    ))
```

Let's try it out.

```bigloo
(pregexp-match ip-re1 "1.2.3.4")        &rarr; ("1.2.3.4")
(pregexp-match ip-re1 "55.155.255.265") &rarr; #f
```

which is fine, except that we also have

```bigloo
(pregexp-match ip-re1 "0.00.000.00") &rarr; ("0.00.000.00")
```

All-zero sequences are not valid IP addresses!  Lookahead to the rescue.
Before starting to match `ip-re1`, we look ahead to ensure we don't
have all zeros.  We could use positive lookahead to ensure there
_is_ a digit other than zero.

```bigloo
(define ip-re
  (string-append
    "(?=.*[1-9])" ;ensure there's a non-0 digit
    ip-re1))
```

Or we could use negative lookahead to ensure that what's ahead isn't
composed of _only_ zeros and dots.

```bigloo
(define ip-re
  (string-append
    "(?![0.]*$)" ;not just zeros and dots
                 ;(note: dot is not metachar inside [])
    ip-re1))
```

The regexp `ip-re` will match all and only valid IP addresses.

```bigloo
(pregexp-match ip-re "1.2.3.4") &rarr; ("1.2.3.4")
(pregexp-match ip-re "0.0.0.0") &rarr; #f
```
