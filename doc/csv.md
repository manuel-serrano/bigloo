<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/csv.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    CSV                                                           -->
<!--==================================================================-->

,(implementation-path "../api/csv/src/Llib/csv.scm")
,(example-path "../test/src/csv0.bgl")

CSV
===

The Bigloo csv library supports the parsing of csv and csv-like
data. By default, it enables the parsing of comma, tab, and pipe
separated data. In addition, facilities are provided that enable
extending the library to support additonal csv-like formats.

> [!IMPORTANT] A module using CSV features must include in its declaration
> the clause `(library csv)`. Example:

```bigloo
(module csv-ex
  (library csv)
```

Parsing CSV records
-------------------

### read-csv-record ###

The function `read-csv-record` has one required argument, the input-port of
the csv data to parse, and an optional argument indicating the lexer
to use, by default the lexer supporting standard csv files. It returns
a single record, as a list, or `eof-object`. Upon error, it will
throw an `&invalid-port-error` or `&io-parse-error` exception.

### read-csv-records ###

The function `read-csv-records` has one required argument, the input-port of
the csv data to parse, and an optional argument indicating the lexer
to use, by default the lexer supporting standard csv files. It returns
all of the records, as a list of lists, or `#eof-object`. Upon
error, it will throw an `&invalid-port-error` or
`&io-parse-error` exception.

### csv-for-each ###

The function `csv-for-each` has two required arguments, a procedure to apply
to each record and the input-port of the csv data to parse, and an
optional argument indicating the lexer to use, by default the lexer
supporting standard csv files. It returns `#unspecified`. Upon
error, it will throw an `&invalid-port-error` or
`&io-parse-error` exception.

### csv-map ###

@code{csv-map} has two required arguments, a procedure to apply to
each record and the input-port of the csv data to parse, and an
optional argument indicating the lexer to use, by default the lexer
supporting standard csv files. It returnsthe results of applying
`proc` to each record as a list. Upon error, it will throw an
`&invalid-port-error` or `&io-parse-error` exception.

CSV Lexers
----------

### +csv-lexer+ ###
The variable `+csv-lexer+` is a bigloo-csv lexer supporting the
standard comma-separated value format.

### +ssv-lexer+ ###
The variable `+ssv-lexer+` is a bigloo-csv lexer supporting the
semi-column-separated value format.

### +tsv-lexer+ ###
The variable `+tsv-lexer+` is a bigloo-csv lexer supporting the 
tab-separated value format.

### +psv-lexer+ ###
The variable `+psv-lexer+` is a bigloo-csv lexer supporting the 
pipe-separated value format.


