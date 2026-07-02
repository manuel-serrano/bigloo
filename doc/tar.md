<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Tar                                                           -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/tar.scm")
,(example-path "../test/src/tar.bgl")

Tar
===

Bigloo provides facility for decoding compressed and uncompresses tar files. 
For instance, here is an example that simulates the Unix command `tar xvfz`
using the class and functions described in this section.

```bigloo
,(include "examples/c/untar.bgl")
```

Classes
-------
### tar-header ###

Functions
---------

### tar-read-header ###
Reads a tar header from `port`. If the input port does not
conform the tar format, an `&io-error` is raised. On success a 
`tar-header` descriptor is returned.

### tar-read-block ###
Reads the content of the `tar-header` block.

### untar  ###
Untars the archive whose content is provided by the input port `ip`.

If `:file` is provided, `untar` extracts the content of the
file named `:file` and returns a string. The file name must exactly
matches the files of the archive files names. If the file does not exist,
`untar` returns `#f`.

If `:files` is provided, `untar` extracts the content of the first
files of `:files`.

If `:file` is not provided, it _untars_ the whole content,
in the directory denoted by `:directory`, which defaults to `(pwd)`.
The function `untar`, returns the whole list of created directories
and files.

