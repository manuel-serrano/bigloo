<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/string.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Zip                                                           -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/gunzip.scm")
,(example-path "../test/src/zip.bgl")

Zip
===

### port->gzip-port ###
Creates a new port that automatically `unzip` the characters read from
`in`.

### port->zlib-port ###
Creates a new port that automatically `unzip` the characters read from
`in`.

### port->inflate-port ###
Creates a new port that automatically `unzip` the characters read from
`in`. It does not parse a gunzip-header before inflating the
content.

### open-input-inflate-file ###
These function open a gzipped file for input. The file is automatically
unzipped when the characters are read. It is equivalent to:

```bigloo
(let ((p (open-input-port path)))
  (port->gzip-port p))
```

The function `open-input-inflate-file` is similar to
`open-input-gzip-file` (see [port](./port.html) but it does not parse
a gunzip-header before inflating the content.

### gunzip-sendchars ###
Transmit all the characters from the gzipped input port `in` to the output port
`out`. 

### inflate-sendchars ###
Transmit all the characters from the gzipped input port `in` to the output port
`out`. 

Note that the function `send-chars` can also be used on gzipped
input-ports.
