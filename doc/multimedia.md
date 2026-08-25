<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/web.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Multimedia library                                            -->
<!--==================================================================-->

,(implementation-path "../api/multimedia/src/Llib/exif.scm")
,(implementation-path "../api/multimedia/src/Llib/jpeg.scm")
,(example-path "../test/src/multimedia0.bgl")

Multimedia Library
==================

> [!IMPORTANT] A module using the multimedia library must include in its 
> declaration the clause `(library multimedia)`. Example:

```bigloo
(module in-a-multimedia-client
  (library multimedia)
  ...)
```

Exif
----

### exif ###
This class defines the objects that are constructed when extracting the
exif information of an image.

### jpeg-exif ###
Extracts the `exif` information of a jpeg image. 

The function `jpeg-exif` tries to extract as much information as possible
but depending on the exif version or the tool that generated it, not all
fields of the `exif` class are filled.

Jpeg
----

### jpeg-dimensions ###
Extracts the width and height of a jpeg image.

### jpeg-parse-dimensions ###
Parses the input port `ip` to extract the jpeg dimensions.

