<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/multimedia.md            -->
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
,(implementation-path "../api/multimedia/src/Llib/color.scm")
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

Jpeg
----

### exif ###
This class defines the objects that are constructed when extracting the
exif information of an image.

### jpeg-exif ###
Extracts the `exif` information of a jpeg image. 

The function `jpeg-exif` tries to extract as much information as possible
but depending on the exif version or the tool that generated it, not all
fields of the `exif` class are filled.

### jpeg-exif-mmap ###
As `jpeg-exif` but extrace the exif data from an open 
[memory-mapped area](./mmap.html).

### jpeg-exif-update! ###
Updates the exif information from that receive in the exif object.

> [!IMPORTANT] The function `jpeg-exif-update!` only updates the exif
> information of the image. It does not add new tags and when updating
> a string information, it limits the number of character written to
> that of the old string.

### jpeg-exif-mmap-update! ###
As `jpeg-exif-update!` but updates the exif data of a 
[memory mapped area](./mmap.html).

### jpeg-exif-reset! ###
Resets the exif information of a image. This does not remove any tag
but set all found tags corresponding to `exif` instances to default
values.

This function might be useful to remove the private information stored
in an jpeg exif metadata.

### jpeg-exif-mmap-reset! ###
As `jpeg-exif-reset!` but for [memory mapped area](./mmap.html).

Jpeg
----

### jpeg-dimensions ###
Extracts the width and height of a jpeg image.

### jpeg-parse-dimensions ###
Parses the input port `ip` to extract the jpeg dimensions.


Color Parsing and Manipulation
------------------------------

### make-hex-color ###
Constructs a web string for the RGB color.

### parse-hex-color ###
Decomposes a hex color (as returned by `make-hex-color`) and returns
the three RGB components.

### parse-web-color ###
Decomposes a a color that can be in either format:

  * #xRGB
  * rgb(R, G, B), rgb(R%, G%, B%)
  * HSL(H, S, V)

### hsv->rgb ###
Transposes an Hue-Saturation-Value color into RGB.

### hsl->rgb ###
Transposes an Hue-Saturation-Luminance color into RGB.

### rgb->hsl ###
Transposes an Reb-Green-Blue color into HSL.

