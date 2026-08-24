<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/web.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Web library                                                   -->
<!--==================================================================-->

,(implementation-path "../api/web/src/Llib/json.scm")
,(example-path "../test/src/web0.bgl")

Web Library
===========

> [!IMPORTANT] A module using the web library must include in its declaration 
> the clause `(library web)`. Example:

```bigloo
(module in-a-web-client
  (library web)
  ...)
```

Json
----

### read-json ###

Parses the input port containing a json file and returns a Bigloo object.
Json is parsed as follows:

  * objects are parsed as Bigloo a-lists;
  * array are parsed as Bigloo vectors;
  * `false` is parsed as `#f`;
  * `null` is parsed as the empty list;
  * numbers are parsed as numbers;
  * strings are parsed as string.
  
### obj->json ###

Outputs the json representation of a Bigloo object in an output port.
This function is symmetric to `read-json` but when it encounters
an unknown value type, it invoked the `fallback` arguments that must
returns a string. This procedure accepts three arguments:

  1. the value;
  2. the output port;
  3. the fallback procedure itself.

### json-stringify ###

Returns a string, which is the json representation of the object passed
as argument.
  



