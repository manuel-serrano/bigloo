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
,(implementation-path "../api/web/src/Llib/xml.scm")
,(implementation-path "../api/web/src/Llib/html.scm")
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
  
The optional argument `input-port` defaults to the current input port.

If the argument `symbol` is not provided or `#f`, the created a-lists  
use strings as keys. If `symbol` is `#t`, the created a-lists use
symbosl as keys.
  
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


XML
---

### XmlElement ###
The class describing xml elements.

### xml-element ###
The `XmlElement` constructor.

### xml-element-children ###
Returns the children of the xml element.


HTML
----

### html-parse ###
Parses HTML read from the input port `port` and returns the list of
parsed elements. THe keyword arguments are:

  * `content-length`: an integer, the character length of the HTML document. 
  This is useful when parsing documents from a socket, for instance, in order 
  to respond to an HTTP request that lacks length information.
  * `encoding`: a symbol, the character encoding, which defaults to UTF-8.
  * `eoi`: a boolean, when true, the document should ends of the end of input.
  * `procedure`: a procedure of 3 or 4 arguments that constructrs the HTML
  elements. The `procedure` argument default to `xml-element`.

### write-html ###
Displays an html element to the optional output port `op` or to the default
output port.



