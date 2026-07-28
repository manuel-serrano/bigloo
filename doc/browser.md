<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/browser.md               -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Browser                                                       -->
<!--==================================================================-->

,(implementation-path "../api/browser/src/Llib/dom.scm")
,(example-path "../test/src/browser0.bgl")

Browser
=======

Bigloo allows [Wasm](./wasm.html) generated code to access the
elements of the hosting web page. The Browser API is implemented these
features.

> [!IMPORTANT] A module using it must include in its declaration 
> the clause `(library browser)`. It is available on with the Wasm
> backend. Example:

```bigloo
(module in-a-web-client
  (library browser)
  ...)
```

Dom
---

### get-element-by-id ###
<!-- [:@wasm] -->

Returns a handle to the HTML element `id`.

### element-inner-html ###
<!-- [:@wasm] -->

Returns a string denoting the current content of an HTML element.

### element-inner-html-set! ###
<!-- [:@wasm] -->

Sets the content of an HTML element.


Browser Interactions
--------------------

### alert ###
<!-- [:@wasm] -->

Displays a message in a HTML popup window.
