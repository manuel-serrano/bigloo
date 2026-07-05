<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/internet.md              -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Internet                                                      -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/url.scm")
,(example-path "../test/src/internet.bgl")

Internet
========

### url-parse ###
The argument `url` can either be a string or an input-port. The function
`url-parse` parses the url and returns four values:

  * the protocol,
  * the optional user info,
  * the host name,
  * the port number,
  * the absolute path

<span></span>

### url-sans-protocol-parse ###

The argument `url` can either be a string or an input-port.

This function behaves as `url-parse` except that it assumes that the protocol
part of the url has already been extracted from the URI. It is explicitly
provided using the `protocol` argument.

### http-url-parse ###
The argument `url` can either be a string or an input-port. As 
`url-parse`, it returns four values.

This function parses URLs as found in HTTP GET responses.
@end deffn

### url-path-encode ###
Encode a path that can be used in valid URL.

### url-encode ###

### uri-encode ###

@deffnx {bigloo procedure} uri-encode url
@deffnx {bigloo procedure} uri-encode-component url
Encode a URL by removing any illegal character.

@smalllisp
(url-encode "http:///tmp/foo") @result{} "http://tmp:80/foo"
(url-encode "http:///tmp/foo&bar") @result{} "http://tmp:80/foo%26"
@end smalllisp

@end deffn

@deffn {bigloo procedure} url-decode url
@deffnx {bigloo procedure} url-decode! url
@deffnx {bigloo procedure} uri-decode url
@deffnx {bigloo procedure} uri-decode! url
@deffnx {bigloo procedure} uri-decode-component url
@deffnx {bigloo procedure} uri-decode-component! url
Decode a URL. The function @code{url-decode!} may return its argument
unmodified if no decoding is for the URL.

The variants @code{-component} treat do not escape URI reserved characters
(i.e., #, /, ?, :, @@, &, =, +, and $).
@end deffn

