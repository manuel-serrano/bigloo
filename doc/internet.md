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
,(implementation-path "../runtime/Unsafe/http.scm")
,(example-path "../test/src/internet.bgl")

Internet
========

URL
---

### url? ###
Returns `#t` iff `str` is a valid URL.

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
Encodes a path that can be used in valid URL. Escapes all the characters
in the set "`# &#34;'&#96;&=%?:\n^^&#91;&#93;\\<>;,{|}()~$!+@`".

### url-encode ###
Encodes a complete URL, encoding the path component with `url-path-encode`.

### url-decode ###
Decodes a string encoded ith `url-encode`. Returns a fresh string.

### url-decode! ###
As `url-decode` but might return its argument.

### uri-encode ###
Encode a string, escaping all the characters in the set "` &#34;&#96;%\n^&#91;&#93;\\<>{|}`".

### uri-encode-component ###
Encode a string, escaping all the characters in the set "`# &#34;&#96;+=%?:\n^&#93;\\<>;/@&$,{|}"`".

### uri-decode-component ###
Decodes a string encoded with `uri-encode-component. Returns a fresh
string.

### uri-decode-component! ###
As `uri-decode-component` but might return its argument.

### www-form-urlencode ###
Encodes a list of arguments suitable for a www-form format of an HTTP
request.

### www-form-urldecode ###
Decodes a string encode with `www-form-urlencode`.

### x-www-form-urlencode ###
Encodes a list of arguments for the path component of a URL.

HTTP
----

### &http-error ###

### &http-redirection ###

### &http-redirection-error ###

### &http-status-error ###

### http-read-line ###
Reads one crlf termined line from the input port `p`. The returned string
contains the line termination. Returns the eof object at the end of file.

### http-read-crlf ###
Reads CRLF terminated line containing only space and tab characters.
Returns the string `"\r\n"`. Raises an `&io-parse-error` is the line
contains other characters.

### http-parse-status-line ###
Parses an HTTP/1.1 (section 6.1) status line of the the form:

```bnf
<StatusLine> --> <HTTPVersion> <Integer> <String> CRFL

<HTTPVersion> --> http / <digit>.<digit>
  | HTTP / <digit>.<digit>
  | https / <digit>.<digit>
  | HTTPS / <digit>.<digit>
  | ICY
```

On success, it returns the values:

  * the http protocol;
  * the status code;
  * the reason string (including the CRLF termination) if the line
  
On failure, raises an `&io-parse-error`.

### http-parse-header ###
Parses an HTTP request header from the input-port `ip`. If provided,
the optional argument `op` must be an output port. It is used to honor
HTTP requests that _expect_ an immediate acknowledge (e.g., a `expect:
100-continue`  header field).

On success `http-parse-header` returns:

  * The list of properties found in the header;
  * the hostname;
  * the port;
  * the content length;
  * the transfer encoding property;
  * the authorization information;
  * the proxy authorization information;
  * the connection.
  
On failure, it raises an `&io-parse-error`.

### http-parse-response ###
Parses the whole response of an HTTP request. The argument `procedure`
is invoked with five arguments:

  * the input port to read the characters of the response,
  * the status code,
  * the header of the response,
  * the content length,
  * the type encoding.

### http-response-body->port ###
Parses an HTTP response and build an output port that delivers the 
characters of the content.

### http-chunks->procedure ###
Returns a procedure that each time invoked returns the next chunk of
a _chunked_ http response, i.e., an http response that announces a _chunked_
transfer encoding.

### http-chunks->port ###
Returns an input port the delivers the body of a _chuncked_ http response.

### http-send-chunks ###
Reads an http chunked response and output the read chunks to the output port.
If `trailer` is true, sends all the character after the last chunk.

### http ###
<!-- [:@NoTest] -->

Opens an HTTP connection. Returns a socket.

It is an error to specify a header twice. In particular, it is illegal
to re-define keyword-ed arguments in the `header:` list. For
instance, it is illegal to include in the `header:` actual list
value a value for the 'Connection' HTTP connection.

The optional argument `args` is used for `post` method. The actual
value should be a list of lists. Each of these sublists must have two
values:

  * the argument name
  * the argument actual value

The argument name can be either a string which is the name of the
argument or a list of two elements. In that case, the first element of
these list is the argument name. The second element should be a string
that denotes additional parameter.

```bigloo
(http :host "localhost" :port 8080 :method 'post
   :header '((enctype: "multipart/form-data"))
   :args `(("x" "foo") (("foo.scm" "filename=\"foo.scm\"\nContent-type: application/octet-stream" ,(with-input-from-file "foo.scm" read-string))))
   ...)
```

An http connection blocks until the connection is established. If the
optional argument `timeout` is provided, the connection must be
established before the specified time interval elapses. The timeout
is expressed in microseconds.

Example:

```bigloo
,(include "./examples/c/wget.bgl")
```
