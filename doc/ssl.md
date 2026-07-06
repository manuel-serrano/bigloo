<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/ssl.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    SSL                                                           -->
<!--==================================================================-->

,(implementation-path "../api/ssl/src/Llib/ssl.scm")
,(example-path "../test/src/ssl0.bgl")

SSL
===

Bigloo allows access to SSL sockets, certificates and private keys, in
order to build secure encrypted and/or signed communications. The SSL
API is implemented in the `ssl` library. 

> [!IMPORTANT] A module using it must include in its declaration 
> the clause `(library ssl)`. Example:

```bigloo
(module ssl-ex
  (library ssl)
```

> [!WARNING] As June 2026, the Wasm backend does not support the SSL library.


SSL Misc
--------

### ssl-version ###
<!-- [:@C-jvm] -->
Returns a string representing the SSL library version number.

### ssl-rand-bytes ###
<!-- [:@C-jvm] -->
Generates random bytes stored in a resulting string.

### ssl-rand-pseudo-bytes ###
<!-- [:@C-jvm] -->
Generates random bytes stored in a resulting string.

### ssl-rand-status ###
<!-- [:@C] -->
Generates a random boolean.

### ssl-rand-poll ###
<!-- [:@C] -->
Generates a random boolean.

### ssl-clear-error ###
<!-- [:@C-jvm] -->
Clear previous SSL errors.

SSL Sockets
-----------

Bigloo defines SSL sockets, on systems that support them, as first
class objects. SSL Sockets permits processes to communicate even if
they are on different machines securely via encrypted connections. SSL
Sockets are useful for creating secure client-server applications.

### ssl-socket? ###
<!-- [:@C-jvm] -->
Returns `#t` if an only if obj is a SSL socket (either client or server).
Returns `#f` otherwise.

### make-ssl-client-socket ###
<!-- [:@C-jvm] -->

The function `make-ssl-client-socket` returns a new client socket
object. This object satisfies the `socket?` predicate (see
[Socket](./socket.html)) can be used in any context where a socket created by
`make-client-socket` can be used.

A SSL client socket establishes a link between the running application
(client) and a remote application (server) listening on port
`port-number` of `hostname`. If optional argument `bufsiz`
is lesser or equal to `1` then the input port associated with the socket is
unbuffered. This is useful for socket clients connected to servers
that do not emit `#\Newline` character after emissions. The optional
argument `buffer` can either be:

  * A positive fixnum, this gives the size of the buffer.
  * The boolean `#t`, a buffer is allocated.
  * The boolean `#f`, the socket is unbufferized.
  * A string, it is used as buffer.

If the optional argument `timeout` is `0`, the execution
blocks until the connection is established. If the `timeout` is
provided, the execution unblocks after `timeout` microseconds
unless the connection is established. If the `protocol` option
argument is given, it specifies the encryption protocol. Accepted
values are `'sslv2`, `'sslv3`, `'sslv23` (alias
`'ssl`), `'tlsv1` (alias `'tls`), `'tlsv1_1`,
`'tlsv1_2`, `'tlsv1_3`, or `'dtlsv1` (alias `'dtls`). The default
value is `'sslv23`.

The SSL socket will sign the connection using the optional arguments
`cert` (for the certificate) and `pkey` (for the private key).
The certificate `cert` must be of type `certificate`, and
the private key `pkey` must be of type `private-key`.
If any of those two arguments is given, they must both be given.
If those optional arguments are missing the connection will be encrypted
but not signed from the client side.

The `CAs` optional argument specifies the list of certificates to
trust as CA (Certificate Authority) for the connection. It must be a 
list of values of type `certificate`. If the list is empty, the
default list of trusted CA is used (set by the system). Note that
giving a list of trusted certificates turns on the peer (server)
certificate validation: an `&io-error` will be raised if the peer
(server) certificate is not signed directly or indirectly by one of the
certificates in `CAs`.

The `accepted-certs` optional argument gives a list of certificate
objects (of type `certificate`) which are accepted as peer (server)
certificate. If `accepted-certs` is `#f` then every peer (server)
certificate is accepted (aside from eventual certificate validation).
If `accepted-certs` is a list, the peer (server) certificate must
match one of the given certificates. Otherwise, an `&io-error` 
will be raised.

The optional `domain` argument specifies the protocol used by the socket.
The supported domains are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.
  * `unspec`: uses IPv4 or IPv6 as determined by getaddrinfo.

If the connection cannot be established, an `&io-error` is raised
(see [Errors](./error.html)).

When a socket is used in unbufferized mode the characters available on
the input port @emph{must} be read exclusively with `read-char` or
`read-line`. It is forbidden to use `read` or any regular grammar.
This limitation is imposed by Rgc (see [Regular Parsing](rgc.html))
that intrinsicly associates buffers with regular grammars. If the
current Rgc implementation is improved on the coming version this
restriction will be eliminated.

The function `make-ssl-client-socket` is defined in the SSL library.
A module that needs this facility must then use a `library` clause
(see [Modules](./module5.html)). The SSL library can also be loaded
from the interpreter using the `library-load` function (see [Eval](eval.html)).

### client-socket-use-ssl! ###
<!-- [:@C-jvm] -->

Returns an SSL socket built from a socket obtained by
`make-client-socket` (see [Socket](./socket.html)). Depending on the
implementation and back-end the returned socket may or may not be
`eq?` to `socket`.


### make-ssl-server-socket ###
<!-- [:@C-jvm] -->

`make-ssl-server-socket` returns a new server socket object which
satisfies the `socket?` predicate and which can be used in any
context where a socket created by `make-server-socket` can be
used (see @ref{Socket}).

A SSL server socket opens the port `port` on the current host
`name` (the server), and allows remote applications (clients) to
connect to it.  listening on port `port-number` of
`hostname`. If the optional argument `port` is not given or is
`0`, the server socket will use the first availailable port
number. If the optional argument `name` is given, the server
socket will be bound to the network interface representing the given
host name. If it is `#f` (the default) the socket will be bound
on every local network interface.  If the `protocol` option
argument is given, it specifies the encryption protocol. Accepted
values are `'sslv2`, `'sslv3`, `'sslv23` (alias
`'ssl`), `'tlsv1` (alias `'tls`), `'tlsv1_1`,
`'tlsv1_2` `'tlsv1_3`, or `'dtlsv1` (alias
`'dtls`). The default value is `'sslv23`.

The SSL socket will sign the connection using the optional arguments
`cert` (for the certificate) and `pkey` (for the private key).
The certificate `cert` must be of type `certificate`, and
the private key `pkey` must be of type `private-key`.
If any of those two arguments is given, they must both be given.
If those optional arguments are missing the connection will be encrypted
but not signed from the server side, which means the peer (client) will
have to provide a certificate/private key pair to encrypt the connection,
and that seldom happens. Typical SSL servers provide their certificate
and private key.

Note that since the peer (client) certificate is only known when we
are accepting a client socket (with `socket-accept`) the `CAs`
and `accepted-certs` optional arguments are only checked during
the accept operation of a server socket.

The `CAs` optional argument specifies the list of certificates to
trust as CA (Certificate Authority) for the connection. It must be a 
list of values of type `certificate`. If the list is empty, the
default list of trusted CA is used (set by the system). Note that
giving a list of trusted certificates turns on the peer (client)
certificate validation: an `&io-error` will be raised if the peer
(client) certificate is not signed directly or indirectly by one of the
certificates in `CAs` when accepting the client socket.

The `accepted-certs` optional argument gives a list of certificate
objects (of type `certificate`) which are accepted as peer (client)
certificate. If `accepted-certs` is `#f` then every peer (client)
certificate is accepted (aside from eventual certificate validation).
If `accepted-certs` is a list, the peer (client) certificate must
match one of the given certificates. Otherwise, an `&io-error` 
will be raised when accepting the client socket.

The optional `domain` argument specifies the protocol used by the socket.
The supported domains are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.

If the connection cannot be established, an `&io-error` is raised
(see @ref{Errors Assertions and Traces}).

The function `make-ssl-server-socket` is defined in the SSL library.
A module that needs this facility must then use a `library` clause
(see [Modules](./modules.html))). The SSL library can also be loaded
from the interpreter using the `library-load` function (see
[Libraries](./module5.html)).


Certificates
------------

Certificates are instances of the `certificate` class. There type
can be checked with `(isa? expr certificate)`.

### read-certificate ###
<!-- [:@C-jvm] -->

Reads an X509 certificate stored in PEM format in the given `file`
name.  If the file cannot be read, it raises an `&io-error`
condition. Otherwise the certificate is returned.

### read-pem-file ###
<!-- [:@C-jvm] -->

Reads a list of X509 certificate stored in PEM format in the given
`file` name. If the file cannot be read, it raises an `&io-error`
condition. Otherwise the list of certificate contained in the file is
returned. 

### certificate-subject ###
<!-- [:@C-jvm] -->
Returns the CommonName (CN) part of the subject of the given certificate.

### certificate-issuer ###
<!-- [:@C-jvm] -->
Returns the CommonName (CN) part of the issuer of the given certificate.

Private Keys
------------

Private keys are instances of the `private-key` class. There type
can be checked with `(isa? expr private-key)`.

### read-private-key ###
<!-- [:@C-jvm] -->
Reads a private key stored in PEM format in the given `file` name.
If the file cannot be read, it raises an
`&io-error` condition. Otherwise the private key is returned.

Hash
----

### ssl-hash ###
<!-- [:@C-jvm] -->

### evp-get-hashes ###
<!-- [:@C-jvm] -->

### ssl-hash-update! ###
<!-- [:@C-jvm] -->

### ssl-hash-digest ###
<!-- [:@C-jvm] -->

Hmac
----

### ssl-hmac ###
<!-- [:@C-jvm] -->

### ssl-hmac-init ###
<!-- [:@C-jvm] -->

### ssl-hmac-update! ###
<!-- [:@C-jvm] -->

### ssl-hmac-digest ###
<!-- [:@C-jvm] -->

### pkcs5-pbkdf2-hmac-sha1 ###
<!-- [:@C-jvm] -->

Sign and Verif
--------------

### ssl-sign ###
<!-- [:@C-jvm] -->

### ssl-verify ###
<!-- [:@NoTest-C-jvm] -->

### ssl-sign-init ###
<!-- [:@NoTest-C-jvm] -->

### ssl-sign-update! ###
<!-- [:@NoTest-C-jvm] -->

### ssl-sign-sign ###
<!-- [:@C-jvm] -->

### ssl-verify-final ###
<!-- [:@C-jvm] -->


Cipher
------

### ssl-cipher ###
<!-- [:@C-jvm] -->

### ssl-cipher-init ###
<!-- [:@C-jvm] -->

### ssl-cipher-initiv ###
<!-- [:@C-jvm] -->

### ssl-cipher-update! ###
<!-- [:@C-jvm] -->

### ssl-cipher-final ###
<!-- [:@C-jvm] -->

### ssl-get-ciphers ###
<!-- [:@C-jvm] -->

### evp-get-ciphers ###
<!-- [:@C-jvm] -->

### ssl-cipher-set-auto-padding ###
<!-- [:@C-jvm] -->

Secure Context
--------------

### secure-context ###
<!-- [:@C-jvm] -->

### secure-context-close ###
<!-- [:@C-jvm] -->

### secure-context-add-root-certs! ###
<!-- [:@C-jvm] -->

### secure-context-set-cert! ###
<!-- [:@C-jvm] -->

### secure-context-set-key! ###
<!-- [:@C-jvm] -->

### secure-context-set-ciphers! ###
<!-- [:@C-jvm] -->

### secure-context-set-session-id-context! ###
<!-- [:@C-jvm] -->

### secure-context-load-pkcs12 ###
<!-- [:@C-jvm] -->

DH
--

### dh ###
<!-- [:@C-jvm] -->

### dh-generate-parameters-ex ###
<!-- [:@C-jvm] -->

### dh-generate-key ###
<!-- [:@C-jvm] -->

### dh-compute-key ###
<!-- [:@C-jvm] -->

### dh-check-pub-key ###
<!-- [:@C-jvm] -->

### dh-size ###
<!-- [:@C-jvm] -->


BN
--

### bn-new ###
<!-- [:@C-jvm] -->

### bn-free ###
<!-- [:@C-jvm] -->

### bn-bin2bn ###
<!-- [:@C-jvm] -->

### bn-num-bytes ###
<!-- [:@C-jvm] -->

### bn-set-word ###
<!-- [:@C-jvm] -->

