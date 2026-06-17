<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/net.md                   -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Network                                                       -->
<!--==================================================================-->

,(include "head.html")

,(implementation-path "../runtime/Llib/socket.scm")
,(example-path "../test/src/net.bgl")

Network
=======

Bigloo defines sockets, on systems that support them, as first class objects.
Sockets permits processes to communicate even if they are on different 
machines. Sockets are useful for creating client-server applications.

Bigloo supports both "stream-oriented" sockets and "datagram"
sockets.  Stream-oriented sockets are created and manipulated with the
following procedures.

Predicates
----------

### socket? ###
Returns `#t` if and only if `obj` is a socket.

### socket-server? ###
Returns `#t` if and only if `obj` is a server socket.

### socket-client? ###
Returns `#t` if and only if `obj` is a client socket.

### datagram-socket? ###
Returns `#t` if and only if `obj` is a datagram socket.


Sockets
-------

### make-client-socket ###
<!-- [:@C-jvm] -->

`make-client-socket` returns a new socket object. This socket establishes
a link between the running application listening on port `port-number`
of `hostname`. If keyword arguments `inbuf` and `outbuf` describe
the buffer to be used. Each can either be:


  * A positive fixnum, this gives the size of the buffer.
  * The boolean `#t`, a buffer is allocated by the Bigloo runtime system
      with a default size.
  * The boolean `#f`, the socket is unbufferized.
  * A string, it is used as buffer.

Unbuffered sockets are useful for socket clients connected to servers
that do not emit `#\Newline` character after emissions. If the optional
argument `timeout` is missing or is `0`, the execution blocks
until the connection is established. If the `timeout` is provided,
the execution unblocks after `timeout` microseconds unless the
connection is established.

The `domain` argument specifies the protocol used by the socket.
The supported domains are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.
  * `unix`: Unix sockets for local inter-process communications.
  * `local`: Same as `unix`.
  * `unspec`: uses IPv4 or IPv6 as determined by getaddrinfo.

If the connection cannot be established, an `&io-error` is raised
(see @ref{Errors Assertions and Traces}).

When a socket is used in unbufferized mode the characters available on
the input port @emph{must} be read exclusively with `read-char`
or `read-line`. It is forbidden to use `read` or any regular
grammar.  This limitation is imposed by Rgc (see @ref{Regular Parsing}) that
intrinsicly associates buffers with regular grammars. If the current Rgc
implementation is improved on the coming version this restriction will
be eliminated.

### socket-hostname ###
<!-- [:@C-jvm] -->

Returns a string which contains the name of the distant host attached to
`socket`. If `socket` has been created with `make-client-socket`
this procedure returns the official name of the distant machine used for 
connection. If `socket` has been created with `make-server-socket`,
this function returns the official name of the client connected to the socket. 
If no client has used yet the socket, this function returns `#f`.

### socket-host-address ###
<!-- [:@C-jvm] -->
Returns a string which contains the IP number of
the distant host attached to `socket`. If `socket` has been
created with `make-client-socket` this procedure returns the
IP number of the distant machine used for connection. If
`socket` has been created with `make-server-socket`, this
function returns the address of the client connected to the
socket.  If no client has used yet the socket, this function returns
`#f`.

### socket-local-address ###
<!-- [:@C-jvm] -->
Returns a string which contains the IP number of
the local host attached to `socket`.

### socket-port-number ###
<!-- [:@C-jvm] -->
Returns the integer number of the port used for `socket`.

@deffn {bigloo procedure} socket-input socket
@deffnx {bigloo procedure} socket-output socket

Returns the file port associated for reading or writing with the program 
connected with `socket`. If no connection has already been established,
these functions return `#f`.

The following example shows how to make a client socket. Here we create a
socket on port 13 of the machine ```kaolin.unice.fr`''@footnote{Port 13
is generally used for testing: making a connection to it permits to know
the distant system's idea of the time of day.}:
```bigloo
(let ((s (make-client-socket "kaolin.unice.fr" 13)))
  (print "Time is: " (read-line (socket-input s)))
  (socket-shutdown  s))
```
@end deffn

@deffn {bigloo procedure} make-server-socket #!optional (port 0) #!key (name #f) (backlog 5) (domain 'inet)
`make-server-socket` returns a new socket object. 
The socket will be listening on the network interface `name`, 
either on the specified `port`, or on a port chosen by the system
(usually the first port available on the network interface). The `name`
can be an IP number as a string, or a host name, whose first IP address will
be used (as returned by the name server lookup).

The `backlog` argument specifies the size of the wait-queue used for
accepting connections.

The `domain` argument specifies the address family to use. The supported domains
are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.
  * `unix`: Unix sockets for local inter-process communications.
  * `local`: Same as `unix`.

@end deffn

@deffn {bigloo procedure} socket-accept socket #!key (errp #t) (inbuf #t) (outbuf #t)
`socket-accept` waits for a client connection on the given
`socket`. It returns a `client-socket`.  If no client is
already waiting for a connection, this procedure blocks its caller;
otherwise, the first connection request on the queue of pending
connections is connected to `socket`. This procedure must be
called on a server socket created with `make-server-socket`. 

The arguments `inbuf` and `outbuf` are similar to the ones
used by `make-client-socket`. That is, each can either be:

  * A positive fixnum, this gives the size of the buffer.
  * The boolean `#t`, a buffer is allocated.
  * The boolean `#f`, the socket is unbufferized.
  * A string, it is used as buffer.

The keyword argument `errp` is a boolean. The value `#t`
means that if an error is raised it is signaled. Otherwise, it is
omitted.

@emph{Note:} When a socket is used in unbufferized mode the characters
available on the input port @emph{must} be read exclusively with
`read-char` or `read-line`. It is forbidden to use `read`
or any regular grammar.  This limitation is imposed by Rgc (see
@ref{Regular Parsing}) that intrinsicly associate buffers with regular
grammars. If the current Rgc implementation is improved on the coming
version this restriction will be suppressed.

The following exemple is a simple server which waits for a connection
on the port 1234@footnote{Under Unix, you can simply connect to
  listening socket with the `telnet` command. With the given
  example, this can be
  achived by typing the following command in a window shell:
  `$ telnet localhost 1234`}. Once the connection with the
distant program is established, we read a line on the input port
associated to the socket and we write the length of this line on its
output port.
```bigloo
(let* ((s (make-server-socket 1234))
       (s2 (socket-accept s)))
  (let ((l (read-line (socket-input s2))))
    (fprint (socket-output s2) "Length is: " (string-length l))
    (flush-output-port (socket-output s2)))
  (socket-close s2)
  (socket-shutdown s))
```
@end deffn

@deffn {bigloo procedure} socket-close socket
The function `socket-close` closes the connection established with
a `socket-client`.
@end deffn

@deffn {bigloo procedure} socket-shutdown socket #!optional (how #t)
`Socket-shutdown` shutdowns the connection associated to `socket`.

`Close` is either a boolean or one of the symbols `RDWR`, `RD`, or
`WR`. The meaning of the optional how (which defaults to `#t`)
is as follows:

  * `#t`, the socket is shutdown for reading and writing
@emph{and} the socket is closed.
  * `#f`, the socket is shutdown for reading and writing.
  * `RDWR`, the socket is shutdown for reading and writing.
  * `RD`, the socket is shutdown for reading.
  * `WD`, the socket is shutdown for writing.

The function `socket-shutdown` returns an integer which is `0`
is the operation has succeeded and a positive integer otherwise.
@end deffn

@deffn {bigloo procedure} socket-down? socket
Returns `#t` if `socket` has been previously closed 
with `socket-shutdown`. It returns `#f` otherwise.
@end deffn

Here is another example of making use of stream sockets:

```bigloo
(define s1 (make-server-socket))
(define s2 #unspecified)

(dynamic-wind 
   ;; Init: Launch an xterm with telnet running
   ;; on the s listening port and connect
   (lambda ()
      (run-process "/usr/X11R6/bin/xterm" "-display" ":0" "-e" "telnet" "localhost" 
		   (number->string (socket-port-number s1)))
      (set! s2 (socket-accept s1))
      (display #"\nWelcome on the socket REPL.\n\n> " (socket-output s2))
      (flush-output-port (socket-output s2)))

   ;; Action: A toplevel like loop
   (lambda ()
      (let loop ()
	 (let ((obj (eval (read (socket-input s2)))))
	    (fprint (socket-output s2) "; Result: " obj)
	    (display "> " (socket-output s2))
	    (flush-output-port (socket-output s2))
	    (loop))))

   ;; Termination: We go here when 
   ;;     -a: an error occurs 
   ;;     -b: connection is closed
   (lambda ()
      (print #"Shutdown ......\n")
      (socket-close s2)
      (socket-shutdown s1)))
```

Here is a second example that uses sockets. It implements
a client-server architecture and it uses unbufferized
(see `socket-accept`) input ports.
@cindex client-server

First, here is the code of the client:

```bigloo
(module client)

(let* ((s (make-client-socket "localhost" 8080 :outbuf #f))
       (p (socket-output s)))
   (display "string" p)
   (newline p)
   (display "abc" p)
   (flush-output-port p)
   (let loop ()
      (loop)))
```

Then, here is the code of the server:

```bigloo
(module server)

(let* ((s (make-server-socket 8080))
       (s2 (socket-accept s :inbuf #f)))
   (let ((pin (socket-input s2)))
      (let loop ()
         (display (read-char pin))
         (flush-output-port (current-output-port))
         (loop))))
```

At, to conclude here the source code for a server waiting for multiple
consecutive connections:

```bigloo
(define (main argv)
   (let ((n (if (pair? (cdr argv))
                (string->integer (cadr argv))
                10))
	 (s (make-server-socket)))
      (print "s: " s)
      (let loop ((i 0))
         (if (<fx i n)
             (let ((s2 (socket-accept s)))
		(print "i: " i " " s2)
		(print (read-line (socket-input s2)))
		(socket-close s2)
                (loop (+fx i 1)))
	     (socket-shutdown s)))))
```

Bigloo also provides primitives dealing with @dfn{datagram} sockets, for
use with transports such as UDP.  These are shown below:

@deffn {bigloo procedure} make-datagram-client-socket hostname port #!optional broadcast (domain 'inet)

return a datagram client socket connected to `hostname` on `port` whose address family is specified by `domain`.

The supported domains
are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.

@end deffn

@deffn {bigloo procedure} make-datagram-server-socket #!optional (port 0) (domain 'inet)
Return a datagram server socket listening on `port`, and whose address family is
specified by `domain`. The supported domains
are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.



@end deffn

@deffn {bigloo procedure} make-datagram-unbound-socket
Return an unbound datagram socket,whose address family is
specified by `domain`. The supported domains
are:

  * `inet`: IPv4 Internet protocols.
  * `inet6`: IPv6 Internet protocols.

It may then be used in conjunction
with `datagram-socket-send` and `datagram-socket-receive`, for
instance send to and receive from a UDP multicast address.
@end deffn

@deffn {bigloo procedure} datagram-socket-input socket
@deffnx {bigloo procedure} datagram-socket-output socket

Returns the file port associated for reading or writing with the program 
connected with datagram `socket`. If no connection has already been established,
these functions return `#f`.

@end deffn

@deffn {bigloo procedure} datagram-socket-receive sock size
Receive up to `size` bytes from datagram socket `sock`, and
return them as a string.
@end deffn

@deffn {bigloo procedure} datagram-socket-send sock message host port
Send string `message` over datagram socket `sock` to `host`
and `port`.  `host` must be a string denoting an IPv4 or IPv6
address.  On success, return the number of bytes actually sent.
@end deffn

@deffn {bigloo procedure} host hostname
@deffnx {bigloo procedure} hostinfo hostname
@cindex io-unknown-host-error

Returns the IP number of `hostname`. When `hostname` is not found,
the `io-unknown-host-error` exception is raided 
(see @ref{Errors Assertions and Traces}).

The function `hostinfo` possibly returns more information about the
host. It returns an association list made out the information about the
host. This list might contain a `name` entry, an `addresses` entry,
and a `aliases` entry.

Some back-ends (e.g., the C back-end) implements DNS caching. This may
dramatically improve the performance of intensive networking applications.
DNS caching can be control by the means of two parameters:
`bigloo-dns-enable-cache` and `bigloo-dns-cache-validity-timeout`
(see @ref{Parameters}).
@end deffn

@deffn {bigloo procedure} get-interfaces
Returns the list of configured interfaces, their associated IP addresses, their
protocol, and, if supported by the system, the hardware address (the mac address).
@end deffn

@deffn {bigloo procedure} get-protocols
Reads all the entries from the protocols database and returns a list
of protocol entries. Each entries consists in a list of three elements:

  * a string denoting the protocol name,
  * an integer denoting the protocol number,
  * a list of strings denoting the protocol aliases.

@end deffn

@deffn {bigloo procedure} get-protocol number-or-name
Returns the protocol entry found in the protocols database. The argument
`number-of-name` is either an integer or a string.
@end deffn

@deffn {bigloo procedure} socket-option socket option-name
@deffnx {bigloo procedure} socket-option-set! socket option-name val
These two functions _get_ and _set_ socket option. The
argument `option-name` must be a keyword. If the `option-name`
is not supported by the Bigloo runtime system, the function
`socket-option` returns the value `#unspecified` otherwise,
it returns the option value.  If the `option-name` is not supported,
the function `socket-option-set!` returns `false`. Otherwise
it returns a non false value.

Here is a list of possibly supported option-name values:

  * `:SO_KEEPALIVE`
  * `:SO_OOBINLINE`
  * `:SO_RCVBUF`
  * `:SO_SNDBUF`
  * `:SO_REUSEADDR`
  * `:SO_TIMEOUT`
  * `:SO_SNDTIMEO`
  * `:SO_RCVTIMEO`
  * `:TCP_CORK`
  * `:TCP_QUICKACK`
  * `:TCP_NODELAY`

The `:SO_KEEPALIVE` option can be use to implement automatic notification
of client disconnection. It requires system tuning for enabling TCP keeplive
support. On Linux additional information may be found on the 
``TCP Keepalive HOWTO'' (see @url{http://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO/}).

@end deffn

