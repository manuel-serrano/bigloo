<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/mail.md                  -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Email library                                                 -->
<!--==================================================================-->

,(implementation-path "../api/mail/src/Llib/rfc2045.scm")
,(implementation-path "../api/mail/src/Llib/rfc2047.scm")
,(implementation-path "../api/mail/src/Llib/rfc2822.scm")
,(implementation-path "../api/mail/src/Llib/vcard.scm")
,(implementation-path "../api/mail/src/Llib/mailbox.scm")
,(implementation-path "../api/mail/src/Llib/maildir.scm")
,(implementation-path "../api/mail/src/Llib/imap.bgl")
,(implementation-path "../api/mail/src/Llib/imap.stk")
,(example-path "../test/src/mail0.bgl")

Email
=====

> [!IMPORTANT] A module using it must include in its declaration 
> the clause `(library mail)`. Example:

```bigloo
(module mail-ex
  (library mail)
  ...)
```

Bigloo provides various facilities for handling emails. It provides
facilities for parsing many formats commonly used in composing mails
(quoted printable, vcard, mime types). It also provides facilities for
dealing with mail servers. For that it proposes an abstracted view of
mail servers with two implementations: `imap` and `maildir`.

  * [RFC 2045](http://tools.ietf.org/html/rfc2045): MIME Part one.
  * [RFC 2047](http://tools.ietf.org/html/rfc2047): MIME, Part three.
  * [RFC 2426](http://tools.ietf.org/html/rfc2426): vcard.
  * [RFC 2822](http://tools.ietf.org/html/rfc2822): Internet Message Format.
  * [RFC 3501](http://tools.ietf.org/html/rfc3501): Imap client.
  * Maildir format.
  
RFC 2045
--------

This section described the functions offered by Bigloo to encode
and decode some of the formats specified in the 
[RFC 2045](http://tools.ietf.org/html/rfc2045).

### quoted-printable-encode ###
Encodes a string into the `quoted-printable` format.

### quoted-printable-decode ###
Decodes a  quoted-printable string string.

### quoted-printable-encode-port ###
As `quoted-printable-encode` but operates on ports.

### quoted-printable-decode-port ###
As `quoted-printable-decode` but operates on ports.

The function `quoted-printable-decode-port` accepts an optional
argument: `rfc2047`. If this argument is `#t`, then the parsing
stops on the prefix `?=`, which is a marker in the mail subject
as specified by the [RFC 2047](http://tools.ietf.org/html/rfc2047)
is found.

### mime-content-type-decode ###
Decodes a mime content and returns three elements:

  * a content type;
  * a content subtype
  * options.
  
### mime-content-type-decode-port ###
As `mime-content-type-decode` but operates on input ports.

### mime-content-disposition-decode ###
Parses its string argument and returns a list describint the
content disposition.

### mime-content-disposition-decode-port ###
Similar to `mime-content-disposition-decode` but operates on input ports.

### mime-multipart-decode ###
Parses a string and return a list of mime sections.

The optional argument `recursive` controls whether subparts of
a multipart section must be decoded are not. If the `recursive` is 
`#t` then all subparts of the multipart content are decoded. The result
is a fully decoded multipart section. If @var{recursive} is `#f` subparts
are not decoded and included in the result as plain strings.

If the optional argument `quiet` is `#f` a warning message is displayed
on parse errors. Otherwise, errors are silently ignored.

### mime-multipart-decode-port ###
Similar to `mime-multipart-decode` but operates on an input port.

RFC 2047
--------

This section described the function offered by Bigloo to decode
the RFC 2047 encoding used in mail headers 
(see @url{}).

### rfc2047-decode ###
This function decodes mail header fields encoded using the 
[RFC 2047](http://tools.ietf.org/html/rfc2047)
specification. The optional argument `charset` specified in which charset
the result should be encoded. The supported values are:

  * utf-8
  * iso-latin-1
  * cp-1252
  
### rfc2047-decode-port ###
Similar to `rfc2047` but operates on input ports.

These functions decode mail header fields encoded using the RFC 2047 
specification. The optional argument @var{charset} specified in which charset
the result should be encoded. The allowed values are:

RFC 2426
--------

### vcard ###

The class `vard` is used to reify in memory a vcard as parsed by
the functions `port->vcard`, `read-vcard`, and `string->vcard`.

Except `emails`, `phones`, and `addresses`, all fields are
optional. They should be either `#f` or a string. Field meanings are
given by the [RFC 2426](http://tools.ietf.org/html/rfc2426).
specification, apart the `x-thumbnail`, `x-color`, and `xx-extras`
that are Bigloo specific.

  * `photo` is a flat list of strings.
  * `x-thumbnail` an optional thumbnail image.
  * `x-color` an optional color.
  * `xx-extras` a list of non standard properties.
  * `phones` is an alist whose elements are pairs of two strings.
  * `addresses` is a list composed of:
    * the postoffice, a string, 
    * a list of strings denoting the street address,
    * a string denoting the city,
    * a string denoting the region,
    * a string denoting the zip code,
    * a string denoting the zip country.

All street values are required and must be provided. The empty string
should be used to denote empty values.

### read-vcard ###
The function `read-vcard` parses a vcard to produce a `vcard` instance.
The optional argument `charset-encoder`, when provided,
must be a function of argument: a string to be decoded. Vcard strings
are UTF-8 encoded. The `charset-encoder` can be used to encode
on-the-fly the strings found in the vcard in a difference encoding.

### port->vcard ###
The function `port->vcard` is similar to `read-vcard` but it returns
`#f` on end-of-file, while `read-vcard` returns the
eof-object.

### string->vcard ###
As `port->vcard` but parses the vcard from the string argument.


RFC 2822
--------

### mail-header->list ###
The function `mail-header->list` parses a mail header that can either
be implemented as a string or an input port. It returns a list of fields.

### email-normalize ###
Extracts the actual email address from an email representation.

### rfc2822-address-display-name ###
Extracts the name component of an email.


RFC 3501
--------

Bigloo implements the [imap protocol](http://tools.ietf.org/html/rfc3501) 
and the `maildir` format. This section presents the API for manipulating 
them both.

### mailbox ###

The abstract class `mailbox` is the common ancestors to all the
mailbox implementations. It allows the definitions of various generic
functions that deal with mail messages and mail folders.

### &mailbox-error ###

The `&mailbox-error` is the super class of all the errors that
can be raised when accessing mail servers, except the parsing errors
that inherit from the `&parse-error` super class.

### maildir ###

### &maildir-error ###

Errors relative to `maildir` mail boxes.

### mailbox-close ###
Closes a mailbox connection.

### mailbox-separator ###

Returns a string denoting the separator (commonly `"` or `.`)
used by the mailbox `m`.

### mailbox-prefix ###
Returns the prefix of the mailbox `m`, a string or `#f`.

### mailbox-hostname ###
Returns the hostname of the mailbox `m`, a string or `#f`.

### mailbox-folders ###
Returns a list of strings denoting the folder names of the `mailbox`.

### mailbox-folder-exists? ###
Returns `#t` if and only if `folder` exists in `mailbox`. Returns
`#f` otherwise.

### mailbox-folder-select! ###
Selects one folder of the mailbox `m`. This function is central to 
mailboxes because all messages are referenced relatively to the 
folder selection. All the functions that operates on `uid`
implicitly access the current folder selection.

### mailbox-folder-unselect! ###
Unselects the mailbox `m` current selected folder.

### mailbox-folder-create! ###
Creates a new `folder` denotes by a fully qualified name.

### mailbox-folder-delete! ###
Deletes an empty `folder` from `m`.

### mailbox-folder-rename! ###
Renames a folder.

### mailbox-folder-move! ###
Moves the `folder` into the destination folder `dest`.

### mailbox-folder-subscribe! ###
Subscribes to a folder. This allows `imap` servers not
to present the entire list of folders. Only subscribed folders are returned
by `mailbox-folders`. These functions have no effect on `maildir1
servers.

### mailbox-folder-unsubscribe! ###
Unsubscribes to a folder.

### mailbox-folder-status ###
Returns the status of the `folder`. A status is an alist made of
the number of unseen mail, the uid validity information, the uid next
value, the number of recent messages, and the overall number of messages.

### mailbox-folder-uids ###
Returns the list of UIDs (a list of integers) of the messages contained
in the currently selected folder.

### mailbox-folder-dates ###
Returns the list of dates of the messages contained
in the currently selected folder.

### mailbox-folder-header-fields ###
Returns the list of headers `fields` of the message of the current
folder.

### mailbox-message ###
Returns the message `uid` in the current folder.

### mailbox-message-path ###
Returns the full path name of the message `uid`.

### mailbox-message-body ###
Returns the body of the message `uid`. If `len` is provided, only
returns the first `len` characters of the body.

### mailbox-message-header ###
Returns the header as a string of the message `uid`.

### mailbox-message-header-list ###
Returns the header as an alist of the message `uid`.

### mailbox-message-header-field ###
Extracts one field from the message header.

### mailbox-message-size ###
Returns the size of the message.

### mailbox-message-info ###
Returns the information relative to the message `uid`. This a list
containing the message identifier, its uid, the message date, the message
size, and the message flags.

### mailbox-message-flags ###
Gets the flags of the message `uid`. This is a list of strings.
Typical flags are: 
  
  * `\Flagged`
  * `\Answered`
  * `\Deleted`
  * `\Seen`

### mailbox-message-flags-set! ###
Setes the message flags.

### mailbox-message-move! ###
Moves the message `uid` into the new `folder` (denoted by 
a string).

### mailbox-message-delete! ###
Deletes the message `uid`.
@end deffn

### mailbox-folder-delete-messages! ###
Deletes the messages marked as `deleted` of the currently selected
folder.

### mailbox-message-create! 
Creates a new message in the `folder` whose content is given the
string `content`.

RFC3501 (IMAP)
--------------

### imap ###
<!-- [:@NoTest] -->

The class representing an imap connection. 

Example:

```bigloo
(define mbox
  (instantiate::imap
    (label "My Remote Mailbox")
    (socket (imap-login (make-client-socket "imap.inria.fr" 993)
                        "amy" "XXX"))))
```

<span></span>

### &imap-parse-error ###

### &imap-error ###

### imap-login ###
<!-- [:@NoTest] -->

Log a user into an imap server. The `socket` must have been created
first. The argument `user` is a string and denotes the user name.
The argument `password` is a string too and it contains the user
password. This function returns as value the `socket` it has received.
If the operation fails the function raises a `&imap-error`
exception.

Example:

```bigloo
(define mbox
   (imap-login (make-client-socket "imap.inria.fr" 993 :timeout 200000) 
               "john" "XXX"))

(print (mailbox-folders mbox))
```

### imap-logout ###
<!-- [:@NoTest] -->
Closes an `imap` connection.

### imap-capability ###
<!-- [:@NoTest] -->
Returns the list of capabilities supported the `imap` server.


### maildir ###
Creates a `maildir` object.

### &maildir-error ###
Maildir errors.


