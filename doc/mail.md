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
,(implementation-path "../api/mail/src/Llib/rfc2822.scm")
,(example-path "../test/src/mail0.bgl")

Email
=====

Bigloo provides various facilities for handling emails. It provides
facilities for parsing many formats commonly used in composing mails
(quoted printable, vcard, mime types). It also provides facilities for
dealing with mail servers. For that it proposes an abstracted view of
mail servers with two implementations: `imap` and `maildir`.

  * RFC 2045:: MIME Part one.
  * RFC 2047:: MIME, Part three.
  * RFC 2426:: vcard.
  * RFC 2822:: Internet Message Format
  * Mail servers:: `imap` and `maildir`
  
> [!IMPORTANT] A module using it must include in its declaration 
> the clause `(library mail)`. Example:

```bigloo
(module mail-ex
  (library mail)
  ...)
```

RFC 2025
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

mime-multipart-decode 
mime-multipart-decode-port 
These two functions parse respectively a @code{string} and an 
@code{input-port} and return a list of mime sections.

If the optional argument @var{recursive} controls whether subparts of
a multipart section must be decoded are not. If the @var{recursive} is 
@code{#t} then all subparts of the multipart content are decoded. The result
is a fully decoded multipart section. If @var{recursive} is @code{#f} subparts
are not decoded and included in the result as plain strings.
@end deffn

@c ------------------------------------------------------------------- @c
@c    RFC 2047 ...                                                     @c
@c ------------------------------------------------------------------- @c
@node RFC 2047, RFC 2426, RFC 2045, Mail
@comment  node-name,  next,  previous, up
@section RFC 2047 -- MIME, Part three
@cindex RFC 2426

This section described the function offered by Bigloo to decode
the RFC 2047 encoding used in mail headers 
(see @url{http://tools.ietf.org/html/rfc2047}).

rfc2047-decode-port
rfc2047-decode

These functions decode mail header fields encoded using the RFC 2047 
specification. The optional argument @var{charset} specified in which charset
the result should be encoded. The allowed values are:

@itemize @bullet
@item @code{utf-8}
@item @code{iso-latin-1}
@item @code{cp-1252}
@end itemize

Example:

@smalllisp
(map char->integer
  (string->list (rfc2047-decode "Poste =?ISO-8859-1?Q?t=E9l=E9phonique?=")))
  @result{} (80 111 115 116 101 32 116 233 108 233 112 104 111 110 105 113 117 101)
(string-for-read (rfc2047-decode "Poste =?ISO-8859-1?Q?t=E9l=E9phonique?=" :charset 'utf8))
  @result{} "Poste t\303\251l\303\251phonique"
@end smalllisp
@end deffn

@c ------------------------------------------------------------------- @c
@c    RFC 2426 ...                                                     @c
@c ------------------------------------------------------------------- @c
@node RFC 2426, RFC 2822, RFC 2047, Mail
@comment  node-name,  next,  previous, up
@section RFC 2426 -- MIME, Part three
@cindex RFC 2426
@cindex VCARD

This section presents the facilities supported by Bigloo for dealing
with @code{vcard}s.

@deffn {bigloo mail class} vcard
@smalllisp
(class vcard
  (version::bstring (default "2.1"))
  (uid (default #f))
  (fn (default #f))
  (familyname (default #f))
  (firstname (default #f))
  (nickname (default #f))
  (photo (default #f))
  (sound (default #f))
  (url (default #f))
  (org (default #f))
  (emails::pair-nil (default '()))
  (phones::pair-nil (default '()))
  (birthday (default #f))
  (addresses::pair-nil (default '()))
  (lang (default #f))
  (related (default #f))
  (key (default #f))
  (notes::pair-nil (default '()))
  (x-thumbnail (default #f))
  (x-color (default #f))
  (xx-extras::pair-nil (default '())))
@end smalllisp

The class @code{vard} is used to reify in memory a vcard as parsed by
the functions @code{port->vcard}, @code{read-vcard}, and @code{string->vcard}.

Except @code{emails}, @code{phones}, and @code{addresses}, all fields
are optional. They should be either @code{#f} or a string. Field meanings
are given by the RFC 2426 specification, apart the @code{x-thumbnail},
@code{x-color}, and @code{xx-extras} that are Bigloo specific.

@itemize @bullet
@item @code{photo} is a flat list of strings.
@item @code{x-thumbnail} an optional thumbnail image.
@item @code{x-color} an optional color.
@item @code{xx-extras} a list of non standard properties.
@item @code{phones} is an alist whose elements are pairs of two strings.
@item @code{addresses} is a list composed of:
  @itemize @bullet
  @item the postoffice, a string, 
  @item a list of strings denoting the street address,
  @item a string denoting the city,
  @item a string denoting the region,
  @item a string denoting the zip code,
  @item a string denoting the zip country.
@end itemize

All street values are required and must be provided. The empty string
should be used to denote empty values.
@end itemize
@end deffn

@deffn {bigloo mail function} read-vcard::vcard ip [:charset-encoder]
@deffnx {bigloo mail function} port->vcard::vcard ip [:charset-encoder]
@deffnx {bigloo mail function} string->vcard::vcard str [:charset-encoder]
These three functions parse a @emph{vcard} to produce a @code{vcard}
instance.  The optional argument @var{charset-encoder}, when provided,
must be a function of argument: a string to be decoded. Vcard strings
are UTF-8 encoded. The @var{charset-encoder} can be used to encode
on-the-fly the strings found in the vcard in a difference encoding.

@code{port->vcard} is similar to @code{read-vcard} but it returns
@code{#f} on end-of-file, while @code{read-vcard} returns the
eof-object.
@end deffn


@c ------------------------------------------------------------------- @c
@c    RFC 2822 ...                                                     @c
@c ------------------------------------------------------------------- @c
@node RFC 2822, Mail servers, RFC 2426, Mail
@comment  node-name,  next,  previous, up
@section RFC 2822 -- Internet Message Format
@cindex RFC 2822

This section described the functions offered by Bigloo to encode
and decode some of the formats specified in the RFC 2822
(@url{http://tools.ietf.org/html/rfc2045}). It mainly supports functions
for parsing email headers and for decoding email addresses.

mail-header->list obj
The function @code{mail-header->list} parses a mail header that can either
be implemented as a string or an input port. It returns a list of fields.

Example:

```
(mail-header->list "Return-Path: <foo.bar@@inria.fr>
Received: from eurus.inria.fr ([unix socket])")
  @result{}
  ((return-path . "<foo.bar@@inria.fr>") (received . "from eurus.inria.fr ([unix socket])"))
```
@end deffn

### email-normalize ###
Extracts the actual email address from an email representation.

### rfc2822-address-display-name ###
Extracts the name component of an email.

RFC 3501
--------

Bigloo implements the @code{imap} protocol
(@url{http://tools.ietf.org/html/rfc3501}) and the @code{maildir}
format. This section presents the API for manipulating them both.

@menu
* mailbox:: main API for dealing with messages and folders.
* imap:: implementation of the RFC 3501 protocol.
* maildir:: implementation of the mail file system format.
@end menu

@c -- mailbox -------------------------------------------------------- @c
@node mailbox, , imap, Mail servers
@subsection Mailboxes

@deffn {bigloo mail class} mailbox
@smalllisp
(abstract-class mailbox
  (label::bstring (default "")))
@end smalllisp

The abstract class @code{mailbox} is the common ancestors to all the
mailbox implementations. It allows the definitions of various generic
functions that deal with mail messages and mail folders.
@end deffn

@deffn {bigloo mail class} &mailbox-error
@smalllisp
(abstract-class &mailbox-error::&error)
@end smalllisp

The @code{&mailbox-error} is the super class of all the errors that
can be raised when accessing mail servers, except the parsing errors
that inherit from the @code{&parse-error} super class.
@end deffn


mailbox-close mailbox
Close the mailbox connection.

Example:
@smalllisp
(let ((mbox (if (network-up?)
                (instantiate::imap (socket ...))
                (instantiate::maildir (path my-local-cache)))))
   (mailbox-close mbox))
@end smalllisp

@end deffn

mailbox-separator mailbox
Returns a string denoting the separator (commonly @code{"} or @code{.})
used by the @var{mailbox}.
@end deffn

mailbox-prefix mailbox
Returns the prefix of the @var{mailbox}, a string or @code{#f}.
@end deffn

mailbox-hostname mailbox
Returns the hostname of the @var{mailbox}, a string or @code{#f}.
@end deffn

mailbox-folders mailbox
Returns a list of strings denoting the folder names of the @var{mailbox}.
@end deffn

mailbox-folder-select! mailbox string
Selects one folder of the @var{mailbox}. This function is central to 
mailboxes because all messages are referenced relatively to the 
folder selection. All the functions that operates on @code{uid} 
implicitly access the current folder selection.
@end deffn

mailbox-folder-unselect! mailbox
Unselects the @var{mailbox} current selected folder.
@end deffn

mailbox-folder-create! mailbox folder
Creates a new @var{folder} denotes by a fully qualified name.

Example
@smalllisp
(mailbox-create! mbox "INBOX.scheme.bigloo")
@end smalllisp
@end deffn

mailbox-folder-delete! mailbox folder
Deletes an empty @var{folder}.
@end deffn

mailbox-folder-rename! mailbox old new
Renames a folder.
@end deffn

mailbox-folder-move! mailbox folder dest
Moves the @var{folder} into the destination folder @var{dest}.
@end deffn

mailbox-subscribe! mailbox folder
mailbox-unsubscribe! mailbox folder
Subscribe/unsubscribe to a folder. This allows @code{imap} servers not
to present the entire list of folders. Only subscribed folders are returned
by @code{mailbox-folders}. These functions have no effect on @code{maildir}
servers.
@end deffn

mailbox-folder-exists? mailbox folder
Returns @code{#t} if and only if @var{folder} exists in @var{mailbox}. Returns
@code{#f} otherwise.
@end deffn

mailbox-folder-status mailbox folder
Returns the status of the @var{folder}. A status is an alist made of
the number of unseen mail, the uid validity information, the uid next
value, the number of recent messages, and the overall number of messages.
@end deffn

mailbox-folder-uids mailbox
Returns the list of UIDs (a list of integers) of the messages contained
in the currently selected folder.
@end deffn

mailbox-folder-dates mailbox
Returns the list of dates of the messages contained
in the currently selected folder.
@end deffn

mailbox-folder-delete-messages! mailbox
Deletes the messages marked as @emph{deleted} of the currently selected
folder.
@end deffn

mailbox-folder-header-fields mailbox field
Returns the list of headers @var{fields} of the message of the current
folder.
@end deffn

mailbox-message mailbox uid
Returns the message @var{uid} in the current folder.
@end deffn

mailbox-message-path mailbox uid
Returns the full path name of the message @var{uid}.
@end deffn

mailbox-message-body
Returns the body of the message @var{uid}. If @var{len} is provided, only
returns the first @var{len} characters of the body.
@end deffn

mailbox-message-header mailbox uid
Returns the header as a string of the message @var{uid}.
@end deffn

mailbox-message-header-list mailbox uid
Returns the header as an alist of the message @var{uid}.
@end deffn

mailbox-message-header-field mailbox uid field
Extracts one field from the message header.
@end deffn

mailbox-message-size mailbox uid
Returns the size of the message.
@end deffn

mailbox-message-info mailbox uid
Returns the information relative to the message @var{uid}. This a list
containing the message identifier, its uid, the message date, the message
size, and the message flags.
@end deffn

mailbox-message-flags mailbox uid
mailbox-message-flags-set! mailbox uid lst
Sets/Gets the flags of the message @var{uid}. This is a list of strings.
Typical flags are: 
  
  * `\Flagged`
  * `\Answered`
  * `\Deleted`
  * `\Seen`

mailbox-message-delete! mailbox uid
Deletes the message @var{uid}.
@end deffn

mailbox-message-move! mailbox uid folder
Moves the message @var{uid} into the new @var{folder} (denoted by 
a string).
@end deffn

mailbox-message-create! mailbox folder content
Creates a new message in the @var{folder} whose content is given the
string @var{content}.
@end deffn


@c -- imap ----------------------------------------------------------- @c
@node imap, mailbox, maildir, Mail servers
@subsection IMAP (RFC 3501)

@deffn {bigloo mail class} imap
@smalllisp
(class imap::mailbox
  (socket::socket read-only))

(define mbox
  (instantiate::maildir
    (label "My Remote Mailbox")
    (socket (imap-login (make-client-socket "imap.inria.fr" 993)
                        "serrano" "XXX"))))
@end smalllisp
@end deffn

@deffn {bigloo mail class} &imap-parse-error
@smalllisp
(class &imap-parse-error::&io-parse-error)
@end smalllisp
@end deffn

@deffn {bigloo mail class} &imap-error
@smalllisp
(class &imap-error::&mailbox-error)
@end smalllisp
@end deffn

imap-login socket user password
Log a user into an imap server. The @var{socket} must have been created
first. The argument @var{user} is a string and denotes the user name.
The argument @var{password} is a string too and it contains the user
password. This function returns as value the @var{socket} it has received.
If the operation fails the function raises a @code{&imap-error}
exception.

Example:

@smalllisp
(define mbox
   (imap-login (make-client-socket "imap.inria.fr" 993 :timeout 200000) 
               "serrano" "XXX"))

(print (mailbox-folders mbox))
@end smalllisp
@end deffn

imap-logout socket
Closes an @code{imap} connection.
@end deffn

imap-capability socket
Returns the list of capabilities supported the @code{imap} server.
@end deffn



@c -- maildir -------------------------------------------------------- @c
@node maildir, imap,  , Mail servers
@subsection Maildir

@deffn {bigloo mail class} maildir
@smalllisp
(class maildir::mailbox
  (prefix::bstring read-only (default "INBOX"))
  (path::bstring read-only))
@end smalllisp

Example:

@smalllisp
(define mbox
  (instantiate::maildir
    (label "My Mailbox")
    (path (make-file-name (getenv "HOME") ".maildir"))))

(tprint (mailbox-folders mbox))
@end smalllisp
@end deffn

@deffn {bigloo mail class} &maildir-error
@smalllisp
(class &maildir-error::&mailbox-error)
@end smalllisp
@end deffn


