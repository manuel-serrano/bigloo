<!--==================================================================-->
<!--    serrano/prgm/project/bigloo/5.0a/doc/digest.md                -->
<!--    ----------------------------------------------------------    -->
<!--    Author      :  manuel serrano                                 -->
<!--    Creation    :  Mon Apr 13 10:38:02 2026                       -->
<!--    Last change :                                                 -->
<!--    Copyright   :  2026 manuel serrano                            -->
<!--    -----------------------------------------------------------   -->
<!--    Digest                                                        -->
<!--==================================================================-->

,(implementation-path "../runtime/Unsafe/base64.scm")
,(example-path "../test/src/digest.bgl")


Digest & Cyclic Redundancy Check
================================

Digest
------

### base64-encode ###
Encodes a string into a base64 representation.

### base64-decode ###
Decodes a string into a base64 representation.

When decoding, if the optional parameter `no-eof-padding` is
`#t`, the decoding success even if the input stream is not padded
with `=` characters

### base64-encode-port ###
Encodes an input port into a base64 representation.

### base64-decode-port ###
Decodes an input port into a base64 representation.

When decode succeeds, `base64-decode-port` returns `#t`, it
returns `#f` otherwise.

When decoding, if the optional parameter `no-eof-padding` is
`#t`, the decoding success even if the input stream is not padded
with `=` characters.

### pem-read-file ###
Reads a PEM (Privacy Enhanced Mail) base64 encoded file.

@deffnx {bigloo procedure} pem-decode-port input-port output-port
Reads a PEM (Privacy Enhanced Mail) base64 encoded file.
@end deffn

@deffn {bigloo procedure} md5sum obj
@deffnx {bigloo procedure} md5sum-string string
@deffnx {bigloo procedure} md5sum-mmap mmap
@deffnx {bigloo procedure} md5sum-file string
@deffnx {bigloo procedure} md5sum-port input-port
Computes MD5 message digest.

The function @code{md5sum} dispatches over its argument and invokes the
ad-hoc function. That is, it invokes @code{md5sum-string} if its 
argument is a string, @code{md5sum-mmap} if it is a mmap, 
@code{md5sum-port} if its argument is an input port.
@end deffn

@deffn {bigloo procedure} hmac-md5sum-string key string
Computes the Hmac MD5 authentication:

@smalllisp
(hmac-md5sum-string (make-string 16 #a011) "Hi There") 
  @result{} "9294727a3638bb1c13f48ef8158bfc9d"
@end smalllisp
@end deffn

@deffn {bigloo procedure} cram-md5sum-string user key string
Challenge-Response Authentication Mechanism as specified in RFC 2195.

The function @code{cram-md5sum-string} assumes that data is base64 encoded.
The result is also base64 encoded.
@end deffn

@deffn {bigloo procedure} sha1sum obj
@deffnx {bigloo procedure} sha1sum-string string
@deffnx {bigloo procedure} sha1sum-mmap mmap
@deffnx {bigloo procedure} sha1sum-file string
@deffnx {bigloo procedure} sha1sum-port input-port
Computes SHA1 message digest.

The function @code{sha1sum} dispatches over its argument and invokes the
ad-hoc function. That is, it invokes @code{sha1sum-string} if its 
argument is a string, @code{sha1sum-mmap} if it is a mmap, 
@code{sha1sum-port} if its argument is an input port.
@end deffn

@deffn {bigloo procedure} hmac-sha1sum-string key string
Computes the Hmac SHA1 authentication:
@end deffn

@deffn {bigloo procedure} sha256sum obj
@deffnx {bigloo procedure} sha256sum-string string
@deffnx {bigloo procedure} sha256sum-mmap mmap
@deffnx {bigloo procedure} sha256sum-file string
@deffnx {bigloo procedure} sha256sum-port input-port
@deffnx {bigloo procedure} sha512sum obj
@deffnx {bigloo procedure} sha512sum-string string
@deffnx {bigloo procedure} sha512sum-mmap mmap
@deffnx {bigloo procedure} sha512sum-file string
@deffnx {bigloo procedure} sha512sum-port input-port
Computes SHA256 and SHA512 message digest.

The function @code{sha256sum} (respec. @code{sha512sum}) dispatches over
its argument and invokes the ad-hoc function. That is, it invokes
@code{sha256sum-string} if its argument is a string,
@code{sha256sum-mmap} if it is a mmap, @code{sha256sum-port} if its
argument is an input port.
@end deffn

@deffn {bigloo procedure} hmac-sha256sum-string key string
@deffnx {bigloo procedure} hmac-sha512sum-string key string
Computes the Hmac SHA256 and SHA512 authentication.
@end deffn

CRC
---
Bigloo provides several known cyclic redundancy checks as well as means to create custom
checks.

Usually CRCs are executed starting with the leftmost bit inside a byte (big endian). However,
especially for serial-port transmissions, a scheme where the least-significant bit is
processed first is desirable. Bigloo's CRC procedures accept a key-parameter
(@code{:big-endian}) (by default @code{#t}) which allows to change this behavior.

The following CRCs (given with the associated polynomial) are provided:
@itemize @bullet
@item @code{itu-4}: 0x3
@item @code{epc-5}: 0x9
@item @code{itu-5}: 0x15
@item @code{usb-5}: 0x5
@item @code{itu-6}: 0x3
@item @code{7}: 0x9
@item @code{atm-8}: 0x7
@item @code{ccitt-8}: 0x8d
@item @code{dallas/maxim-8}: 0x31
@item @code{8}: 0xd5
@item @code{sae-j1850-8}: 0x1d
@item @code{10}: 0x233
@item @code{11}: 0x385
@item @code{12}: 0x80f
@item @code{can-15}: 0x4599
@item @code{ccitt-16}: 0x1021
@item @code{dnp-16}: 0x3d65
@item @code{ibm-16}: 0x8005
@item @code{24}: 0x5d6dcb
@item @code{radix-64-24}: 0x864cfb
@item @code{30}: 0x2030b9cf
@item @code{ieee-32}: 0x4c11db7
@item @code{c-32}: 0x1edc6f41
@item @code{k-32}: 0x741b8cd7
@item @code{q-32}: 0x814141ab
@item @code{iso-64}: 0x1b
@item @code{ecma-182-64}: 0x42f0e1eba9ea3693
@end itemize

@deffn {bigloo procedure} crc-names
Returns a list of all provided CRCs (@code{itu-4}, @code{epc-5}, etc.).
@end deffn

@deffn {bigloo procedure} crc-polynomial name
@deffnx {bigloo procedure} crc-polynomial-le name
Returns the polynomial for the given name. The @code{-le} variant returns the
little endian polynomial.

@smalllisp
(crc-polynomial 'ieee-32)
    @print{} #e79764439 ;; == #ex4c11bd7
(crc-polynomial 24)
    @print{} 6122955    ;; == #x5d6dcb
@end smalllisp
@end deffn

@deffn {bigloo procedure} crc-length name
Returns the length of the specified CRC.
@end deffn

@deffn {bigloo procedure} crc name
@deffnx {bigloo procedure} crc-string
@deffnx {bigloo procedure} crc-port
@deffnx {bigloo procedure} crc-mmap
@deffnx {bigloo procedure} crc-file
Computes the CRC of the given object. @var{name} must be one of the
provided CRC-algorithms. The optional parameter @var{init} can be used to
initialize the CRC. The result of the CRC will be XORed with @var{final-xor}. The result
will however be of the CRC's length. That is, even if @var{final-xor} is bigger then
the CRC's length only the relevant bits will be used to perform the final XOR.

The result will be a number. Depending on the CRC this number can be a fixnum,
an elong, or an llong.

The following example mimicks the UNIX @code{cksum} command:
@smalllisp
(module cksum (main main))
(define (main args)
  (let loop ((sum (crc-file 'ieee-32 (cadr args)))
             (size (elong->fixnum (file-size (cadr args)))))
    (if (=fx size 0)
        (printf "~a ~a ~a\n"
                (bit-andllong #lxFFFFFFFF (elong->llong (bit-notelong sum)))
                (file-size (cadr args))
                (cadr args))
        (loop (crc-string 'ieee-32
                          (string (integer->char-ur (bit-and size #xFF)))
                          :init sum)
	      (bit-rsh size 8)))))
@end smalllisp

In the following example we implement OpenPGP's CRC-24:
@smalllisp
(define (openpgp-crc-24 str)
  (crc-string 'radix-64-24 str :init #xB704CE))
@end smalllisp

Be aware that many common CRCs use -1 as init value and invert the result. For
compatibility with other implementations you might want to try
one of the following alternatives:
@smalllisp
(define (alt1 name obj) (crc name obj :init -1))
(define (alt2 name obj) (crc name obj :final-xor -1))
(define (alt3 name obj) (crc name obj :init -1 :final-xor -1))
@end smalllisp

@end deffn

Bigloo provides means to create additional CRCs: one can either simply provide
a new polynomial or use Bigloo's low level functions.

@deffn {bigloo procedure} register-crc! name poly len
Adds the given CRC to Bigloo's list. Name can be of any type (@code{crc} will
use @code{assoc} to find it in its list). The polynomial can be either a
fixnum, an elong or an llong. @var{len} should give the CRCs size. The
type of the polynomial and the given @var{len} must be consistent. On a 32 bit
machine the following CRC registration would be invalid and yield undefined
results:

@smalllisp
(register-crc! 'invalid 1337 55)
@end smalllisp

As 55 is bigger than the fixnum's bit-size calling @code{crc} with this CRC will
yield undefinde results.
@end deffn

@deffn {bigloo procedure} crc-long::long c::char crc::long poly::long len::long
@deffnx {bigloo procedure} crc-elong::elong c::char crc::elong poly::elong len::long
@deffnx {bigloo procedure} crc-llong::llong c::char crc::llong poly::llong len::long
@deffnx {bigloo procedure} crc-long-le::long c::char crc::long poly::long len::long
@deffnx {bigloo procedure} crc-elong-le::elong c::char crc::elong poly::elong len::long
@deffnx {bigloo procedure} crc-llong-le::llong c::char crc::llong poly::llong len::long
These function perform a CRC operation on one byte. The previously described functions
are based on these low level functions. The result of all the low level functions
will return values that are not cut to the correct length. Usually a crc is done in
a loop, and one needs to @code{bit-and} only when returning the result.
Polynomials can be given with or without the high-order bit.

For instance we could implement @code{openpgp-crc24} as follows:
@smalllisp
(define *openpgp-init* #xB704CE)
(define *radix-64-24-poly* #x864CFB)
(define (openpgp-crc-24 str)
  (let loop ((i 0)
             (crc *openpgp-init*))
    (if (=fx i (string-length str))
        (bit-and crc #xFFFFFF) ;; cut to correct length (24 bits)
        (loop (+fx i 1)
              (crc-long (string-ref str i) crc *radix-64-24-poly* 24)))))
@end smalllisp
@end deffn

@deffn {bigloo procedure} crc-polynomial-be->le len polynomial
Returns the little endian variant of a given polynomial.
