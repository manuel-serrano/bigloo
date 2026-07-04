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
,(implementation-path "../runtime/Unsafe/md5.scm")
,(implementation-path "../runtime/Unsafe/sha1.scm")
,(implementation-path "../runtime/Unsafe/sha2.scm")
,(implementation-path "../runtime/Unsafe/crc.scm")
,(example-path "../test/src/digest.bgl")
,(example-path "../test/src/crc.bgl")


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

### md5sum ###
The function `md5sum` dispatches over its argument and invokes the
ad-hoc function. That is, it invokes `md5sum-string` if its 
argument is a string, `md5sum-mmap` if it is a mmap, 
`md5sum-port` if its argument is an input port.


### md5sum-string ###
Computes MD5 message digest of the string argument.

### md5sum-mmap ###
Computes MD5 message digest of the mmap argument.

### md5sum-file ###
Computes MD5 message digest of the file argument.

### md5sum-port ###
Computes MD5 message digest of the file argument.

### hmac-md5sum-string ###
Computes the Hmac MD5 authentication:

### cram-md5sum-string ###
Challenge-Response Authentication Mechanism as specified in RFC 2195.

The function `cram-md5sum-string` assumes that data is base64 encoded.
The result is also base64 encoded.

### sha1sum ###
The function `sha1sum` dispatches over its argument and invokes the
ad-hoc function. That is, it invokes `sha1sum-string` if its 
argument is a string, `sha1sum-mmap` if it is a mmap, 
`sha1sum-port` if its argument is an input port.

### sha1sum-string ###
Computes SHA1 message digest of a string.

### sha1sum-mmap ###
Computes SHA1 message digest of an mmap.

### sha1sum-file ###
Computes SHA1 message digest of a file.

### sha1sum-port ###
Computes SHA1 message digest of an input-port.

### hmac-sha1sum-string ###
Computes the Hmac SHA1 authentication:

### sha256sum ###
The function `sha256sum` dispatches over its argument and invokes the
ad-hoc function. That is, it invokes `sha256sum-string` if its 
argument is a string, `sha256sum-mmap` if it is a mmap, 
`sha256sum-port` if its argument is an input port.

### sha256sum-string ###
Computes SHA256 message digest of a string.

### sha256sum-mmap ###
Computes SHA256 message digest of an mmap.

### sha256sum-file ###
Computes SHA256 message digest of a file.

### sha256sum-port ###
Computes SHA256 message digest of an input-port.

### hmac-sha256sum-string ###
Computes the Hmac SHA256 authentication:

### sha512sum ###
The function `sha512sum` dispatches over its argument and invokes the
ad-hoc function. That is, it invokes `sha512sum-string` if its 
argument is a string, `sha512sum-mmap` if it is a mmap, 
`sha512sum-port` if its argument is an input port.

### sha512sum-string ###
Computes SHA512 message digest of a string.

### sha512sum-mmap ###
Computes SHA512 message digest of an mmap.

### sha512sum-file ###
Computes SHA512 message digest of a file.

### sha512sum-port ###
Computes SHA512 message digest of an input-port.

### hmac-sha512sum-string ###
Computes the Hmac SHA512 authentication:


CRC
---
Bigloo provides several known cyclic redundancy checks as well as
means to create custom checks.

Usually CRCs are executed starting with the leftmost bit inside a byte
(big endian). However, especially for serial-port transmissions, a
scheme where the least-significant bit is processed first is
desirable. Bigloo's CRC procedures accept a key-parameter
(`:big-endian`) (by default `#t`) which allows to change
this behavior.

The following CRCs (given with the associated polynomial) are provided:

  * `itu-4`: 0x3
  * `epc-5`: 0x9
  * `itu-5`: 0x15
  * `usb-5`: 0x5
  * `itu-6`: 0x3
  * `7`: 0x9
  * `atm-8`: 0x7
  * `ccitt-8`: 0x8d
  * `dallas/maxim-8`: 0x31
  * `8`: 0xd5
  * `sae-j1850-8`: 0x1d
  * `10`: 0x233
  * `11`: 0x385
  * `12`: 0x80f
  * `can-15`: 0x4599
  * `ccitt-16`: 0x1021
  * `dnp-16`: 0x3d65
  * `ibm-16`: 0x8005
  * `24`: 0x5d6dcb
  * `radix-64-24`: 0x864cfb
  * `30`: 0x2030b9cf
  * `ieee-32`: 0x4c11db7
  * `c-32`: 0x1edc6f41
  * `k-32`: 0x741b8cd7
  * `q-32`: 0x814141ab
  * `iso-64`: 0x1b
  * `ecma-182-64`: 0x42f0e1eba9ea3693

### crc-names ###
Returns a list of all provided CRCs (`itu-4`, `epc-5`, etc.).

### crc-polynomial ###
Returns the polynomial for the given name. 

### crc-polynomial-le ###
Returns the little endian polynomial for the given name.

### crc-polynomial-be->le ###
Returns the little endian variant of a given polynomial.

### crc-length ###
Returns the length of the specified CRC. Returns `0`, if `name` is not
a CRC name.

### crc ###
Computes the CRC of the given object. The argument `name` must be one of the
provided CRC-algorithms. The optional parameter `init` can be used
to initialize the CRC. The result of the CRC will be XORed with
`final-xor`. The result will however be of the CRC's length. That
is, even if `final-xor` is bigger then the CRC's length only the
relevant bits will be used to perform the final XOR.

The result will be a number. Depending on the CRC this number can be a
fixnum, an elong, or an llong.

The following example mimicks the UNIX `cksum` command:

```bigloo
(module cksum (main))
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
```

The following example implements OpenPGP's CRC-24:

```bigloo
(define (openpgp-crc-24 str)
  (crc-string 'radix-64-24 str :init #xB704CE))
```

> [!WARNING] Be aware that many common CRCs use -1 as init value and invert the
> result. For compatibility with other implementations you might want to
> try one of the following alternatives:

```bigloo
(define (alt1 name obj) (crc name obj :init -1))
(define (alt2 name obj) (crc name obj :final-xor -1))
(define (alt3 name obj) (crc name obj :init -1 :final-xor -1))
```

<span></span>

### crc-string ###
Computes the CRC of a string.

### crc-port ###
Computes the CRC of an input port.

### crc-mmap ###
Computes the CRC of an mmap.

### crc-file ###
Computes the CRC of a file.

### register-crc! ###
Bigloo provides means to create additional CRCs: one can either simply provide
a new polynomial or use Bigloo's low level functions.

Adds the given CRC to Bigloo's list. Name can be of any type (`crc`
will use `assoc` to find it in its list). The polynomial can be either
a fixnum, an elong or an llong. The argument `len` should give the
CRCs size. The type of the polynomial and the given `len` must be
consistent. On a 32 bit machine the following CRC registration would
be invalid and yield undefined results:

```bigloo
(register-crc! 'invalid 1337 55)
```
As 55 is bigger than the fixnum's bit-size calling `crc` with
this CRC will yield undefinde results.

### crc-long ###
Performs a CRC operation on one byte. The previously described
functions are based on these low level functions. The result of all
the low level functions will return values that are not cut to the
correct length. Usually a crc is done in a loop, and one needs to
`bit-and` only when returning the result.  Polynomials can be
given with or without the high-order bit.

For instance we could implement @code{openpgp-crc24} as follows:
```
(define *openpgp-init* #xB704CE)
(define *radix-64-24-poly* #x864CFB)
(define (openpgp-crc-24 str)
  (let loop ((i 0)
             (crc *openpgp-init*))
    (if (=fx i (string-length str))
        (bit-and crc #xFFFFFF) ;; cut to correct length (24 bits)
        (loop (+fx i 1)
              (crc-long (string-ref str i) crc *radix-64-24-poly* 24)))))
```

<span></span>

### crc-long-le ###
Little endian variant of `crc-long`.

### crc-elong ###
As `crc-long` but for `elong`.

### crc-elong-le ###
As `crc-long-le` but for `elong`.

### crc-llong ###
As `crc-long` but for `llong`.

### crc-llong-le ###
As `crc-long-le` but for `llong`.


