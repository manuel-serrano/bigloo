(module __openpgp-enums
   (export (content-tag->byte::byte content-tag::symbol)
	   (byte->content-tag::symbol i::byte)
	   (literal-format->byte::byte literal-format::symbol)
	   (byte->literal-format::symbol i::byte)
	   (compression-algo->byte::byte algo::symbol)
	   (byte->compression-algo::symbol i::byte)
	   (signature-type->byte::byte type::symbol)
	   (byte->signature-type::symbol i::byte)
	   (public-key-algo->byte::byte algo::symbol)
	   (byte->public-key-algo::symbol i::byte)
	   (symmetric-key-algo->byte::byte algo::symbol)
	   (byte->symmetric-key-algo::symbol i::byte)
	   (subpacket-type->byte::byte type::symbol)
	   (byte->subpacket-type::symbol i::byte)
	   (revocation-code->byte::byte code::symbol)
	   (byte->revocation-code::symbol i::byte)
	   (hash-algo->byte::byte algo::symbol)
	   (byte->hash-algo::symbol i::byte)
	   (s2k-algo->byte::byte algo::symbol)
	   (byte->s2k-algo::symbol i::byte)))

(define-macro (declare-enum name entries)
   (let ((input (gensym 'input)))
   `(begin
       (define (,(symbol-append name '->byte) ,input)
	  (case ,input
	     ,@(map (lambda (entry)
		       `((,(car entry)) ,(cadr entry)))
		    entries)
	     (else
	      (error ',name
		     "Invalid entry"
		     ,input))))
       (define (,(symbol-append 'byte-> name) ,input)
	  (case ,input
	     ,@(map (lambda (entry)
		       `((,(cadr entry)) ',(car entry)))
		    entries)
	     (else
	      (error ',name
		     "Invalid entry"
		     ,input)))))))
	     
(declare-enum content-tag
	      ((reserved 0)
	       (public-key-encrypted-session-key 1)
	       (signature 2)
	       (symmetric-key-encrypted-session-key 3)
	       (one-pass-signature 4)
	       (secret-key 5)
	       (public-key 6)
	       (secret-subkey 7)
	       (compressed 8)
	       (symmetrically-encrypted 9)
	       (marker 10)
	       (literal 11)
	       (trust 12)
	       (ID 13)
	       (public-subkey 14)
	       (user-attribute 17) ;; rfc4880
	       (mdc-symmetrically-encrypted 18) ;; rfc4880
	       (mdc 19)
	       ;; Private or Experimental Values
	       (private-0 60)
	       (private-1 61)
	       (private-2 62)
	       (private-3 63)
	       ))

(declare-enum literal-format
	      ((binary 98)     ;; #\b
	       (text   116)))  ;; #\t

(declare-enum compression-algo
	      ((uncompressed 0)
	       (ZIP 1)
	       (ZLIB 2)
	       (BZip2 3) ;; rfc4880
	       (private-0 100)
	       (private-1 101)
	       (private-2 102)
	       (private-3 103)
	       (private-4 104)
	       (private-5 105)
	       (private-6 106)
	       (private-7 107)
	       (private-8 108)
	       (private-9 109)
	       (private-10 110)
	       ))

(declare-enum signature-type
	      ((binary #x00)
	       (canonical #x01)
	       (standalone #x02)
	       (generic-certif #x10)
	       (persona-certif #x11)
	       (casual-certif #x12)
	       (positive-certif #x13)
	       (subkey-binding #x18)
	       (primary-key-binding #x19) ;; rfc4880
	       (key #x1F)
	       (key-revocation #x20)
	       (subkey-revocation #x28)
	       (certif-revocation #x30)
	       (timestamp #x40)
	       (third-party-confirmation #x50) ;; rfc4880
	       ))

(declare-enum public-key-algo
	      ((rsa-encrypt/sign 1)
	       (rsa-encrypt 2)
	       (rsa-sign 3)
	       (elgamal-encrypt 16)
	       (dsa 17)
	       (elliptic-curve 18)
	       (ecdsa 19)
	       (elgamal-encrypt/sign 20)
	       (diffie-hellman 21)
	       (private-0 100)
	       (private-1 101)
	       (private-2 102)
	       (private-3 103)
	       (private-4 104)
	       (private-5 105)
	       (private-6 106)
	       (private-7 107)
	       (private-8 108)
	       (private-9 109)
	       (private-10 110)
	       ))

(declare-enum symmetric-key-algo
	      ((unencrypted 0)
	       (idea 1)
	       (des3 2)
	       (cast5 3)
	       (blowfish 4)
	       (safer-sk128 5)
	       (des/sk 6)
	       (aes-128 7)
	       (aes-192 8)
	       (aes-256 9)
	       (twofish 10) ;; rfc4880
	       (private-0 100)
	       (private-1 101)
	       (private-2 102)
	       (private-3 103)
	       (private-4 104)
	       (private-5 105)
	       (private-6 106)
	       (private-7 107)
	       (private-8 108)
	       (private-9 109)
	       (private-10 110)
	       ))

(declare-enum subpacket-type
	      ((creation-time 2)
	       (expiration-time 3)
	       (exportable? 4)
	       (trust 5)
	       (regular-expression 6)
	       (revocable? 7)
	       ;; 8 reserved
	       (key-expiration-time 9)
	       (placeholder 10)
	       (preferred-symmetric 11)
	       (revocation-key 12)
	       ;; 13, 14, 15 reserved
	       (issuer-ID 16)
	       ;; 17, 18, 19 reserved
	       (notation 20)
	       (preferred-hash 21)
	       (preferred-compression 22)
	       (key-server-prefs 23)
	       (preferred-key-server 24)
	       (primary-id? 25)
	       (policy 26)
	       (key-flags 27)
	       (signer-ID 28)
	       (revocation-reason 29)
	       (features 30) ;; rfc4880
	       (signature-target 31) ;; rfc4880
	       (embedded-signature 32) ;; rfc4880
	       (private-0 100)
	       (private-1 101)
	       (private-2 102)
	       (private-3 103)
	       (private-4 104)
	       (private-5 105)
	       (private-6 106)
	       (private-7 107)
	       (private-8 108)
	       (private-9 109)
	       (private-10 110)
	       ))

(declare-enum revocation-code
	      ((no-reason #x00)
	       (superseded-key #x01)
	       (compromised-key #x02)
	       (no-longer-used #x03)
	       (invalid-user-ID #x20)
	       (private-0 100)
	       (private-1 101)
	       (private-2 102)
	       (private-3 103)
	       (private-4 104)
	       (private-5 105)
	       (private-6 106)
	       (private-7 107)
	       (private-8 108)
	       (private-9 109)
	       (private-10 110)
	       ))
	       

(declare-enum hash-algo
	      ((md5 1)
	       (sha-1 2)
	       (ripe-md/160 3)
	       ;; 4, 5, 6, 7 are reserved in rfc4880
	       ;(double-width-sha 4)
	       ;(md2 5)
	       ;(tiger/192 6)
	       ;(HAVAL 7)
	       (sha256 8)  ;; rfc4880
	       (sha384 9)  ;; rfc4880
	       (sha512 10) ;; rfc4880
	       (sha224 11) ;; rfc4880
	       (private-0 100)
	       (private-1 101)
	       (private-2 102)
	       (private-3 103)
	       (private-4 104)
	       (private-5 105)
	       (private-6 106)
	       (private-7 107)
	       (private-8 108)
	       (private-9 109)
	       (private-10 110)
	       ))

(declare-enum s2k-algo
	      ((simple 0)
	       (salted 1)
	       (iterated 3)))
