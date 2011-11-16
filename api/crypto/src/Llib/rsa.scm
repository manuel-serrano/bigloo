;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2009-11 Florian Loitsch                           */
;*    -------------------------------------------------------------    */
;*    Message encryption and decryption based on the RSA asymmetric    */
;*    cipher.                                                          */
;*=====================================================================*/

;; ============================================================================
;; This implementation is based on RFC 3447, PKCS #1 v2.1
;; -----------------
;; We have not implemented multi-prime RSA.
;; The code is _not_ secure against timing attacks.
;; Some error codes might give helpful insights to attackers.
;; ============================================================================


;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __crypto-rsa

   (import __crypto-util)

   (export (class Rsa-Key modulus::bignum exponent::bignum)
	   (final-class Complete-Rsa-Key::Rsa-Key
	      ;; for the complete-rsa-key "exponent" takes the role of 'd'
	      e::bignum p::bignum q::bignum
	      exp1::bignum   ;; d mod (p-1)
	      exp2::bignum   ;; d mod (q-1)
	      coeff::bignum) ;; (inverse of q) mod p
	   )
   (export
    ;; key-creation and management
    (generate-rsa-key::Complete-Rsa-Key #!key (size 1024) show-trace)
    (extract-public-rsa-key::Rsa-Key key::Complete-Rsa-Key)
    (extract-private-rsa-key::Rsa-Key key::Complete-Rsa-Key)
    (rsa-key=? key1::Rsa-Key key2::Rsa-Key)

    (rsa-key-length::long key::Rsa-Key)
    
    ;; the basic operations.
    (rsa-encrypt::bignum k::Rsa-Key m::bignum)
    (rsa-decrypt::bignum k::Rsa-Key c::bignum)
    (rsa-sign::bignum    k::Rsa-Key m::bignum)
    (rsa-verify::bool    k::Rsa-Key m::bignum s::bignum)

    ;; nearly the same as above but with "official" names of RFC 3447
    (OS2IP::bignum str::bstring)
    (I2OSP::bstring n::bignum len::long)
    (RSAEP::bignum k::Rsa-Key m::bignum)  ;; encryption
    (RSADP::bignum k::Rsa-Key c::bignum)  ;; decryption
    (RSASP1::bignum k::Rsa-Key m::bignum) ;; sign
    (RSAVP1::bignum k::Rsa-Key s::bignum) ;; verify (does not take m as param)

    ;; more or less deprecated functions
    (PKCS1-v1.5-pad::bstring str::bstring key-len::long mode::long)
    (PKCS1-v1.5-unpad::bstring str::bstring mode::long)

    (RSAES-PKCS1-v1.5-encrypt::bstring key::Rsa-Key m::bstring)
    (RSAES-PKCS1-v1.5-decrypt::bstring key::Rsa-Key c::bstring)
    (RSASSA-PKCS1-v1.5-sign::bstring key::Rsa-Key msg::bstring #!key (hash-algo::symbol 'sha-1))
    (RSASSA-PKCS1-v1.5-sign-bignum::bignum key::Rsa-Key msg::bstring #!key (hash-algo::symbol 'sha-1))
    (RSASSA-PKCS1-v1.5-verify::bool key::Rsa-Key msg::bstring S::bstring)
    (RSASSA-PKCS1-v1.5-verify-bignum::bool key::Rsa-Key msg::bstring S::bignum)

    ;; additional functions that pad, mask, etc. the input-string
    (RSAES-OAEP-encrypt::bstring key::Rsa-Key m::bstring
				 #!key (label::bstring ""))
    (RSAES-OAEP-decrypt::bstring key::Rsa-Key cypher::bstring
				 #!key (label::bstring ""))
    (RSASSA-PSS-sign::bstring key::Rsa-Key msg::bstring)
    (RSASSA-PSS-verify::bool key::Rsa-Key msg::bstring sig::bstring)))

;; key-length in octets
(define (rsa-key-length key::Rsa-Key)
   (/ceilingfx (rsa-key-bit-length key) 8))

;; key-length in bits
(define (rsa-key-bit-length key::Rsa-Key)
   (with-access::Rsa-Key key (modulus)
      (bignum-bit-length modulus)))

;*---------------------------------------------------------------------*/
;*    generate-rsa-key ...                                             */
;*---------------------------------------------------------------------*/
(define (generate-rsa-key #!key (size 1024) show-trace)
   (let* ((size-p (quotientfx size 2))
	  (start-p (exptbx #z2 (fixnum->bignum size-p)))
	  (end-p (*bx start-p #z2))
	  (p (make-random-prime start-p end-p :show-trace show-trace))
	  (start-n (exptbx #z2 (fixnum->bignum size)))
	  (end-n (*bx start-n #z2))
	  (start-q (+bx (quotientbx (-bx start-n #z1) p) #z1))
	  (end-q (quotientbx end-n p)))
      (let loop ()
	 (let ((q (make-random-prime start-q end-q :show-trace show-trace)))
	    (if (not (=bx (gcdbx p q) #z1))
		(loop)
		(let* ((n (*bx p q))
		       (p-1 (-bx p #z1))
		       (q-1 (-bx q #z1))
		       (phi (quotientbx (*bx p-1 q-1) (gcdbx p-1 q-1)))
		       (e (let loop ((e #z65537))
			     (if (=bx #z1 (gcdbx e phi))
				 e
				 (loop (+bx e #z2)))))
		       (d (mod-inverse e phi))
		       (d-mod-p-1 (modulobx d (-bx p #z1)))
		       (d-mod-q-1 (modulobx d (-bx q #z1)))
		       (qInv (mod-inverse q p)))
		   (when show-trace (newline))
		   (instantiate::Complete-Rsa-Key
		      (modulus n)
		      (exponent d)
		      ;; private key information		      
		      (e e)
		      (p p)
		      (q q)
		      (exp1 d-mod-p-1)
		      (exp2 d-mod-q-1)
		      (coeff qInv))))))))

(define (extract-public-rsa-key::Rsa-Key key::Complete-Rsa-Key)
   (with-access::Complete-Rsa-Key key (modulus e)
      (instantiate::Rsa-Key
	 (modulus modulus)
	 (exponent e))))
(define (extract-private-rsa-key::Rsa-Key key::Complete-Rsa-Key)
   (with-access::Complete-Rsa-Key key (modulus exponent)
      (instantiate::Rsa-Key
	 (modulus modulus)
	 (exponent exponent))))
(define (make-rsa-key::Rsa-Key modulus::bignum exponent::bignum)
   (instantiate::Rsa-Key
      (modulus modulus)
      (exponent exponent)))
(define (rsa-key-modulus::bignum key::Rsa-Key)
   (with-access::Rsa-Key key (modulus)
      modulus))
(define (rsa-key-exponent::bignum key::Rsa-Key)
   (with-access::Rsa-Key key (exponent)
      exponent))

(define (rsa-key=? rsa-key1 rsa-key2)
   (define (public-exp k)
      (if (isa? k Complete-Rsa-Key)
	  (with-access::Complete-Rsa-Key k (e) e)
	  (with-access::Rsa-Key k (exponent) exponent)))

   (with-access::Rsa-Key rsa-key1 ((modulus1 modulus))
      (with-access::Rsa-Key rsa-key2 ((modulus2 modulus))
	 (and (=bx modulus1 modulus2)
	      (=bx (public-exp rsa-key1) (public-exp rsa-key2))))))


(define (I2OSP x::bignum x-len::long)
   (bignum->bin-str x x-len))

(define (I2OSP! buffer::bstring from::long x::bignum x-len::long)
   (bignum->bin-str! buffer from x x-len))

(define (OS2IP::bignum str::bstring)
   (bin-str->bignum str))

;; Mask Generating Function 1
(define (MGF1 seed::bstring len::long
	      #!key (hash-fun::procedure sha1sum-bin))
   ;; TODO: missing "mask too long" test.
   (let ((res (make-string len))
	 (buf (string-append seed "1234"))
	 (seed-len (string-length seed)))
      (let loop ((i 0)
		 (c #z0))
	 (cond
	    ((>=fx i len) res)
	    (else
	     (I2OSP! buf seed-len c 4) ;; replace counter in buf
	     (let* ((hash-str (hash-fun buf))
		    (hash-len (string-length hash-str))
		    (blit-len (minfx hash-len (-fx len i))))
		(blit-string! hash-str 0 res i blit-len)
		(loop (+fx i blit-len) (+bx c #z1))))))))

;; padding function
(define (EME-OAEP-encode k::long m::bstring
			 #!key (hash-fun::procedure sha1sum-bin)
			 (mask-fun::procedure MGF1) (label::bstring ""))
   ;                              +----------+---------+-------+
   ;                         DB = |  lHash   |    PS   |   M   |
   ;                              +----------+---------+-------+
   ;                                             |
   ;                   +----------+              V
   ;                   |   seed   |--> MGF ---> xor
   ;                   +----------+              |
   ;                         |                   |
   ;                +--+     V                   |
   ;                |00|    xor <----- MGF <-----|
   ;                +--+     |                   |
   ;                  |      |                   |
   ;                  V      V                   V
   ;                +--+----------+----------------------------+
   ;          EM =  |00|maskedSeed|          maskedDB          |
   ;                +--+----------+----------------------------+

   (let* ((l-hash (hash-fun label))
	  (h-len (string-length l-hash))
	  (m-len (string-length m))
	  (PS-len (-fx- k m-len (*fx 2 h-len) 2))
	  (DB-len (+fx+ h-len PS-len 1 m-len))
	  (DB (make-string DB-len))
	  (seed (make-random-string h-len)))

      ;; DB = lHash || PS || 0x01 || M
      (blit-string! l-hash 0 DB 0 h-len)
      ;; PS = string of zero octets
      (let loop ((i 0))
	 (unless (>=fx i PS-len)
	    (string-set! DB (+fx h-len i) #a000)
	    (loop (+fx i 1))))
      ;; 0x01
      (string-set! DB (+fx+ h-len PS-len) #a001)
      ;; and the final M
      (blit-string! m 0 DB (+fx+ h-len PS-len 1) m-len)

      (let* ((db-mask (mask-fun seed (-fx- k h-len 1)))
	     (masked-db (string-xor DB db-mask))
	     (seed-mask (mask-fun masked-db h-len))
	     (masked-seed (string-xor seed seed-mask))
	     (res-str (make-string (+fx+ 1 h-len DB-len))))
	 ;; res-str = EM = 0x00 || maskedSeed || maskedDB
	 (string-set! res-str 0 #\0)
	 (blit-string! masked-seed 0 res-str 1 h-len)
	 (blit-string! masked-db 0 res-str (+fx 1 h-len) DB-len)
	 res-str)))

(define (EME-OAEP-decode k::long EM::bstring
			 #!key (hash-fun::procedure sha1sum-bin)
			 (mask-fun::procedure MGF1) (label::bstring ""))
   (let* ((l-hash (hash-fun label))
	  (h-len (string-length l-hash))
	  (em-len (string-length EM))
	  (DB-len (-fx- k h-len 1))
	  (dummy-assert (when (not (=fx em-len (+fx+ 1 h-len DB-len)))
			   (error "decryption-error" "bad string" EM)))
	  ;; EM = Y || maskedSeed || maskedDB
	  (Y (string-ref EM 0))
	  (masked-seed (substring EM 1 (+fx 1 h-len)))
	  (masked-DB (substring EM (+fx 1 h-len) em-len))
	  (seed-mask (mask-fun masked-DB h-len))
	  (seed (string-xor masked-seed seed-mask))
	  (db-mask (mask-fun seed DB-len))
	  (DB (string-xor masked-DB db-mask)))
      ;; we should have DB = lhash || PS || 0x01 || M
      ;; PS a string of 0x00
      (when (not (string-prefix? l-hash DB))
	 (error "decryption-error" "hash not correctly rectrieved" DB))
      (let loop ((i h-len))
	 (cond
	    ((>=fx i DB-len)
	     (error "decryption-error" "could not find M" DB))
	    ((char=? #\null (string-ref DB i))
	     ;; still inside PS-part
	     (loop (+fx i 1)))
	    ((char=? #a001 (string-ref DB i))
	     ;; remaining octets are message M
	     (substring DB (+fx i 1) DB-len))
	    (else
	     (error "decryption-error" "expected 0x00 or 0x01" DB))))))
   
;; Encryption.
;; k: rsa public key. we only need the modulus and the exponent.
;;    if a complete key is given we will use the public exponent.
;; m: a message representative.
(define rsa-encrypt RSAEP)
(define (RSAEP::bignum k::Rsa-Key m::bignum)
   (receive (modulus exponent)
      (if (isa? k Complete-Rsa-Key)
	  (with-access::Complete-Rsa-Key k (modulus e) (values modulus e))
	  (with-access::Rsa-Key k (modulus exponent) (values modulus exponent)))
      (when (>=bx m modulus)
	 (error "RSASP1" "message representative out of range"
		(cons m modulus)))
      (expt-modbx m exponent modulus)))

;; Decryption.
;; k: rsa private key. we only need the modulus and the exponent.
;; c: a cyphertext representative.
(define rsa-decrypt RSADP)
(define (RSADP::bignum k::Rsa-Key c::bignum)
   (with-access::Rsa-Key k (modulus exponent)
      (when (>=bx c modulus)
	 (error "RSASP1" "cyphertext representative out of range"
		(cons c modulus)))
      (expt-modbx c exponent modulus)))

;; Signing.
;; k: rsa private key. we only need the modulus and the exponent.
;; m: a message representative.
(define rsa-sign RSASP1)
(define (RSASP1::bignum k::Rsa-Key m::bignum)
   (with-access::Rsa-Key k (modulus exponent)
      (when (>=bx m modulus)
	 (error "RSASP1" "message representative out of range"
		(cons m modulus)))
      (expt-modbx m exponent modulus)))

;; Verification
;; k: rsa public key. we only need the modulus and the exponent.
;;    if a complete key is given we will use the public exponent.
;; s: a signature representative.
(define (RSAVP1::bignum k::Rsa-Key s::bignum)
   (receive (modulus exponent)
      (if (isa? k Complete-Rsa-Key)
	  (with-access::Complete-Rsa-Key k (modulus e) (values modulus e))
	  (with-access::Rsa-Key k (modulus exponent) (values modulus exponent)))
      (when (>=bx s modulus)
	 (error "RSASP1" "signature representative out of range"
		(cons s modulus)))
      (expt-modbx s exponent modulus)))
(define (rsa-verify::bool k::Rsa-Key m::bignum s::bignum)
   (=bx (RSAVP1 k s) m))

(define (PKCS1-v1.5-pad m k mode)
   ;; PS has only size (-fx- k m-len 3)
   ;; but this way we avoid allocating even more.
   (let* ((m-len (string-length m))
	  (PS+pad-len (-fx k m-len))
	  (PS+pad (case mode
		     ((0) (make-string PS+pad-len #a000))
		     ((1) (make-string PS+pad-len #a255))
		     ((2) (make-random-string PS+pad-len))
		     (else (error 'PKCS1-v1.5-pad
				  "unknown padding mode"
				  mode))))
	  (em (string-append PS+pad m)))
	 ;; PS must have non-zero octets in mode 2.
      (when (=fx mode 2)
	 (let loop ((i 2)) ;; skip the first two octets.
	    (when (<fx i (-fx PS+pad-len 1))
	       (when (zerofx? (char->integer (string-ref em i)))
		  (string-set! em i (integer->char-ur (random 256))))
	       (loop (+fx i 1)))))
      (string-set! em 0 #a000)
      (string-set! em 1 (integer->char mode))
      (string-set! em (-fx PS+pad-len 1) #a000)
      em))

(define (PKCS1-v1.5-unpad em mode)
   (define (pkcs1-error)
      (error "PKCS1-v1.5-unpad" "decryption error" #f))

   (let ((k (string-length em)))
      (when (or (<fx mode 0) (>fx mode 2)
		(not (char=? #a000 (string-ref em 0)))
		(not (char=? (integer->char mode) (string-ref em 1))))
	 (pkcs1-error))

      (case mode
	 ((0) ;; simply find first non-zero char.
	  (let loop ((i 2))
	     (cond
		((>=fx i k) (pkcs1-error))
		((char=? #a000 (string-ref em i)) (loop (+fx i 1)))
		(else (substring em i k)))))
	 ((1) ;; simply find first non-#xFF, which must be #x00 char.
	  ;; return rest.
	  (let loop ((i 2))
	     (cond
		((>=fx i k) (pkcs1-error))
		((char=? #a255 (string-ref em i)) (loop (+fx i 1)))
		((char=? #a000 (string-ref em i)) (substring em (+fx i 1) k))
		(else (pkcs1-error)))))
	 ((2) ;; find first #x00 char. return rest.
	  (let loop ((i 2))
	     (cond
		((>=fx i k) (pkcs1-error))
		((and (char=? #a000 (string-ref em i))
		      (<fx i 10))
		 (pkcs1-error))
		((char=? #a000 (string-ref em i))
		 (substring em (+fx i 1) k))
		(else (loop (+fx i 1))))))
	 (else (pkcs1-error))))) ;; to make Bigloo happy.

(define (RSAES-PKCS1-v1.5-encrypt::bstring key::Rsa-Key m::bstring)
   (let ((k (rsa-key-length key))
	 (m-len (string-length m)))
      (when (>fx m-len (-fx k 11))
	 (error "rsa encrypt" "message too long" m))
      (I2OSP (RSAEP key (OS2IP (PKCS1-v1.5-pad m k 2))) k)))

(define (RSAES-PKCS1-v1.5-decrypt::bstring key::Rsa-Key c::bstring)
   (with-handler
      (lambda (e)
	 (error 'rsa-decrypt
		"Decryption failed"
		#f))
      (let ((k (rsa-key-length key))
	    (c-len (string-length c)))
	 (when (not (=fx k c-len))
	    (error "rsa decrypt" "decryption error" #f))
	 (let* ((m (RSADP key (OS2IP c)))
		(em (I2OSP m k)))
	    (PKCS1-v1.5-unpad em 2)))))

(define (RSAES-OAEP-encrypt::bstring key::Rsa-Key m::bstring
				     #!key (label::bstring ""))
   ;; TODO: currently we use sha1 unconditionally. might be interesting to
   ;; allow other hash-functions.
   (define hash-fun sha1sum-bin)
   (define h-len 20) ;; CARE: experimental number.
   
   (let ((k (rsa-key-length key))
	 (m-len (string-length m)))
      ;; TODO: missing length check for label. should not matter, as SHA-1 is
      ;; limited by 2^61-1 octets asd bstring probably can't be that long anyways.
      (when (>fx m-len (-fx- k (*fx 2 h-len) 2))
	 (error "rsa encrypt" "message too long" m))

      (let* ((em (EME-OAEP-encode k m :hash-fun hash-fun :label label)) ;; encoded
	     (m-n (OS2IP em))
	     (c (RSAEP key m-n)))
	 (I2OSP c k))))

(define (RSAES-OAEP-decrypt::bstring key::Rsa-Key cypher::bstring
				     #!key (label::bstring ""))
   ;; TODO: currently we use sha1 unconditionally. might be interesting to
   ;; allow other hash-functions.
   (define hash-fun sha1sum-bin)
   (define h-len 20) ;; CARE: experimental number.

   (with-handler
      (lambda (e)
	 (error 'rsaes-oaep-decrypt
		"Decryption failed"
		#f))
      (let ((k (rsa-key-length key)))
	 ;; TODO: no check against length of hash-fun.
	 (when (not (=fx k (string-length cypher)))
	    (error "rsa decrypt" "decryption error" cypher))
	 (let* ((c (OS2IP cypher))
		(m (RSADP key c))
		(EM (I2OSP m k)))
	    (EME-OAEP-decode k EM)))))

(define (RSASSA-PSS-sign::bstring key::Rsa-Key msg::bstring)
   (let* ((mod-bits (rsa-key-bit-length key))
	  (k (/ceilingfx mod-bits 8))
	  (em (EMSA-PSS-encode msg (-fx mod-bits 1)))
	  (m (OS2IP em))
	  (s (RSASP1 key m)))
      (I2OSP s k)))

(define (RSASSA-PSS-verify::bool key::Rsa-Key msg::bstring sig::bstring)
   (with-handler
      (lambda (e)
	 #f)
      (let* ((mod-bits (rsa-key-bit-length key))
	     (k (/ceilingfx mod-bits 8)))
	 (and (=fx k (string-length sig))
	      (let* ((s (OS2IP sig))
		     (m (RSAVP1 key s))
		     (em-len (/ceilingfx (-fx mod-bits 1) 8))
		     (em (I2OSP m em-len)))
		 (EMSA-PSS-verify msg em (-fx mod-bits 1)))))))

;; the following DER-strings can actually be computed using our DER-module:
;; md2: (encode-DER-str '((oid:1.2.840.113549.2.2 null) "1234567890123456"))
;; md5: (encode-DER-str '((oid:1.2.840.113549.2.5 null) "1234567890123456"))
;; As DER is prefixed we then have to cut off the "1234..." at the end and have
;; our prefixes.
;; Note that the final 16chars are necessary so that the header is
;; correct. Indeed, the precomputed prefixes already contain information about
;; the length of the complete DER encoded string.
;; the oids are given in rfc3447 (and other specs).
(define *DER-md2*
   (hex-str->string "3020300c06082a864886f70d020205000410"))
(define *DER-md5*
   (hex-str->string "3020300c06082a864886f70d020505000410"))
(define *DER-SHA1*
   (hex-str->string "3021300906052b0e03021a05000414"))
(define *DER-SHA256*
   (hex-str->string "3031300d060960864801650304020105000420"))
(define *DER-SHA384*
   (hex-str->string "3041300d060960864801650304020205000430"))
(define *DER-SHA512*
   (hex-str->string "3051300d060960864801650304020305000440"))

(define (hash-algo->procedure::procedure algo::symbol)
   (case algo
      ((md5 MD5 md5sum) md5sum-bin)
      ((sha1 sha-1 SHA-1 sha1sum) sha1sum-bin)
      (else (error "RSA hash-algo->DER-prefix"
		   "unknown hash algorithm (or not implemented)"
		   algo))))

(define (hash-algo->DER-prefix::bstring algo::symbol)
   (case algo
      ((md2 MD2 md2sum) *DER-md2*)
      ((md5 MD5 md5sum) *DER-md5*)
      ((sha1 sha-1 SHA-1 sha1sum) *DER-SHA1*)
      ((sha256 sha-256 SHA-256 sha256sum) *DER-SHA256*)
      ((sha384 sha-384 SHA-384 sha384sum) *DER-SHA384*)
      ((sha512 sha-512 SHA-512 sha512sum) *DER-SHA512*)
      (else (error "RSA hash-algo->DER-prefix"
		   "unknown hash algorithm"
		   algo))))

;; prefix might be b
;; we could again use our DER-implementation and actually decode the
;; DER-string.
(define (DER-prefix->hash-algo::symbol prefix::bstring)
   (define a-list
      `((,*DER-SHA1*   sha-1) ;; most common one in the beginning
	(,*DER-md5*    md5)
	(,*DER-md2*    md2)
	(,*DER-SHA256* sha-256)
	(,*DER-SHA384* sha-384)
	(,*DER-SHA512* sha-512)))

   (let ((proc-p (any (lambda (p)
			 (and (string-prefix? (car p) prefix)
			      (cadr p)))
		      a-list)))
      (when (not proc-p)
	 (error "RSA hash-algo"
		"hash-algorithm not found or not yet implemented"
		#f))
      proc-p))

;; raises an error if the given string is not a valid PKCS1-encoded string.
;; the test is not complete.
;; returns the used hash-algorithm.
(define (EMSA-PKCS1-v1.5-extract-hash-algo em::bstring)
   (DER-prefix->hash-algo (PKCS1-v1.5-unpad em 1)))

(define (EMSA-PKCS1-v1.5-encode::bstring m::bstring em-len::long
					 hash-algo::symbol)
   (let* ((hash-fun (hash-algo->procedure hash-algo))
	  (H (hash-fun m))
	  (digest-info-DER (hash-algo->DER-prefix hash-algo))
	  (str (string-append digest-info-DER H))
	  (str-len (string-length str)))
      (when (<fx em-len (+fx str-len 11))
	 (error "RSA encode" "intended encoded message length too short"
		em-len))
      (PKCS1-v1.5-pad str em-len 1)))

(define (RSASSA-PKCS1-v1.5-sign key::Rsa-Key msg::bstring
				#!key (hash-algo::symbol 'sha-1))
   (let ((k (rsa-key-length key)))
      (I2OSP (RSASSA-PKCS1-v1.5-sign-bignum key msg :hash-algo hash-algo) k)))
(define (RSASSA-PKCS1-v1.5-sign-bignum key::Rsa-Key msg::bstring
				       #!key (hash-algo::symbol 'sha-1))
   (let* ((k (rsa-key-length key))
	  ;; TODO: if the encoding returns with "indented encoded message
	  ;; length too short" then we should convert that into "RSA modulus
	  ;; too short".
	  (em (EMSA-PKCS1-v1.5-encode msg k hash-algo))
	  (m (OS2IP em))
	  (s (RSASP1 key m)))
      s))

(define (RSASSA-PKCS1-v1.5-verify::bool key::Rsa-Key msg::bstring S::bstring)
   (let ((sig-len (string-length S))
	 (k (rsa-key-length key)))
      (and (=fx sig-len k)
	   (let ((s (OS2IP S)))
	      (RSASSA-PKCS1-v1.5-verify-bignum key msg s)))))

;; will return #f when the implementation is incomplete (ie when an
;; non-implemented hash-algorithm is used).
(define (RSASSA-PKCS1-v1.5-verify-bignum::bool key::Rsa-Key msg::bstring
					       s::bignum)
   (let ((intercept-error? #f))
      (with-handler
	 (lambda (e)
	    (if intercept-error?
		#f
		(raise e)))
	 (let ((k (rsa-key-length key)))
	    (let* ((m (RSAVP1 key s))
		   (em (I2OSP m k))
		   (hash-algo (EMSA-PKCS1-v1.5-extract-hash-algo em))
		   (dummy (set! intercept-error? #f))
		   ;; TODO: in theory we should change the error-message.
		   (em_prim (EMSA-PKCS1-v1.5-encode msg k hash-algo)))
	       (string=? em em_prim))))))

(define (left-most-bits-char-mask::char n)
   (case n
      ((1) #a128) ; 0x80
      ((2) #a192) ; 0xC0
      ((3) #a224) ; 0xE0
      ((4) #a240) ; 0xF0
      ((5) #a248) ; 0xF8
      ((6) #a252) ; 0xFC
      ((7) #a254) ; 0xFE
      ((8) #a255) ; 0xFF
      (else (error "left-most-bits-char-mask" "n must be <= 8" n))))

(define (EMSA-PSS-encode::bstring m::bstring em-bits::long
				  #!key (salt-len::long 0)
				  (hash-fun::procedure sha1sum-bin)
				  (mask-fun::procedure MGF1))
   ;                                   +-----------+
   ;                                   |     M     |
   ;                                   +-----------+
   ;                                         |
   ;                                         V
   ;                                       Hash
   ;                                         |
   ;                                         V
   ;                           +--------+----------+----------+
   ;                      M' = |Padding1|  mHash   |   salt   |
   ;                           +--------+----------+----------+
   ;                                          |
   ;                +--------+----------+     V
   ;          DB =  |Padding2|maskedseed|   Hash
   ;                +--------+----------+     |
   ;                          |               |
   ;                          V               |    +--+
   ;                         xor <--- MGF <---|    |bc|
   ;                          |               |    +--+
   ;                          |               |      |
   ;                          V               V      V
   ;                +-------------------+----------+--+
   ;          EM =  |    maskedDB       |maskedseed|bc|
   ;                +-------------------+----------+--+

   ;; TODO: no check if message is too big for hash.
   (let* ((m-hash (hash-fun m))
	  (h-len (string-length m-hash))
	  (em-len (/ceilingfx em-bits 8)))
      (when (<fx em-len (+fx+ h-len salt-len 2))
	 (error "encoding error" "bad size" em-len))
      (let* ((salt (make-random-string salt-len))
	     (Mprim (string-append "\0\0\0\0\0\0\0\0"
				   ; 1 2 3 4 5 6 7 8
				   m-hash salt))
	     (H (hash-fun Mprim))
	     (PS-len (-fx- em-len salt-len h-len 2))
	     (DB-len (-fx- em-len h-len 1))
	     (DB (make-string DB-len #a000)))
	 ;; DB = PS || 0x01 || salt
	 ;; PS = string of 0x00
	 (string-set! DB PS-len #a001)
	 (blit-string! salt 0 DB (+fx PS-len 1) salt-len)
	 (let* ((db-mask (mask-fun H (-fx- em-len h-len 1)))
		(masked-db (string-xor DB db-mask))
		(nb-bits-to-clear (-fx (*fx 8 em-len) em-bits))
		(bit-mask (char-not (left-most-bits-char-mask
				     nb-bits-to-clear)))
		(left-most-octet (string-ref masked-db 0)))
	    ;; set the leftmost bits to 0.
	    (string-set! masked-db 0 (char-and bit-mask left-most-octet))
	    (let ((tmp (string-append masked-db H "\0")))
	       (string-set! tmp (-fx (string-length tmp) 1) #a188) ; 0xBC
	       tmp)))))

(define (EMSA-PSS-verify::bool msg::bstring em::bstring em-bits::long
			       #!key (salt-len::long 0)
			       (hash-fun::procedure sha1sum-bin)
			       (mask-fun::procedure MGF1))
   (bind-exit (inconsistent)
      ;; TODO: no check if message is too big for hash.
      (let* ((m-hash (hash-fun msg))
	     (h-len (string-length m-hash))
	     (em-len (string-length em)))
	 (when (<fx em-len (+fx+ h-len salt-len 2)) (inconsistent #f))
	 (when (not (char=? (string-ref em (-fx em-len 1)) #a188)) ; 0xBC
	    (inconsistent #f))
	 (let* ((DB-len (-fx- em-len h-len 1))
		(masked-db (substring em 0 DB-len))
		(H (substring em DB-len (+fx DB-len h-len)))
		(nb-cleared-bits (-fx (*fx 8 em-len) em-bits))
		(bit-mask (left-most-bits-char-mask nb-cleared-bits))
		(left-most-octet (string-ref masked-db 0)))
	    (when (not (char=? (char-and left-most-octet bit-mask) #a000))
	       (inconsistent #f))
	    (let* ((db-mask (mask-fun H DB-len))
		   (db (string-xor masked-db db-mask))
		   (db-left-most-octet (string-ref db 0))
		   (db-bit-mask (char-not bit-mask))
		   (PS-len (-fx- em-len h-len salt-len 2)))
	       (string-set! db 0 (char-and db-bit-mask db-left-most-octet))
	       (let loop ((i (-fx PS-len 1)))
		  (cond
		     ((<fx i 0) 'done) ;; all 0x00
		     ((char=? #a000 (string-ref db i)) (loop (-fx i 1)))
		     (else (inconsistent #f))))
	       (when (not (char=? #a001 (string-ref db PS-len)))
		  (inconsistent #f))
	       (let* ((salt (substring db (-fx DB-len salt-len) DB-len))
		      (Mprim (string-append "\0\0\0\0\0\0\0\0"
					    ; 1 2 3 4 5 6 7 8
					    m-hash salt))
		      (Hprim (hash-fun Mprim)))
		  (string=? Hprim H)))))))
