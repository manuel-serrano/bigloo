;*=====================================================================*/
;*    .../prgm/project/bigloo/api/openpgp/src/Llib/pgp_logic.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Fri Aug 13 08:28:04 2010                          */
;*    Last change :  Fri Apr 27 11:21:28 2012 (serrano)                */
;*    Copyright   :  2010-12 Florian Loitsch, Manuel Serrano           */
;*    -------------------------------------------------------------    */
;*    OpenPGP logic                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __openpgp-logic
   (library crypto)
   (import __openpgp-util
	   __openpgp-port-util
	   __openpgp-s2k
	   __openpgp-decode
	   __openpgp-encode
	   __openpgp-conversion
	   __openpgp-human
	   __openpgp-algo
	   __openpgp-packets
	   __openpgp-enums
	   __openpgp-composition)
   (export
    (key-id::bstring kp::PGP-Key-Packet)
    (generic fingerprint::bstring kp::PGP-Key-Packet)
    (decrypt-secret-key! kp::PGP-Secret-Key-Packet password::bstring)
    (verify-pgp-signature::pair-nil sig::PGP-Signature key-resolver::procedure
				    #!optional (msg #f))
    (verify-key key::PGP-Key key-resolver::procedure)
    (create-one-pass-signature::PGP-One-Pass-Signature
     msg::bstring key::PGP-Subkey #!key (password-provider #f) (hash-algo 'sha-1)
     (file-name #f) (creation-date #f))
    (create-pgp-signature::PGP-Signature
	 msg::bstring key::PGP-Subkey #!key (password-provider #f)
	 (detached-signature? #f)
	 (hash-algo 'sha-1)
	 (file-name #f) (creation-date #f))
    (key-packet->human-readable::bstring key::PGP-Key-Packet)
    (pgp-key->human-readable::bstring key::PGP-Key)
    (pgp-subkey->human-readable::bstring key::PGP-Subkey)
    (needs-password-for-decryption? kp::PGP-Secret-Key-Packet)
    (symmetric-encrypt::PGP-Symmetrically-Encrypted-Packet data::PGP-Packet
							   key-string::bstring algo::symbol
							   #!key (mdc #t))
    (symmetric-decrypt cipher::PGP-Symmetrically-Encrypted-Packet key-string::bstring algo::symbol)
    (decrypt-symmetric-key-session-key session-packet::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
				       passkey::bstring)
    (decrypt-public-key-session-key session-packet::PGP-Public-Key-Encrypted-Session-Key-Packet
				    subkey::PGP-Subkey
				    password-provider)
    (create-password-session-key-packet
     password::bstring
     session-key
     data-symmetric-algo::symbol ;; used to encrypt the payload.
     #!key
     (hash-algo 'sha-1)
     (symmetric-algo 'cast5)
     (s2k-algo 'iterated))
    (create-public-key-session-key-packet
     public-subkey::PGP-Subkey
     session-key::bstring
     data-symmetric-algo::symbol))) ;; used to encrypt the payload.

(define (PGP-main-key::PGP-Subkey key::PGP-Key)
   (with-access::PGP-Key key (subkeys)
      (car subkeys)))
(define (PGP-main-key-packet::PGP-Key-Packet key::PGP-Key)
   (with-access::PGP-Key key (subkeys)
      (with-access::PGP-Subkey (car subkeys) (key-packet)
	 key-packet)))

;; constructs the string that needs to be hashed to get the signature of a
;; string. Most of the time this will be the identity function.
(define (construct-data-signature-msg str::bstring sig-type::symbol)
   (case sig-type
    ((binary) str)
    ((canonical) (canonical-string str))
    ((standalone)
     (when (not (string-null? str))
	(error 'construct-data-signature-msg
	       "Standalone signatures must have empty strings as messages"
	       str)))
    (else
     (error 'construct-data-signature-msg
	    "Signature type not yet implemented (or not a string-signature)"
	    (cons sig-type (signature-type->human-readable sig-type))))))

;*---------------------------------------------------------------------*/
;*    canonical-string ...                                             */
;*    -------------------------------------------------------------    */
;*    canonicalize a string means replacing the newlines with CRLR     */
;*    (see http://tools.ietf.org/html/rfc4880#section-5.2.4).          */
;*---------------------------------------------------------------------*/
(define (canonical-string str)
   
   (define (count-newline str)
      (let loop ((i (string-index str #\Newline))
		 (c 0))
	 (if i
	     ;; skip crlf
	     (if (and (>fx i 0) (char=? (string-ref str (-fx i 1)) #\Return))
		 (loop (string-index str #\Newline (+fx i 1)) c)
		 (loop (string-index str #\Newline (+fx i 1)) (+fx c 1)))
	     c)))

   (let ((nstr (make-string (+fx (string-length str) (count-newline str)))))
      (let loop ((r 0)
		 (w 0))
	 (let ((i (string-index str #\Newline r)))
	    (cond
	       ((not i)
		;; done
		(blit-string! str r nstr w (-fx (string-length str) r))
		nstr)
	       ((and (>fx i 0) (char=? (string-ref str (-fx i 1)) #\Return))
		;; skip crlf
		(let ((l (+fx 1 (-fx i r))))
		   (blit-string! str r nstr w l)
		   (loop (+fx i 1) (+fx w l))))
	       (else
		;; pur newline
		(let ((l (-fx i r)))
		   (blit-string! str r nstr w l)
		   (string-set! nstr (+ w l) #\Return)
		   (string-set! nstr (+ w (+fx l 1)) #\Newline)
		   (loop (+fx i 1) (+fx w (+fx l 2))))))))))
   
;; constructs the string that needs to be hashed to get the signature of an ID
;; (bound to a key).
(define (construct-certification-signature-str id::bstring key::PGP-Key-Packet
					       sig-type::symbol version)
   (case sig-type
    ((generic-certif persona-certif casual-certif positive-certif)
     (let ((key-str (construct-key-signature-str key 'key)))
	(case version
	   ((3)
	    (string-append key-str id))
	   ((4)
	    (string-append key-str
			   (string (integer->char-ur #xb4)) ;; 5.2.4
			   (fixnum->scalar (string-length id) 4)
			   id))
	   (else (error 'construct-certification-signature-str
			"Unsupported version"
			version)))))
    (else
     (error 'construct-certification-signature-str
	    "Signature type not yet implemented (or not a certification-signature)"
	    (cons sig-type (signature-type->human-readable sig-type))))))

;; constructs the string that needs to be hashed to get the signature of the
;; key.
(define (construct-key-signature-str key::PGP-Key-Packet sig-type::symbol)
   (case sig-type
      ((key key-revocation subkey-revocation)
       (let* ((str-p (open-output-string))
	      (dummy (encode-public-key-content key str-p))
	      (content (close-output-port str-p)))
	  (string-append (string (integer->char-ur #x99))
			 (fixnum->scalar (string-length content) 2)
			 content)))
      (else
       (error 'construct-key-signature-msg
	      "Signature type not yet implemented (or not a key-signature)"
	      (cons sig-type (signature-type->human-readable sig-type))))))

(define (construct-subkey-signature-str subkey::PGP-Key-Packet
					main-key::PGP-Key-Packet
					sig-type::symbol)
   (case sig-type
      ((subkey-binding)
       (string-append (construct-key-signature-str main-key 'key)
		      (construct-key-signature-str subkey 'key)))
      (else
       (error 'construct-subkey-signature-str
	      "Signature type not yet implemented (or not a subkey-signature)"
	      (cons sig-type (signature-type->human-readable sig-type))))))

;; msg is already in correct form (with respect to the signature-type)
(define (verify-signature msg::bstring
			  sig::PGP-Signature-Packet
			  key-resolver::procedure)
   (define (try-keys valid-key? verify keys)
      (any (lambda (subkey)
	      (with-handler
		 (lambda (e)
		    (print e)
		    (warning "#f due to exception") #f)
		 (with-access::PGP-Subkey subkey (key-packet)
		    (with-access::PGP-Key-Packet key-packet (key)
		       (and key
			    (valid-key? key)
			    (verify key)
			    subkey)))))
	   keys))

   (define (verify-signature-for-keys msg::bstring sig::PGP-Signature-Packet
				      keys::pair-nil)
      (with-access::PGP-Signature-Packet sig
	    (signature-type issuer public-key-algo hash-algo signature
			    signed-packet-prefix hash-trailer left-hash
			    version)
	 (let* ((signed-m (string-append msg signed-packet-prefix
					 hash-trailer))
		(hash-fun (hash-algo->procedure hash-algo))
		(hash (hash-fun signed-m))
		(hash-prefix-ok? (string-prefix? left-hash hash)))
	    (trace-item "signature-version: " version)
	    (trace-item "hash-prefix: " (str->hex-string left-hash))
	    (trace-item "hash:        " (str->hex-string hash))
	    (if (not hash-prefix-ok?)
		(begin
		   (trace-item "left-hash failed.")
		   #f)
		(case public-key-algo
		   ((rsa-encrypt/sign rsa-sign)
		    (try-keys (lambda (x) (isa? x Rsa-Key)) ;; key-test
			      (lambda (key)
				 ;; TODO: pass hash-algo to rsa-fun.
				 (RSASSA-PKCS1-v1.5-verify-bignum
				  key signed-m signature))
			      keys))
		   ((dsa) ;; DSA
		    (try-keys (lambda (x) (isa? x Dsa-Key)) ;; key-test
			      (lambda (key)
				 (dsa-verify key
					     (bin-str->bignum hash)
					     (car signature)
					     (cdr signature)))
			      keys))
		   ((elgamal-encrypt/sign) ;; ElGamal
		    (warning "ElGamal signing not yet implemented")
		    (try-keys (lambda (x) (isa? x ElGamal-Key))
			      (lambda (key) #f)
			      keys))
		   (else
		    (warning "Signature algorithm not yet implemented "
			     public-key-algo " "
			     (public-key-algo->human-readable
			      public-key-algo))
		    #f))))))

   (with-trace 4 "verify-signature"
      (with-access::PGP-Signature-Packet sig (issuer)
	 (let ((possible-keys (key-resolver issuer)))
	    (verify-signature-for-keys msg sig possible-keys)))))

(define (key-id::bstring k::PGP-Key-Packet)
   (with-access::PGP-Key-Packet k (version key id)
      (when (not id)
	 (case version
	    ((3)
	     (unless (isa? key Rsa-Key)
		(error "key-id" "v3 key must contain RSA key" key))
	     (with-access::Rsa-Key key (modulus)
		(let* ((str (bignum->bin-str modulus))
		       (len (string-length str)))
		   (if (<fx len 8)
		       len ;; not sure that is correct
		       (set! id (substring str (-fx len 8) len))))))
	    ((4)
	     (let* ((fp (fingerprint k))
		    (len (string-length fp)))
		(set! id (substring fp (-fx len 8) len))))
	    (else
	     (error "key-id" "Unsupported version" version))))
      id))

(define-generic (fingerprint k::PGP-Key-Packet)
   (with-access::PGP-Key-Packet k (version key)
      (case version
	 ((3)
	  (unless (isa? key Rsa-Key)
	     (error "key-id" "v3 key must contain RSA key" key))
	  (with-access::Rsa-Key key (modulus exponent)
	     (let ((modulus-str (bignum->bin-str modulus))
		   (exp-str (bignum->bin-str exponent)))
		(md5sum-bin (string-append modulus-str exp-str)))))
	 ((4)
	  (let ((str-p (open-output-string)))
	     (encode-public-key-content k str-p)
	     (let* ((str (close-output-port str-p))
		    (len (string-length str))
		    (high (bit-and #xFF (bit-rsh len 8)))
		    (low (bit-and #xFF len))
		    (hashed-str (make-string (+fx len 3))))
		(string-set! hashed-str 0 (integer->char-ur #x99))
		(string-set! hashed-str 1 (integer->char-ur high))
		(string-set! hashed-str 2 (integer->char-ur low))
		(blit-string! str 0 hashed-str 3 len)
		(sha1sum-bin hashed-str))))
	 (else
	  (error "fingerprint" "Unsupported version" version)))))

(define (sign-msg msg::bstring hash::bstring hash-algo::symbol
		  key::PGP-Secret-Key-Decoded-Packet)
   ;; TODO: verify that key is still valid.
   (with-access::PGP-Secret-Key-Decoded-Packet key (secret-key
						    algo)
      (case algo
	 ((rsa-encrypt/sign rsa-sign)
	  ;; TODO: pass hash-algo to rsa-fun.
	  ;; Note: RSA receives the msg and not the hash.
	  (RSASSA-PKCS1-v1.5-sign-bignum secret-key msg))
	 ((dsa)  ;; DSA
	  (receive (r s)
	     (dsa-sign secret-key (bin-str->bignum hash))
	     (cons r s)))
	 ;((elgamal-encrypt/sign) 'TODO)
	 (else
	  (error "Signing algorithm not yet implemented"
		 algo
		 (cons algo (public-key-algo->human-readable algo)))))))

(define (needs-password-for-decryption? kp::PGP-Secret-Key-Packet)
   (with-access::PGP-Secret-Key-Packet kp (password-protected-secret-key-data)
      (let* ((secret password-protected-secret-key-data)
	     (protection (char->integer (string-ref secret 0))))
	 (not (zerofx? protection)))))
	 
(define (decrypt-password-protected secret::bstring
				    version::long
				    password::bstring)
   (define (verify-checksum data::bstring checksum::long len::long)
      (let loop ((i 0)
		 (sum 0))
	 (cond
	    ((and (=fx i len) (=fx checksum sum))
	     ;; everything is in order
	     (trace-item "checksum succeeded")
	     #t)
	    ((=fx i len)
	     (trace-item "checksum failed")
	     #f)
	    (else
	     (loop (+fx i 1)
		   (bit-and (+fx sum (char->integer (string-ref data i)))
			    #xFFFF))))))
   (define (verify-sha1-chksum data::bstring checksum::bstring len::long)
      (if (string=? checksum (sha1sum-bin (substring data 0 len)))
	  (begin
	     (trace-item "checksum succeeded")
	     #t)
	  (begin
	     (trace-item "checksum failed")
	     #f)))

   (with-trace 4 "decrypt-password-protected"
      (when (string-null? secret)
	 (error 'decode-secret-key-password-protected
		"secret must not be empty"
		""))
      (let ((protection (char->integer (string-ref secret 0))))
	 (trace-item "Protection: " protection)
	 (case protection
	    ((0) ;; not protected
	     (let* ((sdata-len (string-length secret))
		    (checksum (scalar->fixnum (substring secret
							 (-fx sdata-len 2)
							 sdata-len))))
		(when (not (verify-checksum secret checksum (-fx sdata-len 2)))
		   (error "verify checksum"
			  "Checksum verification failed on non-encrypted content"
			  #f))
		(substring secret 1 (-fx sdata-len 2))))
	    ((254 255) ;; password protected
	     (when (=fx version 3)
		(error "decode-secret-key-password"
		       "version 3 decoding is not yet implemented."
		       #f))
	     ;; 254 seems to be gnupg extension, where the checksum is actually
	     ;; a SHA1 hash.
	     (let* ((p (open-input-string secret))
		    (protection (safe-read-octet p)) ;; again.
		    (symmetric-algo-byte (safe-read-octet p))
		    (symmetric-algo (byte->symmetric-key-algo
				     symmetric-algo-byte))
		    (key-len (symmetric-key-algo-key-byte-len symmetric-algo))
		    (s2k (decode-s2k p))
		    (symmetric-key (apply-s2k s2k password key-len))
		    (IV (safe-read-octets 8 p))
		    (secret-data (read-string p))
		    (decrypter (symmetric-key-algo->procedure symmetric-algo #f))
		    (decoded (decrypter secret-data IV symmetric-key))
		    (dec-len (string-length decoded))
		    (chksum-len (if (=fx protection 254) 20 2))
		    (dummy (when (not (>=fx dec-len chksum-len))
			      (error "decode password-protected secret key"
				     "not enough bytes for checksum"
				     dec-len)))
		    (chksum-str (substring decoded (-fx dec-len chksum-len)
					   dec-len)))
		(trace-item "password-protection: " symmetric-algo-byte " "
		       (symmetric-key-algo->human-readable symmetric-algo))
		(trace-item "symmetric-key: " (str->hex-string symmetric-key))
		(trace-item "IV: " (str->hex-string IV))
		(trace-item "secret-data: " (str->hex-string secret-data)
			    " (len=" (string-length secret-data) ")")
		(trace-item "decoded: " (str->hex-string decoded)
			    " (len=" (string-length decoded) ")")
		;; in version 4 the secret-data contains the hash.
		;; in version 3 its more difficult...
		(if (=fx protection 254) ;; gpg sha1sum
		    (and (verify-sha1-chksum decoded chksum-str
					     (-fx dec-len chksum-len))
			 decoded)
		    (and (verify-checksum decoded (scalar->fixnum chksum-str)
					  (-fx dec-len chksum-len))
			 decoded))))
	    (else
	     (error "decode-password-protected-secret-key"
		    "bad magic byte"
		    protection))))))

(define (create-secret-rsa-key public-key d)
   (with-access::Rsa-Key public-key (modulus)
      (instantiate::Rsa-Key
	 (modulus modulus)
	 (exponent d))))
(define (create-secret-dsa-key public-key x)
   (with-access::Dsa-Key public-key (p q g y)
      (instantiate::Complete-Dsa-Key
	 (p p)
	 (q q)
	 (g g)
	 (y y)
	 (x x))))
(define (create-secret-elgamal-key public-key x)
   (with-access::ElGamal-Key public-key (p g y)
      (instantiate::Complete-ElGamal-Key
	 (p p)
	 (g g)
	 (y y)
	 (x x))))

(define (decrypt-secret-key! kp::PGP-Secret-Key-Packet password::bstring)
   (define (create-secret-key algo key decrypted)
      (case algo
	 ((rsa-encrypt/sign rsa-encrypt rsa-sign)
	  (let ((d (decode-mpi (open-input-string decrypted))))
	     (create-secret-rsa-key key d)))
	 ((elgamal-encrypt elgamal-encrypt/sign)
	  (let ((x (decode-mpi (open-input-string decrypted))))
	     (create-secret-elgamal-key key x)))
	 ((dsa) ;; DSA
	  (let ((x (decode-mpi (open-input-string decrypted))))
	     (create-secret-dsa-key key x)))
	 (else
	  (error "decrypt-secret-key"
		 "Algorithm not yet implemented"
		 (cons algo (public-key-algo->human-readable algo))))))

   (unless (isa? kp PGP-Secret-Key-Decoded-Packet)
      (with-access::PGP-Secret-Key-Packet kp
	    (password-protected-secret-key-data algo key version)
	 (let ((decrypted (decrypt-password-protected
			   password-protected-secret-key-data
			   version
			   password)))
	    (when decrypted
	       (widen!::PGP-Secret-Key-Decoded-Packet kp
		  (secret-key (create-secret-key algo key decrypted))))))))

;; verifies a PGP-Signature (which includes One-Pass-Signatures), for
;; One-Pass-Signatures the msg is already contained inside the
;; signature. Therefore the msg is optional.
;; When given it should however be a string.
(define (verify-pgp-signature::pair-nil sig::PGP-Signature
					key-resolver::procedure
					#!optional (msg #f))
   (when (and (not msg)
	      (not (with-access::PGP-Signature sig (msg) msg)))
      (error "verify-pgp-signature" "Missing message" #f))
   (let ((sig-msg (with-access::PGP-Signature sig (msg) msg)))
      (when (and msg sig-msg)
	 (let ((literal (with-access::PGP-Literal-Packet sig-msg (data) data)))
	    (when (or (not (string? msg))
		      (not (string=? msg literal)))
	       (error "verify-pgp-signature"
		      "Given messages are not the same or not strings"
		      (cons msg literal)))))
      (let ((str (or msg (with-access::PGP-Literal-Packet sig-msg (data) data))))
	 (filter-map (lambda (s)
			(with-access::PGP-Signature-Packet s (signature-type)
			   (verify-signature
			    (construct-data-signature-msg str signature-type)
			    s
			    key-resolver)))
		     (with-access::PGP-Signature sig (sigs) sigs)))))

(define (signature->issuer-id sig::PGP-Signature-Packet
			      #!key (key #f) (key-resolver #f))
   (with-access::PGP-Signature-Packet sig (issuer)
      (cond
	 ((and (not key) (not key-resolver))
	  (str->hex-string issuer))
	 (key
	  (key-str-representation key))
	 (else
	  (let ((possible-keys (key-resolver issuer)))
	     (cond
		((null? possible-keys)
		 (signature->issuer-id sig))
		((null? (cdr possible-keys)) ;; only one match
		 ;; assuming this is the key
		 (signature->issuer-id
		  sig :key (with-access::PGP-Subkey (car possible-keys) (pgp-key)
			      pgp-key)))
		(else ;; more than one match
		 (signature->issuer-id sig))))))))

(define (key-str-representation key::PGP-Key)
   (with-access::PGP-Key key (user-ids)
      (let ((main-key-pkt (PGP-main-key-packet key)))
	 (with-output-to-string
	    (lambda ()
	       (print (str->hex-string (key-id main-key-pkt)))
	       (for-each (lambda (signed-id)
			    (with-access::Signed-ID signed-id (id)
			       (with-access::PGP-ID-Packet id (data)
				  (print data))))
			 user-ids))))))
   
   
(define (verify-key pgp-key::PGP-Key key-resolver::procedure)
   ;; problem: we want to be able to verify the given key, even if it's not yet
   ;; in the resolver. -> we look at the given key first.
   ;; Note: on the rare occasions where there is actually a conflict the
   ;; key-resolver will not be invoked and the given key shadows all other keys
   ;; of the same id.
   (define (local-resolve id)
      (with-access::PGP-Key pgp-key (subkeys)
	 (cond
	    ((any (lambda (sk)
		     (with-access::PGP-Subkey sk (key-packet)
			(and (string=? id (key-id key-packet))
			     sk)))
		  subkeys)
	     =>
	     (lambda (k) (list k)))
	    (else (key-resolver id)))))

   (define (main-key) (PGP-main-key pgp-key))
   (define (main-key-packet) (PGP-main-key-packet pgp-key))

   (define (verify-user-id id::Signed-ID)
      (with-access::Signed-ID id (id sigs)
	 (trace-item "Verifying user-id")
	 (trace-item (with-access::PGP-ID-Packet id (data) data))
	 (when (null? sigs)
	    (trace-item "No signature for user-id"))
	 (for-each
	  (lambda (sig)
	     (with-access::PGP-Signature-Packet sig (version
						     signature-type)
		(let* ((str (construct-certification-signature-str
			     (with-access::PGP-ID-Packet id (data) data)
			     (main-key-packet)
			     signature-type version))
		       (signer (verify-signature str sig local-resolve)))
		   (cond
		      ((not signer)
		       (trace-item "Unverified signature from: "
			      (signature->issuer-id sig)))
		      ((eq? signer (with-access::PGP-Key pgp-key (subkeys)
				      (car subkeys)))
		       (trace-item "Valid self signature"))
		      (else
		       (trace-item "Valid signature from: "
			      (signature->issuer-id sig :key signer)))))))
	  sigs)))

   (define (verify-revocation-sig revoc-sig key-packet)
      (with-access::PGP-Signature-Packet revoc-sig (issuer signature-type)
	 (when (not (eq? signature-type 'key-revocation))
	    (error 'verify-key
		   "Revocation signature is not of revocation-type"
		   (cons signature-type
			 (signature-type->human-readable signature-type))))
	 (let* ((str (construct-key-signature-str key-packet signature-type))
		(signer (verify-signature str revoc-sig local-resolve)))
	    (cond
	       ((eq? pgp-key (with-access::PGP-Subkey signer (pgp-key) pgp-key))
		(trace-item "REVOKED. valid self revocation signature"))
	       (signer
		(trace-item "MAYBE REVOKED. Valid signature from: "
		       (signature->issuer-id revoc-sig :key signer))
		;; TODO find valid self-signature that confirms that this
		;; person actually has the right to revoke our key.
		)
	       (else
		(trace-item "Signature could not be verified."))))))

   (define (verify-subkey-binding-sig sig subkey-pkt::PGP-Key-Packet)
      (with-access::PGP-Signature-Packet sig (signature-type)
	 (when (not (eq? signature-type 'subkey-binding))
	    (error 'verify-subkey-binding
		   "Subkey-binding signature expected"
		   (cons signature-type
			 (signature-type->human-readable signature-type))))
	 (let* ((str (construct-subkey-signature-str subkey-pkt
						     (main-key-packet)
						     signature-type))
		;; binding signatures must be done by main-key.
		(signer (verify-signature str sig (lambda (id) (main-key)))))
	    (if (eq? signer (main-key)) ;; otherwise must be #f
		(trace-item "Valid subkey-binding signature")
		(trace-item "Signature could not be verified (BAD)")))))
		     
   (define (verify-subkey subkey)
      (with-access::PGP-Subkey subkey (key-packet sigs revocation-sigs)
	 (when (and (null? sigs) (not (eq? subkey (main-key))))
	    (trace-item "BAD BAD subkey has no binding signature"))

	 (for-each (lambda (sig)
		      (verify-subkey-binding-sig sig key-packet))
		   sigs)
	 (when (not (null? revocation-sigs))
	    (trace-item "key has revocation signature(s)")
	    (for-each (lambda (sig)
			 (verify-revocation-sig sig key-packet))
		      revocation-sigs))))


   (with-trace 4 "verify-key"
      (with-access::PGP-Key pgp-key (subkeys user-ids)
	 (for-each (lambda (user-id) (verify-user-id user-id))
		   user-ids)
	 (for-each (lambda (subkey)  (verify-subkey subkey))
		   subkeys))))
	   
;; 1:=> (define testy-key (car (decode-armored-pgp-file "/home/flo/NOSAVE/tmp/crypto/rsa-testy.key")))
;; 1:=> (define ignored (decrypt-secret-key! testy-key "xyz"))
;; 1:=> (define sig (create-sig-packet-v4 "abcd" testy-key '() '()))
;; 1:=> (define p (open-output-string)) (encode-packet sig p) (define sig-p (close-output-port p))
;; 1:=> (with-output-to-file "/tmp/tt.sig" (lambda () (display sig-p)))
(define (create-sig-packet-v4::PGP-Signature-v4-Packet
	 msg::bstring
	 key::PGP-Secret-Key-Decoded-Packet
	 ;; usually the following ones will be '()
	 signed-sub-packets::pair-nil
	 unsigned-sub-packets::pair-nil
	 #!key (hash-algo 'sha-1)
	 (signature-type 'binary))
   (let* ((k-id (key-id key))
	  (date (current-date))
	  (algo (with-access::PGP-Key-Packet key (algo) algo))
	  (signature-type signature-type) ;; binary
	  (prefix (create-signed-packet-prefix-v4
		   signature-type
		   algo
		   hash-algo
		   date
		   signed-sub-packets))
	  (trailer (make-string 6)))
      ;; magic trailer bytes. (see 5.2.4, page 31 of RFC2440)
      (string-set! trailer 0 (integer->char-ur #x04))
      (string-set! trailer 1 (integer->char-ur #xFF))
      (blit-string! (fixnum->scalar (string-length prefix) 4) 0
		    trailer 2
		    4)
      (let* ((signed-m (string-append msg prefix trailer))
	     (hash-fun (hash-algo->procedure hash-algo))
	     (hash (hash-fun signed-m))
	     (left-hash (substring hash 0 2))
	     (sig (sign-msg signed-m hash hash-algo key)))
	 (instantiate::PGP-Signature-v4-Packet
	    (version 4)
	    (signature-type signature-type)
	    (issuer k-id)
	    (public-key-algo algo)
	    (hash-algo hash-algo)
	    (creation-date date)
	    (signature sig)
	    (signed-packet-prefix prefix)
	    (hash-trailer trailer)
	    (left-hash left-hash)
	    (secure-sub-packets '())
	    (insecure-sub-packets '())))))

(define (decoded-key-packet key::PGP-Subkey password-provider)
   (with-trace 4 "decoded-key-packet"
      (with-access::PGP-Subkey key (key-packet)
	 (cond
	    ((isa? key-packet PGP-Secret-Key-Decoded-Packet)
	     key-packet)
	    ((not password-provider)
	     (trace-item "No password provider")
	     (error "decode-key"
		"no password-provider has been given"
		#f))
	    ((not (and (procedure? password-provider)
		       (correct-arity? password-provider 1)))
	     (trace-item "Illegal password provider")
	     (error "decode-key"
		"Illegal password provider"
		password-provider))
	    ((isa? key-packet PGP-Key-Packet)
	     (let loop ((count 3))
		(when (=fx count 0)
		   (trace-item "No password attempt left")
		   (error "decode-key"
		      "no password attempt left"
		      #f))
		(let ((pass (password-provider key)))
		   (cond
		      ((not pass)
		       ;; user doesn't want to give us a password. Ignore.
		       (loop (-fx count 1)))
		      ((not (string? pass))
		       (trace-item "password-provider returned no string")
		       (error 'decode-key
			  "bad password (not a string)"
			  pass))
		      (else
		       (decrypt-secret-key! key-packet pass)
		       (unless (isa? key-packet PGP-Secret-Key-Decoded-Packet)
			  (loop (-fx count 1)))))))
	     key-packet)
	    (else
	     (error "decode-key" "Invalid key" key))))))
   
;; key must a secret Subkey.
;; password-provider will be called with the key as parameter and must return
;; the password for the given key (unless the key is already opened).
(define (create-one-pass-signature::PGP-One-Pass-Signature
	 msg::bstring key::PGP-Subkey
	 #!key (password-provider #f) (hash-algo 'sha-1)
	 (file-name #f) (creation-date #f))
   (let* ((secret-key-packet (decoded-key-packet key password-provider))
	  (sig-pkt (create-sig-packet-v4 msg secret-key-packet '() '()
					 :hash-algo hash-algo)))
      (with-access::PGP-Signature-Packet sig-pkt
	    (version signature-type issuer public-key-algo hash-algo)
	 (let ((one-pass-pkt (instantiate::PGP-One-Pass-Signature-Packet
				;; RFC2440 one-pass signature version is 3
				(version 3)
				(signature-type signature-type)
				(issuer issuer)
				(public-key-algo public-key-algo)
				(hash-algo hash-algo)
				(contains-nested-sig? #f)))
	       (literal (instantiate::PGP-Literal-Packet
			   (format 'binary)
			   (for-your-eyes-only? #f)
			   (file-name (or file-name ""))
			   (creation-date (or creation-date (current-date)))
			   (data msg))))
	    (instantiate::PGP-One-Pass-Signature
	       (msg literal)
	       (sigs (list sig-pkt))
	       (one-pass-sigs (list one-pass-pkt)))))))

(define (create-pgp-signature::PGP-Signature
	 msg::bstring key::PGP-Subkey
	 #!key (password-provider #f) (detached-signature? #f)
	 (hash-algo 'sha-1)
	 (file-name #f) (creation-date #f))
   (let* ((secret-key-packet (decoded-key-packet key password-provider))
	  (sig-pkt (create-sig-packet-v4 msg secret-key-packet '() '()
					 :hash-algo hash-algo)))
      (if detached-signature?
	  (instantiate::PGP-Signature
	     (msg #f)
	     (sigs (list sig-pkt)))
	  (instantiate::PGP-Signature
	     (msg (instantiate::PGP-Literal-Packet
		     (format 'binary)
		     (for-your-eyes-only? #f)
		     (file-name (or file-name ""))
		     (creation-date (or creation-date (current-date)))
		     (data msg)))
	     (sigs (list sig-pkt))))))

(define (signature-less sig1 sig2)
   (with-access::PGP-Signature-Packet sig1 ((bns1 signature)
					    (algo1 public-key-algo)
					    (iss1 issuer)
					    (spp1 signed-packet-prefix)
					    (cdate1 creation-date))
      (with-access::PGP-Signature-Packet sig2 ((bns2 signature)
					       (algo2 public-key-algo)
					       (iss2 issuer)
					       (spp2 signed-packet-prefix)
					       (cdate2 creation-date))
	 (let ((ds1 (date->seconds cdate1))
	       (ds2 (date->seconds cdate2)))
	    (cond
	       ((not (=fx ds1 ds2))
		(<fx ds1 ds2))
	       ((not (string=? spp1 spp2))
		(string<? spp1 spp2))
	       ((not (string=? iss1 iss2))
		(string<? iss1 iss2))
	       ((not (eq? algo1 algo2))
		(string<? (symbol->string algo1) (symbol->string algo2)))
	       ((bignum? bns1)
		(<bx bns1 bns2))
	       ((pair? bns1)
		(if (not (=bx (car bns1) (car bns2)))
		    (<bx (car bns1) (car bns2))
		    (if (bignum? (cdr bns1))
			(<bx (cdr bns1) (cdr bns2))
			(error "signature-less"
			   "could not compare signatures"
			   #f))))
	       (else
		(error "signature-less"
		   "could not compare signatures"
		   #f)))))))

(define (merge-sigs sigs1 sigs2)
   (let loop ((sigs1 (sort signature-less sigs1))
	      (sigs2 (sort signature-less sigs2))
	      (res '()))
      (cond
	 ((and (null? sigs1) (null? sigs2))
	  res)
	 ((null? sigs1)
	  (append res sigs2))
	 ((null? sigs2)
	  (append res sigs1))
	 ((signature-less (car sigs1) (car sigs2))
	  (loop (cdr sigs1) sigs2 (cons (car sigs1) res)))
	 ((signature-less (car sigs2) (car sigs1))
	  (loop sigs1 (cdr sigs2) (cons (car sigs2) res)))
	 (else ;; equal.
	  (loop (cdr sigs1) (cdr sigs2) (cons (car sigs1) res))))))

(define (signed-id-less id1 id2)
   (with-access::Signed-ID id1 ((id1 id))
      (with-access::Signed-ID id2 ((id2 id))
	 (with-access::PGP-ID-Packet id1 ((data1 data))
	    (with-access::PGP-ID-Packet id2 ((data2 data))
	       (string<? data1 data2))))))

(define (merge-user-ids ids1 ids2)
   (let loop ((ids1 (sort signed-id-less ids1))
	      (ids2 (sort signed-id-less ids2))
	      (res '()))
      (cond
	 ((and (null? ids1) (null? ids2))
	  res)
	 ((null? ids1)
	  (append res ids2))
	 ((null? ids2)
	  (append res ids1))
	 ((signed-id-less (car ids1) (cadr ids2))
	  (loop (cdr ids1) ids2 (cons (car ids1) res)))
	 ((signed-id-less (car ids2) (cadr ids1))
	  (loop ids1 (cdr ids2) (cons (car ids2) res)))
	 (else ;; same id
	  (with-access::Signed-ID (car ids1) ((id1 id) (sigs1 sigs))
	     (with-access::Signed-ID (car ids2) ((sigs2 sigs))
		(loop (cdr ids1) (cdr ids1)
		   (cons (instantiate::Signed-ID
			    (id id1)
			    (sigs (merge-sigs sigs1 sigs2)))
		      res))))))))

(define (key-less k1 k2)
   ;; key-ids are much faster than fingerprints -> try them first.
   (let ((id1 (key-id k1))
	 (id2 (key-id k2)))
      (if (string=? id1 id2)
	  (string<? (fingerprint k1) (fingerprint k2))
	  (string<? id1 id2))))

(define (subkey-less sk1 sk2)
   (with-access::PGP-Subkey sk1 ((key-packet1 key-packet))
      (with-access::PGP-Subkey sk2 ((key-packet2 key-packet))
	 (key-less key-packet1 key-packet2))))

(define (merge-subkey skey1 skey2)
   (with-access::PGP-Subkey skey1 ((sigs1 sigs)
				   (revocs1 revocation-sigs)
				   (key-packet1 key-packet))
      (with-access::PGP-Subkey skey2 ((sigs2 sigs)
				      (revocs2 revocation-sigs))
	 (instantiate::PGP-Subkey
	    (key-packet key-packet1)
	    (sigs (merge-sigs sigs1 sigs2))
	    (revocation-sigs (merge-sigs revocs1 revocs2))
	    (pgp-key (class-nil PGP-Key))))))

(define (merge-subkeys sks1 sks2)
   (let loop ((sks1 (sort subkey-less sks1))
	      (sks2 (sort subkey-less sks2))
	      (res '()))
      (cond
	 ((and (null? sks1) (null? sks2))
	  res)
	 ((null? sks1)
	  (append res sks2))
	 ((null? sks2)
	  (append res sks1))
	 ((subkey-less (car sks1) (car sks2))
	  (loop (cdr sks1) sks2 (cons (car sks1) res)))
	 ((subkey-less (car sks2) (car sks1))
	  (loop sks1 (cdr sks2) (cons (car sks2) res)))
	 ;; same key
	 (else
	  (loop (cdr sks1) (cdr sks2)
		(cons (merge-subkey (car sks1) (car sks2))
		      res))))))

;; assumes that the two keys have been verified.
(define (merge-keys::PGP-Key key1::PGP-Key key2::PGP-Key)
   (with-access::PGP-Key key1 ((subkeys1 subkeys)
			       (user-ids1 user-ids))
      (with-access::PGP-Key key2 ((subkeys2 subkeys)
				  (user-ids2 user-ids))
	 (let* ((main-key1 (car subkeys1))
		(main-key2 (car subkeys2)))
	    (with-access::PGP-Subkey main-key1 ((kp1 key-packet))
	       (with-access::PGP-Subkey main-key2 ((kp2 key-packet))
		  (unless (string=? (fingerprint kp1) (fingerprint kp2))
		     (error "merge-keys" "Keys are not the same" #f))))
	    (let* ((merged-main-key (merge-subkeys main-key1 main-key2))
		   (other-keys (merge-subkeys (cdr subkeys1) (cdr subkeys2)))
		   (all-keys (cons merged-main-key other-keys))
		   (res-key (instantiate::PGP-Key
			       (subkeys all-keys)
			       (user-ids (merge-user-ids user-ids1 user-ids2)))))
	       (for-each (lambda (sk)
			    (with-access::PGP-Subkey sk (pgp-key)
			       (set! pgp-key res-key)))
		  all-keys)
	       res-key)))))

(define (key-packet->human-readable k-pkt)
   (with-access::PGP-Key-Packet k-pkt (algo)
      (string-append (str->hex-string (key-id k-pkt))
	 " " (public-key-algo->human-readable algo))))

(define (pgp-key->human-readable key)
   (with-output-to-string
      (lambda ()
	 (with-access::PGP-Key key (user-ids subkeys)
	    (for-each (lambda (user-id)
			 (with-access::Signed-ID user-id ((id-pkt id))
			    (with-access::PGP-ID-Packet id-pkt (data)
			       (print data))))
		      user-ids)
	    (for-each (lambda (subkey)
			 (with-access::PGP-Subkey subkey (key-packet)
			    (print (key-packet->human-readable key-packet))))
		      subkeys)))))

(define (pgp-subkey->human-readable pgp-subkey)
   (with-output-to-string
      (lambda ()
	 (with-access::PGP-Subkey pgp-subkey (key-packet pgp-key)
	    (with-access::PGP-Key pgp-key (user-ids)
	       (for-each (lambda (user-id)
			    (with-access::Signed-ID user-id ((id-pkt id))
			       (with-access::PGP-ID-Packet id-pkt (data)
				  (display data)
				  (display " "))))
			 user-ids)
	       (display (key-packet->human-readable key-packet)))))))

(define (make-0-IV len) (make-string len #\null))

(define (symmetric-encrypt::PGP-Symmetrically-Encrypted-Packet
	 data::PGP-Packet key-string::bstring algo::symbol
	 #!key (mdc #t))
   (with-trace 4 "symmetric-encrypt"
      (trace-item "Key-string: " (str->hex-string key-string))
      (trace-item "Algo: " (symmetric-key-algo->human-readable algo))
      (trace-item "Use Modification Detection Code: " mdc)
      (if mdc
	  (mdc-symmetric-encrypt data key-string algo)
	  (non-mdc-symmetric-encrypt data key-string algo))))

(define (non-mdc-symmetric-encrypt data key-string algo)
   (with-trace 5 "non-mdc-symmetric-encrypt"
      ;; RFC 5.7
      (let* ((algo-block-len (symmetric-key-algo-block-byte-len algo))
	     (encrypter (symmetric-key-algo->procedure algo #t))
	     (random-head (make-random-string 10)) ;; last two bytes will be changed.
	     (packet-p (open-output-string))
	     (encoded (encode-packets packet-p data))
	     (literal-data (close-output-port packet-p)))
	 (string-set! random-head 8 (string-ref random-head 6))
	 (string-set! random-head 9 (string-ref random-head 7))
	 (trace-item "literal data: "
		(str->hex-string (substring literal-data
					    0
					    (min 50 (string-length literal-data)))))
	 (trace-item "random prefix: " (str->hex-string random-head))
	 (if (<= algo-block-len 8)
	     ;; After encrypting the first 10 octets, the CFB state is
	     ;; resynchronized if the cipher block size is 8 octets or less.  The
	     ;; last 8 octets of ciphertext are passed through the cipher and the
	     ;; block boundary is reset
	     (let* ((head (encrypter random-head
				     (make-0-IV algo-block-len)
				     key-string))
		    (encrypted-data (encrypter literal-data
					       (substring head 2 10)
					       key-string)))
		(trace-item "encrypted head: " (str->hex-string head))
		(instantiate::PGP-Symmetrically-Encrypted-Packet
		   (data (string-append head encrypted-data))))
	     (let ((encrypted (encrypter (string-append random-head literal-data)
					 (make-0-IV algo-block-len)
					 key-string)))
		(trace-item "encrypted without resynchronization of IV")
		(instantiate::PGP-Symmetrically-Encrypted-Packet
		   (data encrypted)))))))

(define (mdc-symmetric-encrypt data key-string algo)
   (with-trace 5 "mdc-symmetric-encrypt"
      (trace-item "algo: " algo " "
	     (symmetric-key-algo->human-readable algo))
      
      ;; RFC 5.7
      (let* ((algo-block-len (symmetric-key-algo-block-byte-len algo))
	     (encrypter (symmetric-key-algo->procedure algo #t))
	     ;; last two bytes of random-head will will be changed.
	     (random-head (make-random-string (+fx algo-block-len 2)))
	     (packet-p (open-output-string)))
	 (trace-item "algo-block-len: " algo-block-len)
	 (encode-packets packet-p data)
	 ;; last two bytes of random-head must be the same as the preceding bytes.
	 (string-set! random-head algo-block-len
		      (string-ref random-head (-fx algo-block-len 2)))
	 (string-set! random-head (+fx algo-block-len 1)
		      (string-ref random-head (-fx algo-block-len 1)))
	 (trace-item "random head: " (str->hex-string random-head))
	 ;; Attach MDC-packet tag octets.
	 (display (integer->char-ur #xD3) packet-p)
	 (display (integer->char-ur #x14) packet-p)
	 (let* ((literal-data (close-output-port packet-p))
		(hash-string (string-append random-head literal-data))
		(hash (sha1sum-bin hash-string))
		(payload (string-append hash-string hash)))
	    (trace-item "mdc hash: " (str->hex-string hash))
	    (trace-item "literal data: "
		   (str->hex-string
		    (substring literal-data
			       0
			       (min 50 (string-length literal-data)))))
	    (let ((encrypted (encrypter payload
					(make-0-IV algo-block-len)
					key-string)))
	       (instantiate::PGP-MDC-Symmetrically-Encrypted-Packet
		  (version 1)
		  (data encrypted)))))))

(define (symmetric-decrypt
	 encrypted::PGP-Symmetrically-Encrypted-Packet
	 key-string::bstring algo::symbol)
   (with-trace 5 "symmetric-decrypt"
      (trace-item "Key-string: " (str->hex-string key-string))
      (trace-item "Algo: " (symmetric-key-algo->human-readable algo))
      (trace-item (with-access::PGP-Symmetrically-Encrypted-Packet encrypted
			((encrypted-str data))
		     (format "encrypted data: len=~a data=~a"
			(string-length encrypted-str)
			(str->hex-string
			   (substring encrypted-str
			      0
			      (min 50 (string-length encrypted-str)))))))
      (if (isa? encrypted PGP-MDC-Symmetrically-Encrypted-Packet)
	  (mdc-symmetric-decrypt encrypted key-string algo)
	  (non-mdc-symmetric-decrypt encrypted key-string algo))))

(define (non-mdc-symmetric-decrypt
	 encrypted::PGP-Symmetrically-Encrypted-Packet
	 key-string::bstring algo::symbol)
   (with-trace 5 "non-mdc-symmetric-decrypt"
      ;; RFC 5.7
      (with-access::PGP-Symmetrically-Encrypted-Packet encrypted (data)
	 (let* ((algo-block-len (symmetric-key-algo-block-byte-len algo))
		(decrypter (symmetric-key-algo->procedure algo #f)))
	    (cond
	       ((not (>= (string-length data) 10))
		#f)
	       ((<= algo-block-len 8)
		;; After encrypting the first 10 octets, the CFB state is
		;; resynchronized if the cipher block size is 8 octets or less.  The
		;; last 8 octets of ciphertext are passed through the cipher and the
		;; block boundary is reset
		(let* ((c-head (substring data 0 10))
		       (d-head (decrypter c-head
				  (make-0-IV algo-block-len)
				  key-string)))
		   (trace-item "decrypted head: " (str->hex-string d-head))
		   (if (and (char=? (string-ref d-head 6) (string-ref d-head 8))
			    (char=? (string-ref d-head 7) (string-ref d-head 9)))
		       ;; good. password seemed to be fine.
		       (let* ((resync-iv (substring data 2 10))
			      (c-rest (substring data 10 (string-length data)))
			      (d-rest (decrypter c-rest resync-iv key-string))
			      (packet-p (open-input-string d-rest)))
			  (trace-item "Password seemed to be fine")
			  (trace-item "Resynched IV: " (str->hex-string resync-iv))
			  (decode-packets packet-p))
		       (begin
			  (trace-item "Password failed")
			  #f))))
	       (else
		(let* ((c-head (substring data 0 10))
		       (d-head (decrypter c-head
				  (make-0-IV algo-block-len)
				  key-string)))
		   (trace-item "decrypted head: " (str->hex-string d-head))
		   (if (and (char=? (string-ref d-head 6) (string-ref d-head 8))
			    (char=? (string-ref d-head 7) (string-ref d-head 9)))
		       ;; good. password seemed to be fine.
		       (let* ((d-all (decrypter data
					(make-0-IV algo-block-len)
					key-string))
			      (d-rest (substring d-all 10 (string-length d-all)))
			      (packet-p (open-input-string d-rest)))
			  (trace-item "Password seemed to be fine")
			  (decode-packets packet-p))
		       (begin
			  (trace-item "Password failed")
			  #f)))))))))

;; strips the decoded output from its MDC packet.
(define (mdc-symmetric-decrypt
	 encrypted::PGP-MDC-Symmetrically-Encrypted-Packet
	 key-string::bstring algo::symbol)
   (with-trace 5 "mdc-symmetric-decrypt"
      ;; RFC 4880, 5.13
      (with-access::PGP-MDC-Symmetrically-Encrypted-Packet encrypted
	    (version data)
	 (let* ((algo-key-len (symmetric-key-algo-key-byte-len algo))
		(algo-block-len (symmetric-key-algo-block-byte-len algo))
		(decrypter (symmetric-key-algo->procedure algo #f)))
	    (when (not (=fx version 1))
	       (warning "mdc-symmetric-decrypt: bad version. we will try "
		  "(but probably fail)."))
	    (let* ((c-head (substring data 0 (+fx algo-block-len 2)))
		   (d-head (decrypter c-head
			      (make-0-IV algo-block-len)
			      key-string)))
	       (trace-item "decrypted head: " (str->hex-string d-head))
	       (if (and (char=? (string-ref d-head (-fx algo-block-len 2))
			   (string-ref d-head algo-block-len))
			(char=? (string-ref d-head (-fx algo-block-len 1))
			   (string-ref d-head (+fx algo-block-len 1))))
		   ;; good. password seemed to be fine.
		   (let* ((dummy (trace-item "Password seemed to be fine (data len="
				    (string-length data) ")"))
			  (d-all (decrypter data
				    (make-0-IV algo-block-len)
				    key-string))
			  (d-rest (substring d-all
				     (+fx algo-block-len 2)
				     (string-length d-all)))
			  (dummyR (trace-item "encoded package (len="
				     (string-length d-rest) ") "
				     (str->hex-string d-rest)))
			  (packet-p (open-input-string d-rest))
			  (packets (decode-packets packet-p))
			  (dummy-l (cons 'dummy packets)))
		      (let loop ((ps packets)
				 (previous dummy-l))
			 (cond
			    ((or (null? ps)
				 (and (null? (cdr ps))
				      (not (isa? (car ps) PGP-MDC-Packet))))
			     (error "mdc-symmetric-decrypt"
				"No MDC packet found. (tampered data?)"
				#f))
			    ((and (not (null? (cdr ps)))
				  (isa? (car ps) PGP-MDC-Packet))
			     (error "mdc-symmetric-decrypt"
				"MDC packet found at bad position. (tampered data?)"
				#f))
			    ((not (null? (cdr ps)))
			     (loop (cdr ps) ps))
			    (else
			     (let* ((mdc (car ps))
				    (mdc-hash (with-access::PGP-MDC-Packet mdc
						    (hash)
						 hash))
				    (hashed-len (-fx (string-length d-all) 20))
				    (hashed-str (substring d-all 0 hashed-len))
				    (decrypted-hash (sha1sum-bin hashed-str)))
				(when (not (string=? decrypted-hash mdc-hash))
				   (error "mdc-symmetric-decrypt"
				      "Modification Detection detected invalid hash. (tampered data?)"
				      #f))
				(set-cdr! previous '())
				(cdr dummy-l))))))
		   (begin
		      (trace-item "Password failed")
		      #f)))))))

(define (decrypt-symmetric-key-session-key
	 session-packet::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
	 key-string::bstring)
   (with-trace 5 "decrypt-symmetric-key-session-key"
      (with-access::PGP-Symmetric-Key-Encrypted-Session-Key-Packet session-packet
	    (algo s2k encrypted-session-key)
	 (let* ((block-len (symmetric-key-algo-block-byte-len algo))
		(key-len (symmetric-key-algo-key-byte-len algo))
		(symmetric-key (apply-s2k s2k key-string key-len)))
	    (trace-item "session-packet with algo: "
		   (symmetric-key-algo->human-readable algo))
	    (trace-item "symmetric key (result of s2k) used to decrypt session key: "
		   (str->hex-string symmetric-key))
	    (if (not encrypted-session-key)
		(begin
		   (trace-item "no session-key-string. returning s2k result.")
		   (values algo symmetric-key))
		(let* ((decrypter (symmetric-key-algo->procedure algo #f))
		       (decrypted (decrypter encrypted-session-key
					     (make-0-IV block-len)
					     symmetric-key)))
		   ;key-string)))
		   (when (not (> (string-length decrypted) 1))
		      (error 'decrypt-symmetric-key-session-key
			     "bad decrypted session key (length <= 1)"
			     (str->hex-string decrypted)))
		   (trace-item "encrypted secret-data: "
			  (str->hex-string encrypted-session-key))
		   (trace-item "decrypted secret-data: " (str->hex-string decrypted))
		   (let* ((main-algo-byte (char->integer (string-ref decrypted 0)))
			  (main-algo (byte->symmetric-key-algo main-algo-byte))
			  (session-key (substring decrypted 1 (string-length decrypted))))
		      (trace-item "algo: " (symmetric-key-algo->human-readable main-algo))
		      (trace-item "key: " (str->hex-string session-key))
		      (values main-algo session-key))))))))

(define (chksum-16-bit str::bstring)
   (define (byte-ref str i)
      (char->integer (string-ref str i)))

   (let loop ((i 0)
	      (sum 0))
      (if (=fx i (string-length str))
	  sum
	  (loop (+fx i 1)
		(modulofx (+fx sum (byte-ref str i))
			  65536)))))
   
(define (decrypt-public-key-session-key
	 session-packet::PGP-Public-Key-Encrypted-Session-Key-Packet
	 subkey::PGP-Subkey
	 password-provider)
   ;; rfc 2440. 5.1

   (define (byte-ref str i)
      (char->integer (string-ref str i)))

   (define (decompose-and-check decrypted)
      (trace-item "decrypted str: " (str->hex-string decrypted))
      ;; The decrypted string might come from a bignum. In this case it doesn't
      ;; have any leading 0. However the unpadding requires it to start with an
      ;; '\0' character. Simply add it.
      ;; A cleaner solution would be to generate the mpi-string depending on
      ;; the size of the key. In this case it would naturally come with a
      ;; leading '\0' (at least if nothing went wrong).
      (if (not (char=? #\null (string-ref decrypted 0)))
	  (decompose-and-check (string-append (string #\null) decrypted))
	  (let* ((unpadded (PKCS1-v1.5-unpad decrypted 2))
		 (len (string-length unpadded)))
	     (when (not (> len 3))
		(error 'decrypt-public-key-session-key
		       "Bad decrypted session-key"
		       (str->hex-string unpadded)))
	     (trace-item "Unpadded data: " (str->hex-string unpadded))
	     (let* ((algo-byte (byte-ref unpadded 0))
		    (algo (byte->symmetric-key-algo algo-byte))
		    (session-key-str (substring unpadded 1 (-fx len 2)))
		    (expected-chksum (+fx (*fx 256
					       (byte-ref unpadded (-fx len 2)))
					  (byte-ref unpadded (-fx len 1))))
		    (actual-chksum (chksum-16-bit session-key-str)))
		;; check chksum.
		(when (not (=fx actual-chksum expected-chksum))
		   (trace-item "bad checksum." actual-chksum " != " expected-chksum)
		   (error 'decrypt-public-key-session-key
			  "Bad checksum"
			  actual-chksum))
		(trace-item "Decomposed symmetric algo: " algo-byte " "
		       (symmetric-key-algo->human-readable algo))
		(values algo session-key-str)))))

   (with-trace 3 "decrypt-public-key-session-key"
      (let ((secret-key-packet (decoded-key-packet subkey password-provider)))
	 (with-access::PGP-Secret-Key-Decoded-Packet secret-key-packet (secret-key)
	    (with-access::PGP-Public-Key-Encrypted-Session-Key-Packet session-packet
		  (algo encrypted-session-key)
	       (trace-item "have secret key")
	       (trace-item "trying to decrypt session packet using the key")
	       (case algo
		  ((rsa-encrypt/sign rsa-encrypt)
		   (if (not (isa? secret-key Rsa-Key))
		       (begin
			  (trace-item "Expected Rsa key. (got something else)")
			  #f)
		       (let* ((decrypted (rsa-decrypt secret-key
						      encrypted-session-key))
			      (decrypted-str (bignum->bin-str decrypted)))
			  (decompose-and-check decrypted-str))))
		  ((elgamal-encrypt elgamal-encrypt/sign) ;; ElGamal
		   (if (not (isa? secret-key ElGamal-Key))
		       (begin
			  (trace-item "Expected ElGamal key. (got something else)")
			  #f)
		       (let* ((r (car encrypted-session-key))
			      (s (cdr encrypted-session-key))
			      (decrypted (elgamal-decrypt secret-key r s))
			      (decrypted-str (bignum->bin-str decrypted)))
			  (decompose-and-check decrypted-str))))
		  (else
		   (warning "Decryption algorithm not yet implemented "
			    algo " "
			    (public-key-algo->human-readable algo))
		   #f)))))))

;; if session-key is #f then the s2k of the given password should be used as
;; session-key. In this case (values session-key packet) is returned.
;; With a session-key the symmetric-algo is used to encrypt the
;; session-key. Only the encrypted packet is returned.
(define (create-password-session-key-packet
	 password::bstring
	 session-key
	 data-symmetric-algo ;; Used to encrypt the payload.
	 #!key
	 (hash-algo 'sha-1)
	 ;; The symmetric algo is used to encrypt the password (if any).
	 (symmetric-algo 'cast5)
	 (s2k-algo 'iterated))
   (with-trace 4 "create-password-session-key-packet"
      (trace-item "Password session key packet")
      (trace-item "Symmetric algo: "
	     (symmetric-key-algo->human-readable symmetric-algo))
      (trace-item "Hash algo: " (hash-algo->human-readable hash-algo))
      (trace-item "s2k algo: " (s2k-algo->human-readable s2k-algo))
      (let* ((salt (if (eq? s2k-algo 'simple)
		       #f
		       (make-random-string (s2k-salt-length))))
	     ;; TODO: remove hardcoded constants.
	     (count (if (eq? s2k-algo 'iterated)
			(round-iterated-salted-s2k-count (+ 65000
							    (random 6500000)))
			#f))
	     (s2k (make-s2k s2k-algo hash-algo salt count)))
	 (trace-item "S2k salt: " (and salt (str->hex-string salt)))
	 (trace-item "S2k count: " count)
	 (when (and (not session-key) (eq? s2k-algo 'simple))
	    (error
	     'create-password-session-key-packet
	     "Without a session-key the S2k for a session-key must be salted."
	     (s2k-algo->human-readable s2k-algo)))
	 (if (not session-key)
	     (let ((len (symmetric-key-algo-key-byte-len data-symmetric-algo)))
		(trace-item "No Session key. "
		       "The s2k result will serve as session key.")
		(let ((generated-session-key (apply-s2k s2k password len)))
		   (trace-item "Generated session key: "
			  (str->hex-string generated-session-key))
		   (trace-item "Returning both the session key + the key packet.")
		   (values
		    generated-session-key
		    (instantiate::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
		       (version 4)
		       ;; data is encrypted using 
		       (algo data-symmetric-algo)
		       (s2k s2k)
		       (encrypted-session-key #f)))))
	     (let* ((key-len (symmetric-key-algo-key-byte-len symmetric-algo))
		    (block-len (symmetric-key-algo-block-byte-len symmetric-algo))		 
		    (encrypter (symmetric-key-algo->procedure symmetric-algo #t))
		    (decrypter (symmetric-key-algo->procedure symmetric-algo #f))
		    (key-string (apply-s2k s2k password key-len))
		    (data-symmetric-algo-string
		     (string (integer->char (symmetric-key-algo->byte
					     data-symmetric-algo))))
		    ;; Prepend the data-algo in front of the key.
		    (secret-data (string-append data-symmetric-algo-string
						session-key))
		    (encrypted (encrypter secret-data
					  (make-0-IV block-len)
					  key-string)))
		(trace-item "Secret data (algo + session-key)"
		       (str->hex-string secret-data))
		(trace-item "Key-string (result of s2k) used to encrypt the secret data: "
		       (str->hex-string key-string))
		(trace-item "Encrypted algo + session key: "
		       (str->hex-string encrypted))
		
		(instantiate::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
		   (version 4)
		   (algo symmetric-algo)
		   (s2k s2k)
		   (encrypted-session-key encrypted)))))))

(define (create-public-key-session-key-packet
	 public-subkey::PGP-Subkey
	 session-key::bstring
	 data-symmetric-algo) ;; Used to encrypt the payload.
   
   (define (encrypt-unpadded unpadded key algo)
      (case algo
	 ((rsa-encrypt/sign rsa-encrypt)
	  (trace-item "Rsa key")
	  (let* ((key-len (rsa-key-length key))
		 (padded (PKCS1-v1.5-pad unpadded key-len 2)))
	     (trace-item "key-len: " key-len)
	     (trace-item "padded: " (str->hex-string padded))
	     (values 'rsa-encrypt
		     (rsa-encrypt key (bin-str->bignum padded)))))
	 ((elgamal-encrypt elgamal-encrypt/sign)
	  (trace-item "ElGamal key")
	  (let* ((key-len (elgamal-key-length key))
		 (padded (PKCS1-v1.5-pad unpadded key-len 2)))
	     (trace-item "key-len: " key-len)
	     (trace-item "padded: " (str->hex-string padded))
	     (receive (r s)
		(elgamal-encrypt key (bin-str->bignum padded))
		(values 'elgamal-encrypt
			(cons r s)))))
	 (else
	  (error "create-public-key-session-key-packet"
		 "Bad public key algo"
		 (public-key-algo->human-readable algo)))))

   (with-trace 4 "create-public-key-sessions-key-packet"
      (with-access::PGP-Subkey public-subkey (key-packet)
	 (with-access::PGP-Key-Packet key-packet (algo key)
	    (let* ((chksum (chksum-16-bit session-key))
		   (char1 (integer->char (bit-rsh (bit-and chksum #xFF00) 8)))
		   (char2 (integer->char (bit-and chksum #xFF)))
		   (algo-byte (symmetric-key-algo->byte data-symmetric-algo))
		   (unpadded (string-append
			      (string (integer->char algo-byte))
			      session-key
			      (string char1 char2))))
	       (trace-item "chksum: " (format "~x" chksum))
	       (trace-item "unpadded: " (str->hex-string unpadded))
	       (receive (used-public-algo encrypted)
		  (encrypt-unpadded unpadded key algo)
		  (trace-item "encrypted: " encrypted)
		  (instantiate::PGP-Public-Key-Encrypted-Session-Key-Packet
		     (version 3)
		     (id (key-id key-packet))
		     (algo used-public-algo)
		     (encrypted-session-key encrypted))))))))
