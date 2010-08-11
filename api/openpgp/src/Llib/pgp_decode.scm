(module __openpgp-decode
   (library crypto)
   (import __openpgp-util
	   __openpgp-port-util
	   __openpgp-packets
	   __openpgp-human
	   __openpgp-s2k
	   __openpgp-conversion
	   __openpgp-enums)
   (export (decode-packets::pair-nil p::input-port)
	   (decode-s2k p::input-port)
	   (decode-mpi::bignum p::input-port))
   (eval (export-all)))

;; returns <length, partial-length?>
(define (decode-packet-length-v4 p)
   (let ((length-octet (safe-read-octet p)))
      ;; the length type is encoded in the two most significant bits of the
      ;; length octet. However, the spec uses integer-values (and not
      ;; bit-operations) and we therefore do the same.
      (cond
	 ((<fx length-octet 192) ;; one-octet length.
	  (values length-octet #f))
	 ((<fx length-octet 223) ;; two-octet length.
	  (let* ((fst (bit-lsh (-fx length-octet 192) 8))
		 (second (safe-read-octet p))
		 (len (+fx+ fst second 192)))
	     (values len #f)))
	 ((<fx length-octet 255) ;; partial body length. len-header is 1 byte.
	  (let ((len (bit-lsh 1 (bit-and length-octet #x1F))))
	     (values len #t)))
	 (else ;; 5-octet length.
	  (let ((len (decode-scalar p 4)))
	     (values len #f))))))

(define (decode-packet-length-v3 p packet-tag)
   (let ((length-type (bit-and packet-tag #x03)))
      (case length-type
	 ((0) ;; one octet length.
	  (values (safe-read-octet p) #f))
	 ((1) ;; two octet length.
	  (values (decode-scalar p 2) #f))
	 ((2) ;; four-octet length.
	  (values (decode-scalar p 4) #f))
	 ((3) ;; indeterminate length.
	  (values #f #t)))))

;; for v3: partial? is always #f. len might me #f
;; for v4: len is always given. if 'partial?' then the message is chunked.
(define (content-pipe-port p len partial?)
   (cond
      ((not len)
       ;; v3. In theory it should be clear when the content is finished.
       p)
      ((not partial?)
       (length-limited-pipe-port p len))
      (else
       (let ((pp (length-limited-pipe-port p len))
	     (partial? #t))
	  (open-input-procedure
	   (lambda ()
	      (let ((str (read-chars 256 pp)))
		 (cond
		    ((and partial? (eof-object? str))
		     (receive (len p?)
			(decode-packet-length-v4 p)
			(set! partial? p?)
			(set! pp (length-limited-pipe-port p len))
			(read-chars pp 256)))
		    ((eof-object? str) #f)
		    (else str)))))))))

(define (decode-packet p::input-port)
   (define (decode-packet-v3 packet-tag p)
      (debug "old format")
      (let ((content-tag-byte (bit-and #x0F (bit-rsh packet-tag 2))))
	 (when (zerofx? content-tag-byte)
	    (error "decode-content-tag" "invalid tag" 0))
	 (receive (len partial?)
	    (decode-packet-length-v3 p packet-tag)
	    (values (byte->content-tag content-tag-byte) len partial?))))

   (define (decode-packet-v4 packet-tag p)
      (debug "new format")
      (let ((content-tag-byte (bit-and packet-tag #x3F)))
	 (receive (len partial?)
	    (decode-packet-length-v4 p)
	    (values (byte->content-tag content-tag-byte) len partial?))))

   (let ((packet-tag (safe-read-octet p)))
      (when (zerofx? (bit-and #x80 packet-tag))
	 (error "decode-packet" "bad packet" packet-tag))

      (debug "Packet-tag: " packet-tag)
      (receive (content-tag len partial?)
	 (if (zerofx? (bit-and #x40 packet-tag))
	     ;; old packet format
	     (decode-packet-v3 packet-tag p)
	     (decode-packet-v4 packet-tag p))
	 (debug "Content-tag: " content-tag " "
		(content-tag->human-readable content-tag))
	 (debug "Len: " len)
	 (debug "Partial?: " partial?)
	 (let ((cpp (content-pipe-port p len partial?)))
	    (case content-tag
	       ((public-key-encrypted-session-key)
		(decode-public-key-encrypted-session-key cpp))
	       ((signature) (decode-signature cpp))
	       ((symmetric-key-encrypted-session-key)
		(decode-symmetric-key-encrypted-session-key cpp))
	       ((one-pass-signature)
		(decode-one-pass-signature cpp))
	       ((secret-key) (decode-secret-key cpp))
	       ((public-key) (decode-public-key cpp))
	       ((secret-subkey) (decode-secret-subkey cpp))
	       ((compressed) (decode-compressed-data cpp))
	       ((symmetrically-encrypted)
		(decode-symmetrically-encrypted-data cpp))
	       ((marker) (decode-marker cpp)) ;; should be ignored
	       ((literal) (decode-literal-data cpp))
	       ((trust) (decode-trust cpp))
	       ((ID) (decode-user-id cpp))
	       ((public-subkey) (decode-public-subkey cpp))
	       ((user-attribute) (decode-user-attribute cpp))
	       ((mdc-symmetrically-encrypted) (decode-mdc-sym-encrypted cpp))
	       ((mdc) (decode-mdc cpp))
	       (else
		(let* ((tmp (read-string p))
		       (len (string-length tmp)))
		   (debug "remaining length: " len)
		   (debug "str: " (str->hex-string (substring tmp 0 (min 10 len)))))
		(error 'decode-packet
		       "Unknown content-tag"
		       content-tag)))))))

(define (decode-packets p::input-port)
   (let ((c (peek-char p)))
      (if (eof-object? c)
	  '()
	  (let ((packet (decode-packet p)))
	     (cons packet
		   (decode-packets p))))))

(define (decode-s2k p::input-port)
   (let* ((s2k-algo-byte (safe-read-octet p))
	  (s2k-algo (byte->s2k-algo s2k-algo-byte))
	  (hash-algo-byte (safe-read-octet p))
	  (hash-algo (byte->hash-algo hash-algo-byte)))
      (debug "s2k-algo: " s2k-algo-byte " "
	     (s2k-algo->human-readable s2k-algo))
      (debug "hash-algo: " hash-algo-byte " "
	     (hash-algo->human-readable hash-algo))
      (case s2k-algo
	 ((simple)
	  (make-s2k s2k-algo hash-algo #f #f))
	 ((salted)
	  (let ((salt (safe-read-octets (s2k-salt-length) p)))
	     (debug "salt: " (str->hex-string salt))
	     (make-s2k s2k-algo hash-algo salt #f)))
	 ((iterated)
	  (let* ((salt (safe-read-octets (s2k-salt-length) p))
		 (encoded-count (safe-read-char p))
		 (count (octet->iterated-salted-s2k-count encoded-count)))
	     (debug "salt: " (str->hex-string salt))
	     (debug "count: " count " (" (char->integer encoded-count) ")")
	     (make-s2k s2k-algo hash-algo salt count)))
	 (else
	  (error "decode-s2k"
		 "unknown s2k algorithm"
		 s2k-algo)))))

(define (decode-time::date p::input-port #!optional (treat-0-as-present? #f))
   (let ((n (decode-scalar p 4)))
      (if (and treat-0-as-present? (zerofx? n))
	  (current-date)
	  (seconds->date n))))

(define (decode-mpi::bignum p::input-port)
   (let* ((nb-bits (decode-scalar p 2))
	  (nb-octets (/fx (+fx nb-bits 7) 8)))
      (debug "reading mpi of " nb-bits " bits (" nb-octets " octets)")
      (let loop ((i 0)
		 (n #z0))
	 (cond
	    ((=fx i nb-octets)
	     n)
	    (else (loop (+fx i 1)
			(+bx (*bx n #z256)
			     (fixnum->bignum (safe-read-octet p)))))))))

(define (decode-scalar::long p::input-port len::long)
   (let loop ((i 0)
	      (n 0))
      (cond
	 ((=fx i len)
	  n)
	 (else (loop (+fx i 1)
		     (+fx (*fx n 256)
			  (safe-read-octet p)))))))

;; ----------------------------------------------------------------------------
;; 5.1 Public-Key Encrypted Session Key Packets (Tag 1)
;; ----------------------------------------------------------------------------
(define (decode-public-key-encrypted-session-key cpp)
   (let* ((version (safe-read-octet cpp))
	  (id (safe-read-octets 8 cpp))
	  (algo-byte (safe-read-octet cpp))
	  (algo (byte->public-key-algo algo-byte))
	  (secret-data (read-string cpp))
	  (kp (open-input-string secret-data)))
      (when (not (or (=fx version 2)
		     (=fx version 3)))
	 (error "public key encrypted session key"
		"don't know how to decode a packet of this version"
		version))
      (debug "version: " version)
      (debug "id: " (str->hex-string id))
      (debug "algo: " algo " " (public-key-algo->human-readable algo))
      (debug "secret-data: " (str->hex-string secret-data))
      (case algo
	 ((rsa-encrypt/sign rsa-encrypt)
	  (let* ((m**e (decode-mpi kp)))
	     (debug "RSA-m**e: " m**e)
	     (instantiate::PGP-Public-Key-Encrypted-Session-Key-Packet
		(version version)
		(id id)
		(algo algo)
		(encrypted-session-key m**e))))
	 ((elgamal-encrypt elgamal-encrypt/sign)
	  (let* ((g**k (decode-mpi kp))
		 (dummy (debug "g**k done"))
		 (m*y**k (decode-mpi kp)))
	     (debug "ElGamal-g**k: " g**k)
	     (debug "ElGamal-m*y**k: " m*y**k)
	     (instantiate::PGP-Public-Key-Encrypted-Session-Key-Packet
		(version version)
		(id id)
		(algo algo)
		(encrypted-session-key (cons g**k m*y**k)))))
	 (else
	  (error "encrypted session key"
		 "Can't read encrypted-session key with this public algo."
		 (cons algo-byte (public-key-algo->human-readable algo)))))))

;; ----------------------------------------------------------------------------
;; 5.2 Signature Packet (Tag 2)
;; ----------------------------------------------------------------------------
(define (decode-signature p::input-port)
   (let ((version (safe-read-octet p)))
      (debug "Signature version: " version)
      (case version
	 ((3) (decode-signature-v3 p))
	 ((4) (decode-signature-v4 p version))
	 (else (error "decode-signature"
		      "Can't decode signatures of this version"
		      version)))))

;; ----------
;; 5.2.2 Version 3 Signature Packet Format
;; ----------------------------------------------------------------------------
(define (decode-signature-v3 p::input-port)
   (let* ((hashed-len (safe-read-octet p)) ;; must be 5
	  (hashed-str (safe-read-octets hashed-len p))
	  (hashed-str-p (open-input-string hashed-str))
	  (signature-type-byte (safe-read-octet hashed-str-p))
	  (signature-type (byte->signature-type signature-type-byte))
	  (creation-time (decode-time hashed-str-p))
	  (key-id (safe-read-octets 8 p))
	  (public-key-algo-byte (safe-read-octet p))
	  (public-key-algo (byte->public-key-algo public-key-algo-byte))
	  (hash-algo-byte (safe-read-octet p))
	  (hash-algo (byte->hash-algo hash-algo-byte))
	  (left-hash (safe-read-octets 2 p)))
      (when (not (=fx hashed-len 5))
	 (error "decode-signature-v3"
		"hashed len must be = 5"
		hashed-len))
      (debug "signature type: " signature-type-byte " "
	     (signature-type->human-readable signature-type))
      (debug "creation-time: " creation-time)
      (debug "key-id: " (str->hex-string key-id))
      (debug "public-key-algo: " public-key-algo-byte " "
	     (public-key-algo->human-readable public-key-algo))
      (debug "hash-algo: " hash-algo-byte " "
	     (hash-algo->human-readable hash-algo))
      (debug "hashed-str: " (str->hex-string hashed-str))
      (debug "left-hash: " (str->hex-string left-hash))
      (let ((key-data
	     (case public-key-algo
		((rsa-encrypt/sign rsa-sign)
		 (let ((m**d (decode-mpi p)))
		    (debug "RSA m**d: " m**d)
		    m**d))
		((dsa) ;; DSA
		 (let* ((r (decode-mpi p))
			(s (decode-mpi p)))
		    (debug "DSA r: " r)
		    (debug "DSA s: " s)
		    (cons r s)))
		(else
		 (error "decode-signature"
			"public key algorithm not implemented for signature"
			(public-key-algo->human-readable public-key-algo))))))
	 (instantiate::PGP-Signature-v3-Packet
	    (version 3)
	    (signature-type signature-type)
	    (creation-date creation-time)
	    (issuer key-id)
	    (public-key-algo public-key-algo)
	    (hash-algo hash-algo)
	    (signature key-data)
	    (signed-packet-prefix hashed-str)
	    (hash-trailer "")
	    (left-hash left-hash)))))

;; ----------
;; 5.2.3 Version 4 Signature Packet Format
;; ----------------------------------------------------------------------------

;; ------ 5.2.3.1 Signature Subpacket Specification
(define (decode-sub-packet p::input-port)
   (define (preferences->list str converter)
      (let loop ((i 0)
		 (rev-res '()))
	 (if (=fx i (string-length str))
	     (reverse! rev-res)
	     (loop (+fx i 1)
		   (cons (converter (char->integer (string-ref str i)))
			 rev-res)))))

   (receive (len partial?)
      (decode-packet-length-v4 p)
      (when partial? (error "decode-sub-package"
			    "sub-packages must not have partial lengths"
			    #f))
      ;(debug "sub-packet of length: " len)
      (let* ((type-w/-critical (safe-read-octet p))
	     (type-byte (bit-and #x7F type-w/-critical))
	     (type (byte->subpacket-type type-byte))
	     (critical? (not (zerofx? (bit-and #x80 type-w/-critical)))))
	 (debug "sub-packet of type: " type-byte
		" " (subpacket-type->human-readable type))
	 (debug "sub-packet is critical?: " critical?)

	 ;; currently the following
	 (case type
	  ((creation-time)
	   (let ((t (decode-time p)))
	      (debug "Date: " t)
	      (make-PGP-Signature-Sub-Creation-Time critical? t)))
	  ((expiration-time)
	   (let ((t (decode-time p)))
	      (debug "Date: " t)
	      (make-PGP-Signature-Sub-Expiration-Time critical? t)))
	  ((exportable?)
	   (let ((exportable? (=fx 1 (safe-read-octet p))))
	      (debug "Exportable?: " exportable?)
	      (make-PGP-Signature-Sub-Exportable critical? exportable?)))
	  ((trust)
	   (let* ((level (safe-read-octet p))
		  (trust (safe-read-octet p)))
	      (debug "Level/Trust: " level "/" trust)
	      (make-PGP-Signature-Sub-Trust critical? level trust)))
	  ;; TODO ((6) 'regular-expression)
	  ((revocable?)
	   (let ((revocable? (=fx 1 (safe-read-octet p))))
	      (make-PGP-Signature-Sub-Revocable critical? revocable?)))
	  ((key-expiration-time)
	   (let ((t (decode-scalar p 4)))
	      (debug "expiration-time: " t)
	      (make-PGP-Signature-Sub-Key-Expiration-Time critical? t)))
	  ((placeholder) ;; placeholder for backward compatibility
	   (let ((tmp (safe-read-octets (-fx len 1) p)))
	      (debug "generic: " (str->hex-string tmp))
	      (make-PGP-Signature-Sub-Generic critical? type tmp)))
	  ((preferred-symmetric)
	   (let ((prefs (preferences->list
			 (safe-read-octets (-fx len 1) p)
			 byte->symmetric-key-algo)))
	      (debug "preferred symmetrics: "
		     (map symmetric-key-algo->human-readable prefs))
	      (make-PGP-Signature-Sub-Preferred-Symmetric critical? prefs)))
	  ((revocation-key)
	   (let* ((class (safe-read-octet p))
		  (sensitive? (not (zerofx? (bit-and #x40 class))))
		  (algid (safe-read-octet p))
		  (fingerprint (safe-read-octets 20 p)))
	      (debug "class: " class)
	      (debug "sensitive?: " sensitive?)
	      (debug "algid: " algid)
	      (debug "finger-print: " (str->hex-string fingerprint))
	      (when (zerofx? (bit-and #x80 class))
		 (error
		  "decode-signature-sub-packet"
		  "revocation-key signature must have bit 0x80 set"
		  (format "0x~x" class)))
	      (make-PGP-Signature-Sub-Revocation
	       critical? class sensitive? algid fingerprint)))
	  ((issuer-ID)
	   (let ((id (safe-read-octets 8 p)))
	      (debug "Id: " (str->hex-string id))
	      (make-PGP-Signature-Sub-ID critical? id)))
	  ((notation)
	   (let* ((flags (safe-read-octets 4 p))
		  (name-len (decode-scalar p 2))
		  (value-len (decode-scalar p 2))
		  (name-data (safe-read-octets name-len p))
		  (value-data (safe-read-octets value-len p)))
	      (debug "flags: " flags)
	      (debug "name: " name-data)
	      (debug "value: " value-data)
	      (make-PGP-Signature-Sub-Notation
	       critical? flags name-data value-data)))
	  ((preferred-hash)
	   (let ((prefs (preferences->list
			 (safe-read-octets (-fx len 1) p)
			 byte->hash-algo)))
	      (debug "Preferred Hash-algo: "
		     (map hash-algo->human-readable prefs))
	      (make-PGP-Signature-Sub-Preferred-Hash critical? prefs)))
	  ((preferred-compression)
	   (let ((prefs (preferences->list
			 (safe-read-octets (-fx len 1) p)
			 byte->compression-algo)))
	      (debug "Preferred Compression algo: "
		     (map compression-algo->human-readable prefs))
	      (make-PGP-Signature-Sub-Preferred-Compression critical?
							    prefs)))
	  ; TODO ((key-server-prefs) ;; preferred key server preferences
	  ((preferred-key-server)
	   (let ((key-server (safe-read-octets (-fx len 1) p)))
	      (debug "Preferred Key-server:" key-server)
	      (make-PGP-Signature-Sub-Preferred-Key-Server critical?
							   key-server)))
	  ((primary-id?)
	   (let ((prim-id? (not (=fx 0 (safe-read-octet p)))))
	      (debug "Primary Id?: " prim-id?)
	      (make-PGP-Signature-Sub-Primary-ID critical? prim-id?)))
	  ((policy)
	   (let ((policy (safe-read-octets (-fx len 1) p)))
	      (debug "Policy: " policy)
	      (make-PGP-Signature-Sub-Policy critical? policy)))
	  ; TODO ((key-flags) ;; key flags
	  ((signer-ID)
	   (let ((id (safe-read-octets (-fx len 1) p)))
	      (debug "Signer id: " id)
	      (make-PGP-Signature-Sub-Signer-ID critical? id)))
	  ((revocation-reason)
	   (let* ((code-byte (safe-read-octet p))
		  (code (byte->revocation-code code-byte))
		  (reason (safe-read-octets (-fx len 2) p)))
	      (debug "Revocation code: " code-byte " "
		     (revocation-code->human-readable code))
	      (debug "Reason: " reason)
	      (make-PGP-Signature-Sub-Revocation-Reason
	       critical? code reason)))
	  (else
	   (debug "Generic")
	   (make-PGP-Signature-Sub-Generic
	    critical? type (safe-read-octets (-fx len 1) p)))))))

(define (decode-sub-packets p::input-port)
   (let ((c (peek-char p)))
      (if (eof-object? c)
	  '()
	  (let ((sub-packet (decode-sub-packet p)))
	     (cons sub-packet (decode-sub-packets p))))))

;; -------- 5.2.3
(define (decode-signature-v4 p::input-port version)
   (define (find-creation-date sps)
      (let ((pkt (any (lambda (pkt)
			 (and (PGP-Signature-Sub-Creation-Time? pkt)
			      pkt))
		      sps)))
	 
	 (when (not pkt)
	    (error
	     "decode-signature-v4"
	     "invalid signature. Can't find obligatory creation-time sub-packet"
	     #f))
	 (PGP-Signature-Sub-Creation-Time-creation-date pkt)))
   (define (find-issuer sps)
      (let ((pkt (any (lambda (pkt)
			 (and (PGP-Signature-Sub-ID? pkt)
			      pkt))
		      sps)))
	 (when (not pkt)
	    (error
	     "decode-signature-v4"
	     "invalid signature. Can't find obligatory issuer id sub-packet"
	     #f))
	 (PGP-Signature-Sub-ID-key-id pkt)))

   (let* ((signature-type-byte (safe-read-octet p))
	  (signature-type (byte->signature-type signature-type-byte))
	  (public-key-algo-byte (safe-read-octet p))
	  (public-key-algo (byte->public-key-algo public-key-algo-byte))
	  (hash-algo-byte (safe-read-octet p))
	  (hash-algo (byte->hash-algo hash-algo-byte))
	  ;; spd = sub-packet-data
	  (hashed-spd-len-str (safe-read-octets 2 p))
	  (hashed-spd-len (scalar->fixnum hashed-spd-len-str))
	  (hashed-spd (safe-read-octets hashed-spd-len p))
	  (dumm1 (debug "*- decoding hashed subpackets"))
	  (sps1 (decode-sub-packets (open-input-string hashed-spd)))
	  (creation-date (find-creation-date sps1))
	  (unhashed-spd-len (decode-scalar p 2))
	  (dumm1 (debug "*- decoding unhashed subpackets"))
	  (sps2 (decode-sub-packets
		  (length-limited-pipe-port p unhashed-spd-len)))
	  (sps1+sps2 (append sps2 sps1))
	  (issuer (find-issuer sps1+sps2))
	  (left-hash (safe-read-octets 2 p)))
      (debug "signature type: " signature-type-byte " "
	     (signature-type->human-readable signature-type))
      (debug "public-key-algo: " public-key-algo-byte " "
	     (public-key-algo->human-readable public-key-algo))
      (debug "hash-algo: " hash-algo-byte " "
	     (hash-algo->human-readable hash-algo))
      (debug "hashed-subpacket-data: " (str->hex-string hashed-spd))
;       (debug "sps1:")
;       (for-each (lambda (sp)
; 		   (with-access::PGP-Signature-Sub-Packet sp (type)
; 		      (debug "  " (subpacket-type->human-readable type)
; 			     " " sp)))
; 		sps1)
;       (debug "sps2:")
;       (for-each (lambda (sp)
; 		   (with-access::PGP-Signature-Sub-Packet sp (type)
; 		      (debug "  " (subpacket-type->human-readable type)
; 			     " " sp)))
; 		sps2)
      (debug "left-hash: " (str->hex-string left-hash))
      (let* ((signed-packet-prefix-len (+fx 6 hashed-spd-len))
	     (signed-packet-prefix (make-string signed-packet-prefix-len))
	     (hash-trailer (make-string 6)))
	 (string-set! signed-packet-prefix 0 (integer->char-ur version))
	 (string-set! signed-packet-prefix 1
		      (integer->char-ur signature-type-byte))
	 (string-set! signed-packet-prefix 2
		      (integer->char-ur public-key-algo-byte))
	 (string-set! signed-packet-prefix 3 (integer->char-ur hash-algo-byte))
	 (blit-string! hashed-spd-len-str 0 signed-packet-prefix 4 2)
	 (blit-string! hashed-spd 0 signed-packet-prefix 6 hashed-spd-len)

	 ;; trailer magic... (see 5.2.4, page 31 of RFC2440)
	 (string-set! hash-trailer 0 (integer->char-ur version))
	 (string-set! hash-trailer 1 (integer->char-ur #xFF))
	 (blit-string! (fixnum->scalar signed-packet-prefix-len 4) 0
		       hash-trailer 2
		       4)

	 (debug "signed-packet-prefix: " (str->hex-string signed-packet-prefix))
	 (debug "hash-trailer: " (str->hex-string hash-trailer))
	 (let ((key-data
		(case public-key-algo
		   ((rsa-encrypt/sign rsa-sign)
		    (let ((m**d (decode-mpi p)))
		       (debug "RSA m**d: " m**d)
		       m**d))
		   ((dsa)
		    (let* ((r (decode-mpi p))
			   (s (decode-mpi p)))
		       (debug "DSA r: " r)
		       (debug "DSA s: " s)
		       (cons r s)))
		   (else
		    (error "decode-signature"
			   "public key algorithm not implemented"
			   (list public-key-algo-byte
				 (public-key-algo->human-readable
				  public-key-algo)))))))
	    (instantiate::PGP-Signature-v4-Packet
	       (version 4)
	       (signature-type signature-type)
	       (creation-date creation-date)
	       (issuer issuer)
	       (public-key-algo public-key-algo)
	       (hash-algo hash-algo)
	       (signature key-data)
	       (signed-packet-prefix signed-packet-prefix)
	       (hash-trailer hash-trailer)
	       (left-hash left-hash)
	       (secure-sub-packets sps1)
	       (insecure-sub-packets sps2))))))

;; ----------------------------------------------------------------------------
;; 5.3 Symmetric-Key Encrypted Session-Key Packets (Tag 3)
;; ----------------------------------------------------------------------------
(define (decode-symmetric-key-encrypted-session-key cpp)
   (let* ((version (safe-read-octet cpp))
	  (symmetric-algo-byte (safe-read-octet cpp))
	  (symmetric-algo (byte->symmetric-key-algo symmetric-algo-byte))
	  (s2k (decode-s2k cpp))
	  (encrypted-session-key (read-string cpp)))
      (instantiate::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
	 (version version)
	 (algo symmetric-algo)
	 (s2k s2k)
	 (encrypted-session-key (if (string-null? encrypted-session-key)
				    #f
				    encrypted-session-key)))))

;; ----------------------------------------------------------------------------
;; 5.4 One-Pass Signature Packets (Tag 4)
;; ----------------------------------------------------------------------------
(define (decode-one-pass-signature p::input-port)
   (let* ((version (safe-read-octet p))
	  (signature-type-byte (safe-read-octet p))
	  (signature-type (byte->signature-type signature-type-byte))
	  (hash-algo-byte (safe-read-octet p))
	  (hash-algo (byte->hash-algo hash-algo-byte))
	  (public-key-algo-byte (safe-read-octet p))
	  (public-key-algo (byte->public-key-algo public-key-algo-byte))
	  (id (safe-read-octets 8 p))
	  (nested-signature? (zerofx? (safe-read-octet p))))
      (debug "signature type: " signature-type-byte " "
	     (signature-type->human-readable signature-type))
      (debug "public-key-algo: " public-key-algo-byte " "
	     (public-key-algo->human-readable public-key-algo))
      (debug "hash-algo: " hash-algo-byte " "
	     (hash-algo->human-readable hash-algo))
      (debug "id: " (str->hex-string id))
      (debug "nested-sig?: " nested-signature?)
      (instantiate::PGP-One-Pass-Signature-Packet
	 (version version)
	 (signature-type signature-type)
	 (issuer id)
	 (public-key-algo public-key-algo)
	 (hash-algo hash-algo)
	 (contains-nested-sig? nested-signature?))))

;; ----------------------------------------------------------------------------
;; 5.5 Key Material Packet
;; ----------------------------------------------------------------------------

;; ----------
;; 5.5.1.1 Public Key Packet
;; ----------------------------------------------------------------------------
(define make-public-rsa-key make-Rsa-Key)
(define make-public-dsa-key make-Dsa-Key)
(define make-public-elgamal-key make-ElGamal-Key)

(define *dummy-date* (current-date))

(define (decode-public-key p::input-port)
   (let* ((version (safe-read-octet p))
	  (kp (instantiate::PGP-Public-Key-Packet
		 (version version)
		 (algo 'not-yet-set)
		 (creation-date *dummy-date*) ;; typing.
		 (valid-days #f)
		 (key #f))))
      (case version
	 ((2 3 4) 'ok)
	 (else (error "decode-public-key"
		      "version of public key file not supported"
		      version)))
      (decode/fill-key kp version p)
      kp))

(define (decode/fill-key kp::PGP-Key-Packet version p::input-port)
   (let ((creation-date (decode-time p)))
      (debug "creation-date: " creation-date)
      (PGP-Key-Packet-creation-date-set! kp creation-date))
   (when (or (=fx version 2) (=fx version 3))
      (let ((valid-days (decode-scalar p 2))) ;; if 0 indefinite
	 (debug "valid days: " valid-days)
	 (PGP-Key-Packet-valid-days-set! kp valid-days)))
   (let* ((algo-byte (safe-read-octet p))
	  (algo (byte->public-key-algo algo-byte)))
      (when (and (or (=fx version 2) (=fx version 3))
		 (not (or (eq? algo 'rsa-encrypt/sign)
			  (eq? algo 'rsa-encrypt)
			  (eq? algo 'rsa-sign))))
	 (error "decode key v3"
		"only RSA is supported for version 3 keys"
		(public-key-algo->human-readable algo)))
      (debug "algo: " algo " " (public-key-algo->human-readable algo))
      (PGP-Key-Packet-algo-set! kp algo)
      (case algo
	 ((rsa-encrypt/sign rsa-encrypt rsa-sign)
	  (let* ((modulus (decode-mpi p))
		 (exponent (decode-mpi p))
		 (key (make-public-rsa-key modulus exponent)))
	     (debug "modulus: " modulus)
	     (debug "exponent: " exponent)
	     (PGP-Key-Packet-key-set! kp key)))
	 ((dsa) ;; DSA
	  (let* ((port p)
		 (p (decode-mpi port))
		 (q (decode-mpi port))
		 (g (decode-mpi port))
		 (y (decode-mpi port))
		 (key (make-public-dsa-key p q g y)))
	     (debug "p: " p)
	     (debug "q: " q)
	     (debug "g: " g)
	     (debug "y: " y)
	     (PGP-Key-Packet-key-set! kp key)))
	 ((elgamal-encrypt elgamal-encrypt/sign)
	  (let* ((port p)
		 (p (decode-mpi port))
		 (g (decode-mpi port))
		 (y (decode-mpi port))
		 (key (make-public-elgamal-key p g y)))
	     (debug "p: " p)
	     (debug "g: " g)
	     (debug "y: " y)
	     (PGP-Key-Packet-key-set! kp key)))
	 (else (error "decode-key"
		      "public key algorithm must be RSA, DSA or ElGamal"
		      (public-key-algo->human-readable algo))))))

;; ----------
;; 5.5.1.2 Public Subkey Packet
;; ----------------------------------------------------------------------------
(define (decode-public-subkey cpp)
   (let ((p (decode-public-key cpp)))
      (PGP-Public-Key-Packet-subkey?-set! p #t)
      p))


;; ----------
;; 5.5.1.3 Secret Key Packet
;; ----------------------------------------------------------------------------
(define (decode-secret-key cpp::input-port)
   (let* ((version (safe-read-octet cpp))
	  (kp (instantiate::PGP-Secret-Key-Packet
		 (version version)
		 (algo 'not-yet-set)
		 (creation-date *dummy-date*)
		 (valid-days #f)
		 (key #f)
		 (password-protected-secret-key-data ""))))
      (case version
	 ((3 4) (decode/fill-key kp version cpp))
	 (else (error "decode-secret-key"
		      "version incompatible with this implementation"
		      version)))
      (let ((secret-data (read-string cpp)))
	 (debug "secret data: " (str->hex-string secret-data))
	 (with-access::PGP-Secret-Key-Packet kp
	       (password-protected-secret-key-data)
	    (set! password-protected-secret-key-data secret-data))
	 kp)))

;; ----------
;; 5.5.1.4 Secret Subkey Packet
;; ----------------------------------------------------------------------------
(define (decode-secret-subkey cpp)
   (let ((p (decode-secret-key cpp)))
      (PGP-Secret-Key-Packet-subkey?-set! p #t)
      p))


;; ----------------------------------------------------------------------------
;; 5.6 Compressed Data Packet
;; ----------------------------------------------------------------------------
(define (decode-compressed-data cpp)
   ;; note: we discard the compression-packet and return the uncompressed
   ;; packet. Maybe this is not intended in which case we could insert a new
   ;; 'Compression-Packet'.
   (let* ((algo-byte (safe-read-octet  cpp))
	  (algo (byte->compression-algo algo-byte)))
      (debug "Compression Algorithm: " algo " "
	     (compression-algo->human-readable algo))
      (case algo
       ((uncompressed) (decode-packet cpp))
       ((ZIP)
	(instantiate::PGP-Compressed-Packet
	   (packets (decode-packets (port->inflate-port cpp)))))
       ;; TODO: fix compression
       ;((ZLIB) (decode-packet (port->gzip-port cpp)))
       (else (error "decode compressed"
		    "Can't decompresse data"
		    algo)))))

;; ----------------------------------------------------------------------------
;; 5.7 Symmetrically Encrypted Data Packet (Tag 9)
;; ----------------------------------------------------------------------------
(define (decode-symmetrically-encrypted-data cpp)
   (instantiate::PGP-Symmetrically-Encrypted-Packet
      (data (read-string cpp))))

;; ----------------------------------------------------------------------------
;; 5.8 Marker Packet (Obsolete Literal Packet) (Tag 10)
;; ----------------------------------------------------------------------------
(define (decode-marker cpp)
   (let* ((p (safe-read-octet cpp))
	  (g (safe-read-octet cpp))
	  (p2 (safe-read-octet cpp)))
      (unless (and (=fx p #x50)
		   (=fx g #x47)
		   (=fx p2 #x50)
		   (eof-object? (peek-char cpp)))
	 (error "decode-marker"
		"bad marker packet"
		#f))
      (instantiate::PGP-Marker-Packet)))

;; ----------------------------------------------------------------------------
;; 5.9 Literal Data Packet (Tag 11)
;; ----------------------------------------------------------------------------
(define (decode-literal-data p::input-port)
   (let* ((format-byte (safe-read-octet p))
	  (format (byte->literal-format format-byte))
	  (file-len (safe-read-octet p))
	  (file-name (safe-read-octets file-len p))
	  (sensitive? (string=? file-name "_CONSOLE"))
	  (creation-date (decode-time p #t))
	  (data (read-string p)))
      (debug "format: " format-byte " "
	     (literal-format->human-readable format))
      (debug "file: " file-name)
      (debug "creation-date: " creation-date)
      (debug "data: \n-------\n" data "\n---------\n")
      (instantiate::PGP-Literal-Packet
	 (format format)
	 (for-your-eyes-only? sensitive?)
	 (file-name (and (not sensitive?) file-name))
	 (creation-date creation-date)
	 (data data))))

;; ----------------------------------------------------------------------------
;; 5.10 Trust Packet (Tag 12)
;; ----------------------------------------------------------------------------
(define (decode-trust cpp)
   (read-string cpp)
   (warning "ignoring trust packet")
   (instantiate::PGP-Trust-Packet))

;; ----------------------------------------------------------------------------
;; 5.11 User ID Packet (Tag 13)
;; ----------------------------------------------------------------------------
(define (decode-user-id cpp)
   (let ((user-id (read-string cpp)))
      (debug "User ID: " user-id)
      (instantiate::PGP-ID-Packet
	 (data user-id))))

;; ----------------------------------------------------------------------------
;; 5.12 User ID Packet (Tag 17)
;; ----------------------------------------------------------------------------
(define (decode-user-attribute cpp)
   ;; TODO
   (read-string cpp)
   )

;; ----------------------------------------------------------------------------
;; 5.13 Sym. Encrypted Integrity Protected Data Packet (Tag 18)
;; ----------------------------------------------------------------------------
(define (decode-mdc-sym-encrypted cpp)
   (let ((version (safe-read-octet cpp)))
      (debug "version: " version)
      (instantiate::PGP-MDC-Symmetrically-Encrypted-Packet
	 (version version)
	 (data (read-string cpp)))))

;; ----------------------------------------------------------------------------
;; 5.14 Modification Detection Code Packet (Tag 19)
;; ----------------------------------------------------------------------------
(define (decode-mdc cpp)
   (let ((str (read-string cpp)))
      (when (not (=fx (string-length str) 20))
	 (error 'decode-mdc
		"Bad Modification Detection Code Packet"
		#f))
      (debug "sha1 mdc: " (str->hex-string str))
      (instantiate::PGP-MDC-Packet
	 (hash str))))
