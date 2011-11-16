(module __openpgp-encode
   (library crypto)
   (import __openpgp-util
	   __openpgp-conversion
	   __openpgp-packets
	   __openpgp-human
	   __openpgp-enums
	   __openpgp-s2k)
   (export (create-signed-packet-prefix-v4 signature-type::symbol
					public-key-algo::symbol
					hash-algo::symbol
					creation-date
					signed-sub-packets::pair-nil)
	   (encode-packet packet::PGP-Packet p::output-port)
	   (encode-public-key-content packet::PGP-Key-Packet p::output-port)
	   (encode-packets p::output-port . packets)))

(define (encode-packets p::output-port . packets)
   (for-each (lambda (packet) (encode-packet packet p)) packets))

(define (encode-octet n::long p::output-port)
   (when (>=fx n 256)
      (error 'encode-octet
	     "given number is too big"
	     n))
   (display (integer->char-ur n) p))
(define (encode-octets str::bstring len::long p::output-port)
   (when (not (=fx (string-length str) len))
      (error 'encode-octets
	     "given string has bad size"
	     (cons len str)))
   (display str p))

(define (encode-scalar nn::long len::long p::output-port)
   ;; TODO: make this more efficient.
   (display (fixnum->scalar nn len) p))

(define (encode-mpi b::bignum p::output-port)
   ;; TODO: make this more efficient
   (let* ((bit-len (bignum-bit-length b))
	  (octet-len (/ceilingfx bit-len 8))
	  (str (bignum->bin-str b octet-len)))
      (encode-scalar bit-len 2 p)
      (display str p)))

(define (encode-length-v4 len::long p::output-port)
   (cond
      ((<fx len 192)
       ;; one-octet len encodes up to 191.
       (encode-octet len p))
      ((<=fx len 8383)
       (let ((t (-fx len 192)))
	  (encode-octet (+fx 192 (bit-rsh t 8)) p)
	  (encode-octet (bit-and t #xFF) p)))
      (else ;; 5 octet len.
       (encode-octet #xFF p)
       (encode-scalar len 4 p))))

(define (encode-time d::date p::output-port)
   (encode-scalar (date->seconds d) 4 p))
   
(define (encode-prefs prefs::pair-nil converter::procedure p::output-port)
   (for-each (lambda (pref)
		(when (not (fixnum? pref))
		   (error 'encode-pref
			  "Preferences must be integers"
			  pref))
		(encode-octet (converter pref) p))
	     prefs))
(define-generic (packet->content-tag p::PGP-Packet)
   (error 'packet->content-tag
	  "internal error. forgot PGP-Packet type"
	  (class-name (object-class p))))
(define-method (packet->content-tag
		p::PGP-Public-Key-Encrypted-Session-Key-Packet)
   'public-key-encrypted-session-key)
(define-method (packet->content-tag p::PGP-Signature-Packet)
   'signature)
(define-method (packet->content-tag
		p::PGP-Symmetric-Key-Encrypted-Session-Key-Packet)
   'symmetric-key-encrypted-session-key)
(define-method (packet->content-tag p::PGP-One-Pass-Signature-Packet)
   'one-pass-signature)
(define-method (packet->content-tag p::PGP-Secret-Key-Packet)
   (with-access::PGP-Secret-Key-Packet p (subkey?)
      (if subkey?
	  'secret-key
	  'secret-subkey)))
(define-method (packet->content-tag p::PGP-Public-Key-Packet)
   (with-access::PGP-Key-Packet p (subkey?)
      (if subkey?
	  'public-key
	  'public-subkey)))
(define-method (packet->content-tag p::PGP-Compressed-Packet)
   'compressed)
(define-method (packet->content-tag p::PGP-Symmetrically-Encrypted-Packet)
   'symmetrically-encrypted)
(define-method (packet->content-tag p::PGP-MDC-Symmetrically-Encrypted-Packet)
   'mdc-symmetrically-encrypted)
(define-method (packet->content-tag p::PGP-Marker-Packet)
   'marker)
(define-method (packet->content-tag p::PGP-Literal-Packet)
   'literal)
(define-method (packet->content-tag p::PGP-Trust-Packet)
   'trust)
(define-method (packet->content-tag p::PGP-ID-Packet)
   'ID)


(define (encode-packet packet::PGP-Packet p::output-port)
   (let* ((content-tag (packet->content-tag packet))
	  (content-tag-byte (content-tag->byte content-tag))
	  (dummy (debug "Content-tag: " content-tag-byte " "
			(content-tag->human-readable content-tag)))
	  (str-p (open-output-string))
	  (content (encode-content packet str-p))
	  (str (close-output-port str-p))
	  (len (string-length str)))
      (when (>fx content-tag-byte #x1F)
	 (error "encode-packet" "content-tag too big" content-tag))
      ;; we use new-packet-type.
      (debug (format "content-tag-byte: ~x" content-tag-byte))
      (debug (format "encoded-octet: ~x" (+fx+ #x80 #x40 content-tag-byte)))
      (encode-octet (+fx+ #x80 #x40 content-tag-byte) p)
      (debug "length: " len)
      (encode-length-v4 len p)
      (display str p)))
	 
(define-generic (encode-content packet::PGP-Packet p::output-port)
   (with-trace 3 "encode-content (generic)"
      (error 'encode-content
	     "not yet implemented"
	     (class-name (object-class packet)))))

(define-method (encode-content
		packet::PGP-Public-Key-Encrypted-Session-Key-Packet
		p::output-port)
   (with-trace 3 "encode-content ::PGP-Public-Key-Encrypted-Session-Key-Packet"
      (with-access::PGP-Public-Key-Encrypted-Session-Key-Packet packet
	    (version id algo encrypted-session-key)
	 (encode-octet version p)
	 (encode-octets id 8 p)
	 (encode-octet (public-key-algo->byte algo) p)
	 (case algo
	    ((rsa-encrypt/sign rsa-encrypt)
	     (encode-mpi encrypted-session-key p))
	    ((elgamal-encrypt elgamal-encrypt/sign)
	     (when (not (pair? encrypted-session-key))
		(error 'encode-Public-key-encrypted-session-key-packet
		       "bad encrypted-session-key. ElGamal requires a pair"
		       encrypted-session-key))
	     (encode-mpi (car encrypted-session-key) p)
	     (encode-mpi (cdr encrypted-session-key) p))
	    (else
	     (error 'encode-Public-key-encrypted-session-key-packet
		    "Not yet implemented"
		    (public-key-algo->human-readable algo)))))))

;; we do not yet create Signature-v3 packets.

(define (encode-sub-packet sp::PGP-Signature-Sub-Packet
			   p::output-port)
   (let ((str-p (open-output-string)))
      (encode-sub-packet-content sp str-p)
      (let ((str (close-output-port str-p)))
	 (encode-length-v4 (string-length str) p)
	 (display str p))))

(define-generic (encode-sub-packet-content sp::PGP-Signature-Sub-Packet
					   p::output-port)
   (error 'encode-signature-sub-packet-content
	  "not yet implemented"
	  (class-name (object-class sp))))

(define (encode-sub-packet-header type::symbol critical?::bool p::output-port)
   (if critical?
       (encode-octet (+fx (subpacket-type->byte type) #x80) p)
       (encode-octet (subpacket-type->byte type) p)))

(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Generic
					  p::output-port)
   (with-access::PGP-Signature-Sub-Generic sp (critical? type data)
      (encode-sub-packet-header type critical? p)
      (display data p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Creation-Time
					  p::output-port)
   (with-access::PGP-Signature-Sub-Creation-Time sp (critical? creation-date)
      (encode-sub-packet-header 'creation-time critical? p)
      (encode-time creation-date p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Expiration-Time
					  p::output-port)
   (with-access::PGP-Signature-Sub-Expiration-Time sp (critical?
						       expiration-date)
      (encode-sub-packet-header 'expiration-time critical? p)
      (encode-time expiration-date p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Exportable
					  p::output-port)
   (with-access::PGP-Signature-Sub-Exportable sp (critical? exportable?)
      (encode-sub-packet-header 'exportable? critical? p)
      (encode-octet (if exportable? 1 0) p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Trust
					  p::output-port)
   (with-access::PGP-Signature-Sub-Trust sp (critical? level amount)
      (encode-sub-packet-header 'trust critical? p)
      (encode-octet level p)
      (encode-octet amount p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Revocable
					  p::output-port)
   (with-access::PGP-Signature-Sub-Revocable sp (critical? revocable?)
      (encode-sub-packet-header 'revocable? critical? p)
      (encode-octet (if revocable? 1 0) p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Key-Expiration-Time
					  p::output-port)
   (with-access::PGP-Signature-Sub-Key-Expiration-Time sp (critical?
							   expiration-time)
      (encode-sub-packet-header 'key-expiration-time critical? p)
      (encode-scalar expiration-time 4 p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Preferred-Symmetric
					  p::output-port)
   (with-access::PGP-Signature-Sub-Preferred-Symmetric sp (critical? algos)
      (encode-sub-packet-header 'preferred-symmetric critical? p)
      (encode-prefs algos symmetric-key-algo->byte p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Revocation
					  p::output-port)
   (with-access::PGP-Signature-Sub-Revocation sp (critical? clazz
							    algid fingerprint)
      (encode-sub-packet-header 'revocation-key critical? p)
      (encode-octet clazz p)
      (encode-octet algid p)
      (encode-octets fingerprint 20 p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-ID
					  p::output-port)
   (with-access::PGP-Signature-Sub-ID sp (critical? key-id)
      (encode-sub-packet-header 'issuer-ID critical? p)
      (encode-octets key-id 8 p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Notation
					  p::output-port)
   (with-access::PGP-Signature-Sub-Notation sp (critical? flags name value)
      (encode-sub-packet-header 'notation critical? p)
      (encode-octets flags 4 p)
      (encode-scalar (string-length name) 2 p)
      (encode-scalar (string-length value) 2 p)
      (display name p)
      (display value p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Preferred-Hash
					  p::output-port)
   (with-access::PGP-Signature-Sub-Preferred-Hash sp (critical? algos)
      (encode-sub-packet-header 'preferred-hash critical? p)
      (encode-prefs algos hash-algo->byte p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Preferred-Compression
					  p::output-port)
   (with-access::PGP-Signature-Sub-Preferred-Compression sp (critical? algos)
      (encode-sub-packet-header 'preferred-compression critical? p)
      (encode-prefs algos compression-algo->byte p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Preferred-Key-Server
					  p::output-port)
   (with-access::PGP-Signature-Sub-Preferred-Key-Server sp (critical? server)
      (encode-sub-packet-header 'preferred-key-server critical? p)
      (display server p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Primary-ID
					  p::output-port)
   (with-access::PGP-Signature-Sub-Primary-ID sp (critical? primary?)
      (encode-sub-packet-header 'primary-id? critical? p)
      (encode-octet (if primary? 1 0) p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Policy
					  p::output-port)
   (with-access::PGP-Signature-Sub-Policy sp (critical? url)
      (encode-sub-packet-header 'policy critical? p)
      (display url p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Signer-ID
					  p::output-port)
   (with-access::PGP-Signature-Sub-Signer-ID sp (critical? id)
      (encode-sub-packet-header 'signer-ID critical? p)
      (display id p)))
(define-method (encode-sub-packet-content sp::PGP-Signature-Sub-Revocation-Reason
					  p::output-port)
   (with-access::PGP-Signature-Sub-Revocation-Reason sp (critical? code reason)
      (encode-sub-packet-header 'revocation-reason critical? p)
      (encode-octet (revocation-code->byte code) p)
      (display reason p)))
   
(define (encode-sub-packets packets p)
   (let ((str-p (open-output-string)))
      (for-each (lambda (spacket)
		   (encode-sub-packet spacket str-p))
		packets)
      (let ((str (close-output-port str-p)))
	 (encode-scalar (string-length str) 2 p)
	 (display str p))))

(define (date=? d1 d2)
   (and (date? d1)
	(date? d2)
	(=fx (date->seconds d1) (date->seconds d2))))

;; creation-date might be #f in which case it must be present inside the
;; signed-sub-packets.
(define (create-signed-packet-prefix-v4 signature-type::symbol
					public-key-algo::symbol
					hash-algo::symbol
					creation-date
					signed-sub-packets::pair-nil)
   (with-trace 4 "create-signed-packet-prefix-v4"
      (let ((str-p (open-output-string))
	    (version 4)
	    (creation-time-packet
	     (any (lambda (p)
		     (and (isa? p PGP-Signature-Sub-Creation-Time) p))
		  signed-sub-packets))
	    (public-key-algo-byte (public-key-algo->byte public-key-algo))
	    (hash-algo-byte (hash-algo->byte hash-algo))
	    (signature-type-byte (signature-type->byte signature-type)))
	 (trace-item "version: " version)
	 (trace-item "signature-type: " signature-type-byte " "
		(signature-type->human-readable signature-type))
	 (trace-item "public-key-algo: " public-key-algo-byte " "
		(public-key-algo->human-readable public-key-algo))
	 (trace-item "hash-algo: " hash-algo-byte " "
		(hash-algo->human-readable hash-algo))
	 (trace-item "encoding signed-sub-packets")
	 (encode-octet version str-p)
	 (encode-octet signature-type-byte str-p)
	 (encode-octet public-key-algo-byte str-p)
	 (encode-octet hash-algo-byte str-p)
	 (cond
	    ((and (not (date? creation-date))
		  (not creation-time-packet))
	     (error 'create-signed-packet-prefix-v4
		    "creation-date ist mandatory"
		    #f))
	    ((or (not (date? creation-date))
		 ;; both dates are the same.
		 (and creation-time-packet
		      (date=? creation-date
			 (with-access::PGP-Signature-Sub-Creation-Time
			       creation-time-packet (creation-date)
			    creation-date))))
	     (encode-sub-packets signed-sub-packets str-p))
	    ((and (date? creation-date)
		  creation-time-packet)
	     (error 'create-signed-packet-prefix-v4
		    "Conflicting creation-dates"
		    creation-date))
	    (else
	     (let ((packet (instantiate::PGP-Signature-Sub-Creation-Time
			      (critical? #f)
			      (creation-date creation-date))))
		(encode-sub-packets (cons packet signed-sub-packets) str-p))))
	 (close-output-port str-p))))

;; the secure-packets are needed to see if the issuer is in there.
(define (encode-insecure-sub-packets secure-packets
				     insecure-packets issuer p)
   ;; just in case the issuer is not in the packets.
   (let ((issuer-packet (any (lambda (p)
				(and (isa? p PGP-Signature-Sub-ID) p))
			     (append insecure-packets secure-packets))))
      (cond
	 ((not issuer-packet)
	  (let ((new-p (instantiate::PGP-Signature-Sub-ID
			  (critical? #f)
			  (key-id issuer))))
	     (encode-sub-packets (cons new-p insecure-packets) p)))
	 ((not (string=? issuer
		  (with-access::PGP-Signature-Sub-ID issuer-packet (key-id)
		     key-id)))
	  (error 'encode-insecure-sub-packets
		 "Conflicting issuers"
		 issuer))
	 (else
	  (encode-sub-packets insecure-packets p)))))

(define-method (encode-content
		packet::PGP-Signature-v4-Packet
		p::output-port)
   (with-trace 3 "encode-content ::PGP-Signature-v4-Packet"
      (with-access::PGP-Signature-v4-Packet packet
	    (version issuer public-key-algo signature signed-packet-prefix
		     left-hash secure-sub-packets insecure-sub-packets)
	 (when (not signed-packet-prefix)
	    (error 'encode-PGP-Signature-v4-Packet
		   "Signature Packet has not been preprocessed correctly"
		   #f))
	 ;; signed-packet-prefix contains (for v4 packets):
	 ;;   - signature-type
	 ;;   - public-key-algo
	 ;;   - hash-algo
	 ;;   - hashed subpackets (+ length)
	 (display signed-packet-prefix p)
	 (encode-insecure-sub-packets secure-sub-packets
				      insecure-sub-packets issuer p)
	 (encode-octets left-hash 2 p)
	 (case public-key-algo
	    ((rsa-encrypt/sign rsa-sign)
	     (encode-mpi signature p))
	    ((dsa) ;; DSA
	     (when (not (pair? signature))
		(error 'encode-PGP-Signature-v4-Packet
		       "signature-data in DSA-mode must be a pair"
		       signature))
	     (encode-mpi (car signature) p)  ;; r
	     (encode-mpi (cdr signature) p)) ;; s
	    (else
	     (error 'encode-PGP-Signature-v4-Packet
		    "signature-encoding not yet implemented"
		    (cons public-key-algo
			  (public-key-algo->human-readable public-key-algo))))))))

(define (encode-s2k s2k p::output-port)
   (with-trace 5 "encode-s2k"
      (let* ((algo (s2k-algo s2k))
	     (algo-byte (s2k-algo->byte algo))
	     (hash (s2k-hash s2k))
	     (hash-byte (hash-algo->byte hash))
	     (salt (s2k-salt s2k))
	     (count (s2k-count s2k)))
	 (trace-item "s2k-algo: " algo-byte " " (s2k-algo->human-readable algo))
	 (trace-item "hash-algo: " hash-byte " "
		(hash-algo->human-readable hash))
	 (encode-octet algo-byte p)
	 (encode-octet hash-byte p)
	 (case algo
	    ((simple)
	     'nothing-to-do)
	    ((salted)
	     (when (not (string? salt))
		(error 'encode-s2k
		       "expected string as salt"
		       salt))
	     (trace-item "salt: " (str->hex-string salt))
	     (encode-octets salt (s2k-salt-length) p))
	    ((iterated) ;; iterated and salted s2k
	     (when (not (string? salt))
		(error 'encode-s2k
		       "expected string as salt"
		       salt))
	     (when (not (fixnum? count))
		(error 'encode-s2k
		       "expected fixnum as count"
		       count))
	     (let ((encoded-count (iterated-salted-s2k-count->octet count)))
		(trace-item "salt: " (str->hex-string salt))
		(debug "count: " count " (" encoded-count ")")
		(encode-octets salt (s2k-salt-length) p)
		(encode-octet encoded-count p)))
	    (else
	     (error "encode-s2k"
		    "unknown s2k algorithm"
		    algo))))))

(define-method (encode-content
		packet::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
		p::output-port)
   (with-trace 3 "encode-content ::PGP-Symmetric-Key-Encrypted-Session-Key-Packet"
      (with-access::PGP-Symmetric-Key-Encrypted-Session-Key-Packet packet
	    (version algo s2k encrypted-session-key)
	 (when (not (=fx version 4))
	    (error 'encode-Symmetric-Key-Encrypted-Session
		   "Only encoding packets of version 4"
		   version))
	 (trace-item "version: " version)
	 (trace-item "symmetric-algo: " algo " "
		(symmetric-key-algo->human-readable algo))
	 (encode-octet version p)
	 (encode-octet (symmetric-key-algo->byte algo) p)
	 (encode-s2k s2k p)
	 ;; When encrypted-session-key is false, then the session key is computed
	 ;; from the password.
	 (when encrypted-session-key
	    (display encrypted-session-key p)
	    (trace-item "encrypted session key: "
		   (str->hex-string encrypted-session-key))))))

(define-method (encode-content
		packet::PGP-One-Pass-Signature-Packet
		p::output-port)
   (with-trace 3 "encode-content ::PGP-One-Pass-Signature-Packet"
      (with-access::PGP-One-Pass-Signature-Packet packet
	    (version signature-type issuer public-key-algo hash-algo
		     contains-nested-sig?)
	 (trace-item "version: " version)
	 (trace-item "signature-type: " signature-type
		     " " (signature-type->human-readable signature-type))
	 (trace-item "issuer: " (str->hex-string issuer))
	 (trace-item "public-key-algo: " public-key-algo " "
		     (public-key-algo->human-readable public-key-algo))
	 (trace-item "nested-sig?: " contains-nested-sig?)
	 (encode-octet version p)
	 (encode-octet (signature-type->byte signature-type) p)
	 (encode-octet (hash-algo->byte hash-algo) p)
	 (encode-octet (public-key-algo->byte public-key-algo) p)
	 (encode-octets issuer 8 p)
	 (encode-octet (if contains-nested-sig? 0 1) p))))

;; despite the name actually only needs a PGP-Key-Packet. However, even when
;; given a PGP-Secret-Key-Packet this function will only encode the public
;; part.
;; This function is meant to be used for fingerprint calculation.
(define (encode-public-key-content packet::PGP-Key-Packet p::output-port)
   (with-trace 4 "encode-public-key-content"
      (with-access::PGP-Key-Packet packet (version algo creation-date valid-days
						   key)
	 (encode-octet version p)
	 (encode-time creation-date p)
	 (trace-item "Version: " version)
	 (trace-item "Creation-date: " creation-date)
	 (when (or (=fx version 2) (=fx version 3))
	    (when (not (fixnum? valid-days))
	       (error 'encode-public-key
		      "v3 keys must have valid-days (as fixnum)"
		      valid-days))
	    (encode-scalar valid-days 2 p))
	 (encode-octet (public-key-algo->byte algo) p)
	 (when (or (=fx version 2) (=fx version 3))
	    (case algo
	       ((rsa-encrypt/sign rsa-encrypt rsa-sign) 'ok)
	       (else (error 'encode-public-key
			    "v3 keys must be RSA"
			    (cons algo
				  (public-key-algo->human-readable algo))))) )
	 (case algo
	    ((rsa-encrypt/sign rsa-encrypt rsa-sign)
	     (unless (isa? key Rsa-Key)
		(error 'encode-key
		   "invalid Rsa-Key"
		   key))
	     (with-access::Rsa-Key key (modulus exponent)
		(encode-mpi modulus p)
		(encode-mpi exponent p)))
	    ((dsa) ;; DSA
	     (unless (isa? key Dsa-Key)
		(error 'encode-key
		       "invalid Dsa-Key"
		       key))
	     (with-access::Dsa-Key key (q g y (keyp p))
		(encode-mpi keyp p)
		(encode-mpi q p)
		(encode-mpi g p)
		(encode-mpi y p)))
	    ((elgamal-encrypt elgamal-encrypt/sign)
	     (unless (isa? key ElGamal-Key)
		(error 'encode-key
		   "invalid ElGamal-key"
		   key))
	     (with-access::ElGamal-Key key ((elp p) g y)
		(encode-mpi elp p)
		(encode-mpi g p)
		(encode-mpi y p)))
	    (else
	     (error 'encode-key
		    "unsupported public key algorithm"
		    (cons algo
			  (public-key-algo->human-readable algo))))))))

(define-method (encode-content packet::PGP-Secret-Key-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-Secret-Key-Packet"
      (with-access::PGP-Secret-Key-Packet packet
	    (password-protected-secret-key-data)
	 (encode-public-key-content packet p)
	 (display password-protected-secret-key-data p))))

(define-method (encode-content packet::PGP-Public-Key-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-Public-Key-Packet"
      (encode-public-key-content packet p)))

(define-method (encode-content packet::PGP-Symmetrically-Encrypted-Packet
			       p::output-port)
   (with-trace 3 "encode-content ::PGP-Symmetrically-Encrypted-Packet"
      (with-access::PGP-Symmetrically-Encrypted-Packet packet (data)
	 (display data p))))

(define-method (encode-content packet::PGP-Marker-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-Marker-Packet"
      'do-nothing))

(define-method (encode-content packet::PGP-Literal-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-Literal-Packet"
      (with-access::PGP-Literal-Packet packet
	    (format for-your-eyes-only? file-name creation-date data)
	 (when (and for-your-eyes-only? file-name)
	    (error 'encode-content-literal
		   "'for-your-eyes-only' excludes filename"
		   file-name))
	 (let ((file (cond
			(for-your-eyes-only?
			 "_CONSOLE")
			((string? file-name)
			 file-name)
			(else
			 ""))))
	    (when (>fx (string-length file) 255)
	       (error 'encode-content-literal
		      "Filename too long (>255)"
		      file))
	    (trace-item "format: " format " " (literal-format->human-readable format))
	    (trace-item "file: " file)
	    (trace-item "creation-date: " creation-date)
	    (trace-item "data: \n-----------\n"  data "\n-----------\n")
	    (encode-octet (literal-format->byte format) p)
	    (encode-octet (string-length file) p)
	    (display file p)
	    (encode-time creation-date p)
	    (display data p)))))

(define-method (encode-content packet::PGP-Trust-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-Trust-Packet"
      (error 'encode-content-trust
	     "Trust Packet encoding not yet implemented"
	     #f)))

(define-method (encode-content packet::PGP-ID-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-ID-Packet"
      (with-access::PGP-ID-Packet packet (data)
	 (display data p))))

(define-method (encode-content packet::PGP-MDC-Symmetrically-Encrypted-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-MDC-Symmetrically-Encrypted-Packet"
      (with-access::PGP-MDC-Symmetrically-Encrypted-Packet packet (version data)
	 (encode-octet version p)
	 (display data p))))

(define-method (encode-content packet::PGP-MDC-Packet p::output-port)
   (with-trace 3 "encode-content ::PGP-MDC-Packet"
      (with-access::PGP-MDC-Packet packet (hash)
	 (display hash p))))
