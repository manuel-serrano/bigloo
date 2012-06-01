;*=====================================================================*/
;*    .../prgm/project/bigloo/api/openpgp/src/Llib/pgp_decode.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Wed Aug 18 10:24:37 2010                          */
;*    Last change :  Fri Jun  1 11:50:22 2012 (serrano)                */
;*    Copyright   :  2010-12 Florian Loitsch, Manuel Serrano.          */
;*    -------------------------------------------------------------    */
;*    OpenPGP decode                                                   */
;*    -------------------------------------------------------------    */
;*    See rfc4880 for technical details:                               */
;*       http://tools.ietf.org/html/rfc4880                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
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
	   (decode-mpi::bignum p::input-port)))

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
			(read-chars 256 pp)))
		    ((eof-object? str) #f)
		    (else str)))))))))

(define (decode-packet p::input-port)
   (define (decode-packet-v3 packet-tag p)
      (trace-item "old format")
      (let ((content-tag-byte (bit-and #x0F (bit-rsh packet-tag 2))))
	 (when (zerofx? content-tag-byte)
	    (error "decode-content-tag" "invalid tag" 0))
	 (receive (len partial?)
	    (decode-packet-length-v3 p packet-tag)
	    (values (byte->content-tag content-tag-byte) len partial?))))
   
   (define (decode-packet-v4 packet-tag p)
      (trace-item "new format")
      (let ((content-tag-byte (bit-and packet-tag #x3F)))
	 (receive (len partial?)
	    (decode-packet-length-v4 p)
	    (values (byte->content-tag content-tag-byte) len partial?))))
   
   (with-trace 2 "decode-packet"
      (let ((packet-tag (safe-read-octet p)))
	 (when (zerofx? (bit-and #x80 packet-tag))
	    (error "decode-packet" "bad packet" packet-tag))
	 
	 (trace-item "Packet-tag=" packet-tag)
	 (trace-item "port=" p)
	 (receive (content-tag len partial?)
	    (if (zerofx? (bit-and #x40 packet-tag))
		;; old packet format
		(decode-packet-v3 packet-tag p)
		(decode-packet-v4 packet-tag p))
	    (trace-item "Content-tag=" content-tag " "
			(content-tag->human-readable content-tag))
	    (trace-item "Len=" len " partial=" partial?)
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
		      (trace-item "remaining length=" len)
		      (trace-item "str=" (str->hex-string (substring tmp 0 (min 10 len)))))
		   (error 'decode-packet
			  "Unknown content-tag"
			  content-tag))))))))

(define (decode-packets p::input-port)
   (let ((c (peek-char p)))
      (if (eof-object? c)
	  '()
	  (let ((packet (decode-packet p)))
	     (cons packet
		   (decode-packets p))))))

(define (decode-s2k p::input-port)
   (with-trace 5 "decode-s2k"
      (let* ((s2k-algo-byte (safe-read-octet p))
	     (s2k-algo (byte->s2k-algo s2k-algo-byte))
	     (hash-algo-byte (safe-read-octet p))
	     (hash-algo (byte->hash-algo hash-algo-byte)))
	 (trace-item "s2k-algo: " s2k-algo-byte " "
		     (s2k-algo->human-readable s2k-algo))
	 (trace-item "hash-algo: " hash-algo-byte " "
		     (hash-algo->human-readable hash-algo))
	 (case s2k-algo
	    ((simple)
	     (make-s2k s2k-algo hash-algo #f #f))
	    ((salted)
	     (let ((salt (safe-read-octets (s2k-salt-length) p)))
		(trace-item "salt: " (str->hex-string salt))
		(make-s2k s2k-algo hash-algo salt #f)))
	    ((iterated)
	     (let* ((salt (safe-read-octets (s2k-salt-length) p))
		    (encoded-count (safe-read-char p))
		    (count (octet->iterated-salted-s2k-count encoded-count)))
		(trace-item "salt: " (str->hex-string salt))
		(trace-item "count: " count " (" (char->integer encoded-count) ")")
		(make-s2k s2k-algo hash-algo salt count)))
	    (else
	     (error "decode-s2k"
		    "unknown s2k algorithm"
		    s2k-algo))))))

(define (decode-time::date p::input-port #!optional (treat-0-as-present? #f))
   (let ((n (decode-scalar p 4)))
      (if (and treat-0-as-present? (zerofx? n))
	  (current-date)
	  (seconds->date n))))

(define (decode-mpi::bignum p::input-port)
   (with-trace 3 "decode-mpi"
      (let* ((nb-bits (decode-scalar p 2))
	     (nb-octets (/fx (+fx nb-bits 7) 8)))
	 (trace-item "reading mpi of " nb-bits " bits (" nb-octets " octets)")
	 (let loop ((i 0)
		    (n #z0))
	    (cond
	       ((=fx i nb-octets)
		n)
	       (else (loop (+fx i 1)
			   (+bx (*bx n #z256)
				(fixnum->bignum (safe-read-octet p))))))))))

(define (decode-scalar::long p::input-port len::long)
   (with-trace 4 "decode-scalar"
      (trace-item "len=" len)
      (let loop ((i 0)
		 (n 0))
	 (cond
	    ((=fx i len)
	     n)
	    (else
	     (let ((o (safe-read-octet p)))
		(trace-item "o=" o)
		(loop (+fx i 1) (+fx (*fx n 256) o))))))))

;; ----------------------------------------------------------------------------
;; 5.1 Public-Key Encrypted Session Key Packets (Tag 1)
;; ----------------------------------------------------------------------------
(define (decode-public-key-encrypted-session-key cpp)
   (with-trace 4 "decode-public-key-encrypted-session-key"
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
	 (trace-item "version: " version)
	 (trace-item "id: " (str->hex-string id))
	 (trace-item "algo: " algo " " (public-key-algo->human-readable algo))
	 (trace-item "secret-data: " (str->hex-string secret-data))
	 (case algo
	    ((rsa-encrypt/sign rsa-encrypt)
	     (let* ((m**e (decode-mpi kp)))
		(trace-item "RSA-m**e: " m**e)
		(instantiate::PGP-Public-Key-Encrypted-Session-Key-Packet
		   (version version)
		   (id id)
		   (algo algo)
		   (encrypted-session-key m**e))))
	    ((elgamal-encrypt elgamal-encrypt/sign)
	     (let* ((g**k (decode-mpi kp))
		    (dummy (trace-item "g**k done"))
		    (m*y**k (decode-mpi kp)))
		(trace-item "ElGamal-g**k: " g**k)
		(trace-item "ElGamal-m*y**k: " m*y**k)
		(instantiate::PGP-Public-Key-Encrypted-Session-Key-Packet
		   (version version)
		   (id id)
		   (algo algo)
		   (encrypted-session-key (cons g**k m*y**k)))))
	    (else
	     (error "encrypted session key"
		    "Can't read encrypted-session key with this public algo."
		    (cons algo-byte (public-key-algo->human-readable algo))))))))

;; ----------------------------------------------------------------------------
;; 5.2 Signature Packet (Tag 2)
;; ----------------------------------------------------------------------------
(define (decode-signature p::input-port)
   (with-trace 4 "decode-signature"
      (let ((version (safe-read-octet p)))
	 (trace-item "Signature version: " version)
	 (case version
	    ((3) (decode-signature-v3 p))
	    ((4) (decode-signature-v4 p version))
	    (else (error "decode-signature"
			 "Can't decode signatures of this version"
			 version))))))

;; ----------
;; 5.2.2 Version 3 Signature Packet Format
;; ----------------------------------------------------------------------------
(define (decode-signature-v3 p::input-port)
   (with-trace 3 "decode-signature-v3"
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
	 (trace-item "signature type: " signature-type-byte " "
		(signature-type->human-readable signature-type))
	 (trace-item "creation-time: " creation-time)
	 (trace-item "key-id: " (str->hex-string key-id))
	 (trace-item "public-key-algo: " public-key-algo-byte " "
		(public-key-algo->human-readable public-key-algo))
	 (trace-item "hash-algo: " hash-algo-byte " "
		(hash-algo->human-readable hash-algo))
	 (trace-item "hashed-str: " (str->hex-string hashed-str))
	 (trace-item "left-hash: " (str->hex-string left-hash))
	 (let ((key-data
		(case public-key-algo
		   ((rsa-encrypt/sign rsa-sign)
		    (let ((m**d (decode-mpi p)))
		       (trace-item "RSA m**d: " m**d)
		       m**d))
		   ((dsa) ;; DSA
		    (let* ((r (decode-mpi p))
			   (s (decode-mpi p)))
		       (trace-item "DSA r: " r)
		       (trace-item "DSA s: " s)
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
	       (left-hash left-hash))))))

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

   (with-trace 4 "decode-sub-packet"
      (receive (len partial?)
	 (decode-packet-length-v4 p)
	 (when partial? (error "decode-sub-package"
			       "sub-packages must not have partial lengths"
			       #f))
	 ;(trace-item "sub-packet of length: " len)
	 (let* ((type-w/-critical (safe-read-octet p))
		(type-byte (bit-and #x7F type-w/-critical))
		(type (byte->subpacket-type type-byte))
		(critical? (not (zerofx? (bit-and #x80 type-w/-critical)))))
	    (trace-item "sub-packet of type: " type-byte
		   " " (subpacket-type->human-readable type))
	    (trace-item "sub-packet is critical?: " critical?)
	    (trace-item "type=" type)
	    
	    ;; currently the following
	    (case type
	       ((creation-time)
		(let ((t (decode-time p)))
		   (trace-item "Date: " t)
		   (instantiate::PGP-Signature-Sub-Creation-Time
		      (critical? critical?)
		      (creation-date t))))
	       ((expiration-time)
		(let ((t (decode-time p)))
		   (trace-item "Date: " t)
		   (instantiate::PGP-Signature-Sub-Expiration-Time
		      (critical? critical?)
		      (expiration-date t))))
	       ((exportable?)
		(let ((exportable? (=fx 1 (safe-read-octet p))))
		   (trace-item "Exportable?: " exportable?)
		   (instantiate::PGP-Signature-Sub-Exportable
		      (critical? critical?)
		      (exportable? exportable?))))
	       ((trust)
		(let* ((level (safe-read-octet p))
		       (trust (safe-read-octet p)))
		   (trace-item "Level/Trust: " level "/" trust)
		   (instantiate::PGP-Signature-Sub-Trust
		      (critical? critical?)
		      (level level)
		      (amount trust))))
	       ;; TODO ((6) 'regular-expression)
	       ((revocable?)
		(let ((revocable? (=fx 1 (safe-read-octet p))))
		   (instantiate::PGP-Signature-Sub-Revocable
		      (critical? critical?)
		      (revocable? revocable?))))
	       ((key-expiration-time)
		(let ((t (decode-scalar p 4)))
		   (trace-item "expiration-time: " t)
		   (instantiate::PGP-Signature-Sub-Key-Expiration-Time
		      (critical? critical?)
		      (expiration-time t))))
	       ((placeholder) ;; placeholder for backward compatibility
		(let ((tmp (safe-read-octets (-fx len 1) p)))
		   (trace-item "generic: " (str->hex-string tmp))
		   (instantiate::PGP-Signature-Sub-Generic
		      (critical? critical?)
		      (type type)
		      (data tmp))))
	       ((preferred-symmetric)
		(let ((prefs (preferences->list
			      (safe-read-octets (-fx len 1) p)
			      byte->symmetric-key-algo)))
		   (trace-item "preferred symmetrics: "
			  (map symmetric-key-algo->human-readable prefs))
		   (instantiate::PGP-Signature-Sub-Preferred-Symmetric
		      (critical? critical?)
		      (algos prefs))))
	       ((revocation-key)
		(let* ((class (safe-read-octet p))
		       (sensitive? (not (zerofx? (bit-and #x40 class))))
		       (algid (safe-read-octet p))
		       (fingerprint (safe-read-octets 20 p)))
		   (trace-item "class: " class)
		   (trace-item "sensitive?: " sensitive?)
		   (trace-item "algid: " algid)
		   (trace-item "finger-print: " (str->hex-string fingerprint))
		   (when (zerofx? (bit-and #x80 class))
		      (error
			 "decode-signature-sub-packet"
			 "revocation-key signature must have bit 0x80 set"
			 (format "0x~x" class)))
		   (instantiate::PGP-Signature-Sub-Revocation
		      (critical? critical?)
		      (clazz class)
		      (sensitive? sensitive?)
		      (algid algid)
		      (fingerprint fingerprint))))
	       ((issuer-ID)
		(let ((id (safe-read-octets 8 p)))
		   (trace-item "Id: " (str->hex-string id))
		   (instantiate::PGP-Signature-Sub-ID
		      (critical? critical?)
		      (key-id id))))
	       ((notation)
		(let* ((flags (safe-read-octets 4 p))
		       (name-len (decode-scalar p 2))
		       (value-len (decode-scalar p 2))
		       (name-data (safe-read-octets name-len p))
		       (value-data (safe-read-octets value-len p)))
		   (trace-item "flags: " flags)
		   (trace-item "name: " name-data)
		   (trace-item "value: " value-data)
		   (instantiate::PGP-Signature-Sub-Notation
		      (critical? critical?)
		      (flags flags)
		      (name name-data)
		      (value value-data))))
	       ((preferred-hash)
		(let ((prefs (preferences->list
			      (safe-read-octets (-fx len 1) p)
			      byte->hash-algo)))
		   (trace-item "Preferred Hash-algo: "
			  (map hash-algo->human-readable prefs))
		   (instantiate::PGP-Signature-Sub-Preferred-Hash
		      (critical? critical?)
		      (algos prefs))))
	       ((preferred-compression)
		(let ((prefs (preferences->list
				(safe-read-octets (-fx len 1) p)
				byte->compression-algo)))
		   (trace-item "Preferred Compression algo: "
		      (map compression-algo->human-readable prefs))
		   (instantiate::PGP-Signature-Sub-Preferred-Compression
		      (critical? critical?)
		      (algos prefs))))
	       ; TODO ((key-server-prefs) ;; preferred key server preferences
	       ((preferred-key-server)
		(let ((key-server (safe-read-octets (-fx len 1) p)))
		   (trace-item "Preferred Key-server:" key-server)
		   (instantiate::PGP-Signature-Sub-Preferred-Key-Server
		      (critical? critical?)
		      (server key-server))))
	       ((primary-id?)
		(let ((prim-id? (not (=fx 0 (safe-read-octet p)))))
		   (trace-item "Primary Id?: " prim-id?)
		   (instantiate::PGP-Signature-Sub-Primary-ID
		      (critical? critical?)
		      (primary? prim-id?))))
	       ((policy)
		(let ((policy (safe-read-octets (-fx len 1) p)))
		   (trace-item "Policy: " policy)
		   (instantiate::PGP-Signature-Sub-Policy
		      (critical? critical?)
		      (url policy))))
	       ; TODO ((key-flags) ;; key flags
	       ((signer-ID)
		(let ((id (safe-read-octets (-fx len 1) p)))
		   (trace-item "Signer id: " id)
		   (instantiate::PGP-Signature-Sub-Signer-ID
		      (critical? critical?)
		      (id id))))
	       ((revocation-reason)
		(let* ((code-byte (safe-read-octet p))
		       (code (byte->revocation-code code-byte))
		       (reason (safe-read-octets (-fx len 2) p)))
		   (trace-item "Revocation code: " code-byte " "
		      (revocation-code->human-readable code))
		   (trace-item "Reason: " reason)
		   (instantiate::PGP-Signature-Sub-Revocation-Reason
		      (critical? critical?)
		      (code code)
		      (reason reason))))
	       (else
		(trace-item "Generic")
		(instantiate::PGP-Signature-Sub-Generic
		   (critical? critical?)
		   (type type)
		   (data (safe-read-octets (-fx len 1) p)))))))))

(define (decode-sub-packets p::input-port)
   (with-trace 3 "decode-sub-packets"
      (let ((c (peek-char p)))
	 (trace-item "c=" (char->integer c))
	 (if (eof-object? c)
	     '()
	     (let ((sub-packet (decode-sub-packet p)))
		(trace-item "sub-patch=" (typeof sub-packet))
		(cons sub-packet (decode-sub-packets p)))))))

;; -------- 5.2.3
(define (decode-signature-v4 p::input-port version)
   
   (define (find-creation-date sps)
      (let ((pkt (any (lambda (pkt)
			 (and (isa? pkt PGP-Signature-Sub-Creation-Time) pkt))
		      sps)))
	 
	 (when (not pkt)
	    (error
	     "decode-signature-v4"
	     "invalid signature. Can't find obligatory creation-time sub-packet"
	     #f))
	 (with-access::PGP-Signature-Sub-Creation-Time pkt (creation-date)
	    creation-date)))
   (define (find-issuer sps)
      (let ((pkt (any (lambda (pkt)
			 (and (isa? pkt PGP-Signature-Sub-ID) pkt))
		    sps)))
	 (when (not pkt)
	    (error
	       "decode-signature-v4"
	       "invalid signature. Can't find obligatory issuer id sub-packet"
	       #f))
	 (with-access::PGP-Signature-Sub-ID pkt (key-id) key-id)))

   (with-trace 3 "decode-signature-v4"
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
	     (dumm1 (trace-item "*- decoding hashed subpackets="
		       (string-for-read hashed-spd)))
	     (sps1 (decode-sub-packets (open-input-string hashed-spd)))
	     (creation-date (find-creation-date sps1))
	     (dumm2 (trace-item "creation-date=" creation-date))
	     (unhashed-spd-len (decode-scalar p 2))
	     (dumm1 (trace-item "*- decoding unhashed subpackets"))
	     (sps2 (decode-sub-packets
		    (length-limited-pipe-port p unhashed-spd-len)))
	     (sps1+sps2 (append sps2 sps1))
	     (issuer (find-issuer sps1+sps2))
	     (left-hash (safe-read-octets 2 p)))
	 (trace-item "signature type: " signature-type-byte " "
		(signature-type->human-readable signature-type))
	 (trace-item "public-key-algo: " public-key-algo-byte " "
		(public-key-algo->human-readable public-key-algo))
	 (trace-item "hash-algo: " hash-algo-byte " "
		(hash-algo->human-readable hash-algo))
	 (trace-item "hashed-subpacket-data: " (str->hex-string hashed-spd))
	 ;       (trace-item "sps1:")
	 ;       (for-each (lambda (sp)
	 ; 		   (with-access::PGP-Signature-Sub-Packet sp (type)
	 ; 		      (trace-item "  " (subpacket-type->human-readable type)
	 ; 			     " " sp)))
	 ; 		sps1)
	 ;       (trace-item "sps2:")
	 ;       (for-each (lambda (sp)
	 ; 		   (with-access::PGP-Signature-Sub-Packet sp (type)
	 ; 		      (trace-item "  " (subpacket-type->human-readable type)
	 ; 			     " " sp)))
	 ; 		sps2)
	 (trace-item "left-hash: " (str->hex-string left-hash))
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
	    
	    (trace-item "signed-packet-prefix: " (str->hex-string signed-packet-prefix))
	    (trace-item "hash-trailer: " (str->hex-string hash-trailer))
	    (let ((key-data
		   (case public-key-algo
		      ((rsa-encrypt/sign rsa-sign)
		       (let ((m**d (decode-mpi p)))
			  (trace-item "RSA m**d: " m**d)
			  m**d))
		      ((dsa)
		       (let* ((r (decode-mpi p))
			      (s (decode-mpi p)))
			  (trace-item "DSA r: " r)
			  (trace-item "DSA s: " s)
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
		  (insecure-sub-packets sps2)))))))

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
   (with-trace 4 "decode-one-pass-signature"
      (let* ((version (safe-read-octet p))
	     (signature-type-byte (safe-read-octet p))
	     (signature-type (byte->signature-type signature-type-byte))
	     (hash-algo-byte (safe-read-octet p))
	     (hash-algo (byte->hash-algo hash-algo-byte))
	     (public-key-algo-byte (safe-read-octet p))
	     (public-key-algo (byte->public-key-algo public-key-algo-byte))
	     (id (safe-read-octets 8 p))
	     (nested-signature? (zerofx? (safe-read-octet p))))
	 (trace-item "signature type: " signature-type-byte " "
		(signature-type->human-readable signature-type))
	 (trace-item "public-key-algo: " public-key-algo-byte " "
		(public-key-algo->human-readable public-key-algo))
	 (trace-item "hash-algo: " hash-algo-byte " "
		(hash-algo->human-readable hash-algo))
	 (trace-item "id: " (str->hex-string id))
	 (trace-item "nested-sig?: " nested-signature?)
	 (instantiate::PGP-One-Pass-Signature-Packet
	    (version version)
	    (signature-type signature-type)
	    (issuer id)
	    (public-key-algo public-key-algo)
	    (hash-algo hash-algo)
	    (contains-nested-sig? nested-signature?)))))

;; ----------------------------------------------------------------------------
;; 5.5 Key Material Packet
;; ----------------------------------------------------------------------------

;; ----------
;; 5.5.1.1 Public Key Packet
;; ----------------------------------------------------------------------------
(define make-public-rsa-key (lambda (m e)
			       (instantiate::Rsa-Key
				  (modulus m)
				  (exponent e))))
(define make-public-dsa-key (lambda (p q g y)
			       (instantiate::Dsa-Key
				  (p p)
				  (q q)
				  (g g)
				  (y y))))
(define make-public-elgamal-key (lambda (p g y)
				   (instantiate::ElGamal-Key
				      (p p)
				      (g g)
				      (y y))))

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
   (with-trace 2 "decode/fill-key"
      (let ((cd (decode-time p)))
	 (trace-item "creation-date: " cd)
	 (with-access::PGP-Key-Packet kp (creation-date)
	    (set! creation-date cd)))
      (when (or (=fx version 2) (=fx version 3))
	 (let ((vd (decode-scalar p 2))) ;; if 0 indefinite
	    (trace-item "valid days: " vd)
	    (with-access::PGP-Key-Packet kp (valid-days)
	       (set! valid-days vd))))
      (let* ((algo-byte (safe-read-octet p))
	     (algo (byte->public-key-algo algo-byte)))
	 (when (and (or (=fx version 2) (=fx version 3))
		    (not (or (eq? algo 'rsa-encrypt/sign)
			     (eq? algo 'rsa-encrypt)
			     (eq? algo 'rsa-sign))))
	    (error "decode key v3"
		   "only RSA is supported for version 3 keys"
		   (public-key-algo->human-readable algo)))
	 (trace-item "algo: " algo " " (public-key-algo->human-readable algo))
	 (with-access::PGP-Key-Packet kp ((kalgo algo))
	    (set! kalgo algo))
	 (case algo
	    ((rsa-encrypt/sign rsa-encrypt rsa-sign)
	     (let* ((modulus (decode-mpi p))
		    (exponent (decode-mpi p))
		    (k (make-public-rsa-key modulus exponent)))
		(trace-item "modulus: " modulus)
		(trace-item "exponent: " exponent)
		(with-access::PGP-Key-Packet kp (key) (set! key k))))
	    ((dsa) ;; DSA
	     (let* ((port p)
		    (p (decode-mpi port))
		    (q (decode-mpi port))
		    (g (decode-mpi port))
		    (y (decode-mpi port))
		    (k (make-public-dsa-key p q g y)))
		(trace-item "p: " p)
		(trace-item "q: " q)
		(trace-item "g: " g)
		(trace-item "y: " y)
		(with-access::PGP-Key-Packet kp (key) (set! key k))))
	    ((elgamal-encrypt elgamal-encrypt/sign)
	     (let* ((port p)
		    (p (decode-mpi port))
		    (g (decode-mpi port))
		    (y (decode-mpi port))
		    (k (make-public-elgamal-key p g y)))
		(trace-item "p: " p)
		(trace-item "g: " g)
		(trace-item "y: " y)
		(with-access::PGP-Key-Packet kp (key) (set! key k))))
	    (else (error "decode-key"
			 "public key algorithm must be RSA, DSA or ElGamal"
			 (public-key-algo->human-readable algo)))))))

;; ----------
;; 5.5.1.2 Public Subkey Packet
;; ----------------------------------------------------------------------------
(define (decode-public-subkey cpp)
   (let ((p (decode-public-key cpp)))
      (with-access::PGP-Public-Key-Packet p (subkey?) (set! subkey? #t))
      p))


;; ----------
;; 5.5.1.3 Secret Key Packet
;; ----------------------------------------------------------------------------
(define (decode-secret-key cpp::input-port)
   (with-trace 2 "decode-secreate-key"
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
	    (trace-item "secret data: " (str->hex-string secret-data))
	    (with-access::PGP-Secret-Key-Packet kp
		  (password-protected-secret-key-data)
	       (set! password-protected-secret-key-data secret-data))
	    kp))))

;; ----------
;; 5.5.1.4 Secret Subkey Packet
;; ----------------------------------------------------------------------------
(define (decode-secret-subkey cpp)
   (let ((p (decode-secret-key cpp)))
      (with-access::PGP-Secret-Key-Packet p (subkey?) (set! subkey? #t))
      p))

;; ----------------------------------------------------------------------------
;; 5.6 Compressed Data Packet
;; ----------------------------------------------------------------------------
(define (decode-compressed-data cpp)
   ;; note: we discard the compression-packet and return the uncompressed
   ;; packet. Maybe this is not intended in which case we could insert a new
   ;; 'Compression-Packet'.
   (with-trace 2 "decode-compressed-data"
      (let* ((algo-byte (safe-read-octet  cpp))
	     (algo (byte->compression-algo algo-byte)))
	 (trace-item "Compression Algorithm: " algo " "
		     (compression-algo->human-readable algo))
	 (case algo
	    ((uncompressed) (decode-packet cpp))
	    ((ZIP)
	     (let ((pz (port->inflate-port cpp)))
		(unwind-protect
		   (instantiate::PGP-Compressed-Packet
		      (packets (decode-packets pz)))
		   (close-input-port pz))))
	    ;; TODO: fix compression
	    ((ZLIB)
	     (let ((pz (port->zlib-port cpp)))
		(unwind-protect
		   (instantiate::PGP-Compressed-Packet
		      (packets (decode-packets pz)))
		   (close-input-port pz))))
	    (else (error "decode compressed" "Can't decompresse data" algo))))))

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
   (with-trace 5 "decode-literal-data"
      (let* ((format-byte (safe-read-octet p))
	     (format (byte->literal-format format-byte))
	     (file-len (safe-read-octet p))
	     (file-name (safe-read-octets file-len p))
	     (sensitive? (string=? file-name "_CONSOLE"))
	     (creation-date (decode-time p #t))
	     (data (read-string p)))
	 (trace-item "format: " format-byte " "
		     (literal-format->human-readable format))
	 (trace-item "file: " file-name)
	 (trace-item "creation-date: " creation-date)
	 (trace-item "data: \n-------\n" data "\n---------\n")
	 (instantiate::PGP-Literal-Packet
	    (format format)
	    (for-your-eyes-only? sensitive?)
	    (file-name (and (not sensitive?) file-name))
	    (creation-date creation-date)
	    (data data)))))

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
   (with-trace 3 "decode-user-id"
      (let ((user-id (read-string cpp)))
	 (trace-item "User ID: " user-id)
	 (instantiate::PGP-ID-Packet
	    (data user-id)))))

;; ----------------------------------------------------------------------------
;; 5.12 User ID Packet (Tag 17)
;; ----------------------------------------------------------------------------
(define (decode-user-attribute cpp)
   (with-trace 3 "decode-user-attribute"
      (let ((user-attr (read-string cpp)))
	 (trace-item "User ATTR: " (string-for-read user-attr))
	 (instantiate::PGP-Attribute-Packet
	    (data user-attr)))))

;; ----------------------------------------------------------------------------
;; 5.13 Sym. Encrypted Integrity Protected Data Packet (Tag 18)
;; ----------------------------------------------------------------------------
(define (decode-mdc-sym-encrypted cpp)
   (with-trace 4 "decode-mdc-sym-encrypted"
      (trace-item "cpp=" cpp)
      (let* ((version (safe-read-octet cpp))
	     (data (read-string cpp)))
	 (trace-item "version: " version)
	 (trace-item "len: " (string-length data))
	 (instantiate::PGP-MDC-Symmetrically-Encrypted-Packet
	    (version version)
	    (data data)))))

;; ----------------------------------------------------------------------------
;; 5.14 Modification Detection Code Packet (Tag 19)
;; ----------------------------------------------------------------------------
(define (decode-mdc cpp)
   (with-trace 4 "decode-mdc"
      (let ((str (read-string cpp)))
	 (when (not (=fx (string-length str) 20))
	    (error 'decode-mdc
		   "Bad Modification Detection Code Packet"
		   #f))
	 (trace-item "sha1 mdc: " (str->hex-string str))
	 (instantiate::PGP-MDC-Packet
	    (hash str)))))
