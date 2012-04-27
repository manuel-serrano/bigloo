;*=====================================================================*/
;*    .../project/bigloo/api/openpgp/src/Llib/pgp_composition.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Mon Aug 30 09:36:17 2010                          */
;*    Last change :  Fri Apr 27 11:11:40 2012 (serrano)                */
;*    Copyright   :  2010-12 Florian Loitsch, Manuel Serrano           */
;*    -------------------------------------------------------------    */
;*    RFC2440 encoding/decoding                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __openpgp-composition
   (import __openpgp-util
	   __openpgp-packets
	   __openpgp-decode
	   __openpgp-encode
	   __openpgp-port-util)
   (export
    (decode-pgp p::input-port)
    (parse-packets packets::pair-nil)
    (encode-native-pgp pgp-composition::PGP-Composition p::output-port)
    (encode-armored-pgp pgp-composition::PGP-Composition
			main-header-info::bstring
			headers::pair-nil
			p::output-port))
   (export
    (final-class Signed-ID
       id::PGP-ID-Packet
       sigs::pair-nil)
    (final-class PGP-Subkey
       key-packet::PGP-Key-Packet
       sigs::pair-nil ;; nil if main-key otherwise a PGP-Signature-Packet
       revocation-sigs::pair-nil ;; of PGP-Signature-Packet
       pgp-key::PGP-Key) ;; cyclic data-structure...
    (abstract-class PGP-Composition)
    (final-class PGP-Key::PGP-Composition
       subkeys::pair       ;; the first subkey is the main-key.
       user-ids::pair-nil) ;; of Signed-ID
    (abstract-class PGP-Message::PGP-Composition)
    (final-class PGP-Encrypted::PGP-Message
       session-keys::pair-nil ;; of PGP-{Symmetric|Public}-Key-Encrypted-Session-Key-Packets
       encrypted-data::PGP-Symmetrically-Encrypted-Packet) ;; or PGP-MDC-Symm...
    
    (class PGP-Signature::PGP-Message
       msg ;; could be #f in case of a detached signature. Otherwise Literal.
       sigs::pair-nil) ;; of PGP-Signature-Packet
    (final-class PGP-One-Pass-Signature::PGP-Signature
       one-pass-sigs::pair-nil) ;; each one in relation with the sigs.
    (final-class PGP-Literal::PGP-Message
       literal::PGP-Literal-Packet)))

;*---------------------------------------------------------------------*/
;*    decode-pgp-content ...                                           */
;*    -------------------------------------------------------------    */
;*    either returns a PGP-Composition, or a list of PGP-Keys.         */
;*---------------------------------------------------------------------*/
(define (decode-pgp-content p::input-port)
   ;; TODO: we only handle Compressed packets at the "toplevel", as a message
   ;; inside signatures and inside Literals.
   (with-trace 1 "decode-pgp-content"
      (trace-item "p=" p)
      (let ((packets (decode-packets p)))
	 (parse-packets packets))))

;*---------------------------------------------------------------------*/
;*    parse-packets ...                                                */
;*---------------------------------------------------------------------*/
(define (parse-packets packets)
   (with-trace 2 "parse-packets"
      (cond
	 ((null? packets)
	  (error 'parse-packets
		 "no packet decoded"
		 #f))
	 ((and (isa? (car packets) PGP-Key-Packet)
	       (with-access::PGP-Key-Packet (car packets) (subkey?)
		  (not subkey?)))
	  (parse-keys packets))
	 ((or (isa? (car packets) PGP-Symmetrically-Encrypted-Packet)
	      (isa? (car packets) PGP-Session-Key-Packet))
	  (parse-encrypted-message packets))
	 ((isa? (car packets) PGP-Signature-Packet)
	  (parse-signature packets))
	 ((isa? (car packets) PGP-One-Pass-Signature-Packet)
	  (parse-one-pass-signature packets))
	 ((isa? (car packets) PGP-Literal-Packet)
	  (parse-literal packets))
	 ((isa? (car packets) PGP-Compressed-Packet)
	  (trace-item "parsing compressed packet")
	  (when (not (null? (cdr packets)))
	     (warning "discarding packets"))
	  (with-access::PGP-Compressed-Packet (car packets) (packets)
	     (parse-packets packets)))
	 (else
	  (error 'parse-packets
		 "could not parse pgp-message"
		 packets)))))

;*---------------------------------------------------------------------*/
;*    parse-keys ...                                                   */
;*    -------------------------------------------------------------    */
;*    rfc 2440 allows several keys to be concatenated                  */
;*---------------------------------------------------------------------*/
(define (parse-keys packets)
   (with-trace 3 "parse-keys"
      (let loop ((packets packets)
		 (keys '())
		 (user-ids '()))
	 (if (null? packets)
	     keys
	     (receive (key remaining-packets nuser-ids)
		(parse-key packets user-ids)
		(loop remaining-packets
		      (append key keys)
		      (append nuser-ids user-ids)))))))

;*---------------------------------------------------------------------*/
;*    parse-key ...                                                    */
;*---------------------------------------------------------------------*/
(define (parse-key packets user-ids)
   
   (define (revocation-signature? pkt)
      (and (isa? pkt PGP-Signature-Packet)
	   (with-access::PGP-Signature-Packet pkt (signature-type)
	      (eq? signature-type 'key-revocation))))
   
   (define (parse-revocation-sigs packets)
      (let loop ((packets packets)
		 (revocations '()))
	 (cond
	    ((null? packets)
	     (values (reverse! revocations) '()))
	    ((revocation-signature? (car packets))
	     (loop (cdr packets)
		   (cons (car packets) revocations)))
	    (else
	     (values (reverse! revocations) packets)))))
   
   (define (parse-user-id-sigs packets)
      (define (user-id-sig? pkt)
	 (and (isa? pkt PGP-Signature-Packet)
	      (with-access::PGP-Signature-Packet pkt (signature-type)
		 (case signature-type
		    ((generic-certif persona-certif casual-certif positive-certif)
		     #t)
		    (else #f)))))
      (let loop ((packets packets)
		 (sigs '()))
	 (cond
	    ((null? packets)
	     (values (reverse! sigs) '()))
	    ((user-id-sig? (car packets))
	     (loop (cdr packets)
		   (cons (car packets) sigs)))
	    (else
	     (values (reverse! sigs) packets)))))
   
   (define (parse-user-ids packets)
      (with-trace 3 "parse-user-ids"
	 (let loop ((packets packets)
		    (user-ids '()))
	    (cond
	       ((null? packets)
		(values (reverse! user-ids) '()))
	       ((isa? (car packets) PGP-ID-Packet)
		(trace-item "PGP-ID-Packet=" (find-runtime-type (car packets)))
		(receive (sigs remaining-packets)
		   (parse-user-id-sigs (cdr packets))
		   (loop remaining-packets
			 (cons (instantiate::Signed-ID
				  (id (car packets))
				  (sigs sigs))
			       user-ids))))
	       (else
		(values (reverse! user-ids) packets))))))
   
   (define (parse-subkeys packets)
      (define (subkey-sig? pkt)
	 (and (isa? pkt PGP-Signature-Packet)
	      (with-access::PGP-Signature-Packet pkt (signature-type)
		 (eq? signature-type 'subkey-binding))))
      (let loop ((packets packets)
		 (subkeys '()))
	 (cond
	    ((or (null? packets)
		 (null? (cdr packets)))
	     (values (reverse! subkeys) '()))
	    ((isa? (car packets) PGP-Key-Packet)
	     (let liip ((pkts (cdr packets))
			(subkey-sigs '())
			(revoc-sigs '()))
		(cond
		   ((or (null? pkts)
			(and (not (subkey-sig? (car pkts)))
			     (not (revocation-signature? (car pkts)))))
		    (when (null? subkey-sigs)
		       (error 'parse-subkey
			      "Subkey not followed by subkey-binding Signature"
			      #f))
		    (loop pkts
			  (cons (instantiate::PGP-Subkey
				   (key-packet (car packets))
				   (sigs subkey-sigs)
				   (revocation-sigs revoc-sigs)
				   (pgp-key (class-nil PGP-Key)))
				subkeys)))
		   ((subkey-sig? (car pkts))
		    (liip (cdr pkts) (cons (car pkts) subkey-sigs) revoc-sigs))
		   (else
		    (liip (cdr pkts) subkey-sigs
			  (cons (car pkts) revoc-sigs))))))
	    (else
	     (values (reverse! subkeys) packets)))))
   
   (with-trace 2 "parse-key"
      (trace-item "packets=" (map find-runtime-type packets))
      (let ((main-key-packet (car packets)))
	 (receive (revocation-sigs remaining-packets)
	    (parse-revocation-sigs (cdr packets))
	    (receive (nuser-ids remaining-packets)
	       (parse-user-ids remaining-packets)
	       (when (and (null? nuser-ids) (null? user-ids))
		  (error 'parse-key
			 "At least one user ID is required"
			 #f))
	       (receive (subkeys remaining-packets)
		  (parse-subkeys remaining-packets)
		  (if (isa? main-key-packet PGP-Key-Packet)
		      (let* ((main-key (instantiate::PGP-Subkey
					  (key-packet main-key-packet)
					  (sigs '())
					  (revocation-sigs revocation-sigs)
					  (pgp-key (class-nil PGP-Key))))
			     (all-subkeys (cons main-key subkeys))
			     (res-key (instantiate::PGP-Key
					 (user-ids (append nuser-ids user-ids))
					 (subkeys all-subkeys))))
			 (for-each (lambda (skey)
				      (with-access::PGP-Subkey skey (pgp-key)
					 (set! pgp-key res-key)))
				   all-subkeys)
			 (values (list res-key) remaining-packets nuser-ids))
		      (values '() remaining-packets user-ids))))))))

;*---------------------------------------------------------------------*/
;*    parse-encrypted-message ...                                      */
;*---------------------------------------------------------------------*/
(define (parse-encrypted-message packets)
   (with-trace 2 "parse-encrypted-message"
      (let loop ((packets packets)
		 (session-keys '()))
	 (cond
	    ((null? packets)
	     (error 'parse-encrypted-message
		    "missing encrypted data packet"
		    #f))
	    ((isa? (car packets) PGP-Symmetrically-Encrypted-Packet)
	     (when (not (null? (cdr packets)))
		(warning "Packet after encrypted data discarded"))
	     (instantiate::PGP-Encrypted
		(session-keys (reverse! session-keys))
		(encrypted-data (car packets))))
	    ((isa? (car packets) PGP-MDC-Symmetrically-Encrypted-Packet)
	     (when (not (null? (cdr packets)))
		(warning "Packet after encrypted data discarded"))
	     (instantiate::PGP-Encrypted
		(session-keys (reverse! session-keys))
		(encrypted-data (car packets))))
	    ((isa? (car packets) PGP-Session-Key-Packet)
	     (loop (cdr packets)
		   (cons (car packets) session-keys)))
	    (else
	     (warning "unexpected packet encountered and discarded"
		      (car packets))
	     (loop (cdr packets) session-keys))))))

;*---------------------------------------------------------------------*/
;*    parse-signature ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-signature packets)
   (with-trace 2 "parse-signature"
      (let loop ((packets packets)
		 (sigs '()))
	 (cond
	    ((null? packets)
	     ;; detached signature
	     (instantiate::PGP-Signature
		(msg #f)
		(sigs (reverse! sigs))))
	    ((isa? (car packets) PGP-Compressed-Packet)
	     (with-access::PGP-Compressed-Packet (car packets) ((np packets))
		(loop (append np packets) sigs)))
	    ((isa? (car packets) PGP-Signature-Packet)
	     (loop (cdr packets) (cons (car packets) sigs)))
	    ((isa? (car packets) PGP-Literal-Packet)
	     (when (not (null? (cdr packets)))
		(warning "discarding packets after signature message"))
	     (instantiate::PGP-Signature
		(msg (car packets))
		(sigs (reverse! sigs))))
	    (else
	     (warning "unexpected packet encountered and discarded"
		(car packets))
	     (loop (cdr packets) sigs))))))

;*---------------------------------------------------------------------*/
;*    parse-one-pass-signature ...                                     */
;*---------------------------------------------------------------------*/
(define (parse-one-pass-signature packets)
   
   (define (same-sig? op-pkt pkt)
      (with-access::PGP-One-Pass-Signature-Packet op-pkt ((oi issuer)
							  (opka public-key-algo)
							  (ohash hash-algo)
							  (osig signature-type))
	 (with-access::PGP-Signature-Packet pkt (issuer
						   public-key-algo
						   hash-algo
						   signature-type)
	    (and (equal? oi issuer)
		 (eq? opka public-key-algo)
		 (eq? ohash hash-algo)
		 (eq? osig signature-type)))))
   
   (with-trace 2 "parse-one-pass-signature"
      (let loop ((packets packets)
		 (expect-one-pass? #t)
		 (one-pass-sigs '())
		 (msg #f)
		 (sigs '()))
	 (cond
	    ((null? packets)
	     (let ((one-pass-sigs (reverse! one-pass-sigs)))
		(when (not (=fx (length one-pass-sigs)
				(length sigs)))
		   (error 'parse-signature
			  "bad one-pass signature"
			  #f))
		(for-each (lambda (op-pkt sig-pkt)
			     (when (not (same-sig? op-pkt sig-pkt))
				(error 'parse-signature
				       "bad one-pass-signature"
				       #f)))
			  one-pass-sigs
			  sigs)
		(instantiate::PGP-One-Pass-Signature
		   (one-pass-sigs one-pass-sigs)
		   (msg msg)
		   (sigs sigs))))
	    ((isa? (car packets) PGP-Compressed-Packet)
	     (with-access::PGP-Compressed-Packet (car packets) ((np packets))
		(loop (append np packets)
		   expect-one-pass? one-pass-sigs msg sigs)))
	    ((and expect-one-pass?
		  (isa? (car packets) PGP-One-Pass-Signature-Packet))
	     (with-access::PGP-One-Pass-Signature-Packet (car packets)
		   (contains-nested-sig?)
		(loop (cdr packets)
		   contains-nested-sig?
		   (cons (car packets) one-pass-sigs)
		   msg
		   sigs)))
	    (expect-one-pass?
	     (error 'parse-signature
		    "bad one-pass signature"
		    #f))
	    ((and (not msg) (isa? (car packets) PGP-Literal-Packet))
	     (loop (cdr packets) #f one-pass-sigs (car packets) sigs))
	    ((not msg)
	     (error 'parse-signature
		    "bad one-pass signature"
		    #f))
	    ((isa? (car packets) PGP-Signature-Packet)
	     (loop (cdr packets) #f one-pass-sigs msg
		(cons (car packets) sigs)))
	    (else
	     (error 'parse-signature
		    "bad one-pass signature"
		    #f))))))

;*---------------------------------------------------------------------*/
;*    parse-literal ...                                                */
;*---------------------------------------------------------------------*/
(define (parse-literal packets)
   (when (not (null? (cdr packets)))
      (warning "discarding packets"))
   (when (or (null? packets)
	     (not (isa? (car packets) PGP-Literal-Packet)))
      (error 'parse-literal
	     "bad Literal"
	     #f))
   (instantiate::PGP-Literal
      (literal (car packets))))

;*---------------------------------------------------------------------*/
;*    create-chksum64 ...                                              */
;*---------------------------------------------------------------------*/
(define (create-chksum64::bstring data)
   (let* ((computed-chksum (crc-string 'radix-64-24 data
				       :init #xB704CE))
	  (b1 (bit-rsh (bit-and computed-chksum #xFF0000) 16))
	  (b2 (bit-rsh (bit-and computed-chksum #xFF00) 8))
	  (b3 (bit-and computed-chksum #xFF)))
      (base64-encode (string (integer->char-ur b1)
			     (integer->char-ur b2)
			     (integer->char-ur b3)))))

;*---------------------------------------------------------------------*/
;*    armored-pipe-port ...                                            */
;*---------------------------------------------------------------------*/
(define (armored-pipe-port p)
   
   ;; Sample Armored file:
   ;;
   ;; -----BEGIN PGP MESSAGE-----
   ;; Version: GnuPG v1.4.9 (GNU/Linux)
   ;;
   ;; kA0DAAIRVSc1kq6nngkBrA1iA3R0dEoIScVhYmNkiEYEABECAAYFAkoIScUACgkQ
   ;; VSc1kq6nngnB7wCbB8at7dhgxfUenOTojYVrSwGZVYAAoINkBGfMI2XNlkdzIgHj
   ;; 5oSsqZkr
   ;; =fpjg
   ;; -----END PGP MESSAGE-----
   ;;
   ;; will return values:
   ;;   - "PGP MESSAGE"
   ;;   - ("Version" "GnuPG v1.4.9 (GNU/Linux)")
   ;;   - a pipe-port decoding the base64 encoded string.

   (define (safe-read-line p)
      (let ((l (read-line p)))
	 (when (eof-object? l)
	    (error "read-armored" "unexpected end of file" #f))
	 l))
   
   (define (decode-header l)
      (let ((pos (string-index l #\:)))
	 (and pos
	      (list (substring l 0 pos)
		    (substring l (+fx pos 1) (string-length l))))))
   
   (define (verify-checksum data p)
      (define (chksum-error)
	 (error "read-armored" "bad checksum" #f))
      (let ((c (read-char p)))
	 (when (or (not (char? c))
		   (not (char=? c #\=)))
	    (chksum-error))
	 (let ((line (read-line p))
	       (expected (create-chksum64 data)))
	    (trace-item "checksum line=" line)
	    (trace-item "expected chksum=" expected)
	    (when (eof-object? line) (chksum-error))
	    (when (not (string=? line expected)) (chksum-error)))))
   
   (with-trace 1 "armored-pipe-port"
      (trace-item "p=" p)
      (let ((l (safe-read-line p)))
	 (when (not (and (string-prefix? "-----BEGIN" l)
			 (string-suffix? "-----" l)))
	    (error "read-armored" "not an armored file" l))
	 (trace-item "l=\"" l "\"")
	 (let ((main-header-info (substring l 11 (-fx (string-length l) 5))))
	    (let loop ((headers '()))
	       (let ((l (safe-read-line p)))
		  (if (string-null? l)
		      (values main-header-info headers
			      (let* ((p64 (base64-decode-pipe-port p))
				     (data (read-string p64)))
				 (trace-item "data-length=" (string-length data))
				 (verify-checksum data p)
				 (open-input-string data)))
		      (let ((header (decode-header l)))
			 (if header
			     (loop (cons header headers))
			     (loop headers))))))))))

;*---------------------------------------------------------------------*/
;*    decode-pgp ...                                                   */
;*---------------------------------------------------------------------*/
(define (decode-pgp p::input-port)
   (with-trace 1 "decode-pgp"
      (trace-item "p=" p)
      (let ((first-chars (read-chars 10 p)))
	 ;; push-back the read-chars.
	 (unread-string! first-chars p)
	 
	 (if (string=? "-----BEGIN" first-chars)
	     (receive (main-header-info headers composition)
		(decode-armored-pgp p)
		;; discard the headers. If the user wants them he has to call the
		;; armored function directly.
		composition)
	     (decode-native-pgp p)))))

;*---------------------------------------------------------------------*/
;*    decode-armored-pgp ...                                           */
;*---------------------------------------------------------------------*/
(define (decode-armored-pgp p::input-port)
   (with-trace 2 "decode-armored-pgp"
      (receive (main-header-info headers pp)
	 (armored-pipe-port p)
	 (unwind-protect
	    (values main-header-info headers (decode-pgp-content pp))
	    (close-input-port pp)))))

;*---------------------------------------------------------------------*/
;*    decode-native-pgp ...                                            */
;*---------------------------------------------------------------------*/
(define (decode-native-pgp p::input-port)
   (with-trace 2 "decode-native-pgp"
      (decode-pgp-content p)))

;*---------------------------------------------------------------------*/
;*    encode-pgp ::PGP-Composition ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (encode-pgp this::PGP-Composition p::output-port)
   (error 'encode-pgp
	  "Not yet implemented"
	  this))

;*---------------------------------------------------------------------*/
;*    encode-pgp ::PGP-Key ...                                         */
;*---------------------------------------------------------------------*/
(define-method (encode-pgp this::PGP-Key p::output-port)
   (with-access::PGP-Key this (user-ids subkeys)
      (let* ((main-key (car subkeys))
	     (other-subkeys (cdr subkeys)))
	 (with-access::PGP-Subkey main-key (revocation-sigs key-packet)
	    (encode-packet key-packet p)
	    (for-each (lambda (revoc-sig) (encode-packet revoc-sig p))
		      revocation-sigs)
	    (for-each (lambda (user-id)
			 (with-access::Signed-ID user-id (id sigs)
			    (encode-packet id p)
			    (for-each (lambda (sig)
					 (encode-packet sig p))
				      sigs)))
		      user-ids)
	    (for-each (lambda (subkey)
			 (with-access::PGP-Subkey subkey
			       (key-packet sigs revocation-sigs)
			    (encode-packet key-packet p)
			    (for-each (lambda (sig)
					 (encode-packet sig p))
				      sigs)
			    (for-each (lambda (revoc-sig)
					 (encode-packet revoc-sig p))
				      revocation-sigs)))
		      other-subkeys)))))

;*---------------------------------------------------------------------*/
;*    encode-pgp ::PGP-One-Pass-Signature ...                          */
;*---------------------------------------------------------------------*/
(define-method (encode-pgp this::PGP-One-Pass-Signature p::output-port)
   (with-access::PGP-One-Pass-Signature this (msg sigs one-pass-sigs)
      (for-each (lambda (ops)
		   (encode-packet ops p))
		(reverse one-pass-sigs))
      (encode-packet msg p)
      (for-each (lambda (sig)
		   (encode-packet sig p))
		sigs)))

;*---------------------------------------------------------------------*/
;*    encode-pgp ::PGP-Signature ...                                   */
;*---------------------------------------------------------------------*/
(define-method (encode-pgp this::PGP-Signature p::output-port)
   (with-access::PGP-Signature this (msg sigs)
      (for-each (lambda (sig)
		   (encode-packet sig p))
		sigs)
      (when msg (encode-packet msg p))))

;*---------------------------------------------------------------------*/
;*    encode-pgp ::PGP-Encrypted ...                                   */
;*---------------------------------------------------------------------*/
(define-method (encode-pgp this::PGP-Encrypted p::output-port)
   (with-access::PGP-Encrypted this (session-keys encrypted-data)
      (for-each (lambda (session-key)
		   (encode-packet session-key p))
		session-keys)
      (encode-packet encrypted-data p)))

;*---------------------------------------------------------------------*/
;*    encode-native-pgp ...                                            */
;*---------------------------------------------------------------------*/
(define (encode-native-pgp pgp-composition::PGP-Composition p::output-port)
   (encode-pgp pgp-composition p))

;*---------------------------------------------------------------------*/
;*    encode-armored-pgp ...                                           */
;*    -------------------------------------------------------------    */
;*    Counter part to 'decode-armored-pgp'.                            */
;*---------------------------------------------------------------------*/
(define (encode-armored-pgp pgp-composition::PGP-Composition
			    main-header-info::bstring
			    headers::pair-nil
			    p::output-port)
   (let ((pstr (open-output-string)))
      (encode-pgp pgp-composition pstr)
      (let* ((str (close-output-port pstr))
	     (b64-encoded (base64-encode str))
	     (crc-b64 (create-chksum64 str)))
	 (with-output-to-port p
	    (lambda ()
	       (print "-----BEGIN " main-header-info "-----")
	       (for-each (lambda (header)
			    (print (car header) ": " (cadr header)))
			 headers)
	       (print)
	       (print b64-encoded)
	       (display "=")
	       (print crc-b64)
	       (print "-----END " main-header-info "-----"))))))
