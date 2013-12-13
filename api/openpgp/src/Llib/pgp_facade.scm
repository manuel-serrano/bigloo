(module __openpgp-facade
   (option (set! *dlopen-init-gc* #t))
   (import __openpgp-logic
	   __openpgp-composition
	   __openpgp-packets
	   __openpgp-enums
	   __openpgp-util
	   __openpgp-s2k
	   __openpgp-algo
	   __openpgp-human)
   (export
    (pgp-read-string str::bstring)
    (pgp-read-port iport::input-port)
    (pgp-read-file file::bstring)
    (pgp-write-string composition #!key (format 'armored))
    (pgp-write-port oport::output-port composition #!key (format 'armored))
    (pgp-write-file file::bstring composition #!key (format 'armored))
    (pgp-sign msg::bstring key password-provider
	      #!key (detached-signature? #t) (one-pass? #t) (hash-algo 'sha-1))
    (pgp-verify::pair-nil signature key-manager::procedure
			  #!optional (msg #f))
    (pgp-signature-message signature)
    (pgp-password-encrypt msg::bstring password::bstring
			  #!key (hash-algo 'sha-1)
			  (symmetric-algo 'cast5)
			  (mdc #t))
    (pgp-decrypt encrypted #!key
		 (passkey-provider (lambda () #f))
		 (password-provider (lambda (key) #f))
		 (key-manager (lambda (key) '()))
		 (hash-algo 'sha-1)
		 (symmetric-algo 'cast5))
    (pgp-encrypt msg::bstring keys::pair-nil
		 passwords::pair-nil
		 #!key
		 (hash-algo 'sha-1)
		 (symmetric-algo 'cast5))))

(define *bigloo-version* (bigloo-config 'release-number))

(define (pgp-read-string str::bstring)
   (pgp-read-port (open-input-string str)))

(define (pgp-read-port iport::input-port)
   (decode-pgp iport))

(define (pgp-read-file file::bstring)
   (with-trace 1 "pgp-read-file"
      (trace-item "file=" file)
      (let ((p (open-input-file file)))
	 (when (not p)
	    (error "pgp-read-file"
		   "Couldn't open file"
		   file))
	 (unwind-protect
	    (decode-pgp p)
	    (close-input-port p)))))

(define (pgp-write-string composition #!key (format 'armored))
   (let ((p (open-output-string)))
      (pgp-write-port p composition :format format)
      (close-output-port p)))

(define global-format format)

(define (pgp-write-port oport::output-port composition #!key (format 'armored))
   (when (not (isa? composition PGP-Composition))
      (error "pgp-write-port"
	     "Expected PGP Composition"
	     composition))
   (if (eq? format 'armored)
       (let ((main-header-info (if (and (isa? composition PGP-Signature)
					(with-access::PGP-Signature composition
					      (msg)
					   (not msg)))
				   "PGP SIGNATURE"
				   "PGP MESSAGE")))
	  (encode-armored-pgp composition
			      main-header-info
			      `((Version ,(global-format "Bigloo ~a"
							 *bigloo-version*)))
			      oport))
       (encode-native-pgp composition oport)))

(define (pgp-write-file file::bstring composition #!key (format 'armored))
   (let ((p (open-output-file file)))
      (unwind-protect
	 (pgp-write-port p composition :format format)
	 (close-output-port p))))

(define (extract-subkey key encryption?)
   (define (good-for-encryption? key::PGP-Subkey)
      (with-access::PGP-Subkey key (key-packet)
	 (with-access::PGP-Key-Packet key-packet (algo)
	    (memq algo
	       '(rsa-encrypt/sign rsa-encrypt elgamal-encrypt
		 elgamal-encrypt/sign)))))
   (define (good-for-signature? key::PGP-Subkey)
      (with-access::PGP-Subkey key (key-packet)
	 (with-access::PGP-Key-Packet key-packet (algo)
	    (memq algo '(rsa-encrypt/sign rsa-sign dsa elgamal-encrypt/sign)))))

   (cond
      ((isa? key PGP-Subkey)
       key)
      ((not encryption?)
       ;; we assume that the main-key is used for signature.
       (with-access::PGP-Key key (subkeys)
	  (let ((main-key (car subkeys)))
	     (when (not (good-for-signature? main-key))
		(error "extract-subkey"
		   "Couldn't find suitable key for signature."
		   #f))
	     main-key)))
      (else
       (with-access::PGP-Key key (subkeys)
	  (cond
	     ((null? subkeys)
	      ;; can this happen?
	      (error "extract-subkey"
		     "Couldn't find subkey"
		     #f))
	     ((null? (cdr subkeys))
	      (when (not (good-for-encryption? (car subkeys)))
		 (error "extract-subkey"
			"Couldn't find subkey for encryption"
			#f))
	      (car subkeys))
	     ((and (null? (cdr (cdr subkeys)))
		   (good-for-encryption? (cadr subkeys)))
	      ;; prefer non-mainkey for encryption
	      (cadr subkeys))
	     (else
	      (let ((possible-subkeys (filter good-for-encryption?
					      subkeys)))
		 (cond
		    ((null? possible-subkeys)
		     (error "extract-subkey"
			    "Couldn't find suitable subkey"
			    key))
		    ((not (null? (cdr possible-subkeys)))
		     (error "extract-subkey"
			    "Found more than one suitable subkey."
			    (map (lambda (subkey)
				    (with-access::PGP-Subkey subkey (key-packet)
				       (str->hex-string (key-id key-packet))))
				 possible-subkeys)))
		    (else
		     (car possible-subkeys))))))))))

;; key is either a PGP-Key or a PGP-Subkey. In either case it is assumed to
;; be valid. (no revocationg-checks, etc are performed here).
;; password-provider is used when the given key has not yet been
;; "opened", and needs a password. It receives always a Subkey.
(define (pgp-sign msg::bstring key password-provider
		  #!key (detached-signature? #t)
		  (one-pass? #t)
		  (hash-algo 'sha-1))
   (with-trace 2 "pgp-sign"
      (trace-item (if detached-signature? "Detached Signature" "Attached Signature"))
      (cond
	 ((isa? key PGP-Key)
	  (pgp-sign msg
		    (extract-subkey key #f)
		    password-provider
		    :detached-signature? detached-signature?
		    :one-pass? one-pass?
		    :hash-algo hash-algo))
	 ((isa? key PGP-Subkey)
	  (cond
	     (detached-signature?
	      (create-pgp-signature msg key
				    :password-provider password-provider
				    :hash-algo hash-algo
				    :detached-signature? #t))
	     (one-pass?
	      (create-one-pass-signature msg key
					 :password-provider password-provider
					 :hash-algo hash-algo))
	     (else
	      (create-pgp-signature msg key
				    :password-provider password-provider
				    :hash-algo hash-algo
				    :detached-signature? #f))))
	 (else
	  (error "pgp-sign" "Bad Key" key)))))

;; key-manager must return a list of all subkeys that match a given key-id.
;; the optional msg parameter will only be used for detached signatures.
;; the result contains a list of keys which matched the signature.
(define (pgp-verify::pair-nil signature key-manager::procedure
			      #!optional (msg #f))
   (when (not (isa? signature PGP-Signature))
      (error "pgp-verify" "not a signature" signature))
   (verify-pgp-signature signature key-manager msg))


;; returns the signature's message, or #f if there is non in the composition.
(define (pgp-signature-message signature)
   (when (not (isa? signature PGP-Signature))
      (error "pgp-verify" "not a signature" signature))
   (let ((sig-msg (with-access::PGP-Signature signature (msg) msg)))
      (and sig-msg
	   (with-access::PGP-Literal-Packet sig-msg (data)
	      data))))

;; Usage of pgp-password-encrypt is deprecated, since it does not encode the
;; used algorithm (and it has no anti-tampering mechanisms). It might however
;; be convenient for non-public usage.
;; If size does not matter, consider using pgp-encrypt instead.
(define (pgp-password-encrypt msg::bstring password::bstring
			      #!key
			      (hash-algo 'sha-1)
			      (symmetric-algo 'cast5)
			      (mdc #t))
   (let* ((literal-packet (instantiate::PGP-Literal-Packet
			     (format 'binary)
			     (for-your-eyes-only? #f)
			     (file-name "") ; (or file-name ""))
			     (creation-date (current-date))
			     (data msg)))
	  (algo-key-len (symmetric-key-algo-key-byte-len symmetric-algo))
	  (packet (symmetric-encrypt literal-packet
				     (simple-s2k password algo-key-len sha1sum-bin)
				     symmetric-algo
				     :mdc mdc)))
      (instantiate::PGP-Encrypted
	 (session-keys '())
	 (encrypted-data packet))))

(define (pubkey-decrypt encrypted::PGP-Symmetrically-Encrypted-Packet
			pubkey-session-packets key-manager password-provider)
   (define (try-decrypt session-packet subkey)
      (with-handler
	 (lambda (e)
	    (trace-item "Exception: " e)
	    #f)
	 (receive (algo key-string)
	    (decrypt-public-key-session-key session-packet subkey password-provider)
	    (trace-item "public-key-session-key decription succeeded, len="
	       (with-access::PGP-Symmetrically-Encrypted-Packet encrypted (data)
		  (string-length data)))
	    (symmetric-decrypt encrypted key-string algo))))

   (with-trace 2 "pubkey-decrypt"
      (if (and (procedure? key-manager) (correct-arity? key-manager 1))
	  (unless (null? pubkey-session-packets)
	     (let* ((pack (car pubkey-session-packets))
		    (key-id (with-access::PGP-Public-Key-Encrypted-Session-Key-Packet pack (id) id))
		    (subkeys (or (key-manager key-id) '())))
		(for-each (lambda (k)
			     (trace-item "key=" (pgp-subkey->human-readable k)))
			  subkeys)
		(or (any (lambda (subkey) (try-decrypt pack subkey)) subkeys)
		    (pubkey-decrypt encrypted (cdr pubkey-session-packets)
				    key-manager password-provider))))
	  (error "decrypt" "Illegal key-manager" key-manager))))

(define (pwd-decrypt encrypted::PGP-Symmetrically-Encrypted-Packet
		     password-session-packets passkey-provider)
   (define (try-decrypt session-packet passkey)
      (with-handler
	 (lambda (e)
	    (trace-item "symmetric-key-session-key decryption failed")
	    #f)
	 (receive (algo key-string)
	    (decrypt-symmetric-key-session-key session-packet passkey)
	    (trace-item "symmetric-key-session-key decription succeeded")
	    (symmetric-decrypt encrypted key-string algo))))

   (with-trace 2 "pwd-decrypt"
      (if (and (procedure? passkey-provider) (correct-arity? passkey-provider 0))
	  (unless (null? password-session-packets)
	     (let ((passkey (passkey-provider)))
		(any (lambda (packet) (try-decrypt packet passkey))
		     password-session-packets)))
	  (error "decrypt" "Illegal passkey-provider" passkey-provider))))

;*---------------------------------------------------------------------*/
;*    pgp-decrypt ...                                                  */
;*---------------------------------------------------------------------*/
(define (pgp-decrypt encrypted
		     #!key
		     (passkey-provider (lambda () #f))
		     (password-provider (lambda (key) #f))
		     (key-manager (lambda (key) '()))
		     (hash-algo 'sha-1)
		     (symmetric-algo 'cast5))

   (when (not (isa? encrypted PGP-Encrypted))
      (error "pgp-decrypt" "Expected PGP-composition." encrypted))

   (with-trace 2 "pgp-decrypt"
      (with-access::PGP-Encrypted encrypted (session-keys encrypted-data)
	 (with-access::PGP-Symmetrically-Encrypted-Packet encrypted-data (data)
	    (trace-item "encrypted length=" (string-length data)))
	 (when (not (pair? session-keys))
	    (trace-item "No session-key. Create a default one."))
	 
	 ;; if no session-key is inside the composition then
	 ;; a default one is created.
	 ;; rfc2440 specifies hash:md5 and symmetric-algo:idea, but gpg uses sha-1
	 ;; and cast5... In any case the parameters can be changed.
	 ;; in rfc4880 this use (no session-key) is deprecated.
	 (let* ((skeys (if (pair? session-keys)
			   session-keys
			   (list
			    (instantiate::PGP-Symmetric-Key-Encrypted-Session-Key-Packet
			       (version 4)
			       (algo symmetric-algo)
			       (s2k (make-s2k 'simple
					      hash-algo
					      #f #f))
			       (encrypted-session-key #f)))))
		(pubkey-session-packets
		 (filter (lambda (x)
			    (isa? x PGP-Public-Key-Encrypted-Session-Key-Packet))
			 skeys))
		(password-session-packets
		 (filter (lambda (x)
			    (isa? x PGP-Symmetric-Key-Encrypted-Session-Key-Packet))
			 skeys)))
	    ;; first try the public keys, then only the password.
	    (let* ((decrypted (or (pubkey-decrypt encrypted-data
						  pubkey-session-packets
						  key-manager password-provider)
				  (pwd-decrypt encrypted-data
					       password-session-packets
					       passkey-provider))))
	       (when (and (pair? decrypted)
			  (isa? (car decrypted) PGP-Compressed-Packet))
		  (with-access::PGP-Compressed-Packet (car decrypted) (packets)
		     (set! decrypted packets)))
	       (trace-item "decrypted=" decrypted)
	       (cond
		  ((not decrypted)
		   (trace-item "decryption failed")
		   #f)
		  ((and (null? decrypted) (not (pair? decrypted)))
		   (trace-item "no encrypted data.")
		   (error "pgp-decrypt" "No or bad encrypted data" #f))
		  ((isa? (car decrypted) PGP-Literal-Packet)
		   (when (not (null? (cdr decrypted)))
		      (warning "ignoring trailing packet(s)"))
		   (with-access::PGP-Literal-Packet (car decrypted)
			 (format for-your-eyes-only? file-name creation-date data)
		      (trace-item "format: " format " " (literal-format->human-readable format))
		      (trace-item "file: " file-name)
		      (trace-item "creation-date: " creation-date)
		      data))
		  ((and (isa? (car decrypted) PGP-Sig-Packet)
			(pair? (cdr decrypted))
			(isa? (cadr decrypted) PGP-Literal-Packet))
;* 		   (when (not (null? (cddr decrypted)))                */
;* 		      (warning "ignoring trailing packet(s)"))         */
		   (with-access::PGP-Literal-Packet (cadr decrypted)
			 (format for-your-eyes-only? file-name creation-date data)
		      (trace-item "format: " format " " (literal-format->human-readable format))
		      (trace-item "file: " file-name)
		      (trace-item "creation-date: " creation-date)
		      data))
		  (else
		   (error "pgp-decrypt"
			  "Don't know what to do with decrypted data"
			  #f))))))))

;; for simplicity we use the symmetric-algo for both the password encryption
;; and the data encryption. Should be fairly easy to change.
(define (pgp-encrypt msg::bstring keys::pair-nil passwords::pair-nil #!key
		     (hash-algo 'sha-1)
		     (symmetric-algo 'cast5))

   (define (encrypt-and-compose session-key::bstring
				session-key-packets::pair-nil)
      (let* ((literal-packet (instantiate::PGP-Literal-Packet
				(format 'binary)
				(for-your-eyes-only? #f)
				(file-name "") ; (or file-name ""))
				(creation-date (current-date))
				(data msg)))
	     (encrypted-packet (symmetric-encrypt literal-packet
						  session-key
						  symmetric-algo)))
	 (instantiate::PGP-Encrypted
	    (session-keys session-key-packets)
	    (encrypted-data encrypted-packet))))

   (with-trace 2 "pgp-encrypt"
      (when (not (symbol? hash-algo))
	 (error "pgp-encrypt"
		"Expected symbol as hash algorithm"
		hash-algo))
      (when (not (symbol? symmetric-algo))
	 (error "pgp-encrypt"
		"Expected symbol as symmetric key algorithm"
		symmetric-algo))
   
      (if (and (null? keys)
	       (pair? passwords)
	       (null? (cdr passwords)))
	  ;; only one password. Use its s2k as key.
	  (let ((data-symmetric-algo symmetric-algo))
	     (receive (session-key session-key-packet)
		(create-password-session-key-packet (car passwords) #f
						    ;; The symmetric algo used to
						    ;; encrypt the data.
						    data-symmetric-algo
						    :hash-algo hash-algo)
		(encrypt-and-compose session-key (list session-key-packet))))
	  (let* ((session-key-len
		  (symmetric-key-algo-key-byte-len symmetric-algo))
		 (session-key (make-random-string session-key-len)))
	     (trace-item "Shared session key: " (str->hex-string session-key))
	     (let loop ((keys keys)
			(passwords passwords)
			(session-key-packets '()))
		(cond
		   ((and (null? keys)
			 (null? passwords))
		    (encrypt-and-compose session-key session-key-packets))
		   ((pair? keys)
		    (let* ((public-key (car keys))
			   (public-subkey (extract-subkey public-key #t))
			   (session-packet (create-public-key-session-key-packet
					    public-subkey
					    session-key
					    symmetric-algo)))
		       (loop (cdr keys)
			     passwords
			     (cons session-packet session-key-packets))))
		   (else
		    (let* ((password (car passwords))
			   (session-packet (create-password-session-key-packet
					    password
					    session-key
					    ;; The symmetric algo used to encrypt
					    ;; the data.
					    symmetric-algo
					    :hash-algo hash-algo
					    ;; The symmetric algo used to encrypt
					    ;; the password.
					    :symmetric-algo symmetric-algo)))
		       (loop keys
			     (cdr passwords)
			     (cons session-packet session-key-packets))))))))))
