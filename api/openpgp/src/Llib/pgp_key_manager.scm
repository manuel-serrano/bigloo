;; simple manager implementation.
;; looses all keys at shutdown.
(module __openpgp-key-manager
   (import __openpgp-util
	   __openpgp-packets
	   __openpgp-composition
	   __openpgp-logic)
   (export (pgp-key?::bool key)
	   (pgp-subkeys::pair-nil key)
	   (pgp-subkey?::bool subkey)
	   (pgp-key->string key)
	   (pgp-subkey->string subkey)

	   (pgp-key-id::bstring subkey)
	   (pgp-key-fingerprint::bstring subkey)

	   (pgp-make-key-db)
	   (pgp-add-key-to-db db key)
	   (pgp-add-keys-to-db db keys::pair-nil)
	   (pgp-resolve-key::pair-nil db id::bstring)
	   (pgp-db-print-keys db)))

(define (pgp-key? key) (isa? key PGP-Key))
(define (pgp-subkey? subkey) (isa? subkey PGP-Subkey))
;; returns a list of (id . subkey) for a given key.
(define (pgp-subkeys key)
   (unless (isa? key PGP-Key)
      (error "pgp-subkeys" "Expected PGP Key" key))
   (with-access::PGP-Key key (subkeys) subkeys))

(define (pgp-key->string key)
   (unless (isa? key PGP-Key)
      (error "pgp-key->string" "Expected PGP Key" key))
   (pgp-key->human-readable key))

(define (pgp-subkey->string subkey)
   (unless (isa? subkey PGP-Subkey)
      (error "pgp-subkey->string" "Expected PGP Subkey" subkey))
   (pgp-subkey->human-readable subkey))


(define (pgp-key-id::bstring subkey)
   (unless (isa? subkey PGP-Subkey)
      (error "pgp-key-id" "Expected PGP-Subkey" subkey))
   (with-access::PGP-Subkey subkey (key-packet)
      (key-id key-packet)))

(define (pgp-key-fingerprint::bstring subkey)
   (unless (isa? subkey PGP-Subkey)
      (error "pgp-key-id" "Expected PGP-Subkey" subkey))
   (with-access::PGP-Subkey subkey (key-packet)
      (fingerprint key-packet)))

(define (pgp-make-key-db) (list '*pgp-keys*))

(define (pgp-add-keys-to-db db keys::pair-nil)
   (for-each (lambda (key) (pgp-add-key-to-db db key)) keys))

(define (pgp-add-key-to-db db key)
   ;; TODO verify key (or do that before)
   ;; TODO merge if the key is already in there.
   (when (not (isa? key PGP-Key))
      (error 'add-key-to-db
	     "Expected PGP Key"
	     key))
   (when (not (and (pair? db)
		   (eq? (car db) '*pgp-keys*)))
      (error 'add-key-to-db
	     "Expected pgp-key db"
	     db))
   (set-cdr! db (cons key (cdr db))))

(define (valid-subkey k)
   (with-access::PGP-Subkey k (revocation-sigs)
      (null? revocation-sigs)))

;; simply extract all main/subkeys that match the id.
;; the returned list will contain (k::PGP-Key . k::PGP-Key-Packet)
;; However the only guarantee is that the cdr resolves to the given id.
;;
;; If the id is 0 ("00000000") then all keys should be returned.
(define (pgp-resolve-key db id::bstring)
   (when (not (and (pair? db)
		   (eq? (car db) '*pgp-keys*)))
      (error 'add-key-to-db
	     "Expected pgp-key db"
	     db))

   (let loop ((keys (cdr db))
	      (matching '()))
      (if (null? keys)
	  matching
	  (let* ((k (car keys))
		 (valid-subkeys (filter valid-subkey
				   (with-access::PGP-Key k (subkeys)
				      subkeys)))
		 (k-matching (filter (lambda (k)
					(with-access::PGP-Subkey k (key-packet)
					   ;; 00000000 is wild-card key.
					   (or (string=? id "00000000")
					       (string=? (key-id key-packet) id))))
				     valid-subkeys)))
	     (loop (cdr keys)
		   (append k-matching matching))))))

(define (pgp-db-print-keys db)
   (when (not (and (pair? db)
		   (eq? (car db) '*pgp-keys*)))
      (error 'add-key-to-db
	     "Expected pgp-key db"
	     db))

   (define (print-key k::PGP-Key)
      (print (pgp-key->human-readable k)))

   (for-each (lambda (k)
		(print-key k)
		(print)
		(print))
	     (cdr db)))
