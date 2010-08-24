(module __openpgp-algo
   (library crypto)
   (import __openpgp-util
	   __openpgp-human)
   (export (hash-algo->procedure::procedure algo::symbol)
	   (symmetric-key-algo-key-bit-len::long algo::symbol)
	   (symmetric-key-algo-key-byte-len::long algo::symbol)
	   (symmetric-key-algo-block-bit-len::long algo::symbol)
	   (symmetric-key-algo-block-byte-len::long algo::symbol)
	   (symmetric-key-algo->procedure::procedure algo::symbol encrypt?::bool)))

(define (hash-algo->procedure algo)
   (case algo
      ((md5) md5sum-bin)
      ((sha-1) sha1sum-bin)
      ((sha-256) sha256sum-bin)
      (else (error "hash-algo->procedure"
		   "algorithm not implemented"
		   (hash-algo->human-readable algo)))))

(define (symmetric-key-algo-key-byte-len::long algo)
   (/fx (symmetric-key-algo-key-bit-len algo) 8))

(define (symmetric-key-algo-key-bit-len::long algo)
   (case algo
      ((unencrypted) 0)
      ((idea) 128)
      ((des3) 192)
      ((cast5) 128)
      ((blowfish) 128)
      ((safer-sk128) 128)
      ((aes-128) 128)
      ((aes-192) 192)
      ((aes-256) 256)
      (else
       (error "symmetric-key-algo-key-bit-len"
	      "don't know ke size"
	      (symmetric-key-algo->human-readable algo)))))

(define (symmetric-key-algo-block-byte-len::long algo)
   (/fx (symmetric-key-algo-block-bit-len algo) 8))

(define (symmetric-key-algo-block-bit-len::long algo)
   (case algo
      ((unencrypted) 0)
      ((idea) 64)
      ((des3) 64)
      ((cast5) 64)
      ((blowfish) 64)
      ((safer-sk128) 128)
      ((aes-128) 128)
      ((aes-192) 128)
      ((aes-256) 128)
      (else
       (error "symmetric-key-algo-block-bit-len"
	      "don't know ke size"
	      (symmetric-key-algo->human-readable algo)))))

(define (id x) x)
(define (idea-encrypt str IV pwd)
   (encrypt-string 'idea str pwd :IV IV :mode 'cfb :string->key id))
(define (des3-encrypt str IV pwd)
   (encrypt-string 'des3 str pwd :IV IV :mode 'cfb :string->key id))
(define (cast5-encrypt str IV pwd)
   (encrypt-string 'cast-128 str pwd :IV IV :mode 'cfb :string->key id))
(define (aes-encrypt str IV pwd)
   (encrypt-string 'aes str pwd :IV IV :mode 'cfb :string->key id))
(define (idea-decrypt str IV pwd)
   (decrypt-string 'idea str pwd :IV IV :mode 'cfb :string->key id))
(define (des3-decrypt str IV pwd)
   (debug "calling DES3")
   (debug "str: " (str->hex-string str) " " (string-length str))
   (debug "IV: " (str->hex-string IV) " " (string-length IV))
   (debug "pwd: " (str->hex-string pwd) " " (string-length pwd))
   (decrypt-string 'des3 str pwd :IV IV :mode 'cfb :string->key id))
(define (cast5-decrypt str IV pwd)
   (decrypt-string 'cast-128 str pwd :IV IV :mode 'cfb :string->key id))
(define (aes-decrypt str IV pwd)
   (decrypt-string 'aes str pwd :IV IV :mode 'cfb :string->key id))

(define (symmetric-key-algo->procedure algo encrypt?)
   (if encrypt?
       (case algo
	  ((unencrypted) (lambda (x . L) x))
	  ((idea) idea-encrypt)
	  ((des3) des3-encrypt)
	  ((cast5) cast5-encrypt)
	  ;((blowfish) "Blowfish (128 bit key, 16 rounds) [BLOWFISH]")
	  ;((safer-sk128) "SAFER-SK128 (13 rounds) [SAFER]")
	  ;((des/sk) "Reserved for DES/SK")
	  ((aes-128 aes-192 aes-256) aes-encrypt)
	  ;((twofish) "Twofish GPG-extension?")
	  ;((100 101 102 103 104 105 106 107 108 109 110)
	  ;    "Private/Experimental algorithm")
	  (else (error "symmetric-key-algo->procedure"
		       "Algorithm not yet implemented"
		       (symmetric-key-algo->human-readable algo))))
       (case algo
	  ((unencrypted) (lambda (x . L) x))
	  ((idea) idea-decrypt)
	  ((des3) des3-decrypt)
	  ((cast5) cast5-decrypt)
	  ;((blowfish) "Blowfish (128 bit key, 16 rounds) [BLOWFISH]")
	  ;((safer-sk128) "SAFER-SK128 (13 rounds) [SAFER]")
	  ;((des/sk) "Reserved for DES/SK")
	  ((aes-128 aes-192 aes-256) aes-decrypt)
	  ;((100 101 102 103 104 105 106 107 108 109 110)
	  ;    "Private/Experimental algorithm")
	  (else (error "symmetric-key-algo->procedure"
		       "Algorithm not yet implemented"
		       (symmetric-key-algo->human-readable algo))))))
