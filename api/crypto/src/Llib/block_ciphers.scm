(module __crypto-block-ciphers
   (import __crypto-util
	   __crypto-cipher-padding
	   __crypto-string2key)
   (export
    (final-class Block-Cipher
       (name::bstring read-only)
       (block-size::long read-only) ;; in byte.
       (preferred-key-length::long read-only)
       (encrypt!::procedure read-only)
       (decrypt!::procedure read-only)
       (key->encrypt-param::procedure read-only)
       (key->decrypt-param::procedure read-only))
    
    (register-cipher! lookup-name::symbol desc::Block-Cipher)
    (block-cipher-description::Block-Cipher lookup-name::symbol)

    (encrypt::bstring cipher::symbol plain password::bstring
		      #!key (IV #f) (mode 'cfb) (pad 'none)
		      (nonce-init! #f) (nonce-update! #f)
		      (string->key #f))
    (encrypt-string::bstring cipher::symbol plaintext::bstring
			     password::bstring
			     #!key (IV #f) (mode 'cfb) (pad 'none)
			     (nonce-init! #f) (nonce-update! #f)
			     (string->key #f))
    (encrypt-mmap::bstring cipher::symbol plaintext::mmap
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
    (encrypt-port::bstring cipher::symbol plaintext::input-port
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
    (encrypt-file::bstring cipher::symbol filename::bstring
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
    (encrypt-sendchars cipher::symbol in::input-port out::output-port
		       password::bstring
		       #!key (IV #f) (mode 'cfb) (pad 'none)
		       (nonce-init! #f) (nonce-update! #f)
		       (string->key #f))
    (decrypt::bstring cipher::symbol ciphertext password::bstring
		      #!key (IV #f) (mode 'cfb) (pad 'none)
		      (nonce-init! #f) (nonce-update! #f)
		      (string->key #f))
    (decrypt-string::bstring cipher::symbol
			     ciphertext::bstring password::bstring
			     #!key (IV #f) (mode 'cfb) (pad 'none)
			     (nonce-init! #f) (nonce-update! #f)
			     (string->key #f))
    (decrypt-mmap::bstring cipher::symbol
			   ciphertext::mmap password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
    (decrypt-port::bstring cipher::symbol
			   ciphertext::input-port
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
    (decrypt-file::bstring cipher::symbol filename::bstring
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
    (decrypt-sendchars cipher::symbol in::input-port out::output-port
		       password::bstring
		       #!key (IV #f) (mode 'cfb) (pad 'none)
		       (nonce-init! #f) (nonce-update! #f)
		       (string->key #f)))
   (static (abstract-class Cipher-State
	      (block-size::long read-only)
	      (cipher::procedure)
	      (param read-only))
	   (final-class ECB-Encrypt::Cipher-State)
	   (final-class ECB-Decrypt::Cipher-State)
	   (final-class CBC-Encrypt::Cipher-State
	      (buffer::bstring read-only)
	      (chain::bstring read-only))
	   (final-class CBC-Decrypt::Cipher-State
	      buffer::bstring
	      chain::bstring)
	   (final-class PCBC-Encrypt::Cipher-State
	      (buffer::bstring read-only)
	      (chain::bstring read-only))
	   (final-class PCBC-Decrypt::Cipher-State
	      (buffer::bstring read-only)
	      (chain::bstring read-only))

	   (abstract-class Stream-Cipher-State::Cipher-State)
	   (final-class CFB-Encrypt::Stream-Cipher-State
	      (chain::bstring read-only))
	   (final-class CFB-Decrypt::Stream-Cipher-State
	      (buffer::bstring read-only)
	      (chain::bstring read-only))
	   (final-class OFB::Stream-Cipher-State ;; both encrypt and decrypt
	      (chain::bstring read-only))
	   (final-class CTR::Stream-Cipher-State ;; both encrypt and decrypt
	      nonce
	      (nonce-str::bstring read-only)
	      block-counter::long 
	      (nonce-init!::procedure read-only)
	      (nonce-update!::procedure read-only))

	   (final-class Cipher-Mode-State
	      (encrypt?::bool read-only)
	      (block-size::long read-only)
	      (cipher-state::Cipher-State read-only)
	      ;; IV-action: 'init 'read 'write 'done
	      (IV-action::symbol (default 'unspecified))
	      (IV (default #f))
	      (pad/unpad read-only)
	      (loop-buffer::bstring read-only)
	      ;; for streams only.
	      ;; this can also be the position inside the IV!
	      (pos::long (default 0))) ;; position inside block
	   
	   ))

(define (substring-xor-buffer! to to-at str1 str1-at str2 str2-at len)
   (let loop ((i 0))
      (unless (=fx i (+fx to-at len))
	 (string-set! to (+fx to-at i)
		      (char-xor (string-ref str1 (+fx str1-at i))
				(string-ref str2 (+fx str2-at i))))
	 (loop (+fx i 1)))))

;; blit-string-unchecked-range
;; currently not faster, but in theory one could optimize here.
#;(define-inline (blit-string-ur! from from-pos to to-pos len)
   (let loop ((i 0))
      (unless (=fx i len)
	 (string-set! to (+fx to-pos i)
		      (string-ref from (+fx from-pos i)))
	 (loop (+fx i 1)))))
(define (read-string-at! str str-pos to-str to-pos len)
   (let* ((str-len (string-length str)))
      (if (<= (+fx str-pos len) str-len)
	  (begin
	     (blit-string! str str-pos to-str to-pos len)
	     len)
	  (let ((remaining-len (-fx str-len str-pos)))
	     (blit-string! str str-pos to-str to-pos remaining-len)
	     remaining-len))))
(define (read-mmap-at! mmap mmap-pos to-str to-pos len)
   (let ((mmap-len (mmap-length mmap)))
      (cond
	 ((=fx len 0) 0)
	 ((>fx (+fx mmap-pos len) mmap-len)
	  (read-mmap-at! mmap mmap-pos to-str to-pos (-fx mmap-len mmap-pos)))
	 (else
	  (let loop ((i 0))
	     (if (=fx i len)
		 len
		 (begin
		    (string-set! to-str (+fx to-pos i)
				 (mmap-ref mmap (+fx mmap-pos i)))
		    (loop (+fx i 1)))))))))
(define (read-port-at! p p-pos to-str to-pos len)
   (let ((n (read-fill-string! to-str to-pos len p)))
      (if (eof-object? n)
	  0
	  n)))

(define (write-port-at! from from-pos p p-pos len)
   (let loop ((i 0))
      (unless (=fx i len)
	 (write-char (string-ref from (+fx from-pos i)) p)
	 (loop (+fx i 1)))))

(define (current-seconds-nonce size)
   (let ((res (make-string size #a000))
	 (nonce (current-seconds)))
      (let loop ((i 0)
		 (nonce nonce))
	 (unless (=fx i 4)
	    (string-set! res i (integer->char-ur
				(elong->fixnum
				 (bit-andelong nonce #exFF))))
	    (loop (+fx i 1)
		  (bit-rshelong nonce 8))))
      res))
(define (default-nonce-init! str::bstring nonce)
   (blit-string-ur! nonce 0 str 0 (minfx (string-length str)
					   (string-length nonce))))
(define (default-nonce-update! str::bstring nonce i)
   (let loop ((i (-fx (string-length str) 1)))
      (when (>=fx i 0)
	 (let ((c (char->integer (string-ref str i))))
	    (if (=fx c 255)
		(begin
		   (string-set! str i #a000)
		   (loop (-fx i 1)))
		(string-set! str i (integer->char-ur (+fx c 1))))))))

(define-generic (do-cipher-IV-init! state::Cipher-State IV)
   (error 'cipher-IV-init
	  "Internal Error: Could not find method for state"
	  state))
(define-generic (do-cipher-block! state::Cipher-State
				  from::bstring from-pos::long
				  to::bstring to-pos::long)
   (error 'en/decrypt-block
	  "Internal Error: Could not find method for state"
	  state))
(define-generic (do-cipher-partial-block! state::Cipher-State
					  from::bstring
					  from-block-pos::long
					  to::bstring to-block-pos::long
					  ;; at and len are inside the block
					  at::long len::long)
   (error 'en/decrypt-partial-block
	  "Internal Error: Could not find method for state"
	  state))

;; ------------- ECB -------------
(define (create-ECB-state en/decrypt::procedure param block-size::long encrypt?)
   (if encrypt?
       (instantiate::ECB-Encrypt
	  (cipher en/decrypt)
	  (param param)
	  (block-size block-size))
       (instantiate::ECB-Decrypt
	  (cipher en/decrypt)
	  (param param)
	  (block-size block-size))))
(define-method (do-cipher-block! state::ECB-Encrypt from from-pos to to-pos)
   (with-access::ECB-Encrypt state (cipher param)
      (cipher from from-pos to to-pos param)))
(define-method (do-cipher-block! state::ECB-Decrypt from from-pos to to-pos)
   (with-access::ECB-Decrypt state (cipher param)
      (cipher from from-pos to to-pos param)))

;; ------------- CBC -------------
;; ----Encryption
(define (create-CBC-encryption-state encrypt::procedure param block-size::long)
   (instantiate::CBC-Encrypt
      (cipher encrypt)
      (param param)
      (block-size block-size)
      (buffer (make-string block-size))
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::CBC-Encrypt IV)
   (with-access::CBC-Encrypt state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::CBC-Encrypt from from-pos to to-pos)
   (with-access::CBC-Encrypt state (buffer chain cipher block-size param)
      (string-xor-buffer! buffer 0 chain 0 from from-pos block-size)
      (cipher buffer 0 chain 0 param)
      (blit-string-ur! chain 0 to to-pos block-size)))

;; ----Decryption
(define (create-CBC-decryption-state decrypt::procedure param block-size::long)
   (instantiate::CBC-Decrypt
      (cipher decrypt)
      (param param)
      (block-size block-size)
      (buffer (make-string block-size))
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::CBC-Decrypt IV)
   (with-access::CBC-Decrypt state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::CBC-Decrypt from from-pos to to-pos)
   (with-access::CBC-Decrypt state (buffer chain cipher block-size param)
      (blit-string-ur! from from-pos buffer 0 block-size)
      (cipher buffer 0 to to-pos param)
      (string-xor-buffer! to to-pos to to-pos chain 0 block-size)
      (let ((tmp chain))
	 (set! chain buffer)
	 (set! buffer tmp))))

;; ------------- PCBC -------------
;; ----Encryption
(define (create-PCBC-encryption-state encrypt::procedure param block-size::long)
   (instantiate::PCBC-Encrypt
      (buffer (make-string block-size))
      (cipher encrypt)
      (param param)
      (block-size block-size)
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::PCBC-Encrypt IV)
   (with-access::PCBC-Encrypt state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::PCBC-Encrypt from from-pos to to-pos)
   (with-access::PCBC-Encrypt state (chain buffer cipher block-size param)
      (string-xor-buffer! buffer 0 chain 0 from from-pos block-size)
      (cipher buffer 0 buffer 0 param)
      (string-xor-buffer! chain 0 from from-pos buffer 0 block-size)
      (blit-string-ur! buffer 0 to to-pos block-size)))

;; ----Decryption
(define (create-PCBC-decryption-state decrypt::procedure param block-size::long)
   (instantiate::PCBC-Decrypt
      (buffer (make-string block-size))
      (cipher decrypt)
      (param param)
      (block-size block-size)
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::PCBC-Decrypt IV)
   (with-access::PCBC-Decrypt state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::PCBC-Decrypt from from-pos to to-pos)
   (with-access::PCBC-Decrypt state (chain buffer cipher block-size param)
      (cipher from from-pos buffer 0 param)
      (string-xor-buffer! buffer 0 buffer 0 chain 0 block-size)
      (string-xor-buffer! chain 0 from from-pos buffer 0 block-size)
      (blit-string-ur! buffer 0 to to-pos block-size)))


;; ------------- CFB -------------
;; ----Encryption
(define (create-CFB-encryption-state encrypt::procedure param block-size::long)
   (instantiate::CFB-Encrypt
      (cipher encrypt)
      (param param)
      (block-size block-size)
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::CFB-Encrypt IV)
   (with-access::CFB-Encrypt state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::CFB-Encrypt from from-pos to to-pos)
   (with-access::CFB-Encrypt state (chain cipher block-size param)
      (cipher chain 0 chain 0 param)
      (string-xor-buffer! chain 0 from from-pos chain 0 block-size)
      (blit-string-ur! chain 0 to to-pos block-size)))
(define-method (do-cipher-partial-block! state::CFB-Encrypt
					 from from-pos to to-pos at len)
   (with-access::CFB-Encrypt state (chain cipher param)
      (when (=fx at 0)
	 (cipher chain 0 chain 0 param))
      (substring-xor-buffer! chain at chain at from (+fx from-pos at) len)
      (blit-string-ur! chain at to (+fx to-pos at) len)))

;; ----Decryption
(define (create-CFB-decryption-state encrypt::procedure param block-size::long)
   (instantiate::CFB-Decrypt
      (buffer (make-string block-size))
      (cipher encrypt)
      (param param)
      (block-size block-size)
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::CFB-Decrypt IV)
   (with-access::CFB-Decrypt state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::CFB-Decrypt from from-pos to to-pos)
   (with-access::CFB-Decrypt state (chain buffer cipher block-size param)
      (cipher chain 0 buffer 0 param)
      (blit-string-ur! from from-pos chain 0 block-size)
      (string-xor-buffer! to to-pos buffer 0 from from-pos block-size)))
(define-method (do-cipher-partial-block! state::CFB-Decrypt
					 from from-pos to to-pos at len)
   (with-access::CFB-Decrypt state (chain buffer cipher param)
      (when (=fx at 0)
	 (cipher chain 0 buffer 0 param))
      (blit-string-ur! from (+fx from-pos at) chain at len)
      (substring-xor-buffer! to (+fx to-pos at)
			     buffer 0
			     from (+fx from-pos at)
			     len)))

;; ------------- OFB -------------
;; ----Encryption and decryption
(define (create-OFB-state encrypt::procedure param block-size::long)
   (instantiate::OFB
      (cipher encrypt)
      (param param)
      (block-size block-size)
      (chain (make-string block-size))))
(define-method (do-cipher-IV-init! state::OFB IV)
   (with-access::OFB state (chain block-size)
      (blit-string-ur! IV 0 chain 0 block-size)))
(define-method (do-cipher-block! state::OFB from from-pos to to-pos)
   (with-access::OFB state (chain cipher block-size param)
      (cipher chain 0 chain 0 param)
      (string-xor-buffer! to to-pos from from-pos chain 0 block-size)))
(define-method (do-cipher-partial-block! state::OFB
					 from from-pos to to-pos at len)
   (with-access::OFB state (chain cipher param)
      (when (=fx at 0)
	 (cipher chain 0 chain 0 param))
      (substring-xor-buffer! to (+fx to-pos at)
			     from (+fx from-pos at)
			     chain at
			     len)))

;; ------------- CTR -------------
;; ----Encryption and decryption
(define (create-CTR-state encrypt::procedure param
			  nonce-init!::procedure nonce-update!::procedure
			  block-size::long)
   (instantiate::CTR
      (nonce #unspecified) ;; will be set later.
      (cipher encrypt)
      (param param)
      (nonce-str (make-string block-size))
      (nonce-init! nonce-init!)
      (nonce-update! nonce-update!)
      (block-counter 0)
      (block-size block-size)))
(define-method (do-cipher-IV-init! state::CTR IV)
   (with-access::CTR state (nonce nonce-str nonce-init!)
      (set! nonce IV)
      (nonce-init! nonce-str nonce)))
(define-method (do-cipher-block! state::CTR from from-pos to to-pos)
   (with-access::CTR state (nonce nonce-str nonce-update! block-counter
				  cipher block-size param)
      (nonce-update! nonce-str nonce block-counter)
      (set! block-counter (+fx block-counter 1))
      (cipher nonce-str 0 nonce-str 0 param)
      (string-xor-buffer! to to-pos from from-pos nonce-str 0 block-size)))
(define-method (do-cipher-partial-block! state::CTR
					 from from-pos to to-pos at len)
   (with-access::CTR state (nonce nonce-str nonce-update! block-counter
				  cipher block-size param)
      (when (=fx at 0)
	 (nonce-update! nonce-str nonce block-counter)
	 (set! block-counter (+fx block-counter 1))
	 (cipher nonce-str 0 nonce-str 0 param))
      (substring-xor-buffer! to (+fx to-pos at)
			     from (+fx from-pos at)
			     nonce-str at
			     len)))

;; ================= The functions actually doing the work =====================

;; TODO: we could avoid some copies by specializing for strings
(define (encrypt-input!::long mode-state::Cipher-Mode-State
			      from from-pos::long read-at!::procedure
			      to to-pos::long write-at!::procedure)
   (with-access::Cipher-Mode-State mode-state
	 (cipher-state block-size loop-buffer pad/unpad IV IV-action)

      (case IV-action
	 ((init) (do-cipher-IV-init! cipher-state IV))
	 ((write)
	  (do-cipher-IV-init! cipher-state IV)
	  (write-at! IV 0 to to-pos block-size)
	  (set! to-pos (+fx to-pos block-size)))
	 ((done) 'do-nothing)
	 (else (error 'encrypt-input!
		      "Internal Error. Unexpected IV-action"
		      IV-action)))

      (let ((buffer loop-buffer)
	    (pad pad/unpad))
	 (let loop ((i 0))
	    (let ((nb-copied (read-at! from (+fx from-pos i)
				       buffer 0
				       block-size)))
	       (cond
		  ((and (<fx nb-copied block-size) (not pad))
		   (if (=fx nb-copied 0)
		       (+fx i to-pos)
		       (begin
			  (do-cipher-partial-block! cipher-state
						    buffer 0 buffer 0
						    0 nb-copied)
			  (write-at! buffer 0 to (+fx to-pos i) nb-copied)
			  (+fx+ i to-pos nb-copied))))
		  ((<fx nb-copied block-size)
		   (if (pad buffer nb-copied)
		       (begin
			  (do-cipher-block! cipher-state buffer 0 buffer 0)
			  (write-at! buffer 0 to (+fx to-pos i) block-size)
			  (+fx+ i to-pos block-size))
		       (+fx i to-pos)))
		  (else
		   (do-cipher-block! cipher-state buffer 0 buffer 0)
		   (write-at! buffer 0 to (+fx to-pos i) block-size)
		   (loop (+fx i block-size)))))))))

;; TODO: we could avoid some copies by specializing for strings
(define (decrypt-input!::long mode-state::Cipher-Mode-State
			      from from-pos::long read-at!::procedure
			      to to-pos::long write-at!::procedure)
   (with-access::Cipher-Mode-State mode-state
	 (cipher-state block-size loop-buffer pad/unpad IV IV-action)

      (case IV-action
	 ((init) (do-cipher-IV-init! cipher-state IV))
	 ((read) ;; temporarily use the loop-buffer to get the IV
	  (let ((nb-read (read-at! from from-pos loop-buffer 0 block-size)))
	     (when (not (=fx nb-read block-size))
		(error 'IV
		       "Could not read IV"
		       nb-read))
	     (do-cipher-IV-init! cipher-state loop-buffer)
	     (set! from-pos (+fx from-pos block-size))))
	 ((done) 'do-nothing)
	 (else (error 'decrypt-input!
		      "Internal Error. Unexpected IV-action"
		      IV-action)))

      (define (write-last-decrypted! last-decrypted i)
	 (unless (<fx i 0)
	    (write-at! last-decrypted 0 to (+fx to-pos i) block-size)))
   
      (let ((buffer loop-buffer)
	    (unpad pad/unpad)
	    ;; last-decrypted block has never been written.
	    (last-decrypted (make-string block-size)))
	 (let loop ((i 0))
	    (let ((nb-copied (read-at! from (+fx from-pos i)
				       buffer 0
				       block-size)))
	       (cond
		  ((and (<fx nb-copied block-size) (not unpad))
		   (write-last-decrypted! last-decrypted (-fx i block-size))
		   (if (=fx nb-copied 0)
		       (+fx i to-pos)
		       (begin
			  (do-cipher-partial-block! cipher-state
						    buffer 0 buffer 0
						    0 nb-copied)
			  (write-at! buffer 0 to (+fx to-pos i) nb-copied)
			  (+fx+ i to-pos nb-copied))))
		  ((<fx nb-copied block-size)
		   (cond
		      ((and (=fx nb-copied 0) (=fx i 0)) ;; empty input-string
		       to-pos)
		      ((=fx nb-copied 0)
		       (let ((plain-chars (unpad last-decrypted)))
			  (write-at! last-decrypted 0
				     to (+fx to-pos (-fx i block-size))
				     plain-chars)
			  (+fx+ (-fx i block-size) to-pos plain-chars)))
		      (else
		       (error 'decrypt
			      "Message size must be a multiple of block-size"
			      nb-copied))))
		  (else
		   (write-last-decrypted! last-decrypted (-fx i block-size))
		   (do-cipher-block! cipher-state buffer 0 last-decrypted 0)
		   (loop (+fx i block-size)))))))))

; ;; padded streams can not send "unfinished" blocks.
; ;; we let streams always work on strings (usually some kind of buffer)
; (define (en/decrypt-stream encrypt?::bool
; 			   state::Stream-Cipher-State
; 			   in::bstring in-pos::long
; 			   out::bstring out-pos::long
; 			   len::long)
;    (define (do-cipher! state from from-pos to to-pos)
;       (if encrypt?
; 	  (encrypt-block! state from from-pos to to-pos)
; 	  (decrypt-block! state from from-pos to to-pos)))
;    (define (do-partial-cipher! state from from-pos to to-pos at len)
;       (if encrypt?
; 	  (encrypt-partial-block! state from from-pos to to-pos at len)
; 	  (decrypt-partial-block! state from from-pos to to-pos at len)))

;    (with-access::Stream-Cipher-State state (pos block-size)
;       (cond
; 	 ((=fx len 0) 'done)
; 	 ((not (zerofx? pos))
; 	  (let* ((l (minfx (-fx block-size pos) len)))
; 	     (do-partial-cipher! state
; 				 in (-fx in-pos pos)
; 				 out (-fx out-pos pos)
; 				 pos l)
; 	     (set! pos (remainderfx (+fx pos l) block-size))
; 	     (en/decrypt-stream encrypt? state
; 				in (+fx in-pos l)
; 				out (+fx out-pos l)
; 				(-fx len l))))
; 	 (else
; 	  (let loop ((len len)
; 		     (in-pos in-pos)
; 		     (out-pos out-pos))
; 	     (cond
; 		((=fx len 0) 'done)
; 		((<fx len block-size)
; 		 (do-partial-cipher! state
; 				     in in-pos
; 				     out out-pos
; 				     0 block-size)
; 		 (set! pos len))
; 		(else
; 		 (do-cipher! state
; 			     in in-pos
; 			     out out-pos)
; 		 (loop (-fx len block-size)
; 		       (+fx in-pos block-size)
; 		       (+fx out-pos block-size)))))))))

;; ================= The functions decoding the arguments =====================

(define (create-encryption-state::Cipher-Mode-State
	 cipher::symbol password::bstring IV mode pad nonce-init! nonce-update!
	 string->key)
   (with-access::Block-Cipher (block-cipher-description cipher)
	 (block-size preferred-key-length encrypt! key->encrypt-param)

      (define (create-state mode param bs)
	 (case mode
	    ((ecb)  (create-ECB-state encrypt! param bs #t))
	    ((cfb)  (create-CFB-encryption-state encrypt! param bs))
	    ((cbc)  (create-CBC-encryption-state encrypt! param bs))
	    ((pcbc) (create-PCBC-encryption-state encrypt! param bs))
	    ((ofb)  (create-OFB-state encrypt! param bs))
	    ((ctr)  (create-CTR-state encrypt! param
				      (or nonce-init! default-nonce-init!)
				      (or nonce-update! default-nonce-update!)
				      bs))
	    (else (error 'encrypt
			 "Unknown cipher mode"
			 mode))))

      (let* ((key (if string->key
		      (string->key password)
		      (string->key-hash password preferred-key-length
					sha1sum-bin)))
	     (pad-proc (case mode
			  ((ecb cbc pcbc)
			   (case pad
			      ((none) no-pad)
			      ((bit) bit-pad)
			      ((ansi-x.923) ansi-x.923-pad)
			      ((iso-10126) iso-10126-pad)
			      ((pkcs7) pkcs7-pad)
			      ((zero) zero-pad)
			      (else (if (procedure? pad)
					pad
					(error 'encrypt
					       "Invalid padding mode"
					       pad)))))
			  (else #f)))
	     (encryption-IV (or IV
				(case mode
				   ((ecb) #f) ;; no need
				   ;((ctr) (current-seconds-nonce ,block-size))
				   (else (make-random-string block-size)))))
	     (param (key->encrypt-param key))
	     (state (create-state mode param block-size)))
	 (cond
	    ((eq? mode 'ecb) 'do-nothing)
	    ((eq? mode 'ctr) 'do-nothing)
	    (else
	     (when (or (not (string? encryption-IV))
		       (not (>=fx (string-length encryption-IV) block-size)))
		(error 'encryption
		       "IV vector is too small"
		       (if (string? IV)
			   (string-length IV)
			   IV)))))
	 (instantiate::Cipher-Mode-State
	    (encrypt? #t)
	    (block-size block-size)
	    (cipher-state state)
	    (IV-action (cond
			  ((eq? mode 'ecb) 'done)
			  ((and (not IV) encryption-IV) 'write)
			  (else 'init)))
	    (IV encryption-IV)
	    (pad/unpad pad-proc)
	    (loop-buffer (make-string block-size))))))

(define (create-decryption-state::Cipher-Mode-State
	 cipher::symbol password::bstring IV mode pad nonce-init! nonce-update!
	 string->key)
   (with-access::Block-Cipher (block-cipher-description cipher)
	 (block-size preferred-key-length encrypt! decrypt! key->encrypt-param
		     key->decrypt-param)

      (define (create-state mode param bs)
	 (case mode
	    ((ecb)  (create-ECB-state decrypt! param bs #f))
	    ((cbc)  (create-CBC-decryption-state decrypt! param bs))
	    ((pcbc) (create-PCBC-decryption-state decrypt! param bs))
	    ((cfb)  (create-CFB-decryption-state encrypt! param bs))
	    ((ofb)  (create-OFB-state encrypt! param bs))
	    ((ctr)  (create-CTR-state encrypt! param
				      (or nonce-init! default-nonce-init!)
				      (or nonce-update! default-nonce-update!)
				      bs))
	    (else (error 'decrypt
			 "Unknown cipher mode"
			 mode))))
      
      (let* ((key (if string->key
		      (string->key password)
		      (string->key-hash password preferred-key-length
					sha1sum-bin)))
	     (unpad-proc (case mode
			    ((ecb cbc pcbc)
			     (case pad
				((none) no-unpad)
				((bit) bit-unpad)
				((ansi-x.923 iso-10126 pkcs7) byte-unpad)
				((zero) zero-unpad)
				(else
				 (if (procedure? pad)
				     pad
				     (error 'decrypt
					    "Invalid padding mode"
					    pad)))))
			    (else #f)))
	     (param (case mode
		       ((ecb cbc pcbc) (key->decrypt-param key))
		       (else (key->encrypt-param key))))
	     (state (create-state mode param block-size)))
	 (cond
	    ((eq? mode 'ecb) 'do-nothing)
	    ((eq? mode 'ctr) 'do-nothing)
	    ((not IV) 'do-nothing)
	    (else
	     (when (or (not (string? IV))
		       (not (>=fx (string-length IV) block-size)))
		(error 'encryption
		       "IV vector is too small"
		       (if (string? IV)
			   (string-length IV)
			   IV)))))
	 (instantiate::Cipher-Mode-State
	    (encrypt? #f)
	    (block-size block-size)
	    (cipher-state state)
	    (IV-action (cond
			  ((eq? mode 'ecb) 'done)
			  ((not IV) 'read)
			  (else 'init)))
	    (IV IV)
	    (pad/unpad unpad-proc)
	    (loop-buffer (make-string block-size))))))

;; =========================== Public Interface ===============================

;; ------------------- Encryption ----------------------------------------

(define (encrypt::bstring cipher plaintext password::bstring
			  #!key (IV #f) (mode 'cfb) (pad 'none)
			  (nonce-init! #f) (nonce-update! #f)
			  (string->key #f))
   (let ((redirect (cond
		      ((string? plaintext) encrypt-fast-string)
		      ((mmap? plaintext) encrypt-fast-mmap)
		      ((port? plaintext) encrypt-fast-port)
		      (else 'encrypt
			    "Invalid argument"
			    plaintext))))
      (redirect cipher plaintext password IV mode pad
		nonce-init! nonce-update! string->key)))

(define (encrypt-string::bstring cipher plaintext::bstring password::bstring
				 #!key (IV #f) (mode 'cfb)
				 (pad 'none)
				 (nonce-init! #f) (nonce-update! #f)
				 (string->key #f))
   (encrypt-fast-string cipher plaintext password IV mode pad
			nonce-init! nonce-update!
			string->key))

(define (encrypt-fast-string cipher plaintext::bstring password
			     IV mode pad nonce-init! nonce-update!
			     string->key)

   ;; we (may) need two extra blocks: one for the IV, and one for the
   ;; padding.
   (let* ((state (create-encryption-state cipher password IV mode pad
					  nonce-init! nonce-update!
					  string->key))
	  (block-size (with-access::Cipher-Mode-State state (block-size)
			 block-size))
	  (str (make-string (+fx (*fx 2 block-size) (string-length plaintext)))))
      (string-shrink! str
		      (encrypt-input! state
				      plaintext 0 read-string-at!
				      str 0 blit-string-ur!))))

(define (encrypt-mmap::bstring cipher plaintext::mmap password::bstring
			       #!key (IV #f) (mode 'cfb) (pad 'none)
			       (nonce-init! #f) (nonce-update! #f)
			       (string->key #f))
   (encrypt-fast-mmap cipher plaintext password IV mode pad
		      nonce-init! nonce-update!
		      string->key))

(define (encrypt-fast-mmap cipher plaintext::mmap password IV mode
			   pad nonce-init! nonce-update!
			   string->key)
   ;; we (may) need two extra blocks: one for the IV, and one for the
   ;; padding.
   (let* ((len (mmap-length plaintext))
	  (state (create-encryption-state cipher password IV mode pad
					  nonce-init! nonce-update!
					  string->key))
	  (str (make-string (+fx (*fx 2 (with-access::Cipher-Mode-State state (block-size) block-size))
				 len))))
	  
      (string-shrink! str
		      (encrypt-input! state
				      plaintext 0 read-mmap-at!
				      str 0 blit-string-ur!))))

(define (encrypt-port::bstring cipher plaintext::input-port password::bstring
			       #!key (IV #f) (mode 'cfb) (pad 'none)
			       (nonce-init! #f) (nonce-update! #f)
			       (string->key #f))
   (encrypt-fast-port cipher plaintext password IV mode pad
				   nonce-init! nonce-update!
				   string->key))

(define (encrypt-fast-port cipher plaintext::input-port password
			   IV mode pad nonce-init! nonce-update!
			   string->key)
   (let ((p (open-output-string))
	 (state (create-encryption-state cipher password IV mode pad
					 nonce-init! nonce-update!
					 string->key)))
      (encrypt-input! state
		      plaintext 0 read-port-at!
		      p 0 write-port-at!)
      (close-output-port p)))

(define (encrypt-file::bstring cipher filename::bstring password::bstring
			       #!key (IV #f) (mode 'cfb) (pad 'none)
			       (nonce-init! #f) (nonce-update! #f)
			       (string->key #f))
   (let ((p (open-input-file filename)))
      (when (not p)
	 (error 'encrypt-file
		"Could not open file"
		filename))
      (unwind-protect
	 (encrypt-fast-port cipher p password IV mode pad
				   nonce-init! nonce-update!
				   string->key)
	 (close-input-port p))))

(define (encrypt-sendchars cipher in::input-port out::output-port
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f)
			   (string->key #f))
   (let ((state (create-encryption-state cipher password IV mode pad
					 nonce-init! nonce-update!
					 string->key)))
      (encrypt-input! state
		      in 0 read-port-at!
		      out 0 write-port-at!)))

;; ------------------- Decryption ----------------------------------------

(define (decrypt::bstring cipher ciphertext password::bstring
			  #!key (IV #f) (mode 'cfb) (pad 'none)
			  (nonce-init! #f) (nonce-update! #f)
			  (string->key #f))
   (let ((redirect (cond
		      ((string? ciphertext) decrypt-fast-string)
		      ((mmap? ciphertext) decrypt-fast-mmap)
		      ((port? ciphertext) decrypt-fast-port)
		      (else 'decrypt
			    "Invalid argument"
			    ciphertext))))
      (redirect cipher ciphertext password IV mode pad
		nonce-init! nonce-update! string->key)))

(define (decrypt-string::bstring cipher ciphertext::bstring password::bstring
				 #!key (IV #f) (mode 'cfb) (pad 'none)
				 (nonce-init! #f) (nonce-update! #f)
				 (string->key #f))
   (decrypt-fast-string cipher ciphertext password IV mode pad
			nonce-init! nonce-update! string->key))

(define (decrypt-fast-string::bstring cipher ciphertext::bstring password::bstring
				      IV mode pad nonce-init!
				      nonce-update! string->key)
   (let ((str (make-string (string-length ciphertext)))
	 (state (create-decryption-state cipher password IV mode pad
					 nonce-init! nonce-update!
					 string->key)))
      (string-shrink! str
		      (decrypt-input! state
				      ciphertext 0 read-string-at!
				      str 0 blit-string-ur!))))

(define (decrypt-mmap::bstring cipher ciphertext::mmap password::bstring
			       #!key (IV #f) (mode 'cfb) (pad 'none)
			       (nonce-init! #f) (nonce-update! #f)
			       (string->key #f))
   (decrypt-fast-mmap cipher ciphertext password IV mode pad
				     nonce-init! nonce-update! string->key))

(define (decrypt-fast-mmap::bstring cipher ciphertext::mmap password::bstring
				    IV mode pad nonce-init!
				    nonce-update! string->key)
   (let* ((len (mmap-length ciphertext))
	  (str (make-string len))
	  (state (create-decryption-state cipher password IV mode pad
					  nonce-init! nonce-update!
					  string->key)))
      (string-shrink! str
		      (decrypt-input! state
				      ciphertext 0 read-mmap-at!
				      str 0 blit-string-ur!))))

(define (decrypt-port::bstring cipher ciphertext::input-port password::bstring
			       #!key (IV #f) (mode 'cfb) (pad 'none)
			       (nonce-init! #f) (nonce-update! #f)
			       (string->key #f))
   (decrypt-fast-port cipher ciphertext password IV mode pad
		      nonce-init! nonce-update! string->key))

(define (decrypt-fast-port::bstring cipher ciphertext::input-port password::bstring
				    IV mode pad nonce-init!
				    nonce-update! string->key)
   (let ((p (open-output-string))
	 (state (create-decryption-state cipher password IV mode pad
					 nonce-init! nonce-update!
					 string->key)))
      (decrypt-input! state
		      ciphertext 0 read-port-at!
		      p 0 write-port-at!)
      (close-output-port p)))

(define (decrypt-file::bstring cipher filename::bstring password::bstring
			       #!key (IV #f) (mode 'cfb) (pad 'none)
			       (nonce-init! #f) (nonce-update! #f)
			       (string->key #f))
   (let ((p (open-input-file filename)))
      (when (not p)
	 (error 'decrypt-file
		"Could not open file"
		filename))
      (unwind-protect
	 (decrypt-fast-port cipher p password IV mode pad
				   nonce-init! nonce-update!
				   string->key)
	 (close-input-port p))))

(define (decrypt-sendchars cipher in::input-port out::output-port
			   password::bstring
			   #!key (IV #f) (mode 'cfb) (pad 'none)
			   (nonce-init! #f) (nonce-update! #f) (string->key #f))
   (let ((state (create-decryption-state cipher password IV mode pad
					 nonce-init! nonce-update!
					 string->key)))
      (decrypt-input! state
		      in 0 read-port-at!
		      out 0 write-port-at!)))

;; ============================ Cipher Registry ===============================

(define *ciphers* '())

(define (register-cipher! lookup-name::symbol desc)
   (set! *ciphers* (cons (cons lookup-name desc) *ciphers*)))

(define (block-cipher-description::Block-Cipher lookup-name::symbol)
   (let ((desc-p (assq lookup-name *ciphers*)))
      (when (not desc-p)
	 (error 'block-cipher-description
		"Could not find cipher"
		lookup-name))
      (cdr desc-p)))
