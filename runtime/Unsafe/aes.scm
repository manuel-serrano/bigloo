;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/aes.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Chris Veness                                      */
;*    Creation    :  Thu Jun  5 08:00:03 2008                          */
;*    Last change :  Wed Oct  7 11:48:25 2015 (serrano)                */
;*    Copyright   :  2005-15 Chris Veness                              */
;*    -------------------------------------------------------------    */
;*    Advanced Encryption Standard                                     */
;*    -------------------------------------------------------------    */
;*    For a description of AES, visit:                                 */
;*      http://en.wikipedia.org/wiki/Advanced_Encryption_Standard      */
;*    For a description of Cipher modes (e.g., CBC, CTR, ECB, ...):    */
;*      http://en.wikipedia.org/wiki/Block_cipher_modes_of_operation   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __aes

   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
	   __rgc
	   __bit
	   __bignum
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __r4_input_6_10_2
	   __r5_control_features_6_4
	   __mmap
	   __foreign
	   __error
	   __evenv
	   __os
	   __srfi4
	   __intext)
   
   (import __sha1
	   __date
	   __param)
   
   (export (aes-ctr-encrypt::bstring ::obj ::bstring
				     #!optional (nbits 128))
	   (aes-ctr-encrypt-mmap::bstring ::mmap ::bstring
					  #!optional (nbits 128))
	   (aes-ctr-encrypt-string::bstring ::bstring ::bstring
					    #!optional (nbits 128))
	   (aes-ctr-encrypt-port::bstring ::input-port ::bstring
					  #!optional (nbits 128))
	   (aes-ctr-encrypt-file::bstring ::bstring ::bstring
					  #!optional (nbits 128))
	   
	   (aes-ctr-decrypt::bstring ::obj ::bstring #!optional (nbits 128))
	   (aes-ctr-decrypt-mmap::bstring ::mmap ::bstring
					  #!optional (nbits 128))
	   (aes-ctr-decrypt-string::bstring ::bstring ::bstring
					    #!optional (nbits 128))
	   (aes-ctr-decrypt-port::bstring ::input-port ::bstring
					  #!optional (nbits 128))
	   (aes-ctr-decrypt-file::bstring ::bstring ::bstring
					  #!optional (nbits 128)))

   (option (set! *init-mode* 'intern)))

;*---------------------------------------------------------------------*/
;*    u8vector-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (u8vector-ref vec i)
   `(uint8->fixnum ((@ u8vector-ref __srfi4) ,vec ,i)))

;*---------------------------------------------------------------------*/
;*    aes-ctr-encrypt ...                                              */
;*    -------------------------------------------------------------    */
;*    The bit keys (nbits) must either be 128, 192, or 256.            */
;*---------------------------------------------------------------------*/
(define (aes-ctr-encrypt plaintext password #!optional (nbits 128))
   (cond
      ((string? plaintext)
       (aes-ctr-encrypt-string plaintext password nbits))
      ((mmap? plaintext)
       (aes-ctr-encrypt-mmap plaintext password nbits))
      ((input-port? plaintext)
       (aes-ctr-encrypt-port plaintext password nbits))
      (else
       (error 'aes-ctr-encrypt "Illegal argument" plaintext))))

;*---------------------------------------------------------------------*/
;*    aes-ctr-encrypt-string ...                                       */
;*---------------------------------------------------------------------*/
(define (aes-ctr-encrypt-string plaintext password #!optional (nbits 128))
   (%aes-ctr-encrypt plaintext password nbits))
   
;*---------------------------------------------------------------------*/
;*    aes-ctr-encrypt-mmap ...                                         */
;*---------------------------------------------------------------------*/
(define (aes-ctr-encrypt-mmap plaintext password #!optional (nbits 128))
   (%aes-ctr-encrypt plaintext password nbits))
   
;*---------------------------------------------------------------------*/
;*    aes-ctr-encrypt-file ...                                         */
;*---------------------------------------------------------------------*/
(define (aes-ctr-encrypt-file fname password #!optional (nbits 128))
   (let ((mm (open-mmap fname :write #f)))
      (unwind-protect
	 (aes-ctr-encrypt-mmap mm password nbits)
	 (close-mmap mm))))

;*---------------------------------------------------------------------*/
;*    aes-ctr-encrypt-port ...                                         */
;*---------------------------------------------------------------------*/
(define (aes-ctr-encrypt-port input-port password #!optional (nbits 128))
   (aes-ctr-encrypt-string (read-string input-port) password nbits))

;*---------------------------------------------------------------------*/
;*    aes-ctr-decrypt ...                                              */
;*    -------------------------------------------------------------    */
;*    Use AES to decrypt 'ciphertext' with 'password' using 'nBits'    */
;*    key, in Counter mode of operation.                               */
;*---------------------------------------------------------------------*/
(define (aes-ctr-decrypt ciphertext password #!optional (nbits 128))
   (cond
      ((string? ciphertext)
       (aes-ctr-decrypt-string ciphertext password nbits))
      ((mmap? ciphertext)
       (aes-ctr-decrypt-mmap ciphertext password nbits))
      ((input-port? ciphertext)
       (aes-ctr-decrypt-port ciphertext password nbits))
      (else
       (error 'aes-ctr-decrypt "Illegal argument" ciphertext))))
 
;*---------------------------------------------------------------------*/
;*    aes-ctr-decrypt-string ...                                       */
;*---------------------------------------------------------------------*/
(define (aes-ctr-decrypt-string plaintext password #!optional (nbits 128))
   (%aes-ctr-decrypt plaintext password nbits))
   
;*---------------------------------------------------------------------*/
;*    aes-ctr-decrypt-mmap ...                                         */
;*---------------------------------------------------------------------*/
(define (aes-ctr-decrypt-mmap plaintext password #!optional (nbits 128))
   (%aes-ctr-decrypt plaintext password nbits))
   
;*---------------------------------------------------------------------*/
;*    aes-ctr-decrypt-file ...                                         */
;*---------------------------------------------------------------------*/
(define (aes-ctr-decrypt-file fname password #!optional (nbits 128))
   (let ((mm (open-mmap fname :write #f)))
      (unwind-protect
	 (aes-ctr-decrypt-mmap mm password nbits)
	 (close-mmap mm))))

;*---------------------------------------------------------------------*/
;*    aes-ctr-decrypt-port ...                                         */
;*---------------------------------------------------------------------*/
(define (aes-ctr-decrypt-port input-port password #!optional (nbits 128))
   (aes-ctr-decrypt-string (read-string input-port) password nbits))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for decl test incr . body)
   (match-case (list decl test incr)
      (((?var 0) (<fx ?var 4) (+fx ?var ?-))
       ;; unfold the loop
       `(begin
	   (let ((,var 0)) ,@body)
	   (let ((,var 1)) ,@body)
	   (let ((,var 2)) ,@body)
	   (let ((,var 3)) ,@body)))
      (else
       ;; regular iteration
       (let ((loop (gensym (symbol-append 'loop- (car decl) '-))))
	  `(let ,loop (,decl)
		(when ,test
		   ,@body
		   (,loop ,incr)))))))

;*---------------------------------------------------------------------*/
;*    let+ ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (let+ bindings . body)
   (match-case bindings
      (((?var . ?clauses))
       (let ((tvar (string->symbol (format "~a::procedure" var))))
	  `(cond
	      ,@(map (lambda (b)
			(match-case b
			   ((?pred ?value)
			    `(,pred (let ((,tvar ,value)) ,@body)))
			   (else
			    (error 'let+ "illegal form" b))))
		     clauses)
	      (else 0))))
      (else
       (error 'let+ "illegal form" bindings))))
		 
;*---------------------------------------------------------------------*/
;*    blocksize ...                                                    */
;*---------------------------------------------------------------------*/
(define (blocksize::int) 16)

;*---------------------------------------------------------------------*/
;*    noncesize ...                                                    */
;*---------------------------------------------------------------------*/
(define (noncesize::int) 8)

;*---------------------------------------------------------------------*/
;*    /ceiling ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (/ceiling::long a b)
   (let ((r (/ a b)))
      (if (fixnum? r)
	  r
	  (flonum->fixnum (ceiling r)))))

;*---------------------------------------------------------------------*/
;*    %aes-ctr-encrypt ...                                             */
;*    -------------------------------------------------------------    */
;*    The bit keys (nbits) must either be 128, 192, or 256.            */
;*---------------------------------------------------------------------*/
(define (%aes-ctr-encrypt plaintext password #!optional (nbits 128))
   
   ;; user check
   (unless (memv nbits '(128 192 256))
      (error 'aes-ctr-encrypt "Illegal bit keys" nbits))

   ;; internal check
   (unless (or (string? plaintext) (mmap? plaintext))
      (bigloo-type-error 'aes-ctr-encrypt "string or mmap" plaintext))
   
   ;; from the user password, create the aes key
   (let* ((state::vector (make-u8matrix 4 4))
	  (len::int (let+ ((len ((string? plaintext) string-length)
				((mmap? plaintext) mmap-length)))
		       (len plaintext)))
	  (key::u8vector (aes-password->key password nbits state))

	  ;; initialise counter block (NIST SP800-38A B.2):
	  ;; millisecond time-stamp for nonce in 1st 8 bytes,
	  ;; block counter in 2nd 8 bytes
	  (blockcount::long (/ceiling len (blocksize)))
	  (counterblock::u8vector (make-u8vector (blocksize)))
	  (nonce::elong (current-seconds))

	  ;; generate key schedule - an expansion of the key into distinct
	  ;; Key Rounds for each round
	  (keyschedule::vector (aes-key-expansion key))
	  
	  ;; ciphertext as a string
	  (ciphertext::bstring (make-string (+fx (noncesize) len))))
      
      ;; encode nonce
      (for (i 0) (<fx i 4) (+fx i 1)
	   (let ((s (elong->fixnum (abselong (bit-rshelong nonce (*fx i 8))))))
	      (u8vector-set! counterblock i (bit-and s #xff)))
	   (let ((s 0))
	      ;; s should be (bit-rshelong (/elong nonce #ex100000000)
	      ;; but since bigloo supports strings up to 1 << 30, this is
	      ;; always 0
	      (u8vector-set! counterblock (+fx i 4) (bit-and s #xff))))

      (for (b 0) (<fx b blockcount) (+fx b 1)
	   ;; set counter (block #) in last 8 bytes of counter block
	   ;; (leaving nonce in 1st 8 bytes) again done in two stages
	   ;; for 32-bit ops
	   (for (c 0) (<fx c 4) (+fx c 1)
		(let ((j (*fx c 8)))
		   (let ((s (bit-and (bit-ursh b j) #xff)))
		      (u8vector-set! counterblock (-fx 15 c) s)
		      ;; Bigloo string max length is 2^30
		      (u8vector-set! counterblock (-fx 15 (+fx c 4)) 0))))
	   
	   (let* ((ciphercntr (aes-cipher counterblock keyschedule state))
		  (blocklength (if (<fx b (-fx blockcount 1))
				   (blocksize)
				   (+fx (remainder (-fx len 1) (blocksize)) 1)))
		  (start (+fx (noncesize) (*fx b (blocksize)))))

	      (let+ ((ref ((string? plaintext) u8string-ref)
			  ((mmap? plaintext) mmap-ref)))
		 (for (i 0) (<fx i blocklength) (+fx i 1)
		      (let* ((j (+fx (*fx b (blocksize)) i))
			     (p (ref plaintext j))
			     (byte (bit-xor p (u8vector-ref ciphercntr i))))
			 (u8string-set! ciphertext (+fx i start) byte))))))
      
      ;; convert the nonce to a string to go on the front of the ciphertext
      (for (i 0) (<fx i (noncesize)) (+fx i 1)
	   (u8string-set! ciphertext i (u8vector-ref counterblock i)))

      ciphertext))

;*---------------------------------------------------------------------*/
;*    %aes-ctr-decrypt ...                                             */
;*    -------------------------------------------------------------    */
;*    Use AES to decrypt 'ciphertext' with 'password' using 'nBits'    */
;*    key, in Counter mode of operation.                               */
;*---------------------------------------------------------------------*/
(define (%aes-ctr-decrypt ciphertext password #!optional (nbits 128))
   
   ;; user check
   (unless (memv nbits '(128 192 256))
      (error 'aes-ctr-decrypt "Illegal bit keys" nbits))
   
   (let* ((state::vector (make-u8matrix 4 4))
	  (key::u8vector (aes-password->key password nbits state))
	  (keyschedule::vector (aes-key-expansion key))
	  (len::int (-fx (string-length ciphertext) (noncesize)))
	  (blockcount (/ceiling len (blocksize)))
	  (counterblock::u8vector (make-u8vector (blocksize)))
	  (plaintext::bstring (make-string len)))
      
      ;; recover nonce from 1st element of ciphertext
      (for (i 0) (<fx i (noncesize)) (+fx i 1)
	   (u8vector-set! counterblock i (u8string-ref ciphertext i)))
      
      (for (b 0) (<fx b blockcount) (+fx b 1)
	   (for (c 0) (<fx c 4) (+fx c 1)
		(let ((v (bit-and (bit-ursh b (*fx c 8)) #xff)))
		   (u8vector-set! counterblock (-fx 15 c) v))
		(let ((v 0))
		   ;; should be ((b/0x100000000-1) >>> c*8) & 0xff
		   (u8vector-set! counterblock (-fx 15 (+fx c 4)) v)))
	   (let* ((ciphercntr (aes-cipher counterblock keyschedule state))
		  (blocklength (if (<fx b (-fx blockcount 1))
				   (blocksize)
				   (+fx (remainder (-fx len 1) (blocksize)) 1)))
		  (start (*fx b (blocksize)))
		  (end (+fx start blocklength)))
	      (for (i start) (<fx i end) (+fx i 1)
		   (let* ((cipher (u8string-ref ciphertext (+fx i (noncesize))))
			  (cntrbyte (u8vector-ref ciphercntr (-fx i start)))
			  (plainbyte (bit-xor cipher cntrbyte)))
		      (u8string-set! plaintext i plainbyte)))))
      plaintext))

;*---------------------------------------------------------------------*/
;*    Rconn ...                                                        */
;*    -------------------------------------------------------------    */
;*    Rcon is Round Constant used for the Key Expansion                */
;*    [1st col is 2^(r-1) in GF(2^8)                                   */
;*---------------------------------------------------------------------*/
(define Rconn
   '#(#u8(#x00 #x00 #x00 #x00)
      #u8(#x01 #x00 #x00 #x00)
      #u8(#x02 #x00 #x00 #x00)
      #u8(#x04 #x00 #x00 #x00)
      #u8(#x08 #x00 #x00 #x00)
      #u8(#x10 #x00 #x00 #x00)
      #u8(#x20 #x00 #x00 #x00)
      #u8(#x40 #x00 #x00 #x00)
      #u8(#x80 #x00 #x00 #x00)
      #u8(#x1b #x00 #x00 #x00)
      #u8(#x36 #x00 #x00 #x00)))

;*---------------------------------------------------------------------*/
;*    Sbox ...                                                         */
;*    -------------------------------------------------------------    */
;*    Sbox is pre-computed multiplicative inverse in GF(2^8) used      */
;*    in SubBytes and AESKeyExpansion                                  */
;*---------------------------------------------------------------------*/
(define Sbox
   '#u8(#x63 #x7c #x77 #x7b #xf2 #x6b #x6f #xc5 #x30 #x01 #x67 #x2b #xfe #xd7
	     #xab #x76 #xca #x82 #xc9 #x7d #xfa #x59 #x47 #xf0 #xad #xd4 #xa2
	     #xaf #x9c #xa4 #x72 #xc0 #xb7 #xfd #x93 #x26 #x36 #x3f #xf7 #xcc
	     #x34 #xa5 #xe5 #xf1 #x71 #xd8 #x31 #x15 #x04 #xc7 #x23 #xc3 #x18
	     #x96 #x05 #x9a #x07 #x12 #x80 #xe2 #xeb #x27 #xb2 #x75 #x09 #x83
	     #x2c #x1a #x1b #x6e #x5a #xa0 #x52 #x3b #xd6 #xb3 #x29 #xe3 #x2f
	     #x84 #x53 #xd1 #x00 #xed #x20 #xfc #xb1 #x5b #x6a #xcb #xbe #x39
	     #x4a #x4c #x58 #xcf #xd0 #xef #xaa #xfb #x43 #x4d #x33 #x85 #x45
	     #xf9 #x02 #x7f #x50 #x3c #x9f #xa8 #x51 #xa3 #x40 #x8f #x92 #x9d
	     #x38 #xf5 #xbc #xb6 #xda #x21 #x10 #xff #xf3 #xd2 #xcd #x0c #x13
	     #xec #x5f #x97 #x44 #x17 #xc4 #xa7 #x7e #x3d #x64 #x5d #x19 #x73
	     #x60 #x81 #x4f #xdc #x22 #x2a #x90 #x88 #x46 #xee #xb8 #x14 #xde
	     #x5e #x0b #xdb #xe0 #x32 #x3a #x0a #x49 #x06 #x24 #x5c #xc2 #xd3
	     #xac #x62 #x91 #x95 #xe4 #x79 #xe7 #xc8 #x37 #x6d #x8d #xd5 #x4e
	     #xa9 #x6c #x56 #xf4 #xea #x65 #x7a #xae #x08 #xba #x78 #x25 #x2e
	     #x1c #xa6 #xb4 #xc6 #xe8 #xdd #x74 #x1f #x4b #xbd #x8b #x8a #x70
	     #x3e #xb5 #x66 #x48 #x03 #xf6 #x0e #x61 #x35 #x57 #xb9 #x86 #xc1
	     #x1d #x9e #xe1 #xf8 #x98 #x11 #x69 #xd9 #x8e #x94 #x9b #x1e #x87
	     #xe9 #xce #x55 #x28 #xdf #x8c #xa1 #x89 #x0d #xbf #xe6 #x42 #x68
	     #x41 #x99 #x2d #x0f #xb0 #x54 #xbb #x16))

;*---------------------------------------------------------------------*/
;*    xor ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (xor a0 a1 . rest)
   (if (null? rest)
       `(bit-xor ,a0 ,a1)
       `(bit-xor ,a0 (xor ,a1 ,@rest))))

;*---------------------------------------------------------------------*/
;*    make-u8matrix ...                                                */
;*---------------------------------------------------------------------*/
(define (make-u8matrix::vector n::int m::int)
   (let ((v (make-vector n)))
      (for (i 0) (<fx i n) (+fx i 1)
	   (vector-set! v i (make-u8vector m 0)))
      v))

;*---------------------------------------------------------------------*/
;*    u8matrix-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (u8matrix-ref::ulong m::vector i::int j::int)
   (u8vector-ref (vector-ref m i) j))

;*---------------------------------------------------------------------*/
;*    u8matrix-set! ...                                                */
;*---------------------------------------------------------------------*/
(define (u8matrix-set! m::vector i::int j::int v::ulong)
   (u8vector-set! (vector-ref m i) j v))

;*---------------------------------------------------------------------*/
;*    u8string-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define (u8string-ref::int s::bstring i::int)
   (char->integer (string-ref s i)))

;*---------------------------------------------------------------------*/
;*    u8string-set! ...                                                */
;*---------------------------------------------------------------------*/
(define (u8string-set! s::bstring i::int v::int)
   (string-set! s i (integer->char v)))

;*---------------------------------------------------------------------*/
;*    aes-password->key ...                                            */
;*---------------------------------------------------------------------*/
(define (aes-password->key::u8vector password::bstring nbits::int state::vector)
   (let* ((nbytes (/fx nbits 8))
	  (uvec (make-u8vector nbytes 0))
	  (shapass (if (<fx (string-length password) nbytes)
		       (string-append password (sha1sum-string password))
		       password))
	  (plen (string-length shapass)))
      (for (i 0) (<fx i nbytes) (+fx i 1)
	   (u8vector-set! uvec i (u8string-ref shapass i)))
      (aes-cipher uvec (aes-key-expansion uvec) state)))

;*---------------------------------------------------------------------*/
;*    aes-key-expansion ...                                            */
;*    -------------------------------------------------------------    */
;*    generate Key Schedule (byte-array Nr+1 x Nb) from Key            */
;*---------------------------------------------------------------------*/
(define (aes-key-expansion::vector key::u8vector)
   
   (let* ((Nb 4)
	  (Nk (/fx (u8vector-length key) 4))
	  (Nr (+fx Nk 6))
	  (w (make-vector (*fx Nb (+fx Nr 1))))
	  (temp (make-u8vector 4)))
      
      (for (i 0) (<fx i Nk) (+fx i 1)
	   (let* ((j (*fx 4 i))
		  (r (u8vector (u8vector-ref key j)
			       (u8vector-ref key (+fx j 1))
			       (u8vector-ref key (+fx j 2))
			       (u8vector-ref key (+fx j 3)))))
	      (vector-set! w i r)))
      
      (for (i Nk) (<fx i (*fx Nb (+fx Nr 1))) (+fx i 1)
	   (vector-set! w i (make-u8vector 4))
	   (for (t 0) (<fx t 4) (+fx t 1)
		(u8vector-set! temp t (u8matrix-ref w (-fx i 1) t)))
	   (if (zerofx? (remainder i Nk))
	       (begin
		  (subword! (rotword! temp))
		  (for (t 0) (<fx t 4) (+fx t 1)
		       (let ((v (bit-xor (u8vector-ref temp t)
					 (u8matrix-ref Rconn (/fx i Nk) t))))
			  (u8vector-set! temp t v))))
	       (when (and (>fx Nk 6) (=fx (remainder i Nk) 4))
		  (subword! temp)))
	   (for (t 0) (<fx t 4) (+fx t 1)
		(let ((v (bit-xor (u8matrix-ref w (-fx i Nk) t)
				  (u8vector-ref temp t))))
		   (u8matrix-set! w i t v))))
      
      w))
      
;*---------------------------------------------------------------------*/
;*    aes-cipher ...                                                   */
;*---------------------------------------------------------------------*/
(define (aes-cipher input::u8vector w::vector state::vector)
   (let* ((Nb 4)
	  (Nr (-fx (/fx (vector-length w) Nb) 1)))
      
      (for (i 0) (<fx i (*fx 4 Nb)) (+fx i 1)
	   (let ((v (u8vector-ref input i)))
	      (u8matrix-set! state (remainder i 4) (/fx i 4) v)))

      (addroundkey! state w 0 Nb)
      
      (for (round 1) (<fx round Nr) (+fx round 1)
	   (subbytes! state Nb)
	   (shiftrows! state Nb)
	   (mixcolumns! state Nb)
	   (addroundkey! state w round Nb))
      
      (subbytes! state Nb)
      (shiftrows! state Nb)
      (addroundkey! state w Nr Nb)
      
      (let ((output (make-u8vector (*fx 4 Nb))))
	 (for (i 0) (<fx i (*fx 4 Nb)) (+fx i 1)
	      (let ((v (u8matrix-ref state (remainder i 4) (/fx i 4))))
		 (u8vector-set! output i v)))
	 output)))

;*---------------------------------------------------------------------*/
;*    subbytes! ...                                                    */
;*    -------------------------------------------------------------    */
;*    apply SBox to state                                              */
;*---------------------------------------------------------------------*/
(define (subbytes! s::vector Nb::int)
   (for (r 0) (<fx r 4) (+fx r 1)
	(for (c 0) (<fx c Nb) (+fx c 1)
	     (u8matrix-set! s r c (u8vector-ref Sbox (u8matrix-ref s r c))))))
   
;*---------------------------------------------------------------------*/
;*    shiftrows! ...                                                   */
;*    -------------------------------------------------------------    */
;*    shift row r of state S left by r bytes                           */
;*---------------------------------------------------------------------*/
(define (shiftrows! s Nb)
   (let ((t (make-u8vector 4)))
      (for (r 1) (<fx r 4) (+fx r 1)
	   (for (c 0) (<fx c 4) (+fx c 1)
		(u8vector-set! t c (u8matrix-ref s r (remainder (+fx c r) Nb))))
	   (for (c 0) (<fx c 4) (+fx c 1)
		(u8matrix-set! s r c (u8vector-ref t c))))))

;*---------------------------------------------------------------------*/
;*    mixcolumns! ...                                                  */
;*    -------------------------------------------------------------    */
;*    combine bytes of each col of state                               */
;*---------------------------------------------------------------------*/
(define (mixcolumns! s Nb)
   (for (c 0) (<fx c 4) (+fx c 1)
	(let ((a (make-u8vector 4))
	      (b (make-u8vector 4)))
	   (for (i 0) (<fx i 4) (+fx i 1)
		(let ((v (u8matrix-ref s i c)))
		   (u8vector-set! a i v)
		   (if (=fx (bit-and v #x80) 0)
		       (u8vector-set! b i (bit-lsh v 1))
		       (u8vector-set! b i (bit-xor (bit-lsh v 1) #x011b)))))
	   (u8matrix-set! s 0 c (xor (u8vector-ref b 0)
				     (u8vector-ref a 1)
				     (u8vector-ref b 1)
				     (u8vector-ref a 2)
				     (u8vector-ref a 3)))
	   (u8matrix-set! s 1 c (xor (u8vector-ref a 0)
				     (u8vector-ref b 1)
				     (u8vector-ref a 2)
				     (u8vector-ref b 2)
				     (u8vector-ref a 3)))
	   (u8matrix-set! s 2 c (xor (u8vector-ref a 0)
				     (u8vector-ref a 1)
				     (u8vector-ref b 2)
				     (u8vector-ref a 3)
				     (u8vector-ref b 3)))
	   (u8matrix-set! s 3 c (xor (u8vector-ref a 0)
				     (u8vector-ref b 0)
				     (u8vector-ref a 1)
				     (u8vector-ref a 2)
				     (u8vector-ref b 3))))))

;*---------------------------------------------------------------------*/
;*    addroundkey! ...                                                 */
;*    -------------------------------------------------------------    */
;*    xor Round Key into state                                         */
;*---------------------------------------------------------------------*/
(define (addroundkey! state::vector w rnd Nb)
   (for (r 0) (<fx r 4) (+fx r 1)
	(for (c 0) (<fx c Nb) (+fx c 1)
	     (let ((v (bit-xor (u8matrix-ref state r c)
			       (u8matrix-ref w (+fx (*fx rnd 4) c) r))))
		(u8matrix-set! state r c v)))))

;*---------------------------------------------------------------------*/
;*    subword! ...                                                     */
;*    -------------------------------------------------------------    */
;*    apply SBox to 4-byte word w                                      */
;*---------------------------------------------------------------------*/
(define (subword! w::u8vector)
   (for (i 0) (<fx i 4) (+fx i 1)
	(u8vector-set! w i (u8vector-ref Sbox (u8vector-ref w i))))
   w)

;*---------------------------------------------------------------------*/
;*    rotword! ...                                                     */
;*    -------------------------------------------------------------    */
;*    rotate 4-byte word w left by one byte                            */
;*---------------------------------------------------------------------*/
(define (rotword! w::u8vector)
   (let ((tmp (u8vector-ref w 0)))
      (for (i 0) (<fx i 3) (+fx i 1)
	   (u8vector-set! w i (u8vector-ref w (+fx i 1))))
      (u8vector-set! w 3 tmp)
      w))
   
