(module __crypto-des
   (import __crypto-util
	   __crypto-block-ciphers)
   (static
    (final-class DES-Param
       (buff32a::bstring read-only)
       (buff32b::bstring read-only)
       (buff64::bstring read-only)
       (keys::bstring read-only)
       (initial-permutation?::bool read-only))
    (final-class DES3-Param
       (buff32a::bstring read-only)
       (buff32b::bstring read-only)
       (buff64::bstring read-only)
       (keys1::bstring read-only)
       (keys2::bstring read-only)
       (keys3::bstring read-only)
       (initial-permutation?::bool read-only))))

(define *des-rounds* 16)
(define *block-size* 64)
(define *key-size* 56) ;; real key-size


(define (DES-key->param key encrypt? initial-permutation?)
   (let ((key-len (*fx 8 (string-length key))))
      (when (not (or (=fx key-len 56)
		     (=fx key-len 64)))
	 (error 'des
		"Key-length must be 56 or 64 bits"
		key-len))
      (let* ((buff32a (make-des-buffer 32))
	     (buff32b (make-des-buffer 32))
	     (buff64  (make-des-buffer 64))
	     (expanded-keys (key-expansion key 0 key-len encrypt?)))
	 (instantiate::DES-Param
	    (buff32a buff32a)
	    (buff32b buff32b)
	    (buff64 buff64)
	    (keys expanded-keys)
	    (initial-permutation? initial-permutation?)))))

(define (DES-key->encrypt-param key)
   (DES-key->param key #t #t))
(define (DES-key->encrypt-param-no-permutation key)
   (DES-key->param key #t #f))
(define (DES-key->decrypt-param key)
   (DES-key->param key #f #t))
(define (DES-key->decrypt-param-no-permutation key)
   (DES-key->param key #f #f))

(register-cipher! 'des (instantiate::Block-Cipher
			  (name "DES (Data Encryption Standard)")
			  (block-size (/fx *block-size* 8))
			  (preferred-key-length 7)
			  (encrypt! en/decrypt-DES)
			  (decrypt! en/decrypt-DES)
			  (key->encrypt-param DES-key->encrypt-param)
			  (key->decrypt-param DES-key->decrypt-param)))

(register-cipher! 'des-np (instantiate::Block-Cipher
			     (name "DES (Data Encryption Standard). No initial Permutation")
			     (block-size (/fx *block-size* 8))
			     (preferred-key-length 7)
			     (encrypt! en/decrypt-DES)
			     (decrypt! en/decrypt-DES)
			     (key->encrypt-param DES-key->encrypt-param-no-permutation)
			     (key->decrypt-param DES-key->decrypt-param-no-permutation)))


(define (DES3-key->param key encrypt? initial-permutation?)
   (let* ((key-len (*fx 8 (string-length key)))
	  (nb-keys (if (<=fx key-len 128) 2 3))
	  (subkey-len (/fx key-len nb-keys)))
      (when (not (or (=fx key-len 112) (=fx key-len 128)
		     (=fx key-len 168) (=fx key-len 192)))
	 (error 'des
		"Key-length must be 112/128 or 168/192 bits"
		key-len))
      (let* ((buff32a (make-des-buffer 32))
	     (buff32b (make-des-buffer 32))
	     (buff64  (make-des-buffer 64)))
	 (if encrypt?
	     (let* ((keys1 (key-expansion key 0 subkey-len #t))
		    (keys2 (key-expansion key subkey-len subkey-len #f))
		    (keys3 (if (=fx nb-keys 3)
			       (key-expansion key
					      (*fx 2 subkey-len) subkey-len
					      #t)
			       keys1)))
		(instantiate::DES3-Param
		   (buff32a buff32a)
		   (buff32b buff32b)
		   (buff64 buff64)
		   (keys1 keys1)
		   (keys2 keys2)
		   (keys3 keys3)
		   (initial-permutation? initial-permutation?)))
	     (let* ((keys3-inv (key-expansion key 0 subkey-len #f))
		    (keys2-inv (key-expansion key subkey-len subkey-len #t))
		    (keys1-inv (if (=fx nb-keys 3)
				   (key-expansion key
						  (*fx 2 subkey-len) subkey-len
						  #f)
				   keys3-inv)))
		(instantiate::DES3-Param
		   (buff32a buff32a)
		   (buff32b buff32b)
		   (buff64 buff64)
		   (keys1 keys1-inv)
		   (keys2 keys2-inv)
		   (keys3 keys3-inv)
		   (initial-permutation? initial-permutation?)))))))

(define (DES3-key->encrypt-param key)
   (DES3-key->param key #t #t))
(define (DES3-key->encrypt-param-no-permutation key)
   (DES3-key->param key #t #f))
(define (DES3-key->decrypt-param key)
   (DES3-key->param key #f #t))
(define (DES3-key->decrypt-param-no-permutation key)
   (DES3-key->param key #f #f))

(register-cipher! 'des3
		  (instantiate::Block-Cipher
		     (name "Triple DES, Triple Data Encryption Algorithm (DES3, TDEA)")
		     (block-size (/fx *block-size* 8))
		     (preferred-key-length 14)
		     (encrypt! en/decrypt-DES3)
		     (decrypt! en/decrypt-DES3)
		     (key->encrypt-param DES3-key->encrypt-param)
		     (key->decrypt-param DES3-key->decrypt-param)))
(register-cipher! 'des3-np
		  (instantiate::Block-Cipher
		     (name "Triple DES, Triple Data Encryption Algorithm (DES3, TDEA). No initial Permutation.")
		     (block-size (/fx *block-size* 8))
		     (preferred-key-length 14)
		     (encrypt! en/decrypt-DES3)
		     (decrypt! en/decrypt-DES3)
		     (key->encrypt-param DES3-key->encrypt-param-no-permutation)
		     (key->decrypt-param DES3-key->decrypt-param-no-permutation)))

;; --- byte oriented: each byte represents a bit. -> faster...
(define (bit-ref str::bstring i) (char->integer (string-ref str i)))
(define (bit-set! str::bstring i val) (string-set! str i (integer->char-ur val)))

(define (make-des-buffer size) (make-string size #a000))
(define des-buffer-blit! blit-string!)

(define (tt b from len sp)
   (with-output-to-string (lambda () (display-des-buffer b from len sp))))
(define (display-des-buffer buff from len sp)
   (let loop ((i 0))
      (unless (=fx i len)
	 (when (zerofx? (remainderfx i sp)) (display " " ))
	 (display (bit-ref buff (+fx from i)))
	 (loop (+fx i 1)))))

;; len in bits.
(define (des-buffer-xor! target target-pos buffer buf-pos len)
   (let loop ((i 0))
      (unless (=fx i len)
	 (string-set! target (+fx target-pos i)
		      (char-xor (string-ref target (+fx target-pos i))
				(string-ref buffer (+fx buf-pos i))))
	 (loop (+fx i 1)))))

(define (copy-bits->des-buffer from::bstring from-pos-bits::long
			       to::bstring to-pos::long
			       nb-bits::long)
   (let ((nb-bytes (/fx nb-bits 8))
	 (from-pos (/fx from-pos-bits 8)))
      ;; from is native bits
      ;; to is our byte-oriented representation.
      (let loop ((i 0))
	 (unless (=fx i nb-bytes)
	    (let liip ((j 0)
		       (ci (char->integer (string-ref from (+fx from-pos i)))))
	       (unless (=fx j 8)
		  (bit-set! to (+fx to-pos (+fx (*fx i 8) j))
			    (bit-and #x01 (bit-rsh ci 7)))
		  (liip (+fx j 1) (bit-lsh ci 1))))
	    (loop (+fx i 1))))))

(define (copy-des-buffer->bits from::bstring from-pos::long
			       to::bstring to-pos-bits::long
			       nb-bits::long table)
   (let ((nb-bytes (/fx nb-bits 8))
	 (to-pos (/fx to-pos-bits 8)))
      ;; from is our byte-oriented representation.
      ;; to is native bits
      (let loop ((i 0))
	 (unless (=fx i nb-bytes)
	    (let liip ((j 0)
		       (ci 0))
	       (if (=fx j 8)
		   (string-set! to (+fx to-pos i) (integer->char ci))
		   (liip (+fx j 1)
			 (+fx (*fx ci 2)
			      (bit-ref from
				       (+fx from-pos
					    (table-ref table
						       (+fx (*fx i 8) j))))))))
	    
	    (loop (+fx i 1))))))

(define (permutation-copy source source-pos target target-pos table)
   (let loop ((i 0))
      (unless (=fx i (table-length table))
	 (bit-set! target (+fx target-pos i)
		   (bit-ref source (+fx source-pos (table-ref table i))))
	 (loop (+fx i 1)))))

;; ------ bit-oriented

; (define (octet-set! str i val) (string-set! str i (integer->char-ur val)))
; (define (octet-ref str i) (char->integer (string-ref str i)))
; (define (permutation-copy source target table)
;    (let loop ((i 0))
;       (unless (=fx i (/fx (table-length table) 8))
; 	 (let liip ((j 0)
; 		    (ci 0))
; 	    (cond
; 	       ((=fx j 8)
; 		(octet-set! target i ci))
; 	       (else
; 		(loop (+fx j 1)
; 		      (+fx (*fx ci 2)
; 			   (bit-ref source (table-ref table (+fx (*fx i 8) j))))))))
; 	 (loop (+fx i 1)))))


;; Note: most tables are taken from wikipedia. The start numbering the bits with 1.

(define-macro (make-table t)
   (apply string
	  (map (lambda (i)
		  (integer->char-ur (-fx i 1)))
	       ;; t is of form (quote (i0 i1 ...))
	       (cadr t))))

(define (table-length table) (string-length table))
(define (table-ref t i) (char->integer (string-ref t i)))

;; initial permutation
;; first bit of output is taken from 58. bit of input.
;; Note: everything starts with 1. However tables are automatically converted
;; so they start with 0. (make-table macro).
(define *IP-L* (make-table '(58  50  42  34  26  18  10  2
			     60  52  44  36  28  20  12  4
			     62  54  46  38  30  22  14  6
			     64  56  48  40  32  24  16  8)))
(define *IP-R* (make-table '(57  49  41  33  25  17  9   1
			     59  51  43  35  27  19  11  3
			     61  53  45  37  29  21  13  5
			     63  55  47  39  31  23  15  7)))

(define-macro (make-identity-table nb)
   (apply string (map integer->char (iota nb))))

(define *IP-id* (make-identity-table 32))

;; inversio of initial permutation.
;; Note: everything starts with 1. However tables are automatically converted
;; so they start with 0. (make-table macro).
(define *IP-1* (make-table '(40  8  48  16  56  24  64  32
			     39  7  47  15  55  23  63  31
			     38  6  46  14  54  22  62  30
			     37  5  45  13  53  21  61  29
			     36  4  44  12  52  20  60  28
			     35  3  43  11  51  19  59  27
			     34  2  42  10  50  18  58  26
			     33  1  41  9  49  17  57  25)))

;; expansion function
;; Note: everything starts with 1. However tables are automatically converted
;; so they start with 0. (make-table macro).
(define *E* (make-table '(32  1   2   3   4   5
			  4   5   6   7   8   9
			  8   9   10  11  12  13
			  12  13  14  15  16  17
			  16  17  18  19  20  21
			  20  21  22  23  24  25
			  24  25  26  27  28  29
			  28  29  30  31  32  1)))

;; Note: everything starts with 1. However tables are automatically converted
;; so they start with 0. (make-table macro).
(define *P* (make-table '(16 7  20 21
			  29 12 28 17
			  1  15 23 26
			  5  18 31 10
			  2  8  24 14
			  32 27 3  9
			  19 13 30 6
			  22 11 4  25)))

;; Note: everything starts with 1. However tables are automatically converted
;; so they start with 0. (make-table macro).
(define *PC1* (make-table '(;; L
			    57 49 41  33  25  17  9
			    1  58 50  42  34  26  18
			    10 2  59  51  43  35  27
			    19 11 3   60  52  44  36
			    ;; R
			    63  55  47  39  31  23  15
			    7   62  54  46  38  30  22
			    14  6   61  53  45  37  29
			    21  13  5   28  20  12  4)))

;; PC1-56 is used when the password comes as 56bit string.
(define *PC1-56* (make-table '(;; L
			       50 43 36 29 22 15 8 1 51 44 37 30 23 16 9 2 52
			       45 38 31 24 17 10 3 53 46 39 32
			       ;; R
			       56 49 42 35 28 21 14 7 55 48 41 34 27 20 13 6 54
			       47 40 33 26 19 12 5 25 18 11 4)))

;; Note: everything starts with 1. However tables are automatically converted
;; so they start with 0. (make-table macro).
(define *PC2* (make-table '(14  17  11  24  1   5
			    3   28  15  6   21  10
			    23  19  12  4   26  8
			    16  7   27  20  13  2
			    41  52  31  37  47  55
			    30  40  51  45  33  48
			    44  49  39  56  34  53
			    46  42  50  36  29  32)))

(define-macro (create-s-boxes . s-boxes)
   ;; s-boxes (as given here) are indexed as follows:
   ;;         x0000x, x0001x, ....      x1111x
   ;; 0yyyy0    14     4      ...
   ;; 0yyyy1     0     15     ...
   ;; 1yyyy0     4     1      ...
   ;; 1yyyy1    15     12     ...
   ;;
   ;; However we want to be able to index directly into the vector using
   ;; xyyyyx -> rearrange.

   (define (rearrange v)
      (let ((res (make-vector 64)))
	 (let loop ((i 0))
	    (cond
	       ((=fx i 64) res)
	       (else
		(let ((index (+ (bit-and #x0F (bit-rsh i 1))
				(bit-and #x20 i)
				(bit-lsh (bit-and i 1) 4))))
		   (vector-set! res i (vector-ref v index)))
		(loop (+fx i 1)))))))

   (define (list->strings l)
      (apply string (map integer->char-ur l)))

   (let* ((boxes1 (map (lambda (box)
			 ;; box is of form (quote (14 4 ...))
			  (rearrange (list->vector (cadr box))))
		      s-boxes))
	  (boxes2 (map vector->list boxes1))
	  (boxes3 (map list->strings boxes2)))
      (list 'quote (list->vector boxes3))))

(define *s-boxes*
   (create-s-boxes  '(14 4  13  1  2 15 11  8  3 10  6 12  5  9  0  7 
		      0  15  7  4 14  2 13  1 10  6 12 11  9  5  3  8 
		      4  1  14  8 13  6  2 11 15 12  9  7  3 10  5  0 
		      15 12  8  2  4  9  1  7  5 11  3 14 10  0  6 13)
		    '(15  1  8 14  6 11  3  4  9  7  2 13 12  0  5 10 
		      3  13  4  7 15  2  8 14 12  0  1 10  6  9 11  5 
		      0  14  7 11 10  4 13  1  5  8 12  6  9  3  2 15 
		      13  8 10  1  3 15  4  2 11  6  7 12  0  5 14  9)
		    '(10  0  9 14  6  3 15  5  1 13 12  7 11  4  2  8 
		      13  7  0  9  3  4  6 10  2  8  5 14 12 11 15  1 
		      13  6  4  9  8 15  3  0 11  1  2 12  5 10 14  7 
		      1  10 13  0  6  9  8  7  4 15 14  3 11  5  2 12)
		    '(7  13 14  3  0  6  9 10  1  2  8  5 11 12  4 15
		      13  8 11  5  6 15  0  3  4  7  2 12  1 10 14  9
		      10  6  9  0 12 11  7 13 15  1  3 14  5  2  8  4
		      3  15  0  6 10  1 13  8  9  4  5 11 12  7  2 14)
		    '(2  12  4  1  7 10 11  6  8  5  3 15 13  0 14  9
		      14 11  2 12  4  7 13  1  5  0 15 10  3  9  8  6
		      4   2  1 11 10 13  7  8 15  9 12  5  6  3  0 14
		      11  8 12  7  1 14  2 13  6 15  0  9 10  4  5  3)
		    '(12  1 10 15  9  2  6  8  0 13  3  4 14  7  5 11
		      10 15  4  2  7 12  9  5  6  1 13 14  0 11  3  8
		      9  14 15  5  2  8 12  3  7  0  4 10  1 13 11  6
		      4   3  2 12  9  5 15 10 11 14  1  7  6  0  8 13)
		    '(4 11  2 14 15  0  8 13  3 12  9  7  5 10  6  1
		      13 0 11  7  4  9  1 10 14  3  5 12  2 15  8  6
		      1  4 11 13 12  3  7 14 10 15  6  8  0  5  9  2
		      6 11 13  8  1  4 10  7  9  5  0 15 14  2  3 12)
		    '(13  2  8  4  6 15 11  1 10  9  3 14  5  0 12  7
		      1  15 13  8 10  3  7  4 12  5  6 11  0 14  9  2 
		      7  11  4  1  9 12 14  2  0  6 10 13 15  3  5  8 
		      2   1 14  7  4 10  8 13 15 12  9  0  3  5  6 11)))

(define (s-box box val)
   (char->integer (string-ref (vector-ref *s-boxes* box)
			      val)))

(define *rotations* '#(1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1))
(define (round-rotation i) (vector-ref *rotations* i))

(define (final-permutation from to)
   (permutation-copy from 0 to 0 *IP-1*))

(define (pc2 subkeys buffer)
   (permutation-copy subkeys 0 buffer 0 *PC2*))

(define *subkey-size* 28)
(define *round-key-size* 48)

(define (key-rotation! key by)
   ;; key has 1 byte we can "abuse" (at the end)
   (bit-set! key *key-size* (bit-ref key *subkey-size*))
   (bit-set! key *subkey-size* (bit-ref key 0))
   (when (=fx by 2)
      (bit-set! key (+fx *key-size* 1) (bit-ref key (+fx *subkey-size* 1)))
      (bit-set! key (+fx *subkey-size* 1) (bit-ref key 1)))
   (let loop ((i 0))
      (unless (=fx i *key-size*) 
	 (bit-set! key i (bit-ref key (+fx i by)))
	 (loop (+fx i 1)))))

(define (key-expansion key::bstring at::long len::long encrypt?::bool)
   ;; 16 rounds of 48 bit keys.
   (let ((res (make-des-buffer (*fx *round-key-size* *des-rounds*)))
	 (key-buffer (make-des-buffer (+fx 8 *key-size*)))) ;; one byte
      ;; copy the key temporarily into the res-buffer. Just for one instruction.
      (copy-bits->des-buffer key at res 0 len)
      (case len
	 ((56) ;; without parity-bits
	  (permutation-copy res 0 key-buffer 0 *PC1-56*))
	 ((64) ;; with parity-bits (that we don't look at)
	  (permutation-copy res 0 key-buffer 0 *PC1*))
	 (else (error "DES key expansion"
		      "Key must be either 56 or 64 bits long"
		      len)))

      (let loop ((i 0))
	 (unless (=fx i *des-rounds*)
	    (key-rotation! key-buffer (round-rotation i))
	    (let ((round-pos (if encrypt?
				 i
				 (-fx (-fx *des-rounds* i) 1))))
	       (permutation-copy key-buffer 0
				 res (*fx round-pos *round-key-size*)
				 *PC2*)
	       (loop (+fx i 1)))))
      res))

(define *block-size/2* (/fx *block-size* 2))

(define (do-round! L R i buffer64 keys)
   ;; expansion
   (permutation-copy R 0 buffer64 0 *E*)
   ;; key mixing
   (des-buffer-xor! buffer64 0 keys (*fx *round-key-size* i) *round-key-size*)
   ;; substitution
   (let loop ((i 0))
      (unless (=fx i 8)
	 (let* ((b0 (bit-ref buffer64 (+fx (*fx i 6) 0)))
		(b1 (bit-ref buffer64 (+fx (*fx i 6) 1)))
		(b2 (bit-ref buffer64 (+fx (*fx i 6) 2)))
		(b3 (bit-ref buffer64 (+fx (*fx i 6) 3)))
		(b4 (bit-ref buffer64 (+fx (*fx i 6) 4)))
		(b5 (bit-ref buffer64 (+fx (*fx i 6) 5)))
		(s-box-index (+fx+ (bit-lsh b0 5)
				   (bit-lsh b1 4)
				   (bit-lsh b2 3)
				   (bit-lsh b3 2)
				   (bit-lsh b4 1)
				   b5))
		(subs (s-box i s-box-index)))
	    (bit-set! buffer64 (+fx (*fx i 4) 0) (bit-and 1 (bit-rsh subs 3)))
	    (bit-set! buffer64 (+fx (*fx i 4) 1) (bit-and 1 (bit-rsh subs 2)))
	    (bit-set! buffer64 (+fx (*fx i 4) 2) (bit-and 1 (bit-rsh subs 1)))
	    (bit-set! buffer64 (+fx (*fx i 4) 3) (bit-and 1 subs))
	    (loop (+fx i 1)))))
   ;; we do P and xor at the same time. (just more efficient).
   (let loop ((i 0))
      (unless (=fx i *block-size/2*)
	 (bit-set! L i (bit-xor (bit-ref L i)
				(bit-ref buffer64 (table-ref *P* i))))
	 (loop (+fx i 1)))))

(define-inline (do-des-rounds! L R keys buffer64)
   (let loop ((i 0)
	      (L L)
	      (R R))
      (unless (=fx i *des-rounds*)
	 (do-round! L R i buffer64 keys)
	 (loop (+fx i 1) R L))))

(define (init! in-block in-pos L R buffer64 permute?)
   (if permute?
       (begin
	  (copy-bits->des-buffer in-block in-pos buffer64 0 *block-size*)
	  (permutation-copy buffer64 0 L 0 *IP-L*)
	  (permutation-copy buffer64 0 R 0 *IP-R*))
       (let ((bs/2 *block-size/2*))
	  (copy-bits->des-buffer in-block in-pos L 0 bs/2)
	  (copy-bits->des-buffer in-block (+fx in-pos bs/2) R 0 bs/2))))

(define (finalize out-block out-pos L R buffer64 permute?)
   (if permute?
       (begin
	  (des-buffer-blit! R 0 buffer64 0 *block-size/2*)
	  (des-buffer-blit! L 0 buffer64 *block-size/2* *block-size/2*)
	  (copy-des-buffer->bits buffer64 0
				 out-block out-pos
				 *block-size* *IP-1*))
       (begin
	  (copy-des-buffer->bits R 0
				 out-block out-pos
				 *block-size/2* *IP-id*)
	  (copy-des-buffer->bits L 0
				 out-block (+fx out-pos *block-size/2*)
				 *block-size/2* *IP-id*))))
   
(define (en/decrypt-DES in-block::bstring in-pos::long
			out-block::bstring out-pos::long
			param::DES-Param)
   (with-access::DES-Param param (buff32a buff32b buff64 keys initial-permutation?)
      (let ((L buff32a)
	    (R buff32b)
	    (buffer64 buff64)
	    (permute? initial-permutation?))
	 (init! in-block in-pos L R buffer64 permute?)
	 (do-des-rounds! L R keys buffer64)
	 (finalize out-block out-pos L R buffer64 permute?))))

(define (en/decrypt-DES3 in-block::bstring in-pos::long
			 out-block::bstring out-pos::long
			 param::DES3-Param)
   (with-access::DES3-Param param (buff32a buff32b buff64 keys1 keys2 keys3
					   initial-permutation?)
      (let ((L buff32a)
	    (R buff32b)
	    (buffer64 buff64)
	    (permute? initial-permutation?))
	 (init! in-block in-pos L R buffer64 permute?)
	 (do-des-rounds! L R keys1 buffer64)
	 (do-des-rounds! R L keys2 buffer64)
	 (do-des-rounds! L R keys3 buffer64)
	 (finalize out-block out-pos L R buffer64 permute?))))
