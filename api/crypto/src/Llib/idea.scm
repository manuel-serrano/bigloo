(module __crypto-idea
   (import __crypto-block-ciphers
	   __crypto-util))

(define *block-size* 8)

(define (key->decrypt-param key)
   (inverse-keys (key-expansion key)))

(register-cipher! 'idea
		  (instantiate::Block-Cipher
		     (name "IDEA (International Data Encryption Algorithm)")
		     (block-size *block-size*)
		     (preferred-key-length 16)
		     (encrypt! encrypt-block)
		     (decrypt! encrypt-block)
		     (key->encrypt-param key-expansion)
		     (key->decrypt-param key->decrypt-param)))

(define *rounds* 8)

;; note: the keys for each round start at "1" (to be consistent with the
;; description of Wikipedia (as of 22/07/2009)
(define (key keys round i)
   (vector-ref keys (+fx (*fx 6 round) (-fx i 1))))

(define (key-expansion::vector key::bstring)
   (define (extract-subkey-from-key str at)
      (let* ((i (*fx 2 at))
	     (c1 (string-ref str i))
	     (c2 (string-ref str (+fx i 1)))
	     (i1 (char->integer c1))
	     (i2 (char->integer c2)))
	 (+fx (*fx i1 256) i2)))

   (define (extract-subkey subkeys i)
      ;; get the subkeys from the previous row, where rows are 16byte blocks.
      ;; Each row is shifted by 25bits.
      (let* ((previous-row (*fx 8 (-fx (/fx i 8) 1)))
	     (off1 (modulofx (+fx i 1) 8))
	     (off2 (modulofx (+fx i 2) 8))
	     (subk1 (vector-ref subkeys (+fx previous-row off1)))
	     (subk2 (vector-ref subkeys (+fx previous-row off2))))
	 (bit-and #xFFFF
		  (+fx (bit-lsh subk1 9)
		       (bit-rsh subk2 7)))))

   (when (not (=fx (string-length key) 16))
      (error 'idea
	     "Key must be 128 bits long"
	     (*fx 8 (string-length key))))
   ;; each round uses 6 subkeys.
   ;; last half-round uses 4 subkeys.
   (let* ((nb-sub-keys (+fx (*fx 6 *rounds*) 4))
	  (res (make-vector nb-sub-keys)))
      ;; first 8 keys are verbatim copies.
      (let loop ((i 0))
	 (when (<fx i 8)
	    (vector-set! res i (extract-subkey-from-key key i))
	    (loop (+fx i 1))))

      (let loop ((j 8))
	 (when (<fx j nb-sub-keys)
	    (vector-set! res j (extract-subkey res j))
	    (loop (+fx j 1))))

      res))

(define (inverse-keys::vector keys::vector)
   (define (add-inv k)
      (bit-and #xFFFF (-fx #x10000 k)))
   (define (mult-inv k)
      (cond
	 ((<fx k 2)
	  k)
	 (else
	     (let loop ((x k)  (y #x10001)
			(u1 1) (u2 0)
			(v1 0) (v2 1))
		(cond
		   ((and (zerofx? y) (>=fx u1 0))
		    u1)
		   ((zerofx? y)
		    (bit-and #xFFFF (+fx #x10001 u1)))
		   (else
		    (let ((q (quotientfx x y))
			  (r (remainderfx x y)))
		       (loop y r u2 (-fx u1 (*fx q u2)) v2 (-fx v1 (*fx q v2))))))))))

   ;; round 0 starts at 4 for setting.
   (define (inv-key-set! i offset keys k)
      (vector-set! keys (+fx (*fx 6 i) (+fx offset 3)) k))
   
   (let ((res (make-vector (vector-length keys))))
      (vector-set! res 0 (mult-inv (key keys 8 1)))
      (vector-set! res 1 (add-inv  (key keys 8 2)))
      (vector-set! res 2 (add-inv  (key keys 8 3)))
      (vector-set! res 3 (mult-inv (key keys 8 4)))
      (let loop ((i 0)
		 (j 7))
	 (when (<fx i (-fx *rounds* 1)) ;; last 6 subkeys are different.
	    (inv-key-set! i 1 res (key keys j 5))
	    (inv-key-set! i 2 res (key keys j 6))
	    (inv-key-set! i 3 res (mult-inv (key keys j 1)))
	    (inv-key-set! i 4 res (add-inv (key keys j 3)))
	    (inv-key-set! i 5 res (add-inv (key keys j 2)))
	    (inv-key-set! i 6 res (mult-inv (key keys j 4)))
	    (loop (+fx i 1) (-fx j 1))))
      (inv-key-set! 7 1 res (key keys 0 5))
      (inv-key-set! 7 2 res (key keys 0 6))
      (inv-key-set! 7 3 res (mult-inv (key keys 0 1)))
      (inv-key-set! 7 4 res (add-inv (key keys 0 2)))
      (inv-key-set! 7 5 res (add-inv (key keys 0 3)))
      (inv-key-set! 7 6 res (mult-inv (key keys 0 4)))

      res))

;; there are definitely better faster ways to do this, but this is the easiest
;; way (if we want to stay portable).
;; this way we only require longs to have at least 32 bits.
(define (idea* a b)
   (cond
      ((=fx a 0) (bit-and #xFFFF (-fx #x10001 b)))
      ((=fx b 0) (bit-and #xFFFF (-fx #x10001 a)))
      (else
       (let* ((ae (fixnum->elong a))
	      (be (fixnum->elong b))
	      (ae*be (*elong ae be))
	      (rl (elong->fixnum (bit-andelong ae*be #exFFFF)))
	      (rh (elong->fixnum (bit-andelong #exFFFF
					       (bit-rshelong ae*be 16))))
	      (diff (-fx rl rh)))
	  (if (<fx diff 0)
	      (bit-and #xFFFF (+fx #x10001 diff))
	      (bit-and #xFFFF diff))))))

;; in theory 'idea*' is equivalent to 'idea2*'.
(define (idea2* a b)
   (cond
      ((=fx a 0) (idea2* #x10000 b))
      ((=fx b 0) (idea2* a #x10000))
      (else
       (let* ((aa (fixnum->llong a))
	      (bb (fixnum->llong b))
	      (t (remainderllong (* aa bb)
				 #lx10001))
	      (f (llong->fixnum t)))
	  (bit-and #xFFFF f)))))

(define (idea+ a b)
   (bit-and #xFFFF (+fx a b)))

(define (idea^ a b)
   (bit-xor a b))

(define (do-half-round a b c d round keys)
   (values (idea* a (key keys round 1))
	   (idea+ c (key keys round 2))
	   (idea+ b (key keys round 3))
	   (idea* d (key keys round 4))))

;; one round.
;; I have copied the image from wikipedia to the source-tree (idea-round.png).
;; red: idea*
;; green: idea+
;; blue: idea^
(define (do-round a0 b0 c0 d0 round keys)
   ;; numbers represent the depth in picture.
   ;; the middle columns are x and y.

   (let* ((a1 (idea* a0 (key keys round 1)))
	  (b1 (idea+ b0 (key keys round 2)))
	  (c1 (idea+ c0 (key keys round 3)))
	  (d1 (idea* d0 (key keys round 4)))
	  ;; next line (the blue cross)
	  (x2 (idea^ a1 c1))
	  ;; next line
	  (y3 (idea^ b1 d1))
	  ;;
	  (x4 (idea* x2 (key keys round 5)))
	  (y4 (idea+ x4 y3))
	  ;;
	  (y5 (idea* y4 (key keys round 6)))
	  (x5 (idea+ x4 y5))
	  ;; 
	  (a6 (idea^ a1 y5))
	  (c6 (idea^ c1 y5))
	  ;;
	  (b7 (idea^ b1 x5))
	  (d7 (idea^ d1 x5)))

      (values a6 c6 b7 d7)))

(define (encrypt-block from::bstring from-pos::long to::bstring to-pos::long
		       keys::vector)
   (define (read-uint16-at str i)
      (+fx (*fx (char->integer (string-ref str i)) 256)
	   (char->integer (string-ref str (+fx i 1)))))
   (define (write-uint16-at str i val)
      (string-set! str i (integer->char-ur (/fx val 256)))
      (string-set! str (+fx i 1) (integer->char-ur (bit-and #xFF val))))

   (let loop ((i 0)
	      (a (read-uint16-at from (+fx from-pos 0)))
	      (b (read-uint16-at from (+fx from-pos 2)))
	      (c (read-uint16-at from (+fx from-pos 4)))
	      (d (read-uint16-at from (+fx from-pos 6))))
      (cond
	 ((=fx i *rounds*)
	  (receive (a b c d)
	     (do-half-round a b c d i keys)
	     (write-uint16-at to (+fx to-pos 0) a)
	     (write-uint16-at to (+fx to-pos 2) b)
	     (write-uint16-at to (+fx to-pos 4) c)
	     (write-uint16-at to (+fx to-pos 6) d)))
	 (else
	  (receive (a_ b_ c_ d_)
	     (do-round a b c d i keys)
	     (loop (+fx i 1)
		   a_ b_ c_ d_))))))
