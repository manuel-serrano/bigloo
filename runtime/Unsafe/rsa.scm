;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/rsa.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Chris Veness                                      */
;*    Creation    :  Thu Jun  5 08:00:03 2008                          */
;*    Last change :  Tue Jun 17 08:15:39 2014 (serrano)                */
;*    Copyright   :  2005-14 Chris Veness                              */
;*    -------------------------------------------------------------    */
;*    Message encryption and decryption based on the RSA asymmetric    */
;*    cipher.                                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rsa

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
	   __r4_numbers_6_5_flonum_dtoa
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
	   __r4_output_6_10_3
	   __r5_control_features_6_4
	   __structure
	   __mmap
	   __foreign
	   __error
	   __evenv
	   __os
	   __srfi4
	   __param)

   (export (make-rsa-key-pair #!key (size 1024) show-trace)
	   (public-rsa-key rsa-key-pair)
	   (private-rsa-key rsa-key-pair)
	   
	   (rsa-key= rsa-key1 rsa-key2)
	   
;* 	   (rsa-key->list rsa-key)                                     */
;* 	   (list->rsa-key lst)                                         */
	   
	   (PKCS1-pad u8vect final-len)
	   (PKCS1-unpad u8vect)
	   
	   (rsa-encrypt-u8vector u8vect rsa-key final-len)
	   (rsa-decrypt-u8vector u8vect rsa-key)
	   
	   (rsa-encrypt-string ::bstring ::obj)
	   (rsa-decrypt-string ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    u8vector-ref ...                                                 */
;*---------------------------------------------------------------------*/
(define-macro (u8vector-ref vec i)
   `(uint8->fixnum ((@ u8vector-ref __srfi4) ,vec ,i)))

;*---------------------------------------------------------------------*/
;*    rsa-key ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct rsa-key size modulus exponent)

;*---------------------------------------------------------------------*/
;*    random-prime ...                                                 */
;*---------------------------------------------------------------------*/
(define (random-prime start::bignum end::bignum show-trace)
   
   (define (product-of-primes n)
      (let loop ((n (-fx n 1)) (p #z2) (i 3))
	 (cond
	    ((=fx n 0)
	     p)
	    ((=bx #z1 (gcdbx (fixnum->bignum i) p))
	     (loop (-fx n 1) (*bx p (fixnum->bignum i)) (+fx i 2)))
	    (else
	     (loop n p (+fx i 2))))))
   
   (when show-trace
      (display ".")
      (flush-output-port (current-output-port)))
   
   (let ((prod-small-primes (product-of-primes 300)))
      
      (define (likely-prime? n)
	 (and (=bx #z1 (gcdbx n prod-small-primes))
	      (=bx #z1 (expt-modbx #z2 (-bx n #z1) n))))
      
      (let loop ((i 1))
	 (when show-trace
	    (display "+")
	    (flush-output-port (current-output-port)))
	 (let* ((x (+bx start (randombx (-bx end start))))
		(n (if (oddbx? x) x (+bx x #z1))))
	    (if (or (>=bx n end) (not (likely-prime? n)))
		(loop (+ i 1))
		n)))))

;*---------------------------------------------------------------------*/
;*    gcd-ext ...                                                      */
;*---------------------------------------------------------------------*/
(define (gcd-ext x y)
   (let loop ((x x)
	      (y y)
	      (u1 #z1)
	      (u2 #z0)
	      (v1 #z0)
	      (v2 #z1))
      (if (zerobx? y)
	  (list x u1 v1)
	  (let ((q (quotientbx x y))
		(r (remainderbx x y)))
	     (loop y r u2 (-bx u1 (*bx q u2)) v2 (-bx v1 (*bx q v2)))))))

;*---------------------------------------------------------------------*/
;*    mod-inverse ...                                                  */
;*---------------------------------------------------------------------*/
(define (mod-inverse x b)
   (let* ((x1 (modulobx x b))
	  (g (gcd-ext x1 b)))
      (if (not (=bx (car g) #z1))
	  (error 'mod-inverse
		 "internal error, numbers are not relatively prime"
		 (cons x b))
	  (modulobx (cadr g) b))))

;*---------------------------------------------------------------------*/
;*    make-rsa-key-pair ...                                            */
;*---------------------------------------------------------------------*/
(define (make-rsa-key-pair #!key (size 1024) show-trace)
   (let* ((size-p (quotientfx size 2))
	  (start-p (exptbx #z2 (fixnum->bignum size-p)))
	  (end-p (*bx start-p #z2))
	  (p (random-prime start-p end-p show-trace))
	  (start-n (exptbx #z2 (fixnum->bignum size)))
	  (end-n (*bx start-n #z2))
	  (start-q (+bx (quotientbx (-bx start-n #z1) p) #z1))
	  (end-q (quotientbx end-n p)))
      (let loop ()
	 (let ((q (random-prime start-q end-q show-trace)))
	    (if (not (=bx (gcdbx p q) #z1))
		(loop)
		(let* ((n (*bx p q))
		       (p-1 (-bx p #z1))
		       (q-1 (-bx q #z1))
		       (phi (quotientbx (*bx p-1 q-1) (gcdbx p-1 q-1)))
		       (e (let loop ((e #z65537))
			     (if (=bx #z1 (gcdbx e phi))
				 e
				 (loop (+bx e #z2)))))
		       (d (mod-inverse e phi)))
		   (when show-trace (newline))
		   ;; public and private keys
		   (cons (rsa-key size n e) 
			 (rsa-key size n d))))))))

;*---------------------------------------------------------------------*/
;*    public-rsa-key ...                                               */
;*---------------------------------------------------------------------*/
(define (public-rsa-key rsa-key-pair)
   (car rsa-key-pair))

;*---------------------------------------------------------------------*/
;*    private-rsa-key ...                                              */
;*---------------------------------------------------------------------*/
(define (private-rsa-key rsa-key-pair)
   (cdr rsa-key-pair))

;*---------------------------------------------------------------------*/
;*    rsa-key= ...                                                     */
;*---------------------------------------------------------------------*/
(define (rsa-key= rsa-key1 rsa-key2)
   (and (=fx (rsa-key-size rsa-key1) (rsa-key-size rsa-key2))
	(=bx (rsa-key-modulus rsa-key1) (rsa-key-modulus rsa-key2))
	(=bx (rsa-key-exponent rsa-key1) (rsa-key-exponent rsa-key2))))

;*---------------------------------------------------------------------*/
;*    rsa-key->list ...                                                */
;*---------------------------------------------------------------------*/
;* (define (rsa-key->list rsa-key)                                     */
;*    (list (rsa-key-size rsa-key)                                     */
;* 	 (bignum->base64-string (rsa-key-modulus rsa-key))             */
;* 	 (bignum->base64-string (rsa-key-exponent rsa-key))))          */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    list->rsa-key ...                                                *} */
;* {*---------------------------------------------------------------------*} */
;* (define (list->rsa-key lst)                                         */
;*    (if (not (and (list? lst) (= 3 (length lst))))                   */
;*        (error 'list->rsa-key "improperly formatted RSA key" lst)    */
;*        (let* ((size (car lst))                                      */
;* 	      (modulus-str (cadr lst))                                 */
;* 	      (exponent-str (caddr lst))                               */
;* 	      (modulus (and (string? modulus-str)                      */
;* 			    (base64-string->bignum modulus-str)))      */
;* 	      (exponent (and (string? exponent-str)                    */
;* 			     (base64-string->bignum exponent-str))))   */
;* 	  (if (not (and (memv size '(512 1024 2048))                   */
;* 			modulus                                        */
;* 			exponent))                                     */
;* 	      (error 'list->rsa-key "improperly formatted RSA key" list) */
;* 	      (rsa-key size modulus exponent)))))                 */

;*---------------------------------------------------------------------*/
;*    PKCS1-pad ...                                                    */
;*---------------------------------------------------------------------*/
(define (PKCS1-pad u8vect final-len)
   (let* ((len (u8vector-length u8vect))
	  (n (- final-len (+ len 3))))
      (if (< n 8)
	  (error 'PKCS1-pad
		 "not enough space is available for proper padding"
		 n)
	  (let ((pad
		 (let loop ((lst '(0)) (i 0))
		    (if (< i n)
			(loop (cons (+ 1 (random 255)) lst) (+ i 1))
			(list->u8vector (cons 0 (cons 2 lst)))))))
	     (u8vector-append pad u8vect)))))

;*---------------------------------------------------------------------*/
;*    PKCS1-unpad ...                                                  */
;*---------------------------------------------------------------------*/
(define (PKCS1-unpad u8vect)
   
   (define (err)
      (error 'PKCS1-unpad "improperly padded message" u8vect))
   
   (let ((len (u8vector-length u8vect)))
      (let loop1 ((i 0))
	 (if (>= i len)
	     (err)
	     (let ((x (u8vector-ref u8vect i)))
		(cond ((= x 0)
		       (loop1 (+ i 1)))
		      ((not (= x 2))
		       (err))
		      (else
		       (let loop2 ((j (+ i 1)))
			  (if (>= j len)
			      (err)
			      (let ((x (u8vector-ref u8vect j)))
				 (cond
				    ((not (= x 0))
				     (loop2 (+ j 1)))
				    ((< (- j i) 8)
				     ;; need at least 8 byte pad
				     (err))
				    (else
				     (subu8vector
				      u8vect (+ j 1) len)))))))))))))

;*---------------------------------------------------------------------*/
;*    rsa-crypt ...                                                    */
;*---------------------------------------------------------------------*/
(define (rsa-crypt message rsa-key)
   ;; encryption and decryption
   (expt-modbx message (rsa-key-exponent rsa-key) (rsa-key-modulus rsa-key)))

;*---------------------------------------------------------------------*/
;*    rsa-encrypt-u8vector ...                                         */
;*---------------------------------------------------------------------*/
(define (rsa-encrypt-u8vector u8vect rsa-key final-len)
   (bignum->u8vector
    (rsa-crypt
     (u8vector->bignum (PKCS1-pad u8vect final-len))
     rsa-key)))

;*---------------------------------------------------------------------*/
;*    rsa-decrypt-u8vector ...                                         */
;*---------------------------------------------------------------------*/
(define (rsa-decrypt-u8vector u8vect rsa-key)
   (PKCS1-unpad
    (bignum->u8vector
     (rsa-crypt
      (u8vector->bignum u8vect)
      rsa-key))))

;*---------------------------------------------------------------------*/
;*    get-u8vector-password ...                                        */
;*    -------------------------------------------------------------    */
;*    Implementation of a subset of RFC 2898 (PKCS #5: Password-Based  */
;*    Cryptography Specification Version 2.0).                         */
;*---------------------------------------------------------------------*/
(define (get-u8vector-password password)
   (if (string? password)
       (list->u8vector (map! char->integer (string->list password)))
       password))

;*---------------------------------------------------------------------*/
;*    u8vector-xor ...                                                 */
;*---------------------------------------------------------------------*/
(define (u8vector-xor u8vect1 u8vect2)
   (let* ((len (u8vector-length u8vect1))
	  (result (make-u8vector len)))
      (let loop ((i (- len 1)))
	 (if (>= i 0)
	     (begin
		(u8vector-set!
		 result
		 i
		 (bit-xor (u8vector-ref u8vect1 i) (u8vector-ref u8vect2 i)))
		(loop (- i 1)))
	     result))))

;*---------------------------------------------------------------------*/
;*    make-salt ...                                                    */
;*    -------------------------------------------------------------    */
;*    default is 64 bit salt                                           */
;*---------------------------------------------------------------------*/
;* (define (make-salt #!optional (len 8))                              */
;*    (make-random-u8vector len))                                      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    PBKDF1 ...                                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define (PBKDF1 password salt iter-count len)                       */
;*    (if (> len 20)                                                   */
;*        (error 'PBKDF1 "derived key too long" len)                   */
;*        (let ((algorithm (if (> len 16) 'sha-1 'md5))                */
;* 	     (password (get-u8vector-password password)))              */
;* 	  (let loop ((k 0)                                             */
;* 		     (t (u8vector-append password salt)))              */
;* 	     (if (< k iter-count)                                      */
;* 		 (loop (+ k 1)                                         */
;* 		       (digest-u8vector t algorithm 'u8vector))        */
;* 		 (subu8vector t 0 len))))))                            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    PBKDF2 ...                                                       *} */
;* {*---------------------------------------------------------------------*} */
;* (define (PBKDF2 password salt iter-count len)                       */
;*                                                                     */
;*    (define (u32-be i)                                               */
;*       (u8vector                                                     */
;*        (bit-and #xff (bit-rsh i 24))                                */
;*        (bit-and #xff (bit-rsh i 16))                                */
;*        (bit-and #xff (bit-rsh i  8))                                */
;*        (bit-and #xff i)))                                           */
;*                                                                     */
;*    (define (PRF password salt)                                      */
;*       ;; TODO: should really be using HMAC-SHA-1 (see RFC 2898)     */
;*       (digest-u8vector                                              */
;*        (u8vector-append password salt)                              */
;*        'sha-1                                                       */
;*        'u8vector))                                                  */
;*                                                                     */
;*    (define (F password salt iter-count i)                           */
;*       (let ((x (PRF password (u8vector-append salt (u32-be i)))))   */
;* 	 (let loop ((k 1) (u x) (t x))                                 */
;* 	    (if (< k iter-count)                                       */
;* 		(let ((x (PRF password u)))                            */
;* 		   (loop (+ k 1) x (u8vector-xor t x)))                */
;* 		t))))                                                  */
;*                                                                     */
;*    (if (> len 65536) ;; arbitrary limit, more than enough for most purposes */
;*        (error 'PBKDF2 "derived key too long" len)                   */
;*        (let ((password (get-u8vector-password password)))           */
;* 	  (let loop ((i 1)                                             */
;* 		     (n len)                                           */
;* 		     (lst '()))                                        */
;* 	     (if (> n 0)                                               */
;* 		 (let* ((t (F password salt iter-count i))             */
;* 			(x (u8vector-length t)))                       */
;* 		    (loop (+ i 1)                                      */
;* 			  (- n x)                                      */
;* 			  (cons (if (> n x)                            */
;* 				    t                                  */
;* 				    (subu8vector t 0 n))               */
;* 				lst)))                                 */
;* 		 (apply-u8vector-append (reverse lst)))))))            */

;*---------------------------------------------------------------------*/
;*    expt-modbx ...                                                   */
;*---------------------------------------------------------------------*/
(define (expt-modbx x y m)
   
   (define (expt-mod n e m)
      (cond
	 ((zerobx? e)
	  #z1)
	 ((evenbx? e)
	  (expt-mod (modulobx (*bx n n) m) (quotientbx e #z2) m))
	 (else
	  (modulobx (*bx n (expt-mod n (-bx e #z1) m)) m))))
   
   (expt-mod x y m))

;*---------------------------------------------------------------------*/
;*    u8vector-append ...                                              */
;*---------------------------------------------------------------------*/
(define (u8vector-append::u8vector v1::u8vector v2::u8vector)
   (let* ((len1 (u8vector-length v1))
	  (len2 (u8vector-length v2))
	  (len (+fx len1 len2))
	  (res (make-u8vector len)))
      (let loop ((i 0))
	 (if (<fx i len1)
	     (begin
		(u8vector-set! res i (u8vector-ref v1 i))
		(loop (+fx i 1)))
	     (let loop ((j 0))
		(if (<fx j len2)
		    (begin
		       (u8vector-set! res (+fx j i) (u8vector-ref v2 j))
		       (loop (+fx j 1)))
		    res))))))

;*---------------------------------------------------------------------*/
;*    subu8vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (subu8vector::u8vector v::u8vector start::long end::long)
   (let* ((len (-fx end start))
	  (res (make-u8vector len)))
      (let loop ((i start))
	 (if (<fx i end)
	     (begin
		(u8vector-set! res (-fx i start) (u8vector-ref v i))
		(loop (+fx i 1)))
	     res))))

;*---------------------------------------------------------------------*/
;*    apply-u8vector-append ...                                        */
;*---------------------------------------------------------------------*/
(define (apply-u8vector-append lst::pair-nil)
   (let* ((len (apply + (map u8vector-length lst)))
	  (res (make-u8vector len)))
      (let loop ((lst lst)
		 (i 0))
	 (if (null? lst)
	     res
	     (let* ((v (car lst))
		    (len (u8vector-length v)))
		(let liip ((j 0))
		   (if (=fx j len)
		       (loop (cdr lst) (+fx i len))
		       (begin
			  (u8vector-set! res (+fx i j) (u8vector-ref v j))
			  (liip (+fx j 1))))))))))

;*---------------------------------------------------------------------*/
;*    bignum->u8vector ...                                             */
;*---------------------------------------------------------------------*/
(define (bignum->u8vector::u8vector n::bignum)
   
   (define (vector-size n)
      (let loop ((size 1)
		 (acc #z255))
	 (if (>bx n acc)
	     (loop (+fx size 1) (*bx acc #z255))
	     size)))
   
   (let* ((size (vector-size n))
	  (res (make-u8vector size)))
      (let loop ((i 0)
		 (v n))
	 (if (=fx i size)
	     res
	     (begin
		(u8vector-set! res i (bignum->fixnum (remainderbx v #z256)))
		(loop (+fx i 1) (/bx v #z256)))))))

;*---------------------------------------------------------------------*/
;*    u8vector->bignum ...                                             */
;*---------------------------------------------------------------------*/
(define (u8vector->bignum::bignum u8vect::u8vector)
   (let loop ((i (-fx (u8vector-length u8vect) 1))
	      (a #z0))
      (if (=fx i -1)
	  a
	  (let ((d (fixnum->bignum (u8vector-ref u8vect i))))
	     (loop (-fx i 1) (+bx (*bx a #z256) d))))))

;*---------------------------------------------------------------------*/
;*    make-random-u8vector ...                                         */
;*---------------------------------------------------------------------*/
(define (make-random-u8vector len)
   (define (random-fill-u8vector! vec)
      (let loop ((i (-fx (u8vector-length vec) 1)))
	 (if (=fx i -1)
	     vec
	     (begin
		(u8vector-set! vec i (random 256))
		(loop (-fx i 1))))))
   (define (file-fill-u8vector! vec p)
      (let loop ((i (-fx (u8vector-length vec) 1)))
	 (if (=fx i -1)
	     vec
	     (begin
		(u8vector-set! vec i (read-byte p))
		(loop (-fx i 1))))))
   (let ((res (make-u8vector len)))
      (if (file-exists? "/dev/random")
	  (let ((p (open-input-file "/dev/random")))
	     (if (input-port? p)
		 (file-fill-u8vector! res p)
		 (random-fill-u8vector! res)))
	  (random-fill-u8vector! res))))

;*---------------------------------------------------------------------*/
;*    rsa-encrypt-string ...                                           */
;*---------------------------------------------------------------------*/
(define (rsa-encrypt-string str key)
   (apply string
	  (map! integer->char
		(u8vector->list
		 (rsa-encrypt-u8vector
		  (list->u8vector (map! char->integer (string->list str)))
		  key
		  (+fx (string-length str) 12)))))) ;; padding

;*---------------------------------------------------------------------*/
;*    rsa-decrypt-string ...                                           */
;*---------------------------------------------------------------------*/
(define (rsa-decrypt-string str key)
   (apply string
	  (map! integer->char
		(u8vector->list
		 (rsa-decrypt-u8vector
		  (list->u8vector (map! char->integer (string->list str)))
		  key)))))

