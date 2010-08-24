;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/sha1.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 26 08:40:27 2008                          */
;*    Last change :  Tue Aug 24 11:46:03 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SHA-1 Bigloo implementation                                      */
;*    -------------------------------------------------------------    */
;*    Based on a JavaScript implementation by Chris Veness             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sha1
   
   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
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
	   __srfi4)
   
   (import __param
	   __hmac)
   
   (export (sha1sum::bstring ::obj)
	   (sha1sum-mmap::bstring ::mmap)
	   (sha1sum-string::bstring ::bstring)
	   (sha1sum-port::bstring ::input-port)
	   (sha1sum-file::bstring ::bstring)

	   (hmac-sha1sum-string::bstring ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    for ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (for decl test incr . body)
   (let ((loop (gensym (symbol-append 'loop- (car decl) '-))))
      `(let ,loop (,decl)
	    (when ,test
	       ,@body
	       (,loop ,incr)))))

;*---------------------------------------------------------------------*/
;*    u32 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u32 hi lo)
   `(bit-or (bit-lsh ,hi 16) ,lo))

;*---------------------------------------------------------------------*/
;*    u16 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u16 hi lo)
   `(bit-or (bit-lsh ,hi 8) ,lo))

;*---------------------------------------------------------------------*/
;*    u32-hi ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (u32-hi w)
   `(bit-ursh ,w 16))

;*---------------------------------------------------------------------*/
;*    u32-low ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (u32-lo w)
   `(bit-and ,w (-fx (bit-lsh 1 16) 1)))

;*---------------------------------------------------------------------*/
;*    rotl32 ...                                                       */
;*---------------------------------------------------------------------*/
(define (rotl32::ulong x::ulong n::int)
   (bit-or (bit-lsh x n) (bit-ursh x (-fx 32 n))))

;*---------------------------------------------------------------------*/
;*    u32->string ...                                                  */
;*---------------------------------------------------------------------*/
(define (u32->string w::ulong)
   (let* ((r (make-string 8 #\0))
	  (s1 (integer->string (u32-hi w) 16))
	  (l1 (string-length s1))
	  (s2 (integer->string (u32-lo w) 16))
	  (l2 (string-length s2)))
      (blit-string! s1 0 r (-fx 4 l1) l1)
      (blit-string! s2 0 r (+fx 4 (-fx 4 l2)) l2)
      r))

;*---------------------------------------------------------------------*/
;*    u32-fill! ...                                                    */
;*---------------------------------------------------------------------*/
(define (u32-fill! str offset w::ulong)
   (let* ((s1 (integer->string (u32-hi w) 16))
	  (l1 (string-length s1))
	  (s2 (integer->string (u32-lo w) 16))
	  (l2 (string-length s2)))
      (blit-string! s1 0 str (+fx offset (-fx 4 l1)) l1)
      (blit-string! s2 0 str (+fx offset (+fx 4 (-fx 4 l2))) l2)))
   
;*---------------------------------------------------------------------*/
;*    u160->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (u160->string w0::ulong w1::ulong w2::ulong w3::ulong w4::ulong)
   (let ((r (make-string 40 #\0)))
      (u32-fill! r 0 w0)
      (u32-fill! r 8 w1)
      (u32-fill! r 16 w2)
      (u32-fill! r 24 w3)
      (u32-fill! r 32 w4)
      r))

;*---------------------------------------------------------------------*/
;*    K ...                                                            */
;*---------------------------------------------------------------------*/
(define K
   (let ((v (make-u32vector 4)))
      (u32vector-set! v 0 (u32 #x5a82 #x7999))
      (u32vector-set! v 1 (u32 #x6ed9 #xeba1))
      (u32vector-set! v 2 (u32 #x8f1b #xbcdc))
      (u32vector-set! v 3 (u32 #xca62 #xc1d6))
      v))

;*---------------------------------------------------------------------*/
;*    u32string-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (u32string-ref::ulong s::bstring i::int)
   (let ((len (string-length s)))
      (cond
	 ((<fx i len) (char->integer (string-ref s i)))
	 ((=fx i len) #x80)
	 (else 0))))

;*---------------------------------------------------------------------*/
;*    u32mmap-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (u32mmap-ref::ulong s::mmap i::elong)
   (let ((len (mmap-length s)))
      (cond
	 ((<fx i len) (char->integer (mmap-ref s i)))
	 ((=fx i len) #x80)
	 (else 0))))

;*---------------------------------------------------------------------*/
;*    u32matrix-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (u32matrix-ref::ulong m::vector i::int j::int)
   (u32vector-ref (vector-ref m i) j))

;*---------------------------------------------------------------------*/
;*    f ...                                                            */
;*---------------------------------------------------------------------*/
(define (f::ulong s::int x::ulong y::ulong z::ulong)
   (case s
      ((0)
       (bit-xor (bit-and x y) (bit-and (bit-not x) z)))
      ((1 3)
       (bit-xor x (bit-xor y z)))
      (else
       (bit-xor (bit-xor (bit-and x y) (bit-and x z)) (bit-and y z)))))

;*---------------------------------------------------------------------*/
;*    sha1 ...                                                         */
;*---------------------------------------------------------------------*/
(define (sha1 len::long M::vector)
   
   (let ((W::u32vector (make-u32vector 80 0))
	 (N::long (vector-length M)))
      
      ;; add length (in bits) into final pair of 32-bit integers (big-endian)
      (let* ((vec (vector-ref M (-fx N 1)))
	     (v14 0) ;; bigloo does not support length > 30 bits
	     (v15 (*fx len 8)))
	 (u32vector-set! vec 14 v14)
	 (u32vector-set! vec 15 v15))
      
      ;; set initial hash value
      (let* ((h0::ulong (u32 #x6745 #x2301))
	     (h1::ulong (u32 #xEFCD #xAB89))
	     (h2::ulong (u32 #x98BA #xDCFE))
	     (h3::ulong (u32 #x1032 #x5476))
	     (h4::ulong (u32 #xC3D2 #xE1F0)))
	 
	 ;; HASH computation
	 (for (i 0) (<fx i N) (+fx i 1)
	      
	      ;; 1 - prepare message schedule 'W'
	      (for (t 0) (<fx t 16) (+fx t 1)
		   (u32vector-set! W t (u32matrix-ref M i t)))
	      (for (t 16) (<fx t 80) (+fx t 1)
		   (let* ((w0::ulong (u32vector-ref W (-fx t 3)))
			  (w1::ulong (u32vector-ref W (-fx t 8)))
			  (w2::ulong (u32vector-ref W (-fx t 14)))
			  (w3::ulong (u32vector-ref W (-fx t 16)))
			  (v::ulong (bit-xor w0 (bit-xor w1 (bit-xor w2 w3)))))
		      (u32vector-set! W t (rotl32 v 1))))
	      
	      ;; 2 - initialize five working variables a, b, c, d, e
	      ;; with previous hash value
	      (let ((a::ulong h0)
		    (b::ulong h1)
		    (c::ulong h2)
		    (d::ulong h3)
		    (e::ulong h4))
		 
		 ;; 3 - main loop
		 (for (t 0) (<fx t 80) (+fx t 1)
		      (let* ((s (/fx t 20))
			     (a5::ulong (rotl32 a 5))
			     (f::ulong (f s b c d))
			     (k::ulong (u32vector-ref K s))
			     (w::ulong (u32vector-ref W t))
			     (y::ulong (+fx a5 (+fx f (+fx e (+fx k w))))))
			 (set! e d)
			 (set! d c)
			 (set! c (rotl32 b 30))
			 (set! b a)
			 (set! a (bit-and y (u32 #xffff #xffff)))))

		 (set! h0 (bit-and (+fx h0 a) (u32 #xffff #xffff)))
		 (set! h1 (bit-and (+fx h1 b) (u32 #xffff #xffff)))
		 (set! h2 (bit-and (+fx h2 c) (u32 #xffff #xffff)))
		 (set! h3 (bit-and (+fx h3 d) (u32 #xffff #xffff)))
		 (set! h4 (bit-and (+fx h4 e) (u32 #xffff #xffff)))))
	 
	 (u160->string h0 h1 h2 h3 h4))))

;*---------------------------------------------------------------------*/
;*    sha1sum-string ...                                               */
;*---------------------------------------------------------------------*/
(define (sha1sum-string str::bstring)

   ;; convert str into 512-bit/16-integer blocks arrays of integers
   (let* ((len::long (string-length str))
	  (l::long (+fx (inexact->exact (ceiling (/ len 4))) 2))
	  (N::long (inexact->exact (ceiling (/ l 16))))
	  (M::vector (make-vector N)))

      (for (i 0) (<fx i N) (+fx i 1)
	   (let ((vec (make-u32vector 16)))
	      (for (j 0) (<fx j 16) (+fx j 1)
		   (let* ((n::int (+fx (*fx i 64) (*fx j 4)))
			  (v0::ulong (u32string-ref str n))
			  (v1::ulong (u32string-ref str (+fx n 1)))
			  (v2::ulong (u32string-ref str (+fx n 2)))
			  (v3::ulong (u32string-ref str (+fx n 3)))
			  (v (u32 (u16 v0 v1) (u16 v2 v3))))
		      (u32vector-set! vec j v))
		   (vector-set! M i vec))))

      (sha1 len M)))

;*---------------------------------------------------------------------*/
;*    sha1sum-mmap ...                                                 */
;*---------------------------------------------------------------------*/
(define (sha1sum-mmap str::mmap)

   ;; convert str into 512-bit/16-integer blocks arrays of integers
   (let* ((len::long (mmap-length str))
	  (l::long (+fx (inexact->exact (ceiling (/ len 4))) 2))
	  (N::long (inexact->exact (ceiling (/ l 16))))
	  (M::vector (make-vector N)))

      (for (i 0) (<fx i N) (+fx i 1)
	   (let ((vec (make-u32vector 16)))
	      (for (j 0) (<fx j 16) (+fx j 1)
		   (let* ((n::elong (+fx (*fx i 64) (*fx j 4)))
			  (v0::ulong (u32mmap-ref str n))
			  (v1::ulong (u32mmap-ref str (+fx n 1)))
			  (v2::ulong (u32mmap-ref str (+fx n 2)))
			  (v3::ulong (u32mmap-ref str (+fx n 3)))
			  (v (u32 (u16 v0 v1) (u16 v2 v3))))
		      (u32vector-set! vec j v))
		   (vector-set! M i vec))))

      (sha1 len M)))

;*---------------------------------------------------------------------*/
;*    sha1sum-port ...                                                 */
;*---------------------------------------------------------------------*/
(define (sha1sum-port ip::input-port)
   
   ;; convert str into 512-bit/16-integer blocks arrays of integers
   (let ((buf::bstring (make-string 64)))
      
      (let loop ((len 0)
		 (L '()))
	 ;; fill the string
	 (string-fill! buf #a000)
	 (let* ((l (read-fill-string! buf 0 64 ip))
		(nlen (+fx l len))
		(vec (make-u32vector 16 0)))
	    (when (<fx l 64) (string-set! buf l #a128))
	    (for (j 0) (<fx j 16) (+fx j 1)
		 (let* ((n::int (*fx j 4))
			(v0::ulong (char->integer (string-ref buf n)))
			(v1::ulong (char->integer (string-ref buf (+fx n 1))))
			(v2::ulong (char->integer (string-ref buf (+fx n 2))))
			(v3::ulong (char->integer (string-ref buf (+fx n 3))))
			(v (u32 (u16 v0 v1) (u16 v2 v3))))
		    (u32vector-set! vec j v)))
	    (if (<fx l 64)
		;; we are done
		(let ((M (list->vector
			  (reverse!
			   (if (=fx l 63)
			       (cons* (make-u32vector 16 0) vec L)
			       (cons vec L))))))
		   (sha1 nlen M))
		(loop nlen (cons vec L)))))))

;*---------------------------------------------------------------------*/
;*    sha1sum-file ...                                                 */
;*---------------------------------------------------------------------*/
(define (sha1sum-file fname)
   (let ((mm (open-mmap fname :write #f)))
      (if (mmap? mm)
	  (unwind-protect
	     (sha1sum-mmap mm)
	     (close-mmap mm))
	  (let ((p (open-input-file fname)))
	     (unwind-protect
		(sha1sum-port p)
		(close-input-port p))))))

;*---------------------------------------------------------------------*/
;*    sha1sum ...                                                      */
;*---------------------------------------------------------------------*/
(define (sha1sum obj)
   (cond
      ((mmap? obj)
       (sha1sum-mmap obj))
      ((string? obj)
       (sha1sum-string obj))
      ((input-port? obj)
       (sha1sum-port obj))
      (else
       (error "sha1sum" "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    hmac-sha1sum-string ...                                          */
;*---------------------------------------------------------------------*/
(define (hmac-sha1sum-string key message)
   (hmac-string key message sha1sum-string))

