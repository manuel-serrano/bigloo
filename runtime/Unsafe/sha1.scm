;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/sha1.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 26 08:40:27 2008                          */
;*    Last change :  Wed Oct  7 11:51:14 2015 (serrano)                */
;*    Copyright   :  2008-15 Manuel Serrano                            */
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
(define-macro (for range . body)
   (let ((for (gensym 'for))
	 (stop (gensym 'stop)))
      `(let ((,stop ,(caddr range)))
	  (let ,for ((,(car range) ,(cadr range)))
	       (when (<fx ,(car range) ,stop)
		  ,@body
		  (,for (+fx ,(car range) 1)))))))

;*---------------------------------------------------------------------*/
;*    u32 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u32 hi lo)
   `(bit-oru32 (bit-lshu32 (fixnum->uint32 ,hi) 16) (fixnum->uint32 ,lo)))

;*---------------------------------------------------------------------*/
;*    u16 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u16 hi lo)
   `(uint32->fixnum (bit-oru32 (bit-lshu32 ,hi 8) ,lo)))

;*---------------------------------------------------------------------*/
;*    u32-hi ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (u32-hi w)
   `(uint32->fixnum (bit-urshu32 ,w 16)))

;*---------------------------------------------------------------------*/
;*    u32-low ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (u32-lo w)
   `(uint32->fixnum (bit-andu32 ,w #u32:65535)))

;*---------------------------------------------------------------------*/
;*    rotl32 ...                                                       */
;*---------------------------------------------------------------------*/
(define (rotl32::uint32 x::uint32 n::int)
   (bit-oru32 (bit-lshu32 x n) (bit-urshu32 x (-fx 32 n))))

;*---------------------------------------------------------------------*/
;*    u32-fill! ...                                                    */
;*---------------------------------------------------------------------*/
(define (u32-fill! str offset w::uint32)
   (let* ((s1 (integer->string (u32-hi w) 16))
	  (l1 (string-length s1))
	  (s2 (integer->string (u32-lo w) 16))
	  (l2 (string-length s2)))
      (blit-string! s1 0 str (+fx offset (-fx 4 l1)) l1)
      (blit-string! s2 0 str (+fx offset (+fx 4 (-fx 4 l2))) l2)))
   
;*---------------------------------------------------------------------*/
;*    u160->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (u160->string w0::uint32 w1::uint32 w2::uint32 w3::uint32 w4::uint32)
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
(define (u32string-ref::uint32 s::bstring i::int)
   (let ((len (string-length s)))
      (cond
	 ((<fx i len) (fixnum->uint32 (char->integer (string-ref s i))))
	 ((=fx i len) #u32:128)
	 (else #u32:0))))

;*---------------------------------------------------------------------*/
;*    u32mmap-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (u32mmap-ref::uint32 s::mmap i::elong)
   (let ((len (mmap-length s)))
      (cond
	 ((<fx i len) (fixnum->uint32 (char->integer (mmap-ref s i))))
	 ((=fx i len) #u32:128)
	 (else #u32:0))))

;*---------------------------------------------------------------------*/
;*    u32matrix-ref ...                                                */
;*---------------------------------------------------------------------*/
(define (u32matrix-ref::uint32 m::vector i::int j::int)
   (u32vector-ref (vector-ref m i) j))

;*---------------------------------------------------------------------*/
;*    f ...                                                            */
;*---------------------------------------------------------------------*/
(define (f::uint32 s::int x::uint32 y::uint32 z::uint32)
   (case s
      ((0)
       (bit-xoru32 (bit-andu32 x y) (bit-andu32 (bit-notu32 x) z)))
      ((1 3)
       (bit-xoru32 x (bit-xoru32 y z)))
      (else
       (bit-xoru32
	  (bit-xoru32 (bit-andu32 x y) (bit-andu32 x z)) (bit-andu32 y z)))))

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
      (let* ((h0::uint32 (u32 #x6745 #x2301))
	     (h1::uint32 (u32 #xEFCD #xAB89))
	     (h2::uint32 (u32 #x98BA #xDCFE))
	     (h3::uint32 (u32 #x1032 #x5476))
	     (h4::uint32 (u32 #xC3D2 #xE1F0)))
	 
	 ;; HASH computation
	 (for (i 0 N)
	    
	    ;; 1 - prepare message schedule 'W'
	    (for (t 0 16)
	       (u32vector-set! W t (u32matrix-ref M i t)))
	    (for (t 16 80)
	       (let* ((w0::uint32 (u32vector-ref W (-fx t 3)))
		      (w1::uint32 (u32vector-ref W (-fx t 8)))
		      (w2::uint32 (u32vector-ref W (-fx t 14)))
		      (w3::uint32 (u32vector-ref W (-fx t 16)))
		      (v (bit-xoru32 w0 (bit-xoru32 w1 (bit-xoru32 w2 w3)))))
		  (u32vector-set! W t (rotl32 v 1))))
	    
	    ;; 2 - initialize five working variables a, b, c, d, e
	    ;; with previous hash value
	    (let ((a::uint32 h0)
		  (b::uint32 h1)
		  (c::uint32 h2)
		  (d::uint32 h3)
		  (e::uint32 h4))
	       
	       ;; 3 - main loop
	       (for (t 0 80)
		  (let* ((s (/fx t 20))
			 (a5::uint32 (rotl32 a 5))
			 (f::uint32 (f s b c d))
			 (k::uint32 (u32vector-ref K s))
			 (w::uint32 (u32vector-ref W t))
			 (y::uint32 (+u32 a5 (+u32 f (+u32 e (+u32 k w))))))
		     (set! e d)
		     (set! d c)
		     (set! c (rotl32 b 30))
		     (set! b a)
		     (set! a (bit-andu32 y (u32 #xffff #xffff)))))
	       
	       (set! h0 (bit-andu32 (+u32 h0 a) (u32 #xffff #xffff)))
	       (set! h1 (bit-andu32 (+u32 h1 b) (u32 #xffff #xffff)))
	       (set! h2 (bit-andu32 (+u32 h2 c) (u32 #xffff #xffff)))
	       (set! h3 (bit-andu32 (+u32 h3 d) (u32 #xffff #xffff)))
	       (set! h4 (bit-andu32 (+u32 h4 e) (u32 #xffff #xffff)))))
	 
	 (u160->string h0 h1 h2 h3 h4))))

;*---------------------------------------------------------------------*/
;*    /ceiling ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (/ceiling::long a b)
   (let ((r (/ a b)))
      (if (fixnum? r)
	  r
	  (flonum->fixnum (ceiling r)))))

;*---------------------------------------------------------------------*/
;*    sha1sum-string ...                                               */
;*---------------------------------------------------------------------*/
(define (sha1sum-string str::bstring)
   
   ;; convert str into 512-bit/16-integer blocks arrays of integers
   (let* ((len::long (string-length str))
	  (l::long (+fx (/ceiling (+fx len 1) 4) 2))
	  (N::long (/ceiling l 16))
	  (M::vector (make-vector N)))
      
      (for (i 0 N)
	 (let ((vec (make-u32vector 16)))
	    (for (j 0 16)
	       (let* ((n::int (+fx (*fx i 64) (*fx j 4)))
		      (v0::uint32 (u32string-ref str n))
		      (v1::uint32 (u32string-ref str (+fx n 1)))
		      (v2::uint32 (u32string-ref str (+fx n 2)))
		      (v3::uint32 (u32string-ref str (+fx n 3)))
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
	  (l::long (+fx (/ceiling (+fx len 1) 4) 2))
	  (N::long (/ceiling l 16))
	  (M::vector (make-vector N)))
      
      (for (i 0 N)
	 (let ((vec (make-u32vector 16)))
	    (for (j 0 16)
	       (let* ((n::elong (+fx (*fx i 64) (*fx j 4)))
		      (v0::uint32 (u32mmap-ref str n))
		      (v1::uint32 (u32mmap-ref str (+fx n 1)))
		      (v2::uint32 (u32mmap-ref str (+fx n 2)))
		      (v3::uint32 (u32mmap-ref str (+fx n 3)))
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
		 (L '())
		 (nL 0))
	 ;; fill the string
	 (string-fill! buf #a000)
	 (let* ((c (read-fill-string! buf 0 64 ip))
		(l (if (eof-object? c) 0 c))
		(nlen (+fx l len))
		(vec (make-u32vector 16 0)))
	    (when (<fx l 64) (string-set! buf l #a128))
	    (for (j 0 16)
	       (let* ((n::int (*fx j 4))
		      (v0::uint32 (char->integer (string-ref buf n)))
		      (v1::uint32 (char->integer (string-ref buf (+fx n 1))))
		      (v2::uint32 (char->integer (string-ref buf (+fx n 2))))
		      (v3::uint32 (char->integer (string-ref buf (+fx n 3))))
		      (v (u32 (u16 v0 v1) (u16 v2 v3))))
		  (u32vector-set! vec j v)))
	    (if (<fx l 64)
		;; we are done
		(let* ((fl::long (+fx (/ceiling (+fx nlen 1) 4) 2))
		       (N::long (/ceiling fl 16))
		       (M (list->vector
			     (reverse!
				(if (>fx N (+fx 1 nL))
				    (cons* (make-u32vector 16 #u32:0) vec L)
				    (cons vec L))))))
		   (sha1 nlen M))
		(loop nlen (cons vec L) (+fx nL 1)))))))

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

