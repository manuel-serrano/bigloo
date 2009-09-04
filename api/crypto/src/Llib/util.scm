(module __crypto-util
   (export (macro +fx+)
	   (macro -fx-)
	   (macro debug)
	   (bignum-bit-length::long b::bignum)
	   (inline /ceilingfx::long x::long y::long)
	   (inline bignum->char::char b::bignum)
	   (inline char->bignum::bignum c::char)
	   (inline char-xor::char c1::char c2::char)
	   (string-xor::bstring str1::bstring str2::bstring)
	   (inline string-xor! str1 str2 len)
	   (inline string-xor-buffer! target target-pos str1 str1-pos str2 str2-pos len)
	   (make-random-string len::long #!key (show-trace #f))
	   (make-random-bignum len::long #!key (show-trace #f))
	   (make-random-prime::bignum from::bignum to::bignum #!key (show-trace #f))
	   (read-armored-base64-data::bstring p)
	   (inline str->hex-string::bstring str::bstring)
	   (inline hex-str->string::bstring str::bstring)
	   (inline hex-str->string!::bstring str::bstring)
	   (mod-inverse x b)
	   (expt-modbx x y m)
	   (sha1sum-bin::bstring in)
	   (md5sum-bin::bstring in)
	   (bin-str->bignum::bignum str::bstring)
	   (bignum->bin-str! buffer::bstring at::long x::bignum #!optional (len::long -1))
	   (bignum->bin-str::bstring b::bignum #!optional (len::long -1))))

(define-macro (debug . str)
   `(tprint ,@str))

(define-macro (+fx+ x . L)
   (cond
      ((null? L) x)
      (else `(+fx ,x (+fx+ ,@L)))))

(define-macro (-fx- x . L)
   `(-fx ,x (+fx+ ,@L)))

(define (bignum-bit-length::long b::bignum)
   (let loop ((b b)
	      (res 0))
      (let ((divided (/bx b #z256)))
	 (cond
	    ((zerobx? b) res)
	    ((zerobx? divided) ;; this is the last octet
	     (let ((x (bignum->fixnum b)))
		(cond
		   ((<fx x #x02) (+fx res 1))
		   ((<fx x #x04) (+fx res 2))
		   ((<fx x #x08) (+fx res 3))
		   ((<fx x #x10) (+fx res 4))
		   ((<fx x #x20) (+fx res 5))
		   ((<fx x #x40) (+fx res 6))
		   ((<fx x #x80) (+fx res 7))
		   (else (+fx res 8)))))
	    (else
	     (loop divided (+fx res 8)))))))

(define-inline (/ceilingfx x y)
   (let ((q (quotientfx x y))
	 (r (remainderfx x y)))
      (cond
	 ((zerofx? r) q)
	 ((>fx r 0)   (+fx q 1))
	 (else        (-fx q 1)))))

(define-inline (bignum->char::char b::bignum)
   (when (not (<bx b #z256))
      (error "bignum->char" "bignum must be < 256" b))
   (integer->char-ur (bignum->fixnum b)))
(define-inline (char->bignum::bignum c::char)
   (fixnum->bignum (char->integer c)))
(define (bin-str->bignum::bignum str::bstring)
   (let loop ((i 0)
	      (res #z0))
      (if (=fx i (string-length str))
	  res
	  (loop (+fx i 1)
		(+bx (*bx res #z256)
		     (fixnum->bignum (char->integer (string-ref str i))))))))
(define (bignum->bin-str::bstring b::bignum #!optional (len::long -1))
   (let* ((len (if (=fx len -1)
		   (/ceilingfx (bignum-bit-length b) 8)
		   len))
	  (str (make-string len)))
      (bignum->bin-str! str 0 b len)
      str))
(define (bignum->bin-str! buffer::bstring at::long x::bignum #!optional
			  (len::long -1))
   (define (last-char-digit::char x::bignum)
      (integer->char-ur (bignum->fixnum (remainderbx x #z256))))

   (let ((len (if (=fx len -1)
		  (/ceilingfx (bignum-bit-length x) 8)
		  len)))
      (let loop ((x x)
		 (i (-fx len 1)))
	 (cond
	    ((and (<fx i 0)
		  (zerobx? x))
	     buffer)
	    ((<fx i 0)
	     (error "bignum->bin-str!" "integer too large" x))
	    (else
	     (string-set! buffer (+fx at i) (last-char-digit x))
	     (loop (/bx x #z256) (-fx i 1)))))))

(define (make-random-string len::long #!key (show-trace #f))
   ;; TODO: currently make-random-string falls back to bigloo-random (with a
   ;; warning, but still). try to get more random-bytes from /dev/random, and
   ;; make fallback optional.
   ;; Furthermore: /dev/urandom is faster, but probably still safe.
   (define (make-bglrandom-string len)
      (let ((res (make-string len)))
	 (let loop ((i 0))
	    (cond
	       ((>=fx i len)
		res)
	       (else
		(string-set! res i (integer->char-ur (random 256)))
		(loop (+fx i 1)))))))

   (if (file-exists? "/dev/urandom")
       (let ((p (open-input-file "/dev/urandom")))
	  (if (input-port? p)
	      (unwind-protect
		 (let ((str (read-chars len p)))
		    (if (and (string? str)
			     (=fx len (string-length str)))
			str
			(begin
			   (warning "/dev/random did not work")
			   (make-bglrandom-string len))))
		 (close-input-port p))
	      (make-bglrandom-string len)))
       (make-bglrandom-string len)))

;; length is in bit.
(define (make-random-bignum len::long #!key (show-trace #f))
   (if (=fx len 0)
       #z0
       (let* ((str-len (/fx (+fx len 7) 8))
	      (str (make-random-string str-len))
	      (last-bits (remainder len 8))
	      (mask (case last-bits
		       ((0) #xFF) ((7) #x7F) ((6) #x3F)
		       ((5) #x1F) ((4) #x0F) ((3) #x07)
		       ((2) #x03) ((1) #x01)))
	      (first-char (string-ref str 0))
	      (first-char-n (char->integer first-char))
	      (masked (integer->char-ur (bit-and mask first-char-n))))
	  (string-set! str 0 masked)
	  (let loop ((i 0)
		     (res #z0))
	     (if (>=fx i str-len)
		 res
		 (loop (+fx i 1)
		       (+bx (*bx #z256 res)
			    (char->bignum (string-ref str i)))))))))

(define-inline (char-xor::char c1::char c2::char)
   (integer->char-ur (bit-xor (char->integer c1) (char->integer c2))))

(define (string-xor::bstring str1::bstring str2::bstring)
   (let ((len (string-length str1)))
      (when (not (=fx len (string-length str2)))
	 (error "string-xor" "strings don't have same length" str2))
      (let ((res (make-string len)))
	 (let loop ((i 0))
	    (cond
	       ((>=fx i len)
		res)
	       (else
		(string-set! res i (char-xor (string-ref str1 i)
					     (string-ref str2 i)))
		(loop (+fx i 1))))))))

;; str1 = str1^str2
(define-inline (string-xor! str1 str2 len)
   (string-xor-buffer! str1 0 str1 0 str2 0 len))
(define-inline (string-xor-buffer! target target-pos str1 str1-pos str2
				   str2-pos len)
   (let loop ((i 0))
      (if (=fx i len)
	  target
	  (begin
	     (string-set! target (+fx target-pos i)
			  (char-xor (string-ref str1 (+fx str1-pos i))
				    (string-ref str2 (+fx str2-pos i))))
	     (loop (+fx i 1))))))

(define (read-armored-base64-data p)
   (let ((data (let loop ((str ""))
		  (let ((l (read-line p)))
		     (cond
			((eof-object? l) str)
			((string-prefix? "--" l) str)
			(else (loop (string-append str l))))))))
      (base64-decode data)))

(define-inline (str->hex-string::bstring str::bstring)
   (string-hex-extern str))

(define-inline (hex-str->string::bstring hex-str::bstring)
   (string-hex-intern hex-str))
(define-inline (hex-str->string!::bstring hex-str::bstring)
   (string-hex-intern! hex-str))

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

(define (mod-inverse x b)
   (let* ((x1 (modulobx x b))
	  (g (gcd-ext x1 b)))
      (if (not (=bx (car g) #z1))
	  (error 'mod-inverse
		 "internal error, numbers are not relatively prime"
		 (cons x b))
	  (modulobx (cadr g) b))))

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

(define (sha1sum-bin::bstring in)
   (hex-str->string! (sha1sum in)))

(define (md5sum-bin::bstring in)
   (hex-str->string! (md5sum in)))

;*---------------------------------------------------------------------*/
;*    random-prime ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-random-prime start::bignum end::bignum #!key (show-trace #f))
   ;; TODO: make-random-prime does not use real random-port.			   
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
