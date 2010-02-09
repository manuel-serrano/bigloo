;; we require elongs to have at least 32bits.

(define (^b32 a b) (bit-xorelong a b))
(define (&b32 a b) (bit-andelong a b))

(define-macro (^b32^ x . L) (if (null? L) x `(^b32 ,x (^b32^ ,@L))))

(define (<<<b32 x by)
   (if (=fx by 0)
       x
       (let ((shifted-by-1 (bit-andelong #ex7FFFFFFF (bit-rshelong x 1))))
	  (+elong (bit-andelong #exFFFFFFFF (bit-lshelong x by))
		  (bit-rshelong shifted-by-1 (-fx 31 by))))))
(define (+b32 x y) (bit-andelong #exFFFFFFFF (+elong x y)))
(define (-b32 x y) (bit-andelong #exFFFFFFFF (-elong x y)))

(define (fixnum->b32::elong n::long) (fixnum->elong n))
(define (b32->fixnum::long n::elong) (elong->fixnum n))

(define (bytes4->bits32 str::bstring at::long)
   (define (char-value-at str at)
      (fixnum->elong (char->integer (string-ref str at))))

   (let loop ((i at)
	      (res #e0))
      (if (=fx i (+fx at 4))
	  res
	  (loop (+fx i 1)
		(+elong (bit-lshelong res 8) (char-value-at str i))))))

(define (bits32->bytes4 str::bstring at::long v::elong)
   (define (elong->char-at str at v)
      (string-set! str at
		   (integer->char (elong->fixnum v))))

   (let loop ((i (+fx at 3))
	      (v v))
      (unless (<fx i at)
	 (let ((q (bit-urshelong v 8))
	       (r (bit-andelong v #exFF)))
	    (elong->char-at str i r)
	    (loop (-fx i 1) q)))))

;; returns the byte at given pos
(define (byte32::long v::elong pos::long)
   (let loop ((i 3)
	      (v v))
      (if (=fx pos i)
	  (elong->fixnum (bit-andelong #exFF v))
	  (loop (-fx i 1) (bit-urshelong v 8)))))

(define vector-ref32 vector-ref)
(define list->vector32 list->vector)
