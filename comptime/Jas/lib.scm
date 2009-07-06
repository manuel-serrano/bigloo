(module jas_lib
   (export (u2 ::int)
	   (u4 ::int)
	   (w2 ::int)
	   (w4elong ::elong)
	   (w4 ::long)
	   (w4llong ::llong)
	   (f2 ::float)
	   (f4 ::double) 
	   (string->utf8 s)))

;; Simple but useful
(define (u2 n::int)
   (list (bit-and (bit-rsh n 8) #xFF)
	 (bit-and n #xFF) ))
   
(define (u4 n::int)
   (list (bit-and (bit-rsh n 24) #xFF)
	 (bit-and (bit-rsh n 16) #xFF)
	 (bit-and (bit-rsh n 8) #xFF)
	 (bit-and n #xFF) ))

(define (w2 n::int)
   (list (bit-and (bit-rsh n 16) #xFFFF)
	 (bit-and n #xFFFF) ))

(define (bit-elow16 n1::elong)
   (elong->fixnum (bit-andelong n1 (fixnum->elong #xFFFF))) )

(define (w4elong n::elong)
   (list (bit-elow16 (bit-rshelong (bit-rshelong (bit-rshelong n 16) 16) 16))
	 (bit-elow16 (bit-rshelong (bit-rshelong n 16) 16))
	 (bit-elow16 (bit-rshelong n 16))
	 (bit-elow16 n) ))

(define (w4 n::long)
   ;; CARE (bit-rsh 2 32) = 2
   (list (bit-and (bit-rsh (bit-rsh (bit-rsh n 16) 16) 16) #xFFFF)
	 (bit-and (bit-rsh (bit-rsh n 16) 16) #xFFFF)
	 (bit-and (bit-rsh n 16) #xFFFF)
	 (bit-and n #xFFFF) ))

(define (bit-llow16 n1::llong)
   (llong->fixnum (bit-andllong n1 (fixnum->llong #xFFFF))) )

(define (w4llong n::llong)
   ;; CARE (bit-rsh 2 32) = 2
   (list (bit-llow16 (bit-rshllong (bit-rshllong (bit-rshllong n 16) 16) 16))
	 (bit-llow16 (bit-rshllong (bit-rshllong n 16) 16))
	 (bit-llow16 (bit-rshllong n 16))
	 (bit-llow16 n) ))

(define (f2 n::float)
   (string->shortlist (float->ieee-string n)) )

(define (f4 n::double)
   (string->shortlist (double->ieee-string n)) )

(define (string->shortlist s)
   (let ( (n (string-length s)) )
      (define (collect i)
	 (if (=fx i n)
	     '()
	     (cons (bit-or (bit-lsh (char->integer (string-ref s i)) 8)
			   (char->integer (string-ref s (+fx i 1))) )
		   (collect (+fx i 2)) )))
      (collect 0) ))

(define (string->utf8 s)
   (produce-utf8 s (make-string (utf8-length s))) )

(define (sref s i)
   (char->integer (string-ref s i)) )

(define (sset s i cn)
   (string-set! s i (integer->char cn)) )

(define (utf8-length s)
   (let ( (n (string-length s)) )
      (define (walk s i r)
	 (if (=fx i n)
	     r
	     (walk s (+fx i 1) (+fx r (utf8-length1 (sref s i)))) ))
      (walk s 0 0) ))

(define (utf8-length1 cn)
   (cond
      ((= cn 0) 2)
      ((< cn #x80) 1)
      ((< cn #x800) 2)
      (else 3) ))

(define (produce-utf8 s s8)
   (let ( (n (string-length s)) )
      (define (walk i j)
	 (if (=fx i n)
	     s8
	     (walk (+fx i 1) (+fx j (produce-utf8-1 s8 j (sref s i)))) ))
      (walk 0 0) ))

(define (produce-utf8-1 s8 j cn)
   (define (outbyte x cn)
      (sset s8 (+fx j x) cn) )
   (cond
      ((= cn 0)
       (outbyte 0 #xC0)
       (outbyte 1 #x80)
       2 )
      ((< cn #x80)
       (outbyte 0 cn)
       1 )
      ((< cn #x800)
       (outbyte 0 (bit-or #xC0 (bit-rsh cn 6)))
       (outbyte 1 (bit-or #x80 (bit-and cn #x3F)))
       2 )
      (else
       (outbyte 0 (bit-or #xE0 (bit-rsh cn 12)))
       (outbyte 1 (bit-or #x80 (bit-and (bit-rsh cn 6) #x3F)))
       (outbyte 2 (bit-or #x80 (bit-and cn #x3F)))
       3 )))
