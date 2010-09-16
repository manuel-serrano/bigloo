(module __openpgp-s2k
   (library crypto)
   (import __openpgp-human
	   __openpgp-algo
	   __openpgp-util)
   (export (simple-s2k::bstring str::bstring len::long hash-fun::procedure)
	   (salted-s2k::bstring str::bstring len::long hash-fun::procedure
				salt::bstring)
	   (iterated-salted-s2k::bstring str::bstring len::long
					 hash-fun::procedure salt::bstring
					 count::long)
	   (make-s2k algo hash salt count)
	   (s2k-algo s2k)
	   (s2k-hash s2k)
	   (s2k-salt s2k)
	   (s2k-count s2k)
	   (apply-s2k s2k passwd::bstring len::long)
	   (octet->iterated-salted-s2k-count::long o::char)
	   (iterated-salted-s2k-count->octet::long count::long)
	   (round-iterated-salted-s2k-count::long count::long)
	   (s2k-salt-length::int)))

(define *s2k-salt-length* 8)
(define (s2k-salt-length::int) *s2k-salt-length*)

(define *s2k-EXPBIAS* 6)
(define *s2k-OFFSET* 16)
;; min-s2k-count == 1024
(define *min-s2k-count* (bit-lsh (+fx #x0 *s2k-OFFSET*)
				 (+fx #x0 *s2k-EXPBIAS*)))
;; max-s2k-count == 65011712
(define *max-s2k-count* (bit-lsh (+fx #xF *s2k-OFFSET*)
				 (+fx #xF *s2k-EXPBIAS*)))

;; rounds the input number, so that it can be exactly encoded by the
;; s2k-encoding mechanism.
(define (round-iterated-salted-s2k-count::long count::long)
   (let ((octet (iterated-salted-s2k-count->octet count)))
      (octet->iterated-salted-s2k-count (integer->char octet))))

(define (octet->iterated-salted-s2k-count::long o::char)
   (let ((ov (char->integer o)))
      (bit-lsh (+fx *s2k-OFFSET* (bit-and ov #x0F))
	       (+fx (bit-rsh ov 4) *s2k-EXPBIAS*))))

;; if we are in between iterations just round up.
(define (iterated-salted-s2k-count->octet::long count::long)
   (cond
      ((<=fx count *min-s2k-count*) #x00)
      ((>=fx count *max-s2k-count*) #xFF)
      (else
       (let loop ((right-digit+offset (bit-rsh count *s2k-EXPBIAS*))
		  (left-digit 0))
	  ;; the left digit decides how far we must shift the right digit.
	  ;; the right digit+offset (offset=16) has a value of 16-31.
	  ;; We thus shift the initial count to the right until we satisfy this
	  ;; property. (As we come from above, we only need to check if we are
	  ;; <=31)
	  ;; Note that we shift the initial count to the right by the
	  ;; obligatory *s2k-EXPBIAS*
	  (if (<=fx right-digit+offset (+fx *s2k-OFFSET* #x0F)) ;; (<=fx .. 31)
	      ;; combine the two and round up (if necessary).
	      (let ((res (+fx (bit-lsh left-digit 4)
			      (-fx right-digit+offset *s2k-OFFSET*))))
		 ;; now the rounding up.
		 (let liip ((res res))
		    (let ((c (integer->char-ur res)))
		       (if (< (octet->iterated-salted-s2k-count c) count)
			   (liip (+fx res 1))
			   res))))
	      (loop (bit-rsh right-digit+offset 1)
		    (+fx left-digit 1)))))))

(define (simple-s2k::bstring str::bstring len::long hash-fun::procedure)
   (string->key-simple str len hash-fun))

(define (salted-s2k::bstring str::bstring len::long hash-fun::procedure
			     salt::bstring)
   (string->key-salted str len hash-fun salt))

;; note: we take the byte-count and not the char.
;;       the char must hence be already decoded.
;; CARE: long might not be enough for 'count'. [flo]
(define (iterated-salted-s2k::bstring str::bstring len::long
				      hash-fun::procedure salt::bstring
				      count::long)
   (string->key-iterated-salted str len hash-fun salt count))

(define (make-s2k algo hash salt count)
   (let ((t (make-S2K)))
      (S2K-algo-set! t algo)
      (S2K-hash-set! t hash)
      (S2K-salt-set! t salt)
      (S2K-count-set! t count)
      t))
(define (s2k-algo s2k) (S2K-algo s2k))
(define (s2k-hash s2k) (S2K-hash s2k))
(define (s2k-salt s2k) (S2K-salt s2k))
(define (s2k-count s2k) (S2K-count s2k))

(define-struct S2K algo hash salt count)

(define (apply-s2k s2k pwd::bstring len::long)
   (with-trace 5 "apply-s2k"
      (when (not (S2K? s2k))
	 (error "apply-s2k"
		"S2K-struct expected"
		s2k))
      (trace-item "Applying s2k " pwd " (" len ")")
      (let ((algo (S2K-algo s2k))
	    (hash-algo (S2K-hash s2k))
	    (salt (S2K-salt s2k))
	    (count (S2K-count s2k)))
	 (case algo
	    ((simple) ;; simple-hash
	     (trace-item "Simple S2K (no salt no iteration)")
	     (simple-s2k pwd len (hash-algo->procedure hash-algo)))
	    ((salted) ;; salted s2k
	     (let ((salt (S2K-salt s2k)))
		(trace-item "Salted S2K: " (str->hex-string salt))
		(salted-s2k pwd len (hash-algo->procedure hash-algo) salt)))
	    ((iterated) ;; iterated and salted s2k
	     (let ((salt (S2K-salt s2k))
		   (count (S2K-count s2k)))
		(trace-item "Salted iterated S2K: " count " " (str->hex-string salt))
		(iterated-salted-s2k pwd len (hash-algo->procedure hash-algo) salt
				     count)))
	    (else (error "apply-s2k"
			 "bad S2K struct"
			 s2k))))))
