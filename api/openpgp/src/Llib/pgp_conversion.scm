(module __openpgp-conversion
   (export (fixnum->scalar::bstring nn::long len::long)
	   (scalar->fixnum::long scalar::bstring)))

;; spec requires n to be unsigned, but does not really tell size.
;; we use long instead.
;; simple big-endian encoding.
(define (fixnum->scalar::bstring nn::long len::long)
   (let ((str (make-string len)))
      (let loop ((i (-fx len 1))
		 (n nn))
	 (cond
	    ((and (zerofx? n)
		  (<fx i 0))
	     str)
	    ((<fx i 0)
	     (error "fixnum->scalar" "number too big for requested size" nn))
	    (else
	     (string-set! str i (integer->char-ur (remainderfx n 256)))
	     (loop (-fx i 1) (/fx n 256)))))))
(define (scalar->fixnum::long scalar::bstring)
   (let loop ((i 0)
	      (n 0))
      (cond
	 ((=fx i (string-length scalar))
	  n)
	 (else (loop (+fx i 1)
		     (+fx (*fx n 256)
			  (char->integer (string-ref scalar i))))))))
