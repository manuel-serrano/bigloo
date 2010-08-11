(module __crypto-string2key
   (import __crypto-util)
   (export (string->key-zero::bstring str::bstring target-length::long)
	   (string->key-hash::bstring str::bstring target-length::long hash-fun::procedure)
	   (string->key-simple::bstring str::bstring target-len::long
					hash-fun::procedure)
	   (string->key-salted::bstring str::bstring target-len::long hash-fun::procedure
					salt::bstring)
	   (string->key-iterated-salted::bstring str::bstring target-len::long
						 hash-fun::procedure
						 salt::bstring count::long)))

(define (string->key-zero::bstring pwd::bstring target-length::long)
   (let ((len (string-length pwd)))
      (cond
	 ((=fx len target-length) pwd)
	 ((<fx (string-length pwd) target-length)
	  (let ((res (make-string target-length #a000)))
	     (blit-string! pwd 0 res 0 len)
	     res))
	 (else (substring pwd 0 target-length)))))

(define (string->key-hash::bstring pwd::bstring target-length::long hash-fun::procedure)
   (let* ((hash-str (hash-fun pwd))
	  (hash-len (string-length hash-str)))
      (let loop ((str hash-str)
		 (len hash-len))
	 (cond
	    ((<fx len target-length)
	     (loop (string-append str hash-str) (+fx len hash-len)))
	    ((=fx len target-length)
	     str)
	    (else
	     (substring str 0 target-length))))))

;; OpenPGP simple s2k
(define (string->key-simple::bstring str::bstring len::long
				     hash-fun::procedure)
   (let ((res (make-string len)))
      (let loop ((i 0)
		 (res-pos 0))
	 (if (=fx res-pos len)
	     res
	     (let* ((preload (make-string i #a000))
		    (hash (hash-fun (string-append preload str)))
		    (hash-len (string-length hash))
		    (chars-to-blit (minfx hash-len (-fx len res-pos))))
		(blit-string! hash 0 res res-pos chars-to-blit)
		(loop (+fx i 1) (+fx res-pos chars-to-blit)))))))

;; OpenPGP salted s2k
(define (string->key-salted::bstring str::bstring len::long hash-fun::procedure
				     salt::bstring)
   (string->key-simple (string-append salt str) len hash-fun))

;; OpenPGP iterated salted s2k
(define (string->key-iterated-salted::bstring str::bstring len::long
					      hash-fun::procedure
					      salt::bstring count::long)
   (let* ((res (make-string len))
	  (salt+str (string-append salt str))
	  (salt+str-len (string-length salt+str)))
      (let loop ((i 0)
		 (res-pos 0))
	 (if (=fx res-pos len)
	     res
	     (let* ((cnt count)
		    (hashed-completely? #f)
		    (preloaded? #f)
		    (f (lambda ()
			  ;; salt+str must be hashed completely at least once.
			  (cond
			     ((and (not preloaded?) (not (zero? i)))
			      (set! preloaded? #t)
			      (make-string i #a000))
			     ((not hashed-completely?)
			      (set! hashed-completely? #t)
			      (set! cnt (-fx cnt salt+str-len))
			      salt+str)
			     ((=fx cnt 0) #f)
			     ((<fx cnt salt+str-len)
			      (let ((t cnt))
				 (set! cnt 0)
				 (substring salt+str 0 t)))
			     (else
			      (set! cnt (-fx cnt salt+str-len))
			      salt+str))))
		    (hash (hash-fun (open-input-procedure f)))
		    (hash-len (string-length hash))
		    (chars-to-blit (minfx hash-len (-fx len res-pos))))
		(blit-string! hash 0 res res-pos chars-to-blit)
		(loop (+fx i 1) (+fx res-pos chars-to-blit)))))))
