(module __crypto-cipher-padding
   (import __crypto-util)
   (export (no-pad::bool plain::bstring size-plain::long)
	   (no-unpad::long plain::bstring)
	   (bit-pad::bool plain::bstring size-plain::long)
	   (bit-unpad::long plain::bstring)
	   (ansi-x.923-pad::bool plain::bstring size-plain::long)
	   (iso-10126-pad::bool plain::bstring size-plain::long)
	   (pkcs7-pad::bool plain::bstring size-plain::long)
	   (byte-unpad::long plain::bstring)
	   (zero-pad plain::bstring size-plain::long)
	   (zero-unpad::long plain::bstring)))

(define (no-pad plain::bstring size-plain::long)
   ;; works for decryption as well.
   (if (not (=fx size-plain 0))
       (error "No-padding"
	      "input string size not multiple of block-size"
	      #f)
       #f))

(define (no-unpad::long plain::bstring)
   (string-length plain))

(define (bit-pad::bool plain::bstring size-plain::long)
   (let ((block-size (string-length plain)))
      ;; add a 1 bit after the message and fill with 0s until the message length
      ;; is a multiple of block-size.
      (string-set! plain size-plain (integer->char-ur #x80))
      (let loop ((i (+fx size-plain 1)))
	 (if (=fx i block-size)
	     #t
	     (begin
		(string-set! plain i #a000)
		(loop (+fx i 1)))))))

(define (bit-unpad::long plain::bstring)
   (let ((block-size (string-length plain)))
      ;; undo bit-pad
      (let loop ((i (-fx block-size 1)))
	 (cond
	    ((<fx i 0)
	     (error 'bit-unpad
		    "cipher was not bit-padded"
		    (str->hex-string plain)))
	    ((char=? (string-ref plain i) #a000)
	     (loop (-fx i 1)))
	    (else
	     (when (not (char=? (string-ref plain i) (integer->char-ur #x80)))
		(error 'bit-unpad
		       "cipher was not bit-padded."
		       (str->hex-string plain)))
	     i)))))

(define (byte-pad::bool fill-char ;; when #f use random. otherwise char.
			plain::bstring
			size-plain::long)
   ;; add fill-chars, and finally a number indicating the number of
   ;; added-chars.
   ;; Ex (fill-char == 00, block-size == 8): DD DD DD 00 00 00 00 05
   (let ((block-size (string-length plain)))
      (let loop ((i size-plain))
	 (cond
	    ((=fx i (-fx block-size 1))
	     (string-set! plain i
			  (integer->char-ur (-fx block-size size-plain)))
	     #t)
	    ((not fill-char)
	     (string-set! plain i (integer->char-ur (random 256)))
	     (loop (+fx i 1)))
	    (else
	     (string-set! plain i fill-char)
	     (loop (+fx i 1)))))))

(define (ansi-x.923-pad::bool plain::bstring size-plain::long)
   (byte-pad #a000 plain size-plain))

(define (iso-10126-pad::bool plain::bstring size-plain::long)
   (byte-pad #f plain size-plain))

(define (pkcs7-pad::bool plain::bstring size-plain::long)
   (let ((block-size (string-length plain)))
      (byte-pad (integer->char-ur (-fx block-size size-plain)) plain size-plain)))

(define (byte-unpad::long plain::bstring)
   (let ((block-size (string-length plain)))
      ;; undo byte-pad
      (let* ((last-char (string-ref plain (-fx block-size 1)))
	     (nb-added-chars (char->integer last-char)))
	 (when (>fx nb-added-chars block-size)
	    (error 'byte-unpad
		   "cipher was not byte-padded"
		   plain))
	 (-fx block-size nb-added-chars))))

(define (zero-pad plain::bstring size-plain::long)
   (let ((block-size (string-length plain)))
      (if (=fx size-plain 0)
	  #f
	  (let loop ((i size-plain))
	     (if (=fx i block-size)
		 #t
		 (begin
		    (string-set! plain i #a000)
		    (loop (+fx i 1))))))))

(define (zero-unpad::long plain::bstring)
   (let ((block-size (string-length plain)))
      (let loop ((i (-fx block-size 1)))
	 (cond
	    ((<fx i 0) 0)
	    ((char=? (string-ref plain i) #a000)
	     (loop (-fx i 1)))
	    (else (+fx i 1))))))
