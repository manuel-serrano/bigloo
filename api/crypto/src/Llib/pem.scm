(module __crypto-pem
   (import __crypto-rsa
	   __crypto-dsa
	   __crypto-DER
	   __crypto-util)
   (export (read-pem-key in)
	   (read-pem-key-string str::bstring)
	   (read-pem-key-port p::input-port)
	   (read-pem-key-file f::bstring)
	   (write-pem-key key out #!optional public-key-only?)
	   (write-pem-key-port key out::output-port #!optional public-key-only?)
	   (write-pem-key-file key out::bstring #!optional public-key-only?)
	   (write-pem-key-string::bstring key #!optional public-key-only?)))

(define (any->bignum x)
   (cond
      ((bignum? x) x)
      ((fixnum? x) (fixnum->bignum x))
      ((elong? x) (elong->bignum x))
      ((llong? x) (llong->bignum x))
      (else (error "pem" "could not convert to bignum" x))))

;; These are assigned by ASN.1
(define *rsa-oid* 'oid:1.2.840.113549.1.1.1)
(define *dsa-oid* 'oid:1.2.840.10040.4.1)

;; =====================  READING =============================================

(define (rsa-read-complete-key-pem p) ;; needs to be a private key.
   (let* ((l (decode-DER (open-input-string (read-armored-base64-data p))))
	  (n (any->bignum (cadr l)))
	  (e (any->bignum (caddr l)))
	  (d (any->bignum (cadddr l)))
	  (p (any->bignum (cadddr (cdr l))))
	  (q (any->bignum (cadddr (cddr l))))
	  (d-mod-p-1 (any->bignum (cadddr (cdddr l))))
	  (d-mod-q-1 (any->bignum (cadddr (cddddr l))))
	  (qInv (any->bignum (cadddr (cddddr (cdr l))))))
      (instantiate::Complete-Rsa-Key
	 (modulus n)
	 (exponent d)
	 ;; private key information
	 (e e)
	 (p p)
	 (q q)
	 (exp1 d-mod-p-1)
	 (exp2 d-mod-q-1)
	 (coeff qInv))))

(define (dsa-read-complete-key-pem p) ;; needs to be a private key.
   (let* ((l (decode-DER (open-input-string (read-armored-base64-data p))))
	  (p (any->bignum (cadr l)))
	  (q (any->bignum (caddr l)))
	  (g (any->bignum (cadddr l)))
	  (y (any->bignum (cadddr (cdr l))))
	  (x (any->bignum (cadddr (cddr l)))))
      (instantiate::Complete-Dsa-Key
	 (p p)
	 (q q)
	 (g g)
	 (y y)
	 ;; private information
	 (x x))))

(define (rsa-read-public-key-pem l)
   (let* ((algo (car l))
	  (null (cadr algo)))
      (when (not (eq? null 'null))
	 (error "read-public-key-pem"
		"Expected Algo of form (oid:... null)."
		algo))
      (when (not (isa? (cadr l) DER-BitString))
	 (error "read-public-key-pem"
		"No BitString entry found"
		(cadr l)))
      (with-access::DER-BitString (cadr l) (data) ;; don't need unused bits.
	 (let* ((key-data (decode-DER (open-input-string data)))
		(n (any->bignum (car key-data)))
		(e (any->bignum (cadr key-data))))
	    (instantiate::Rsa-Key
	       (modulus n)
	       (exponent e))))))

(define (dsa-read-public-key-pem l) ;; needs to be public key.
   (let* ((algo (car l))
	  (key-data (cadr algo))
	  (p (any->bignum (car key-data)))
	  (q (any->bignum (cadr key-data)))
	  (g (any->bignum (caddr key-data))))
      (when (not (isa? (cadr l) DER-BitString))
	 (error "read-public-key-pem"
		"No BitString entry found"
		(cadr l)))
      (with-access::DER-BitString (cadr l) (data) ;; don't need unused bits.
	 (let ((y (any->bignum (decode-DER (open-input-string data)))))
	    (instantiate::Dsa-Key
	       (p p)
	       (q q)
	       (g g)
	       (y y))))))

(define (read-public-key-pem p) ;; needs to be public key.
   (let* ((l (decode-DER (open-input-string (read-armored-base64-data p))))
	  (algo (car l))
	  (oid (car algo)))
      (cond
	 ((eq? oid *rsa-oid*) (rsa-read-public-key-pem l))
	 ((eq? oid *dsa-oid*) (dsa-read-public-key-pem l))
	 (else
	  (error "read-public-key-pem"
		 "Unknown Object ID"
		 oid)))))

(define (read-pem-key-port p::input-port)
   (let ((first-line (read-line p)))
      (when (eof-object? first-line) (error "read-pem-key" "bad key-file" #f))
      (cond
	 ((string=? "-----BEGIN RSA PRIVATE KEY-----" first-line)
	  (rsa-read-complete-key-pem p))
	 ((string=? "-----BEGIN DSA PRIVATE KEY-----" first-line)
	  (dsa-read-complete-key-pem p))
	 ((string=? "-----BEGIN PUBLIC KEY-----" first-line)
	  (read-public-key-pem p))
	 (else (error "read-pem-key" "does not seem to be a key file"
		      first-line)))))

(define (read-pem-key-file f::bstring)
   (let ((p (open-input-file f)))
      (when (not p)
	 (error "rsa-read-pem-key-file"
		"Could not open file"
		f))
      (unwind-protect
	 (read-pem-key-port p)
	 (close-input-port p))))

(define (read-pem-key-string str::bstring)
   (let ((p (open-input-string str)))
      (unwind-protect
	 (read-pem-key-port p)
	 (close-input-port p))))

(define (read-pem-key in)
   (cond
      ((input-port? in) (read-pem-key-port in))
      ((string? in) (read-pem-key-file in))
      (else (error "read-pem-key"
		   "Bad input. Requires either port or file-name"
		   in))))

;; =====================  WRITING =============================================


(define (rsa-write-public-pem key p)
   (display "-----BEGIN PUBLIC KEY-----\n" p)
   (let ((str-p (open-output-string)))
      (with-access::Rsa-Key key (modulus exponent)
	 (encode-DER (list modulus exponent) str-p))
      (let* ((data (close-output-port str-p))
	     (bitstring (instantiate::DER-BitString
			   (data data)
			   (unused-bits 0)))
	     (str-p2 (open-output-string)))
	 (encode-DER `((,*rsa-oid* null) ,bitstring) str-p2)
	 (display (base64-encode (close-output-port str-p2)) p)))
   (display "\n-----END PUBLIC KEY-----\n" p))

(define (rsa-write-private-pem key port)
   (display "-----BEGIN RSA PRIVATE KEY-----\n" port)
   (let ((str-p (open-output-string)))
      (with-access::Complete-Rsa-Key key (modulus exponent e p q exp1 exp2
						 coeff)
	 (encode-DER (list 0 ;; version
			   modulus e exponent p q exp1 exp2 coeff)
		       str-p)
	 (display (base64-encode (close-output-port str-p)) port)
	 (display "\n-----END RSA PRIVATE KEY-----\n" port))))

(define (dsa-write-public-pem key port)
   (display "-----BEGIN PUBLIC KEY-----\n" port)
   (with-access::Dsa-Key key (p q g y)
      (let ((str-p (open-output-string)))
	 (encode-DER y str-p)
	 (let* ((data (close-output-port str-p))
		(bitstring (instantiate::DER-BitString
			      (data data)
			      (unused-bits 0)))
		(str-p2 (open-output-string)))
	    (encode-DER `((,*dsa-oid* (,p ,q ,g))
			  ,bitstring)
			str-p2)
	    (display (base64-encode (close-output-port str-p2)) port))))
   (display "\n-----END PUBLIC KEY-----\n" port))

(define (dsa-write-private-pem key port)
   (display "-----BEGIN DSA PRIVATE KEY-----\n" port)
   (let ((str-p (open-output-string)))
      (with-access::Complete-Dsa-Key key (p q g y x)
	 (encode-DER (list 0 ;; version
			   p q g y x)
		     str-p)
	 (display (base64-encode (close-output-port str-p)) port)
	 (display "\n-----END DSA PRIVATE KEY-----\n" port))))

(define (write-pem-key-port key p::output-port #!optional public-key-only?)
   (cond
      ((and (isa? key Complete-Rsa-Key) (not public-key-only?))
       (rsa-write-private-pem key p))
      ((isa? key Rsa-Key)
       (rsa-write-public-pem key p))
      ((and (isa? key Complete-Dsa-Key) (not public-key-only?))
       (dsa-write-private-pem key p))
      ((isa? key Dsa-Key)
       (dsa-write-public-pem key p))
      (else (error "write-pem-key-port"
		   "Not an RSA/DSA key"
		   key))))

(define (write-pem-key-file key f::bstring #!optional public-key-only?)
   (let ((p (open-output-file f)))
      (when (not p)
	 (error "write-pem-key-file"
		"Could not open file"
		f))
      (unwind-protect
	 (write-pem-key-port key p public-key-only?)
	 (close-output-port p))))

(define (write-pem-key-string key #!optional public-key-only?)
   (let ((p (open-output-string)))
      (write-pem-key-port key p public-key-only?)
      (close-output-port p)))

(define (write-pem-key key out #!optional public-key-only?)
   (cond
      ((output-port? out) (write-pem-key-port key out public-key-only?))
      ((string? out) (write-pem-key-file key out public-key-only?))
      (else (error "write-pem-key"
		   "Bad parameter. Requires either port or file-name"
		   out))))
