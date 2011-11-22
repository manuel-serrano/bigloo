(module __crypto-DER
   (export (encode-DER el p)
	   (decode-DER p))
   (export (final-class DER-BitString
	      (data::bstring read-only)
	      (unused-bits::long read-only))
	   (final-class DER-Set
	      (set::pair-nil read-only))))

;; As of 2009 04 30 only a subset of DER is implemented. [flo]
;; We did not even try to be efficient.


;; ITU-T X.690
;; http://www.itu.int/ITU-T/studygroups/com10/languages/X.690_1297.pdf

(define *universal-tags*
   ;; found at http://www.obj-sys.com/asn1tutorial/node124.html
   '(;(reserved 0)
     (bool 1)
     (int 2)
     (bit-string 3)
     (octet-string 4)
     (null 5)
     (object-identifier 6)
     (object-descriptor 7)
     (instance-of 8)
     (real 9)
     (enumerated 10)
     (embedded 11)
     (utf8 12)
     (relative 13)
     (sequence 16)
     (set 17)
     (numeric-string 18)
     (printable-string 19)
     (teletex-string 20)
     (videotex-string 21)
     (ia5-string 22)
     (utc-time 23)
     (generalized-time 24)
     (graphic-string 25)
     (visible-string 26)
     (general-string 27)
     (universal-string 28)
     (character-string 29)
     (bmp-string 30)))

(define *class-tag*
   ;; 8.1.2.2, table 1
   '((universal   #x00)
     (application #x40)
     (context     #x80)
     (private     #x70)))

(define *prim/constr-tag*
   ;; 8.1.2.3, figure 3
   '((primitive   #x00)
     (constructed #x20)))

(define *universal-inv-tags*
   (map (lambda (p) (list (cadr p) (car p))) *universal-tags*))
(define *class-inv-tag*
   (map (lambda (p) (list (cadr p) (car p))) *class-tag*))
(define *prim/constr-inv-tag*
   (map (lambda (p) (list (cadr p) (car p))) *prim/constr-tag*))

(define (lookup x from)
   (let ((t (assq x from)))
      (when (not t)
	 (error "DER lookup"
		"could not find"
		x))
      (cadr t)))

(define (universal-tag t) (lookup t *universal-tags*))
(define (class-tag c)     (lookup c *class-tag*))
(define (prim/constr-tag p/c) (lookup p/c *prim/constr-tag*))
(define (universal-inv-tag t) (lookup t *universal-inv-tags*))
(define (class-inv-tag c)     (lookup c *class-inv-tag*))
(define (prim/constr-inv-tag p/c) (lookup p/c *prim/constr-inv-tag*))

(define-macro (+fx+ . L)
   (cond
      ((null? L)
       (error '+fx+ "not enough arguments" L))
      ((null? (cdr L)) (car L))
      (else `(+fx ,(car L) (+fx+ ,@(cdr L))))))

(define (display-octet o p)
   (define (any->char any)
      (cond
	 ((char? any) any)
	 ((fixnum? any) (integer->char any))
	 ((bignum? any) (any->char (bignum->fixnum any)))
	 ((elong? any) (any->char (elong->fixnum any)))
	 ((llong? any) (any->char (llong->fixnum any)))
	 (else (error "display-octet"
		      "could not convert number to char"
		      any))))

   (display (any->char o) p))

(define (read-octet p)
   (let ((c (read-char p)))
      (when (eof-object? c)
	 (error "read-octet"
		"unexpected end of file"
		#f))
      (char->integer c)))

;; ===============================================================================
;; identifier encoding/decoding
;; ===============================================================================

;; n must be < 31
(define (encode-short-identifier class prim/constr n p)
   (assert (n) (<fx n 31))
   ;; 8.1.2.3
   (display-octet (+fx+ (class-tag class)
			(prim/constr-tag prim/constr)
			n)
		  p))
(define (encode-long-identifier class prim/constr n p)
   (define bits7-mask #x7F)
   ;; 8.1.2.4
   (let ((start-octet (+fx+ (class-tag class)
			    (prim/constr-tag prim/constr)
			    #x1F)))
      (display-octet start-octet p)
      ;; just make it simple...
      ;; not tail recursive.
      (let rec ((n n)
		(bit8 #x00))
	 (if (<fx n bits7-mask) ;; mask covers whole number
	     (display-octet (+fx bit8 n) p)
	     (begin
		(rec (bit-rsh n 7)
		     ;; all but the last printed octet have bit8 set to 1
		     #x80)
		(display-octet (+fx bit8 (bit-and n bits7-mask)) p))))))

(define (encode-identifier class prim/constr n p)
   (if (<fx n 31)
       (encode-short-identifier class prim/constr n p)
       (encode-long-identifier class prim/constr n p)))

(define (decode-identifier p)
   (define *class-mask* #xC0)
   (define *prim/constr-mask* #x20)
   (define *tag-number-mask* #x1F)
   (let* ((o (read-octet p))
	  (class (class-inv-tag (bit-and o *class-mask*)))
	  (prim/constr (prim/constr-inv-tag (bit-and o *prim/constr-mask*)))
	  (tag-num (bit-and o *tag-number-mask*)))
      (if (<fx tag-num 31)
	  (values class prim/constr tag-num)
	  (let loop ((tag-num 0))
	     (let* ((o (read-octet p))
		    (last? (zerofx? (bit-and #x80 o)))
		    (data (bit-and #x7F o))
		    (new-tag-num (+fx (bit-lsh tag-num 7) data)))
		(if last?
		    (values class prim/constr new-tag-num)
		    (loop new-tag-num)))))))

(define (decode-universal-identifier tag-num)
   (universal-inv-tag tag-num))

;; ===============================================================================
;; length encoding/decoding
;; ===============================================================================

;; l must be <= 127
(define (encode-short-definite-length l p)
   ;; 8.1.3.4
   (assert (l) (<=fx l 127))
   (display-octet l p))

(define (encode-long-definite-length l p)
   ;; 8.1.3.5
   (define (nb-octets-needed l)
      (if (=fx l 0)
	  0
	  (+fx 1 (nb-octets-needed (bit-rsh l 8)))))

   (let ((nb-octets (nb-octets-needed l)))
      (assert (nb-octets) (<fx nb-octets #x7F))
      (display-octet (+fx #x80 nb-octets) p)
      ;; recursive
      (let rec ((l l))
	 (unless (zerofx? l)
	    (rec (bit-rsh l 8))
	    (display-octet (bit-and l #xFF) p)))))

(define (encode-definite-length l p)
   (if (<=fx l 127)
       (encode-short-definite-length l p)
       (encode-long-definite-length l p)))

(define (encode-indefinite-length-start p)
   ;; 8.1.3.6.1
   (display-octet #x80 p))

(define (encode-indefinite-length-end p)
   ;; 8.1.5
   (display-octet #x00 p)
   (display-octet #x00 p))

(define (decode-length p)
   (let ((o (read-octet p)))
      (cond
	 ((=fx o #x80) ;; indefinite length
	  #f)
	 ((zerofx? (bit-and o #x80))
	  ;; short definite length
	  (bit-and o #x7F))
	 (else ;; long definite length
	  (let loop ((len 0)
		     (remaining (bit-and o #x7F)))
	     (if (zerofx? remaining)
		 len
		 (loop (+fx (*fx len 256)
			    (read-octet p))
		       (-fx remaining 1))))))))

;; ===============================================================================
;; value encoding/decoding
;; ===============================================================================

(define (encode-bool b p)
   (encode-identifier 'universal 'primitive (universal-tag 'bool) p)
   (encode-definite-length 1 p)
   (display-octet (if b #x01 #x00) p))

(define (decode-bool p)
   (let ((l (decode-length p)))
      (when (not (=fx l 1))
	 (error "ASN-1 decode bool"
		"length must be equal to 1"
		l))
      (not (=fx (read-octet p) #x00))))

;; with length
;; we only cover signed numbers here.
;; should work with any given number (bignum, etc.)
(define (encode-int/enum-content n p)
   ;; 8.3.1-3
   (if (< n 256)
       (begin
	  (encode-definite-length 1 p)
	  (display-octet n p))
       (let rec ((n n)
		 (len 0))
	  (if (zero? n)
	      (encode-definite-length len p)
	      (begin
		 (rec (quotient n 256) (+fx len 1))
		 (display-octet (remainder n #x100) p))))))
   
(define (encode-int n p)
   ;; 8.3
   (encode-identifier 'universal 'primitive (universal-tag 'int) p)
   (encode-int/enum-content n p))

(define (encode-enumerated n p) ;; enums of C
   ;; 8.4
   (encode-identifier 'universal 'primitive (universal-tag 'enumerated) p)
   (encode-int/enum-content n p))

(define (decode-int/enumerated p)
   (let loop ((n 0)
	      (len (decode-length p)))
      (cond
	 ((zerofx? len) n)
	 ((and (fixnum? n) (>fx n (/fx (maxvalfx) 256)))
	  (loop (fixnum->bignum n) len))
	 (else
	  (loop (+ (* n 256) (read-octet p)) (-fx len 1))))))
		   
(define (encode-real r p)
   ;; 8.5
   ;; TODO. we do not yet have an easy way to get access to a doubles exponent.
   (error "encode-real"
	  "not yet implemented"
	  #f))

(define (encode-null p)
   (display-octet #x05 p)  ;; null-type, primitive
   (display-octet #x00 p)) ;; of zero length

(define (decode-null p)
   (let ((o (read-octet p)))
      (assert (o) (=fx o #x00)))
   'null)

;; when definite build string first in memory and print it.
;; when indefinite write the chars one by one.
(define (encode-sequence/set-content l p definite? display-fun)
   ;; 8.9.3
   (define (display-els l p)
      (for-each (lambda (el)
		   (display-fun el p))
		l))

   (if definite?
       (let ((str-p (open-output-string)))
	  (display-els l str-p)
	  (let ((str (close-output-port str-p)))
	     (encode-definite-length (string-length str) p)
	     (let loop ((i 0))
		(when (<fx i (string-length str))
		   (display-octet (string-ref str i) p)
		   (loop (+fx i 1))))))
       (begin
	  (encode-indefinite-length-start p)
	  (display-els l p)
	  (encode-indefinite-length-end p))))

(define (encode-sequence l p #!key (definite? #t) (display-fun encode-DER))
   ;; 8.9
   (encode-identifier 'universal 'constructed
		       (universal-tag 'sequence)
		       p)
   (encode-sequence/set-content l p definite? display-fun))

(define (encode-set s::DER-Set p #!key (definite? #t) (display-fun encode-DER))
   ;; 8.11
   (encode-identifier 'universal 'constructed
		       (universal-tag 'set)
		       p)
   (with-access::DER-Set s (set)
      (encode-sequence/set-content set p definite? display-fun)))

(define (decode-sequence/set p)
   (let ((len (decode-length p)))
      (if len ;; definite
	  (let* ((buff (read-chars len p))
		 (str-p (open-input-string buff)))
	     (when (not (=fx (string-length buff) len))
		(error "decode-sequence" "bad ASN1 sequence" #f))
	     (let loop ((rev-res '()))
		(if (eof-object? (peek-char str-p))
		    (reverse! rev-res)
		    (loop (cons (decode-DER str-p) rev-res)))))
	  (error "decode-sequence/set"
		 "only definite ASN-length decoding is implemented"
		 #f))))

(define (decode-sequence p)
   (decode-sequence/set p))

(define (decode-set p)
   (instantiate::DER-Set (set (decode-sequence/set p))))

(define (oids->symbol sub-oids)
   (string->symbol
    (apply string-append
	   (cons* "oid:"
		  (number->string (car sub-oids))
		  (map (lambda (oid)
			  (string-append "." (number->string oid)))
		       (cdr sub-oids))))))

(define (symbol->oids sym)
   (when (not (symbol? sym))
      (error "ASN1 object-id"
	     "invalid object-id. Object-ids must be symbols."
	     sym))
   (let ((str (symbol->string sym)))
      (when (not (string-prefix? "oid:" str))
	 (error "ASN1 object-id"
		"invalid object-id. Object-ids must start with 'oid:'"
		sym))
      (let loop ((rev-oids '())
		 (str (substring str (string-length "oid:")
				 (string-length str))))
	 (if (string-null? str)
	     (reverse! rev-oids)
	     (let ((oid (string->integer str))
		   (i (string-index str #\.)))
		(if (not i)
		    (loop (cons oid rev-oids) "")
		    (loop (cons oid rev-oids)
			  (substring str (+fx i 1) (string-length str)))))))))

(define (decode-object-id p)
   (define (decode-sub-oid p)
      (let loop ((res 0))
	 (let ((o (read-octet p)))
	    (cond
	       ((eof-object? o)
		(error "decode-object-id" "bad ASN1 object-id" #f))
	       ((not (zerofx? (bit-and #x80 o)))
		;; there are other bytes following.
		(loop (+fx (bit-lsh res 7) (bit-and o #x7F))))
	       (else
		(+fx (bit-lsh res 7) (bit-and o #x7F)))))))

   (define (split-first-oid sub-oid)
      (let ((sub-oid1 (quotientfx sub-oid 40))
	    (sub-oid2 (remainderfx sub-oid 40)))
	 ;; pfff: oid1 must be smaller than 4 (only 3 root oids). Should
	 ;;       it be bigger then we actually stole some amount from oid2.
	 ;;       Fix this in a loop.
	 (let loop ((oid1 sub-oid1)
		    (oid2 sub-oid2))
	    (cond
	       ((<fx oid1 3) (list oid1 oid2))
	       (else (loop (-fx oid1 1) (+fx oid2 40)))))))

   (let ((len (decode-length p)))
      (if len ;; definite
	  (let* ((buff (read-chars len p))
		 (str-p (open-input-string buff)))
	     (when (not (=fx (string-length buff) len))
		(error "decode-object-id" "bad ASN1 object-id" #f))
	     (let loop ((rev-oids '()))
		(if (eof-object? (peek-char str-p))
		    (oids->symbol (reverse! rev-oids))
		    (let ((sub-oid (decode-sub-oid str-p)))
		       (if (null? rev-oids) ;; the first one.
			   (loop (reverse! (split-first-oid sub-oid)))
			   (loop (cons sub-oid rev-oids)))))))
	  (error "decode-object-id"
		 "only definite ASN-length decoding is implemented"
		 #f))))

(define (encode-object-id el p)
   (define (merge-first-oids oids)
      (+fx (*fx 40 (car oids)) (cadr oids)))

   (define (encode-oid oid p)
      (define (inner oid last?)
	 (cond
	    (last?
	     ;; last one to be printed
	     (inner (bit-rsh oid 7) #f)
	     (display-octet (bit-and #x7F oid) p))
	    ((zerofx? oid)
	     'do-nothing)
	    (else
	     (inner (bit-rsh oid 7) #f)
	     (display-octet (+fx #x80 (bit-and #x7F oid)) p))))
      (inner oid #t))

   (let* ((oids (symbol->oids el))
	  (str-p (open-output-string))
	  (len (length oids)))
      (when (<fx len 2)
	 (error "encode-object-id" "don't know how to encode object id: " el))
      (encode-identifier 'universal 'primitive
			 (universal-tag 'object-identifier)
			 p)
      (let loop ((oids (cons (merge-first-oids oids) (cddr oids))))
	 (if (null? oids)
	     (let ((str (close-output-port str-p)))
		(encode-definite-length (string-length str) p)
		(let loop ((i 0))
		   (when (<fx i (string-length str))
		      (display-octet (string-ref str i) p)
		      (loop (+fx i 1)))))
	     (begin
		(encode-oid (car oids) str-p)
		(loop (cdr oids)))))))
		
(define (decode-octet-string p primitive?)
   (let* ((len (decode-length p))
	  (str (read-chars len p)))
      (if primitive?
	  (when (not (=fx len (string-length str)))
	     (error "decode-octet-string"
		    "bad DER octet string"
		    #f))
	     str)
	  (let ((str-p (open-output-string str)))
	     (let loop ((rev-strs '()))
		(if (eof-object? (peek-char str-p))
		    (apply string-append (reverse! rev-strs))
		    (receive (class prim/constr tag-num)
		       (decode-identifier str-p)
		       (when (not (eq? class 'universal))
			  (error "decode-DER octet string"
				 "bad octet string"
				 class))
		       (let ((tag (universal-inv-tag tag-num)))
			  (when (not (eq? 'octet-string tag))
			     (error "decode-DER octet string"
				    "bad octet string"
				    tag))
			  (loop (cons (decode-octet-string p (eq? 'primitive
								  prim/constr))
				      rev-strs)))))))))

(define (encode-octet-string str p)
   (when (not (string? str))
      (error "encode-octet-string"
	     "encode-octet-string requires string"
	     str))
   (let ((len (string-length str)))
      (encode-identifier 'universal 'primitive
			 (universal-tag 'octet-string)
			 p)
      (encode-definite-length len p)
      (display str p)))

(define (decode-bitstring p)
   (let ((len (decode-length p)))
      (if len ;; definite
	  (let* ((initial-octet (read-char p))
		 (data (read-chars (-fx len 1) p)))
	     (when (not (=fx (-fx len 1) (string-length data)))
		(error "decode-bitstring"
		       "bad DER bit string"
		       #f))
	     (instantiate::DER-BitString
		(data data)
		(unused-bits (char->integer initial-octet))))
	  (error "decode-DER bitstring"
		 "Only definite length implemented"
		 #f))))

(define (encode-bitstring bs::DER-BitString p)
   (with-access::DER-BitString bs (data unused-bits)
      (let ((len (string-length data)))
	 (encode-identifier 'universal 'primitive
			    (universal-tag 'bit-string)
			    p)
	 (encode-definite-length (+fx len 1) p)
	 (display-octet unused-bits p)
	 (display data p))))
   
(define (encode-DER el p)
   (cond
      ((boolean? el) (encode-bool el p))
      ((exact? el) (encode-int el p))
      ((inexact? el) (encode-real el p))
      ((pair? el) (encode-sequence el p))
      ((isa? el DER-Set) (encode-set el p))
      ((eq? el 'null) (encode-null p))
      ((symbol? el) (encode-object-id el p))
      ((string? el) (encode-octet-string el p))
      ((isa? el DER-BitString) (encode-bitstring el p))
      (else (error "encode-DER"
		   "encoding not yet implemented"
		   el))))

(define (skip-unknown p)
   (let ((len (decode-length p)))
      (read-chars len p)))

(define (decode-DER p)
   (receive (class prim/constr tag-num)
      (decode-identifier p)
      (when (not (eq? class 'universal))
	 (error "decode-DER"
		"only universal tags are implemented"
		class))
      (let ((tag (universal-inv-tag tag-num)))
	 (case tag
	    ((bool) (decode-bool p))
	    ((int) (decode-int/enumerated p))
	    ((sequence) (decode-sequence p))
	    ((set) (decode-set p))
	    ((null) (decode-null p))
	    ((object-identifier) (decode-object-id p))
	    ((octet-string) (decode-octet-string p (eq? 'primitive prim/constr)))
	    ((bit-string) (decode-bitstring p))
	    (else (error "decode-DER"
 			 "not yet implemented"
 			 tag))))))
