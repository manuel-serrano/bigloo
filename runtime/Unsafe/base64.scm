;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/base64.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 29 17:52:57 2004                          */
;*    Last change :  Wed Sep 24 19:29:48 2014 (serrano)                */
;*    Copyright   :  2004-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    base64 encoding/decoding                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __base64
   
   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
	   __bit
	   __bignum
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_numbers_6_5_flonum_dtoa
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2 
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __foreign
	   __error
	   __evenv
	   __os
	   __object
	   __r4_input_6_10_2
	   __r4_output_6_10_3
	   __rgc
	   __srfi4)
   
   (import __param)
   
   (export (base64-encode::bstring ::bstring #!optional (padding 76))
	   (base64-decode::bstring ::bstring #!optional eof-no-padding)
	   (base64-encode-port ::input-port ::output-port  #!optional (padding 76))
	   (base64-decode-port ::input-port ::output-port #!optional eof-no-padding)
	   (pem-decode-port ::input-port ::output-port)
	   (pem-read-file::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    base64-table ...                                                 */
;*---------------------------------------------------------------------*/
(define (base64-table)
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;*---------------------------------------------------------------------*/
;*    encode-char ...                                                  */
;*---------------------------------------------------------------------*/
(define (encode-char o)
   (string-ref (base64-table) o))

;*---------------------------------------------------------------------*/
;*    base64-encode ...                                                */
;*---------------------------------------------------------------------*/
(define (base64-encode s #!optional (padding 76))
   (let* ((x 0)
	  (y 0)
	  (n 3)
	  (len (string-length s))
	  (len-3 (-fx len 3))
	  (rlen (*fx 4 (/fx (+fx 2 len) 3)))
	  (pad (if (and (fixnum? padding) (>fx padding 0))
		   (/fx (*fx 3 padding) 4)
		   -1))
	  (flen (if (>fx pad 0)
		    (+fx rlen (/fx rlen padding))
		    rlen))
	  (res (make-string flen #\Newline)))
      (let loop ((x 0)
		 (y 0))
	 (if (<=fx x len-3)
	     (let* ((i0 (char->integer (string-ref s x)))
		    (i1 (char->integer (string-ref s (+fx 1 x))))
		    (i2 (char->integer (string-ref s (+fx 2 x))))
		    (o0 (bit-rsh (bit-and i0 #xfc) 2))
		    (o1 (bit-or (bit-lsh (bit-and i0 #x03) 4)
				(bit-rsh (bit-and i1 #xf0) 4)))
		    (o2 (bit-or (bit-lsh (bit-and i1 #x0f) 2)
				(bit-rsh (bit-and i2 #xc0) 6)))
		    (o3 (bit-and i2 #x3f)))
		(string-set! res y (encode-char o0))
		(string-set! res (+fx y 1) (encode-char o1))
		(string-set! res (+fx y 2) (encode-char o2))
		(string-set! res (+fx y 3) (encode-char o3))
		(let ((nx (+fx x 3)))
		   (if (and (>fx pad 0) (=fx (remainderfx nx pad) 0))
		       (loop nx (+fx y 5))
		       (loop nx (+fx y 4)))))
	     (case (-fx len x)
		((2)
		 (let* ((i0 (char->integer (string-ref s x)))
			(i1 (char->integer (string-ref s (+fx x 1))))
			(o0 (bit-rsh (bit-and i0 #xfc) 2))
			(o1 (bit-or (bit-lsh (bit-and i0 #x03) 4)
				    (bit-rsh (bit-and i1 #xf0) 4)))
			(o2 (bit-lsh (bit-and i1 #x0f) 2)))
		    (string-set! res y (encode-char o0))
		    (string-set! res (+fx y 1) (encode-char o1))
		    (string-set! res (+fx y 2) (encode-char o2))
		    (string-set! res (+fx y 3) #\=)))
		((1)
		 (let* ((i (char->integer (string-ref s x)))
			(o0 (bit-rsh (bit-and i #xfc) 2))
			(o1 (bit-lsh (bit-and i #x03) 4)))
		    (string-set! res y (encode-char o0))
		    (string-set! res (+fx y 1) (encode-char o1))
		    (string-set! res (+fx y 2) #\=)
		    (string-set! res (+fx y 3) #\=))))))
      res))

;*---------------------------------------------------------------------*/
;*    base64-encode-port ...                                           */
;*---------------------------------------------------------------------*/
(define (base64-encode-port ip op #!optional (padding 76))
   (let ((pad (-fx padding 4)))
      (let loop ((i 0))
	 (let ((i0 (read-byte ip)))
	    (unless (eof-object? i0)
	       (let ((i1 (read-byte ip)))
		  (if (eof-object? i1)
		      (let* ((o0 (bit-rsh (bit-and i0 #xfc) 2))
			     (o1 (bit-lsh (bit-and i0 #x03) 4)))
			 (write-char (encode-char o0) op)
			 (write-char (encode-char o1) op)
			 (write-char #\= op)
			 (write-char #\= op))
		      (let ((i2 (read-byte ip)))
			 (if (eof-object? i2)
			     (let* ((o0 (bit-rsh (bit-and i0 #xfc) 2))
				    (o1 (bit-or (bit-lsh (bit-and i0 #x03) 4)
						(bit-rsh (bit-and i1 #xf0) 4)))
				    (o2 (bit-lsh (bit-and i1 #x0f) 2)))
				(write-char (encode-char o0) op)
				(write-char (encode-char o1) op)
				(write-char (encode-char o2) op)
				(write-char #\= op))
			     (let* ((o0 (bit-rsh (bit-and i0 #xfc) 2))
				    (o1 (bit-or (bit-lsh (bit-and i0 #x03) 4)
						(bit-rsh (bit-and i1 #xf0) 4)))
				    (o2 (bit-or (bit-lsh (bit-and i1 #x0f) 2)
						(bit-rsh (bit-and i2 #xc0) 6)))
				    (o3 (bit-and i2 #x3f)))
				(write-char (encode-char o0) op)
				(write-char (encode-char o1) op)
				(write-char (encode-char o2) op)
				(write-char (encode-char o3) op)
				(if (and (>=fx i pad) (>fx pad 0))
				    (begin
				       (newline op)
				       (loop 0))
				    (loop (+fx i 4)))))))))))))

;*---------------------------------------------------------------------*/
;*    byte-table ...                                                   */
;*---------------------------------------------------------------------*/
(define byte-table
   (let ((table (make-u8vector 128 0)))
      (define (byte-set! t o v)
	 (u8vector-set! t o v))
      (let loop ((i 0))
	 (when (<fx i 26)
	    (byte-set! table (+fx i (char->integer #\A)) i)
	    (byte-set! table (+fx i (char->integer #\a)) (+fx i 26))
	    (loop (+fx i 1))))
      (let loop ((i 0))
	 (when (<fx i 10)
	    (byte-set! table (+fx i (char->integer #\0)) (+fx i 52))
	    (loop (+fx i 1))))
      (byte-set! table (char->integer #\+) 62)
      (byte-set! table (char->integer #\-) 62)
      (byte-set! table (char->integer #\/) 63)
      (byte-set! table (char->integer #\_) 63)
      table))

;*---------------------------------------------------------------------*/
;*    decode-byte ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (decode-byte::int c::byte)
   (int8->fixnum (u8vector-ref byte-table c)))

;*---------------------------------------------------------------------*/
;*    decode-char ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (decode-char::int c::char)
   (decode-byte (char->integer c)))

;*---------------------------------------------------------------------*/
;*    actual-string-length ...                                         */
;*---------------------------------------------------------------------*/
(define (actual-string-length s)
   (let loop ((i (-fx (string-length s) 1)))
      (if (=fx i 0)
	  i
	  (let ((c (string-ref s i)))
	     (if (or (char=? c #\Newline) (char=? c #\Return))
		 (loop (-fx i 1))
		 (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    base64-decode ...                                                */
;*---------------------------------------------------------------------*/
(define (base64-decode s #!optional eof-no-padding)
   (let* ((len (actual-string-length s))
	  (nlen (if eof-no-padding
		    (*fx (+fx 1 (/fx len 4)) 3)
		    (*fx (/fx len 4) 3)))
	  (res (make-string nlen)))
      (let loop ((x 0)
		 (y 0))
	 (if (<fx x len)
	     (let* ((c (string-ref s (+fx x 0)))
		    (q0 (decode-char c)))
		(if (and (=fx q0 0)
			 (or (char=? c #\Newline) (char=? c #\Return)))
		    (loop (+fx x 1) y)
		    (cond
		       ((<=fx x (-fx len 4))
			(let* ((q1 (decode-char (string-ref s (+fx x 1))))
			       (q2 (decode-char (string-ref s (+fx x 2))))
			       (q3 (decode-char (string-ref s (+fx x 3))))
			       (v0 (bit-or (bit-lsh q0 2)
				      (bit-rsh q1 4)))
			       (v1 (bit-or (bit-and (bit-lsh q1 4) #xf0)
				      (bit-rsh q2 2)))
			       (v2 (bit-or (bit-and (bit-lsh q2 6) #xc0)
				      q3)))
			   (string-set! res y (integer->char v0))
			   (string-set! res (+fx y 1) (integer->char v1))
			   (string-set! res (+fx y 2) (integer->char v2))
			   (loop (+fx x 4) (+fx y 3))))
		       ((and (<=fx x (-fx len 3)) eof-no-padding)
			(let* ((c1 (string-ref s (+fx x 1)))
			       (c2 (string-ref s (+fx x 2)))
			       (q1 (decode-char c1))
			       (q2 (decode-char c2))
			       (v0 (bit-or (bit-lsh q0 2)
				      (bit-rsh q1 4)))
			       (v1 (bit-or (bit-and (bit-lsh q1 4) #xf0)
				      (bit-rsh q2 2))))
			   (string-set! res y (integer->char v0))
			   (string-set! res (+fx y 1) (integer->char v1))
			   (cond
			      ((char=? c1 #\=)
			       (string-shrink! res y))
			      ((char=? c2 #\=)
			       (string-shrink! res (+fx y 1)))
			      (else
			       (string-shrink! res (+fx y 2))))))
		       ((and (<=fx x (-fx len 2)) eof-no-padding)
			(let* ((c1 (string-ref s (+fx x 1)))
			       (q1 (decode-char c1))
			       (v0 (bit-or (bit-lsh q0 2)
				      (bit-rsh q1 4))))
			   (string-set! res y (integer->char v0))
			   (if (char=? c1 #\=)
			       (string-shrink! res y)
			       (string-shrink! res (+fx y 1)))))
		       ((and (<=fx x (-fx len 1)) eof-no-padding)
			(let* ((q1 (decode-char #\=))
			       (v0 (bit-or (bit-lsh q0 2) (bit-rsh q1 4))))
			   (string-set! res y (integer->char v0))
			   (string-shrink! res y)))
		       (else
			(string-shrink! res (+fx y 1))))))
	     (cond
		((and (>fx len 2) (char=? (string-ref s (-fx len 2)) #\=))
		 (string-shrink! res (-fx y 2)))
		((and (>fx len 1) (char=? (string-ref s (-fx len 1)) #\=))
		 (string-shrink! res (-fx y 1)))
		(else
		 (if (<fx y nlen)
		     (string-shrink! res y)
		     res)))))))

;*---------------------------------------------------------------------*/
;*    base64-decode-grammar ...                                        */
;*    -------------------------------------------------------------    */
;*    write-char, write-byte, and display-string are too slow, hence   */
;*    we use an ad-hoc buffer to speed up outputs. The size of this    */
;*    buffer must be a multiplier of 3.                                */
;*---------------------------------------------------------------------*/
(define base64-decode-grammar
   ;; http://en.wikipedia.org/wiki/Base64
   (regular-grammar ((variant (in "-+/_"))
		     (chunk (or (in ("AZ")) (in ("az")) (in ("09")) variant))
		     op buf w len hook
		     eof-no-padding)

      (define (chunk4)
       (let* ((q0 (decode-byte (the-byte-ref 0)))
	      (q1 (decode-byte (the-byte-ref 1)))
	      (q2 (decode-byte (the-byte-ref 2)))
	      (q3 (decode-byte (the-byte-ref 3)))
	      (v0 (bit-or (bit-lsh q0 2) (bit-rsh q1 4)))
	      (v1 (bit-or (bit-and (bit-lsh q1 4) #xf0) (bit-rsh q2 2)))
	      (v2 (bit-or (bit-and (bit-lsh q2 6) #xc0) q3)))
	  (string-set-ur! buf w (integer->char v0))
	  (string-set-ur! buf (+fx w 1) (integer->char v1))
	  (string-set-ur! buf (+fx w 2) (integer->char v2))
	  (let ((nw (+fx w 3)))
	     (if (=fx nw len)
		 (begin
		    (display-string buf op)
		    (set! w 0))
		 (set! w nw)))))
      
      (define (chunk3)
       (let* ((q0 (decode-byte (the-byte-ref 0)))
	      (q1 (decode-byte (the-byte-ref 1)))
	      (q2 (decode-byte (the-byte-ref 2)))
	      (v0 (bit-or (bit-lsh q0 2) (bit-rsh q1 4)))
	      (v1 (bit-or (bit-and (bit-lsh q1 4) #xf0) (bit-rsh q2 2)))
	      (v2 (bit-and (bit-lsh q2 6) #xc0)))
	  (string-set-ur! buf w (integer->char v0))
	  (string-set-ur! buf (+fx w 1) (integer->char v1))
	  (string-set-ur! buf (+fx w 2) (integer->char v2))
	  (display-substring buf 0 (+fx w 2) op)))

      (define (chunk2)
       (let* ((q0 (decode-byte (the-byte-ref 0)))
	      (q1 (decode-byte (the-byte-ref 1)))
	      (v0 (bit-or (bit-lsh q0 2) (bit-rsh q1 4)))
	      (v1 (bit-and (bit-lsh q1 4) #xf0)))
	  (string-set-ur! buf w (integer->char v0))
	  (string-set-ur! buf (+fx w 1) (integer->char v1))
	  (display-substring buf 0 (+fx w 1) op)))

      (define (chunk1)
       (let* ((q0 (decode-byte (the-byte-ref 0)))
	      (q1 (decode-char #\=))
	      (v0 (bit-or (bit-lsh q0 2) (bit-rsh q1 4))))
	  (string-set-ur! buf w (integer->char v0))
	  (display-substring buf 0 w op)))
      
      ((= 4 chunk)
       (chunk4)
       (ignore))
      ((: (= 3 chunk) #\=)
       (chunk3))
      ((eof (= 3 chunk))
       (when eof-no-padding (chunk3)))
      ((: (= 2 chunk) #\= #\=)
       (chunk2))
      ((eof (: (= 2 chunk) (? #\=)))
       (when eof-no-padding (chunk2)))
      ((: chunk #\= #\= #\=)
       (chunk1))
      ((eof chunk)
       (when eof-no-padding (chunk1)))
      ((or #\Newline #\Return)
       (ignore))
      (else
       (let ((c (the-failure)))
	  (if (or (eof-object? c) (hook c))
	      (begin
		 (when (>fx w 0)
		    (display-substring buf 0 w op))
		 #t)
	     (ignore))))))

;*---------------------------------------------------------------------*/
;*    base64-decode-port ...                                           */
;*---------------------------------------------------------------------*/
(define (base64-decode-port ip op #!optional eof-no-padding)
   (read/rp base64-decode-grammar ip op (make-string 84) 0 84
      (lambda (c) #f) #t))

;*---------------------------------------------------------------------*/
;*    pem-markup-grammar ...                                           */
;*---------------------------------------------------------------------*/
(define pem-markup-grammar
   (regular-grammar (count)
      ((+ #\-)
       (set! count (+fx (the-length) count))
       (ignore))
      ((: (+ #\-) #\Newline)
       (-fx (the-length) 1))
      ((+ (out #\- #\Newline #\Return))
       (let* ((s (the-string))
	      (counte (ignore)))
	  (if (eq? count counte)
	      s
	      (raise (instantiate::&io-parse-error
			(proc "pem-decode-port")
			(msg "Illegal PEM markup")
			(obj (list s count counte)))))))
      (else
       (let ((c (the-failure)))
	  (raise (instantiate::&io-parse-error
		    (proc "pem-decode-port")
		    (msg "Illegal character in PEM markup")
		    (obj (format "{~a}~a" c (read-line (the-port))))))))))

;*---------------------------------------------------------------------*/
;*    pem-decode-port ...                                              */
;*---------------------------------------------------------------------*/
(define (pem-decode-port ip op)
   (define (hook start c)
      ;; check the correctness of the closing markup
      (if (not (char=? c #\-))
	  (raise (instantiate::&io-parse-error
		    (proc "pem-decode-port")
		    (msg "Illegal character")
		    (obj (format "{~a}~a" c (read-line ip)))))
	  (let ((end (read/rp pem-markup-grammar ip 1)))
	     (if (substring-at? end "END " 0)
		 (if (string=? start (substring end 5 (string-length end)))
		     #t
		     (raise
		      (instantiate::&io-parse-error
			 (proc "pem-decode-port")
			 (msg "PEM begin/end markup mismatch")
			 (obj end))))))))
   ;; read the PEM header
   (let ((start (read/rp pem-markup-grammar ip 0)))
      (if (substring-at? start "BEGIN " 0)
	  (read/rp base64-decode-grammar ip op (make-string 84) 0 84
		   (lambda (c)
		      (hook (substring start 7 (string-length start)) c))
		   #f)
	  (raise
	   (instantiate::&io-parse-error
	      (proc "pem-decode-port")
	      (msg "Illegal PEM begin markup")
	      (obj start))))))

;*---------------------------------------------------------------------*/
;*    pem-read-file ...                                                */
;*---------------------------------------------------------------------*/
(define (pem-read-file file)
   (let ((p (open-output-string)))
      (with-input-from-file file
	 (lambda ()
	    (pem-decode-port (current-input-port) p)))
      (close-output-port p)))
