;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; LEB128 format writing.
;;
;; The format is described in section 5.2.2 of the specification and
;; https://en.wikipedia.org/wiki/LEB128.
;;
;; We do not use the usual implementation based on bitwise operations to not
;; bother with integer representation in bigloo.

(module leb128
   (export (leb128-write-signed n out-port)
           (leb128-write-unsigned n out-port)))

(define (new-write-byte b out-port)
   (cond ((elong? b)
          (write-byte (elong->fixnum b) out-port))
         ((bignum? b)
          (write-byte (bignum->fixnum b) out-port))
         (#t (write-byte b out-port))))

(define (leb128-write-unsigned n out-port)
   (if (< n 128)
       (new-write-byte n out-port)
       (begin
          (new-write-byte (+ 128 (modulo n 128)) out-port)
          (leb128-write-unsigned (quotient n 128) out-port))))

(define (leb128-write-signed n out-port)
   (cond
      ((and (<= 0 n) (> 64 n))
       (new-write-byte n out-port))
      ((and (> 0 n) (<= -64 n))
       (new-write-byte (+ 128 n) out-port))
      (#t
       (let* ((m (modulo n 128)))
          (new-write-byte (+ 128 m) out-port)
          (leb128-write-signed (quotient (- n m) 128) out-port)))))
