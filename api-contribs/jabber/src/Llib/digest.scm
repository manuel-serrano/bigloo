(module digest
	(export  (digest-string str algorithm result-type))
		(library pthread)
	)

(define (hex8 n) ;; assumes n is a fixnum
  (substring (number->string (+ n #x100) 16) 1 3))

(define (hex16 n) ;; assumes n is a fixnum
  (substring (number->string (+ n #x10000) 16) 1 5))

(define-struct block-digest
  hash-update
  hash
  block
  block-pos
  bit-pos
  big-endian?
  width
)

(define-struct digest
  end
  update
  state)

(define-macro (snow-fxand x y) `(bit-and ,x ,y))
(define-macro (snow-fxior x y) `(bit-or  ,x ,y))
(define-macro (snow-fxxor x y) `(bit-xor ,x ,y))
(define-macro (snow-fxnot x)   `(bit-not ,x))

(define-macro (snow-fxarithmetic-shift-left x n)
  `(bit-lsh ,x ,n))

(define-macro (snow-fxarithmetic-shift-right x n)
  `(bit-rsh ,x ,n))

;;;----------------------------------------------------------------------------

(define (hash-block->hex-string hb big-endian? width)

  (define (hex x)
    (string-ref "0123456789abcdef" (snow-fxand x 15)))

  (let* ((len (quotient width 16))
         (n (* len 4))
         (str (make-string n)))
    (let loop ((i (- len 1)) (j (- n 4)))
      (if (< i 0)
          str
          (let ((x (vector-ref hb (if big-endian? (snow-fxxor i 1) i)))
                (j1 (if big-endian? (+ j 0) (+ j 2)))
                (j2 (if big-endian? (+ j 2) (+ j 0))))
            (string-set!
             str
             (+ j1 0)
             (hex (snow-fxarithmetic-shift-right x 12)))
            (string-set!
             str
             (+ j1 1)
             (hex (snow-fxarithmetic-shift-right x 8)))
            (string-set!
             str
             (+ j2 0)
             (hex (snow-fxarithmetic-shift-right x 4)))
            (string-set!
             str
             (+ j2 1)
             (hex (snow-fxarithmetic-shift-right x 0)))
            (loop (- i 1) (- j 4)))))))

(define (hash-block->u8vector hb big-endian? width)
  (let* ((len (quotient width 16))
         (n (* len 2))
         (u8vect (make-u8vector n 0)))
    (let loop ((i (- len 1)) (j (- n 2)))
      (if (< i 0)
          u8vect
          (let ((x (vector-ref hb (if big-endian? (snow-fxxor i 1) i)))
                (j1 (if big-endian? (+ j 0) (+ j 1)))
                (j2 (if big-endian? (+ j 1) (+ j 0))))
            (u8vector-set! u8vect j1 (snow-fxarithmetic-shift-right x 8))
            (u8vector-set! u8vect j2 (snow-fxand #xff x))
            (loop (- i 1) (- j 2)))))))

(define-macro (LO var)
  (string->symbol
   (string-append (symbol->string var) "-" (symbol->string 'L))))

(define-macro (HI var)
  (string->symbol
   (string-append (symbol->string var) "-" (symbol->string 'H))))

(define-macro (wlet var lo hi body)
  `(let ((,(string->symbol
            (string-append (symbol->string var) "-" (symbol->string 'L)))
          ,lo)
         (,(string->symbol
            (string-append (symbol->string var) "-" (symbol->string 'H)))
          ,hi))
     ,body))

(define-macro (cast-u16 x)
  `(snow-fxand #xffff ,x))

(define-macro (shift-left-u16 n shift)
  `(snow-fxarithmetic-shift-left
    (snow-fxand ,n ,(- (expt 2 (- 16 shift)) 1))
    ,shift))

(define-macro (wshr dst w r body)
  (if (< r 16)
      `(wlet ,dst
             (snow-fxior
              (snow-fxarithmetic-shift-right (LO ,w) ,r)
              (shift-left-u16 (HI ,w) ,(- 16 r)))
             (snow-fxarithmetic-shift-right (HI ,w) ,r)
             ,body)
      `(wlet ,dst
             (snow-fxarithmetic-shift-right (HI ,w) ,(- r 16))
             0
             ,body)))

(define-macro (wrot dst w r body)
  (if (< r 16)
      `(wlet ,dst
             (snow-fxior
              (shift-left-u16 (LO ,w) ,r)
              (snow-fxarithmetic-shift-right (HI ,w) ,(- 16 r)))
             (snow-fxior
              (shift-left-u16 (HI ,w) ,r)
              (snow-fxarithmetic-shift-right (LO ,w) ,(- 16 r)))
             ,body)
      `(wlet ,dst
             (snow-fxior
              (shift-left-u16 (HI ,w) ,(- r 16))
              (snow-fxarithmetic-shift-right (LO ,w) ,(- 32 r)))
             (snow-fxior
              (shift-left-u16 (LO ,w) ,(- r 16))
              (snow-fxarithmetic-shift-right (HI ,w) ,(- 32 r)))
             ,body)))

(define-macro (wadd dst a b body)
  `(wlet R
         (+ (LO ,a) (LO ,b))
         (+ (HI ,a) (HI ,b))
         (wlet ,dst
               (cast-u16 (LO R))
               (cast-u16
                (+ (HI R)
                   (snow-fxarithmetic-shift-right (LO R) 16)))
               ,body)))

(define-macro (wxor dst a b body)
  `(wlet ,dst
         (snow-fxxor (LO ,a) (LO ,b))
         (snow-fxxor (HI ,a) (HI ,b))
         ,body))

(define-macro (wior dst a b body)
  `(wlet ,dst
         (snow-fxior (LO ,a) (LO ,b))
         (snow-fxior (HI ,a) (HI ,b))
         ,body))

(define-macro (wand dst a b body)
  `(wlet ,dst
         (snow-fxand (LO ,a) (LO ,b))
         (snow-fxand (HI ,a) (HI ,b))
         ,body))

(define-macro (wnot dst a body)
  `(wlet ,dst
         (snow-fxnot (LO ,a))
         (snow-fxnot (HI ,a))
         ,body))

(define-macro (wref dst v i body)
  (if (number? i)
      `(wlet ,dst
             (vector-ref ,v ,(+ (* 2 i) 0))
             (vector-ref ,v ,(+ (* 2 i) 1))
             ,body)
      `(wlet ,dst
             (vector-ref ,v (+ (* 2 ,i) 0))
             (vector-ref ,v (+ (* 2 ,i) 1))
             ,body)))

(define-macro (wset v i x)
  (if (number? i)
      `(begin
         (vector-set! ,v ,(+ (* 2 i) 0) (LO ,x))
         (vector-set! ,v ,(+ (* 2 i) 1) (HI ,x)))
      `(begin
         (vector-set! ,v (+ (* 2 ,i) 0) (LO ,x))
         (vector-set! ,v (+ (* 2 ,i) 1) (HI ,x)))))

;;;----------------------------------------------------------------------------

(define (convert-hash-block digest result-type)
  (let* ((bd (digest-state digest))
         (hash (block-digest-hash bd)))
    (case result-type
      ((hex)
       (hash-block->hex-string
        hash
        (block-digest-big-endian? bd)
        (block-digest-width bd)))
      ((u8vector)
       (hash-block->u8vector
        hash
        (block-digest-big-endian? bd)
        (block-digest-width bd)))
      (else
       (print "unsupported digest result-type" result-type)))))

(define (process-last-block digest)
  (let* ((bd
          (digest-state digest))
         (block-pos
          (block-digest-block-pos bd))
         (bit-pos
          (block-digest-bit-pos bd))
         (buf
          (make-u8vector 8 0)))

    (digest-update-u8 digest #x80) ;; add byte-aligned 1 bit

    (let ((zero-padding-bytes
           (quotient
            (snow-fxand 511 (- 448 (block-digest-bit-pos bd)))
            8)))
      (let loop1 ((n zero-padding-bytes))
        (if (< 0 n)
            (let ((m (min 8 n)))
              (digest-update-subu8vector
               digest
               buf
               0
               m) ;; add 0 bits
              (loop1 (- n m))))))

    (u8vector-set!
     buf
     0
     (snow-fxand #xff bit-pos))

    (u8vector-set!
     buf
     1
     (snow-fxior
      (snow-fxarithmetic-shift-left (snow-fxand #x7f block-pos) 1)
      (snow-fxand #x01 (snow-fxarithmetic-shift-right bit-pos 8))))

    (let loop2 ((i 2)
                (n (snow-fxarithmetic-shift-right block-pos 7)))
      (if (> n 0)
          (begin
            (u8vector-set! buf i (snow-fxand #xff n))
            (loop2 (+ i 1)
                   (snow-fxarithmetic-shift-right n 8)))))

    (if (block-digest-big-endian? bd)
        (let loop3 ((i 3))
          (if (>= i 0)
              (let ((t (u8vector-ref buf i)))
                (u8vector-set! buf i (u8vector-ref buf (- 7 i)))
                (u8vector-set! buf (- 7 i) t)
                (loop3 (- i 1))))))

    (digest-update-subu8vector digest buf 0 8)));; add message length (in bits)

(define (end-block-digest digest result-type)
  (process-last-block digest)
  (convert-hash-block digest result-type))

(define (digest-update-block-digest digest u8vect start end)
  (let* ((bd (digest-state digest))
         (block (block-digest-block bd)))

    (define (aligned8 i bit-pos)

      ;; bit-pos is a multiple of 8

      (if (< i end)
          (let ((j (snow-fxarithmetic-shift-right bit-pos 4)))
            (if (= 0 (snow-fxand bit-pos 15))
                (begin
                  (if (block-digest-big-endian? bd)
                      (let ((j (snow-fxxor j 1)))
                        (vector-set!
                         block
                         j
                         (snow-fxarithmetic-shift-left
                          (u8vector-ref u8vect i)
                          8)))
                      (vector-set!
                       block
                       j
                       (u8vector-ref u8vect i)))
                  (let ((new-bit-pos (+ bit-pos 8)))
                    (aligned8 (+ i 1) new-bit-pos)))
                (begin
                  (if (block-digest-big-endian? bd)
                      (let ((j (snow-fxxor j 1)))
                        (vector-set!
                         block
                         j
                         (+ (vector-ref block j)
                            (u8vector-ref u8vect i))))
                      (vector-set!
                       block
                       j
                       (+ (vector-ref block j)
                          (snow-fxarithmetic-shift-left
                           (u8vector-ref u8vect i)
                           8))))
                  (let ((new-bit-pos (+ bit-pos 8)))
                    (if (= 512 new-bit-pos)
                      (begin
                        ((block-digest-hash-update bd) digest)
                        (block-digest-block-pos-set!
                         bd
                         (+ (block-digest-block-pos bd) 1))
                        (aligned16 (+ i 1) 0))
                      (aligned16 (+ i 1) new-bit-pos))))))
          (block-digest-bit-pos-set! bd bit-pos)))

    (define (aligned16 i bit-pos)

      ;; bit-pos is a multiple of 16

      (if (< (+ i 1) end)
          (let ((j (snow-fxarithmetic-shift-right bit-pos 4)))
            (if (block-digest-big-endian? bd)
                (let ((j (snow-fxxor j 1)))
                  (vector-set!
                   block
                   j
                   (+
                    (snow-fxarithmetic-shift-left
                     (u8vector-ref u8vect i)
                     8)
                    (u8vector-ref u8vect (+ i 1)))))
                (vector-set!
                 block
                 j
                 (+
                  (snow-fxarithmetic-shift-left
                   (u8vector-ref u8vect (+ i 1))
                   8)
                  (u8vector-ref u8vect i))))
            (let ((new-bit-pos (+ bit-pos 16)))
              (if (= 512 new-bit-pos)
                  (begin
                    ((block-digest-hash-update bd) digest)
                    (block-digest-block-pos-set!
                     bd
                     (+ (block-digest-block-pos bd) 1))
                    (aligned16 (+ i 2) 0))
                  (aligned16 (+ i 2) new-bit-pos))))
          (aligned8 i bit-pos)))

    (let ((bit-pos (block-digest-bit-pos bd)))
      (cond ((= 0 (snow-fxand bit-pos 15)) ;; 16 bit boundary?
             (aligned16 start bit-pos))
            (else
             ;; (= 0 (snow-fxand bit-pos 7)) ;; 8 bit boundary?
             (aligned8 start bit-pos))))))

;;;----------------------------------------------------------------------------

;; SHA-1 digest.

(define (hash-block-init-sha-1)
  (vector #x2301 #x6745
          #xab89 #xefcd
          #xdcfe #x98ba
          #x5476 #x1032
          #xe1f0 #xc3d2))

(define (digest-update-sha-1 digest)
  (let* ((bd (digest-state digest))
         (hash (block-digest-hash bd))
         (block (block-digest-block bd)))
    (wref OLDA hash 0
    (wref OLDB hash 1
    (wref OLDC hash 2
    (wref OLDD hash 3
    (wref OLDE hash 4
    (let loop ((j 0)
               (A-L OLDA-L) (A-H OLDA-H)
               (B-L OLDB-L) (B-H OLDB-H)
               (C-L OLDC-L) (C-H OLDC-H)
               (D-L OLDD-L) (D-H OLDD-H)
               (E-L OLDE-L) (E-H OLDE-H))

      (define (stage1)
        (if (< j 16)

            (wref T1 block j
            (stage2 T1-L T1-H))

            (wref T1 block (- j 3)
            (wref T2 block (- j 8)
            (wxor T3 T1 T2
            (wref T4 block (- j 14)
            (wxor T5 T3 T4
            (wref T6 block (- j 16)
            (wxor T7 T5 T6
            (wrot X T7 1
            (begin
              (wset block j X)
              (stage2 X-L X-H))))))))))))

      (define (stage2 X-L X-H)
        (cond ((< j 20)
               (wand T1 B C
               (wnot T2 B
               (wand T3 D T2
               (wior T4 T1 T3
               (wlet T5 #x7999 #x5a82
               (wadd T6 T4 T5
               (stage3 X-L X-H T6-L T6-H))))))))
              ((< j 40)
               (wxor T1 B C
               (wxor T2 D T1
               (wlet T3 #xeba1 #x6ed9
               (wadd T4 T2 T3
               (stage3 X-L X-H T4-L T4-H))))))
              ((< j 60)
               (wand T1 B C
               (wand T2 B D
               (wior T3 T1 T2
               (wand T4 C D
               (wior T5 T3 T4
               (wlet T6 #xbcdc #x8f1b
               (wadd T7 T5 T6
               (stage3 X-L X-H T7-L T7-H)))))))))
              (else
               (wxor T1 B C
               (wxor T2 D T1
               (wlet T3 #xc1d6 #xca62
               (wadd T4 T2 T3
               (stage3 X-L X-H T4-L T4-H))))))))

      (define (stage3 X-L X-H Y-L Y-H)
        (wrot T1 A 5
        (wadd T2 E T1
        (wadd T3 X T2
        (wadd T4 Y T3
        (wrot T5 B 30
        (loop (+ j 1)
              T4-L T4-H
              A-L A-H
              T5-L T5-H
              C-L C-H
              D-L D-H)))))))

      (if (< j 80)

          (stage1)

          (begin
            (wadd NEWA A OLDA (wset hash 0 NEWA))
            (wadd NEWB B OLDB (wset hash 1 NEWB))
            (wadd NEWC C OLDC (wset hash 2 NEWC))
            (wadd NEWD D OLDD (wset hash 3 NEWD))
            (wadd NEWE E OLDE (wset hash 4 NEWE))))))))))))


(define (open-diges-sha-1)
  (let ((md (make-digest))(mbd (make-block-digest)))

    (block-digest-hash-update-set! mbd  digest-update-sha-1)
    (block-digest-hash-set! mbd (hash-block-init-sha-1))
    (block-digest-block-set! mbd (make-vector 160 0))
    (block-digest-block-pos-set! mbd 0)
    (block-digest-bit-pos-set! mbd 0)
    (block-digest-big-endian?-set! mbd #t)
    (block-digest-width-set! mbd 160)

   (digest-end-set! md end-block-digest)
   (digest-update-set! md  digest-update-block-digest)
   (digest-state-set! md mbd)
   md
    ))

;;;----------------------------------------------------------------------------

(define (open-digest algorithm)
   (open-diges-sha-1))

(define-macro (digest-default-result-type) ''hex)

(define (close-digest
          digest
          result-type)
  ((digest-end digest) digest (digest-default-result-type))

)

(define (digest-update-subu8vector digest u8vect start end)
  ((digest-update digest) digest u8vect start end)
  ) 

(define zero-u8vector (make-u8vector 4 0))

(define (get-zero-u8vector) zero-u8vector)

(define (digest-update-u8 digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (make-u8vector 1 (snow-fxand n #xff)))
   0
   1))

(define (digest-update-u16-le digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-u8vector 2)))
         (u8vector-set!
          u8vect
          0
          (snow-fxand n #xff))
         (u8vector-set!
          u8vect
          1
          (snow-fxand (snow-fxarithmetic-shift-right n 8) #xff))
         u8vect))
   0
   2))

(define (digest-update-u16-be digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-u8vector 2)))
         (u8vector-set!
          u8vect
          1
          (snow-fxand n #xff))
         (u8vector-set!
          u8vect
          0
          (snow-fxand (snow-fxarithmetic-shift-right n 8) #xff))
         u8vect))
   0
   2))

(define (digest-update-u32-le digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-u8vector 4)))
         (u8vector-set!
          u8vect
          0
          (snow-fxand n #xff))
         (u8vector-set!
          u8vect
          1
          (snow-fxand (snow-fxarithmetic-shift-right n 8) #xff))
         (u8vector-set!
          u8vect
          2
          (snow-fxand (snow-fxarithmetic-shift-right n 16) #xff))
         (u8vector-set!
          u8vect
          3
          (snow-fxand (snow-fxarithmetic-shift-right n 24) #xff))
         u8vect))
   0
   4))

(define (digest-update-u32-be digest n) ;; assumes n is a fixnum
  (digest-update-subu8vector
   digest
   (if (eqv? n 0)
       (get-zero-u8vector)
       (let ((u8vect (make-u8vector 4)))
         (u8vector-set!
          u8vect
          3
          (snow-fxand n #xff))
         (u8vector-set!
          u8vect
          2
          (snow-fxand (snow-fxarithmetic-shift-right n 8) #xff))
         (u8vector-set!
          u8vect
          1
          (snow-fxand (snow-fxarithmetic-shift-right n 16) #xff))
         (u8vector-set!
          u8vect
          0
          (snow-fxand (snow-fxarithmetic-shift-right n 24) #xff))
         u8vect))
   0
   4))

(define (digest-string
          str
          algorithm
          result-type)
  (digest-substring
   str
   0
   (string-length str)
   algorithm
   (digest-default-result-type)))

(define (digest-substring
          str
          start
          end
          algorithm
          result-type )
  (let* ((len (- end start))
         (u8vect (make-u8vector len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (u8vector-set! u8vect i (char->integer (string-ref str i)))
            (loop (+ i 1)))
          (digest-subu8vector u8vect 0 len algorithm  (digest-default-result-type))))))

(define (digest-u8vector
          u8vect
          algorithm
          result-type)
  (digest-subu8vector
   u8vect
   0
   (u8vector-length u8vect)
   algorithm
   (digest-default-result-type) ))

(define (digest-subu8vector
          u8vect
          start
          end
          algorithm
          result-type)
  (let ((digest (open-digest algorithm)))
    (digest-update-subu8vector digest u8vect start end)
    (close-digest digest (digest-default-result-type))))

