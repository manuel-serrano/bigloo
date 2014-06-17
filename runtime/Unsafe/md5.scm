;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Unsafe/md5.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  6 14:35:24 2005                          */
;*    Last change :  Tue Jun 17 08:01:46 2014 (serrano)                */
;*    Copyright   :  2002-14 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    MD5 encryption                                                   */
;*    -------------------------------------------------------------    */
;*    Based on an implementation by Jens Axel Søgaard.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __md5

   (use    __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
	   __rgc
	   __bit
	   __bignum
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __r4_input_6_10_2
	   __r5_control_features_6_4
	   __mmap
	   __foreign
	   __error
	   __evenv
	   __os
	   __srfi4)

   (import __param
	   __base64
	   __hmac)

   (export (md5sum::bstring ::obj)
	   (md5sum-mmap::bstring ::mmap)
	   (md5sum-string::bstring ::bstring)
	   (md5sum-port::bstring ::input-port)
	   (md5sum-file::bstring ::bstring)

	   (hmac-md5sum-string::bstring ::bstring ::bstring)
	   (cram-md5sum-string::bstring ::bstring ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    md5sum ...                                                       */
;*---------------------------------------------------------------------*/
(define (md5sum obj)
   (cond
      ((mmap? obj)
       (md5sum-mmap obj))
      ((string? obj)
       (md5sum-string obj))
      ((input-port? obj)
       (md5sum-port obj))
      (else
       (error 'md5sum "Illegal argument" obj))))

;*---------------------------------------------------------------------*/
;*    md5sum-file ...                                                  */
;*---------------------------------------------------------------------*/
(define (md5sum-file fname)
   (let ((mm (open-mmap fname :write #f)))
      (unwind-protect
	 (md5sum-mmap mm)
	 (close-mmap mm))))

;*---------------------------------------------------------------------*/
;*    make-word ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (make-word hi lo)
   `(bit-or (bit-lsh ,hi 16) ,lo))

;*---------------------------------------------------------------------*/
;*    word-hi ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (word-hi w)
   `(bit-ursh ,w 16))

;*---------------------------------------------------------------------*/
;*    word-low ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (word-lo w)
   `(bit-and ,w (-fx (bit-lsh 1 16) 1)))

;*---------------------------------------------------------------------*/
;*    bytes->word ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (bytes->word a::long b::long c::long d::long)
   (make-word (+fx (bit-lsh a 8) b) (+fx (bit-lsh c 8) d)))

;*---------------------------------------------------------------------*/
;*    +w ...                                                           */
;*---------------------------------------------------------------------*/
(define-inline (+w::long a::long b::long)
   (+fx a b))

;*---------------------------------------------------------------------*/
;*    +w4 ...                                                          */
;*---------------------------------------------------------------------*/
(define (+w4::long a::long b::long c::long d::long)
   (+w (+w (+w a b) c) d))

;*---------------------------------------------------------------------*/
;*    word-or ...                                                      */
;*---------------------------------------------------------------------*/
(define (word-or::long a::long b::long)
   (bit-or a b))

;*---------------------------------------------------------------------*/
;*    word-not ...                                                     */
;*---------------------------------------------------------------------*/
(define (word-not::long a::long)
   (bit-not a))

;*---------------------------------------------------------------------*/
;*    word-xor ...                                                     */
;*---------------------------------------------------------------------*/
(define (word-xor::long a::long b::long)
   (bit-xor a b))

;*---------------------------------------------------------------------*/
;*    word-and ...                                                     */
;*---------------------------------------------------------------------*/
(define (word-and::long a::long b::long)
   (bit-and a b))

;*---------------------------------------------------------------------*/
;*    u32 ...                                                          */
;*---------------------------------------------------------------------*/
(define-macro (u32 . l)
;*    (list->s32vector (map (lambda (x) (fixnum->uint32 x)) l)))       */
   `(let ((v (make-s32vector ,(length l))))
       ,@(map (lambda (x i) `(s32vector-set! v ,i ,(fixnum->uint32 x)))
	  l (iota (length l)))
       v))

;*---------------------------------------------------------------------*/
;*    masks ...                                                        */
;*---------------------------------------------------------------------*/
(define masks
   '#(#x0 #x1 #x3 #x7 #xF #x1F #x3F #x7F #xFF
      #x1FF #x3FF #x7FF #xFFF #x1FFF #x3FFF #x7FFF #xFFFF))

;*---------------------------------------------------------------------*/
;*    rot ...                                                          */
;*---------------------------------------------------------------------*/
(define (rot::long hi::long lo::long s::long)
   (make-word
    (bit-or (bit-lsh (bit-and hi (vector-ref masks (-fx 16 s))) s)
	    (bit-and (bit-rsh lo (-fx 16 s)) (vector-ref masks s)))
    (bit-or (bit-lsh (bit-and lo (vector-ref masks (-fx 16 s))) s)
	    (bit-and (bit-rsh hi (-fx 16 s)) (vector-ref masks s)))))

;*---------------------------------------------------------------------*/
;*    bit-lshw ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (bit-lshw a s)
   `(let ((w::long ,a))
       ,(cond
	   ((and (<fx 0 s) (<fx s 16))
	    `(rot (word-hi w) (word-lo w) ,s))
	   ((<fx s 32)
	    `(rot (word-lo w) (word-hi w) ,(-fx s 16)))
	   (else
	    (error 'bit-lshw "shift out of range: " s)))))

;*---------------------------------------------------------------------*/
;*    string-word-at ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (string-word-at message i)
   (bytes->word
    (char->integer (string-ref message (+fx i 3)))
    (char->integer (string-ref message (+fx i 2)))
    (char->integer (string-ref message (+fx i 1)))
    (char->integer (string-ref message i))))

;*---------------------------------------------------------------------*/
;*    string-word-at ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (string-word-at-mmap message::mmap i::elong)
   (bytes->word
    (char->integer ($mmap-ref message (+elong i #e3)))
    (char->integer ($mmap-ref message (+elong i #e2)))
    (char->integer ($mmap-ref message (+elong i #e1)))
    (char->integer ($mmap-ref message i))))

;*---------------------------------------------------------------------*/
;*    byte-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define (byte-set! str::bstring offset::long int::long)
   (string-set! str offset (integer->char int)))

;*---------------------------------------------------------------------*/
;*    md5sum-mmap ...                                                  */
;*---------------------------------------------------------------------*/
(define (md5sum-mmap mm)
   (multiple-value-bind (len padding)
      (step1-2-mmap mm)
      (step3-4-5-mmap mm len padding)))

;*---------------------------------------------------------------------*/
;*    md5sum-string ...                                                */
;*---------------------------------------------------------------------*/
(define (md5sum-string str)
   (multiple-value-bind (len padding)
      (step1-2-string str (fixnum->elong (string-length str)))
      (step3-4-5-string str len padding)))

;*---------------------------------------------------------------------*/
;*    md5sum-port ...                                                  */
;*---------------------------------------------------------------------*/
(define (md5sum-port port)
   (step3-4-1-2-5-port port))

;*---------------------------------------------------------------------*/
;*    step1-2-string ...                                               */
;*    -------------------------------------------------------------    */
;*    ALEN is the actual length, the one used for the padding.         */
;*    It is not necessarily the string length that might be shorted    */
;*    (e.g., when invoked from MD5SUM-PORT).                           */
;*---------------------------------------------------------------------*/
(define (step1-2-string str alen::elong)
   (let* ((len (string-length str))
	  (mod (modulofx len 64))
	  (imod (/fx len #e64))
	  (plen (*fx imod #e64)))
      (cond
	 ((>=fx mod 56)
	  (let ((padding (make-string 128 #a000))
		(str (substring str plen len)))
	     ;; fill with the initial sequence of bits
	     (blit-string! str 0 padding 0 (string-length str))
	     (string-set! padding (string-length str) #a128)
	     (step1-padding-length! padding 128 alen)
	     (values plen padding)))
	 ((=fx mod 0)
	  (let ((padding (make-string 64 #a000)))
	     (string-set! padding 0 #a128)
	     (step1-padding-length! padding 64 alen)
	     (values len padding)))
	 (else
	  (let* ((imod (/fx len #e64))
		 (plen (*fx imod #e64))
		 (clen (-fx len plen))
		 (str (substring str plen len))
		 (padding (make-string 64 #a000)))
	     ;; fill with the initial sequence of bits
	     (blit-string! str 0 padding 0 (string-length str))
	     ;; add the padding
	     (string-set! padding (-fx len plen) #a128)
	     ;; add the length padding
	     (step1-padding-length! padding 64 alen)
	     (values plen padding))))))

;*---------------------------------------------------------------------*/
;*    step1-2-mmap ...                                                 */
;*---------------------------------------------------------------------*/
(define (step1-2-mmap mm)
   
   (define (byte-set! str offset int)
      (string-set! str offset (integer->char int)))
   
   (let* ((len (mmap-length mm))
	  (mod (modulofx len 64))
	  (imod (/elong len #e64))
	  (plen (*elong imod #e64)))
      (cond
	 ((>=fx mod 56)
	  (let ((padding (make-string 128 #a000))
		(str (mmap-substring mm plen len)))
	     ;; fill with the initial sequence of bits
	     (blit-string! str 0 padding 0 (string-length str))
	     (string-set! padding (string-length str) #a128)
	     (step1-padding-length! padding 128 len)
	     (values plen padding)))
	 ((=fx mod 0)
	  (let ((padding (make-string 64 #a000)))
	     (string-set! padding 0 #a128)
	     (step1-padding-length! padding 64 len)
	     (values len padding)))
	 (else
	  (let* ((imod (/elong len #e64))
		 (plen (*elong imod #e64))
		 (clen (-elong len plen))
		 (str (mmap-substring mm plen len))
		 (padding (make-string 64 #a000)))
	     ;; fill with the initial sequence of bits
	     (blit-string! str 0 padding 0 (string-length str))
	     ;; add the padding
	     (string-set! padding (-elong len plen) #a128)
	     ;; add the length padding
	     (step1-padding-length! padding 64 len)
	     (values plen padding))))))

;*---------------------------------------------------------------------*/
;*    step1-padding-length! ...                                        */
;*---------------------------------------------------------------------*/
(define (step1-padding-length! padding o len::elong)
   (byte-set! padding (-fx o 4)
	      (bit-and (elong->fixnum (bit-rshelong len 29)) 255))
   (byte-set! padding (-fx o 5)
	      (bit-and (elong->fixnum (bit-rshelong len 21)) 255))
   (byte-set! padding (-fx o 6)
	      (bit-and (elong->fixnum (bit-rshelong len 13)) 255))
   (byte-set! padding (-fx o 7)
	      (bit-and (elong->fixnum (bit-rshelong len 5)) 255))
   (byte-set! padding (-fx o 8)
	      (bit-and (elong->fixnum (bit-lshelong len 3)) 255)))

;*---------------------------------------------------------------------*/
;*    make-R ...                                                       */
;*---------------------------------------------------------------------*/
(define (make-R)
   (let ((R (make-s32vector 4)))
      (s32vector-set! R 0 (bytes->word #x67 #x45 #x23 #x01))
      (s32vector-set! R 1 (bytes->word #xef #xcd #xab #x89))
      (s32vector-set! R 2 (bytes->word #x98 #xba #xdc #xfe))
      (s32vector-set! R 3 (bytes->word #x10 #x32 #x54 #x76))
      R))

;*---------------------------------------------------------------------*/
;*    %step3-common ...                                                */
;*    -------------------------------------------------------------    */
;*    For performance concerns, this *must* be implement as a macro.   */
;*---------------------------------------------------------------------*/
(define-macro (%step3-common)
   '(let* ((A (int32->fixnum (s32vector-ref R 0)))
	   (B (int32->fixnum (s32vector-ref R 1)))
	   (C (int32->fixnum (s32vector-ref R 2)))
	   (D (int32->fixnum (s32vector-ref R 3)))
	   
	   (A (+w B (bit-lshw (+w4 A (F B C D) s0 (make-word 55146 42104)) 7)))
	   (D (+w A (bit-lshw (+w4 D (F A B C) s1 (make-word 59591 46934)) 12)))
	   (C (+w D (bit-lshw (+w4 C (F D A B) s2 (make-word 9248 28891)) 17)))
	   (B (+w C (bit-lshw (+w4 B (F C D A) s3 (make-word 49597 52974)) 22)))
	   (A (+w B (bit-lshw (+w4 A (F B C D) s4 (make-word 62844 4015)) 7)))
	   (D (+w A (bit-lshw (+w4 D (F A B C) s5 (make-word 18311 50730)) 12)))
	   (C (+w D (bit-lshw (+w4 C (F D A B) s6 (make-word 43056 17939)) 17)))
	   (B (+w C (bit-lshw (+w4 B (F C D A) s7 (make-word 64838 38145)) 22)))
	   (A (+w B (bit-lshw (+w4 A (F B C D) s8 (make-word 27008 39128)) 7)))
	   (D (+w A (bit-lshw (+w4 D (F A B C) s9 (make-word 35652 63407)) 12)))
	   (C (+w D (bit-lshw (+w4 C (F D A B) s10 (make-word 65535 23473)) 17)))
	   (B (+w C (bit-lshw (+w4 B (F C D A) s11 (make-word 35164 55230)) 22)))
	   (A (+w B (bit-lshw (+w4 A (F B C D) s12 (make-word 27536 4386)) 7)))
	   (D (+w A (bit-lshw (+w4 D (F A B C) s13 (make-word 64920 29075)) 12)))
	   (C (+w D (bit-lshw (+w4 C (F D A B) s14 (make-word 42617 17294)) 17)))
	   (B (+w C (bit-lshw (+w4 B (F C D A) s15 (make-word 18868 2081)) 22)))
	   
	   (A (+w B (bit-lshw (+w4 A (G B C D) s1 (make-word 63006 9570)) 5)))
	   (D (+w A (bit-lshw (+w4 D (G A B C) s6 (make-word 49216 45888)) 9)))
	   (C (+w D (bit-lshw (+w4 C (G D A B) s11 (make-word 9822 23121)) 14)))
	   (B (+w C (bit-lshw (+w4 B (G C D A) s0 (make-word 59830 51114)) 20)))
	   (A (+w B (bit-lshw (+w4 A (G B C D) s5 (make-word 54831 4189)) 5)))
	   (D (+w A (bit-lshw (+w4 D (G A B C) s10 (make-word 580 5203)) 9)))
	   (C (+w D (bit-lshw (+w4 C (G D A B) s15 (make-word 55457 59009)) 14)))
	   (B (+w C (bit-lshw (+w4 B (G C D A) s4 (make-word 59347 64456)) 20)))
	   (A (+w B (bit-lshw (+w4 A (G B C D) s9 (make-word 8673 52710)) 5)))
	   (D (+w A (bit-lshw (+w4 D (G A B C) s14 (make-word 49975 2006)) 9)))
	   (C (+w D (bit-lshw (+w4 C (G D A B) s3 (make-word 62677 3463)) 14)))
	   (B (+w C (bit-lshw (+w4 B (G C D A) s8 (make-word 17754 5357)) 20)))
	   (A (+w B (bit-lshw (+w4 A (G B C D) s13 (make-word 43491 59653)) 5)))
	   (D (+w A (bit-lshw (+w4 D (G A B C) s2 (make-word 64751 41976)) 9)))
	   (C (+w D (bit-lshw (+w4 C (G D A B) s7 (make-word 26479 729)) 14)))
	   (B (+w C (bit-lshw (+w4 B (G C D A) s12 (make-word 36138 19594)) 20)))
	   
	   (A (+w B (bit-lshw (+w4 A (H B C D) s5 (make-word 65530 14658)) 4)))
	   (D (+w A (bit-lshw (+w4 D (H A B C) s8 (make-word 34673 63105)) 11)))
	   (C (+w D (bit-lshw (+w4 C (H D A B) s11 (make-word 28061 24866)) 16)))
	   (B (+w C (bit-lshw (+w4 B (H C D A) s14 (make-word 64997 14348)) 23)))
	   (A (+w B (bit-lshw (+w4 A (H B C D) s1 (make-word 42174 59972)) 4)))
	   (D (+w A (bit-lshw (+w4 D (H A B C) s4 (make-word 19422 53161)) 11)))
	   (C (+w D (bit-lshw (+w4 C (H D A B) s7 (make-word 63163 19296)) 16)))
	   (B (+w C (bit-lshw (+w4 B (H C D A) s10 (make-word 48831 48240)) 23)))
	   (A (+w B (bit-lshw (+w4 A (H B C D) s13 (make-word 10395 32454)) 4)))
	   (D (+w A (bit-lshw (+w4 D (H A B C) s0 (make-word 60065 10234)) 11)))
	   (C (+w D (bit-lshw (+w4 C (H D A B) s3 (make-word 54511 12421)) 16)))
	   (B (+w C (bit-lshw (+w4 B (H C D A) s6 (make-word 1160 7429)) 23)))
	   (A (+w B (bit-lshw (+w4 A (H B C D) s9 (make-word 55764 53305)) 4)))
	   (D (+w A (bit-lshw (+w4 D (H A B C) s12 (make-word 59099 39397)) 11)))
	   (C (+w D (bit-lshw (+w4 C (H D A B) s15 (make-word 8098 31992)) 16)))
	   (B (+w C (bit-lshw (+w4 B (H C D A) s2 (make-word 50348 22117)) 23)))
	   
	   (A (+w B (bit-lshw (+w4 A (II B C D) s0 (make-word 62505 8772)) 6)))
	   (D (+w A (bit-lshw (+w4 D (II A B C) s7 (make-word 17194 65431)) 10)))
	   (C (+w D (bit-lshw (+w4 C (II D A B) s14 (make-word 43924 9127)) 15)))
	   (B (+w C (bit-lshw (+w4 B (II C D A) s5 (make-word 64659 41017)) 21)))
	   (A (+w B (bit-lshw (+w4 A (II B C D) s12 (make-word 25947 22979)) 6)))
	   (D (+w A (bit-lshw (+w4 D (II A B C) s3 (make-word 36620 52370)) 10)))
	   (C (+w D (bit-lshw (+w4 C (II D A B) s10 (make-word 65519 62589)) 15)))
	   (B (+w C (bit-lshw (+w4 B (II C D A) s1 (make-word 34180 24017)) 21)))
	   (A (+w B (bit-lshw (+w4 A (II B C D) s8 (make-word 28584 32335)) 6)))
	   (D (+w A (bit-lshw (+w4 D (II A B C) s15 (make-word 65068 59104)) 10)))
	   (C (+w D (bit-lshw (+w4 C (II D A B) s6 (make-word 41729 17172)) 15)))
	   (B (+w C (bit-lshw (+w4 B (II C D A) s13 (make-word 19976 4513)) 21)))
	   (A (+w B (bit-lshw (+w4 A (II B C D) s4 (make-word 63315 32386)) 6)))
	   (D (+w A (bit-lshw (+w4 D (II A B C) s11 (make-word 48442 62005)) 10)))
	   (C (+w D (bit-lshw (+w4 C (II D A B) s2 (make-word 10967 53947)) 15)))
	   (B (+w C (bit-lshw (+w4 B (II C D A) s9 (make-word 60294 54161)) 21))))
       
       (s32vector-set! R 0
	  (fixnum->int32 (+w A (int32->fixnum (s32vector-ref R 0)))))
       (s32vector-set! R 1
	  (fixnum->int32 (+w B (int32->fixnum (s32vector-ref R 1)))))
       (s32vector-set! R 2
	  (fixnum->int32 (+w C (int32->fixnum (s32vector-ref R 2)))))
       (s32vector-set! R 3
	  (fixnum->int32 (+w D (int32->fixnum (s32vector-ref R 3)))))))

;*---------------------------------------------------------------------*/
;*    step3-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (step3-string R message i)
   
   (define (F x y z)
      (word-or (word-and x y) (word-and (word-not x) z)))
   
   (define (G x y z)
      (word-or (word-and x z) (word-and y (word-not z))))
   
   (define (H x y z)
      (word-xor x (word-xor y z)))
   
   (define (II x y z)
      (word-xor y (word-or x (word-not z))))

   (let ((s0 (string-word-at message (+fx i 0)))
	 (s1 (string-word-at message (+fx i 4)))
	 (s2 (string-word-at message (+fx i 8)))
	 (s3 (string-word-at message (+fx i 12)))
	 (s4 (string-word-at message (+fx i 16)))
	 (s5 (string-word-at message (+fx i 20)))
	 (s6 (string-word-at message (+fx i 24)))
	 (s7 (string-word-at message (+fx i 28)))
	 (s8 (string-word-at message (+fx i 32)))
	 (s9 (string-word-at message (+fx i 36)))
	 (s10 (string-word-at message (+fx i 40)))
	 (s11 (string-word-at message (+fx i 44)))
	 (s12 (string-word-at message (+fx i 48)))
	 (s13 (string-word-at message (+fx i 52)))
	 (s14 (string-word-at message (+fx i 56)))
	 (s15 (string-word-at message (+fx i 60))))
      
      (%step3-common)))

;*---------------------------------------------------------------------*/
;*    step3-mmap ...                                                   */
;*---------------------------------------------------------------------*/
(define (step3-mmap R message i)
   
   (define (F x y z)
      (word-or (word-and x y) (word-and (word-not x) z)))
   
   (define (G x y z)
      (word-or (word-and x z) (word-and y (word-not z))))
   
   (define (H x y z)
      (word-xor x (word-xor y z)))
   
   (define (II x y z)
      (word-xor y (word-or x (word-not z))))
   
   (let ((s0 (string-word-at-mmap message (+elong i #e0)))
	  (s1 (string-word-at-mmap message (+elong i #e4)))
	  (s2 (string-word-at-mmap message (+elong i #e8)))
	  (s3 (string-word-at-mmap message (+elong i #e12)))
	  (s4 (string-word-at-mmap message (+elong i #e16)))
	  (s5 (string-word-at-mmap message (+elong i #e20)))
	  (s6 (string-word-at-mmap message (+elong i #e24)))
	  (s7 (string-word-at-mmap message (+elong i #e28)))
	  (s8 (string-word-at-mmap message (+elong i #e32)))
	  (s9 (string-word-at-mmap message (+elong i #e36)))
	  (s10 (string-word-at-mmap message (+elong i #e40)))
	  (s11 (string-word-at-mmap message (+elong i #e44)))
	  (s12 (string-word-at-mmap message (+elong i #e48)))
	  (s13 (string-word-at-mmap message (+elong i #e52)))
	  (s14 (string-word-at-mmap message (+elong i #e56)))
	  (s15 (string-word-at-mmap message (+elong i #e60))))
      
      (%step3-common)))

;*---------------------------------------------------------------------*/
;*    step3-4-5-string ...                                             */
;*---------------------------------------------------------------------*/
(define (step3-4-5-string message l::long padding)
   (let ((R (make-R)))
      (let loop ((i 0))
	 (if (=fx i l)
	     (step4-5 R padding)
	     (begin
		(step3-string R message i)
		(loop (+fx i 64)))))))

;*---------------------------------------------------------------------*/
;*    step3-4-5-mmap ...                                               */
;*---------------------------------------------------------------------*/
(define (step3-4-5-mmap message l::elong padding)
   (let ((R (make-R)))
      (let loop ((i::elong #e0))
	 (if (=elong i l)
	     (step4-5 R padding)
	     (begin
		(step3-mmap R message i)
		(loop (+elong i #e64)))))))

;*---------------------------------------------------------------------*/
;*    step3-4-1-2-5-port ...                                           */
;*---------------------------------------------------------------------*/
(define (step3-4-1-2-5-port p)
   (let ((R (make-R))
	 (buf (make-string 64)))
      (let loop ((i 0))
	 (let ((len (read-chars! buf 64 p)))
	    (if (=fx len 64)
		(begin
		   (step3-string R buf 0)
		   (loop (+fx i 64)))
		(multiple-value-bind (_ padding)
		   (step1-2-string (string-shrink! buf len)
				   (fixnum->elong (+fx i len)))
		   (step4-5 R padding)))))))

;*---------------------------------------------------------------------*/
;*    step4-5 ...                                                      */
;*---------------------------------------------------------------------*/
(define (step4-5 R padding)
   (step3-string R padding 0)
   (when (>fx (string-length padding) 64)
      (step3-string R padding 64))
   (step5 (s32vector-ref R 0)
	  (s32vector-ref R 1)
	  (s32vector-ref R 2)
	  (s32vector-ref R 3)))

;*---------------------------------------------------------------------*/
;*    step5 ...                                                        */
;*---------------------------------------------------------------------*/
(define (step5 a b c d)
   
   (define (string-hex-at! r i h)
      (let ((s "0123456789abcdef"))
	 (if (>=fx h 16)
	     (begin
		(string-set! r (+fx i 1) (string-ref s (bit-and h #xf)))
		(string-set! r i (string-ref s (bit-and (bit-rsh h 4) #xf))))
	     (string-set! r (+fx i 1) (string-ref s h)))))
   
   (define (string-word-at! r i w::int32)
      (let ((w::long (int32->fixnum w)))
	 (string-hex-at! r i (bit-and (word-lo w) #xff))
	 (string-hex-at! r (+fx i 2) (bit-and (bit-rsh (word-lo w) 8) #xff))
	 (string-hex-at! r (+fx i 4) (bit-and (word-hi w) #xff))
	 (string-hex-at! r (+fx i 6) (bit-and (bit-rsh (word-hi w) 8) #xff))))
   
   (let ((s (make-string 32 #\0)))
      (string-word-at! s 0 a)
      (string-word-at! s 8 b)
      (string-word-at! s 16 c)
      (string-word-at! s 24 d)
      s))

;*---------------------------------------------------------------------*/
;*    hmac-md5sum-string ...                                           */
;*    -------------------------------------------------------------    */
;*    See RFC2202 (http://tools.ietf.org/html/rfc2202) for             */
;*    hmac-md5 test case.                                              */
;*---------------------------------------------------------------------*/
(define (hmac-md5sum-string key message)
   (hmac-string key message md5sum-string))

;*---------------------------------------------------------------------*/
;*    cram-md5sum-string ...                                           */
;*    -------------------------------------------------------------    */
;*    Challenge-Response Authentication Mechanism as specified in      */
;*    RFC 2195 (http://tools.ietf.org/html/rfc2195).                   */
;*---------------------------------------------------------------------*/
(define (cram-md5sum-string user key data)
   ;; CRAM-MD5 digest suppose that you got base64-encoded data
   ;; and the result is base64-encoded
   (base64-encode
    (string-append user " " (hmac-md5sum-string key (base64-decode data)))))

