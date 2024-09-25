;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Unsafe/bignumber.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Marc Feeley                                       */
;*    Creation    :  Tue Mar 11 11:32:17 2008                          */
;*    Last change :  Wed Sep 25 11:23:29 2024 (serrano)                */
;*    Copyright   :  2006-24 Marc Feeley                               */
;*    -------------------------------------------------------------    */
;*    Bigloo two implementations                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bignum
   
   (import  __error
	    __object
	    __thread
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __dsssl
	    __bexit
	    __srfi4
	    __bit
	    __evenv
	    
	    __r5_control_features_6_4
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_control_features_6_9)
   
   (cond-expand
      (enable-gmp
       (include "./Unsafe/bignumber-gmp.sch"))
      (else
       (include "./Unsafe/bignumber-generic.sch")))
   
   (extern
      (macro $bignum?::bool (::obj) "BIGNUMP")
      
      (macro $bignum->fixnum-safe::obj (::obj) "BGL_SAFE_BX_TO_FX")
      (macro +fx-safe::obj (::long ::long) "BGL_SAFE_PLUS_FX")
      (macro -fx-safe::obj (::long ::long) "BGL_SAFE_MINUS_FX")
      (macro *fx-safe::obj (::long ::long) "BGL_SAFE_MUL_FX")
      
      (macro +elong-safe::obj (::elong ::elong) "BGL_SAFE_PLUS_ELONG")
      (macro -elong-safe::obj (::elong ::elong) "BGL_SAFE_MINUS_ELONG")
      (macro *elong-safe::obj (::elong ::elong) "BGL_SAFE_MUL_ELONG")
      
      (macro +llong-safe::obj (::llong ::llong) "BGL_SAFE_PLUS_LLONG")
      (macro -llong-safe::obj (::llong ::llong) "BGL_SAFE_MINUS_LLONG")
      (macro *llong-safe::obj (::llong ::llong) "BGL_SAFE_MUL_LLONG")
      
      (macro $quotientfx-safe::obj (::long ::long) "BGL_SAFE_QUOTIENT_FX")
      (macro $quotientelong-safe::obj (::elong ::elong) "BGL_SAFE_QUOTIENT_ELONG")
      (macro $quotientllong-safe::obj (::llong ::llong) "BGL_SAFE_QUOTIENT_LLONG"))
   
   (java
      (class foreign
	 (method static $bignum?::bool (::obj)
	    "BIGNUMP")
	 (method static $fixnum->bignum::bignum (::long)
	    "LONG_TO_BIGNUM")
	 (method static $elong->bignum::bignum (::elong)
	    "ELONG_TO_BIGNUM")
	 (method static $llong->bignum::bignum (::llong)
	    "LLONG_TO_BIGNUM")
	 (method static $uint64->bignum::bignum (::uint64)
	    "LLONG_TO_BIGNUM")
	 (method static $int64->bignum::bignum (::int64)
	    "LLONG_TO_BIGNUM")
	 (method static $bignum->int64::llong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum->uint64::llong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum->fixnum::long (::bignum)
	    "bgl_bignum_to_long")
	 (method static $bignum->elong::elong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum->llong::llong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum-cmp::int (::bignum ::bignum)
	    "CMP_BIGNUM")
	 (method static $zerobx?::bool (::bignum)
	    "ZEROP_BIGNUM")
	 (method static $evenbx?::bool (::bignum)
	    "EVENP_BIGNUM")
	 (method static $oddbx?::bool (::bignum)
	    "ODDP_BIGNUM")
	 (method static $positivebx?::bool (::bignum)
	    "POSITIVEP_BIGNUM")
	 (method static $negativebx?::bool (::bignum)
	    "NEGATIVEP_BIGNUM")
	 (method static $absbx::bignum (::bignum)
	    "ABS_BIGNUM")
	 (method static $negbx::bignum (::bignum)
	    "NEG_BIGNUM")
	 (method static $+bx::bignum (::bignum ::bignum)
	    "PLUS_BIGNUM")
	 (method static $-bx::bignum (::bignum ::bignum)
	    "MINUS_BIGNUM")
	 (method static $*bx::bignum (::bignum ::bignum)
	    "MUL_BIGNUM")
	 (method static $divrembx::bignum (::bignum ::bignum)
	    "DIVREM_BIGNUM")
	 (method static $quotientbx::bignum (::bignum ::bignum)
	    "QUOTIENT_BIGNUM")
	 (method static $bignum->fixnum-safe::obj (::obj)
	    "BGL_SAFE_BX_TO_FX")
	 (method static $quotientfx-safe::obj (::long ::long)
	    "SAFE_DIV_FX")
	 (method static $quotientelong-safe::obj (::elong ::elong)
	    "SAFE_DIV_ELONG")
	 (method static $quotientllong-safe::obj (::llong ::llong)
	    "SAFE_DIV_LLONG")
	 (method static $remainderbx::bignum (::bignum ::bignum)
	    "REMAINDER_BIGNUM")
	 (method static $gcdbx::bignum (::bignum ::bignum)
	    "GCD_BIGNUM")
	 (method static $lcmbx::bignum (::bignum ::bignum)
	    "LCM_BIGNUM")
	 (method static $bignum->string::string (::bignum ::long)
	    "bgl_bignum_to_string")
	 (method static $string->bignum::bignum (::string ::int)
	    "bgl_string_to_bignum")
	 (method static $string->integer-obj::obj (::string ::int)
	    "bgl_string_to_integer_obj")
	 (method static $randbx::bignum (::bignum)
	    "bgl_rand_bignum")
	 (method static +fx-safe::obj (::long ::long)
	    "SAFE_PLUS_FX")
	 (method static -fx-safe::obj (::long ::long)
	    "SAFE_MINUS_FX")
	 (method static *fx-safe::obj (::long ::long)
	    "SAFE_MUL_FX")
	 (method static $bitlshbx::bignum (::bignum ::long)
	    "BITLSH_BIGNUM")
	 (method static $bitrshbx::bignum (::bignum ::long)
	    "BITRSH_BIGNUM")
	 (method static $bitorbx::bignum (::bignum ::bignum)
	    "BITOR_BIGNUM")
	 (method static $bitxorbx::bignum (::bignum ::bignum)
	    "BITXOR_BIGNUM")
	 (method static $bitandbx::bignum (::bignum ::bignum)
	    "BITAND_BIGNUM")
	 (method static $bitmaskbx::bignum (::bignum ::long)
	    "BITMASK_BIGNUM")
	 (method static $bitnotbx::bignum (::bignum)
	    "BITNOT_BIGNUM")
	 (method static +elong-safe::obj (::elong ::elong)
	    "SAFE_PLUS_ELONG")
	 (method static -elong-safe::obj (::elong ::elong)
	    "SAFE_MINUS_ELONG")
	 (method static *elong-safe::obj (::elong ::elong)
	    "SAFE_MUL_ELONG")
	 
	 (method static +llong-safe::obj (::llong ::llong)
	    "SAFE_PLUS_LLONG")
	 (method static -llong-safe::obj (::llong ::llong)
	    "SAFE_MINUS_LLONG")
	 (method static *llong-safe::obj (::llong ::llong)
	    "SAFE_MUL_LLONG")
	 
	 (method static $bignum->flonum::double (::bignum)
	    "BIGNUM_TO_FLONUM")
	 (method static $flonum->bignum::bignum (::double)
	    "FLONUM_TO_BIGNUM")))
   
   (export
      (inline bignum?::bool ::obj)
      (inline fixnum->bignum::bignum ::long)
      (inline bignum->fixnum::long ::bignum)
      (inline bignum->elong::elong ::bignum)
      (inline bignum->llong::llong ::bignum)
      (inline elong->bignum::bignum ::elong)
      (inline llong->bignum::bignum ::llong)
      (inline =bx::bool ::bignum ::bignum)
      (inline >bx::bool ::bignum ::bignum)
      (inline >=bx::bool ::bignum ::bignum)
      (inline <bx::bool ::bignum ::bignum)
      (inline <=bx::bool ::bignum ::bignum)
      (inline zerobx?::bool ::bignum)
      (inline positivebx?::bool ::bignum)
      (inline negativebx?::bool ::bignum)
      (inline oddbx?::bool ::bignum)
      (inline evenbx?::bool ::bignum)
      (minbx::bignum ::bignum . pair)
      (maxbx::bignum ::bignum . pair)
      (inline +bx::bignum ::bignum ::bignum)
      (inline -bx::bignum ::bignum ::bignum)
      (inline *bx::bignum ::bignum ::bignum)
      (inline /bx::bignum ::bignum ::bignum)
      (inline negbx::bignum ::bignum)
      (inline absbx::bignum ::bignum)
      (inline remainderbx::bignum ::bignum ::bignum)
      (inline quotientbx::bignum ::bignum ::bignum)
      (modulobx::bignum ::bignum ::bignum)
      (gcdbx::bignum . pair)
      (lcmbx::bignum . pair)
      (inline exptbx::bignum ::bignum ::bignum)
      (bignum->string::bstring ::bignum #!optional (radix::long 10))
      (string->bignum::bignum ::bstring #!optional (radix::long 10))
      (bignum->octet-string::bstring ::bignum)
      (octet-string->bignum::bignum ::bstring)
      (inline randombx::bignum ::bignum))
   
   (pragma
      ($bignum? side-effect-free (predicate-of bignum) no-cfa-top nesting fail-safe)
      ($bignum->fixnum-safe fail-safe)
      (+fx-safe fail-safe side-effect-free)
      (-fx-safe fail-safe side-effect-free)
      (*fx-safe fail-safe side-effect-free)
      (+elong-safe fail-safe side-effect-free)
      (-elong-safe fail-safe side-effect-free)
      (*elong-safe fail-safe side-effect-free)
      (+llong-safe fail-safe side-effect-free)
      (-llong-safe fail-safe side-effect-free)
      (*llong-safe fail-safe side-effect-free)
      (bignum? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (fixnum->bignum side-effect-free no-cfa-top nesting (effect))
      (bignum->fixnum no-alloc side-effect-free no-cfa-top nesting (effect))
      (bignum->elong no-alloc side-effect-free no-cfa-top nesting (effect))
      (bignum->llong no-alloc side-effect-free no-cfa-top nesting (effect))
      (elong->bignum side-effect-free no-cfa-top nesting (effect))
      (llong->bignum side-effect-free no-cfa-top nesting (effect))
      (bignum->string side-effect-free no-cfa-top nesting (effect))
      (bignum->octet-string side-effect-free no-cfa-top nesting (effect))
      (string->bignum side-effect-free no-cfa-top nesting (effect))
      (octet-string->bignum side-effect-free no-cfa-top nesting (effect))
      (=bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (>bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (>=bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (<bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (<=bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (oddbx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (evenbx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (+bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (-bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (*bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (/bx no-alloc side-effect-free no-cfa-top nesting (effect))
      (remainderbx no-alloc side-effect-free no-cfa-top nesting (effect))
      (modulobx no-alloc side-effect-free no-cfa-top nesting (effect))
      (quotientbx no-alloc side-effect-free no-cfa-top nesting (effect))
      (gcdbx side-effect-free no-cfa-top nesting (effect))
      (lcmbx no-alloc side-effect-free no-cfa-top nesting (effect))
      (exptbx side-effect-free no-cfa-top nesting (effect))
      (positivebx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (negativebx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (zerobx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
      (negbx side-effect-free no-cfa-top nesting (effect) fail-safe)
      (absbx side-effect-free no-cfa-top nesting (effect) fail-safe)
      (randombx side-effect-free no-cfa-top nesting (effect))))

;*---------------------------------------------------------------------*/
;*    preallocated constants                                           */
;*---------------------------------------------------------------------*/
(define Z0 #z0)
(define Z1 #z1)
(define Z2 #z2)
(define Z256 #z256)

;*---------------------------------------------------------------------*/
;*    bignum? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bignum? obj) ($bignum? obj))

;*---------------------------------------------------------------------*/
;*    casts ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (fixnum->bignum x) ($fixnum->bignum x))
(define-inline (bignum->fixnum x) ($bignum->fixnum x))

(define-inline (bignum->elong x) ($bignum->elong x))
(define-inline (bignum->llong x) ($bignum->llong x))

(define-inline (elong->bignum x) ($elong->bignum x))
(define-inline (llong->bignum x) ($llong->bignum x))


;*---------------------------------------------------------------------*/
;*    binary comparisons                                               */
;*---------------------------------------------------------------------*/
(define-inline (=bx n1 n2) (=fx ($bignum-cmp n1 n2) 0))
(define-inline (<bx n1 n2) (<fx ($bignum-cmp n1 n2) 0))
(define-inline (>bx n1 n2) (>fx ($bignum-cmp n1 n2) 0))
(define-inline (<=bx n1 n2) (<=fx ($bignum-cmp n1 n2) 0))
(define-inline (>=bx n1 n2) (>=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    zero comparisons                                                 */
;*---------------------------------------------------------------------*/
(define-inline (zerobx? n) ($zerobx? n))
(define-inline (positivebx? n) ($positivebx? n))
(define-inline (negativebx? n) ($negativebx? n))

;*---------------------------------------------------------------------*/
;*    odd/even                                                         */
;*---------------------------------------------------------------------*/
(define-inline (oddbx? x) ($oddbx? x))
(define-inline (evenbx? x) ($evenbx? x))

;*---------------------------------------------------------------------*/
;*    min/max ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (min/max test-proc x . xs)
  `(let loop ((y ,x)
	      (xs ,xs))
     (if (null? xs)
	 y
	 (if (,test-proc (car xs) y)
	     (loop (car xs) (cdr xs))
	     (loop y (cdr xs))))))

(define (minbx n1 . nn) (min/max <bx n1 . nn))
(define (maxbx n1 . nn) (min/max >bx n1 . nn))

;*---------------------------------------------------------------------*/
;*    unary ops                                                        */
;*---------------------------------------------------------------------*/
(define-inline (negbx n1) ($negbx n1))
(define-inline (absbx n) ($absbx n))

;*---------------------------------------------------------------------*/
;*    binary ops                                                       */
;*---------------------------------------------------------------------*/
(define-inline (+bx z1 z2) ($+bx z1 z2))
(define-inline (-bx z1 z2) ($-bx z1 z2))
(define-inline (*bx z1 z2) ($*bx z1 z2))
(define-inline (/bx z1 z2) ($quotientbx z1 z2))

(define-inline (quotientbx n1 n2) ($quotientbx n1 n2))
(define-inline (remainderbx n1 n2) ($remainderbx n1 n2))

;*---------------------------------------------------------------------*/
;*    modulobx ...                                                     */
;*---------------------------------------------------------------------*/
(define (modulobx x y)
   (let ((r (remainderbx x y)))
      (if (zerobx? r)
	  r
	  (if (positivebx? y)
	      (if (positivebx? r) r (+bx y r))
	      (if (negativebx? r) r (+bx y r))))))

;*---------------------------------------------------------------------*/
;*    gcdbx ...                                                        */
;*---------------------------------------------------------------------*/
(define (gcdbx . x)
  (cond ((null? x) (fixnum->bignum 0))
	((null? (cdr x)) (absbx (car x)))
	(else
	 (let loop ((tmp ($gcdbx (absbx (car x)) (absbx (cadr x))))
		    (left (cddr x)))
	   (if (pair? left)
	       (loop ($gcdbx tmp (absbx (car left))) (cdr left))
	       tmp)))))

;*---------------------------------------------------------------------*/
;*    lcm ...                                                          */
;*---------------------------------------------------------------------*/
(define (lcmbx . x)
  (cond ((null? x) (fixnum->bignum 1))
	((null? (cdr x)) (absbx (car x)))
	(else
	 (let loop ((tmp ($lcmbx (car x) (cadr x)))
		    (left (cddr x)))
	   (if (pair? left)
	       (loop ($lcmbx tmp (car left)) (cdr left))
	       tmp)))))

;*---------------------------------------------------------------------*/
;*    exptbx ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (exptbx x y)
   (cond-expand
      (bigloo-c
       ($exptbx x y))
      (else
       (cond
	  ((zerobx? y) #z1)
	  ((evenbx? y) (exptbx (*bx x x) (quotientbx y #z2)))
	  (else (*bx x (exptbx x (-bx y #z1))))))))

;*---------------------------------------------------------------------*/
;*    randombx ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (randombx max::bignum)
   (if (=fx (bignum->fixnum max) 0) #z0 ($randbx max)))

;*---------------------------------------------------------------------*/
;*    bignum->string ...                                               */
;*---------------------------------------------------------------------*/
(define (bignum->string x #!optional (radix::long 10))
   (cond
      ((and (>=fx radix 2) (<=fx radix 36))
       ($bignum->string x radix))
      (else
       (error "bignum->string" "Illegal radix" radix))))

;*---------------------------------------------------------------------*/
;*    bignum->octet-string ...                                         */
;*---------------------------------------------------------------------*/
(define (bignum->octet-string x)
   (define (/ceilingfx x y)
      (let ((q (quotientfx x y))
	    (r (remainderfx x y)))
	 (cond
	    ((zerofx? r) q)
	    ((>fx r 0)   (+fx q 1))
	    (else        (-fx q 1)))))

   (define (bignum-bit-length::long b::bignum)
      (let loop ((b b)
		 (res 0))
	 (let ((divided (/bx b Z256)))
	    (cond
	       ((zerobx? b) res)
	       ((zerobx? divided) ;; this is the last octet
		(let ((x (bignum->fixnum b)))
		   (cond
		      ((<fx x #x02) (+fx res 1))
		      ((<fx x #x04) (+fx res 2))
		      ((<fx x #x08) (+fx res 3))
		      ((<fx x #x10) (+fx res 4))
		      ((<fx x #x20) (+fx res 5))
		      ((<fx x #x40) (+fx res 6))
		      ((<fx x #x80) (+fx res 7))
		      (else (+fx res 8)))))
	       (else
		(loop divided (+fx res 8)))))))

   (define (last-char-digit::char x::bignum)
      (integer->char-ur (bignum->fixnum (remainderbx x Z256))))

   (let* ((len (/ceilingfx (bignum-bit-length x) 8))
	  (buffer (make-string len)))
      (let loop ((x x)
		 (i (-fx len 1)))
	 (cond
	    ((and (<fx i 0)
		  (zerobx? x))
	     buffer)
	    ((<fx i 0)
	     (error "bignum->bin-str!" "integer too large" x))
	    (else
	     (string-set! buffer i (last-char-digit x))
	     (loop (/bx x Z256) (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    octet-string->bignum ...                                         */
;*---------------------------------------------------------------------*/
(define (octet-string->bignum str)
   (let loop ((i 0)
	      (res Z0))
      (if (=fx i (string-length str))
	  res
	  (loop (+fx i 1)
		(+bx (*bx res Z256)
		     (fixnum->bignum (char->integer (string-ref str i))))))))

;*---------------------------------------------------------------------*/
;*    string->bignum ...                                               */
;*---------------------------------------------------------------------*/
(define (string->bignum string #!optional (radix::long 10))
  (if (and (>=fx radix 2) (<=fx radix 36))
      ($string->bignum string radix)
      (error "string->bignum" "Illegal radix" radix)))

