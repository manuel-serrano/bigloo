;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/number.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 24 09:59:43 1995                          */
;*    Last change :  Sun Dec 30 15:20:57 2018 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.5. Numbers (page 18, r4)                                       */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Numbers@                                                */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_numbers_6_5
   
   (import  __error
	    __param)

   (use     __type
	    __bigloo
	    __tvector
	    __r4_equivalence_6_2
	    __r4_numbers_6_5_fixnum
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_pairs_and_lists_6_3
	    __r4_vectors_6_8
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r5_control_features_6_4
	    __bignum
	    
	    __evenv)

   (include "Ieee/bignum.sch")
   
   (extern  (macro $fixnum->flonum::double (::long)   "(double)")
	    (macro $flonum->fixnum::long (::double) "(long)")
	    
	    (macro $elong->flonum::double (::elong)  "(double)")
	    (macro $flonum->elong::long (::double) "(long)")
	    
	    (macro $llong->flonum::double (::llong)  "(double)")
	    (macro $flonum->llong::llong (::double) "(BGL_LONGLONG_T)")
	    
	    (macro $flonum->int32::int32 (::double) "(int32_t)")
	    (macro $int32->flonum::double (::int32) "(double)")
	    (macro $flonum->uint32::int32 (::double) "(uint32_t)")
	    (macro $uint32->flonum::double (::uint32) "(double)")
	    
	    (macro $flonum->int64::int64 (::double) "(int64_t)")
	    (macro $int64->flonum::double (::int64) "(double)")
	    (macro $flonum->uint64::uint64 (::double) "(uint64_t)")
	    (macro $uint64->flonum::double (::uint64) "(double)")

	    (export exact->inexact "bgl_exact_to_inexact")
	    (export inexact->exact "bgl_inexact_to_exact"))

   (java    (class foreign
	       (method static $fixnum->flonum::double (::long)
		  "FIXNUM_TO_FLONUM")
	       (method static $flonum->fixnum::long (::double)
		  "FLONUM_TO_FIXNUM")
	       
	       (method static $elong->flonum::double (::elong)
		  "ELONG_TO_FLONUM")
	       (method static $flonum->elong::elong (::double)
		  "FLONUM_TO_ELONG")
	       
	       (method static $llong->flonum::double (::llong)
		  "LLONG_TO_FLONUM")
	       (method static $flonum->llong::llong (::double)
		  "FLONUM_TO_LLONG")
	       
	       (method static $flonum->int32::int32 (::double)
		  "BGL_FLONUM_TO_INT32")
	       (method static $int32->flonum::double (::int32)
		  "BGL_INT32_TO_FLONUM")
	       (method static $flonum->uint32::uint32 (::double)
		  "BGL_FLONUM_TO_INT32")
	       (method static $uint32->flonum::double (::uint32)
		  "BGL_INT32_TO_FLONUM")
   
	       (method static $flonum->int64::int64 (::double)
		  "BGL_FLONUM_TO_INT64")
	       (method static $int64->flonum::double (::int64)
		  "BGL_INT64_TO_FLONUM")
	       (method static $flonum->uint64::uint64 (::double)
		  "BGL_FLONUM_TO_INT64")
	       (method static $uint64->flonum::double (::uint64)
		  "BGL_INT64_TO_FLONUM")))
   
   (export  (number?::bool obj)
	    (inline exact?::bool z)
	    (inline inexact?::bool z)
	    (complex?::bool x)
	    (rational?::bool x)
	    (inline flonum->fixnum::long ::double)
	    (inline fixnum->flonum::double ::long)
	    (inline flonum->elong::elong ::double)
	    (inline elong->flonum::double ::elong)
	    (inline flonum->llong::llong ::double)
	    (inline llong->flonum::double ::llong)
	    (inline bignum->flonum::double ::bignum)
	    (inline flonum->bignum::bignum ::double)
	    
	    (inline flonum->int32::int32 ::double)
	    (inline int32->flonum::double  ::int32)
	    (inline flonum->uint32::uint32 ::double)
	    (inline uint32->flonum::double  ::uint32)
	    
	    (inline flonum->int64::int64 ::double)
	    (inline int64->flonum::double  ::int64)
	    (inline flonum->uint64::uint64 ::double)
	    (inline uint64->flonum::double  ::uint64)
	    (2=::bool x y)
	    (=::bool x y . z)
	    (2<::bool x y) 
	    (<::bool x y . z)
	    (2>::bool x y)
	    (>::bool x y . z)
	    (2<=::bool x y)
	    (<=::bool x y . z)
	    (2>=::bool x y)
	    (>=::bool x y . z)
	    (zero?::bool x)
	    (positive?::bool x)
	    (negative?::bool x)
	    (max x . y)
	    (2max x y)
	    (min x . y)
	    (2min x y)
	    (2+ x y)
	    (+ . x)
	    (2* x y)
	    (* . x)
	    (2- x y)
	    (- x . y)
	    (2/ x y)
	    (/ x . y)
	    (abs x)
	    (floor x)
	    (ceiling x)
	    (truncate x)
	    (round x)
	    (exp::double x) 
	    (log::double x) 
	    (sin::double x) 
	    (cos::double x) 
	    (tan::double x) 
	    (asin::double x) 
	    (acos::double x) 
	    (atan::double x . y) 
	    (sqrt::double x) 
	    (expt x y)
	    (exact->inexact z)
	    (inexact->exact z)
	    (number->string::bstring x #!optional (radix 10))
	    (string->number ::bstring #!optional (radix 10)))

   (pragma  ($fixnum->flonum side-effect-free args-safe (effect) no-cfa-top)
	    ($flonum->fixnum side-effect-free args-safe (effect) no-cfa-top)
	    ($elong->flonum side-effect-free args-safe (effect) no-cfa-top)
	    ($flonum->elong side-effect-free args-safe (effect) no-cfa-top)
	    ($llong->flonum side-effect-free args-safe (effect) no-cfa-top)
	    ($flonum->llong side-effect-free args-safe (effect) no-cfa-top)
	    (bignum->flonum side-effect-free no-cfa-top nesting (effect) no-cfa-top)
 	    (flonum->bignum side-effect-free args-safe (effect) no-cfa-top)
	    (flonum->int32 side-effect-free args-safe (effect) no-cfa-top)
	    (int32->flonum side-effect-free args-safe (effect) no-cfa-top)
	    (flonum->uint32 side-effect-free args-safe (effect) no-cfa-top)
	    (uint32->flonum side-effect-free args-safe (effect) no-cfa-top)
	    (flonum->int64 side-effect-free args-safe (effect) no-cfa-top)
	    (int64->flonum side-effect-free args-safe (effect) no-cfa-top)
	    (flonum->uint64 side-effect-free args-safe (effect) no-cfa-top)
	    (uint64->flonum side-effect-free args-safe (effect) no-cfa-top)
	    (2= side-effect-free (effect) no-cfa-top)
	    (= side-effect-free (effect) no-cfa-top)
	    (2< side-effect-free (effect) no-cfa-top)
	    (< side-effect-free (effect) no-cfa-top)
	    (2> side-effect-free (effect) no-cfa-top)
	    (> side-effect-free (effect) no-cfa-top)
	    (2<= side-effect-free (effect) no-cfa-top)
	    (<= side-effect-free (effect) no-cfa-top)
	    (2>= side-effect-free (effect) no-cfa-top)
	    (>= side-effect-free (effect) no-cfa-top)
	    (zero? side-effect-free (effect) no-cfa-top)
	    (positive? side-effect-free (effect) no-cfa-top)
	    (negative? side-effect-free (effect) no-cfa-top)
	    (max side-effect-free (effect) no-cfa-top)
	    (2max side-effect-free (effect) no-cfa-top)
	    (min side-effect-free (effect) no-cfa-top)
	    (2min side-effect-free (effect) no-cfa-top)
	    (2+ side-effect-free (effect) no-cfa-top)
	    (+ side-effect-free (effect) no-cfa-top)
	    (2* side-effect-free (effect) no-cfa-top)
	    (* side-effect-free (effect) no-cfa-top)
	    (2/ side-effect-free (effect) no-cfa-top)
	    (/ side-effect-free (effect) no-cfa-top)
	    (2- side-effect-free (effect) no-cfa-top)
	    (- side-effect-free (effect) no-cfa-top)
	    (abs side-effect-free (effect) no-cfa-top)
	    (floor side-effect-free (effect) no-cfa-top)
	    (ceiling side-effect-free (effect) no-cfa-top)
	    (truncate side-effect-free (effect) no-cfa-top)
	    (round side-effect-free (effect) no-cfa-top)
	    (exp side-effect-free (effect) no-cfa-top)
	    (log side-effect-free (effect) no-cfa-top)
	    (sin side-effect-free (effect) no-cfa-top)
	    (cos side-effect-free (effect) no-cfa-top)
	    (tan side-effect-free (effect) no-cfa-top)
	    (asin side-effect-free (effect) no-cfa-top)
	    (acos side-effect-free (effect) no-cfa-top)
	    (atan side-effect-free (effect) no-cfa-top)
	    (sqrt side-effect-free (effect) no-cfa-top)
	    (expt side-effect-free (effect) no-cfa-top)
	    (exact->inexact side-effect-free args-safe (effect) no-cfa-top)
	    (inexact->exact side-effect-free args-safe (effect) no-cfa-top)
	    (number->string side-effect-free (effect) no-cfa-top)
	    (string->number side-effect-free (effect) no-cfa-top)))

;*---------------------------------------------------------------------*/
;*    number? ...                                                      */
;*---------------------------------------------------------------------*/
(define (number? obj)
   (or (fixnum? obj)
       (flonum? obj)
       (elong? obj)
       (llong? obj)
       (int8? obj)
       (uint8? obj)
       (int16? obj)
       (uint16? obj)
       (int32? obj)
       (uint32? obj)
       (int64? obj)
       (uint64? obj)
       (bignum? obj)))

;*---------------------------------------------------------------------*/
;*    exact? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (exact? z)
   (or (fixnum? z)
       (elong? z)
       (llong? z)
       (int8? z)
       (uint8? z)
       (int16? z)
       (uint16? z)
       (int32? z)
       (uint32? z)
       (int64? z)
       (uint64? z)
       (bignum? z)))

;*---------------------------------------------------------------------*/
;*    inexact? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (inexact? z)
   (flonum? z))

;*---------------------------------------------------------------------*/
;*    complex? ...                                                     */
;*---------------------------------------------------------------------*/
(define (complex? x)
   (number? x))

;*---------------------------------------------------------------------*/
;*    rational? ...                                                    */
;*---------------------------------------------------------------------*/
(define (rational? x)
   (real? x))

;*---------------------------------------------------------------------*/
;*    flonum->fixnum ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (flonum->fixnum x) ($flonum->fixnum x))
(define-inline (fixnum->flonum x) ($fixnum->flonum x))
		       
;*---------------------------------------------------------------------*/
;*    flonum->elong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (flonum->elong x) ($flonum->elong x))
(define-inline (elong->flonum x) ($elong->flonum x))
		       
;*---------------------------------------------------------------------*/
;*    flonum->llong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (flonum->llong x) ($flonum->llong x))
(define-inline (llong->flonum x) ($llong->flonum x))
		       
;*---------------------------------------------------------------------*/
;*    flonum->bignum ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (flonum->bignum x) ($flonum->bignum x))
(define-inline (bignum->flonum x) ($bignum->flonum x))

;*---------------------------------------------------------------------*/
;*    flonum->int32 ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (flonum->int32 x) ($flonum->int32 x))
(define-inline (int32->flonum x) ($int32->flonum x))

(define-inline (flonum->uint32 x) ($flonum->uint32 x))
(define-inline (uint32->flonum x) ($uint32->flonum x))

;*---------------------------------------------------------------------*/
;*    flonum->int64 ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (flonum->int64 x) ($flonum->int64 x))
(define-inline (int64->flonum x) ($int64->flonum x))

(define-inline (flonum->uint64 x) ($flonum->uint64 x))
(define-inline (uint64->flonum x) ($uint64->flonum x))

;*---------------------------------------------------------------------*/
;*    integer unboxing                                                 */
;*---------------------------------------------------------------------*/
(define ($subelong->elong x)
   (cond-expand
      (bint61
       (cond
	  ((elong? x) x)
	  ((int8? x) (fixnum->elong (int8->fixnum x)))
	  ((uint8? x) (fixnum->elong (uint8->fixnum x)))
	  ((int16? x) (fixnum->elong (int16->fixnum x)))
	  ((uint16? x) (fixnum->elong (uint16->fixnum x)))
	  ((int32? x) (fixnum->elong (int32->fixnum x)))
	  ((uint32? x) (fixnum->elong (uint32->fixnum x)))
	  ((int64? x) (fixnum->elong (int64->fixnum x)))))
      (else
       (cond
	  ((elong? x) x)
	  ((int8? x) (fixnum->elong (int8->fixnum x)))
	  ((uint8? x) (fixnum->elong (uint8->fixnum x)))
	  ((int16? x) (fixnum->elong (int16->fixnum x)))
	  ((uint16? x) (fixnum->elong (uint16->fixnum x)))
	  ((int32? x) (fixnum->elong (int32->fixnum x)))))))

(define ($subllong->llong x)
   (cond-expand
      (bint61
       x)
      (else
       (cond
	  ((llong? x) x)
	  ((uint32? x) (uint32->llong x))
	  ((int64? x) (int64->llong x))))))

(define ($subelong? x)
   (cond-expand
      (bint61
       (or (elong? x) (int8? x) (uint8? x) (int16? x) (uint16? x) (int32? x)
	   (uint32? x) (int64? x)))
      (else
       (or (elong? x) (int8? x) (uint8? x) (int16? x) (uint16? x) (int32? x)))))

(define ($subllong? x)
   (cond-expand
      (bint61
       (llong? x))
      (else
       (or (llong? x) (uint32? x) (int64? x)))))

;*---------------------------------------------------------------------*/
;*    2op :: ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (2op op x y)
   (let ((opfx (symbol-append op 'fx))
	 (opbx (symbol-append op 'bx))
	 (opfl (symbol-append op 'fl))
	 (opelong (symbol-append op 'elong))
	 (opllong (symbol-append op 'llong))
	 (opuint64 (symbol-append op 'u64))
	 (oppost 'begin))
      (case op
	 ((+ - * /)
	  (set! opfx (symbol-append opfx '-safe))
	  (set! opelong (symbol-append opelong '-safe))
	  (set! opllong (symbol-append opllong '-safe))
	  (set! oppost '$bignum->fixnum-safe)))
      `(cond
	  ((fixnum? ,x)
	   (cond
	      ((fixnum? ,y)
	       (,opfx ,x ,y))
	      ((flonum? ,y)
	       (,opfl ($fixnum->flonum ,x) ,y))
	      (($subelong? ,y)
	       (,opelong (fixnum->elong ,x) ($subelong->elong ,y)))
	      (($subllong? ,y)
	       (,opllong (fixnum->llong ,x) ($subllong->llong ,y)))
	      ((uint64? ,y)
	       (,opuint64 (llong->uint64 (fixnum->llong ,x)) ,y))
	      ((bignum? ,y)
	       (,oppost (,opbx (fixnum->bignum ,x) ,y)))
	      (else
	       (error ,(symbol->string op) "not a number" ,y))))
	  ((flonum? ,x)
	   (cond
	      ((flonum? ,y)
	       (,opfl ,x ,y))
	      ((fixnum? ,y)
	       (,opfl ,x ($fixnum->flonum ,y)))
	      (($subelong? ,y)
	       (,opfl ,x ($elong->flonum ($subelong->elong ,y))))
	      (($subllong? ,y)
	       (,opfl ,x ($llong->flonum ($subllong->llong ,y))))
	      ((uint64? ,y)
	       (,opfl ,x (uint64->flonum ,y)))
	      ((bignum? ,y)
	       (,opfl ,x (bignum->flonum ,y)))
	      (else
	       (error ,(symbol->string op) "not a number" ,y))))
	  (($subelong? ,x)
	   (cond
	      ((fixnum? ,y)
	       (,opelong ($subelong->elong ,x) (fixnum->elong ,y)))
	      (($subelong? ,y)
	       (,opelong ($subelong->elong ,x) ($subelong->elong ,y)))
	      ((flonum? ,y)
	       (,opfl ($elong->flonum ($subelong->elong ,x)) ,y))
	      (($subllong? ,y)
	       (,opllong ($elong->llong ($subelong->elong ,x)) ($subllong->llong ,y)))
	      ((uint64? ,y)
	       (,opuint64 (llong->uint64 ($elong->llong ($subelong->elong ,x))) ,y))
	      ((bignum? ,y)
	       (,opbx (elong->bignum ($subelong->elong ,x)) ,y))
	      (else
	       (error ,(symbol->string op) "not a number" ,y))))
	  (($subllong? ,x)
	   (cond
	      ((fixnum? ,y)
	       (,opllong ($subllong->llong ,x) (fixnum->llong ,y)))
	      ((flonum? ,y)
	       (,opfl ($llong->flonum ,x) ,y))
	      (($subllong? ,y)
	       (,opllong ($subllong->llong ,x) ($subllong->llong ,y)))
	      (($subelong? ,y)
	       (,opllong ($subllong->llong ,x) ($elong->llong ($subelong->elong ,y))))
	      ((bignum? ,y)
	       (,opbx (llong->bignum ($subllong->llong ,x)) ,y))
	      ((uint64? ,y)
	       (,opuint64 (llong->uint64 ,x) ,y))
	      (else
	       (error ,(symbol->string op) "not a number" ,y))))
	  ((uint64? ,x)
	   (cond
	      ((fixnum? ,y)
	       (,opuint64 ,x (fixnum->uint64 ,y)))
	      ((uint64? ,y)
	       (,opuint64 ,x ,y))
	      ((flonum? ,y)
	       (,opfl ($uint64->flonum ,x) ,y))
	      (($subllong? ,y)
	       (,opuint64 ,x (llong->uint64 ($subllong->llong ,y))))
	      (($subelong? ,y)
	       (,opuint64 ,x (llong->uint64 ($elong->llong ($subelong->elong ,y)))))
	      ((bignum? ,y)
	       (,opbx ($uint64->bignum ,x) ,y))
	      (else
	       (error ,(symbol->string op) "not a number5" ,y))))
 	  ((bignum? ,x)
 	   (cond
 	      ((bignum? ,y)
 	       (,oppost (,opbx ,x ,y)))
 	      ((fixnum? ,y)
 	       (,oppost (,opbx ,x (fixnum->bignum ,y))))
 	      ((flonum? ,y)
 	       (,opfl (bignum->flonum ,x) ,y))
 	      (($subelong? ,y)
 	       (,opbx ,x (elong->bignum ($subelong->elong ,y))))
 	      (($subllong? ,y)
 	       (,opbx ,x (llong->bignum ($subllong->llong ,y))))
	      ((uint64? ,y)
 	       (,opbx ,x ($uint64->bignum ,y)))
 	      (else
 	       (error ,(symbol->string op) "not a number" ,y))))
	  (else
	   (error ,(symbol->string op) "not a number" ,x)))))

;*---------------------------------------------------------------------*/
;*    2= ...                                                           */
;*---------------------------------------------------------------------*/
(define (2= x y)
   (2op = x y))

;*---------------------------------------------------------------------*/
;*    = ...                                                            */
;*---------------------------------------------------------------------*/
(define (= x y . z)
   (define (=-list x z)
      (cond
	 ((null? z) #t)
	 ((2= x (car z)) (=-list x (cdr z)))
	 (else #f)))
   (and (2= x y) (=-list y z)))
 
;*---------------------------------------------------------------------*/
;*    2< ...                                                           */
;*---------------------------------------------------------------------*/
(define (2< x y)
   (2op < x y))

;*---------------------------------------------------------------------*/
;*    < ...                                                            */
;*---------------------------------------------------------------------*/
(define (< x y . z)
   (define (<-list x z)
      (cond
	 ((null? z) #t)
	 ((2< x (car z)) (<-list (car z) (cdr z)))
	 (else #f)))
   (and (2< x y) (<-list y z)))
   
;*---------------------------------------------------------------------*/
;*    2> ...                                                           */
;*---------------------------------------------------------------------*/
(define (2> x y)
   (2op > x y))

;*---------------------------------------------------------------------*/
;*    > ...                                                            */
;*---------------------------------------------------------------------*/
(define (> x y . z)
   (define (>-list x z)
      (cond
	 ((null? z) #t)
	 ((2> x (car z)) (>-list (car z) (cdr z)))
	 (else #f)))
   (and (2> x y) (>-list y z)))
 
;*---------------------------------------------------------------------*/
;*    2<= ...                                                          */
;*---------------------------------------------------------------------*/
(define (2<= x y)
   (2op <= x y))

;*---------------------------------------------------------------------*/
;*    <= ...                                                           */
;*---------------------------------------------------------------------*/
(define (<= x y . z)
   (define (<=-list x z)
      (cond
	 ((null? z) #t)
	 ((2<= x (car z)) (<=-list (car z) (cdr z)))
	 (else #f)))
   (and (2<= x y) (<=-list y z)))

;*---------------------------------------------------------------------*/
;*    2>= ...                                                          */
;*---------------------------------------------------------------------*/
(define (2>= x y)
   (2op >= x y))

;*---------------------------------------------------------------------*/
;*    >= ...                                                           */
;*---------------------------------------------------------------------*/
(define (>= x y . z)
   (define (>=-list x z)
      (cond
	 ((null? z) #t)
	 ((2>= x (car z)) (>=-list (car z) (cdr z)))
	 (else #f)))
   (and (2>= x y) (>=-list y z)))

;*---------------------------------------------------------------------*/
;*    zero? ...                                                        */
;*---------------------------------------------------------------------*/
(define (zero? x)
   (cond
      ((fixnum? x) (zerofx? x))
      ((flonum? x) (zerofl? x))
      ((elong? x) (=elong x #e0))
      ((llong? x) (=llong x #l0))
      ((bignum? x) (zerobx? x))
      (else (error "zero" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    positive? ...                                                    */
;*---------------------------------------------------------------------*/
(define (positive? x)
   (cond
      ((fixnum? x) (positivefx? x))
      ((flonum? x) (positivefl? x))
      ((elong? x) (>elong x #e0))
      ((llong? x) (>llong x #l0))
      ((bignum? x) (positivebx? x))
      (else (error "positive" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    negative? ...                                                    */
;*---------------------------------------------------------------------*/
(define (negative? x)
   (cond
      ((fixnum? x) (negativefx? x))
      ((flonum? x) (negativefl? x))
      ((elong? x) (<elong x #e0))
      ((llong? x) (<llong x #l0))
      ((bignum? x) (negativebx? x))
      (else (error "negative" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    2max ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (2maxfx x y)
   (if (>fx x y) x y))
(define-inline (2maxfl x y)
   (if (>fl x y) x y))
(define-inline (2maxelong x y)
   (if (>elong x y) x y))
(define-inline (2maxllong x y)
   (if (>llong x y) x y))
(define-inline (2maxbx x y)
   (if (>bx x y) x y))
(define-inline (2maxu64 x y)
   (if (>u64 x y) x y))

;*---------------------------------------------------------------------*/
;*    2max ...                                                         */
;*---------------------------------------------------------------------*/
(define (2max x y)
   (2op 2max x y))

;*---------------------------------------------------------------------*/
;*    max ...                                                          */
;*---------------------------------------------------------------------*/
(define (max x . y)
   (let loop ((x x)
	      (y y))
      (if (pair? y)
	  (loop (2max x (car y)) (cdr y))
	  x)))

;*---------------------------------------------------------------------*/
;*    2min ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (2minfx x y)
   (if (>fx x y) y x))
(define-inline (2minfl x y)
   (if (>fl x y) y x))
(define-inline (2minelong x y)
   (if (>elong x y) y x))
(define-inline (2minllong x y)
   (if (>llong x y) y x))
(define-inline (2minbx x y)
   (if (>bx x y) y x))
(define-inline (2minu64 x y)
   (if (>u64 x y) y x))

;*---------------------------------------------------------------------*/
;*    2min ...                                                         */
;*---------------------------------------------------------------------*/
(define (2min x y)
   (2op 2min x y))

;*---------------------------------------------------------------------*/
;*    min ...                                                          */
;*---------------------------------------------------------------------*/
(define (min x . y)
   (let loop ((x x)
	      (y y))
      (if (pair? y)
	  (loop (2min x (car y)) (cdr y))
	  x)))

;*---------------------------------------------------------------------*/
;*    2+ ...                                                           */
;*---------------------------------------------------------------------*/
(define (2+ x y)
   (2op + x y))

;*---------------------------------------------------------------------*/
;*    + ...                                                            */
;*---------------------------------------------------------------------*/
(define (+ . x)
   ;; CARE: 0 cannot be the initial value of the recursion, otherwise,
   ;; -0.0 + -0.0 would be incorrectly handled
   (if (null? x)
       0
       (let loop ((sum (car x))
		  (x (cdr x)))
	  (if (pair? x)
	      (loop (2+ sum (car x)) (cdr x))
	      sum))))

;*---------------------------------------------------------------------*/
;*    2* ...                                                           */
;*---------------------------------------------------------------------*/
(define (2* x y)
   (2op * x y))

;*---------------------------------------------------------------------*/
;*    * ...                                                            */
;*---------------------------------------------------------------------*/
(define (*  . x)
   (let loop ((product 1)
	      (x x))
      (if (pair? x)
	  (loop (2* product (car x)) (cdr x))
	  product)))

;*---------------------------------------------------------------------*/
;*    2- ...                                                           */
;*---------------------------------------------------------------------*/
(define (2- x y)
   (2op - x y))

;*---------------------------------------------------------------------*/
;*    - ...                                                            */
;*---------------------------------------------------------------------*/
(define (- x . y)
    (if (pair? y)
	(let loop ((result (2- x (car y)))
		   (args (cdr y)))
	   (if (pair? args)
	       (loop (2- result (car args)) (cdr args))
	       result))
	(2- 0 x)))

;*---------------------------------------------------------------------*/
;*    2/ ...                                                           */
;*---------------------------------------------------------------------*/
(define (2/ x y)
   (cond
      ((fixnum? x)
       (cond
	  ((fixnum? y)
	   (if (=fx (remainderfx x y) 0)
	       (/fx x y)
	       (/fl ($fixnum->flonum x) ($fixnum->flonum y))))
	  ((flonum? y)
	   (/fl ($fixnum->flonum x) y))
	  ((elong? y)
	   (let ((ex (fixnum->elong x)))
	      (if (=elong (remainderelong ex y) #e0)
		  (/elong ex y)
		  (/fl ($fixnum->flonum x) ($elong->flonum y)))))
	  ((llong? y)
	   (let ((lx (fixnum->llong x)))
	      (if (=llong (remainderllong lx y) #l0)
		  (/llong lx y)
		  (/fl ($fixnum->flonum x) ($llong->flonum y)))))
	  ((bignum? y)
	   (multiple-value-bind (q r)
	      ($divrembx (fixnum->bignum x) y)
	      (if (zerobx? r)
		  q
		  (/fl ($fixnum->flonum x) (bignum->flonum y)))))
	  (else
	   (error "/" "not a number" y))))
      ((flonum? x)
       (cond
	  ((flonum? y)
	   (/fl x y))
	  ((fixnum? y)
	   (/fl x ($fixnum->flonum y)))
	  ((elong? y)
	   (/fl x ($elong->flonum y)))
	  ((llong? y)
	   (/fl x ($llong->flonum y)))
	  ((bignum? y)
	   (/fl x (bignum->flonum y)))
	  (else
	   (error "/" "not a number" y))))
      ((elong? x)
       (cond
	  ((fixnum? y)
	   (let ((ey (fixnum->elong y)))
	      (if (=elong (remainderelong x ey) #e0)
		  (/elong x ey)
		  (/fl ($elong->flonum x) ($fixnum->flonum y)))))
	  ((flonum? y)
	   (/fl ($elong->flonum x) y))
	  ((elong? y)
	   (if (=elong (remainderelong x y) #e0)
	       (/elong x y)
	       (/fl ($elong->flonum x) ($elong->flonum y))))
	  ((llong? y)
	   (let* ((fx ($elong->flonum x))
		  (lx ($flonum->llong fx)))
	      (if (=llong (remainderllong lx y) #l0)
		  (/llong lx y)
		  (/fl fx ($llong->flonum y)))))
	  ((bignum? y)
	   (multiple-value-bind (q r)
	      ($divrembx (elong->bignum x) y)
	      (if (zerobx? r)
		  q
		  (/fl ($elong->flonum x) (bignum->flonum y)))))
	  (else
	   (error "/" "not a number" y))))
      ((llong? x)
       (cond
	  ((fixnum? y)
	   (let ((ly (fixnum->llong y)))
	      (if (=llong (remainderllong x ly) #l0)
		  (/llong x ly)
		  (/fl ($llong->flonum x) ($fixnum->flonum y)))))
	  ((flonum? y)
	   (/fl ($llong->flonum x) y))
	  ((elong? y)
	   (let* ((fy ($elong->flonum y))
		  (ly ($flonum->llong fy)))
	      (if (=llong (remainderllong x ly) #l0)
		  (/llong x ly)
		  (/fl ($llong->flonum x) fy))))
	  ((llong? y)
	   (if (=llong (remainderllong x y) #l0)
	       (/llong x y)
	       (/fl ($llong->flonum x) ($llong->flonum y))))
	  ((bignum? y)
	   (multiple-value-bind (q r)
	      ($divrembx (llong->bignum x) y)
	      (if (zerobx? r)
		  q
		  (/fl ($llong->flonum x) (bignum->flonum y)))))
	  (else
	   (error "/" "not a number" y))))
      ((bignum? x)
       (cond
 	  ((fixnum? y)
	   (multiple-value-bind (q r)
	      ($divrembx x (fixnum->bignum y))
 	      (if (zerobx? r)
 		  q
 		  (/fl (bignum->flonum x) ($fixnum->flonum y)))))
 	  ((flonum? y)
 	   (/fl (bignum->flonum x) y))
 	  ((elong? y)
	   (multiple-value-bind (q r)
	      ($divrembx x (elong->bignum y))
 	      (if (zerobx? r)
 		  q
 		  (/fl (bignum->flonum x) ($elong->flonum y)))))
 	  ((llong? y)
	   (multiple-value-bind (q r)
	      ($divrembx x (llong->bignum y))
 	      (if (zerobx? r)
 		  q
 		  (/fl (bignum->flonum x) ($llong->flonum y)))))
 	  ((bignum? y)
	   (multiple-value-bind (q r)
	      ($divrembx x y)
 	      (if (zerobx? r)
 		  q
 		  (/fl (bignum->flonum x) (bignum->flonum y)))))
  	  (else
  	   (error "/" "not a number" y))))
      (else
       (error "/" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    / ...                                                            */
;*---------------------------------------------------------------------*/
(define (/ x . y)
    (if (pair? y)
	(let loop ((result (2/ x (car y)))
		   (z (cdr y)))
	     (if (pair? z)
		 (loop (2/ result (car z))
		       (cdr z))
		 result))
	(2/ 1 x)))

;*---------------------------------------------------------------------*/
;*    abs ...                                                          */
;*---------------------------------------------------------------------*/
(define (abs x)
   (cond
      ((fixnum? x)
       (if (=fx x (minvalfx))
	   (negbx (fixnum->bignum x))
	   (absfx x)))
      ((flonum? x)
       (absfl x))
      ((elong? x)
       (if (=elong x (minvalelong))
	   (negbx (elong->bignum x))
	   (abselong x)))
      ((llong? x)
       (if (=llong x (minvalllong))
	   (negbx (llong->bignum x))
	   (absllong x)))
      ((bignum? x)
       (absbx x))
      (else
       (error "abs" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    floor ...                                                        */
;*---------------------------------------------------------------------*/
(define (floor x)
   (cond
      ((fixnum? x) x)
      ((flonum? x) (floorfl x))
      ((elong? x) x)
      ((llong? x) x)
      ((bignum? x) x)
      (else (error "floor" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    ceiling ...                                                      */
;*---------------------------------------------------------------------*/
(define (ceiling x)
   (cond
      ((fixnum? x) x)
      ((flonum? x) (ceilingfl x))
      ((elong? x) x)
      ((llong? x) x)
      ((bignum? x) x)
      (else (error "ceiling" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    truncate ...                                                     */
;*---------------------------------------------------------------------*/
(define (truncate x)
   (cond
      ((fixnum? x) x)
      ((flonum? x) (truncatefl x))
      ((elong? x) x)
      ((llong? x) x)
      (else (error "truncate" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    round ...                                                        */
;*---------------------------------------------------------------------*/
(define (round x)
   (cond
      ((fixnum? x) x)
      ((flonum? x) (roundfl x))
      ((elong? x) x)
      ((llong? x) x)
      ((bignum? x) x)
      (else (error "round" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    exp ...                                                          */
;*---------------------------------------------------------------------*/
(define (exp x)
   (cond
      ((flonum? x) (expfl x))
      ((fixnum? x) (expfl ($fixnum->flonum x)))
      ((elong? x) (expfl ($elong->flonum x)))
      ((llong? x) (expfl ($llong->flonum x)))
      ((bignum? x) (expfl (bignum->flonum x)))
      (else (error "exp" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    log ...                                                          */
;*---------------------------------------------------------------------*/
(define (log x)
   (cond
      ((flonum? x) (logfl x))
      ((fixnum? x) (logfl ($fixnum->flonum x)))
      ((elong? x) (logfl ($elong->flonum x)))
      ((llong? x) (logfl ($llong->flonum x)))
      ((bignum? x) (logfl ($bignum->flonum x)))
      (else (error "log" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    sin ...                                                          */
;*---------------------------------------------------------------------*/
(define (sin x)
   (cond
      ((flonum? x) (sinfl x))
      ((fixnum? x) (sinfl ($fixnum->flonum x)))
      ((elong? x) (sinfl ($elong->flonum x)))
      ((llong? x) (sinfl ($llong->flonum x)))
      ((bignum? x) (sinfl (bignum->flonum x)))
      (else (error "sin" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    cos ...                                                          */
;*---------------------------------------------------------------------*/
(define (cos x)
   (cond
      ((flonum? x) (cosfl x))
      ((fixnum? x) (cosfl ($fixnum->flonum x)))
      ((elong? x) (cosfl ($elong->flonum x)))
      ((llong? x) (cosfl ($llong->flonum x)))
      ((bignum? x) (cosfl (bignum->flonum x)))
      (else (error "cos" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    tan ...                                                          */
;*---------------------------------------------------------------------*/
(define (tan x)
   (cond
      ((flonum? x) (tanfl x))
      ((fixnum? x) (tanfl ($fixnum->flonum x)))
      ((elong? x) (tanfl ($elong->flonum x)))
      ((llong? x) (tanfl ($llong->flonum x)))
      ((bignum? x) (tanfl (bignum->flonum x)))
      (else (error "tan" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    asin ...                                                         */
;*---------------------------------------------------------------------*/
(define (asin x)
   (cond
      ((flonum? x) (asinfl x))
      ((fixnum? x) (asinfl ($fixnum->flonum x)))
      ((elong? x) (asinfl ($elong->flonum x)))
      ((llong? x) (asinfl ($llong->flonum x)))
      ((bignum? x) (asinfl (bignum->flonum x)))
      (else (error "asin" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    acos ...                                                         */
;*---------------------------------------------------------------------*/
(define (acos x)
   (cond
      ((flonum? x) (acosfl x))
      ((fixnum? x) (acosfl ($fixnum->flonum x)))
      ((elong? x) (acosfl ($elong->flonum x)))
      ((llong? x) (acosfl ($llong->flonum x)))
      ((bignum? x) (acosfl (bignum->flonum x)))
      (else (error "acos" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    atan ...                                                         */
;*---------------------------------------------------------------------*/
(define (atan x . y)
   (let ((y (if (pair? y)
		(let ((y (car y)))
		   (cond
		      ((fixnum? y) ($fixnum->flonum y))
		      ((flonum? y) y)
		      (else (error "atan" "not a number" y))))
		#f)))
      (define (do-atanfl x) 
	 (if (number? y)
	     (atanfl x y)
	     (atanfl x)))
      (cond
	 ((flonum? x) (do-atanfl x))
	 ((fixnum? x) (do-atanfl ($fixnum->flonum x)))
	 ((elong? x) (do-atanfl ($elong->flonum x)))
	 ((llong? x) (do-atanfl ($llong->flonum x)))
	 ((bignum? x) (do-atanfl (bignum->flonum x)))
	 (else (error "atan" "not a number" x)))))

;*---------------------------------------------------------------------*/
;*    sqrt ...                                                         */
;*---------------------------------------------------------------------*/
(define (sqrt x)
   (cond
      ((fixnum? x) (sqrtfl ($fixnum->flonum x)))
      ((flonum? x) (sqrtfl x))
      ((elong? x) (sqrtfl ($elong->flonum x)))
      ((llong? x) (sqrtfl ($llong->flonum x)))
      ((bignum? x) (sqrtfl (bignum->flonum x)))
      (else (error "sqrt" "not a number" x))))

;*---------------------------------------------------------------------*/
;*    expt ...                                                         */
;*---------------------------------------------------------------------*/
(define (expt x y)
   (cond
      ((and (flonum? x) (flonum? y) (=fl x 0.0) (=fl y 0.0))
       1.0)
      ((and (fixnum? x) (fixnum? y) (>=fx y 0))
       (exptfx x y))
      ((and (bignum? x) (bignum? y) (positivebx? y))
       (exptbx x y))
      ((bignum? x)
       (let ((y1 (cond
		    ((flonum? y) ($fixnum->bignum ($flonum->fixnum y)))
		    ((fixnum? y) ($fixnum->bignum y))
		    ((elong? y) ($elong->bignum y))
		    ((llong? y) ($llong->bignum y))
		    ((bignum? y) y)
		    (else (error "expt" "not a number" y)))))
	  (exptbx x y1)))
      (else
       (let ((x1 (cond
		    ((flonum? x) x)
		    ((fixnum? x) ($fixnum->flonum x))
		    ((elong? x) ($elong->flonum x))
		    ((llong? x) ($llong->flonum x))
		    ((bignum? x) (bignum->flonum x))
		    (else (error "expt" "not a number" x))))
	     (y1 (cond
		    ((flonum? y) y)
		    ((fixnum? y) ($fixnum->flonum y))
		    ((elong? y) ($elong->flonum y))
		    ((llong? y) ($llong->flonum y))
		    ((bignum? y) (bignum->flonum y))
		    (else (error "expt" "not a number" y)))))
	  (exptfl x1 y1)))))

;*---------------------------------------------------------------------*/
;*    exact->inexact ...                                               */
;*---------------------------------------------------------------------*/
(define (exact->inexact z)
   (cond
      ((fixnum? z) ($fixnum->flonum z))
      ((flonum? z) z)
      ((elong? z) ($elong->flonum z))
      ((llong? z) ($llong->flonum z))
      ((bignum? z) (bignum->flonum z))
      (else z)))

;*---------------------------------------------------------------------*/
;*    max int values ...                                               */
;*---------------------------------------------------------------------*/
(define *maxintfl* (fixnum->flonum $maxvalfx))
(define *minintfl* (fixnum->flonum $minvalfx))

;*---------------------------------------------------------------------*/
;*    inexact->exact ...                                               */
;*---------------------------------------------------------------------*/
(define (inexact->exact z)
   (if (inexact? z)
       (if (and (>=fl z *minintfl*) (<=fl z *maxintfl*))
	   ($flonum->fixnum z)
	   ($flonum->bignum z))
       z))

(define (inexact->exact-old z)
   (if (inexact? z)
       ($flonum->fixnum z)
       z))
 
;*---------------------------------------------------------------------*/
;*    number->string ...                                               */
;*---------------------------------------------------------------------*/
(define (number->string x #!optional (radix 10))
   (cond
      ((not (integer? radix)) (error 'number->string "Illegal radix" radix))
      ((fixnum? x) (integer->string x radix))
      ((flonum? x) (real->string x))
      ((elong? x) (elong->string x radix))
      ((llong? x) (llong->string x radix))
      ((bignum? x) (bignum->string x radix))
      ((int8? x) (integer->string (int8->fixnum x) radix))
      ((uint8? x) (integer->string (uint8->fixnum x) radix))
      ((int16? x) (integer->string (int16->fixnum x) radix))
      ((uint16? x) (integer->string (uint16->fixnum x) radix))
      ((int32? x) (llong->string (int32->llong x) radix))
      ((uint32? x) (llong->string (uint32->llong x) radix))
      ((int64? x) (llong->string (int64->llong x) radix))
      ((uint64? x) (llong->string (uint64->llong x) radix))
      (else (error "number->string" "Argument not a number" x))))

;*---------------------------------------------------------------------*/
;*    @deffn string->number@ ...                                       */
;*---------------------------------------------------------------------*/
(define (string->number x #!optional (radix 10))
   
   (define (integer-string? x r)
      (let ((len (string-length x)))
	 (let loop ((i (-fx len 1)))
	    (cond ((=fx -1 i)
		   #t)
		  ((and (char>=? (string-ref x i) #\0)
			(char<=? (string-ref x i) #\1)
			(>=fx r 2))
		   (loop (-fx i 1)))
		  ((and (char>=? (string-ref x i) #\2)
			(char<=? (string-ref x i) #\7)
			(>=fx r 8))
		   (loop (-fx i 1)))
		  ((and (char>=? (string-ref x i) #\8)
			(char<=? (string-ref x i) #\9)
			(>=fx r 10))
		   (loop (-fx i 1)))
		  ((and (char>=? (string-ref x i) #\a)
			(char<=? (string-ref x i) #\f)
			(=fx r 16))
		   (loop (-fx i 1)))
		  ((and (char>=? (string-ref x i) #\A)
			(char<=? (string-ref x i) #\F)
			(=fx r 16))
		   (loop (-fx i 1)))
		  ((or (char=? (string-ref x i) #\-)
		       (char=? (string-ref x i) #\+))
		   (and (=fx i 0) (>fx len 1)))
		  (else #f)))))
   
   (define (real-string? x)
      (let ((len (string-length x)))
	 (let loop ((i 0)
		    (e #f)
		    (p 0)
		    (d #f))
	    (cond ((=fx i len)
		   d)
		  ((and (char>=? (string-ref x i) #\0)
			(char<=? (string-ref x i) #\9))
		   (loop (+fx i 1)
			 e
			 0
			 #t))
		  ((char=? (string-ref x i) #\.)
		   (loop (+fx i 1)
			 e
			 0
			 d))
		  ((or (char=? (string-ref x i) #\e)
		       (char=? (string-ref x i) #\E))
		   (if (or e (not d))
		       #f
		       (loop (+fx i 1)
			     #t
			     (+fx i 1)
			     d)))
		  ((or (char=? (string-ref x i) #\-)
		       (char=? (string-ref x i) #\+))
		   (and (or (=fx i 0) (=fx i p))
			(loop (+fx i 1)
			      e
			      0
			      d)))
		  (else #f)))))
   
   (cond
      ((not (integer? radix))
       (error 'number->string "Illegal radix" radix))
      ((=fx (string-length x) 0)
       #f)
      ((integer-string? x radix)
       (string->integer-obj x radix))
      ((string=? x "+nan.0")
       +nan.0)
      ((string=? x "+inf.0")
       +inf.0)
      ((string=? x "-inf.0")
       -inf.0)
      ((real-string? x)
       (if (=fx radix 10)
	   (string->real x)
	   (error "string->number"
		  "Only radix `10' is legal for floating point number"
		  radix)))
      (else
       #f)))
