;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/flonum.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 26 14:04:03 1992                          */
;*    Last change :  Mon Jan 27 05:09:36 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.5. Numbers (page 18, r4) The `flonum' functions                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_numbers_6_5_flonum
   
   (import  __error
	    __param
	    __r4_equivalence_6_2
	    __r4_numbers_6_5_flonum_dtoa)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_booleans_6_1
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_characters_6_6
	    __r4_pairs_and_lists_6_3
	    __r4_symbols_6_4
	    __r4_numbers_6_5_fixnum
	    
	    __evenv)
   
   (extern  (macro $modf::double (::double ::void*) "modf")
	    (macro c-flonum?::bool (::obj) "REALP")
	    (macro $flonum?::bool (::obj) "FLONUMP")
	    (infix macro c-=fl::bool (::double ::double) "==")
	    (infix macro c-<fl::bool (::double ::double) "<")
	    (infix macro c-<=fl::bool (::double ::double) "<=")
	    (infix macro c->fl::bool (::double ::double) ">")
	    (infix macro c->=fl::bool (::double ::double) ">=")
	    (infix macro c-+fl::double (::double ::double) "+")
	    (infix macro c--fl::double (::double ::double) "-")
	    (infix macro c-*fl::double (::double ::double) "*")
	    (infix macro c-/fl::double (::double ::double) "/")
	    (macro c-negfl::double (::double) "NEG")
	    (macro c-floor::double (::double) "floor")
	    (macro c-ceiling::double (::double) "ceil")
	    (macro c-fmod::double (::double ::double) "fmod")
	    (macro c-exp::double (::double) "exp")
	    (macro c-log::double (::double) "log")
	    (macro c-sin::double (::double) "sin")
	    (macro c-cos::double (::double) "cos")
	    (macro c-tan::double (::double) "tan")
	    (macro c-asin::double (::double) "asin")
	    (macro c-acos::double (::double) "acos")
	    (macro c-atan::double (::double) "atan")
	    (macro c-atan2::double (::double ::double) "atan2")
	    (macro c-sqrt::double (::double) "sqrt")
	    (macro c-pow::double (::double ::double) "pow")
	    (macro strtod::double (::string ::long) "strtod")
	    (macro $strtod::double (::string) "STRTOD")
	    (macro c-absfl::double (::double) "fabs")
	    (macro c-maxfl::double (::double ::double) "BGL_FL_MAX2")
	    (macro c-minfl::double (::double ::double) "BGL_FL_MIN2")
	    (macro $signbit::long (::double) "BGL_SIGNBIT")
	    (macro c-roundfl::double (::double) "BGL_FL_ROUND")
	    (macro c-isfinite::bool (::double) "BGL_IS_FINITE")
	    (macro c-isinf::bool (::double) "BGL_IS_INF")
	    (macro c-isnan::bool (::double) "BGL_IS_NAN")
	    (%ieee-string->double::double (::bstring)
					  "bgl_ieee_string_to_double")
	    (%double->ieee-string::bstring (::double)
					   "bgl_double_to_ieee_string")
	    (%ieee-string->float::float (::bstring)
					  "bgl_ieee_string_to_float")
	    (%float->ieee-string::bstring (::float)
					   "bgl_float_to_ieee_string")
	    (macro %double->llong-bits::llong (::double) "DOUBLE_TO_LLONG_BITS")
	    (macro %llong-bits->double::double (::llong) "LLONG_BITS_TO_DOUBLE")
	    (macro %float->int-bits::int (::float) "FLOAT_TO_INT_BITS")
	    (macro %int-bits->float::float (::int) "INT_BITS_TO_FLOAT")
	    (macro $randomfl::double () "RANDOMFL"))
   
   (java    (class foreign
	       (method static c-flonum?::bool (::obj)
		  "REALP")
	       (method static $flonum?::bool (::obj)
		  "REALP")
	       (method static c-=fl::bool (::double ::double)
		  "EQ_FL")
	       (method static c-<fl::bool (::double ::double)
		  "LT_FL")
	       (method static c-<=fl::bool (::double ::double)
		  "LE_FL")
	       (method static c->fl::bool (::double ::double)
		  "GT_FL")
	       (method static c->=fl::bool (::double ::double)
		  "GE_FL")
	       (method static c-+fl::double (::double ::double)
		  "PLUS_FL")
	       (method static c--fl::double (::double ::double)
		  "MINUS_FL")
	       (method static c-*fl::double (::double ::double)
		  "MUL_FL")
	       (method static c-/fl::double (::double ::double)
		  "DIV_FL")
	       (method static c-negfl::double (::double)
		  "NEG_FL")
	       (method static c-fmod::double (::double ::double)
		  "fmod")
	       (method static c-floor::double (::double)
		  "floor")
	       (method static c-ceiling::double (::double)
		  "ceil")
	       (method static c-exp::double (::double)
		  "exp")
	       (method static c-log::double (::double)
		  "log")
	       (method static c-sin::double (::double)
		  "sin")
	       (method static c-cos::double (::double)
		  "cos")
	       (method static c-tan::double (::double)
		  "tan")
	       (method static c-asin::double (::double)
		  "asin")
	       (method static c-acos::double (::double)
		  "acos")
	       (method static c-atan::double (::double)
		  "atan")
	       (method static c-atan2::double (::double ::double)
		  "atan2")
	       (method static c-sqrt::double (::double)
		  "sqrt")
	       (method static c-pow::double (::double ::double)
		  "pow")
	       (method static strtod::double (::string ::long)
		  "strtod")
	       (method static $strtod::double (::string)
		  "strtod")
	       (method static c-absfl::double (::double)
		  "abs")
	       (method static c-maxfl::double (::double ::double)
		  "max")
	       (method static c-minfl::double (::double ::double)
		  "min")
	       (method static $signbit::long (::double)
		  "BGL_SIGNBIT")
	       (method static c-roundfl::double (::double)
		  "round")
	       (method static c-isfinite::bool (::double)
		  "isfinite")
	       (method static c-isinf::bool (::double)
		  "isinf")
	       (method static c-isnan::bool (::double)
		  "isnan")
	       (method static %ieee-string->double::double (::bstring)
		  "bgl_ieee_string_to_double")
	       (method static %double->ieee-string::bstring (::double)
		  "bgl_double_to_ieee_string")
	       (method static %ieee-string->float::float (::bstring)
		  "bgl_ieee_string_to_float")
	       (method static %float->ieee-string::bstring (::float)
		  "bgl_float_to_ieee_string")
	       (method static %double->llong-bits::llong (::double)
		  "DOUBLE_TO_LLONG_BITS")
	       (method static %llong-bits->double::double (::llong)
		  "LLONG_BITS_TO_DOUBLE")
	       (method static %float->int-bits::int (::float)
		  "FLOAT_TO_INT_BITS")
	       (method static %int-bits->float::float (::int)
		  "INT_BITS_TO_FLOAT")
	       (method static $randomfl::double ()
		  "RANDOMFL")))
   
   (export  (inline real?::bool ::obj)
	    (inline flonum?::bool ::obj)
	    (inline =fl::bool ::double ::double)
	    (inline >fl::bool ::double ::double)
	    (inline >=fl::bool ::double ::double)
	    (inline <fl::bool ::double ::double)
	    (inline <=fl::bool ::double ::double)
	    (inline zerofl?::bool ::double)
	    (inline positivefl?::bool ::double)
	    (inline negativefl?::bool ::double)
	    (maxfl::double ::double . rn)
	    (minfl::double ::double . rn)
	    (inline max-2fl::double ::double ::double)
	    (inline min-2fl::double ::double ::double)
	    (inline +fl::double ::double ::double)
	    (inline -fl::double ::double ::double)
	    (inline *fl::double ::double ::double)
	    (inline /fl::double ::double ::double)
	    (inline negfl::double ::double)
	    (inline absfl::double ::double)
	    (inline floorfl::double ::double)
	    (inline ceilingfl::double ::double)
	    (inline truncatefl::double ::double)
	    (roundfl::double ::double)
	    (inline remainderfl::double ::double ::double)
	    (inline expfl::double ::double)
	    (inline logfl::double ::double)
	    (inline sinfl::double ::double)
	    (inline cosfl::double ::double)
	    (inline tanfl::double ::double)
	    (inline asinfl::double ::double)
	    (inline acosfl::double ::double)
	    (atanfl::double ::double . y)
	    (inline atan-1fl::double ::double)
	    (inline atan-2fl::double ::double ::double)
	    (inline atan-2fl-ur::double ::double ::double)
	    (inline sqrtfl::double ::double)
	    (inline sqrtfl-ur::double ::double)
	    (inline exptfl::double ::double ::double)
	    (inline signbitfl::long ::double)
	    (inline integerfl?::bool ::double)
	    (inline finitefl?::bool ::double)
	    (inline infinitefl?::bool ::double)
	    (inline nanfl?::bool ::double)
	    (inline evenfl?::bool ::double)
	    (inline oddfl?::bool ::double)
	    (inline string->real::double ::string)
	    (inline ieee-string->real::real ::bstring)
	    (inline real->ieee-string::bstring ::real)
	    (inline ieee-string->double::double ::bstring)
	    (inline double->ieee-string::bstring ::double)
	    (inline ieee-string->float::float ::bstring)
	    (inline float->ieee-string::bstring ::float)
	    (inline double->llong-bits::llong ::double)
	    (inline llong-bits->double::double ::llong)
	    (inline float->int-bits::int ::float)
	    (inline int-bits->float::float ::int)
	    (inline randomfl::double))
   
   (pragma  (c-flonum? no-alloc side-effect-free (predicate-of real) no-cfa-top nesting fail-safe)
	    ($flonum? no-alloc side-effect-free (predicate-of real) no-cfa-top nesting fail-safe)
	    (real? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (c-=fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c->fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c->=fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-<fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-<=fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-+fl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c--fl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-*fl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-/fl side-effect-free no-cfa-top nesting args-safe)
	    (c-negfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-exp side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-log side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-sin side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-cos side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-tan side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-asin side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-acos side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-atan side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-sqrt side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-absfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-minfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-maxfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($signbit side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-roundfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-isnan no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-isinf no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (c-isfinite no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (flonum? no-alloc (predicate-of real) no-cfa-top nesting fail-safe)
 	    (real? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (=fl no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (>fl no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (>=fl no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (<fl no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (<=fl no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (+fl side-effect-free no-cfa-top nesting fail-safe)
	    (-fl side-effect-free no-cfa-top nesting fail-safe)
	    (*fl side-effect-free no-cfa-top nesting fail-safe)
	    (/fl side-effect-free no-cfa-top nesting)
	    (negfl side-effect-free no-cfa-top nesting fail-safe)
	    (expfl side-effect-free no-cfa-top nesting fail-safe)
	    (logfl side-effect-free no-cfa-top nesting fail-safe)
	    (sinfl side-effect-free no-cfa-top nesting fail-safe)
	    (cosfl side-effect-free no-cfa-top nesting fail-safe)
	    (tanfl side-effect-free no-cfa-top nesting fail-safe)
	    (asinfl side-effect-free no-cfa-top nesting fail-safe)
	    (acosfl side-effect-free no-cfa-top nesting fail-safe)
	    (atanfl side-effect-free no-cfa-top nesting fail-safe)
	    (sqrtfl side-effect-free no-cfa-top nesting fail-safe)
	    (absfl side-effect-free no-cfa-top nesting fail-safe)
	    (minfl side-effect-free no-cfa-top nesting fail-safe)
	    (maxfl side-effect-free no-cfa-top nesting fail-safe)
	    (signbitfl side-effect-free no-cfa-top nesting fail-safe)
	    (integerfl? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (infinitefl? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (finitefl? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (nanfl? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (double->llong-bits side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (llong-bits->double side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (float->int-bits side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (int-bits->float side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (%double->llong-bits side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (%llong-bits->double side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (%float->int-bits side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (%int-bits->float side-effect-free no-cfa-top nesting args-safe fail-safe)))

;*---------------------------------------------------------------------*/
;*    real? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (real? obj)
   (if (c-fixnum? obj)
       #t
       (c-flonum? obj)))

;*---------------------------------------------------------------------*/
;*    flonum? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (flonum? obj)
   (c-flonum? obj))

;*---------------------------------------------------------------------*/
;*    =fl ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (=fl r1 r2)
   (c-=fl r1 r2))

;*---------------------------------------------------------------------*/
;*    <fl ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (<fl r1 r2)
   (c-<fl r1 r2))

;*---------------------------------------------------------------------*/
;*    >fl ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (>fl r1 r2)
   (c->fl r1 r2))

;*---------------------------------------------------------------------*/
;*    <=fl ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (<=fl r1 r2)
   (c-<=fl r1 r2))

;*---------------------------------------------------------------------*/
;*    >=fl ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (>=fl r1 r2)
   (c->=fl r1 r2))

;*---------------------------------------------------------------------*/
;*    zerofl? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (zerofl? r)
   (=fl r 0.0))

;*---------------------------------------------------------------------*/
;*    positivefl? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (positivefl? r)
   (>fl r 0.0))

;*---------------------------------------------------------------------*/
;*    negativefl? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (negativefl? r)
   (<fl r 0.0))

;*---------------------------------------------------------------------*/
;*    opfl ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (+fl r1 r2)
   (c-+fl r1 r2))
(define-inline (-fl r1 r2)
   (c--fl r1 r2))
(define-inline (*fl r1 r2)
   (c-*fl r1 r2))
(define-inline (/fl r1 r2)
   (c-/fl r1 r2))

;*---------------------------------------------------------------------*/
;*    negfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (negfl r1)
   (c-negfl r1))
    
;*---------------------------------------------------------------------*/
;*    maxfl ...                                                        */
;*---------------------------------------------------------------------*/
(define (maxfl r1 . rn)
   (let loop ((max r1)
	      (rn  rn))
      (if (null? rn)
	  max
	  (loop (max-2fl (car rn) max) (cdr rn)))))

;*---------------------------------------------------------------------*/
;*    max-2fl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (max-2fl r1 r2)
   (c-maxfl r1 r2))

;*---------------------------------------------------------------------*/
;*    min-2fl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (min-2fl r1 r2)
   (c-minfl r1 r2))
   
;*---------------------------------------------------------------------*/
;*    minfl ...                                                        */
;*---------------------------------------------------------------------*/
(define (minfl r1 . rn)
   (let loop ((min r1)
	      (rn  rn))
      (if (null? rn)
	  min
	  (loop (min-2fl (car rn) min) (cdr rn)))))
   
;*---------------------------------------------------------------------*/
;*    absfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (absfl r)
   (c-absfl r))

;*---------------------------------------------------------------------*/
;*    floorfl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (floorfl r)
   (c-floor r))

;*---------------------------------------------------------------------*/
;*    ceilingfl ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (ceilingfl r)
   (c-ceiling r))

;*---------------------------------------------------------------------*/
;*    truncatefl ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (truncatefl r)
   (if (negativefl? r)
       (ceilingfl r)
       (floorfl r)))

;*---------------------------------------------------------------------*/
;*    roundfl ...                                                      */
;*---------------------------------------------------------------------*/
(define (roundfl r)
   (c-roundfl r))

;*---------------------------------------------------------------------*/
;*    remainderfl ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (remainderfl n1 n2)
   (c-fmod n1 n2))

;*---------------------------------------------------------------------*/
;*    expfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (expfl x)
   (c-exp x))

;*---------------------------------------------------------------------*/
;*    logfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (logfl x)
   (c-log x))
 
;*---------------------------------------------------------------------*/
;*    sinfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (sinfl x)
   (c-sin x))

;*---------------------------------------------------------------------*/
;*    cosfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cosfl x)
   (c-cos x))

;*---------------------------------------------------------------------*/
;*    tanfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (tanfl x)
   (c-tan x))

;*---------------------------------------------------------------------*/
;*    asinfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (asinfl x)
   (c-asin x))

;*---------------------------------------------------------------------*/
;*    acosfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (acosfl x)
   (c-acos x))

;*---------------------------------------------------------------------*/
;*    atanfl ...                                                       */
;*---------------------------------------------------------------------*/
(define (atanfl x . y)
   (if (null? y)
       (c-atan x)
       (let ((y (car y)))
	  (atan-2fl x y))))

;*---------------------------------------------------------------------*/
;*    atan-1fl ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (atan-1fl x)
   (c-atan x))

;*---------------------------------------------------------------------*/
;*    atan-2fl ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (atan-2fl x y)
   (let ((t (if (=fl x 0.0)
		(=fl y 0.0)
		#f)))
      (if t
	  (let ((proc::obj ($string->bstring "atanfl"))
		(msg::obj ($string->bstring "Domain error"))
		(obj::obj ($double->real 0.0)))
	     ;; !!! Warning
	     ;; the_failure is prefered to Error in order to fix a
	     ;; registers allocation bug in gcc 2.96
	     (the_failure proc msg obj)
	     0.0)
	  (c-atan2 x y))))

;*---------------------------------------------------------------------*/
;*    atan-2fl-ur ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (atan-2fl-ur x y)
   (c-atan2 x y))

;*---------------------------------------------------------------------*/
;*    sqrtfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (sqrtfl r)
   (if (<fl r 0.0)
       (let ((proc::obj ($string->bstring "sqrtfl"))
	     (msg::obj ($string->bstring "Domain error"))
	     (obj::obj ($double->real r)))
	  (begin
	     (error proc msg obj)
	     0.0))
       (c-sqrt r)))

;*---------------------------------------------------------------------*/
;*    sqrtfl-ur ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (sqrtfl-ur r)
   (c-sqrt r))

;*---------------------------------------------------------------------*/
;*    exptfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (exptfl r1 r2)
   (c-pow r1 r2))

;*---------------------------------------------------------------------*/
;*    signbitfl ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (signbitfl r)
   ($signbit r))

;*---------------------------------------------------------------------*/
;*    integerfl? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (integerfl? r)
   (cond-expand
      (bigloo-c
       (let ((intpart::double 0.0))
	  (and (finitefl? r)
	       (=fl ($modf r (pragma::void* "&($1)" (pragma intpart))) 0.0))))
      (else
       (and (finitefl? r) (=fl r (floorfl r))))))

;*---------------------------------------------------------------------*/
;*    evenfl? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (evenfl? r)
   (integerfl? (/fl r 2.0)))

;*---------------------------------------------------------------------*/
;*    oddfl? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (oddfl? r)
   (and (integerfl? r)
	(not (evenfl? r))))

;*---------------------------------------------------------------------*/
;*    finitefl? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (finitefl? r)
   (c-isfinite r))

;*---------------------------------------------------------------------*/
;*    infinitefl? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (infinitefl? r)
   (c-isinf r))

;*---------------------------------------------------------------------*/
;*    nanfl? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (nanfl? r)
   (c-isnan r))

;*---------------------------------------------------------------------*/
;*    string->real ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (string->real string)
   (cond
      ((string=? string "+nan.0")
       +nan.0)
      ((string=? string "+inf.0")
       +inf.0)
      ((string=? string "-inf.0")
       -inf.0)
      (else
       ($strtod string))))

;*---------------------------------------------------------------------*/
;*    ieee-string->real ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (ieee-string->real string)
   (%ieee-string->double string))

;*---------------------------------------------------------------------*/
;*    real->ieee-string ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (real->ieee-string real)
   (%double->ieee-string real))

;*---------------------------------------------------------------------*/
;*    ieee-string->double ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (ieee-string->double string)
   (%ieee-string->double string))

;*---------------------------------------------------------------------*/
;*    double->ieee-string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (double->ieee-string double)
   (%double->ieee-string double))

;*---------------------------------------------------------------------*/
;*    ieee-string->float ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (ieee-string->float string)
   (%ieee-string->float string))

;*---------------------------------------------------------------------*/
;*    float->ieee-string ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (float->ieee-string float)
   (%float->ieee-string float))

;*---------------------------------------------------------------------*/
;*    double->llong-bits ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (double->llong-bits::llong n::double)
   (%double->llong-bits n))

;*---------------------------------------------------------------------*/
;*    llong-bits->double ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (llong-bits->double::double n::llong)
   (%llong-bits->double n))

;*---------------------------------------------------------------------*/
;*    float->int-bits ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (float->int-bits::int n::float)
   (%float->int-bits n))

;*---------------------------------------------------------------------*/
;*    int-bits->float ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (int-bits->float::float n::int)
   (%int-bits->float n))

;*---------------------------------------------------------------------*/
;*    randomfl ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (randomfl::double)
   ($randomfl))
