;*=====================================================================*/
;*    serrano/prgm/project/bigloo/flt/runtime/Ieee/flonum.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 26 14:04:03 1992                          */
;*    Last change :  Tue Dec 10 07:32:59 2024 (serrano)                */
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
	    (macro $flonum?::bool (::obj) "REALP")
	    (macro $fast-flonum?::bool (::obj) "BGL_FAST_REALP")
	    (macro $fast-real->double::double (::obj) "BGL_FAST_REAL_TO_DOUBLE")
	    (infix macro $=fl::bool (::double ::double) "==")
	    (infix macro $<fl::bool (::double ::double) "<")
	    (infix macro $<=fl::bool (::double ::double) "<=")
	    (infix macro $>fl::bool (::double ::double) ">")
	    (infix macro $>=fl::bool (::double ::double) ">=")
	    (infix macro $+fl::double (::double ::double) "+")
	    (infix macro $-fl::double (::double ::double) "-")
	    (infix macro $*fl::double (::double ::double) "*")
	    (infix macro $/fl::double (::double ::double) "/")
	    (macro $negfl::double (::double) "NEG")
	    (macro $floor::double (::double) "floor")
	    (macro $ceiling::double (::double) "ceil")
	    (macro $fmod::double (::double ::double) "fmod")
	    (macro $exp::double (::double) "exp")
	    (macro $log::double (::double) "log")
	    (macro $log2::double (::double) "log2")
	    (macro $log10::double (::double) "log10")
	    (macro $sin::double (::double) "sin")
	    (macro $cos::double (::double) "cos")
	    (macro $tan::double (::double) "tan")
	    (macro $asin::double (::double) "asin")
	    (macro $acos::double (::double) "acos")
	    (macro $atan::double (::double) "atan")
	    (macro $atan2::double (::double ::double) "atan2")
	    (macro $sqrt::double (::double) "sqrt")
	    (macro $pow::double (::double ::double) "pow")
	    (macro strtod::double (::string ::long) "strtod")
	    (macro $strtod::double (::string) "STRTOD")
	    (macro $absfl::double (::double) "fabs")
	    (macro $maxfl::double (::double ::double) "BGL_FL_MAX2")
	    (macro $minfl::double (::double ::double) "BGL_FL_MIN2")
	    (macro $signbit::long (::double) "BGL_SIGNBIT")
	    (macro $roundfl::double (::double) "BGL_FL_ROUND")
	    (macro $isfinite::bool (::double) "BGL_IS_FINITE")
	    (macro $isinf::bool (::double) "BGL_IS_INF")
	    (macro $isnan::bool (::double) "BGL_IS_NAN")
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
	       (method static $flonum?::bool (::obj)
		  "REALP")
	       (method static $fast-flonum?::bool (::obj)
		  "BGL_FAST_REALP")
	       (method static $fast-real->double::double (::obj)
		  "BGL_FAST_REAL_TO_DOUBLE")
	       (method static $=fl::bool (::double ::double)
		  "EQ_FL")
	       (method static $<fl::bool (::double ::double)
		  "LT_FL")
	       (method static $<=fl::bool (::double ::double)
		  "LE_FL")
	       (method static $>fl::bool (::double ::double)
		  "GT_FL")
	       (method static $>=fl::bool (::double ::double)
		  "GE_FL")
	       (method static $+fl::double (::double ::double)
		  "PLUS_FL")
	       (method static $-fl::double (::double ::double)
		  "MINUS_FL")
	       (method static $*fl::double (::double ::double)
		  "MUL_FL")
	       (method static $/fl::double (::double ::double)
		  "DIV_FL")
	       (method static $negfl::double (::double)
		  "NEG_FL")
	       (method static $fmod::double (::double ::double)
		  "fmod")
	       (method static $floor::double (::double)
		  "floor")
	       (method static $ceiling::double (::double)
		  "ceil")
	       (method static $exp::double (::double)
		  "exp")
	       (method static $log::double (::double)
		  "log")
	       (method static $log2::double (::double)
		  "log2")
	       (method static $log10::double (::double)
		  "log10")
	       (method static $sin::double (::double)
		  "sin")
	       (method static $cos::double (::double)
		  "cos")
	       (method static $tan::double (::double)
		  "tan")
	       (method static $asin::double (::double)
		  "asin")
	       (method static $acos::double (::double)
		  "acos")
	       (method static $atan::double (::double)
		  "atan")
	       (method static $atan2::double (::double ::double)
		  "atan2")
	       (method static $sqrt::double (::double)
		  "sqrt")
	       (method static $pow::double (::double ::double)
		  "pow")
	       (method static strtod::double (::string ::long)
		  "strtod")
	       (method static $strtod::double (::string)
		  "strtod")
	       (method static $absfl::double (::double)
		  "abs")
	       (method static $maxfl::double (::double ::double)
		  "max")
	       (method static $minfl::double (::double ::double)
		  "min")
	       (method static $signbit::long (::double)
		  "BGL_SIGNBIT")
	       (method static $roundfl::double (::double)
		  "round")
	       (method static $isfinite::bool (::double)
		  "isfinite")
	       (method static $isinf::bool (::double)
		  "isinf")
	       (method static $isnan::bool (::double)
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
	    (inline log2fl::double ::double)
	    (inline log10fl::double ::double)
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
	    (inline string->real::double ::bstring)
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
   
   (pragma  ($flonum? no-alloc side-effect-free (predicate-of double) no-cfa-top nesting fail-safe)
	    ($fast-flonum? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    (real? no-alloc side-effect-free no-cfa-top nesting fail-safe)
	    ($=fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($>fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($>=fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($<fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($<=fl no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($+fl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($-fl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($*fl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($/fl side-effect-free no-cfa-top nesting args-safe)
	    ($negfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($exp side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($log side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($log2 side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($log10 side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($sin side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($cos side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($tan side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($asin side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($acos side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($atan side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($sqrt side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($absfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($minfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($maxfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($signbit side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($roundfl side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($isnan no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($isinf no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
	    ($isfinite no-alloc side-effect-free no-cfa-top nesting args-safe fail-safe)
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
	    (log2fl side-effect-free no-cfa-top nesting fail-safe)
	    (log10fl side-effect-free no-cfa-top nesting fail-safe)
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
   (if ($fixnum? obj)
       #t
       ($flonum? obj)))

;*---------------------------------------------------------------------*/
;*    flonum? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (flonum? obj)
   ($flonum? obj))

;*---------------------------------------------------------------------*/
;*    =fl ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (=fl r1 r2)
   ($=fl r1 r2))

;*---------------------------------------------------------------------*/
;*    <fl ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (<fl r1 r2)
   ($<fl r1 r2))

;*---------------------------------------------------------------------*/
;*    >fl ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (>fl r1 r2)
   ($>fl r1 r2))

;*---------------------------------------------------------------------*/
;*    <=fl ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (<=fl r1 r2)
   ($<=fl r1 r2))

;*---------------------------------------------------------------------*/
;*    >=fl ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (>=fl r1 r2)
   ($>=fl r1 r2))

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
   ($+fl r1 r2))
(define-inline (-fl r1 r2)
   ($-fl r1 r2))
(define-inline (*fl r1 r2)
   ($*fl r1 r2))
(define-inline (/fl r1 r2)
   ($/fl r1 r2))

;*---------------------------------------------------------------------*/
;*    negfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (negfl r1)
   ($negfl r1))
    
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
   ($maxfl r1 r2))

;*---------------------------------------------------------------------*/
;*    min-2fl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (min-2fl r1 r2)
   ($minfl r1 r2))
   
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
   ($absfl r))

;*---------------------------------------------------------------------*/
;*    floorfl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (floorfl r)
   ($floor r))

;*---------------------------------------------------------------------*/
;*    ceilingfl ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (ceilingfl r)
   ($ceiling r))

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
   ($roundfl r))

;*---------------------------------------------------------------------*/
;*    remainderfl ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (remainderfl n1 n2)
   ($fmod n1 n2))

;*---------------------------------------------------------------------*/
;*    expfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (expfl x)
   ($exp x))

;*---------------------------------------------------------------------*/
;*    logfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (logfl x)
   ($log x))
 
;*---------------------------------------------------------------------*/
;*    log2fl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (log2fl x)
   ($log2 x))
 
;*---------------------------------------------------------------------*/
;*    log10fl ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (log10fl x)
   ($log10 x))
 
;*---------------------------------------------------------------------*/
;*    sinfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (sinfl x)
   ($sin x))

;*---------------------------------------------------------------------*/
;*    cosfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (cosfl x)
   ($cos x))

;*---------------------------------------------------------------------*/
;*    tanfl ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (tanfl x)
   ($tan x))

;*---------------------------------------------------------------------*/
;*    asinfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (asinfl x)
   ($asin x))

;*---------------------------------------------------------------------*/
;*    acosfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (acosfl x)
   ($acos x))

;*---------------------------------------------------------------------*/
;*    atanfl ...                                                       */
;*---------------------------------------------------------------------*/
(define (atanfl x . y)
   (if (null? y)
       ($atan x)
       (let ((y (car y)))
	  (atan-2fl x y))))

;*---------------------------------------------------------------------*/
;*    atan-1fl ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (atan-1fl x)
   ($atan x))

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
	  ($atan2 x y))))

;*---------------------------------------------------------------------*/
;*    atan-2fl-ur ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (atan-2fl-ur x y)
   ($atan2 x y))

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
       ($sqrt r)))

;*---------------------------------------------------------------------*/
;*    sqrtfl-ur ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (sqrtfl-ur r)
   ($sqrt r))

;*---------------------------------------------------------------------*/
;*    exptfl ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (exptfl r1 r2)
   ($pow r1 r2))

;*---------------------------------------------------------------------*/
;*    signbitfl ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (signbitfl r)
   ($signbit r))

;*---------------------------------------------------------------------*/
;*    integerfl? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (integerfl? r)
   (and (finitefl? r) (=fl r (floorfl r))))

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
   ($isfinite r))

;*---------------------------------------------------------------------*/
;*    infinitefl? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (infinitefl? r)
   ($isinf r))

;*---------------------------------------------------------------------*/
;*    nanfl? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (nanfl? r)
   ($isnan r))

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
