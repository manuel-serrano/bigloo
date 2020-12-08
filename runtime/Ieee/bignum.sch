;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/bignum.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 11 11:28:42 2008                          */
;*    Last change :  Wed Dec 11 06:57:29 2019 (serrano)                */
;*    Copyright   :  2008-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bigloo native api                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (extern
      ($fixnum->bignum::bignum (::long) "bgl_long_to_bignum")
      ($elong->bignum::bignum (::elong) "bgl_long_to_bignum")
      ($llong->bignum::bignum (::llong) "bgl_llong_to_bignum")
      ($uint64->bignum::bignum (::uint64) "bgl_uint64_to_bignum")
      ($bignum->fixnum::long (::bignum) "bgl_bignum_to_long")
      ($bignum->elong::elong (::bignum) "bgl_bignum_to_long")
      ($bignum->llong::llong (::bignum) "bgl_bignum_to_llong")
      ($bignum-cmp::int (::bignum ::bignum) "bgl_bignum_cmp")
      ($absbx::bignum (::bignum) "bgl_bignum_abs")
      ($negbx::bignum (::bignum) "bgl_bignum_neg")
      ($divrembx::obj (::bignum ::bignum) "bgl_bignum_div")
      ($evenbx?::bool (::bignum) "bgl_bignum_even")
      ($oddbx?::bool (::bignum) "bgl_bignum_odd")
      ($+bx::bignum (::bignum ::bignum) "bgl_bignum_add")
      ($-bx::bignum (::bignum ::bignum) "bgl_bignum_sub")
      ($*bx::bignum (::bignum ::bignum) "bgl_bignum_mul")
      ($quotientbx::bignum (::bignum ::bignum) "bgl_bignum_quotient")
      ($remainderbx::bignum (::bignum ::bignum) "bgl_bignum_remainder")
      ($gcdbx::bignum (::bignum ::bignum) "bgl_bignum_gcd")
      ($lcmbx::bignum (::bignum ::bignum) "bgl_bignum_lcm")
      ($string->bignum::bignum (::string ::int) "bgl_string_to_bignum")
      ($string->integer-obj::obj (::string ::int) "bgl_string_to_integer_obj")
      ($bignum->string::bstring  (::bignum ::int) "bgl_bignum_to_string")
      ($randbx::bignum (::bignum) "bgl_rand_bignum")
      ($seed-rand::void (::long) "bgl_seed_rand")
      
      (macro $zerobx?::bool (::bignum) "BXZERO")
      (macro $positivebx?::bool (::bignum) "BXPOSITIVE")
      (macro $negativebx?::bool (::bignum) "BXNEGATIVE")
      
      ($bignum->flonum::double (::bignum) "bgl_bignum_to_flonum")
      ($flonum->bignum::bignum (::double) "bgl_flonum_to_bignum"))

   (cond-expand
      (enable-gmp
       (with __bignum))
      (else
       (import __bignum)))

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

   (cond-expand
      (enable-gmp
       (pragma
	($fixnum->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
	($elong->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
	($llong->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
	($uint64->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
	($oddbx? side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($evenbx? side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($bignum-cmp side-effect-free no-cfa-top nesting (effect) fail-safe)
	($zerobx? side-effect-free no-cfa-top nesting (effect) fail-safe)
	($positivebx? side-effect-free no-cfa-top nesting (effect) fail-safe)
	($negativebx? side-effect-free no-cfa-top nesting (effect) fail-safe)
	($absbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($negbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($+bx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($-bx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($*bx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($quotientbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($remainderbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($gcdbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($lcmbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	($flonum->bignum side-effect-free args-safe (effect) fail-safe)
	($bignum->flonum side-effect-free no-cfa-top nesting (effect) fail-safe))))

   (pragma
      ($bignum? side-effect-free (predicate-of bignum) no-cfa-top nesting fail-safe)
      ($bignum->fixnum-safe fail-safe)
      (+fx-safe fail-safe)
      (-fx-safe fail-safe)
      (*fx-safe fail-safe)
      (+elong-safe fail-safe)
      (-elong-safe fail-safe)
      (*elong-safe fail-safe)
      (+llong-safe fail-safe)
      (-llong-safe fail-safe)
      (*llong-safe fail-safe)))



   
